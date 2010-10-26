/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

#include <stdlib.h>
#include <string.h>
#include "cxx-utils.h"
#include "cxx-scope.h"
#include "cxx-exprtype.h"
#include "cxx-koenig.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-entrylist.h"

#define MAX_ASSOCIATED_SCOPES (256)

typedef 
struct associated_scopes_tag
{
    int num_associated_scopes;
    scope_t* associated_scopes[256];
} associated_scopes_t;

static associated_scopes_t compute_associated_scopes(int num_arguments, type_t** argument_type_list);

scope_entry_list_t* koenig_lookup(
        int num_arguments,
        type_t** argument_type_list,
        decl_context_t normal_decl_context,
        AST id_expression)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "KOENIG: Doing argument dependent lookup using the given parameters types\n");

        if (num_arguments > 0)
        {
            int i = 0;
            for (i = 0; i < num_arguments; i++)
            {
                fprintf(stderr, "KOENIG:    [%d] %s\n", i, print_declarator(argument_type_list[i]));
            }
        }
        else
        {
                fprintf(stderr, "KOENIG:    <No arguments>\n");
        }
    }

    associated_scopes_t associated_scopes;
    associated_scopes = compute_associated_scopes(num_arguments, argument_type_list);

    DEBUG_CODE()
    {
        fprintf(stderr, "KOENIG: Besides the normal lookup, %d associated "
                "scopes will be considered too\n",
                associated_scopes.num_associated_scopes);
    }

    scope_entry_list_t *result = NULL;

    // First do normal lookup
    result = query_id_expression(normal_decl_context, id_expression);

    // For every associated scope
    int i;
    for (i = 0; i < associated_scopes.num_associated_scopes; i++)
    {
        scope_t* current_scope = associated_scopes.associated_scopes[i];

        // Query only the scope
        // Wrap the scope (this is a design quirk of decl_context_t)
        decl_context_t current_context;
        memset(&current_context, 0, sizeof(current_context));
        current_context.current_scope = current_scope;
        
        DEBUG_CODE()
        {
            fprintf(stderr, "KOENIG: Looking up in associated scope '%p'\n", current_scope);
        }

        scope_entry_list_t* current_result = query_in_scope(current_context, id_expression);

        result = entry_list_merge(result, current_result);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "KOENIG: Argument dependent lookup ended\n");
    }
    
    return result;
}

static void compute_associated_scopes_aux(associated_scopes_t* associated_scopes, 
        int num_arguments, type_t** argument_type_list);

static void compute_associated_scopes_rec(associated_scopes_t* associated_scopes, 
        type_t* argument_type);

static associated_scopes_t compute_associated_scopes(int num_arguments, type_t** argument_type_list)
{
    associated_scopes_t result;
    memset(&result, 0, sizeof(result));

    compute_associated_scopes_aux(&result, num_arguments, argument_type_list);

    return result;
}

static void compute_associated_scopes_aux(associated_scopes_t* associated_scopes, 
        int num_arguments, type_t** argument_type_list)
{
    int i;
    for (i = 0; i < num_arguments; i++)
    {
        type_t* argument_type = argument_type_list[i];
        compute_associated_scopes_rec(associated_scopes, argument_type);
    }
}

static void compute_set_of_associated_classes_scope(type_t* type_info, associated_scopes_t* associated_scopes);

static void add_associated_scope(associated_scopes_t* associated_scopes, scope_t* sc)
{
    ERROR_CONDITION(associated_scopes->num_associated_scopes >= MAX_ASSOCIATED_SCOPES,
            "Too many associated scopes", 0);
    
    ERROR_CONDITION(sc->kind != NAMESPACE_SCOPE, 
            "Associated scopes by means of Koenig only can be namespace scopes", 0);

    int i;
    for (i = 0; i < associated_scopes->num_associated_scopes; i++)
    {
        // Do not add if already there
        if (associated_scopes->associated_scopes[i] == sc)
            return;
    }

    associated_scopes->associated_scopes[associated_scopes->num_associated_scopes] = sc;
    associated_scopes->num_associated_scopes++;

    // If this scope is an inline one, add the enclosing scope as well
    if (sc->related_entry != NULL
            && sc->related_entry->entity_specs.is_inline)
    {
        add_associated_scope(associated_scopes, sc->contained_in);
    }
}

static void compute_associated_scopes_rec(associated_scopes_t* associated_scopes, 
        type_t* argument_type)
{
    argument_type = no_ref(advance_over_typedefs(argument_type));

    /*
     * If T is a fundamental type, its associated sets of namespaces and classes are empty
     */
    if (is_fundamental_type(argument_type))
        // Nothing else to be done
        return;


    /*
     * If T is a class type, including unions, its associated classes are, the
     * class itself, the class of which it is a member (if any), and its direct
     * and indirect base classes. Its associated namespaces are the namespaces
     * in which its associated classes are defined.
     */
    if (is_class_type(argument_type))
    {
        type_t* class_type = get_actual_class_type(argument_type);
        compute_set_of_associated_classes_scope(class_type, associated_scopes);

        /*
         * Furthermore, if T is a class template specialization, its associated
         * namespaces and classes also include: the namespaces and classes
         * associated with the types of the template arguments provided for
         * template type parameters (excluding template template parameters); the
         * namespaces of which any template template arguments are members and the
         * classes of which any member templates used as template template
         * arguments are members.
         *
         * Example:
         *
         * namespace L
         * {
         *   template <class T>
         *   struct A
         *   {
         *   };
         * }
         *
         * namespace K
         * {
         *    struct M { };
         *    void f(L::A<M>);
         * };
         *
         * void g()
         * {
         *   L::A<K::M> a;
         *   f(a);
         * }
         */
        if (is_template_specialized_type(class_type))
        {
            template_argument_list_t* arg_list = template_specialized_type_get_template_arguments(class_type);
            int i;
            for (i = 0; i < arg_list->num_arguments; i++)
            {
                template_argument_t* arg = arg_list->argument_list[i];
                if (arg->kind == TAK_TYPE
                        || arg->kind == TAK_TEMPLATE)
                {
                    compute_associated_scopes_rec( associated_scopes, arg->type);
                }
            }
        }

        // Nothing else to be done
        return;
    }

    /*
     * If T is an enumeration type its associated namespace is the namespace in which it
     * is defined. If it is a class member, its associated class is the member's class, else
     * it has no associated class.
     */
    if (is_enum_type(argument_type))
    {
        decl_context_t type_decl_context = enum_type_get_context(argument_type);
        scope_t* outer_namespace = type_decl_context.namespace_scope;

        ERROR_CONDITION(outer_namespace == NULL, "The enclosing namespace scope is NULL!", 0);

        add_associated_scope(associated_scopes,
                outer_namespace);

        if (is_named_type(argument_type))
        {
            scope_entry_t* symbol = named_type_get_symbol(argument_type);
            if (symbol->entity_specs.is_member)
            {
                type_t* class_type = symbol->entity_specs.class_type;
                compute_associated_scopes_rec(associated_scopes, class_type);
            }
        }

        // Nothing else to be done
        return;
    }

    /*
     * If T is a pointer to U or an array of U, its associated namespaces and classes
     * are those associated with U
     */
    if (is_pointer_type(argument_type)
            || is_array_type(argument_type))
    {
        type_t* pointed_type = NULL;
        if (is_pointer_type(argument_type))
        {
            pointed_type = pointer_type_get_pointee_type(argument_type);
        }
        else
        {
            pointed_type = array_type_get_element_type(argument_type);
        }

        compute_associated_scopes_rec(associated_scopes, pointed_type);

        // Nothing else to be done
        return;
    }

    /*
     * If T is a function type, its associated namespaces and classes are those associated
     * with the parameter types and those associated with the return type
     */
    if (is_function_type(argument_type))
    {
        // Parameter types
        int i;
        for (i = 0; i < function_type_get_num_parameters(argument_type); i++)
        {
            type_t* current_parameter = function_type_get_parameter_type_num(argument_type, i);

            compute_associated_scopes_rec(associated_scopes, current_parameter);
        }

        // Return type
        compute_associated_scopes_rec(associated_scopes, function_type_get_return_type(argument_type));

        // Nothing else to be done
        return;
    }

    if (is_pointer_to_member_type(argument_type))
    {
        /*
         * If T is a pointer to member function of class X, its associated namespaces and classes
         * are those associated with the function parameter types and return type, together
         * with those associated with X
         */
        /*
         * If T is a pointer to a data member of class X, its associated namespaces and classes
         * are those associated with the member type together with those associated with X
         */
        compute_associated_scopes_rec(associated_scopes, 
                pointer_type_get_pointee_type(argument_type));

        type_t* pointed_class_type = pointer_to_member_type_get_class_type(argument_type);

        compute_associated_scopes_rec(associated_scopes, pointed_class_type);
        return;
    }
}

static void compute_set_of_associated_classes_scope_rec(type_t* type_info,
        associated_scopes_t* associated_scopes);

static void compute_set_of_associated_classes_scope(type_t* type_info, associated_scopes_t* associated_scopes)
{
    compute_set_of_associated_classes_scope_rec(type_info, associated_scopes);
}

static void compute_set_of_associated_classes_scope_rec(type_t* type_info, 
        associated_scopes_t* associated_scopes)
{
    // Add the scope of the current class
    ERROR_CONDITION(class_type_get_context(type_info).current_scope == NULL, "Error, this scope should not be NULL", 0);

    scope_t* outer_namespace = class_type_get_context(type_info).namespace_scope;

    ERROR_CONDITION(outer_namespace == NULL, "Enclosing namespace not found", 0);

    add_associated_scope(associated_scopes,
            outer_namespace);

    if (is_named_type(type_info))
    {
        scope_entry_t* symbol = named_type_get_symbol(type_info);
        if (symbol->entity_specs.is_member)
        {
            type_t* class_type = symbol->entity_specs.class_type;

            compute_associated_scopes_rec(associated_scopes, class_type);
        }
    }

    type_t* class_type = get_actual_class_type(type_info);

    // Add the bases
    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        char is_dependent = 0;
        scope_entry_t* base_symbol = class_type_get_base_num(class_type, i, NULL, &is_dependent);
        if (is_dependent)
            continue;

        type_t* base_type_info = base_symbol->type_information;

        compute_set_of_associated_classes_scope_rec(base_type_info, associated_scopes);
    }
}
