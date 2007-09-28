/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include <stdlib.h>
#include <string.h>
#include "cxx-utils.h"
#include "cxx-scope.h"
#include "cxx-exprtype.h"
#include "cxx-koenig.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"

typedef 
struct associated_scopes_tag
{
    int num_associated_scopes;
    scope_t** associated_scopes;
} associated_scopes_t;

static associated_scopes_t compute_associated_scopes(int num_arguments, argument_type_info_t** argument_type_list);


char koenig_can_be_used(AST called_expression, decl_context_t decl_context)
{
    switch (ASTType(called_expression))
    {
        // At the moment consider only this case but let room for non-member
        // operators
        case AST_SYMBOL :
            break;
        default:
            // Koenig cannot be used here
            return 0;
    }

    // Now lookup for the symbol and check if we find a member function
    // where we will state that it cannot be used there
    scope_entry_list_t* list = query_id_expression(decl_context, called_expression);

    // If nothing found it might be due to Koenig
    if (list == NULL)
        return 1;

    enum cxx_symbol_kind valid_functions[] =
    {
        SK_FUNCTION,
        SK_TEMPLATE_FUNCTION
    };

    list = filter_symbol_kind_set(list, 2, valid_functions);

    if (list == NULL)
    {
        // This means that something that is not a function was found so Koenig
        // should not be applied here
        return 0;
    }

    scope_entry_list_t* iter = list;
    while (iter != NULL)
    {
        scope_entry_t* entry = iter->entry;

        // If the function found is a member one, Koenig cannot be done
        if (entry->is_member)
            return 0;
        // If the function found is a block-scope declared one, Koenig cannot
        // be done (TODO: check that using function declarations are not considered
        // here)
        if (entry->decl_context.current_scope->kind == BLOCK_SCOPE)
            return 0;

        iter = iter->next;
    }

    // Nothing prevents us to do Koenig
    return 1;
}

scope_entry_list_t* koenig_lookup(
        int num_arguments,
        argument_type_info_t** argument_type_list, 
        decl_context_t decl_context)
{
    associated_scopes_t associated_scopes;
    associated_scopes = compute_associated_scopes(num_arguments, argument_type_list);
    
    return NULL;
}

static void compute_associated_scopes_aux(associated_scopes_t* associated_scopes, 
        int num_arguments, argument_type_info_t** argument_type_list);

static void compute_associated_scopes_rec(associated_scopes_t* associated_scopes, 
        type_t* argument_type);

static associated_scopes_t compute_associated_scopes(int num_arguments, argument_type_info_t** argument_type_list)
{
    associated_scopes_t result;
    memset(&result, 0, sizeof(result));

    compute_associated_scopes_aux(&result, num_arguments, argument_type_list);

    return result;
}

static void compute_associated_scopes_aux(associated_scopes_t* associated_scopes, 
        int num_arguments, argument_type_info_t** argument_type_list)
{
    int i;
    for (i = 0; i < num_arguments; i++)
    {
        type_t* argument_type = argument_type_list[i]->type;
        compute_associated_scopes_rec(associated_scopes, argument_type);
    }
}

static scope_entry_t** compute_set_of_associated_classes_scope(type_t* type_info, associated_scopes_t* associated_scopes);

static void compute_associated_scopes_rec(associated_scopes_t* associated_scopes, 
        type_t* argument_type)
{
    argument_type = advance_over_typedefs(argument_type);

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
        compute_set_of_associated_classes_scope(argument_type, associated_scopes);

        // Nothing else to be done
        return;
    }

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
    if (is_specialized_class_type(argument_type))
    {
        compute_set_of_associated_classes_scope(argument_type, associated_scopes);

        template_argument_list_t* template_arguments = template_type_get_template_arguments(argument_type);

        // Now for every parameter 
        int i;
        for (i = 0; i < template_arguments->num_arguments; i++)
        {
            template_argument_t* curr_argument = template_arguments->argument_list[i];
            if (curr_argument->kind == TAK_NONTYPE)
            {
                // Non-type arguments do not contribute in the scopes
            }
            else if (curr_argument->kind == TAK_TYPE)
            {
                compute_associated_scopes_rec(associated_scopes, curr_argument->type);
            }
            else if (curr_argument->kind == TAK_TEMPLATE)
            {
            }
        }
    }

    /*
     * If T is an enumeration type its associated namespace is the namespace in which it
     * is defined. If it is a class member, its associated class is the member's class, else
     * it has no associated class.
     */
    if (is_enumerated_type(argument_type))
    {
        scope_t* outer_namespace = enum_type_get_context(argument_type).namespace_scope;

        ERROR_CONDITION(outer_namespace == NULL, "The enclosing namespace scope is NULL!", 0);

        P_LIST_ADD_ONCE_FUN(associated_scopes->associated_scopes, associated_scopes->num_associated_scopes,
                outer_namespace, same_scope);

        if (is_named_type(argument_type))
        {
            scope_entry_t* symbol = named_type_get_symbol(argument_type);
            if (symbol->is_member)
            {
                type_t* class_type = symbol->class_type;
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

static scope_entry_t** compute_set_of_associated_classes_scope(type_t* type_info, associated_scopes_t* associated_scopes)
{
    compute_set_of_associated_classes_scope_rec(type_info, associated_scopes);
    return NULL;
}

static void compute_set_of_associated_classes_scope_rec(type_t* type_info, 
        associated_scopes_t* associated_scopes)
{
    // Add the scope of the current class
    ERROR_CONDITION(class_type_get_context(type_info).current_scope == NULL, "Error, this scope should not be NULL", 0);

    scope_t* outer_namespace = class_type_get_context(type_info).namespace_scope;

    ERROR_CONDITION(outer_namespace == NULL, "Enclosing namespace not found", 0);

    P_LIST_ADD_ONCE_FUN(associated_scopes->associated_scopes, 
            associated_scopes->num_associated_scopes, 
            outer_namespace,
            same_scope);

    if (is_named_type(type_info))
    {
        scope_entry_t* symbol = named_type_get_symbol(type_info);
        if (symbol->is_member)
        {
            type_t* class_type = symbol->class_type;

            compute_associated_scopes_rec(associated_scopes, class_type);
        }
    }

    type_t* class_type = get_actual_class_type(type_info);

    // Add the bases
    int i;
    for (i = 0; i < class_type_get_num_bases(class_type); i++)
    {
        scope_entry_t* base_symbol = class_type_get_base_num(class_type, i, NULL);
        type_t* type_info = base_symbol->type_information;

        compute_set_of_associated_classes_scope_rec(type_info, associated_scopes);
    }
}
