/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
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

// Koenig lookup may need to instantiate something
#include "cxx-instantiation.h"

typedef 
struct associated_scopes_tag
{
    int num_associated_scopes;
    scope_t** associated_scopes;
    int num_associated_classes;
    scope_entry_t** associated_classes;
} koenig_lookup_info_t;

static void compute_associated_scopes(
        koenig_lookup_info_t* koenig_lookup_info,
        int num_arguments, type_t** argument_type_list,
        const locus_t* locus);

scope_entry_list_t* koenig_lookup(
        int num_arguments,
        type_t** argument_type_list,
        const decl_context_t* normal_decl_context,
        nodecl_t nodecl_simple_name,
        const locus_t* locus)
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

    koenig_lookup_info_t koenig_info;
    compute_associated_scopes(&koenig_info, num_arguments, argument_type_list, locus);

    DEBUG_CODE()
    {
        fprintf(stderr, "KOENIG: Besides the normal lookup, %d associated "
                "scopes will be considered too\n",
                koenig_info.num_associated_scopes);
    }

    scope_entry_list_t *result = NULL;

    // First do normal lookup, filtering non-visible function friend declaration
    result = query_name_str_flags(normal_decl_context, 
            nodecl_get_text(nodecl_simple_name), 
            NULL,
            DF_IGNORE_FRIEND_DECL);

    // For every associated scope
    int i;
    for (i = 0; i < koenig_info.num_associated_scopes; i++)
    {
        scope_t* current_scope = koenig_info.associated_scopes[i];

        // Query only the scope
        // Wrap the scope (this is a design quirk of const decl_context_t*)
        decl_context_t* current_context = decl_context_empty();
        current_context->current_scope = current_scope;
        
        DEBUG_CODE()
        {
            fprintf(stderr, "KOENIG: Looking up in associated scope '%p'\n", current_scope);
        }

        scope_entry_list_t* current_result = query_nodecl_name_flags(current_context, 
                nodecl_simple_name, 
                NULL,
                DF_ONLY_CURRENT_SCOPE);

        scope_entry_list_t* filtered_friends = NULL;
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(current_result);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_entry = entry_list_iterator_current(it);

            if (!symbol_entity_specs_get_is_friend_declared(current_entry))
            {
                // Nothing to do
                filtered_friends = entry_list_add(filtered_friends, current_entry);
                continue;
            }

            // This is a friend declared, check it against every associated class
#define i (1=1)
#define it (1=1)
            char found_in_an_associated_class = 0;
            int j;
            for (j = 0; j < koenig_info.num_associated_classes && !found_in_an_associated_class; j++)
            {
                scope_entry_t* current_class = koenig_info.associated_classes[j];
                scope_entry_list_t* friend_list = class_type_get_friends(current_class->type_information);

                scope_entry_list_iterator_t* it2 = NULL;
                for (it2 = entry_list_iterator_begin(friend_list);
                        !entry_list_iterator_end(it2) && !found_in_an_associated_class;
                        entry_list_iterator_next(it2))
                {
                    scope_entry_t* friend_decl = entry_list_iterator_current(it2);
                    scope_entry_t* current_friend = symbol_entity_specs_get_alias_to(friend_decl);

                    found_in_an_associated_class = (current_friend == current_entry);
                }

                entry_list_iterator_free(it2);
            }
#undef it
#undef i

            if (found_in_an_associated_class)
            {
                filtered_friends = entry_list_add(filtered_friends, current_entry);
            }
        }
        entry_list_iterator_free(it);

        scope_entry_list_t* old_result = result;

        result = entry_list_merge(old_result, filtered_friends);
        entry_list_free(old_result);
        entry_list_free(filtered_friends);
        entry_list_free(current_result);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "KOENIG: Argument dependent lookup ended\n");
    }

    DELETE(koenig_info.associated_scopes);
    DELETE(koenig_info.associated_classes);

    return result;
}

static void compute_associated_scopes_aux(koenig_lookup_info_t* koenig_info, 
        int num_arguments, type_t** argument_type_list,
        const locus_t* locus);

static void compute_associated_scopes_rec(koenig_lookup_info_t* koenig_info, 
        type_t* argument_type,
        const locus_t* locus);

static void compute_associated_scopes(
        koenig_lookup_info_t* result,
        int num_arguments, type_t** argument_type_list,
        const locus_t* locus)
{
    memset(result, 0, sizeof(*result));

    compute_associated_scopes_aux(result, num_arguments, argument_type_list, locus);
}

static void compute_associated_scopes_aux(koenig_lookup_info_t* koenig_info, 
        int num_arguments, type_t** argument_type_list,
        const locus_t* locus)
{
    int i;
    for (i = 0; i < num_arguments; i++)
    {
        type_t* argument_type = argument_type_list[i];
        compute_associated_scopes_rec(koenig_info, argument_type, locus);
    }
}

static void compute_set_of_associated_classes_scope(type_t* type_info, koenig_lookup_info_t* koenig_info,
        const locus_t* locus);

static void add_associated_scope(koenig_lookup_info_t* koenig_info, scope_t* sc)
{
    ERROR_CONDITION(sc->kind != NAMESPACE_SCOPE, 
            "Associated scopes by means of Koenig only can be namespace scopes", 0);

    P_LIST_ADD_ONCE(koenig_info->associated_scopes, koenig_info->num_associated_scopes, sc);

    // If this scope is an inline one, add the enclosing scope as well
    if (sc->related_entry != NULL
            && symbol_entity_specs_get_is_inline(sc->related_entry))
    {
        add_associated_scope(koenig_info, sc->contained_in);
    }
}

static void add_associated_class(koenig_lookup_info_t* koenig_info, scope_entry_t* class_symbol)
{
    if (class_symbol->kind == SK_TYPEDEF)
    {
        type_t* advanced_type = advance_over_typedefs(class_symbol->type_information);
        class_symbol = named_type_get_symbol(advanced_type);
    }

    if (class_symbol->kind == SK_TEMPLATE_ALIAS)
    {
        class_symbol = named_type_get_symbol(class_symbol->type_information);
    }

    ERROR_CONDITION(class_symbol == NULL
            || class_symbol->kind != SK_CLASS, "Symbol must be a class", 0);

    int i;
    for (i = 0; i < koenig_info->num_associated_classes; i++)
    {
        // Do not add if already there
        if (equivalent_types(
                    koenig_info->associated_classes[i]->type_information,
                    class_symbol->type_information))
            return;
    }

    P_LIST_ADD(koenig_info->associated_classes, koenig_info->num_associated_classes, class_symbol);
}

static void compute_associated_scopes_rec(
        koenig_lookup_info_t* koenig_info, 
        type_t* argument_type,
        const locus_t* locus)
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
        compute_set_of_associated_classes_scope(argument_type, koenig_info, locus);

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
        argument_type = get_actual_class_type(argument_type);
        if (is_template_specialized_type(argument_type))
        {
            template_parameter_list_t* template_parameters = template_specialized_type_get_template_arguments(argument_type);
            int i;
            for (i = 0; i < template_parameters->num_parameters; i++)
            {
                template_parameter_value_t* arg = template_parameters->arguments[i];
                if (arg->kind == TPK_TYPE
                        || arg->kind == TPK_TEMPLATE
                        || arg->kind == TPK_TYPE_PACK
                        || arg->kind == TPK_TEMPLATE_PACK)
                {
                    compute_associated_scopes_rec(koenig_info, arg->type, locus);
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
        const decl_context_t* type_decl_context = enum_type_get_context(argument_type);
        scope_t* outer_namespace = type_decl_context->namespace_scope;

        ERROR_CONDITION(outer_namespace == NULL, "The enclosing namespace scope is NULL!", 0);

        add_associated_scope(koenig_info,
                outer_namespace);

        if (is_named_type(argument_type))
        {
            scope_entry_t* symbol = named_type_get_symbol(argument_type);
            if (symbol_entity_specs_get_is_member(symbol))
            {
                type_t* class_type = symbol_entity_specs_get_class_type(symbol);
                compute_set_of_associated_classes_scope(class_type, koenig_info, locus);
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

        compute_associated_scopes_rec(koenig_info, pointed_type, locus);

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

            compute_associated_scopes_rec(koenig_info, current_parameter, locus);
        }

        // Return type
        compute_associated_scopes_rec(koenig_info,
                function_type_get_return_type(argument_type),
                locus);

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
        compute_associated_scopes_rec(koenig_info, 
                pointer_type_get_pointee_type(argument_type),
                locus);

        type_t* pointed_class_type = pointer_to_member_type_get_class_type(argument_type);

        compute_associated_scopes_rec(koenig_info, pointed_class_type, locus);
        return;
    }
}

static void compute_set_of_associated_classes_scope_rec(type_t* type_info,
        koenig_lookup_info_t* koenig_info,
        const locus_t* locus);

static void compute_set_of_associated_classes_scope(type_t* type_info, koenig_lookup_info_t* koenig_info,
        const locus_t* locus)
{
    compute_set_of_associated_classes_scope_rec(type_info, koenig_info, locus);
}

static void compute_set_of_associated_classes_scope_rec(type_t* type_info,
        koenig_lookup_info_t* koenig_info,
        const locus_t* locus)
{
    // Ignore typedefs
    type_info = advance_over_typedefs(type_info);

    // Add the scope of the current class
    ERROR_CONDITION(!is_named_class_type(type_info), "This must be a named class type", 0);
    ERROR_CONDITION(class_type_get_context(type_info)->current_scope == NULL, "Error, this scope should not be NULL", 0);

    scope_entry_t* class_symbol = named_type_get_symbol(type_info);
    int i;
    for (i = 0; i < koenig_info->num_associated_classes; i++)
    {
        // Do nothing if already here
        if (koenig_info->associated_classes[i] == class_symbol)
            return;
    }

    scope_t* outer_namespace = class_type_get_context(type_info)->namespace_scope;

    ERROR_CONDITION(outer_namespace == NULL, "Enclosing namespace not found", 0);

    add_associated_scope(koenig_info, outer_namespace);

    class_type_complete_if_possible(
            named_type_get_symbol(type_info),
            named_type_get_symbol(type_info)->decl_context,
            locus);

    add_associated_class(koenig_info, class_symbol);

    if (symbol_entity_specs_get_is_member(class_symbol))
    {
        type_t* class_type = symbol_entity_specs_get_class_type(class_symbol);
        compute_associated_scopes_rec(koenig_info, class_type, locus);
    }

    // Add the bases
    for (i = 0; i < class_type_get_num_bases(type_info); i++)
    {
        char is_dependent = 0;
        scope_entry_t* base_symbol = class_type_get_base_num(type_info, i, 
                /* is_virtual */ NULL, 
                &is_dependent,
                /* is_expansion */ NULL,
                /* access_specifier */ NULL);
        if (is_dependent)
            continue;

        compute_set_of_associated_classes_scope_rec(get_user_defined_type(base_symbol), koenig_info, locus);
    }
}
