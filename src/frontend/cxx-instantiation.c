/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-scope.h"
#include "cxx-entrylist.h"

#include "cxx-printscope.h"

AST instantiate_tree(AST orig_tree, decl_context_t context_of_being_instantiated);

static scope_entry_t* add_duplicate_member_to_class(decl_context_t context_of_being_instantiated,
        type_t* being_instantiated,
        scope_entry_t* member_of_template)
{
    scope_entry_t* new_member = new_symbol(context_of_being_instantiated, 
            context_of_being_instantiated.current_scope,
            member_of_template->symbol_name);

    *new_member = *member_of_template;

    new_member->decl_context = context_of_being_instantiated;
    new_member->entity_specs.class_type = being_instantiated;

    class_type_add_member(get_actual_class_type(being_instantiated), new_member);

    return new_member;
}

typedef
struct type_map_tag
{
    type_t* orig_type;
    type_t* new_type;
} type_map_t;

static scope_entry_t* instantiate_template_type_member(type_t* template_type, 
        decl_context_t context_of_being_instantiated,
        scope_entry_t *member_of_template,
        type_t* being_instantiated, 
        char is_class,
        const char* filename, 
        int line,
        type_map_t** template_map, 
        int *num_items_template_map)
{
    // This is the primary template
    template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);

    scope_entry_t* new_member = new_symbol(context_of_being_instantiated, 
            context_of_being_instantiated.current_scope,
            member_of_template->symbol_name);

    template_parameter_list_t* update_template_parameters = calloc(1, sizeof(*update_template_parameters));
    // Update the template parameters
    int i;
    for (i = 0; i < template_parameters->num_template_parameters; i++)
    {
        template_parameter_t* template_parameter = template_parameters->template_parameters[i];

        template_parameter_t* updated_template_parameter = calloc(1, sizeof(*updated_template_parameter));

        updated_template_parameter->kind = template_parameter->kind;
        updated_template_parameter->entry = template_parameter->entry;
        updated_template_parameter->has_default_argument = template_parameter->has_default_argument;

        if (updated_template_parameter->has_default_argument)
        {
            updated_template_parameter->default_template_argument 
                = calloc(1, sizeof(*updated_template_parameter->default_template_argument));
            template_argument_t* default_template_argument = updated_template_parameter->default_template_argument;

            default_template_argument->nesting = template_parameter->entry->entity_specs.template_parameter_nesting;
            default_template_argument->position = template_parameter->entry->entity_specs.template_parameter_position;

            switch (template_parameter->kind)
            {
                case TPK_TYPE:
                    {
                        default_template_argument->kind = TAK_TYPE;
                        default_template_argument->type = update_type_for_instantiation(
                                template_parameter->default_template_argument->type,
                                context_of_being_instantiated,
                                filename, line);

                        break;
                    }
                case TPK_TEMPLATE:
                    {
                        default_template_argument->kind = TAK_TEMPLATE;
                        default_template_argument->type = update_type_for_instantiation(
                                template_parameter->default_template_argument->type,
                                context_of_being_instantiated,
                                filename, line);
                        break;
                    }
                case TPK_NONTYPE:
                    {
                        default_template_argument->kind = TAK_NONTYPE;
                        default_template_argument->type = update_type_for_instantiation(
                                template_parameter->default_template_argument->type,
                                context_of_being_instantiated,
                                filename, line);
                        default_template_argument->expression = template_parameter->default_template_argument->expression;
                        default_template_argument->expression_context = context_of_being_instantiated;
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                        break;
                    }
            }
        }

        P_LIST_ADD(update_template_parameters->template_parameters,
                update_template_parameters->num_template_parameters,
                updated_template_parameter);
    }

    type_t* base_type = NULL;
    
    if (is_class)
    {
        base_type = member_of_template->type_information;
    }
    else
    {
        base_type = update_type_for_instantiation(
                            member_of_template->type_information,
                            context_of_being_instantiated,
                            filename, line);
    }

    new_member->kind = SK_TEMPLATE;
    new_member->type_information = 
        get_new_template_type(update_template_parameters,
                base_type,
                new_member->symbol_name,
                context_of_being_instantiated,
                member_of_template->line,
                member_of_template->file);

    new_member->file = member_of_template->file;
    new_member->line = member_of_template->line;

    type_map_t new_map;
    new_map.orig_type = template_type;
    new_map.new_type = new_member->type_information;

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Adding new template to template map\n");
    }

    P_LIST_ADD((*template_map), (*num_items_template_map), new_map);

    template_type_set_related_symbol(new_member->type_information, new_member);

    type_t* new_primary_template = template_type_get_primary_type(new_member->type_information);

    named_type_get_symbol(new_primary_template)->decl_context = context_of_being_instantiated;

    named_type_get_symbol(new_primary_template)->entity_specs = 
        named_type_get_symbol(
                template_type_get_primary_type(
                    template_specialized_type_get_related_template_type(member_of_template->type_information)))->entity_specs;

    named_type_get_symbol(new_primary_template)->entity_specs.is_user_declared = 1;

    named_type_get_symbol(new_primary_template)->entity_specs.class_type = being_instantiated;

    class_type_add_member(
            get_actual_class_type(being_instantiated),
            named_type_get_symbol(new_primary_template));

    if (is_class)
    {
        // Fix some bits inherited from the original class type
        class_type_set_enclosing_class_type(get_actual_class_type(new_primary_template),
                get_actual_class_type(being_instantiated));

        class_type_add_typename(
                get_actual_class_type(being_instantiated),
                named_type_get_symbol(new_primary_template));
    }

    return new_member;
}

static void instantiate_member(type_t* selected_template UNUSED_PARAMETER, 
        type_t* being_instantiated, 
        scope_entry_t* member_of_template, 
        decl_context_t context_of_being_instantiated,
        const char* filename, int line,
        type_map_t** template_map, 
        int *num_items_template_map,
        type_map_t** enum_map,
        int *num_items_enum_map
        )
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating member '%s' at '%s:%d'\n", 
                member_of_template->symbol_name,
                member_of_template->file,
                member_of_template->line);
    }

    switch (member_of_template->kind)
    {
        case SK_VARIABLE:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        member_of_template);

                new_member->type_information = update_type_for_instantiation(
                        new_member->type_information,
                        context_of_being_instantiated,
                        filename, line);

                if (is_named_class_type(new_member->type_information))
                {
                    if (class_type_is_incomplete_independent(get_actual_class_type(new_member->type_information)))
                    {
                        scope_entry_t* class_entry = named_type_get_symbol(new_member->type_information);
                        instantiate_template_class(class_entry, context_of_being_instantiated, filename, line);
                    }
                }

                if (new_member->entity_specs.is_bitfield)
                {
                    // Evaluate the bitfield expression
                    if (expression_is_constant(new_member->entity_specs.bitfield_expr))
                    {
                        if (const_value_is_zero(
                                    const_value_gt(
                                        expression_get_constant(new_member->entity_specs.bitfield_expr),
                                        const_value_get_zero(/* bytes*/ 4, /* sign */ 1))))
                        {
                            running_error("%s:%d: error: invalid bitfield of size '%d'",
                                    new_member->file, new_member->line, 
                                    const_value_cast_to_4(
                                        expression_get_constant(new_member->entity_specs.bitfield_expr)));
                        }

                        new_member->entity_specs.bitfield_expr = const_value_to_tree(
                                expression_get_constant(new_member->entity_specs.bitfield_expr));
                        new_member->entity_specs.bitfield_expr_context =
                            context_of_being_instantiated;
                    }
                    else
                    {
                        running_error("%s:%d: error: bitfield specification is not a constant expression", 
                                new_member->file, new_member->line);
                    }
                }

                if (new_member->entity_specs.is_static)
                {
                    class_type_add_static_data_member(get_actual_class_type(being_instantiated), new_member);
                }
                else
                {
                    class_type_add_nonstatic_data_member(get_actual_class_type(being_instantiated), new_member);
                }

                if (member_of_template->language_dependent_value != NULL)
                {
                    new_member->language_dependent_value = ast_copy_for_instantiation(member_of_template->language_dependent_value);

                    check_initialization(new_member->language_dependent_value, context_of_being_instantiated, 
                            new_member->type_information);
                    new_member->value = expression_get_nodecl(new_member->language_dependent_value);
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: Member '%s' is a %s data member with type '%s'\n", 
                            new_member->symbol_name,
                            new_member->entity_specs.is_static ? "static" : "non-static",
                            print_type_str(new_member->type_information, context_of_being_instantiated));
                }
                break;
            }
        case SK_TYPEDEF:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        member_of_template);

                new_member->type_information = update_type_for_instantiation(
                        new_member->type_information,
                        context_of_being_instantiated,
                        filename, line);
                class_type_add_typename(get_actual_class_type(being_instantiated), new_member);

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: Member '%s' is a typedef. Instantiated type is '%s'\n",
                            new_member->symbol_name,
                            print_type_str(new_member->type_information, context_of_being_instantiated));
                }

                break;
            }
        case SK_ENUM:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        member_of_template);

                new_member->type_information = get_new_enum_type(context_of_being_instantiated);

                class_type_add_typename(get_actual_class_type(being_instantiated), new_member);

                // Register a map

                type_map_t new_map;
                new_map.orig_type = member_of_template->type_information;
                new_map.new_type = new_member->type_information;

                P_LIST_ADD((*enum_map), (*num_items_enum_map), new_map);

                break;
            }
        case SK_ENUMERATOR:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        member_of_template);

                type_t* new_type = NULL;
                // Lookup of related enum type
                int i;
                for (i = 0; i < (*num_items_enum_map); i++)
                {
                    if ((*enum_map)[i].orig_type == get_actual_enum_type(member_of_template->type_information))
                    {
                        new_type = (*enum_map)[i].new_type;
                        break;
                    }
                }

                // For named enums, the enum symbol should appear before in the class
                ERROR_CONDITION (new_type == NULL
                        && is_named_enumerated_type(member_of_template->type_information),
                        "Enum new type not found", 0);

                if (new_type == NULL)
                {
                    // Sign it now if is an unnamed enum
                    new_type = get_new_enum_type(context_of_being_instantiated);

                    type_map_t new_map;
                    new_map.orig_type = member_of_template->type_information;
                    new_map.new_type = new_type;

                    P_LIST_ADD((*enum_map), (*num_items_enum_map), new_map);
                }

                member_of_template->type_information = new_type;

                ERROR_CONDITION(member_of_template->language_dependent_value == NULL,
                        "An enumerator always has a related expression", 0);

                new_member->language_dependent_value = ast_copy_for_instantiation(member_of_template->language_dependent_value);

                check_expression(new_member->language_dependent_value, context_of_being_instantiated);

                new_member->value = expression_get_nodecl(new_member->language_dependent_value);

                enum_type_add_enumerator(new_type, new_member);

                break;
            }
        case SK_CLASS:
            {
                if (!is_template_specialized_type(member_of_template->type_information))
                {
                    scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                            being_instantiated,
                            member_of_template);

                    template_parameter_list_t* tpl_empty = calloc(1, sizeof(*tpl_empty));

                    type_t* template_type = get_new_template_type(tpl_empty, 
                            member_of_template->type_information, 
                            new_member->symbol_name, 
                            context_of_being_instantiated, 
                            new_member->line, 
                            new_member->file);

                    scope_entry_t* primary_template = named_type_get_symbol(template_type_get_primary_type(template_type));
                    primary_template->entity_specs.is_user_declared = 1;

                    type_t* primary_specialization = primary_template->type_information;

                    // Fix some bits inherited from the original class type
                    class_type_set_enclosing_class_type(get_actual_class_type(primary_specialization),
                            get_actual_class_type(being_instantiated));

                    set_is_complete_type(primary_specialization, /* is_complete */ 1);

                    template_argument_list_t *tpl_arg_empty = calloc(1, sizeof(*tpl_arg_empty));

                    // FIXME - Update class type internal class info -> trees, at least
                    new_member->type_information = 
                        named_type_get_symbol(
                                template_type_get_specialized_type(template_type,
                                    tpl_arg_empty,
                                    tpl_empty,
                                    context_of_being_instantiated,
                                    new_member->line, new_member->file))->type_information;

                    AST orig_bases_tree, orig_body_tree;
                    class_type_get_instantiation_trees(member_of_template->type_information,
                            &orig_body_tree, &orig_bases_tree);

                    class_type_set_instantiation_trees(get_actual_class_type(new_member->type_information),
                            orig_body_tree, orig_bases_tree);

                    set_is_complete_type(new_member->type_information, /* is_complete */ 0);
                    set_is_dependent_type(new_member->type_information, /* is_dependent */ 0);

                    class_type_add_typename(get_actual_class_type(being_instantiated), new_member);
                }
                else
                {
                    type_t* template_type = template_specialized_type_get_related_template_type(member_of_template->type_information);
                    type_t* primary_template = template_type_get_primary_type(template_type);

                    if (named_type_get_symbol(primary_template)->type_information == member_of_template->type_information)
                    {
                        instantiate_template_type_member(template_type,
                                context_of_being_instantiated,
                                member_of_template,
                                being_instantiated, 
                                /* is_class */ 1,
                                filename, 
                                line,
                                template_map, 
                                num_items_template_map);
                    }
                    else
                    {
                        type_t* new_template_type = NULL;
                        // Search in the map
                        int i;
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "INSTANTIATION: Searching in template map (num_elems = %d)\n",
                                    *num_items_template_map);
                        }

                        for (i = 0; i < *num_items_template_map; i++)
                        {
                            if ((*template_map)[i].orig_type == template_type)
                            {
                                new_template_type = (*template_map)[i].new_type;
                                break;
                            }
                        }

                        ERROR_CONDITION(new_template_type == NULL, "Template type in instantiated class not found", 0);

                        // We are not using update_type because it would reask
                        // a new specialization to the wrong template type
                        template_argument_list_t *template_args =
                            template_specialized_type_get_template_arguments(member_of_template->type_information);

                        template_parameter_list_t *template_params =
                            template_specialized_type_get_template_parameters(member_of_template->type_information);

                        template_argument_list_t* new_template_args = calloc(1, sizeof(*new_template_args));

                        for  (i = 0; i < template_args->num_arguments; i++)
                        {
                            template_argument_t *template_arg = template_args->argument_list[i];

                            template_argument_t *new_template_arg = calloc(1, sizeof(*new_template_arg));

                            new_template_arg->kind = template_arg->kind;

                            switch (template_arg->kind)
                            {
                                case TAK_TYPE:
                                case TAK_TEMPLATE:
                                    {
                                        new_template_arg->type = update_type_for_instantiation(template_arg->type,
                                                context_of_being_instantiated, 
                                                filename, line);
                                        break;
                                    }
                                case TAK_NONTYPE:
                                    {
                                        new_template_arg->type = update_type_for_instantiation(template_arg->type,
                                                context_of_being_instantiated,
                                                filename, line);

                                        new_template_arg->expression = template_arg->expression;

                                        if (expression_is_constant(template_arg->expression))
                                        {
                                            new_template_arg->expression = 
                                                const_value_to_tree(expression_get_constant(template_arg->expression));
                                        }

                                        new_template_arg->expression_context = context_of_being_instantiated;
                                        break;
                                    }
                                default:
                                    {
                                        break;
                                    }
                            }

                            new_template_arg->position = template_arg->position;
                            new_template_arg->nesting = template_arg->nesting;

                            P_LIST_ADD(new_template_args->argument_list, new_template_args->num_arguments, new_template_arg);
                        }

                        // Now ask a new specialization
                        type_t* new_template_specialized_type = template_type_get_specialized_type_after_type(new_template_type,
                                new_template_args,
                                template_params,
                                member_of_template->type_information,
                                context_of_being_instantiated,
                                member_of_template->line, 
                                member_of_template->file);

                        named_type_get_symbol(new_template_specialized_type)->entity_specs.is_user_declared = 1;

                        class_type_add_member(
                                get_actual_class_type(being_instantiated),
                                named_type_get_symbol(new_template_specialized_type));

                        class_type_add_typename(
                                get_actual_class_type(being_instantiated),
                                named_type_get_symbol(new_template_specialized_type));
                    }
                }
                break;
            }
        case SK_TEMPLATE:
            {
                internal_error("Code unreachable\n", 0);
                break;
            }
        case SK_FUNCTION:
            {
                scope_entry_t* new_member = NULL;
                if (!is_template_specialized_type(member_of_template->type_information))
                {
                    new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                            being_instantiated,
                            member_of_template);

                    // FIXME - Maybe we should create also a 0-template like in classes?
                    new_member->type_information = update_type_for_instantiation(
                            new_member->type_information,
                            context_of_being_instantiated,
                            filename, line);

                    class_type_add_member_function(get_actual_class_type(being_instantiated), new_member);
                }
                else
                {
                    type_t* template_type = template_specialized_type_get_related_template_type(member_of_template->type_information);
                    type_t* primary_template = template_type_get_primary_type(template_type);

                    if (named_type_get_symbol(primary_template)->type_information != member_of_template->type_information)
                    {
                        internal_error("Code unreachable\n", 0);
                    }

                    new_member = instantiate_template_type_member(template_type,
                            context_of_being_instantiated,
                            member_of_template,
                            being_instantiated, 
                            /* is_class */ 0,
                            filename, 
                            line,
                            template_map, 
                            num_items_template_map);

                    new_member->defined = 0;

                    // We work on the primary template
                    type_t* primary_type = template_type_get_primary_type(new_member->type_information);
                    new_member = named_type_get_symbol(primary_type);
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: New member function '%s'\n",
                            print_decl_type_str(new_member->type_information, 
                                context_of_being_instantiated, 
                                get_qualified_symbol_name(new_member, 
                                    context_of_being_instantiated)));
                }

                // Functions are not defined yet
                new_member->defined = 0;

                if (member_of_template->entity_specs.is_constructor)
                {
                    class_type_add_constructor(get_actual_class_type(being_instantiated), new_member);

                    if (member_of_template->entity_specs.is_default_constructor)
                    {
                        class_type_set_default_constructor(get_actual_class_type(being_instantiated), new_member);
                    }

                    if (member_of_template->entity_specs.is_copy_constructor)
                    {
                        class_type_add_copy_constructor(get_actual_class_type(being_instantiated), new_member);
                    }

                    CXX1X_LANGUAGE()
                    {
                        if (member_of_template->entity_specs.is_move_constructor)
                        {
                            class_type_add_move_constructor(get_actual_class_type(being_instantiated), new_member);
                        }
                    }
                }
                if (member_of_template->entity_specs.is_destructor)
                {
                    class_type_set_destructor(get_actual_class_type(being_instantiated), new_member);
                }
                if (member_of_template->entity_specs.is_conversion)
                {
                    class_type_add_conversion_function(get_actual_class_type(being_instantiated), new_member);
                }
                if (member_of_template->entity_specs.is_copy_assignment_operator)
                {
                    class_type_add_copy_assignment_operator(get_actual_class_type(being_instantiated), new_member);
                }
                CXX1X_LANGUAGE()
                {
                    if (member_of_template->entity_specs.is_move_assignment_operator)
                    {
                        class_type_add_move_assignment_operator(get_actual_class_type(being_instantiated), new_member);
                    }
                }
                if (member_of_template->entity_specs.is_virtual)
                {
                    class_type_add_virtual_function(get_actual_class_type(being_instantiated), new_member);
                }

                break;
            }
            // This is only possible because of using declarations / or qualified members
            // which refer to dependent entities
        case SK_DEPENDENT_ENTITY:
            {
                ERROR_CONDITION(member_of_template->language_dependent_value == NULL,
                        "Invalid expression for dependent entity", 0);

                scope_entry_list_t *entry_list = query_id_expression(context_of_being_instantiated, member_of_template->language_dependent_value);
                
                if (entry_list == NULL
                        || !entry_list_head(entry_list)->entity_specs.is_member)
                {
                    running_error("%s: invalid using declaration '%s' while instantiating\n", 
                            ast_location(member_of_template->language_dependent_value),
                            prettyprint_in_buffer(member_of_template->language_dependent_value));
                }

                scope_entry_t* entry = entry_list_head(entry_list);
                if (!class_type_is_base(entry->entity_specs.class_type, 
                            get_actual_class_type(being_instantiated)))
                {
                    running_error("%s: entity '%s' is not a member of a base of class '%s'\n",
                            ast_location(member_of_template->language_dependent_value),
                                get_qualified_symbol_name(entry,
                                    context_of_being_instantiated),
                                get_qualified_symbol_name(named_type_get_symbol(being_instantiated), 
                                    context_of_being_instantiated)
                            );
                }

                scope_entry_list_iterator_t* it = NULL;
                for (it = entry_list_iterator_begin(entry_list);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    entry = entry_list_iterator_current(it);
                    class_type_add_member(get_actual_class_type(being_instantiated), entry);

                    // Insert the symbol in the context
                    insert_entry(context_of_being_instantiated.current_scope, entry);

                    if (entry->kind == SK_FUNCTION
                            && entry->entity_specs.is_conversion)
                    {
                        class_type_add_conversion_function(get_actual_class_type(being_instantiated), entry);
                    }
                }
                entry_list_iterator_free(it);
                entry_list_free(entry_list);

                break;
            }
        default:
            {
                internal_error("Unexpected member kind=%s\n", symbol_kind_name(member_of_template));
            }
    }
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        decl_context_t context_of_being_instantiated,
        const char* filename, int line);

// Using typesystem
static void instantiate_specialized_template_class(type_t* selected_template,
        type_t* being_instantiated,
        deduction_set_t* unification_set,
        const char *filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating class '%s'\n", 
                print_declarator(being_instantiated));
    }

    ERROR_CONDITION(!is_named_class_type(being_instantiated), "Must be a named class", 0);

    scope_entry_t* named_class = named_type_get_symbol(being_instantiated);

    AST instantiation_body = NULL;
    AST instantiation_base_clause = NULL;
    class_type_get_instantiation_trees(get_actual_class_type(selected_template), 
            &instantiation_body, &instantiation_base_clause);

    // template_parameter_list_t* selected_template_parameters 
    //     = template_specialized_type_get_template_parameters(get_actual_class_type(selected_template));

    instantiation_body = ast_copy_for_instantiation(instantiation_body);
    instantiation_base_clause = ast_copy_for_instantiation(instantiation_base_clause);

    decl_context_t template_parameters_context = new_template_context(named_class->decl_context);
    // Clear the template nesting level, if we are instantiating it is conceptually 0
    template_parameters_context.decl_flags &= ~DF_TEMPLATE;
    template_parameters_context.decl_flags &= ~DF_EXPLICIT_SPECIALIZATION;
    template_parameters_context.template_nesting = 0;
    // Empty template parameters
    template_parameters_context.template_parameters = calloc(1, sizeof(*template_parameters_context.template_parameters));

    decl_context_t inner_decl_context = new_class_context(template_parameters_context, 
            named_class);

    class_type_set_inner_context(named_class->type_information, inner_decl_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Injecting template parameters\n");
    }

    int i;
    for (i = 0; i < unification_set->num_deductions; i++)
    {
        deduction_t* current_deduction = unification_set->deduction_list[i];

        ERROR_CONDITION(current_deduction->num_deduced_parameters != 1,
                "Number of deduced parameters is not 1!", 0);

        int j;
        for (j = 0; j < current_deduction->num_deduced_parameters; j++)
        {
            // const char* deduced_parameter_name = get_name_of_template_parameter(selected_template_parameters,
            //         current_deduction->parameter_nesting,
            //         current_deduction->parameter_position);

            char tpl_param_name[256] = { 0 };

            snprintf(tpl_param_name, 255, ".tpl_%d_%d",
                    current_deduction->parameter_nesting,
                    current_deduction->parameter_position);
            scope_entry_t* param_symbol = new_symbol(template_parameters_context,
                    template_parameters_context.template_scope, tpl_param_name);

            switch (current_deduction->kind)
            {
                case TPK_TYPE :
                    {
                        // We use a typedef
                        param_symbol->kind = SK_TYPEDEF;
                        param_symbol->entity_specs.is_template_argument = 1;
                        param_symbol->type_information = current_deduction->deduced_parameters[0]->type;

                        break;
                    }
                case TPK_TEMPLATE :
                    {
                        // The template type has to be used here
                        param_symbol->kind = SK_TEMPLATE;
                        param_symbol->entity_specs.is_template_argument = 1;
                        // These are always kept as named types in the compiler
                        param_symbol->type_information = 
                            named_type_get_symbol(current_deduction->deduced_parameters[0]->type)->type_information;

                        break;
                    }
                case TPK_NONTYPE :
                    {
                        param_symbol->kind = SK_VARIABLE;
                        param_symbol->entity_specs.is_template_argument = 1;
                        param_symbol->type_information = current_deduction->deduced_parameters[0]->type;

                        if (expression_is_constant(current_deduction->deduced_parameters[0]->expression))
                        {
                            param_symbol->language_dependent_value = 
                                const_value_to_tree(
                                        expression_get_constant(current_deduction->deduced_parameters[0]->expression));
                            param_symbol->value = const_value_to_nodecl(
                                    expression_get_constant(param_symbol->language_dependent_value));
                        }
                        else
                        {
                            param_symbol->language_dependent_value = current_deduction->deduced_parameters[0]->expression;
                            param_symbol->value = expression_get_nodecl(param_symbol->language_dependent_value);
                        }
                        break;
                    }
                default:
                    {
                        internal_error("Invalid parameter kind", 0);
                    }
            }
        }

    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Template parameters injected\n");
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Injected context\n");
        print_scope(inner_decl_context);
        fprintf(stderr, "INSTANTIATION: End of injected context \n");
    }

    if (instantiation_base_clause != NULL)
    {
        instantiate_bases(
                get_actual_class_type(selected_template),
                get_actual_class_type(being_instantiated),
                inner_decl_context,
                filename, line
                );
    }
    
    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(inner_decl_context, 
            inner_decl_context.current_scope, named_class->symbol_name);

    *injected_symbol = *named_class;

    injected_symbol->do_not_print = 1;
    injected_symbol->entity_specs.is_injected_class_name = 1;
    injected_symbol->entity_specs.injected_class_referred_symbol = named_class;

    /*
     * Note that the standard allows code like this one
     *
     * template <typename _T>
     * struct A { };
     *
     * template <>
     * struct A<int> 
     * {
     *   typedef int K;  (1)
     *   A<int>::K k;    (2)
     * };
     *
     * So we can use the name 'A<int>::K' inside the class provided 'K' has been declared
     * before the point we refer it (so: switching declarations (1) and (2) would not work).
     *
     * This also affects the injected class-name, so, for another example
     *
     * template <typename _T>
     * struct B
     * {
     *   typedef _T P;             (3)
     *   typename B::P k;          (4)
     * };
     *
     * Note that in a partial, or not at all, specialized class the injected
     * class name is dependent so 'typename' is mandatory (like in (4)). It is
     * redundant since the injected-class name obviously refers to the current
     * class so no ambiguity would arise between identifiers and typenames.
     * Seems that a DR has been filled on that.
     *
     * All this explanation is here just to justify that the class is already
     * complete and independent just before start the parsing of its members.
     * Otherwise all the machinery would try to instantiate it again and again
     * (and this is not good at all).
     */
    set_is_complete_type(being_instantiated, /* is_complete */ 1);
    set_is_dependent_type(being_instantiated, /* is_dependent */ 0);

    set_is_complete_type(get_actual_class_type(being_instantiated), /* is_complete */ 1);
    set_is_dependent_type(get_actual_class_type(being_instantiated), /* is_dependent */ 0);

    int num_members = class_type_get_num_members(get_actual_class_type(selected_template));

    type_map_t* template_map = NULL;
    int num_items_template_map = 0;

    type_map_t* enum_map = NULL;
    int num_items_enum_map = 0;

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Have to instantiate %d members\n", num_members);
    }
    for (i = 0; i < num_members; i++)
    {
        scope_entry_t* member = class_type_get_member_num(get_actual_class_type(selected_template), i);

        instantiate_member(selected_template, 
                being_instantiated, 
                member, 
                inner_decl_context,
                filename, line,
                &template_map, &num_items_template_map,
                &enum_map, &num_items_enum_map);
    }

    // The symbol is defined after this
    named_class->defined = 1;

    // Finish the class
    finish_class_type(get_actual_class_type(being_instantiated), being_instantiated, 
            named_class->decl_context, filename, line);

    if (CURRENT_CONFIGURATION->explicit_instantiation)
    {
        // Caution this is experimental code not intended for production
        // Caution 2, at the moment just print to stdout to see we are not going nuts with the tree

        AST orig_definition_tree = named_type_get_symbol(selected_template)->entity_specs.definition_tree;

        fprintf(stderr, "============== ORIGINAL DEFINITION TREE of '%s' =======================\n",
                print_type_str(selected_template, inner_decl_context));
        fprintf(stderr, "%s\n", prettyprint_in_buffer(orig_definition_tree));
        fprintf(stderr, "============== INSTANTIATED DEFINITION TREE of '%s' ===================\n",
                print_type_str(being_instantiated, inner_decl_context));
        AST instantiated_definition_tree 
            = instantiate_tree(orig_definition_tree, inner_decl_context);
        fprintf(stderr, "%s\n", prettyprint_in_buffer(instantiated_definition_tree));
        fprintf(stderr, "===============================================================\n");
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: End of instantiation of class '%s'\n", 
                print_declarator(being_instantiated));
    }
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        decl_context_t context_of_being_instantiated,
        const char* filename, int line)
{
    int i, num_bases = class_type_get_num_bases(selected_class_type);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Updating bases\n");
    }

    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class_sym = class_type_get_base_num(selected_class_type, i, &is_virtual, 
                &is_dependent, &access_specifier);

        type_t* base_class_named_type = NULL;
        if (base_class_sym->kind == SK_DEPENDENT_ENTITY)
        {
            base_class_named_type = base_class_sym->type_information;
        }
        else
        {
            base_class_named_type = get_user_defined_type(base_class_sym);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Updating base class '%s'\n", 
                    print_declarator(base_class_named_type));
        }

        type_t* upd_base_class_named_type = update_type_for_instantiation(base_class_named_type,
                context_of_being_instantiated,
                filename, line);

        scope_entry_t* upd_base_class_sym = named_type_get_symbol(upd_base_class_named_type);

        // If the entity (being an independent one) has not been completed, then instantiate it
        if (class_type_is_incomplete_independent(get_actual_class_type(upd_base_class_named_type)))
        {
            instantiate_template_class(upd_base_class_sym, context_of_being_instantiated, filename, line);
        }

        class_type_add_base_class(instantiated_class_type, upd_base_class_sym, is_virtual, /* is_dependent */ 0, access_specifier);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Finished updating bases\n");
    }
}

void instantiate_template_class(scope_entry_t* entry, decl_context_t decl_context, const char *filename, int line)
{
    if (entry->kind != SK_CLASS
            && entry->kind != SK_TYPEDEF)
    {
        internal_error("Invalid symbol\n", 0);
    }

    if (entry->kind == SK_TYPEDEF)
    {
        entry = named_type_get_symbol(advance_over_typedefs(entry->type_information));
    }

    type_t* template_specialized_type = entry->type_information;

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating class '%s' at '%s:%d'\n",
                print_type_str(get_user_defined_type(entry), entry->decl_context),
                entry->file,
                entry->line);
    }


    if (!is_template_specialized_type(template_specialized_type)
            || !is_class_type(template_specialized_type)
            || !class_type_is_incomplete_independent(template_specialized_type))
    {
        internal_error("Symbol '%s' is not a class eligible for instantiation", entry->symbol_name);
    }

    type_t* template_type = 
        template_specialized_type_get_related_template_type(template_specialized_type);

    deduction_set_t* unification_set = NULL;

    type_t* selected_template = solve_class_template(decl_context,
            template_type, 
            get_user_defined_type(entry),
            &unification_set, filename, line);

    if (selected_template != NULL)
    {
        if (is_incomplete_type(selected_template))
        {
            running_error("%s:%d: instantiation of '%s' is not possible at this point since its most specialized template '%s' is incomplete\n", 
                    filename, line, 
                    print_type_str(get_user_defined_type(entry), decl_context),
                    print_type_str(selected_template, decl_context));
        }

        instantiate_specialized_template_class(selected_template, 
                get_user_defined_type(entry),
                unification_set, filename, line);
    }
    else
    {
        running_error("%s:%d: instantiation of '%s' is not possible at this point\n", 
                filename, line, print_type_str(get_user_defined_type(entry), decl_context));
    }
}

#if 0
void instantiate_template_function(scope_entry_t* entry, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        const char* filename UNUSED_PARAMETER, 
        int line UNUSED_PARAMETER)
{
    // Do nothing if we are checking for ambiguities as this may cause havoc
    if (checking_ambiguity())
        return;

    if (entry->kind != SK_FUNCTION)
    {
        internal_error("Invalid symbol\n", 0);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating function '%s' with type '%s' at '%s:%d\n",
                entry->symbol_name,
                print_declarator(entry->type_information),
                entry->file,
                entry->line);
    }

    type_t* template_specialized_type = entry->type_information;

    if (!is_template_specialized_type(template_specialized_type)
            || !is_function_type(template_specialized_type))
    {
        internal_error("Symbol '%s' is not a template function eligible for instantiation", entry->symbol_name);
    }

    type_t* template_type = template_specialized_type_get_related_template_type(template_specialized_type);
    scope_entry_t* template_symbol = template_type_get_related_symbol(template_type);

    // The primary specialization is a named type, even if the named type is a function!
    type_t* primary_specialization_type = template_type_get_primary_type(template_symbol->type_information);
    scope_entry_t* primary_specialization_function = named_type_get_symbol(primary_specialization_type);
    type_t* primary_specialization_function_type = primary_specialization_function->type_information;

    // If the primary specialization is not defined, no instantiation may happen
    if (!primary_specialization_function->defined)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Not instantiating since primary template has not been defined\n");
        }
        return;
    }

    // Do nothing
    if (entry->defined)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Instantiation already performed\n");
        }
        return;
    }

    // Functions are easy. Since they cannot be partially specialized, like
    // classes do, their template parameters always match the computed template
    // arguments. In fact, overload machinery did this part for us

    // Get a new template context where we will sign in the template arguments
    decl_context_t template_parameters_context = new_template_context(entry->decl_context);

    template_parameter_list_t *template_parameters 
        = template_specialized_type_get_template_parameters(template_specialized_type);

    template_argument_list_t *template_arguments
        = template_specialized_type_get_template_arguments(template_specialized_type);

    // FIXME: Does this hold when in C++0x we allow default template parameters in functions?
    ERROR_CONDITION(template_arguments->num_arguments != template_parameters->num_template_parameters,
            "Mismatch between template arguments and parameters! %d != %d\n", 
            template_arguments->num_arguments,
            template_parameters->num_template_parameters);

    // Inject template arguments
    int i;
    for (i = 0; i < template_parameters->num_template_parameters; i++)
    {
        template_parameter_t* template_param = template_parameters->template_parameters[i];
        template_argument_t* template_argument = template_arguments->argument_list[i];

        switch (template_param->kind)
        {
            case TPK_TYPE:
                {
                    ERROR_CONDITION(template_argument->kind != TAK_TYPE,
                            "Mismatch between template argument kind and template parameter kind", 0);

                    scope_entry_t* injected_type = new_symbol(template_parameters_context,
                            template_parameters_context.template_scope, template_param->entry->symbol_name);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Injecting typedef '%s' for type '%s'\n", injected_type->symbol_name, 
                                print_declarator(template_argument->type));
                    }

                    injected_type->kind = SK_TYPEDEF;
                    injected_type->entity_specs.is_template_argument = 1;
                    injected_type->type_information = template_argument->type;
                    break;
                }
            case TPK_TEMPLATE:
                {
                    ERROR_CONDITION(template_argument->kind != TAK_TEMPLATE,
                            "Mismatch between template argument kind and template parameter kind", 0);

                    scope_entry_t* injected_type = new_symbol(template_parameters_context, 
                            template_parameters_context.template_scope, template_param->entry->symbol_name);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Injecting template name '%s'\n", injected_type->symbol_name);
                    }

                    injected_type->kind = SK_TEMPLATE;
                    injected_type->entity_specs.is_template_argument = 1;
                    injected_type->type_information = 
                        named_type_get_symbol(template_argument->type)->type_information;
                    break;
                }
            case TPK_NONTYPE:
                {
                    ERROR_CONDITION(template_argument->kind != TAK_NONTYPE,
                            "Mismatch between template argument kind and template parameter kind", 0);

                    scope_entry_t* injected_nontype = new_symbol(template_parameters_context, 
                            template_parameters_context.template_scope, template_param->entry->symbol_name);

                    injected_nontype->kind = SK_VARIABLE;
                    injected_nontype->entity_specs.is_template_argument = 1;
                    injected_nontype->type_information = template_argument->type;

                    injected_nontype->expression_value =
                        const_value_to_tree(
                                expression_get_constant(template_argument->expression));

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Injecting parameter '%s' with expression '%s'\n", injected_nontype->symbol_name, 
                                prettyprint_in_buffer(injected_nontype->expression_value));
                    }
                    break;
                }
            default:
                internal_error("Invalid template parameter kind %s\n", symbol_kind_name(template_param->kind));
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Getting tree of function '%s' || %s\n", entry->symbol_name, print_declarator(primary_specialization_function_type));
    }

    AST orig_function_definition = function_type_get_function_definition_tree(primary_specialization_function_type);

    ERROR_CONDITION(orig_function_definition == NULL,
            "Invalid function definition tree!", 0);

    // Remove dependent types
    AST dupl_function_definition = ast_copy_for_instantiation(orig_function_definition);

    template_parameters_context.decl_flags |= DF_TEMPLATE;
    template_parameters_context.decl_flags |= DF_EXPLICIT_SPECIALIZATION;

    // Temporarily disable ambiguity testing
    char old_test_status = get_test_expression_status();
    set_test_expression_status(0);

    build_scope_function_definition(dupl_function_definition,
            template_parameters_context);

    set_test_expression_status(old_test_status);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: ended instantation of function template '%s'\n",
                print_declarator(template_specialized_type));
    }
}
#endif

static AST get_id_expression_for_entry(scope_entry_t* entry, decl_context_t decl_context,
        const char* filename, int line);
static void get_type_id_tree_of_type_split(type_t* t, decl_context_t decl_context, 
        AST *type_specifier_seq, 
        AST *abstract_decl, 
        const char* filename, int line);

static AST ast_append_to_list(AST list, AST new_item)
{
    if (list == NULL)
        return ast_list_leaf(new_item);
    else
        return ast_list(list, new_item);
}

static AST get_type_specifier_seq_of_type(type_t* t, decl_context_t decl_context UNUSED_PARAMETER, const char* filename, int line)
{
    t = get_foundation_type(t);

    AST type_specifier = NULL;
    AST nontype_specifier_seq = NULL;

    if (is_any_int_type(t))
    {
        type_specifier = ASTLeaf(AST_INT_TYPE, filename, line, NULL);

        if (is_unsigned_int_type(t)
                || is_unsigned_long_int_type(t)
                || is_unsigned_long_long_int_type(t))
        {
            nontype_specifier_seq = 
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_UNSIGNED_TYPE, filename, line, NULL));
        }
        if (is_signed_long_int_type(t)
                || is_unsigned_long_int_type(t))
        {
            nontype_specifier_seq = 
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_LONG_TYPE, filename, line, NULL));
        }
        if (is_signed_long_long_int_type(t)
                || is_unsigned_long_long_int_type(t))
        {
            nontype_specifier_seq = 
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_LONG_TYPE, filename, line, NULL));
            nontype_specifier_seq = 
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_LONG_TYPE, filename, line, NULL));
        }
    }
    else if (is_floating_type(t))
    {
        type_specifier = ASTLeaf(AST_FLOAT_TYPE, filename, line, NULL);
    }
    else if (is_double_type(t) 
            || is_long_double_type(t))
    {
        type_specifier = ASTLeaf(AST_DOUBLE_TYPE, filename, line, NULL);

        if (is_long_double_type(t))
        {
            nontype_specifier_seq = 
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_LONG_TYPE, filename, line, NULL));
        }
    }
    else if (is_char_type(t)
            || is_unsigned_char_type(t)
            || is_signed_char_type(t))
    {
        type_specifier = ASTLeaf(AST_CHAR_TYPE, filename, line, NULL);

        if (is_unsigned_char_type(t))
        {
            nontype_specifier_seq =
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_UNSIGNED_TYPE, filename, line, NULL));
        }
        else
        {
            nontype_specifier_seq =
                ast_append_to_list(nontype_specifier_seq,
                        ASTLeaf(AST_SIGNED_TYPE, filename, line, NULL));
        }
    }
    else if (is_void_type(t))
    {
        type_specifier = ASTLeaf(AST_VOID_TYPE, filename, line, NULL);
    }
    else if (is_bool_type(t))
    {
        type_specifier = ASTLeaf(AST_BOOL_TYPE, filename, line, NULL);
    }
    else if (is_named_enumerated_type(t)
            || is_named_class_type(t))
    {
        scope_entry_t* entry = named_type_get_symbol(t);
        type_specifier = 
            ASTMake1(AST_SIMPLE_TYPE_SPEC, 
                    get_id_expression_for_entry(entry, decl_context, filename, line),
                    filename, line, NULL);
    }
    else
    {
        internal_error("Unhandled type", 0);
    }

    if (is_const_qualified_type(t))
    {
        nontype_specifier_seq =
            ast_append_to_list(nontype_specifier_seq, 
                    ASTLeaf(AST_CONST_SPEC, filename, line, "const"));
    }
    if (is_volatile_qualified_type(t))
    {
        nontype_specifier_seq =
            ast_append_to_list(nontype_specifier_seq, 
                    ASTLeaf(AST_VOLATILE_SPEC, filename, line, "volatile"));
    }
    if (is_restrict_qualified_type(t))
    {
        nontype_specifier_seq =
            ast_append_to_list(nontype_specifier_seq, 
                    ASTLeaf(AST_GCC_RESTRICT_SPEC, filename, line, "__restrict"));
    }

    AST type_specifier_seq = ASTMake3(AST_TYPE_SPECIFIER_SEQ, 
            nontype_specifier_seq,
            type_specifier,
            NULL,
            filename, line, NULL);

    return type_specifier_seq;
}

static void get_abstract_declarator(AST *abstract_decl, 
        type_t* t, 
        decl_context_t decl_context,
        const char* filename, int line)
{
    if (is_pointer_type(t)
            || is_pointer_to_member_type(t))
    {
        AST cv_seq = NULL;

        if (is_const_qualified_type(t))
        {
            cv_seq = ast_append_to_list(cv_seq, ASTLeaf(AST_CONST_SPEC, filename, line, NULL));
        }
        if (is_volatile_qualified_type(t))
        {
            cv_seq = ast_append_to_list(cv_seq, ASTLeaf(AST_VOLATILE_SPEC, filename, line, NULL));
        }
        if (is_restrict_qualified_type(t))
        {
            cv_seq = ast_append_to_list(cv_seq, ASTLeaf(AST_GCC_RESTRICT_SPEC, filename, line, NULL));
        }

        AST id_expr = NULL;

        if (is_pointer_to_member_type(t))
        {
            scope_entry_t* class = pointer_to_member_type_get_class(t);
            id_expr = get_id_expression_for_entry(class, decl_context, filename, line);
        }

        AST pointer_spec = ASTMake2(AST_POINTER_SPEC, id_expr, cv_seq, filename, line, NULL);
        *abstract_decl = ASTMake2(AST_POINTER_DECLARATOR,
                pointer_spec, NULL, filename, line, 0);

        type_t* pointee = pointer_type_get_pointee_type(t);

        char requires_parentheses = is_function_type(pointee) || is_array_type(pointee);

        if (requires_parentheses)
        {
            *abstract_decl = ASTMake1(AST_PARENTHESIZED_DECLARATOR,
                    *abstract_decl, filename, line, 0);
        }

        get_abstract_declarator(abstract_decl, pointer_type_get_pointee_type(t), decl_context, filename, line);
    }
    else if (is_array_type(t))
    {
        *abstract_decl = ASTMake2(AST_DECLARATOR_ARRAY, 
                *abstract_decl,
                nodecl_get_ast(array_type_get_array_size_expr(t)), filename, line, 0);

        get_abstract_declarator(abstract_decl, array_type_get_element_type(t), decl_context, filename, line);
    }
    else if (is_function_type(t))
    {
        AST cv_seq = NULL;
        if (is_const_qualified_type(t))
        {
            cv_seq = ast_append_to_list(cv_seq, ASTLeaf(AST_CONST_SPEC, filename, line, "const"));
        }

        AST parameter_decl_clause = NULL;

        char has_ellipsis = function_type_get_has_ellipsis(t);
        int num_parameters = function_type_get_num_parameters(t);
        if (num_parameters == 0)
        {
            parameter_decl_clause = ASTLeaf(AST_EMPTY_PARAMETER_DECLARATION_CLAUSE, filename, line, NULL);
        }
        else
        {
            if (has_ellipsis)
                num_parameters--;

            int i;
            for (i = 0; i < num_parameters; i++)
            {
                type_t* param_type = function_type_get_parameter_type_num(t, i);

                AST param_type_spec = NULL, param_abstr_decl = NULL;

                get_type_id_tree_of_type_split(param_type, decl_context, 
                        &param_type_spec, 
                        &param_abstr_decl, 
                        filename, line);

                AST parameter_decl = ASTMake3(AST_PARAMETER_DECL, 
                        param_type_spec, param_abstr_decl, NULL,
                        filename, line, NULL);

                parameter_decl_clause = ast_append_to_list(parameter_decl_clause,
                        parameter_decl);
            }

            if (has_ellipsis)
            {
                parameter_decl_clause = ast_append_to_list(parameter_decl_clause, 
                        ASTLeaf(AST_VARIADIC_ARG, filename, line, "..."));
            }
        }

        *abstract_decl = ASTMake4(AST_DECLARATOR_FUNC, 
                *abstract_decl, 
                parameter_decl_clause, 
                cv_seq, 
                NULL, 
                filename, line, NULL);

        if (function_type_get_return_type(t) != NULL)
        {
            get_abstract_declarator(abstract_decl, 
                    function_type_get_return_type(t), 
                    decl_context, filename, line);
        }
    }
    // Do nothing
}

static void get_type_id_tree_of_type_split(type_t* t, decl_context_t decl_context, 
        AST *type_specifier_seq, 
        AST *abstract_decl, 
        const char* filename, int line)
{
    *type_specifier_seq = get_type_specifier_seq_of_type(t, decl_context, filename, line);
    get_abstract_declarator(abstract_decl, t, decl_context, filename, line);
}

static AST get_type_id_tree_of_type(type_t* t, decl_context_t decl_context, const char* filename, int line)
{
    AST type_specifier_seq = NULL;
    AST abstract_decl = NULL;

    get_type_id_tree_of_type_split(t, decl_context, 
            &type_specifier_seq,
            &abstract_decl,
            filename, line);

    return ASTMake2(AST_TYPE_ID,
            type_specifier_seq,
            abstract_decl,
            filename, line, NULL);
}


static AST get_template_id_of_entry(scope_entry_t* entry, 
        template_argument_list_t* template_arguments,
        decl_context_t decl_context,
        const char* filename, int line)
{
    AST template_argument_list = NULL;

    int i;
    for (i = 0; i < template_arguments->num_arguments; i++)
    {
        template_argument_t* template_arg = template_arguments->argument_list[i];

        switch (template_arg->kind)
        {
            case TAK_NONTYPE:
                {
                    AST arg = ASTMake1(AST_TEMPLATE_EXPRESSION_ARGUMENT, 
                            ASTMake1(AST_EXPRESSION, template_arg->expression, filename, line, NULL),
                            filename, line, NULL);

                    template_argument_list = ast_append_to_list(template_argument_list, arg);
                    break;
                }
            case TAK_TYPE:
                {
                    AST arg = ASTMake1(AST_TEMPLATE_TYPE_ARGUMENT, 
                            get_type_id_tree_of_type(template_arg->type, decl_context, filename, line),
                            filename, line, NULL);

                    template_argument_list = ast_append_to_list(template_argument_list, arg);
                    break;
                }
            case TAK_TEMPLATE:
                {
                    internal_error("Not yet implemented this case", 0);
                    break;
                }
            default:
                internal_error("Invalid template argument", 0);
        }
    }

    AST template_id = 
        ASTMake2(AST_TEMPLATE_ID, 
                ASTLeaf(AST_SYMBOL, filename, line, entry->symbol_name),
                template_argument_list,
                filename, line, NULL);

    return template_id;
}

static AST get_tree_name_of_class(scope_entry_t* entry, decl_context_t decl_context, 
        const char* filename, int line)
{
    type_t* type_info = entry->type_information;

    if (!is_template_specialized_type(type_info))
    {
        AST name = ASTLeaf(AST_SYMBOL, filename, line, entry->symbol_name);
        return name;
    }
    else
    {
        template_argument_list_t* template_args 
            = template_specialized_type_get_template_arguments(entry->type_information);
        return get_template_id_of_entry(entry, template_args, decl_context, filename, line);
    }
}

static AST get_tree_name_of_symbol(scope_entry_t* entry, decl_context_t decl_context,
        const char* filename, int line)
{
    if (entry->kind == SK_CLASS)
    {
        return get_tree_name_of_class(entry, decl_context, filename, line);
    }
    else if (entry->kind == SK_FUNCTION
            && is_template_specialized_type(entry->type_information))
    {
        return get_template_id_of_entry(entry, 
                template_specialized_type_get_template_arguments(entry->type_information),
                decl_context, filename, line);
    }
    else
    {
        return ASTLeaf(AST_SYMBOL, filename, line, entry->symbol_name);
    }
}

static AST get_id_expression_for_entry(scope_entry_t* entry, decl_context_t decl_context,
        const char* filename, int line)
{
    AST global_scope = ASTLeaf(AST_GLOBAL_SCOPE, filename, line, NULL);

    decl_context = entry->decl_context;

    scope_t* current_scope = decl_context.current_scope;
    scope_t* enclosing_scope = current_scope;

    AST qualification_nest = NULL;

    while (enclosing_scope != NULL
            && enclosing_scope != decl_context.global_scope)
    {
        AST qualif_name = NULL;
        if (enclosing_scope->kind == NAMESPACE_SCOPE)
        {
            qualif_name = ASTLeaf(AST_SYMBOL, filename, line, enclosing_scope->related_entry->symbol_name);
        }
        else if (enclosing_scope->kind == CLASS_SCOPE)
        {
            qualif_name = get_tree_name_of_class(enclosing_scope->related_entry, 
                    decl_context, filename, line);
        }
        else
        {
            internal_error("Invalid namespace kind %d", enclosing_scope->kind);
        }

        qualification_nest = 
            ASTMake2(AST_NESTED_NAME_SPECIFIER, qualif_name, qualification_nest, filename, line, NULL);

        enclosing_scope = enclosing_scope->contained_in;
    }

    AST symbol_name = get_tree_name_of_symbol(entry, decl_context, filename, line);

    AST qualified_id = ASTMake3(AST_QUALIFIED_ID, 
            global_scope, qualification_nest, symbol_name, 
            filename, line, NULL);
    return qualified_id;
}

static AST get_innermost_nonnull_declarator(AST a, int *child_num)
{
    ERROR_CONDITION((a == NULL), "Tree cannot be null here", 0);

    switch(ASTType(a))
    {
        case AST_INIT_DECLARATOR :
        case AST_MEMBER_DECLARATOR :
        case AST_GCC_MEMBER_DECLARATOR :
        case AST_DECLARATOR :
        case AST_PARENTHESIZED_DECLARATOR :
        case AST_DECLARATOR_ARRAY :
        case AST_DECLARATOR_FUNC :
        case AST_GCC_FUNCTIONAL_DECLARATOR :
            {
                *child_num = 0;
                if (ASTSon0(a) != NULL)
                    return get_innermost_nonnull_declarator(ASTSon0(a), child_num); 
                break;
            }
        case AST_POINTER_DECLARATOR :
        case AST_GCC_DECLARATOR :
            {
                *child_num = 1;
                if (ASTSon1(a) != NULL)
                    return get_innermost_nonnull_declarator(ASTSon1(a), child_num);
                break;
            }
        case AST_GCC_POINTER_DECLARATOR :
            {
                *child_num = 2;
                if (ASTSon2(a) != NULL)
                    return get_innermost_nonnull_declarator(ASTSon2(a), child_num);
                break;
            }
        case AST_DECLARATOR_ID_EXPR :
        case AST_AMBIGUITY :
            {
                internal_error("Invalid node here", 0);
                break;
            }
        default:
            {
                internal_error("Unknown node '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }

    return a;
}

static void merge_type_specifier_seq(
        AST declarative_tree, 
        AST new_type_specifier_seq)
{
    // It is always the zeroth child, but check it anyways
    AST orig_type_specifier_seq = ASTSon0(declarative_tree);
    ERROR_CONDITION(ASTType(orig_type_specifier_seq) != AST_TYPE_SPECIFIER_SEQ, 
            "Invalid tree, it is not a type-specifier-seq", 0);

    // Replace the type_specifier
    AST orig_type_spec = ASTSon1(orig_type_specifier_seq);
    AST new_type_spec = ASTSon1(new_type_specifier_seq);

    ast_replace(orig_type_spec, new_type_spec);

    // Enlarge the first nontype specifiers
    AST orig_nontype_spec = ASTSon0(orig_type_specifier_seq);
    AST new_nontype_spec = ASTSon0(new_type_specifier_seq);

    // Later on we will remove repeated cv-qualifiers
    if (orig_nontype_spec != NULL)
    {
        while (ASTSon0(orig_nontype_spec) != NULL)
        {
            orig_nontype_spec = ASTSon0(orig_nontype_spec);
        }

        ast_set_child(orig_nontype_spec, 0, new_nontype_spec);
    }
    else
    {
        ast_set_child(orig_type_specifier_seq, 0, new_nontype_spec);
    }

    // Remove repeated cv-qualifiers (const, volatile, restrict)
    char has_const = 0;
    char has_volatile = 0;
    char has_restrict = 0;

    AST trees[] = { 
        ASTSon0(orig_type_specifier_seq), 
        ASTSon2(orig_type_specifier_seq), 
        NULL };

    int k;
    for (k = 0; k < 2; k++)
    {
        if (trees[k] == NULL)
            continue;

        AST iter;
        for_each_element(trees[k], iter)
        {
            AST item = ASTSon1(iter);

            char* flag = NULL;

            if (ASTType(item) == AST_CONST_SPEC)
                flag = &has_const;

            if (ASTType(item) == AST_VOLATILE_SPEC)
                flag = &has_volatile;

            if (ASTType(item) == AST_GCC_RESTRICT_SPEC)
                flag = &has_restrict;

            if (flag != NULL)
            {
                if (!(*flag))
                {
                    *flag = 1;
                }
                else
                {
                    // Remove it from the list (this is fine during iteration since we
                    // only check the parent of iter)
                    // To remove simply relink the parents children to ours 
                    AST parent = ASTParent(iter);
                    ast_set_child(parent, 0, ASTSon0(item));
                }
            }
        }
    }
}

static void merge_declarators_aux(
        AST old_declarator,
        AST new_abstract_decl)
{
    char old_declarator_is_ptr = (ASTType(old_declarator) == AST_POINTER_DECLARATOR
            || ASTType(old_declarator) == AST_GCC_POINTER_DECLARATOR);

    int child = -1;

    // Chain with my parent
    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (ast_get_child(ASTParent(old_declarator), i) == old_declarator)
        {
            ast_set_child(ASTParent(old_declarator), i, new_abstract_decl);
            break;
        }
    }

    AST innermost_decl = get_innermost_nonnull_declarator(new_abstract_decl, &child);

    char innermost_is_ptr = (ASTType(innermost_decl) == AST_POINTER_DECLARATOR
            || ASTType(innermost_decl) == AST_GCC_POINTER_DECLARATOR);

    if (old_declarator_is_ptr
            && !innermost_is_ptr)
    {
        old_declarator = ASTMake1(AST_PARENTHESIZED_DECLARATOR,
                old_declarator, 
                ASTFileName(old_declarator), ASTLine(old_declarator), NULL);
    }

    ast_set_child(innermost_decl, child, old_declarator);
}


static void merge_declarators(
        AST declarative_tree, 
        AST new_abstract_decl)
{
    // Nothing to do here
    if (new_abstract_decl == NULL)
        return;

    if (ASTType(declarative_tree) == AST_SIMPLE_DECLARATION
            || ASTType(declarative_tree) == AST_MEMBER_DECLARATION)
    {
        // These work on lists
        AST list = ASTSon1(declarative_tree), iter;
        if (list != NULL)
        {
            int i = 0;
            for_each_element(list, iter)
            {
                AST item = ASTSon1(iter);

                if (ASTType(item) == AST_BITFIELD_DECLARATOR)
                    continue;

                // The declarator is again always the zeroth child
                if (i == 0)
                {
                    merge_declarators_aux(ASTSon0(item), new_abstract_decl);
                }
                else
                {
                    // After the first one we need to copy
                    merge_declarators_aux(ASTSon0(item), 
                            ast_copy_for_instantiation(new_abstract_decl));
                }
                i++;
            }
        }
    }
    else
    {
        AST old_declarator = ASTSon1(declarative_tree);
        merge_declarators_aux(old_declarator, new_abstract_decl);
    }
}

static void merge_declaration(
        AST declarative_tree, 
        AST new_type_specifier_seq, 
        AST new_abstract_decl)
{
    // Mix first the type specifier seq
    merge_type_specifier_seq(declarative_tree, new_type_specifier_seq);
    merge_declarators(declarative_tree, new_abstract_decl);
}

static void update_type_tree(AST orig_tree, AST type_specifier_seq, AST abstract_decl)
{
    AST parent = ASTParent(orig_tree);
    AST type_specifier = ASTSon1(type_specifier_seq);

    switch (ASTType(parent))
    {
        case AST_SIMPLE_TYPE_SPEC:
            {
                AST parent_parent = ASTParent(parent);
                switch (ASTType(parent_parent))
                {
                    case AST_EXPLICIT_TYPE_CONVERSION:
                        {
                            if (abstract_decl != NULL)
                            {
                                running_error("%s: while instantiating, template argument '%s %s' is not valid\n",
                                        ast_location(orig_tree),
                                        prettyprint_in_buffer(type_specifier),
                                        prettyprint_in_buffer(abstract_decl));
                            }
                            ast_replace(orig_tree, type_specifier);
                            break;
                        }
                    case AST_TYPE_SPECIFIER_SEQ:
                        {
                            // type_specifier_seq declarator 
                            // Declarator may be null
                            AST declarative_tree = ASTParent(parent_parent);
                            merge_declaration(declarative_tree, type_specifier_seq, abstract_decl);
                            break;
                        }
                    default:
                        {
                            internal_error("Unhandled case", 0);
                            break;
                        }
                }
                break;
            }
        case AST_NESTED_NAME_SPECIFIER:
            {
                if (abstract_decl != NULL)
                {
                    running_error("%s: while instantiating, template argument '%s %s' is not valid\n",
                            ast_location(orig_tree),
                            prettyprint_in_buffer(type_specifier),
                            prettyprint_in_buffer(abstract_decl));
                }

                ast_replace(orig_tree, type_specifier);
                break;
            }
        default:
            {
                internal_error("Unhandled case", 0);
                break;
            }
    }
}

static void instantiate_tree_rec(AST orig_tree, decl_context_t context_of_being_instantiated)
{
    if (orig_tree == NULL)
        return;

    if (is_template_parameter_name(orig_tree))
    {
        scope_entry_t* entry = lookup_template_parameter_name(context_of_being_instantiated, orig_tree);

        switch (entry->kind)
        {
            case SK_VARIABLE:
                {
                    AST copied_expr = 
                        ASTMake1(AST_PARENTHESIZED_EXPRESSION,
                                ast_copy_for_instantiation(entry->language_dependent_value), 
                                ASTFileName(orig_tree),
                                ASTLine(orig_tree), 
                                NULL);

                    ast_replace(orig_tree, copied_expr);
                    break;
                }
            case SK_TYPEDEF:
                {
                    AST type_specifier_seq = NULL;
                    AST abstract_decl = NULL;
                    get_type_id_tree_of_type_split(entry->type_information, 
                            context_of_being_instantiated, 
                            &type_specifier_seq,
                            &abstract_decl,
                            ASTFileName(orig_tree), ASTLine(orig_tree));

                    update_type_tree(orig_tree, type_specifier_seq, abstract_decl);
                    break;
                }
            case SK_TEMPLATE:
                {
                    AST full_name = get_id_expression_for_entry(entry, 
                            context_of_being_instantiated, 
                            ASTFileName(orig_tree), 
                            ASTLine(orig_tree));
                    ast_replace(orig_tree, full_name);
                    break;
                }
            case SK_TEMPLATE_PARAMETER:
            case SK_TEMPLATE_TEMPLATE_PARAMETER:
            case SK_TEMPLATE_TYPE_PARAMETER:
                {
                    // Do nothing
                    break;
                }
            default:
                internal_error("Invalid symbol kind '%s'\n", symbol_kind_name(entry));
        }
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            instantiate_tree_rec(ast_get_child(orig_tree, i), context_of_being_instantiated);
        }
    }
}


AST instantiate_tree(AST orig_tree, decl_context_t context_of_being_instantiated)
{
    AST result = ast_copy_for_instantiation(orig_tree);

    instantiate_tree_rec(result, context_of_being_instantiated);
    return result;
}
