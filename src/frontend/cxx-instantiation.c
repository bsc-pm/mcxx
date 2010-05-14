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

#include <string.h>
#include "cxx-utils.h"
#include "cxx-ast.h"
#include "cxx-solvetemplate.h"
#include "cxx-instantiation.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-ambiguity.h"
#include "cxx-scope.h"

#include "cxx-printscope.h"

// static const char* get_name_of_template_parameter(
//         template_parameter_list_t* template_parameters,
//         int nesting,
//         int position)
// {
//     int i;
//     for (i = 0; i < template_parameters->num_template_parameters; i++)
//     {
//         template_parameter_t* current_template_parameter 
//             = template_parameters->template_parameters[i];
// 
//         if ((current_template_parameter->entry->entity_specs.template_parameter_nesting == nesting)
//                 && (current_template_parameter->entry->entity_specs.template_parameter_position == position))
//         {
//             return current_template_parameter->entry->symbol_name;
//         }
//     }
// 
//     internal_error("Not found template parameter with nest=%d and position=%d",
//             nesting, position);
// }

// Using tree
// static void instantiate_specialized_template_class(type_t* selected_template,
//         type_t* being_instantiated,
//         deduction_set_t* unification_set,
//         const char *filename, int line)
// {
//     DEBUG_CODE()
//     {
//         fprintf(stderr, "INSTANTIATION: About to instantiate class '%s'\n", 
//                 named_type_get_symbol(being_instantiated)->symbol_name);
//     }
//     ERROR_CONDITION(!is_named_class_type(being_instantiated), "Must be a named class", 0);
// 
//     scope_entry_t* named_class = named_type_get_symbol(being_instantiated);
// 
//     AST instantiation_body = NULL;
//     AST instantiation_base_clause = NULL;
//     class_type_get_instantiation_trees(get_actual_class_type(selected_template), 
//             &instantiation_body, &instantiation_base_clause);
// 
//     template_parameter_list_t* selected_template_parameters 
//         = template_specialized_type_get_template_parameters(get_actual_class_type(selected_template));
// 
//     instantiation_body = ast_copy_for_instantiation(instantiation_body);
//     instantiation_base_clause = ast_copy_for_instantiation(instantiation_base_clause);
// 
//     decl_context_t template_parameters_context = new_template_context(named_class->decl_context);
//     // Clear the template nesting level, if we are instantiating it is conceptually 0
//     template_parameters_context.decl_flags &= ~DF_TEMPLATE;
//     template_parameters_context.decl_flags &= ~DF_EXPLICIT_SPECIALIZATION;
//     template_parameters_context.template_nesting = 0;
//     // Empty template parameters
//     template_parameters_context.template_parameters = calloc(1, sizeof(*template_parameters_context));
// 
//     decl_context_t inner_decl_context = new_class_context(template_parameters_context, 
//             /* FIXME the qualification name should be more useful */named_class->symbol_name,
//             named_class->type_information);
// 
//     class_type_set_inner_context(named_class->type_information, inner_decl_context);
// 
//     DEBUG_CODE()
//     {
//         fprintf(stderr, "INSTANTIATION: Injecting template parameters\n");
//     }
// 
//     int i;
//     for (i = 0; i < unification_set->num_deductions; i++)
//     {
//         deduction_t* current_deduction = unification_set->deduction_list[i];
// 
//         ERROR_CONDITION(current_deduction->num_deduced_parameters != 1,
//                 "Number of deduced parameters is not 1!", 0);
// 
//         int j;
//         for (j = 0; j < current_deduction->num_deduced_parameters; j++)
//         {
//             const char* deduced_parameter_name = get_name_of_template_parameter(selected_template_parameters,
//                     current_deduction->parameter_nesting,
//                     current_deduction->parameter_position);
//             switch (current_deduction->kind)
//             {
//                 case TPK_TYPE :
//                     {
//                         // Note that we sign in the symbol in template_scope and not in current_scope
//                         scope_entry_t* injected_type = new_symbol(template_parameters_context, 
//                                 template_parameters_context.template_scope, deduced_parameter_name);
// 
//                         // We use a typedef
//                         injected_type->kind = SK_TYPEDEF;
//                         injected_type->entity_specs.is_template_argument = 1;
//                         injected_type->type_information = get_new_typedef(current_deduction->deduced_parameters[0]->type);
//                         break;
//                     }
//                 case TPK_TEMPLATE :
//                     {
//                         // Note that we sign in the symbol in template_scope and not in current_scope
//                         scope_entry_t* injected_type = new_symbol(template_parameters_context, 
//                                 template_parameters_context.template_scope, deduced_parameter_name);
// 
//                         // The template type has to be used here
//                         injected_type->kind = SK_TEMPLATE;
//                         injected_type->entity_specs.is_template_argument = 1;
//                         // These are always kept as named types in the compiler
//                         injected_type->type_information = 
//                             named_type_get_symbol(current_deduction->deduced_parameters[0]->type)->type_information;
//                         break;
//                     }
//                 case TPK_NONTYPE :
//                     {
//                         scope_entry_t* injected_nontype = new_symbol(template_parameters_context, 
//                                 template_parameters_context.template_scope, deduced_parameter_name);
// 
//                         injected_nontype->kind = SK_VARIABLE;
//                         injected_nontype->entity_specs.is_template_argument = 1;
//                         injected_nontype->type_information = current_deduction->deduced_parameters[0]->type;
// 
//                         // Fold it, as makes things easier
//                         literal_value_t literal_value = evaluate_constant_expression(current_deduction->deduced_parameters[0]->expression,
//                                 current_deduction->deduced_parameters[0]->decl_context);
//                         AST evaluated_tree = tree_from_literal_value(literal_value);
//                         AST fake_initializer = evaluated_tree;
//                         injected_nontype->expression_value = fake_initializer;
//                         break;
//                     }
//                 default:
//                     {
//                         internal_error("Invalid parameter kind", 0);
//                     }
//             }
//         }
// 
//     }
// 
//     DEBUG_CODE()
//     {
//         fprintf(stderr, "INSTANTIATION: Template parameters injected\n");
//     }
// 
//     DEBUG_CODE()
//     {
//         fprintf(stderr, "INSTANTIATION: Injected context\n");
//         print_scope(inner_decl_context);
//         fprintf(stderr, "INSTANTIATION: End of injected context \n");
//     }
// 
//     enter_class_specifier();
// 
//     if (instantiation_base_clause != NULL)
//     {
//         build_scope_base_clause(instantiation_base_clause, 
//                 get_actual_class_type(being_instantiated), 
//                 inner_decl_context);
//     }
//     
//     // Inject the class name
//     scope_entry_t* injected_symbol = new_symbol(inner_decl_context, 
//             inner_decl_context.current_scope, named_class->symbol_name);
// 
//     *injected_symbol = *named_class;
// 
//     injected_symbol->do_not_print = 1;
//     injected_symbol->entity_specs.is_injected_class_name = 1;
//     injected_symbol->entity_specs.injected_class_referred_symbol = named_class;
// 
//     /*
//      * Note that the standard allows code like this one
//      *
//      * template <typename _T>
//      * struct A { };
//      *
//      * template <>
//      * struct A<int> 
//      * {
//      *   typedef int K;  (1)
//      *   A<int>::K k;    (2)
//      * };
//      *
//      * So we can use the name 'A<int>::K' inside the class provided 'K' has been declared
//      * before the point we refer it (so: switching declarations (1) and (2) would not work).
//      *
//      * This also affects the injected class-name, so, for another example
//      *
//      * template <typename _T>
//      * struct B
//      * {
//      *   typedef _T P;             (3)
//      *   typename B::P k;          (4)
//      * };
//      *
//      * Note that in a partial, or not at all, specialized class the injected
//      * class name is dependent so 'typename' is mandatory (like in (4)). It is
//      * redundant since the injected-class name obviously refers to the current
//      * class so no ambiguity would arise between identifiers and typenames.
//      * Seems that a DR has been filled on that.
//      *
//      * All this explanation is here just to justify that the class is already
//      * complete and independent just before start the parsing of its members.
//      * Otherwise all the machinery would try to instantiate it again and again
//      * (and this is not good at all).
//      */
//     set_is_complete_type(being_instantiated, /* is_complete */ 1);
//     set_is_dependent_type(being_instantiated, /* is_dependent */ 0);
// 
//     set_is_complete_type(get_actual_class_type(being_instantiated), /* is_complete */ 1);
//     set_is_dependent_type(get_actual_class_type(being_instantiated), /* is_dependent */ 0);
// 
//     if (instantiation_body != NULL)
//     {
//         inner_decl_context.decl_flags |= DF_INSTANTIATING;
// 
//         // Fix this AS_PUBLIC one day
//         build_scope_member_specification_first_step(inner_decl_context, instantiation_body, AS_PUBLIC,
//                 being_instantiated);
//     }
// 
//     // The symbol is defined after this
//     named_class->defined = 1;
// 
//     leave_class_specifier();
//     
//     // Finish the class
//     finish_class_type(get_actual_class_type(being_instantiated), being_instantiated, 
//             named_class->decl_context, filename, line);
// 
//     DEBUG_CODE()
//     {
//         fprintf(stderr, "INSTANTIATION: End of instantiation of class '%s'\n", 
//                 named_type_get_symbol(being_instantiated)->symbol_name);
//     }
// }
//
//
//


// static scope_entry_list_t* filter_any_non_type(scope_entry_list_t* entry_list)
// {
//     // Filter the types
//     enum cxx_symbol_kind type_filter[] = {
//         SK_ENUM ,
//         SK_CLASS ,
//         SK_TYPEDEF ,
//         SK_TEMPLATE_TYPE_PARAMETER,
//         SK_TEMPLATE_TEMPLATE_PARAMETER,
//         SK_TEMPLATE,
//         SK_GCC_BUILTIN_TYPE
//     };
// 
//     return filter_symbol_kind_set(entry_list, STATIC_ARRAY_LENGTH(type_filter), type_filter);
// }

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

    fprintf(stderr, "INSTANTIATION: Adding new template to template map\n");

    P_LIST_ADD((*template_map), (*num_items_template_map), new_map);

    template_type_set_related_symbol(new_member->type_information, new_member);

    type_t* new_primary_template = template_type_get_primary_type(new_member->type_information);

    named_type_get_symbol(new_primary_template)->decl_context = context_of_being_instantiated;

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
    fprintf(stderr, "INSTANTIATION: Instantiating member '%s' at '%s:%d'\n", 
            member_of_template->symbol_name,
            member_of_template->file,
            member_of_template->line);

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
                    if (is_constant_expression(new_member->entity_specs.bitfield_expr, context_of_being_instantiated))
                    {
                        literal_value_t literal =
                            evaluate_constant_expression(new_member->entity_specs.bitfield_expr,
                                    context_of_being_instantiated);
                        
                        if (literal_value_is_zero(literal)
                                || literal_value_is_negative(literal))
                        {
                            char valid = 0;
                            int val = literal_value_to_int(literal, &valid);

                            running_error("%s:%d: error: invalid bitfield of size '%d'",
                                new_member->file, new_member->line, val);
                        }

                        new_member->entity_specs.bitfield_expr =
                            tree_from_literal_value(literal);
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

                fprintf(stderr, "INSTANTIATION: Member '%s' is a %s data member with type '%s'\n", 
                        new_member->symbol_name,
                        new_member->entity_specs.is_static ? "static" : "non-static",
                        print_type_str(new_member->type_information, context_of_being_instantiated));
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

                fprintf(stderr, "INSTANTIATION: Member '%s' is a typedef. Instantiated type is '%s'\n",
                        new_member->symbol_name,
                        print_type_str(new_member->type_information, context_of_being_instantiated));

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

                    type_t* primary_specialization = named_type_get_symbol(template_type_get_primary_type(template_type))->type_information;

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
                        fprintf(stderr, "INSTANTIATION: Searching in template map (num_elems = %d)\n",
                                *num_items_template_map);

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

                                        if (is_constant_expression(template_arg->expression, context_of_being_instantiated))
                                        {
                                            literal_value_t literal_value = evaluate_constant_expression(template_arg->expression, 
                                                    context_of_being_instantiated);
                                            new_template_arg->expression = tree_from_literal_value(literal_value);
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

                    char is_dependent = 0;
                    int max_qualif = 0;
                    fprintf(stderr, "INSTANTIATION: New member function '%s'\n",
                            print_decl_type_str(new_member->type_information, 
                                context_of_being_instantiated, 
                                get_fully_qualified_symbol_name(new_member, 
                                    context_of_being_instantiated, 
                                    &is_dependent, &max_qualif)));
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
                }
                if (member_of_template->entity_specs.is_destructor)
                {
                    class_type_set_destructor(get_actual_class_type(being_instantiated), new_member);
                }
                if (member_of_template->entity_specs.is_conversion)
                {
                    class_type_add_conversion_function(get_actual_class_type(being_instantiated), new_member);
                }
                if (member_of_template->entity_specs.is_assignment_operator)
                {
                    class_type_add_copy_assignment_operator(get_actual_class_type(being_instantiated), new_member);
                }

                break;
            }
        default:
            {
                internal_error("Unexpected member kind=%d\n", member_of_template->kind);
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
    fprintf(stderr, "INSTANTIATION: Instantiating class '%s'\n", 
            print_declarator(being_instantiated));

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
            /* FIXME the qualification name should be more useful */named_class->symbol_name,
            named_class->type_information);

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

                        // Fold it, as makes things easier
                        literal_value_t literal_value = evaluate_constant_expression(current_deduction->deduced_parameters[0]->expression,
                                current_deduction->deduced_parameters[0]->decl_context);
                        AST evaluated_tree = tree_from_literal_value(literal_value);
                        AST fake_initializer = evaluated_tree;
                        param_symbol->expression_value = fake_initializer;
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

    fprintf(stderr, "INSTANTIATION: Have to instantiate %d members\n", num_members);
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

    fprintf(stderr, "INSTANTIATION: End of instantiation of class '%s'\n", 
            print_declarator(being_instantiated));
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        decl_context_t context_of_being_instantiated,
        const char* filename, int line)
{
    int i, num_bases = class_type_get_num_bases(selected_class_type);

    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        scope_entry_t* base_class_sym = class_type_get_base_num(selected_class_type, i, &is_virtual, 
                &is_dependent);

        type_t* base_class_named_type = NULL;
        if (base_class_sym->kind == SK_DEPENDENT_ENTITY)
        {
            base_class_named_type = base_class_sym->type_information;
        }
        else
        {
            base_class_named_type = get_user_defined_type(base_class_sym);
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

        class_type_add_base_class(instantiated_class_type, upd_base_class_sym, is_virtual, /* is_dependent */ 0);
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

    fprintf(stderr, "INSTANTIATION: Instantiating class '%s' at '%s:%d'\n",
            print_type_str(get_user_defined_type(entry), entry->decl_context),
            entry->file,
            entry->line);


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
        instantiate_specialized_template_class(selected_template, 
                get_user_defined_type(entry),
                unification_set, filename, line);
    }
    else
    {
        internal_error("Could not instantiate template '%s' declared for first time in '%s:%d'", 
                entry->symbol_name,
                entry->file,
                entry->line);
    }
}

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


                    // Fold it, as makes things easier
                    literal_value_t literal_value = evaluate_constant_expression(template_argument->expression,
                            template_argument->expression_context);
                    AST evaluated_tree = tree_from_literal_value(literal_value);
                    AST fake_initializer = evaluated_tree;
                    injected_nontype->expression_value = fake_initializer;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "Injecting parameter '%s' with expression '%s'\n", injected_nontype->symbol_name, 
                                prettyprint_in_buffer(fake_initializer));
                    }
                    break;
                }
            default:
                internal_error("Invalid template parameter kind %d\n", template_param->kind);
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


