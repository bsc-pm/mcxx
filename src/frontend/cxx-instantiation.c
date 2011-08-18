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
#include "cxx-typededuc.h"
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
    new_member->entity_specs.is_member = 1;
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

typedef
struct translation_info_tag
{
    decl_context_t context_of_template;
    decl_context_t context_of_being_instantiated;
} translation_info_t;

static decl_context_t translation_function(decl_context_t decl_context, void *d)
{
    translation_info_t * p = (translation_info_t*)d;

    decl_context_t result = decl_context;

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Translating context %p (template = %p || being instantiated = %p)\n", 
                result.class_scope, 
                p->context_of_template.class_scope, 
                p->context_of_being_instantiated.class_scope);
    }

    if (result.class_scope == p->context_of_template.class_scope)
    {
        result.class_scope = p->context_of_being_instantiated.class_scope;
    }
    if (result.current_scope == p->context_of_template.current_scope)
    {
        result.current_scope = p->context_of_being_instantiated.class_scope;
    }

    return result;
}

static scope_entry_t* instantiate_template_type_member(type_t* template_type, 
        decl_context_t context_of_being_instantiated,
        scope_entry_t *member_of_template,
        type_t* being_instantiated, 
        char is_class,
        decl_context_t context_translation_function(decl_context_t, void*),
        void *translation_data,
        const char* filename, 
        int line,
        type_map_t** template_map, 
        int *num_items_template_map)
{
    // This is the primary template
    template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);


    template_parameter_list_t* updated_template_parameters = duplicate_template_argument_list(template_parameters);
    updated_template_parameters->enclosing = context_of_being_instantiated.template_parameters;

    decl_context_t new_context_for_template_parameters = context_of_being_instantiated;
    new_context_for_template_parameters.template_parameters = updated_template_parameters;

        
    // Update the template parameters
    int i;
    for (i = 0; i < updated_template_parameters->num_parameters; i++)
    {
        if (updated_template_parameters->arguments[i] != NULL)
        {
            updated_template_parameters->arguments[i] = update_template_parameter_value(
                    template_parameters->arguments[i],
                    new_context_for_template_parameters, 
                    filename, line);
        }
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
                new_context_for_template_parameters,
                context_translation_function, translation_data,
                filename, line);
    }

    scope_entry_t* new_member = new_symbol(new_context_for_template_parameters, 
            new_context_for_template_parameters.current_scope,
            member_of_template->symbol_name);

    new_member->kind = SK_TEMPLATE;
    new_member->type_information = 
        get_new_template_type(updated_template_parameters,
                base_type,
                new_member->symbol_name,
                new_context_for_template_parameters,
                member_of_template->line,
                member_of_template->file);

    new_member->entity_specs.is_member = 1;
    new_member->entity_specs.class_type = being_instantiated;

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

    named_type_get_symbol(new_primary_template)->decl_context = new_context_for_template_parameters;

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
    }

    return new_member;
}

static void instantiate_emit_member_function(scope_entry_t* entry, const char* filename, int line);

static void instantiate_member(type_t* selected_template UNUSED_PARAMETER, 
        type_t* being_instantiated, 
        scope_entry_t* member_of_template, 
        decl_context_t context_of_being_instantiated,
        decl_context_t context_translation_function(decl_context_t, void*),
        void *translation_data,
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
                        context_translation_function, translation_data,
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
                    if (nodecl_is_constant(new_member->entity_specs.bitfield_size))
                    {
                        if (const_value_is_zero(
                                    const_value_gt(
                                        nodecl_get_constant(new_member->entity_specs.bitfield_size),
                                        const_value_get_zero(/* bytes*/ 4, /* sign */ 1))))
                        {
                            running_error("%s:%d: error: invalid bitfield of size '%d'",
                                    new_member->file, new_member->line, 
                                    const_value_cast_to_4(
                                        nodecl_get_constant(new_member->entity_specs.bitfield_size)));
                        }

                        new_member->entity_specs.bitfield_size_context =
                            context_of_being_instantiated;
                    }
                    else
                    {
                        // ???? FIXME - What about sizes that depend on a nontype template argument?
                        running_error("%s:%d: error: bitfield specification is not a constant expression", 
                                new_member->file, new_member->line);
                    }
                }

                if (!nodecl_is_null(member_of_template->value))
                {
                    internal_error("Not yet implemented", 0);
                    // new_member->language_dependent_value = ast_copy_for_instantiation(member_of_template->language_dependent_value);

                    // check_initialization(new_member->language_dependent_value, context_of_being_instantiated, 
                    //         new_member->type_information);
                    // new_member->value = expression_get_nodecl(new_member->language_dependent_value);
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
                        context_translation_function, translation_data,
                        filename, line);

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: Member '%s' is a typedef. Instantiated type is '%s'\n",
                            new_member->symbol_name,
                            print_type_str(new_member->type_information, context_of_being_instantiated));
                }
                ERROR_CONDITION(is_dependent_type(new_member->type_information), "Invalid type", 0);

                break;
            }
        case SK_ENUM:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        member_of_template);

                new_member->type_information = get_new_enum_type(context_of_being_instantiated);

                // Register a map

                type_map_t new_map;
                new_map.orig_type = member_of_template->type_information;
                new_map.new_type = get_user_defined_type(new_member);

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

                new_member->type_information = new_type;

                ERROR_CONDITION(nodecl_is_null(member_of_template->value),
                        "An enumerator always has a related expression", 0);

                internal_error("Not yet implemented", 0);
                // new_member->language_dependent_value = ast_copy_for_instantiation(member_of_template->language_dependent_value);
                // check_expression(new_member->language_dependent_value, context_of_being_instantiated);

                // new_member->value = expression_get_nodecl(new_member->language_dependent_value);

                enum_type_add_enumerator(new_type, new_member);

                break;
            }
        case SK_CLASS:
            {
                if (!is_template_specialized_type(member_of_template->type_information))
                {
                    template_parameter_list_t* tpl_empty = calloc(1, sizeof(*tpl_empty));
                    tpl_empty->enclosing = context_of_being_instantiated.template_parameters;

                    decl_context_t new_context_of_being_instantiated = context_of_being_instantiated;
                    new_context_of_being_instantiated.template_parameters = tpl_empty;

                    scope_entry_t* new_fake_template_symbol = calloc(1, sizeof(*new_fake_template_symbol));
                    new_fake_template_symbol->kind = SK_TEMPLATE;
                    new_fake_template_symbol->symbol_name = member_of_template->symbol_name;
                    new_fake_template_symbol->file = member_of_template->file;
                    new_fake_template_symbol->line = member_of_template->line;

                    type_t* template_type = get_new_template_type(tpl_empty, 
                            member_of_template->type_information, 
                            member_of_template->symbol_name, 
                            new_context_of_being_instantiated, 
                            member_of_template->line, 
                            member_of_template->file);

                    new_fake_template_symbol->type_information = template_type;
                    template_type_set_related_symbol(template_type, new_fake_template_symbol);

                    scope_entry_t* primary_template = named_type_get_symbol(template_type_get_primary_type(template_type));
                    primary_template->entity_specs.is_user_declared = 1;

                    type_t* primary_specialization = primary_template->type_information;

                    // Fix some bits inherited from the original class type
                    class_type_set_enclosing_class_type(get_actual_class_type(primary_specialization),
                            get_actual_class_type(being_instantiated));

                    set_is_complete_type(primary_specialization, /* is_complete */ 1);

                    scope_entry_t* new_member = named_type_get_symbol(
                            template_type_get_specialized_type(template_type,
                                tpl_empty,
                                new_context_of_being_instantiated,
                                member_of_template->file, member_of_template->line));

                    insert_entry(context_of_being_instantiated.current_scope, new_member);

                    new_member->entity_specs = member_of_template->entity_specs;
                    new_member->entity_specs.is_member = 1;
                    new_member->entity_specs.class_type = being_instantiated;

                    class_type_add_member(get_actual_class_type(being_instantiated), new_member);

                    AST orig_bases_tree, orig_body_tree;
                    class_type_get_instantiation_trees(member_of_template->type_information,
                            &orig_body_tree, &orig_bases_tree);

                    class_type_set_instantiation_trees(get_actual_class_type(new_member->type_information),
                            orig_body_tree, orig_bases_tree);

                    set_is_complete_type(new_member->type_information, /* is_complete */ 0);
                    set_is_dependent_type(new_member->type_information, /* is_dependent */ 0);

                    if (new_member->entity_specs.is_anonymous_union)
                    {
                        instantiate_template_class(new_member, context_of_being_instantiated, filename, line);
                        scope_entry_t* anon_member = finish_anonymous_class(new_member, context_of_being_instantiated);

                        // Add this member to the current class
                        anon_member->entity_specs.is_member = 1;
                        anon_member->entity_specs.class_type = being_instantiated;
                        anon_member->entity_specs.access = new_member->entity_specs.access;
                        class_type_add_member(being_instantiated, anon_member);
                    }
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
                                context_translation_function, translation_data,
                                filename, line,
                                template_map, num_items_template_map);
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

                        template_parameter_list_t *template_args =
                            template_specialized_type_get_template_arguments(member_of_template->type_information);

                        template_parameter_list_t *new_template_args = duplicate_template_argument_list(template_args);

                        for (i = 0; i < new_template_args->num_parameters; i++)
                        {
                            new_template_args->arguments[i] = update_template_parameter_value(
                                    template_args->arguments[i],
                                    context_of_being_instantiated, filename, line);
                        }

                        // Now ask a new specialization
                        type_t* new_template_specialized_type = template_type_get_specialized_type_after_type(new_template_type,
                                new_template_args,
                                member_of_template->type_information,
                                context_of_being_instantiated,
                                member_of_template->file, 
                                member_of_template->line);

                        named_type_get_symbol(new_template_specialized_type)->entity_specs.is_user_declared = 1;

                        class_type_add_member(
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
                            context_translation_function, translation_data,
                            filename, line);

                    new_member->entity_specs.is_non_emitted = 1;
                    new_member->entity_specs.emission_template = member_of_template;
                    new_member->entity_specs.emission_handler = instantiate_emit_member_function;
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
                            context_translation_function, translation_data,
                            filename, line,
                            template_map, num_items_template_map);

                    new_member->defined = 1;
                    // new_member->entity_specs.is_non_emitted = 1;

                    // We work on the primary template
                    type_t* primary_type = template_type_get_primary_type(new_member->type_information);
                    new_member = named_type_get_symbol(primary_type);

                    new_member->entity_specs.is_inline = named_type_get_symbol(primary_template)->entity_specs.is_inline;
                    new_member->entity_specs.is_static = named_type_get_symbol(primary_template)->entity_specs.is_static;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: New member function '%s'\n",
                            print_decl_type_str(new_member->type_information, 
                                new_member->decl_context,
                                get_qualified_symbol_name(new_member, 
                                    new_member->decl_context)));
                }

                if (member_of_template->entity_specs.is_constructor)
                {
                    if (member_of_template->entity_specs.is_default_constructor)
                    {
                        class_type_set_default_constructor(get_actual_class_type(being_instantiated), new_member);
                    }
                }
                if (member_of_template->entity_specs.is_destructor)
                {
                    class_type_set_destructor(get_actual_class_type(being_instantiated), new_member);
                }

                break;
            }
            // This is only possible because of using declarations / or qualified members
            // which refer to dependent entities
            // case SK_DEPENDENT_ENTITY:
            //     {
            //         ERROR_CONDITION(member_of_template->language_dependent_value == NULL,
            //                 "Invalid expression for dependent entity", 0);

            //         scope_entry_list_t *entry_list = query_id_expression(context_of_being_instantiated, member_of_template->language_dependent_value);
            //         
            //         if (entry_list == NULL
            //                 || !entry_list_head(entry_list)->entity_specs.is_member)
            //         {
            //             running_error("%s: invalid using declaration '%s' while instantiating\n", 
            //                     ast_location(member_of_template->language_dependent_value),
            //                     prettyprint_in_buffer(member_of_template->language_dependent_value));
            //         }

            //         scope_entry_t* entry = entry_list_head(entry_list);
            //         if (!class_type_is_base(entry->entity_specs.class_type, 
            //                     get_actual_class_type(being_instantiated)))
            //         {
            //             running_error("%s: entity '%s' is not a member of a base of class '%s'\n",
            //                     ast_location(member_of_template->language_dependent_value),
            //                         get_qualified_symbol_name(entry,
            //                             context_of_being_instantiated),
            //                         get_qualified_symbol_name(named_type_get_symbol(being_instantiated), 
            //                             context_of_being_instantiated)
            //                     );
            //         }

            //         scope_entry_list_iterator_t* it = NULL;
            //         for (it = entry_list_iterator_begin(entry_list);
            //                 !entry_list_iterator_end(it);
            //                 entry_list_iterator_next(it))
            //         {
            //             entry = entry_list_iterator_current(it);
            //             class_type_add_member(get_actual_class_type(being_instantiated), entry);

            //             // Insert the symbol in the context
            //             insert_entry(context_of_being_instantiated.current_scope, entry);
            //         }
            //         entry_list_iterator_free(it);
            //         entry_list_free(entry_list);

            //         break;
            //     }
        case SK_USING:
            {
                // Two cases: a) the entity is actually dependent: it will have only one SK_DEPENDENT_ENTITY 
                //            b) the entity is not dependent: it may have more than one element
                scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(member_of_template->type_information);
                scope_entry_t* entry = entry_list_head(entry_list);

                internal_error("Not yet implemented", 0);
                if (entry->kind == SK_DEPENDENT_ENTITY)
                {
#if 0
                    ERROR_CONDITION(entry_list_size(entry_list) != 1, "Invalid list", 0);
                    introduce_using_entity_id_expr(entry->language_dependent_value, 
                            context_of_being_instantiated, 
                            member_of_template->entity_specs.access);
#endif
                }
                else
                {

                    introduce_using_entities(entry_list, context_of_being_instantiated, 
                            named_type_get_symbol(being_instantiated),
                            /* is_class_scope */ 1,
                            member_of_template->entity_specs.access,
                            member_of_template->file,
                            member_of_template->line);
                }
                break;
            }
        default:
            {
                internal_error("Unexpected member kind=%s\n", symbol_kind_name(member_of_template));
            }
    }
}

static void instantiate_dependent_friend(type_t* selected_template UNUSED_PARAMETER,
        type_t* being_instantiated, 
        scope_entry_t* friend,
        decl_context_t context_of_being_instantiated,
        decl_context_t context_translation_function(decl_context_t, void*),
        void *translation_data,
        const char *filename, 
        int line)
{
    internal_error("Not yet implemented", 0);

#if 0
    decl_context_t orig_decl_context;
    AST declarator = nodecl_unwrap_cxx_dependent_expr(friend->value, &orig_decl_context);
    type_t* member_type = update_type_for_instantiation(friend->type_information,
            context_of_being_instantiated,
            context_translation_function, translation_data,
            filename, line);

    // FIXME - We should use the original one!
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));
    gather_info.is_friend = 1;

    build_scope_friend_declarator(context_of_being_instantiated,
            &gather_info,
            being_instantiated,
            member_type,
            declarator);
#endif
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        decl_context_t context_of_being_instantiated,
        decl_context_t context_translation_function(decl_context_t, void*),
        void *translation_data,
        const char* filename, int line);

static void instantiate_specialized_template_class(type_t* selected_template,
        type_t* being_instantiated,
        deduction_set_t* deduction_set,
        const char *filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating class '%s'\n", 
                print_declarator(being_instantiated));
    }

    ERROR_CONDITION(!is_named_class_type(being_instantiated), "Must be a named class", 0);

    scope_entry_t* being_instantiated_sym = named_type_get_symbol(being_instantiated);

    AST instantiation_body = NULL;
    AST instantiation_base_clause = NULL;
    class_type_get_instantiation_trees(get_actual_class_type(selected_template), 
            &instantiation_body, &instantiation_base_clause);

    instantiation_body = ast_copy_for_instantiation(instantiation_body);
    instantiation_base_clause = ast_copy_for_instantiation(instantiation_base_clause);

    // Update the template parameter with the deduced template parameters
    decl_context_t instantiation_context = being_instantiated_sym->decl_context;

    // Build the template arguments. We use the selected template to update its deduction
    template_parameter_list_t* template_arguments = 
        build_template_parameter_list_from_deduction_set(
                template_specialized_type_get_template_parameters(get_actual_class_type(selected_template)),
                deduction_set);
    // But the selected_template might be a nested one in a dependent context so we must update
    // the enclosing template arguments with those of the original class
    ERROR_CONDITION(being_instantiated_sym->decl_context.template_parameters == NULL, "Wrong nesting in template parameters", 0);

    template_arguments->enclosing = being_instantiated_sym->decl_context.template_parameters->enclosing;

    // Our instantiation context is ready
    instantiation_context.template_parameters = template_arguments;

    template_specialized_type_update_template_parameters(being_instantiated_sym->type_information,
            instantiation_context.template_parameters);

    decl_context_t inner_decl_context = new_class_context(instantiation_context, 
            being_instantiated_sym);

    being_instantiated_sym->decl_context = instantiation_context;

    class_type_set_inner_context(being_instantiated_sym->type_information, inner_decl_context);

    translation_info_t translation_info;
    translation_info.context_of_template = class_type_get_inner_context(get_actual_class_type(selected_template));
    translation_info.context_of_being_instantiated = inner_decl_context;

    if (instantiation_base_clause != NULL)
    {
        instantiate_bases(
                get_actual_class_type(selected_template),
                get_actual_class_type(being_instantiated),
                inner_decl_context,
                translation_function,
                &translation_info,
                filename, line
                );
    }
    
    // Inject the class name
    scope_entry_t* injected_symbol = new_symbol(inner_decl_context, 
            inner_decl_context.current_scope, being_instantiated_sym->symbol_name);

    *injected_symbol = *being_instantiated_sym;

    injected_symbol->do_not_print = 1;
    injected_symbol->entity_specs.is_member = 1;
    injected_symbol->entity_specs.class_type = get_user_defined_type(being_instantiated_sym);
    injected_symbol->entity_specs.is_injected_class_name = 1;

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

    type_map_t* template_map = NULL;
    int num_items_template_map = 0;

    type_map_t* enum_map = NULL;
    int num_items_enum_map = 0;

    scope_entry_list_t * members = class_type_get_members(get_actual_class_type(selected_template));
    scope_entry_list_t * friends = class_type_get_friends(get_actual_class_type(selected_template));
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Have to instantiate %d members\n", entry_list_size(members));
    }

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(members);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* member = entry_list_iterator_current(it);

        instantiate_member(selected_template, 
                being_instantiated, 
                member, 
                inner_decl_context,
                translation_function,
                &translation_info,
                filename, line,
                &template_map, &num_items_template_map,
                &enum_map, &num_items_enum_map);
    }
    entry_list_iterator_free(it);
    entry_list_free(members);

    // Friends
    for (it = entry_list_iterator_begin(friends);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* friend = entry_list_iterator_current(it);

        if (friend->kind == SK_DEPENDENT_FRIEND)
        {
            instantiate_dependent_friend(selected_template, 
                    being_instantiated, 
                    friend,
                    inner_decl_context,
                    translation_function,
                    &translation_info,
                    filename, line);
        }
        else if (friend->kind == SK_FUNCTION
                || friend->kind == SK_CLASS)
        {
            class_type_add_friend_symbol(get_actual_class_type(being_instantiated), friend);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(friends);

    // The symbol is defined after this
    being_instantiated_sym->defined = 1;

    // Finish the class (this order does not match the one used in buildscope, does it?)
    nodecl_t nodecl_finish_class = nodecl_null();
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Finishing class '%s'\n", 
                print_declarator(being_instantiated));
    }
    finish_class_type(get_actual_class_type(being_instantiated), being_instantiated, 
            being_instantiated_sym->decl_context, filename, line, &nodecl_finish_class);
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Class '%s' finished\n", 
                print_declarator(being_instantiated));
    }

    scope_entry_t* selected_template_sym = named_type_get_symbol(selected_template);
    if (selected_template_sym->entity_specs.is_member)
    {
        scope_entry_t* enclosing_class = named_type_get_symbol(selected_template_sym->entity_specs.class_type);
        class_type_add_member(enclosing_class->type_information, being_instantiated_sym);
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
        decl_context_t context_translation_function(decl_context_t, void*),
        void *translation_data,
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
        char is_dependent_base = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class_sym = class_type_get_base_num(selected_class_type, i, &is_virtual, 
                &is_dependent_base, &access_specifier);

        type_t* base_class_named_type = NULL;
        if (base_class_sym->kind == SK_DEPENDENT_ENTITY)
        {
            base_class_named_type = base_class_sym->type_information;
        }
        else
        {
            if (!is_dependent_base)
            {
                // Maybe we do not need to update anything...
                base_class_named_type = get_user_defined_type(base_class_sym);
            }
            else
            {
                base_class_named_type = get_dependent_typename_type_from_parts(base_class_sym, NULL);
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Updating base class '%s'\n", 
                    print_declarator(base_class_named_type));
        }

        type_t* upd_base_class_named_type = update_type_for_instantiation(base_class_named_type,
                context_of_being_instantiated,
                context_translation_function, translation_data,
                filename, line);

        ERROR_CONDITION( is_dependent_type(upd_base_class_named_type), "Invalid base class update %s", 
                print_type_str(upd_base_class_named_type, context_of_being_instantiated));

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

    type_t* selected_template = solve_class_template(
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

static nodecl_t nodecl_instantiation_units;
typedef
struct instantiation_item_tag
{
    scope_entry_t* symbol;
    const char* filename;
    int line;
} instantiation_item_t;

static instantiation_item_t** symbols_to_instantiate;
static int num_symbols_to_instantiate;

void instantiation_init(void)
{
    nodecl_instantiation_units = nodecl_null();
    symbols_to_instantiate = NULL;
    num_symbols_to_instantiate = 0;
}

static void instantiate_every_symbol(scope_entry_t* entry,
        const char* filename,
        int line);

nodecl_t instantiation_instantiate_pending_functions(void)
{
    int tmp_num_symbols_to_instantiate = num_symbols_to_instantiate;
    instantiation_item_t** tmp_symbols_to_instantiate = symbols_to_instantiate;

    num_symbols_to_instantiate = 0;
    symbols_to_instantiate = NULL;

    int i;
    for (i = 0; i < tmp_num_symbols_to_instantiate; i++)
    {
        instantiate_every_symbol(
                tmp_symbols_to_instantiate[i]->symbol,
                tmp_symbols_to_instantiate[i]->filename,
                tmp_symbols_to_instantiate[i]->line);

        free(tmp_symbols_to_instantiate[i]);
    }
    free(tmp_symbols_to_instantiate);

    if (num_symbols_to_instantiate != 0)
    {
        return instantiation_instantiate_pending_functions();
    }
    else
    {
        return nodecl_instantiation_units;
    }
}

static char compare_instantiate_items(instantiation_item_t* current_item, instantiation_item_t* new_item)
{
    return current_item->symbol == new_item->symbol;
}

void instantiation_add_symbol_to_instantiate(scope_entry_t* entry,
        const char* filename,
        int line)
{
    instantiation_item_t* item = calloc(1, sizeof(*item));
    item->symbol = entry;
    item->filename = filename;
    item->line = line;

    int old_num = num_symbols_to_instantiate;

    P_LIST_ADD_ONCE_FUN(symbols_to_instantiate, 
            num_symbols_to_instantiate, 
            item, 
            compare_instantiate_items);

    // Crummy way to know if it was added
    if (old_num == num_symbols_to_instantiate)
    {
        free(item);
    }
}

static void instantiate_template_function(scope_entry_t* entry, const char* filename, int line)
{
    internal_error("Not yet implemented", 0);

#if 0
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating function '%s' with type '%s' at '%s:%d\n",
                entry->symbol_name,
                print_declarator(entry->type_information),
                entry->file,
                entry->line);
    }

    // Update to the instantiation point
    entry->file = filename;
    entry->line = line;

    type_t* template_specialized_type = entry->type_information;

    type_t* template_type = template_specialized_type_get_related_template_type(template_specialized_type);
    scope_entry_t* template_symbol = template_type_get_related_symbol(template_type);

    // The primary specialization is a named type, even if the named type is a function!
    type_t* primary_specialization_type = template_type_get_primary_type(template_symbol->type_information);
    scope_entry_t* primary_specialization_function = named_type_get_symbol(primary_specialization_type);
    // type_t* primary_specialization_function_type = primary_specialization_function->type_information;

    AST orig_function_definition = primary_specialization_function->entity_specs.definition_tree;
    
    // Remove dependent types
    AST dupl_function_definition = ast_copy_for_instantiation(orig_function_definition);

    // Why do we do this?
    // Temporarily disable ambiguity testing
    char old_test_status = get_test_expression_status();
    set_test_expression_status(0);

    decl_context_t instantiation_context = entry->decl_context;

    nodecl_t nodecl_function_code = nodecl_null();
    build_scope_function_definition(dupl_function_definition, 
            entry, 
            instantiation_context, 
            // This is not entirely true
            /* is_template */ 1,
            /* is_explicit_instantiation */ 1,
            &nodecl_function_code);

    entry->entity_specs.definition_tree = dupl_function_definition;

    nodecl_instantiation_units = nodecl_concat_lists(nodecl_instantiation_units,
            nodecl_function_code);

    set_test_expression_status(old_test_status);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: ended instantation of function template '%s'\n",
                print_declarator(template_specialized_type));
    }
#endif
}

static scope_entry_t* being_instantiated_now[MCXX_MAX_TEMPLATE_NESTING_LEVELS];
static int num_being_instantiated_now = 0;

void instantiate_template_function_if_needed(scope_entry_t* entry, const char* filename, int line)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiation request of template function '%s' at '%s:%d\n",
                print_decl_type_str(entry->type_information, entry->decl_context, 
                    get_qualified_symbol_name(entry, entry->decl_context)),
                filename,
                line);
    }

    // Do nothing if we are checking for ambiguities as this may cause havoc
    if (checking_ambiguity())
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Not instantiating since we are checking for ambiguities\n");
        }
        return;
    }

    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid symbol", 0);

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
    // type_t* primary_specialization_function_type = primary_specialization_function->type_information;

    // If the primary specialization is not defined, no instantiation may happen
    if (!primary_specialization_function->defined)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Not instantiating since primary template has not been defined\n");
        }
        return;
    }

    if (entry->defined)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Function already instantiated\n");
        }
        return;
    }

    int i;
    for (i = 0; i < num_being_instantiated_now; i++)
    {
        if (being_instantiated_now[i] == entry)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INSTANTIATION: This function is currently being instantiated\n");
            }
            return;
        }
    }

    ERROR_CONDITION(num_being_instantiated_now == MCXX_MAX_TEMPLATE_NESTING_LEVELS, 
            "Too many instantiation template levels %d", MCXX_MAX_TEMPLATE_NESTING_LEVELS);

    being_instantiated_now[num_being_instantiated_now] = entry;
    num_being_instantiated_now++;

    instantiate_template_function(entry, filename, line);

    num_being_instantiated_now--;
    being_instantiated_now[num_being_instantiated_now] = NULL;
}

static void instantiate_default_arguments_of_function(scope_entry_t* entry)
{
    decl_context_t instantiation_context = entry->decl_context;
    // Update the default arguments if any
    if (entry->entity_specs.default_argument_info != NULL)
    {
        int i;
        for (i = 0; i < entry->entity_specs.num_parameters; i++)
        {
            default_argument_info_t* argument_info = entry->entity_specs.default_argument_info[i];


            internal_error("Not yet implemented", 0);
#if 0
            if (argument_info != NULL
                    && !nodecl_is_null(argument_info->argument)
                    && nodecl_is_cxx_dependent_expr(argument_info->argument))
            {
                decl_context_t dummy;
                AST expr = nodecl_unwrap_cxx_dependent_expr(argument_info->argument, &dummy);
                expr = ast_copy_for_instantiation(expr);

                nodecl_t new_nodecl = nodecl_null();
                char c = check_expression(expr, instantiation_context, &new_nodecl);

                // Do not update anything on error
                if (!c)
                    continue;

                ERROR_CONDITION(nodecl_is_cxx_dependent_expr(new_nodecl), "Invalid dependent nodecl when updating default argument %d", i);

                argument_info->argument = new_nodecl;
                argument_info->context = instantiation_context;
            }
#endif
        }
    }
}

static void instantiate_emit_member_function(scope_entry_t* entry, const char* filename, int line)
{
    internal_error("Not yet implemented", 0);
#if 0
    ERROR_CONDITION(entry->kind != SK_FUNCTION, "Invalid function", 0);

    ERROR_CONDITION(!entry->entity_specs.is_non_emitted, "Invalid function is not yet nonemitted", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiation request of non-emitted (non template) function '%s' at '%s:%d\n",
                print_decl_type_str(entry->type_information, entry->decl_context, 
                    get_qualified_symbol_name(entry, entry->decl_context)),
                filename,
                line);
    }

    instantiate_default_arguments_of_function(entry);

    scope_entry_t* emission_template = entry->entity_specs.emission_template;
    ERROR_CONDITION(emission_template == NULL, "Invalid emission template\n", 0);

    if (!emission_template->defined)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "INSTANTIATION: Function not defined, do not emit\n");
        }
        return;
    }
    entry->defined = 0;

    int i;
    for (i = 0; i < num_being_instantiated_now; i++)
    {
        if (being_instantiated_now[i] == entry)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INSTANTIATION: This function is currently being emitted\n");
            }
            return;
        }
    }

    being_instantiated_now[num_being_instantiated_now] = entry;
    num_being_instantiated_now++;

    entry->entity_specs.is_non_emitted = 0;
    entry->entity_specs.emission_handler = NULL;
    entry->entity_specs.emission_template = NULL;

    // Remove dependent types
    AST dupl_function_definition = ast_copy_for_instantiation(emission_template->entity_specs.definition_tree);

    nodecl_t nodecl_function_code = nodecl_null();
    build_scope_function_definition(dupl_function_definition, 
            entry, 
            entry->decl_context, 
            // FIXME - This is not entirely true
            /* is_template */ 1,
            /* is_explicit_instantiation */ 1,
            &nodecl_function_code);

    entry->entity_specs.definition_tree = dupl_function_definition;

    nodecl_instantiation_units = nodecl_concat_lists(nodecl_instantiation_units,
            nodecl_function_code);

    num_being_instantiated_now--;
    being_instantiated_now[num_being_instantiated_now] = NULL;
#endif
}

AST instantiate_tree(AST orig_tree UNUSED_PARAMETER, 
        decl_context_t context_of_being_instantiated UNUSED_PARAMETER)
{
    internal_error("Not supported anymore", 0);
}

static void instantiate_every_symbol(scope_entry_t* entry,
        const char* filename,
        int line)
{
    if (entry != NULL
            && entry->kind == SK_FUNCTION)
    {
        if (is_template_specialized_type(entry->type_information))
        {
            instantiate_template_function_if_needed(entry, 
                    filename, line);
        }
        else if (entry->entity_specs.is_non_emitted)
        {
            (entry->entity_specs.emission_handler)(entry, filename, line);
        }
    }
}

