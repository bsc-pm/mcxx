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
#include "cxx-graphviz.h"
#include "cxx-diagnostic.h"
#include "cxx-codegen.h"

#include "cxx-printscope.h"

static scope_entry_t* add_duplicate_member_to_class(
        const decl_context_t* context_of_being_instantiated,
        type_t* being_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map,
        scope_entry_t* member_of_template)
{
    scope_entry_t* new_member = new_symbol(context_of_being_instantiated,
            context_of_being_instantiated->current_scope,
            member_of_template->symbol_name);

    *new_member = *member_of_template;
    symbol_clear_indirect_types(new_member);

    symbol_entity_specs_set_is_member(new_member, 1);
    symbol_entity_specs_set_is_instantiable(new_member, 0);
    symbol_entity_specs_set_is_member_of_anonymous(new_member, 0);
    new_member->decl_context = context_of_being_instantiated;
    symbol_entity_specs_set_class_type(new_member, being_instantiated);

    symbol_entity_specs_copy_default_argument_info_from(new_member, member_of_template);
    symbol_entity_specs_copy_related_symbols_from(new_member, member_of_template);
    symbol_entity_specs_copy_function_parameter_info_from(new_member, member_of_template);
    symbol_entity_specs_copy_gcc_attributes_from(new_member, member_of_template);
    symbol_entity_specs_copy_ms_attributes_from(new_member, member_of_template);

    // aligned attribute requires special treatment
    gcc_attribute_t gcc_aligned_attr;
    if (symbol_has_gcc_attribute(new_member, "aligned", &gcc_aligned_attr))
    {
        nodecl_t aligned_value = instantiate_expression_non_executable(
                nodecl_list_head(gcc_aligned_attr.expression_list),
                context_of_being_instantiated,
                instantiation_symbol_map, /* pack_index */ -1);

        gcc_aligned_attr.expression_list = nodecl_make_list_1(aligned_value);
        symbol_update_gcc_attribute(new_member, "aligned", gcc_aligned_attr);
    }

    class_type_add_member(get_actual_class_type(being_instantiated), new_member,
            member_of_template->decl_context,
            /* is_definition */ member_of_template->defined);

    return new_member;
}

typedef
struct translation_info_tag
{
    const decl_context_t* context_of_template;
    const decl_context_t* context_of_being_instantiated;
} translation_info_t;

static scope_entry_t* instantiate_template_type_member(type_t* template_type, 
        const decl_context_t* context_of_being_instantiated,
        scope_entry_t *member_of_template,
        type_t* being_instantiated, 
        char is_class,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map)
{
    scope_entry_t* template_symbol = template_type_get_related_symbol(template_type);
    template_parameter_list_t* template_parameters = template_type_get_template_parameters(template_type);

    template_parameter_list_t* updated_template_parameters = duplicate_template_argument_list(template_parameters);
    updated_template_parameters->enclosing = context_of_being_instantiated->template_parameters;

    decl_context_t* new_context_for_template_parameters = decl_context_clone(context_of_being_instantiated);
    new_context_for_template_parameters->template_parameters = updated_template_parameters;

    // Update the template parameters
    int i;
    for (i = 0; i < updated_template_parameters->num_parameters; i++)
    {
        if (updated_template_parameters->arguments[i] != NULL)
        {
            updated_template_parameters->arguments[i] = update_template_parameter_value(
                    template_parameters->arguments[i],
                    new_context_for_template_parameters, 
                    instantiation_symbol_map,
                    locus,
                    /* pack_index */ -1);


            if (updated_template_parameters->arguments[i] == NULL)
            {
                error_printf_at(locus, "could not instantiate template arguments of template type\n");
                return NULL;
            }

            // Preserve default parameters of template functions
            if (!is_class)
            {
                updated_template_parameters->arguments[i]->is_default = 
                    template_parameters->arguments[i]->is_default;
            }
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
                locus,
                instantiation_symbol_map,
                /* pack_index */ -1);
        if (is_error_type(base_type))
            return NULL;
    }

    scope_entry_t* new_member = new_symbol(new_context_for_template_parameters,
            new_context_for_template_parameters->current_scope,
            member_of_template->symbol_name);

    new_member->kind = SK_TEMPLATE;

    if (member_of_template->kind == SK_TEMPLATE_ALIAS)
    {
        new_member->type_information =
            get_new_template_alias_type(updated_template_parameters,
                    base_type,
                    new_member->symbol_name,
                    new_context_for_template_parameters,
                    member_of_template->locus);
    }
    else
    {
        new_member->type_information =
            get_new_template_type(updated_template_parameters,
                    base_type,
                    new_member->symbol_name,
                    new_context_for_template_parameters,
                    member_of_template->locus);
    }

    symbol_entity_specs_set_is_member(new_member, 1);
    symbol_entity_specs_set_class_type(new_member, being_instantiated);

    new_member->locus = member_of_template->locus;

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Adding new template to instantiation symbol map\n");
    }

    instantiation_symbol_map_add(instantiation_symbol_map, template_symbol, new_member);

    template_type_set_related_symbol(new_member->type_information, new_member);

    type_t* new_primary_template = template_type_get_primary_type(new_member->type_information);

    scope_entry_t* new_primary_symbol = named_type_get_symbol(new_primary_template);
    new_primary_symbol->decl_context = new_context_for_template_parameters;

    symbol_entity_specs_copy_from(
            new_primary_symbol,
            named_type_get_symbol(
                template_type_get_primary_type(
                    template_specialized_type_get_related_template_type(member_of_template->type_information))));

    symbol_entity_specs_set_is_instantiable(new_primary_symbol, 1);
    symbol_entity_specs_set_class_type(new_primary_symbol, being_instantiated);

    class_type_add_member(
            get_actual_class_type(being_instantiated),
            new_primary_symbol,
            new_primary_symbol->decl_context,
            /* is_definition */ 1);

    if (is_class)
    {
        // Fix some bits inherited from the original class type
        class_type_set_enclosing_class_type(get_actual_class_type(new_primary_template),
                get_actual_class_type(being_instantiated));
    }

    return new_member;
}

static void instantiate_nontemplate_member_class_of_template_class(
        scope_entry_t* being_instantiated_sym,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus);

static void instantiate_member(type_t* selected_template UNUSED_PARAMETER, 
        type_t* being_instantiated, 
        scope_entry_t* member_of_template, 
        const decl_context_t* context_of_being_instantiated,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map
        )
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating member '%s' at '%s'\n", 
                member_of_template->symbol_name,
                locus_to_str(member_of_template->locus));
    }

    switch (member_of_template->kind)
    {
        case SK_VARIABLE:
            {
                scope_entry_t* new_member;
                if (is_named_class_type(member_of_template->type_information)
                        && symbol_entity_specs_get_is_anonymous_union(
                            named_type_get_symbol(member_of_template->type_information)))
                {
                    // The union class has to be already mapped
                    scope_entry_t* new_class = instantiation_symbol_do_map(
                            instantiation_symbol_map,
                            named_type_get_symbol(member_of_template->type_information));

                    ERROR_CONDITION(new_class == NULL, "Anonymous union type not found in the map type!\n", 0);

                    instantiate_nontemplate_member_class_of_template_class(new_class, context_of_being_instantiated, locus);

                    scope_entry_t* anon_member = finish_anonymous_class(new_class, context_of_being_instantiated);

                    anon_member->type_information = get_user_defined_type(new_class);

                    // Add this member to the current class
                    symbol_entity_specs_set_is_member(anon_member, 1);
                    symbol_entity_specs_set_access(anon_member, symbol_entity_specs_get_access(new_class));
                    symbol_entity_specs_set_class_type(anon_member, get_user_defined_type(new_class));

                    class_type_add_member(being_instantiated, anon_member,
                            anon_member->decl_context, /* is_definition */ 1);

                    new_member = anon_member;
                }
                else
                {
                    new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                            being_instantiated,
                            instantiation_symbol_map,
                            member_of_template);

                    new_member->type_information = update_type_for_instantiation(
                            new_member->type_information,
                            context_of_being_instantiated,
                            member_of_template->locus,
                            instantiation_symbol_map,
                            /* pack_index */ -1);

                    if (is_error_type(new_member->type_information))
                        return;
                }

                // if (is_named_class_type(new_member->type_information))
                // {
                //     type_t* t = advance_over_typedefs(new_member->type_information);

                //     scope_entry_t* class_entry = named_type_get_symbol(t);
                //     class_type_complete_if_needed(class_entry, context_of_being_instantiated, locus);
                // }

                if (symbol_entity_specs_get_is_bitfield(new_member))
                {
                    symbol_entity_specs_set_bitfield_size(new_member, instantiate_expression(
                            symbol_entity_specs_get_bitfield_size(new_member),
                            context_of_being_instantiated,
                            instantiation_symbol_map,
                            /* pack_index */ -1));

                    // Evaluate the bitfield expression
                    if (nodecl_is_constant(symbol_entity_specs_get_bitfield_size(new_member)))
                    {
                        if (const_value_is_zero(
                                    const_value_gt(
                                        nodecl_get_constant(symbol_entity_specs_get_bitfield_size(new_member)),
                                        const_value_get_zero(/* bytes*/ 4, /* sign */ 1))))
                        {
                            error_printf_at(new_member->locus, "invalid bitfield of size '%d'\n",
                                    const_value_cast_to_4(
                                        nodecl_get_constant(symbol_entity_specs_get_bitfield_size(new_member))));

                            // Fix it to 1
                            symbol_entity_specs_set_bitfield_size(new_member, const_value_to_nodecl(const_value_get_signed_int(1)));
                        }

                        new_member->related_decl_context = context_of_being_instantiated;
                    }
                    else
                    {
                        error_printf_at(new_member->locus, "bitfield specification is not a constant expression");

                        // Fix it to 1
                        symbol_entity_specs_set_bitfield_size(new_member, const_value_to_nodecl(const_value_get_signed_int(1)));
                    }
                }

                if (!nodecl_is_null(member_of_template->value)
                        && symbol_entity_specs_get_is_defined_inside_class_specifier(member_of_template))
                {
                    nodecl_t new_expr = instantiate_expression(member_of_template->value,
                            context_of_being_instantiated,
                            instantiation_symbol_map, /* pack_index */ -1);

                    // Update the value of the new instantiated member
                    new_member->value = new_expr;

                    if (nodecl_get_kind(new_expr) == NODECL_CXX_INITIALIZER
                            || nodecl_get_kind(new_expr) == NODECL_CXX_EQUAL_INITIALIZER
                            || nodecl_get_kind(new_expr) == NODECL_CXX_PARENTHESIZED_INITIALIZER
                            || nodecl_get_kind(new_expr) == NODECL_CXX_BRACED_INITIALIZER)
                    {
                        check_nodecl_initialization(
                                new_expr,
                                context_of_being_instantiated,
                                new_member,
                                get_unqualified_type(new_member->type_information),
                                &new_member->value,
                                /* is_auto */ 0,
                                /* is_decltype_auto */ 0);
                    }
                    else
                    {
                        // No need to check anything
                    }
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: Member '%s' is a %s data member with type '%s'\n", 
                            new_member->symbol_name,
                            symbol_entity_specs_get_is_static(new_member) ? "static" : "non-static",
                            print_type_str(new_member->type_information, context_of_being_instantiated));
                }

                instantiation_symbol_map_add(instantiation_symbol_map,
                        member_of_template,
                        new_member);

                break;
            }
        case SK_TYPEDEF:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        instantiation_symbol_map,
                        member_of_template);

                new_member->type_information = update_type_for_instantiation(
                        new_member->type_information,
                        context_of_being_instantiated,
                        member_of_template->locus,
                        instantiation_symbol_map,
                        /* pack_index */ -1);
                if (is_error_type(new_member->type_information))
                    return;

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: Member '%s' is a typedef. Instantiated type is '%s'\n",
                            new_member->symbol_name,
                            print_type_str(new_member->type_information, context_of_being_instantiated));
                }
                ERROR_CONDITION(is_dependent_type(new_member->type_information),
                        "Invalid type '%s' (was '%s')",
                        print_declarator(new_member->type_information),
                        print_declarator(member_of_template->type_information));

                instantiation_symbol_map_add(instantiation_symbol_map,
                        member_of_template,
                        new_member);
                break;
            }
        case SK_ENUM:
            {
                scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                        being_instantiated,
                        instantiation_symbol_map,
                        member_of_template);

                char is_scoped = is_scoped_enum_type(member_of_template->type_information);

                new_member->type_information = get_new_enum_type(
                        context_of_being_instantiated,
                        is_scoped);

                const decl_context_t* new_enumerator_context = context_of_being_instantiated;

                CXX11_LANGUAGE()
                {
                    new_enumerator_context = new_class_context(new_enumerator_context,
                            new_member);
                    new_member->related_decl_context = new_enumerator_context;
                }

                const_value_t* max_value = NULL;
                const_value_t* min_value = NULL;

                char underlying_type_is_fixed =
                    enum_type_get_underlying_type_is_fixed(member_of_template->type_information);

                type_t* underlying_type = get_signed_int_type();

                int i, N = enum_type_get_num_enumerators(member_of_template->type_information);
                for (i = 0; i < N; i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(
                            member_of_template->type_information,
                            i);

                    scope_entry_t* new_enumerator = new_symbol(new_enumerator_context,
                            new_enumerator_context->current_scope,
                            enumerator->symbol_name);

                    new_enumerator->kind = SK_ENUMERATOR;
                    new_enumerator->locus = enumerator->locus;
                    new_enumerator->type_information = get_signed_int_type();
                    new_enumerator->value = instantiate_expression(
                            enumerator->value,
                            new_enumerator_context,
                            instantiation_symbol_map,
                            /* pack_index */ -1);

                    if (is_any_reference_type(nodecl_get_type(new_enumerator->value)))
                    {
                        new_enumerator->value =
                            cxx_nodecl_make_conversion(new_enumerator->value,
                                    no_ref(nodecl_get_type(new_enumerator->value)),
                                    new_enumerator_context,
                                    enumerator->locus);
                    }

                    ERROR_CONDITION(
                            !nodecl_is_constant(new_enumerator->value),
                            "This enumerator is not constant", 0);

                    new_enumerator->type_information = nodecl_get_type(new_enumerator->value);

                    enum_type_add_enumerator(new_member->type_information, new_enumerator);

                    if (!is_scoped)
                    {
                        // This is an unscoped enumerator
                        class_type_add_member(
                                get_actual_class_type(being_instantiated),
                                new_enumerator,
                                new_enumerator->decl_context,
                                /* is_definition */ 1);

                        symbol_entity_specs_set_is_member(new_enumerator, 1);
                        symbol_entity_specs_set_access(new_enumerator, symbol_entity_specs_get_access(enumerator));
                        symbol_entity_specs_set_class_type(new_enumerator, being_instantiated);
                        symbol_entity_specs_set_is_defined_inside_class_specifier(new_enumerator, 1);

                        CXX11_LANGUAGE()
                        {
                            insert_entry(context_of_being_instantiated->current_scope,
                                    new_enumerator);
                        }
                    }

                    instantiation_symbol_map_add(
                            instantiation_symbol_map,
                            enumerator,
                            new_enumerator);

                    if (!underlying_type_is_fixed)
                    {
#define B_(x) const_value_is_nonzero(x)
                        const_value_t* current_value = nodecl_get_constant(new_enumerator->value);

                        if (min_value == NULL
                                || B_(const_value_lt(current_value, min_value)))
                        {
                            min_value = current_value;
                        }
                        if (max_value == NULL
                                || B_(const_value_lt(max_value, current_value)))
                        {
                            max_value = current_value;
                        }
#undef B_

                        if (min_value == NULL)
                            min_value = const_value_get_unsigned_int(0);
                        if (max_value == NULL)
                            max_value = const_value_get_unsigned_int(0);

                        underlying_type = compute_underlying_type_enum(min_value, max_value, underlying_type,
                                CURRENT_CONFIGURATION->code_shape.short_enums);
                    }
                }

                if (underlying_type_is_fixed)
                {
                    underlying_type =
                        update_type_for_instantiation(
                                enum_type_get_underlying_type(member_of_template->type_information),
                                context_of_being_instantiated,
                                member_of_template->locus,
                                instantiation_symbol_map,
                                /* pack_index */ -1);
                }

                enum_type_set_underlying_type(new_member->type_information,
                        underlying_type);

                // Fix the type of the new enumerators to be enum and not integer
                N = enum_type_get_num_enumerators(new_member->type_information);
                for (i = 0; i < N; i++)
                {
                    scope_entry_t* enumerator = enum_type_get_enumerator_num(
                            new_member->type_information,
                            i);

                    enumerator->type_information = get_user_defined_type(new_member);
                }

                set_is_complete_type(new_member->type_information, 1);
                set_is_complete_type(get_user_defined_type(new_member), 1);

                instantiation_symbol_map_add(instantiation_symbol_map,
                        member_of_template,
                        new_member);
                break;
            }
        case SK_ENUMERATOR:
            {
                // Already handled in SK_ENUM
                break;
            }
        case SK_CLASS:
            {
                if (!is_template_specialized_type(member_of_template->type_information))
                {
                    scope_entry_t* new_member = add_duplicate_member_to_class(context_of_being_instantiated,
                            being_instantiated,
                            instantiation_symbol_map,
                            member_of_template);

                    new_member->type_information = get_new_class_type(
                            context_of_being_instantiated,
                            class_type_get_class_kind(member_of_template->type_information));

                    class_type_set_enclosing_class_type(
                            get_actual_class_type(new_member->type_information),
                            get_actual_class_type(being_instantiated));
                    set_is_complete_type(new_member->type_information,
                            is_complete_type(member_of_template->type_information));

                    // Use this when completing this class
                    symbol_entity_specs_set_emission_template(new_member, member_of_template);

                    // Do not share these
                    symbol_entity_specs_free_related_symbols(new_member);

                    instantiation_symbol_map_add(instantiation_symbol_map,
                            member_of_template,
                            new_member);
                }
                else
                {
                    type_t* template_type = template_specialized_type_get_related_template_type(member_of_template->type_information);
                    type_t* primary_template = template_type_get_primary_type(template_type);

                    if (named_type_get_symbol(primary_template)->type_information == member_of_template->type_information)
                    {
                        scope_entry_t* new_member = instantiate_template_type_member(template_type,
                                context_of_being_instantiated,
                                member_of_template,
                                being_instantiated,
                                /* is_class */ 1,
                                locus,
                                instantiation_symbol_map);
                        if (new_member == NULL)
                            return;
                    }
                    else
                    {
                        scope_entry_t* template_name = template_type_get_related_symbol(template_type);

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "INSTANTIATION: Searching template name in instantiation symbol map\n");
                        }
                        scope_entry_t* instantiated_template_name =
                            instantiation_symbol_do_map(instantiation_symbol_map, template_name);

                        ERROR_CONDITION(instantiated_template_name == NULL,
                                "Instantiated template name not found", 0);

                        template_parameter_list_t *template_params = duplicate_template_argument_list(
                                template_specialized_type_get_template_parameters(member_of_template->type_information));

                        template_params->enclosing = context_of_being_instantiated->template_parameters;

                        template_parameter_list_t *template_args = duplicate_template_argument_list(
                                template_specialized_type_get_template_arguments(member_of_template->type_information));

                        template_args->enclosing = context_of_being_instantiated->template_parameters;

                        int i;
                        for (i = 0; i < template_args->num_parameters; i++)
                        {
                            template_args->arguments[i] = update_template_parameter_value(
                                    template_args->arguments[i],
                                    context_of_being_instantiated,
                                    instantiation_symbol_map,
                                    locus,
                                    /* pack_index */ -1);
                        }

                        // Now ask a new specialization
                        type_t* new_template_specialized_type =
                            template_type_get_specialized_type_for_instantiation(
                                    instantiated_template_name->type_information,
                                    template_args,
                                    member_of_template->type_information,
                                    context_of_being_instantiated,
                                    member_of_template->locus);

                        scope_entry_t* specialized_class_sym = 
                                named_type_get_symbol(new_template_specialized_type);
                        template_specialized_type_update_template_parameters(
                                specialized_class_sym->type_information,
                                template_params);
                        template_specialized_type_update_template_parameters(
                                class_symbol_get_canonical_symbol(specialized_class_sym)->type_information,
                                template_params);

                        symbol_entity_specs_set_is_instantiable(specialized_class_sym, 1);
                        symbol_entity_specs_set_is_user_declared(specialized_class_sym, 0);

                        class_type_add_member(
                                get_actual_class_type(being_instantiated),
                                specialized_class_sym,
                                specialized_class_sym->decl_context,
                                specialized_class_sym->defined
                                );
                    }
                }
                break;
            }
        case SK_TEMPLATE_ALIAS:
            {
                type_t* template_type = template_specialized_type_get_related_template_type(member_of_template->type_information);
                // type_t* primary_template = template_type_get_primary_type(template_type);

                scope_entry_t* new_member = instantiate_template_type_member(template_type,
                        context_of_being_instantiated,
                        member_of_template,
                        being_instantiated,
                        /* is_class */ 0,
                        locus,
                        instantiation_symbol_map);
                if (new_member == NULL)
                    return;

                instantiation_symbol_map_add(instantiation_symbol_map,
                        member_of_template,
                        new_member);
                break;
            }
        case SK_TEMPLATE:
            {
                // We do not keep these as class members, always their specializations
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
                            instantiation_symbol_map,
                            member_of_template);

                    new_member->type_information = update_type_for_instantiation(
                            new_member->type_information,
                            context_of_being_instantiated,
                            member_of_template->locus,
                            instantiation_symbol_map,
                            /* pack_index */ -1);

                    if (is_error_type(new_member->type_information))
                        return;

                    // Note that at this point the related entries of the new member are
                    // the same as the original member.
                    //
                    // With C++11, the number of related entries of the new member may
                    // differ from the original one, so it's important to create a new
                    // list of symbols at this point.
                    //
                    // (this code has been copied from cxx-instantiation.c)
                    {
                        // 1. Remove the default argument info from the new member
                        symbol_entity_specs_free_default_argument_info(new_member);

                        // 2. Reserve enough space to store the default argument info of the new member
                        int total_num_parameters =
                            function_type_get_num_parameters(new_member->type_information);
                        symbol_entity_specs_reserve_default_argument_info(new_member, total_num_parameters);

                        // 3. Create new parameters for the new function

                        // 3.1 This new parameters are going to be declared in a
                        //     prototype scope (as if it was a declaration)
                        const decl_context_t* new_decl_context =
                            new_prototype_context(context_of_being_instantiated);

                        int num_ori_param, num_new_param = 0;
                        for (num_ori_param = 0;
                                num_ori_param < symbol_entity_specs_get_num_related_symbols(member_of_template);
                                num_ori_param++)
                        {
                            scope_entry_t* ori_param =
                                symbol_entity_specs_get_related_symbols_num(member_of_template, num_ori_param);

                            scope_entry_t* new_param = new_symbol(new_decl_context,
                                    new_decl_context->current_scope,
                                    ori_param->symbol_name);

                            new_param->kind = ori_param->kind;
                            new_param->type_information =
                                update_type_for_instantiation(
                                        ori_param->type_information,
                                        context_of_being_instantiated,
                                        member_of_template->locus,
                                        instantiation_symbol_map,
                                        /* pack */ -1);

                            // WARNING - This is a usual source of issues
                            symbol_entity_specs_copy_from(new_param, ori_param);
                            symbol_entity_specs_free_function_parameter_info(new_param);

                            if (ori_param->kind == SK_VARIABLE)
                            {
                                new_param->value = instantiate_expression(ori_param->value,
                                        new_decl_context,
                                        instantiation_symbol_map,
                                        /* pack_index */ -1);

                                symbol_entity_specs_add_related_symbols(new_member, new_param);
                                symbol_set_as_parameter_of_function(new_param,
                                        new_member,
                                        /* nesting */ 0, /* position */ num_new_param);

                                symbol_entity_specs_set_default_argument_info_num(
                                        new_member,
                                        num_new_param,
                                        symbol_entity_specs_get_default_argument_info_num(
                                            member_of_template,
                                            num_ori_param));

                                default_argument_info_t* current_default_arg =
                                    symbol_entity_specs_get_default_argument_info_num(new_member, num_new_param);
                                if (current_default_arg != NULL
                                        && CURRENT_CONFIGURATION->explicit_instantiation)
                                {
                                    current_default_arg->is_hidden = 1;
                                }

                                num_new_param++;
                            }
                            else if (ori_param->kind == SK_VARIABLE_PACK)
                            {
                                int num_types = sequence_of_types_get_num_types(new_param->type_information);

                                nodecl_t nodecl_sym_list = nodecl_null();

                                int num_sub_parameter;
                                for (num_sub_parameter = 0; num_sub_parameter < num_types; num_sub_parameter++)
                                {
                                    type_t* t = sequence_of_types_get_type_num(new_param->type_information,
                                            num_sub_parameter);

                                    const char* c = NULL;
                                    uniquestr_sprintf(
                                            &c, "_%s__%d",
                                            ori_param->symbol_name,
                                            num_sub_parameter);

                                    scope_entry_t* new_sub_parameter = new_symbol(new_decl_context,
                                            new_decl_context->current_scope,
                                            c);

                                    new_sub_parameter->kind = SK_VARIABLE;
                                    new_sub_parameter->type_information = t;

                                    // WARNING - This is a usual source of issues
                                    symbol_entity_specs_copy_from(new_sub_parameter, ori_param);
                                    symbol_entity_specs_free_function_parameter_info(new_sub_parameter);

                                    symbol_entity_specs_add_related_symbols(new_member, new_sub_parameter);

                                    symbol_set_as_parameter_of_function(new_sub_parameter,
                                            new_member,
                                            /* nesting */ 0, /* position */ num_new_param);
                                    num_new_param++;
                                }

                                new_param->value = nodecl_sym_list;
                            }
                        }
                    }

                    new_member->defined = 0;
                    symbol_entity_specs_set_function_code(new_member, nodecl_null());
                    symbol_entity_specs_set_is_instantiable(new_member, 1);
                    symbol_entity_specs_set_emission_template(new_member, member_of_template);
                    symbol_entity_specs_set_function_code(new_member, nodecl_null());
                    // Do not share these
                    symbol_entity_specs_free_related_symbols(new_member);
                }
                else
                {
                    type_t* template_type = template_specialized_type_get_related_template_type(
                            member_of_template->type_information);
                    type_t* primary_template = template_type_get_primary_type(template_type);
                    scope_entry_t* primary_template_sym = named_type_get_symbol(primary_template);

                    if (primary_template_sym->type_information != member_of_template->type_information)
                    {
                        internal_error("Code unreachable\n", 0);
                    }

                    new_member = instantiate_template_type_member(template_type,
                            context_of_being_instantiated,
                            member_of_template,
                            being_instantiated,
                            /* is_class */ 0,
                            locus,
                            instantiation_symbol_map);

                    if (new_member == NULL)
                        return;

                    // We work on the primary template
                    type_t* primary_type = template_type_get_primary_type(new_member->type_information);
                    new_member = named_type_get_symbol(primary_type);

                    new_member->defined = 0;
                    symbol_entity_specs_set_function_code(new_member, nodecl_null());
                    symbol_entity_specs_set_is_instantiable(new_member, 1);
                    symbol_entity_specs_set_emission_template(new_member, primary_template_sym);
                    symbol_entity_specs_set_is_inline(new_member, symbol_entity_specs_get_is_inline(primary_template_sym));
                    symbol_entity_specs_set_is_static(new_member, symbol_entity_specs_get_is_static(primary_template_sym));
                    symbol_entity_specs_set_is_user_declared(new_member, symbol_entity_specs_get_is_user_declared(primary_template_sym));
                    symbol_entity_specs_set_is_defined_inside_class_specifier(new_member,
                            symbol_entity_specs_get_is_defined_inside_class_specifier(primary_template_sym));
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "INSTANTIATION: New member function '%s'\n",
                            print_decl_type_str(new_member->type_information, 
                                new_member->decl_context,
                                get_qualified_symbol_name(new_member, 
                                    new_member->decl_context)));
                }

                symbol_entity_specs_set_is_copy_constructor(new_member,
                        function_is_copy_constructor(new_member, being_instantiated));
                symbol_entity_specs_set_is_copy_assignment_operator(new_member,
                        function_is_copy_assignment_operator(new_member, being_instantiated));

                CXX11_LANGUAGE()
                {
                    symbol_entity_specs_set_is_move_constructor(new_member,
                            function_is_move_constructor(new_member, being_instantiated));
                    symbol_entity_specs_set_is_move_assignment_operator(new_member,
                            function_is_move_assignment_operator(new_member, being_instantiated));
                }

                if (symbol_entity_specs_get_is_constructor(member_of_template))
                {
                    if (symbol_entity_specs_get_is_default_constructor(member_of_template))
                    {
                        class_type_set_default_constructor(get_actual_class_type(being_instantiated), new_member);
                    }
                }
                if (symbol_entity_specs_get_is_destructor(member_of_template))
                {
                    class_type_set_destructor(get_actual_class_type(being_instantiated), new_member);
                }

                hide_using_declarations(get_actual_class_type(being_instantiated), new_member);

                instantiation_symbol_map_add(instantiation_symbol_map,
                        member_of_template,
                        new_member);

                break;
            }
        case SK_USING:
        case SK_USING_TYPENAME:
            {
                // Two cases: a) the entity is actually dependent: it will have only one SK_DEPENDENT_ENTITY 
                //            b) the entity is not dependent: it may have more than one element
                scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(member_of_template->type_information);
                scope_entry_t* entry = entry_list_head(entry_list);

                if (entry->kind == SK_DEPENDENT_ENTITY)
                {
                    ERROR_CONDITION(entry_list_size(entry_list) != 1, "Invalid list", 0);

                    entry_list = query_dependent_entity_in_context(context_of_being_instantiated,
                            entry,
                            /* pack_index */ -1,
                            NULL,
                            instantiation_symbol_map,
                            member_of_template->locus);
                }

                if (entry_list == NULL)
                    return;

                introduce_using_entities_in_class(
                        nodecl_null(),
                        entry_list, context_of_being_instantiated,
                        named_type_get_symbol(being_instantiated),
                        symbol_entity_specs_get_access(member_of_template),
                        /* is_typename */ 0,
                        member_of_template->locus);
                break;
            }
        case SK_MEMBER_STATIC_ASSERT:
            {
                nodecl_t nodecl_cxx_static_assert = member_of_template->value;
                ERROR_CONDITION(nodecl_get_kind(nodecl_cxx_static_assert) != NODECL_CXX_STATIC_ASSERT, "Invalid node", 0);

                nodecl_t nodecl_predicate = nodecl_get_child(nodecl_cxx_static_assert, 0);
                nodecl_t nodecl_message = nodecl_get_child(nodecl_cxx_static_assert, 0);

                nodecl_predicate = instantiate_expression(
                        nodecl_predicate,
                        context_of_being_instantiated,
                        instantiation_symbol_map,
                        /* pack_index */ -1);

                nodecl_t nodecl_static_assert = nodecl_null();
                build_scope_nodecl_static_assert(
                        nodecl_predicate,
                        nodecl_message,
                        context_of_being_instantiated,
                        &nodecl_static_assert);
                ERROR_CONDITION(!nodecl_is_null(nodecl_static_assert)
                        && !nodecl_is_err_expr(nodecl_static_assert), "Invalid node", 0);

                break;
            }
        default:
            {
                internal_error("Unexpected member kind=%s\n", symbol_kind_name(member_of_template));
            }
    }
}

static void instantiate_dependent_friend_class(
        type_t* being_instantiated UNUSED_PARAMETER,
        scope_entry_t* friend UNUSED_PARAMETER,
        const decl_context_t* context_of_being_instantiated UNUSED_PARAMETER,
        instantiation_symbol_map_t* instantiation_symbol_map UNUSED_PARAMETER,
        const locus_t* locus UNUSED_PARAMETER)
{
    // FIXME - Not yet implemented. It requires the type be readjusted to the proper template nesting
    // See below
#if 0
    const char* declared_name = NULL;
    type_t* declared_type = NULL;

    if (is_unnamed_class_type(friend->type_information)
            || is_template_type(friend->type_information))
    {
        declared_name = friend->symbol_name;
        declared_type = friend->type_information;
    }
    else
    {
        declared_type = update_type_for_instantiation(
                    friend->type_information,
                    context_of_being_instantiated, friend->locus,
                    instantiation_symbol_map,
                    /* pack_index */ -1);
    }

    // Here we need to readjust everything to the new context
    //
    // Rationale:
    //    template <typename T>
    //    struct A
    //    {
    //         template <typename S> // Here S has coordinates nesting=2, position=0
    //         friend struct B;
    //    };
    //
    //    template <typename S>  // Here S has coordinates nesting=1, position=0
    //    struct B { };
    //
    //    A<int> a;
    //
    // When instantiating A<int>, now the friend 'B' must be adjusted so nesting is not 2 but 1
    build_scope_friend_class_declaration(
            declared_type,
            declared_name,
            context_of_being_instantiated,
            locus);
#endif
}

static void instantiate_dependent_friend_function(
        type_t* being_instantiated,
        scope_entry_t* friend,
        const decl_context_t* context_of_being_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus)
{
    // At the end of this function, the symbol 'new_function' will be added to
    // the set of friends
    scope_entry_t* new_function = NULL;

    type_t* new_type = update_type_for_instantiation(friend->type_information,
            context_of_being_instantiated, friend->locus,
            instantiation_symbol_map,
            /* pack_index */ -1);

    if (is_error_type(new_type))
        return;

    char is_template_id = nodecl_name_ends_in_template_id(friend->value);
    char is_templ_funct_decl =(is_template_specialized_type(friend->type_information) &&
            friend->decl_context->template_parameters->parameters != context_of_being_instantiated->template_parameters->parameters);
    char is_qualified = (nodecl_get_kind(friend->value) == NODECL_CXX_DEP_NAME_NESTED
            || nodecl_get_kind(friend->value) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED);

    if (is_template_id)
    {
        // FIXME - Try to avoid this sort of dance
        scope_entry_t** symbols = NEW_VEC0(
                scope_entry_t*,
                symbol_entity_specs_get_num_friend_candidates(friend));
        int i;
        for (i = 0; i < symbol_entity_specs_get_num_friend_candidates(friend); i++)
        {
            symbols[i] = symbol_entity_specs_get_friend_candidates_num(friend, i);
        }

        scope_entry_list_t* candidates_list =
            entry_list_from_symbol_array(
                    symbol_entity_specs_get_num_friend_candidates(friend),
                    symbols);
        DELETE(symbols);

        // Does candidates list contain a SK_DEPENDENT_ENTITY?
        if (candidates_list != NULL)
        {
            scope_entry_t* sym = entry_list_head(candidates_list);
            if (sym->kind == SK_DEPENDENT_ENTITY)
            {
                entry_list_free(candidates_list);

                // Try to solve the dependent entity
                candidates_list =
                    query_dependent_entity_in_context(
                        context_of_being_instantiated, sym,
                        /* pack_index */ -1,
                        NULL,
                        instantiation_symbol_map,
                        locus);
            }
        }

        if (candidates_list != NULL)
        {
            template_parameter_list_t* explicit_temp_params = NULL;
            nodecl_t new_name = instantiate_expression(friend->value, context_of_being_instantiated,
                    instantiation_symbol_map, /* pack_index */ -1);

            if (nodecl_name_ends_in_template_id(new_name))
            {
                explicit_temp_params = nodecl_name_get_last_template_arguments(new_name);
            }
            else
            {
                type_t* nodecl_type = nodecl_get_type(new_name);
                ERROR_CONDITION(nodecl_type == NULL || !is_unresolved_overloaded_type(nodecl_type), "Code unreachable\n", 0);
                explicit_temp_params = unresolved_overloaded_type_get_explicit_template_arguments(nodecl_type);
            }

            // We may need to update the explicit template argument list
            template_parameter_list_t* updated_explicit_temp_params = NULL;
            if (explicit_temp_params != NULL)
            {
                updated_explicit_temp_params = update_template_argument_list(
                        context_of_being_instantiated, explicit_temp_params,
                        instantiation_symbol_map,
                        locus,
                        /* pack_index */ -1);
            }

            scope_entry_list_t* new_friend_list = solve_template_function_in_declaration(
                    candidates_list, updated_explicit_temp_params, new_type, locus);

            if (new_friend_list != NULL)
            {
                if (entry_list_size(new_friend_list) == 1)
                {
                    new_function = entry_list_head(new_friend_list);
                }
                else
                {
                    error_printf_at(locus, "friend function declaration is ambiguous '%s'\n", friend->symbol_name);

                    scope_entry_list_iterator_t* it = NULL;
                    for (it = entry_list_iterator_begin(new_friend_list);
                            !entry_list_iterator_end(it);
                            entry_list_iterator_next(it))
                    {
                        scope_entry_t* current_entry = entry_list_iterator_current(it);

                        info_printf_at(current_entry->locus, "%s\n",
                                print_decl_type_str(current_entry->type_information, current_entry->decl_context, 
                                    get_qualified_symbol_name(current_entry, current_entry->decl_context)));
                    }
                    entry_list_iterator_free(it);
                }
                entry_list_free(new_friend_list);
            }
        }

        if (new_function == NULL)
        {
            error_printf_at(locus, "function '%s' shall refer a specialization of a function template\n", friend->symbol_name);
            return;
        }
        entry_list_free(candidates_list);
    }
    else
    {
        decl_flags_t decl_flags = DF_DEPENDENT_TYPENAME;
        if (!is_qualified)
        {
            decl_flags |= DF_ONLY_CURRENT_SCOPE;
        }

        decl_context_t* lookup_context = decl_context_clone(context_of_being_instantiated);
        lookup_context->current_scope = lookup_context->namespace_scope;

        scope_entry_list_t* candidates_list =
            query_nodecl_name_flags(lookup_context, friend->value, NULL, decl_flags);

        // Does candidates list contain a SK_DEPENDENT_ENTITY?
        if (candidates_list != NULL)
        {
            scope_entry_t* sym = entry_list_head(candidates_list);
            if (sym->kind == SK_DEPENDENT_ENTITY)
            {
                entry_list_free(candidates_list);

                // Try to solve the dependent entity
                candidates_list =
                    query_dependent_entity_in_context(
                            context_of_being_instantiated, sym,
                            /* pack_index */ -1, NULL,
                            instantiation_symbol_map,
                            locus);
            }
        }

        // 1. The declaration is not a template function
        if (!is_templ_funct_decl)
        {
            ERROR_CONDITION(is_dependent_type(new_type),
                    "At this point, the type '%s' cannot be dependent\n",
                    print_declarator(new_type));

            // 1.1 It's a qualified or unqualified template-id -> refers to a
            // specialization of a function template
            // This case has been already handled

            scope_entry_list_t* filtered_entry_list =
                filter_symbol_kind(candidates_list, SK_FUNCTION);

            // 1.2 It's a qualified/unqualified name -> refers to a nontemplate function
            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(filtered_entry_list);
                    !entry_list_iterator_end(it) && new_function == NULL;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* sym_candidate = entry_list_iterator_current(it);
                if (sym_candidate->kind == SK_FUNCTION
                        && equivalent_types(new_type, sym_candidate->type_information))
                {
                    new_function = sym_candidate;
                }
            }
            entry_list_iterator_free(it);
            entry_list_free(filtered_entry_list);

            //  1.3 It's a qualified name and we have not found a candidate in 1.2 ->
            //  refers to a matching specialization of a template function
            if (new_function == NULL && is_qualified)
            {
                nodecl_t new_name = instantiate_expression(friend->value, context_of_being_instantiated,
                        instantiation_symbol_map, /* pack_index */ -1);

                // We may need to update the explicit template argument list
                template_parameter_list_t* expl_templ_param = NULL;
                template_parameter_list_t* nodecl_templ_param =
                    nodecl_name_get_last_template_arguments(new_name);
                if (nodecl_templ_param != NULL)
                {
                    expl_templ_param = update_template_argument_list(
                            context_of_being_instantiated, nodecl_templ_param,
                            instantiation_symbol_map,
                            locus,
                            /* pack_index */ -1);
                }

                scope_entry_list_t* new_friend_list = solve_template_function_in_declaration(
                        candidates_list, expl_templ_param, new_type, locus);

                if (new_friend_list != NULL)
                {
                    if (entry_list_size(new_friend_list) == 1)
                    {
                        new_function = entry_list_head(new_friend_list);
                    }
                    else
                    {
                        error_printf_at(locus, "friend function declaration is ambiguous '%s'\n", friend->symbol_name);

                        for (it = entry_list_iterator_begin(new_friend_list);
                                !entry_list_iterator_end(it);
                                entry_list_iterator_next(it))
                        {
                            scope_entry_t* current_entry = entry_list_iterator_current(it);

                            info_printf_at(current_entry->locus, "%s\n",
                                    print_decl_type_str(current_entry->type_information, current_entry->decl_context, 
                                        get_qualified_symbol_name(current_entry, current_entry->decl_context)));
                        }
                        entry_list_iterator_free(it);
                    }
                    entry_list_free(new_friend_list);
                }

                if (new_function == NULL)
                {
                    error_printf_at(locus, "function '%s' shall refer a nontemplate function or a specialization of a function template\n", friend->symbol_name);
                    return;
                }
            }

            //  1.4 It's a unqualified name and we have not found a candidate in 1.2 -> declares a non template function
            if (new_function == NULL && !is_qualified)
            {
                // A few interesting details:
                //  - The new friend symbol must be created in the innermost enclosing namespace scope
                //  - This new friend has not template parameters
                decl_context_t *declaration_context = decl_context_clone(context_of_being_instantiated);
                declaration_context->current_scope = declaration_context->namespace_scope;
                declaration_context->template_parameters = NULL;

                new_function = new_symbol(declaration_context, declaration_context->current_scope, friend->symbol_name);

                new_function->kind = SK_FUNCTION;
                new_function->locus = locus;
                new_function->type_information = new_type;
                symbol_entity_specs_copy_from(new_function, friend);
                new_function->defined = friend->defined;
            }
        }
        // 2. Otherwise, It is a template function declaration
        else
        {
            ERROR_CONDITION(!is_dependent_type(new_type),
                    "At this point, this type must be dependent\n", 0);

            // We need to alineate the template parameters of the updated type
            // Example:
            //
            //       template < typename _T1 >
            //       struct A
            //       {
            //           template < typename _T2 >
            //           friend void foo( _T1, _T2);
            //       };
            //
            //       template < typename _T >
            //       void foo (int, _T)
            //       {
            //       }
            //
            //       A<int> a;
            //
            // The instantiation of the depedent friend function declaration
            // shall refer to ::foo, but currently the template parameters of
            // the instantiated type are not correctly alineated and do not
            // match with the template parameters of ::foo.
            //
            // Idea: We duplicate the template parameter list and
            // replace all SK_TEMPLATE_{TYPE,NONTYPE,TEMPLATE}_PARAMETER by a new symbol with
            // template_parameter_nesting = 1. Later, we update the new_type
            // with this new list of template parameters

            char something_has_changed = 0;
            template_parameter_list_t* alineated_temp_params =
                duplicate_template_argument_list(friend->decl_context->template_parameters);
            int i;
            for (i = 0; i < alineated_temp_params->num_parameters; ++i)
            {
                template_parameter_t* current_temp_param = alineated_temp_params->parameters[i];
                if (current_temp_param->entry != NULL
                        && (current_temp_param->entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                        || current_temp_param->entry->kind == SK_TEMPLATE_NONTYPE_PARAMETER
                        || current_temp_param->entry->kind == SK_TEMPLATE_TEMPLATE_PARAMETER))
                {
                    something_has_changed = 1;

                    scope_entry_t* new_entry = NEW0(scope_entry_t);
                    memcpy(new_entry, current_temp_param->entry, sizeof(*current_temp_param->entry));
                    symbol_clear_indirect_types(new_entry);
                    symbol_entity_specs_set_template_parameter_nesting(new_entry, 1);
                    current_temp_param->entry = new_entry;
                }
            }

            if (something_has_changed)
            {
                decl_context_t* new_context = decl_context_clone(context_of_being_instantiated);
                new_context->template_parameters = alineated_temp_params;
                type_t* alineated_type = update_type_for_instantiation(new_type,
                        new_context,
                        friend->locus,
                        instantiation_symbol_map,
                        /* pack_index */ -1);

                if (alineated_type == NULL)
                    return;

                new_type = alineated_type;
            }

            scope_entry_list_t* filtered_entry_list = filter_symbol_kind(candidates_list, SK_TEMPLATE);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(filtered_entry_list);
                    !entry_list_iterator_end(it) && new_function == NULL;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* template_candidate = entry_list_iterator_current(it);
                scope_entry_t* primary_symbol_candidate =
                    named_type_get_symbol(template_type_get_primary_type(template_candidate->type_information));

                if (primary_symbol_candidate->kind == SK_FUNCTION
                        && equivalent_types(new_type, primary_symbol_candidate->type_information))
                {
                    new_function = primary_symbol_candidate;
                }
            }
            entry_list_iterator_free(it);
            entry_list_free(filtered_entry_list);

            if (new_function == NULL)
            {
                if (is_qualified)
                {
                    error_printf_at(locus, "template function '%s' shall refer a declared template function\n", friend->symbol_name);
                    return;
                }
                else
                {
                    // We create a new SK_TEMPLATE symbol
                    decl_context_t* new_decl_context = decl_context_clone(context_of_being_instantiated);
                    new_decl_context->current_scope = context_of_being_instantiated->namespace_scope;

                    scope_entry_t* new_template = new_symbol(new_decl_context,
                            new_decl_context->current_scope, friend->symbol_name);

                    new_template->kind = SK_TEMPLATE;
                    new_template->locus = locus;

                    // The new template symbol created to represent the
                    // instantiation of the current template friend function
                    // only needs the last level of template parameters (the
                    // others should be independent)
                    alineated_temp_params->enclosing = NULL;
                    lookup_context->template_parameters = alineated_temp_params;

                    // We create the new_type of the new template symbol
                    new_template->type_information = get_new_template_type(alineated_temp_params,
                            new_type, new_template->symbol_name, lookup_context, locus);

                    template_type_set_related_symbol(new_template->type_information, new_template);

                    // We copy the entity specs of the friend to the primary specialization of this new template
                    type_t* new_primary_type = template_type_get_primary_type(new_template->type_information);
                    scope_entry_t* new_primary_symbol = named_type_get_symbol(new_primary_type);
                    symbol_entity_specs_copy_from(new_primary_symbol, friend);

                    // We never add the template symbol as a friend of the class
                    // being instantiated, we always add the primary specialization
                    new_function = new_primary_symbol;
                }
            }

        }
        entry_list_free(candidates_list);
    }

    scope_entry_t* new_friend = NEW0(scope_entry_t);
    new_friend->kind = SK_FRIEND_FUNCTION;
    new_friend->decl_context = context_of_being_instantiated;
    symbol_entity_specs_set_alias_to(new_friend, new_function);

    class_type_add_friend_symbol(get_actual_class_type(being_instantiated), new_friend);
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        const decl_context_t* context_of_being_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus);

static void instantiate_class_common(
        scope_entry_t* being_instantiated_sym,
        type_t* being_instantiated,
        scope_entry_t* selected_template_sym,
        type_t* selected_template,
        instantiation_symbol_map_t* enclosing_instantiation_symbol_map,
        const decl_context_t* inner_decl_context,
        const locus_t* locus)
{
    instantiation_symbol_map_t* instantiation_symbol_map =
        instantiation_symbol_map_push(enclosing_instantiation_symbol_map);
    symbol_entity_specs_set_instantiation_symbol_map(being_instantiated_sym, instantiation_symbol_map);

    instantiate_bases(
            get_actual_class_type(selected_template),
            get_actual_class_type(being_instantiated),
            inner_decl_context,
            instantiation_symbol_map,
            being_instantiated_sym->locus
            );

    if (!symbol_entity_specs_get_is_unnamed(being_instantiated_sym))
    {
        // Inject the class name
        scope_entry_t* injected_symbol = new_symbol(inner_decl_context,
                inner_decl_context->current_scope, being_instantiated_sym->symbol_name);

        *injected_symbol = *being_instantiated_sym;
        // the injected class name is logically in the class-scope
        injected_symbol->decl_context = inner_decl_context;
        injected_symbol->do_not_print = 1;

        symbol_entity_specs_set_is_member(injected_symbol, 1);
        symbol_entity_specs_set_class_type(injected_symbol, get_user_defined_type(being_instantiated_sym));
        symbol_entity_specs_set_is_injected_class_name(injected_symbol, 1);

        // Map the original injected class name to the new one
        scope_entry_list_t* entry_list = 
            query_in_scope_str(class_type_get_inner_context(selected_template),
                    selected_template_sym->symbol_name,
                    NULL);
        ERROR_CONDITION((entry_list == NULL), "Injected class name not found", 0);
        scope_entry_t* original_injected_class_name = entry_list_head(entry_list);

        ERROR_CONDITION(original_injected_class_name == NULL
                || !symbol_entity_specs_get_is_injected_class_name(original_injected_class_name),
                "Injected class name not found", 0);

        instantiation_symbol_map_add(instantiation_symbol_map, original_injected_class_name, injected_symbol);

        entry_list_free(entry_list);

        register_symbol_this_in_class_scope(being_instantiated_sym);
    }

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
                locus,
                instantiation_symbol_map);
    }
    entry_list_iterator_free(it);
    entry_list_free(members);

    // Friends
    for (it = entry_list_iterator_begin(friends);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* friend = entry_list_iterator_current(it);

        if (friend->kind == SK_DEPENDENT_FRIEND_FUNCTION)
        {
            instantiate_dependent_friend_function(being_instantiated,
                    friend, inner_decl_context,
                    instantiation_symbol_map,
                    locus);
        }
        else if (friend->kind == SK_DEPENDENT_FRIEND_CLASS)
        {
            instantiate_dependent_friend_class(being_instantiated,
                    friend, inner_decl_context,
                    instantiation_symbol_map,
                    locus);
        }
        else
        {
            internal_error("Unexpected friend symbol '%s'\n", symbol_kind_name(friend));
        }
        // else if (friend->kind == SK_CLASS)
        // {
        //     // The symbol 'friend' may has a dependent type. Example:
        //     //
        //     //
        //     //    template < typename T1>
        //     //        struct B {};
        //     //
        //     //    template < typename T2>
        //     //        struct A
        //     //        {
        //     //            friend struct B<T2>; (1)
        //     //        };
        //     //
        //     //    A<int> foo; (2)
        //     //
        //     //
        //     // The symbol 'B<T2>' created in (1) has kind 'SK_CLASS' but his type is dependent
        //     // In the instantiation (2) we should modify his type

        //     scope_entry_t* new_friend = friend;
        //     if (is_dependent_type(friend->type_information))
        //     {
        //         type_t* new_type = update_type_for_instantiation(get_user_defined_type(friend),
        //                 inner_decl_context,
        //                 friend->locus,
        //                 instantiation_symbol_map,
        //                 /* pack_index */ -1);
        //         if (new_type == NULL)
        //             continue;
        //         new_friend = named_type_get_symbol(new_type);
        //     }

        //     class_type_add_friend_symbol(get_actual_class_type(being_instantiated), new_friend);
        // }
        // else if (friend->kind == SK_FUNCTION)
        // {
        //     // This code is unreachable because all the dependent friend functions 
        //     // of a template class always will be a SK_DEPENDENT_FRIEND_FUNCTION.
        //     // (See function 'find_dependent_friend_function_declaration' in buildscope)
        //     internal_error("Code unreachable", 0);
        // }
        // else
        // {
        //     internal_error("Code unreachable", 0);
        // }
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
            being_instantiated_sym->decl_context, locus, &nodecl_finish_class);
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Class '%s' finished\n", 
                print_declarator(being_instantiated));
    }

    push_instantiated_entity(being_instantiated_sym);
}

typedef
struct instantiate_class_header_message_fun_data_tag
{
    type_t* being_instantiated;
    scope_entry_t* being_instantiated_sym;
    const locus_t* locus;
} instantiate_class_header_message_fun_data_t;

static const char* instantiate_class_header_message_fun(void* v)
{
    const char* instantiation_header = NULL;

    instantiate_class_header_message_fun_data_t* p =
        (instantiate_class_header_message_fun_data_t*)v;

    uniquestr_sprintf(&instantiation_header,
            "%s: info: while instantiating class '%s'\n",
            locus_to_str(p->locus),
            print_type_str(p->being_instantiated, p->being_instantiated_sym->decl_context));

    return instantiation_header;
}

static void instantiate_specialized_template_class(type_t* selected_template,
        type_t* being_instantiated,
        template_parameter_list_t* template_arguments,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: %s: Instantiating class '%s' using '%s' as a template\n",
                locus_to_str(locus),
                print_declarator(being_instantiated),
                print_declarator(selected_template));
    }

    ERROR_CONDITION(!is_named_class_type(being_instantiated), "Must be a named class", 0);

    scope_entry_t* selected_template_sym = named_type_get_symbol(selected_template);
    scope_entry_t* being_instantiated_sym = named_type_get_symbol(being_instantiated);

    header_message_fun_t instantiation_header;
    instantiation_header.message_fun = instantiate_class_header_message_fun;
    {
        instantiate_class_header_message_fun_data_t* p
            = NEW0(instantiate_class_header_message_fun_data_t);
        p->being_instantiated = being_instantiated;
        p->being_instantiated_sym = being_instantiated_sym;
        p->locus = locus;
        instantiation_header.data = p;
    }

    diagnostic_context_push_instantiation(instantiation_header);

    // Update the template parameter with the deduced template parameters
    decl_context_t* instantiation_context = decl_context_clone(being_instantiated_sym->decl_context);

    // But the selected_template might be a nested one in a dependent context so we must update
    // the enclosing template arguments with those of the original class
    ERROR_CONDITION(being_instantiated_sym->decl_context->template_parameters == NULL, "Wrong nesting in template parameters", 0);

    template_arguments->enclosing = being_instantiated_sym->decl_context->template_parameters->enclosing;

    // Our instantiation context is ready
    instantiation_context->template_parameters = template_arguments;

    template_specialized_type_update_template_parameters(being_instantiated_sym->type_information,
            instantiation_context->template_parameters);
    template_specialized_type_update_template_parameters(
            class_symbol_get_canonical_symbol(being_instantiated_sym)->type_information,
            instantiation_context->template_parameters);

    const decl_context_t* inner_decl_context = new_class_context(instantiation_context, 
            being_instantiated_sym);
    if (inner_decl_context->template_parameters->num_parameters > 0)
    {
        // Only real template types can become explicit specializations
        inner_decl_context->template_parameters->is_explicit_specialization = 1;
    }

    being_instantiated_sym->decl_context = instantiation_context;

    class_type_set_inner_context(being_instantiated_sym->type_information, inner_decl_context);

    // From now this class acts as instantiated
    symbol_entity_specs_set_is_instantiated(being_instantiated_sym, 1);

    instantiation_symbol_map_t* enclosing_instantiation_symbol_map = NULL;
    if (symbol_entity_specs_get_is_member(selected_template_sym))
    {
        scope_entry_t* enclosing_class = named_type_get_symbol(symbol_entity_specs_get_class_type(selected_template_sym));
        enclosing_instantiation_symbol_map = symbol_entity_specs_get_instantiation_symbol_map(enclosing_class);
    }

    instantiate_class_common(
            being_instantiated_sym,
            being_instantiated,
            selected_template_sym,
            selected_template,
            enclosing_instantiation_symbol_map,
            inner_decl_context,
            locus);

    diagnostic_context_pop_and_commit();

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: End of instantiation of class '%s'\n", 
                print_declarator(being_instantiated));
    }
}

static void instantiate_bases(
        type_t* selected_class_type,
        type_t* instantiated_class_type,
        const decl_context_t* context_of_being_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus)
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
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;
        scope_entry_t* base_class_sym = class_type_get_base_num(selected_class_type, i,
                &is_virtual,
                &is_dependent_base,
                &is_expansion,
                &access_specifier);

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
                locus,
                instantiation_symbol_map,
                /* pack_index */ -1);

        if (is_error_type(upd_base_class_named_type))
            continue;

        if (!is_class_type(upd_base_class_named_type))
        {
            error_printf_at(locus, "type '%s' is not a class type\n",
                    print_type_str(upd_base_class_named_type, context_of_being_instantiated));
            continue;
        }

        scope_entry_t* upd_base_class_sym = named_type_get_symbol(
                advance_over_typedefs(upd_base_class_named_type));

        // If the entity (being an independent one) has not been completed, then instantiate it
        class_type_complete_if_needed(upd_base_class_sym, context_of_being_instantiated, locus);

        ERROR_CONDITION(
                class_type_is_incomplete_independent(
                    get_actual_class_type(upd_base_class_sym->type_information)),
                "Invalid class %s\n",
                print_declarator(get_user_defined_type(upd_base_class_sym)));

        class_type_add_base_class(
                get_actual_class_type(instantiated_class_type),
                upd_base_class_sym, 
                is_virtual,
                /* is_dependent */ 0,
                /* is_expansion */ 0,
                access_specifier);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Finished updating bases\n");
    }
}

static type_t* solve_template_for_instantiation(scope_entry_t* entry,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus)
{
    diagnostic_context_push_buffered();

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
        fprintf(stderr, "INSTANTIATION: Instantiating class '%s' at '%s'\n",
                print_type_str(get_user_defined_type(entry), entry->decl_context),
                locus_to_str(entry->locus));
    }

    if (!is_template_specialized_type(template_specialized_type)
            || !is_class_type(template_specialized_type)
            || !class_type_is_incomplete_independent(template_specialized_type))
    {
        internal_error("Symbol '%s' is not a class eligible for instantiation", entry->symbol_name);
    }

    type_t* template_type =
        template_specialized_type_get_related_template_type(template_specialized_type);

    type_t* selected_template = solve_class_template(
            template_type,
            get_user_defined_type(entry),
            deduced_template_arguments, locus);

    diagnostic_context_pop_and_discard();

    return selected_template;
}

static void instantiate_template_class(scope_entry_t* entry, 
        const decl_context_t* decl_context, 
        type_t* selected_template, 
        template_parameter_list_t* deduced_template_arguments,
        const locus_t* locus)
{
    //Ignore typedefs
    if (entry->kind != SK_CLASS)
    {
        ERROR_CONDITION(!is_named_class_type(entry->type_information), "Invalid class type", 0);

        type_t* t = advance_over_typedefs(entry->type_information);
        entry = named_type_get_symbol(t);

        ERROR_CONDITION(entry->kind != SK_CLASS, "Invalid class symbol", 0);
    }

    if (selected_template == NULL)
        selected_template = solve_template_for_instantiation(entry, decl_context, &deduced_template_arguments, locus);

    if (selected_template != NULL)
    {
        if (is_incomplete_type(selected_template))
        {
            error_printf_at(
                    locus,
                    "instantiation of '%s' is not possible at this point since "
                    "its most specialized template '%s' is incomplete\n",
                    print_type_str(get_user_defined_type(entry), decl_context),
                    print_type_str(selected_template, decl_context));
            return;
        }

        instantiate_specialized_template_class(selected_template,
                get_user_defined_type(entry),
                deduced_template_arguments, locus);
    }
    else
    {
        error_printf_at(locus, "instantiation of '%s' is not possible at this point\n", print_type_str(get_user_defined_type(entry), decl_context));
        return;
    }
}

static void instantiate_nontemplate_member_class_of_template_class(
        scope_entry_t* being_instantiated_sym,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus)
{
    ERROR_CONDITION(being_instantiated_sym->kind != SK_CLASS
            || !symbol_entity_specs_get_is_member(being_instantiated_sym),
            "Invalid symbol", 0);
    type_t* being_instantiated = get_user_defined_type(being_instantiated_sym);

    scope_entry_t* selected_template_sym = symbol_entity_specs_get_emission_template(being_instantiated_sym);
    ERROR_CONDITION(selected_template_sym == NULL
            || selected_template_sym->kind != SK_CLASS, "Invalid emission template", 0);

    type_t* selected_template = get_user_defined_type(selected_template_sym);

    if (is_incomplete_type(selected_template))
    {
        error_printf_at(locus,
                "instantiation of '%s' is not possible at this point since "
                "its template '%s' is incomplete\n",
                print_type_str(being_instantiated, decl_context),
                print_type_str(selected_template, decl_context));
        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: %s: Instantiating class '%s' using '%s' as a template\n",
                locus_to_str(locus),
                print_declarator(being_instantiated),
                print_declarator(selected_template));
    }

    symbol_entity_specs_set_emission_template(being_instantiated_sym, NULL);

    const decl_context_t* instantiation_context = being_instantiated_sym->decl_context;

    const decl_context_t* inner_decl_context = new_class_context(instantiation_context,
            being_instantiated_sym);

    class_type_set_inner_context(being_instantiated_sym->type_information, inner_decl_context);

    header_message_fun_t instantiation_header;
    instantiation_header.message_fun = instantiate_class_header_message_fun;
    {
        instantiate_class_header_message_fun_data_t* p =
            NEW0(instantiate_class_header_message_fun_data_t);
        p->being_instantiated = being_instantiated;
        p->being_instantiated_sym = being_instantiated_sym;
        p->locus = locus;
        instantiation_header.data = p;
    }
    diagnostic_context_push_instantiation(instantiation_header);

    instantiation_symbol_map_t* enclosing_instantiation_symbol_map = NULL;
    scope_entry_t* enclosing_class = named_type_get_symbol(symbol_entity_specs_get_class_type(being_instantiated_sym));
    enclosing_instantiation_symbol_map = symbol_entity_specs_get_instantiation_symbol_map(enclosing_class);

    instantiate_class_common(
            being_instantiated_sym,
            being_instantiated,
            selected_template_sym,
            selected_template,
            enclosing_instantiation_symbol_map,
            inner_decl_context,
            locus);

    diagnostic_context_pop_and_commit();

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: End of instantiation of class '%s'\n", 
                print_declarator(being_instantiated));
    }
}

static char member_function_may_be_instantiated(scope_entry_t* entry)
{
    ERROR_CONDITION(!symbol_entity_specs_get_is_member(entry), "This function must be a member", 0);

    if (is_template_specialized_type(entry->type_information))
    {
        entry =
            named_type_get_symbol(
                    template_type_get_primary_type(
                        template_specialized_type_get_related_template_type(entry->type_information)));
    }

    return symbol_entity_specs_get_is_instantiable(entry)
        && symbol_entity_specs_get_emission_template(entry) != NULL
        && symbol_entity_specs_get_emission_template(entry)->defined
        && !nodecl_is_null(
                symbol_entity_specs_get_function_code(
                    symbol_entity_specs_get_emission_template(entry)));
}

static char nonmember_template_function_may_be_instantiated(scope_entry_t* entry)
{
    ERROR_CONDITION(!is_template_specialized_type(entry->type_information), "Function must be specialized", 0);

    if (is_template_specialized_type(entry->type_information))
    {
        entry =
            named_type_get_symbol(
                    template_type_get_primary_type(
                        template_specialized_type_get_related_template_type(entry->type_information)));
    }

    return symbol_entity_specs_get_is_instantiable(entry)
        && symbol_entity_specs_get_emission_template(entry) != NULL
        && symbol_entity_specs_get_emission_template(entry)->defined
        && !nodecl_is_null(
                symbol_entity_specs_get_function_code(
                    symbol_entity_specs_get_emission_template(
                        entry)));
}

char function_may_be_instantiated(scope_entry_t* entry)
{
    if (symbol_entity_specs_get_is_member(entry))
    {
        return member_function_may_be_instantiated(entry);
    }
    else if (is_template_specialized_type(entry->type_information))
    {
        return nonmember_template_function_may_be_instantiated(entry);
    }
    return 0;
}

static char template_class_needs_to_be_instantiated(scope_entry_t* entry)
{
    return (class_type_is_incomplete_independent(entry->type_information) // it is independent incomplete
            && !symbol_entity_specs_get_is_instantiated(entry)); // and we need to instantiated at this point
}

// Tries to instantiate if the current class is an independent incomplete class (and it is not an explicit specialization being defined)
void instantiate_template_class_if_needed(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus)
{
    if (template_class_needs_to_be_instantiated(entry))
    {
        instantiate_template_class(entry, decl_context, /* selected_template */ NULL, /* deduced_template_arguments */ NULL, locus);
    }
}

// Used in overload as it temptatively tries to instantiate classes lest they
// were a based or a derived class of another
char instantiate_template_class_if_possible(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus)
{
    if (!template_class_needs_to_be_instantiated(entry))
        return 1;

    // Try to see if it can actually be instantiated
    template_parameter_list_t* deduced_template_arguments = NULL;
    type_t* selected_template =
        solve_template_for_instantiation(entry, decl_context, &deduced_template_arguments, locus);

    // No specialized template is eligible for it, give up
    if (selected_template == NULL
        || is_incomplete_type(selected_template))
        return 0;

    instantiate_template_class(entry, decl_context, selected_template, deduced_template_arguments, locus);

    return 1;
}

void instantiate_nontemplate_member_class_if_needed(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    ERROR_CONDITION(entry->kind != SK_CLASS
            || !symbol_entity_specs_get_is_member(entry),
            "Invalid symbol", 0);

    if (symbol_entity_specs_get_emission_template(entry) == NULL)
        return;

    instantiate_nontemplate_member_class_of_template_class(entry, decl_context, locus);
}

char instantiate_nontemplate_member_class_if_possible(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    ERROR_CONDITION(entry->kind != SK_CLASS
            || !symbol_entity_specs_get_is_member(entry),
            "Invalid symbol", 0);

    if (symbol_entity_specs_get_emission_template(entry) == NULL)
        return 1;

    if (is_incomplete_type(symbol_entity_specs_get_emission_template(entry)->type_information))
        return 0;

    instantiate_nontemplate_member_class_of_template_class(entry, decl_context, locus);

    return 1;
}

static nodecl_t nodecl_instantiation_units;
typedef
struct instantiation_item_tag
{
    scope_entry_t* symbol;
    const locus_t* locus;
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
        const locus_t* locus);

static void mark_class_and_bases_as_user_declared(scope_entry_t* sym)
{
    ERROR_CONDITION(sym->kind != SK_CLASS, "Invalid symbol %s", symbol_kind_name(sym));

    if (symbol_entity_specs_get_is_user_declared(sym))
        return;

    symbol_entity_specs_set_is_user_declared(sym, 1);
    if (symbol_entity_specs_get_is_member(sym))
    {
        mark_class_and_bases_as_user_declared(named_type_get_symbol(symbol_entity_specs_get_class_type(sym)));
    }

    int num_bases = class_type_get_num_bases(sym->type_information);
    int i;
    for (i = 0; i < num_bases; i++)
    {
        char is_virtual = 0;
        char is_dependent = 0;
        char is_expansion = 0;
        access_specifier_t access_specifier = AS_UNKNOWN;

        scope_entry_t* base = class_type_get_base_num(
                sym->type_information, i,
                &is_virtual,
                &is_dependent,
                &is_expansion,
                &access_specifier);

        mark_class_and_bases_as_user_declared(base);
    }
}

void instantiation_instantiate_pending_functions(nodecl_t* nodecl_output)
{
    while (num_symbols_to_instantiate > 0)
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
                    tmp_symbols_to_instantiate[i]->locus);


            DELETE(tmp_symbols_to_instantiate[i]);
        }
        DELETE(tmp_symbols_to_instantiate);
    }

    if (!nodecl_is_null(nodecl_instantiation_units))
    {
        *nodecl_output = nodecl_append_to_list(*nodecl_output,
                nodecl_make_source_comment("Explicit instantiation of functions",
                    nodecl_get_locus(*nodecl_output)));
        *nodecl_output = nodecl_concat_lists(*nodecl_output,
                nodecl_instantiation_units);
    }

    int N = 0;
    nodecl_t* list = nodecl_unpack_list(*nodecl_output, &N);
    dhash_ptr_t* sym_hash = dhash_ptr_new(5);

    // First declare template functions
    int i;
    for (i = 0; i < N; i++)
    {
        if (nodecl_get_kind(list[i]) == NODECL_CXX_EXPLICIT_INSTANTIATION_DECL)
        {
            scope_entry_t* sym = nodecl_get_symbol(list[i]);
            if (dhash_ptr_query(sym_hash, (const char*)sym) != NULL)
                continue;

            DEBUG_CODE()
            {
                fprintf(stderr, "INSTANTIATION: Explicit instantiation declaration -> %s\n",
                        get_qualified_symbol_name(sym, sym->decl_context));
            }

            if  (sym->kind == SK_FUNCTION)
            {
                dhash_ptr_insert(sym_hash, (const char*)sym, sym);
            }
            else if (sym->kind == SK_CLASS)
            {
                dhash_ptr_insert(sym_hash, (const char*)sym, sym);

                scope_entry_list_t* members = class_type_get_members(sym->type_information);

                scope_entry_list_iterator_t* it;
                for (it = entry_list_iterator_begin(members);
                        !entry_list_iterator_end(it);
                        entry_list_iterator_next(it))
                {
                    scope_entry_t* current_member = entry_list_iterator_current(it);
                    dhash_ptr_insert(sym_hash, (const char*)current_member, current_member);

                    if (is_template_specialized_type(current_member->type_information))
                    {
                        type_t* template_type =
                            template_specialized_type_get_related_template_type(current_member->type_information);
                        int num_specializations = template_type_get_num_specializations(template_type);
                        int j;
                        for (j = 0; j < num_specializations; j++)
                        {
                            scope_entry_t* specialization =
                                named_type_get_symbol(
                                        template_type_get_specialization_num(template_type, j));

                            DEBUG_CODE()
                            {
                                if (specialization->kind == SK_FUNCTION)
                                {
                                    fprintf(stderr, "INSTANTIATION:   sf-> %s\n",
                                            print_decl_type_str(
                                                specialization->type_information,
                                                specialization->decl_context,
                                                get_qualified_symbol_name(specialization,
                                                    specialization->decl_context)));
                                }
                                else
                                {
                                    fprintf(stderr, " INSTANTIATION:  sc-> %s\n",
                                            get_qualified_symbol_name(specialization,
                                                specialization->decl_context));
                                }
                            }
                            dhash_ptr_insert(sym_hash, (const char*)specialization, specialization);
                        }
                    }
                    else if (current_member->kind == SK_FUNCTION)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "INSTANTIATION:   mf-> %s\n",
                                    print_decl_type_str(
                                        current_member->type_information,
                                        current_member->decl_context,
                                        get_qualified_symbol_name(current_member,
                                            current_member->decl_context)));
                        }
                    }
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "INSTANTIATION:    m-> %s\n",
                                    get_qualified_symbol_name(current_member,
                                        current_member->decl_context));
                        }
                    }
                }

                entry_list_iterator_free(it);
                entry_list_free(members);
            }
        }
        else if (nodecl_get_kind(list[i]) == NODECL_CXX_IMPLICIT_INSTANTIATION
                || nodecl_get_kind(list[i]) == NODECL_CXX_EXPLICIT_INSTANTIATION_DEF)
        {
            scope_entry_t* sym = nodecl_get_symbol(list[i]);
            if (dhash_ptr_query(sym_hash, (const char*)sym) != NULL)
                continue;

            DEBUG_CODE()
            {
                fprintf(stderr, "INSTANTIATION: Explicit instantiation definition/Implicit definition -> %s\n",
                        get_qualified_symbol_name(sym, sym->decl_context));
            }

            if  (sym->kind == SK_FUNCTION)
            {
                if (is_template_specialized_type(sym->type_information)
                        || (symbol_entity_specs_get_is_constexpr(sym)
                            && !nodecl_is_null(symbol_entity_specs_get_function_code(sym))))
                {
                    decl_context_t* templated_context = decl_context_clone(CURRENT_COMPILED_FILE->global_decl_context);
                    templated_context->template_parameters = sym->decl_context->template_parameters;

                    nodecl_t parent = nodecl_get_parent(list[i]);
                    nodecl_t new_decl;

                    if (symbol_entity_specs_get_is_constexpr(sym)
                            && !nodecl_is_null(symbol_entity_specs_get_function_code(sym)))
                    {
                        new_decl = symbol_entity_specs_get_function_code(sym);
                    }
                    else
                    {
                        new_decl = nodecl_make_cxx_decl(
                                nodecl_make_context(
                                    nodecl_null(),
                                    templated_context,
                                    sym->locus),
                                sym,
                                sym->locus);
                    }

                    nodecl_replace(list[i], new_decl);

                    // Reparent node
                    nodecl_set_parent(list[i], parent);
                    // Reparent children
                    int k;
                    for (k = 0; k < MCXX_MAX_AST_CHILDREN; k++)
                    {
                        if (!nodecl_is_null(nodecl_get_child(list[i], k)))
                            nodecl_set_parent(nodecl_get_child(list[i], k), list[i]);
                    };

                    dhash_ptr_insert(sym_hash, (const char*)sym, sym);
                }
            }
        }
    }

    // Now the template classes that must be emitted
    for (i = 0; i < N; i++)
    {
        if (nodecl_get_kind(list[i]) == NODECL_CXX_IMPLICIT_INSTANTIATION
                || nodecl_get_kind(list[i]) == NODECL_CXX_EXPLICIT_INSTANTIATION_DEF)
        {
            scope_entry_t* sym = nodecl_get_symbol(list[i]);
            if (dhash_ptr_query(sym_hash, (const char*)sym) != NULL)
                continue;

            if  (sym->kind == SK_CLASS)
            {
                mark_class_and_bases_as_user_declared(sym);
                decl_context_t* templated_context = NULL;
                if (symbol_entity_specs_get_is_defined_inside_class_specifier(sym))
                {
                    templated_context = decl_context_clone(sym->decl_context);
                }
                else
                {
                    templated_context = decl_context_clone(CURRENT_COMPILED_FILE->global_decl_context);
                }
                templated_context->template_parameters = sym->decl_context->template_parameters;

                nodecl_t parent = nodecl_get_parent(list[i]);
                nodecl_t new_def = nodecl_make_cxx_def(
                        nodecl_make_context(
                            nodecl_null(),
                            templated_context,
                            sym->locus),
                        sym,
                        sym->locus);

                nodecl_replace(list[i], new_def);

                // Reparent node
                nodecl_set_parent(list[i], parent);
                // Reparent children
                int k;
                for (k = 0; k < MCXX_MAX_AST_CHILDREN; k++)
                {
                    if (!nodecl_is_null(nodecl_get_child(list[i], k)))
                        nodecl_set_parent(nodecl_get_child(list[i], k), list[i]);
                };

                dhash_ptr_insert(sym_hash, (const char*)sym, sym);
            }

        }
    }

    dhash_ptr_destroy(sym_hash);

    DELETE(list);
}

static char compare_instantiate_items(instantiation_item_t* current_item, instantiation_item_t* new_item)
{
    return current_item->symbol == new_item->symbol;
}

void instantiation_add_symbol_to_instantiate(scope_entry_t* entry,
        const locus_t* locus)
{
    instantiation_item_t* item = NEW0(instantiation_item_t);
    item->symbol = entry;
    item->locus = locus;

    int old_num = num_symbols_to_instantiate;

    P_LIST_ADD_ONCE_FUN(symbols_to_instantiate,
            num_symbols_to_instantiate,
            item,
            compare_instantiate_items);

    // Crummy way to know if it was added
    if (old_num == num_symbols_to_instantiate)
    {
        DELETE(item);
    }
}

static char instantiate_true_template_function(scope_entry_t* entry, const locus_t* locus UNUSED_PARAMETER)
{
    ERROR_CONDITION(entry == NULL || entry->kind != SK_FUNCTION,
            "Invalid symbol", 0);
    ERROR_CONDITION(!is_template_specialized_type(entry->type_information),
            "This is not a specialized function", 0);
    ERROR_CONDITION(!nodecl_is_null(symbol_entity_specs_get_function_code(entry)),
            "Attempting to instantiate a specialized function apparently already instantiated", 0);

    entry->locus = locus;

    type_t* template_specialized_type = entry->type_information;

    type_t* template_type = template_specialized_type_get_related_template_type(template_specialized_type);
    scope_entry_t* template_symbol = template_type_get_related_symbol(template_type);

    // The primary specialization is a named type, even if the named type is a function!
    type_t* primary_specialization_type = template_type_get_primary_type(template_symbol->type_information);
    scope_entry_t* primary_specialization_function = named_type_get_symbol(primary_specialization_type);

    scope_entry_t* emission_template =
        symbol_entity_specs_get_emission_template(primary_specialization_function);
    ERROR_CONDITION(emission_template == NULL, "Function lacks emission template", 0);
    if (!emission_template->defined)
        return 0;

    nodecl_t orig_function_code = symbol_entity_specs_get_function_code(emission_template);
    ERROR_CONDITION(nodecl_is_null(orig_function_code), "Invalid function code", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating function '%s' with type '%s' at '%s\n",
                entry->symbol_name,
                print_type_str(entry->type_information, entry->decl_context),
                locus_to_str(entry->locus));
    }

    instantiation_symbol_map_t* instantiation_symbol_map = NULL;
    if (symbol_entity_specs_get_is_member(entry))
    {
        instantiation_symbol_map =
            symbol_entity_specs_get_instantiation_symbol_map(
                    named_type_get_symbol(symbol_entity_specs_get_class_type(entry)));
    }

    nodecl_t instantiated_function_code = instantiate_function_code(
            orig_function_code,
            primary_specialization_function->decl_context,
            entry->decl_context,
            primary_specialization_function,
            entry,
            instantiation_symbol_map
            );

    ERROR_CONDITION(nodecl_is_null(instantiated_function_code), "Instantiation failed to generate a function code", 0);

    symbol_entity_specs_set_function_code(entry, instantiated_function_code);
    entry->defined = 1;
    symbol_entity_specs_set_is_instantiated(entry, 1);
    symbol_entity_specs_set_is_instantiable(entry, 0);
    symbol_entity_specs_set_is_user_declared(entry, 1);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: ended instantation of function template '%s'\n",
                print_declarator(template_specialized_type));
    }

    return 1;
}

static char instantiate_nontemplate_member_function_of_template_class(scope_entry_t* entry, const locus_t* locus)
{
    ERROR_CONDITION(entry == NULL || entry->kind != SK_FUNCTION,
            "Invalid symbol", 0);
    ERROR_CONDITION(is_template_specialized_type(entry->type_information),
            "Invalid function type", 0);
    ERROR_CONDITION(!nodecl_is_null(symbol_entity_specs_get_function_code(entry)),
            "Attempting to instantiate a specialized function apparently already instantiated", 0);

    entry->locus = locus;

    scope_entry_t* emission_template =
        symbol_entity_specs_get_emission_template(entry);

    ERROR_CONDITION(emission_template == NULL,
            "Invalid nontemplate member function", 0);

    // Cannot be instantiated
    if (!emission_template->defined)
    {
        fprintf(stderr, "Cannot be instantiated: %s\n",
                get_qualified_symbol_name(emission_template,
                    emission_template->decl_context));
        return 0;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: Instantiating nontemplate member function of template '%s' with type '%s' at '%s\n",
                get_qualified_symbol_name(entry, entry->decl_context),
                print_type_str(entry->type_information, entry->decl_context),
                locus_to_str(entry->locus));
    }

    nodecl_t orig_function_code = symbol_entity_specs_get_function_code(
            symbol_entity_specs_get_emission_template(
                entry));

    ERROR_CONDITION(nodecl_is_null(orig_function_code), "Invalid function code", 0);

    instantiation_symbol_map_t* instantiation_symbol_map =
        symbol_entity_specs_get_instantiation_symbol_map(named_type_get_symbol(symbol_entity_specs_get_class_type(entry)));

    nodecl_t instantiated_function_code = instantiate_function_code(
            orig_function_code,
            entry->decl_context,
            entry->decl_context,
            emission_template,
            entry,
            instantiation_symbol_map
            );

    ERROR_CONDITION(nodecl_is_null(instantiated_function_code), "Instantiation failed to generate a function code", 0);

    symbol_entity_specs_set_function_code(entry, instantiated_function_code);
    entry->defined = 1;
    symbol_entity_specs_set_is_instantiated(entry, 1);
    symbol_entity_specs_set_is_instantiable(entry, 0);
    symbol_entity_specs_set_is_user_declared(entry, 1);

    DEBUG_CODE()
    {
        fprintf(stderr, "INSTANTIATION: ended instantation of nontemplate member function template '%s'\n",
                get_qualified_symbol_name(entry, entry->decl_context));
    }

    return 1;
}

static scope_entry_t* being_instantiated_now[MCXX_MAX_TEMPLATE_NESTING_LEVELS];
static int num_being_instantiated_now = 0;

static char compare_gcc_attribute(gcc_attribute_t current_item, gcc_attribute_t new_item)
{
    return current_item.attribute_name == new_item.attribute_name;
}

static template_parameter_list_t* copy_template_parameters(template_parameter_list_t* t)
{
    if (t == NULL)
        return NULL;

    template_parameter_list_t* res = NEW0(template_parameter_list_t);
    *res = *t;

    res->enclosing = copy_template_parameters(t->enclosing);

    return res;
}

typedef
struct instantiate_function_header_message_fun_data_tag
{
    scope_entry_t* entry;
    const locus_t* locus;
} instantiate_function_header_message_fun_data_t;

static const char* instantiate_function_header_message_fun(void* v)
{
    const char* instantiation_header = NULL;
    instantiate_function_header_message_fun_data_t* p =
        (instantiate_function_header_message_fun_data_t*)v;

    uniquestr_sprintf(&instantiation_header,
            "%s: info: while instantiating function '%s'\n",
            locus_to_str(p->locus),
            print_decl_type_str(p->entry->type_information,
                p->entry->decl_context,
                get_qualified_symbol_name(p->entry, p->entry->decl_context)));

    return instantiation_header;
}

static char instantiate_template_function_internal(scope_entry_t* entry, const locus_t* locus)
{
    ERROR_CONDITION(entry == NULL || entry->kind != SK_FUNCTION,
            "Invalid symbol", 0);

    if (symbol_entity_specs_get_is_instantiated(entry))
        return 0;

    int i;
    for (i = 0; i < num_being_instantiated_now; i++)
    {
        if (being_instantiated_now[i] == entry)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INSTANTIATION: This function is currently being instantiated\n");
            }
            return 0;
        }
    }

    ERROR_CONDITION(num_being_instantiated_now == MCXX_MAX_TEMPLATE_NESTING_LEVELS, 
            "Too many instantiation template levels %d", MCXX_MAX_TEMPLATE_NESTING_LEVELS);

    being_instantiated_now[num_being_instantiated_now] = entry;
    num_being_instantiated_now++;

    header_message_fun_t instantiation_header;
    instantiation_header.message_fun = instantiate_function_header_message_fun;
    {
        instantiate_function_header_message_fun_data_t* p =
            NEW0(instantiate_function_header_message_fun_data_t);
        p->entry = entry;
        p->locus = locus;
        instantiation_header.data = p;
    }
    diagnostic_context_push_instantiation(instantiation_header);

    char was_instantiated = 0;

    if (!symbol_entity_specs_get_is_member(entry)
            && is_template_specialized_type(entry->type_information)
            && nonmember_template_function_may_be_instantiated(entry))
    {
        was_instantiated = instantiate_true_template_function(entry, locus);
    }
    else if (symbol_entity_specs_get_is_member(entry)
            && member_function_may_be_instantiated(entry))
    {
        if (is_template_specialized_type(entry->type_information))
        {
            was_instantiated = instantiate_true_template_function(entry, locus);
        }
        else
        {
            was_instantiated = instantiate_nontemplate_member_function_of_template_class(entry, locus);
        }
    }
    else
    {
        internal_error("This function cannot be instantiated", 0);
    }

    if (was_instantiated)
    {
        if (!symbol_entity_specs_get_is_inline(entry))
        {
            gcc_attribute_t gcc_attr = { uniquestr("weak"), nodecl_null() };
            symbol_entity_specs_insert_gcc_attributes_cmp(
                    entry,
                    gcc_attr,
                    compare_gcc_attribute);
        }

        decl_context_t* updated_decl_context = decl_context_clone(entry->decl_context);
        updated_decl_context->template_parameters = copy_template_parameters(updated_decl_context->template_parameters);
        entry->decl_context = updated_decl_context;

        template_parameter_list_t* tpl = entry->decl_context->template_parameters;

        if (is_template_specialized_type(entry->type_information))
        {
            if (tpl != NULL)
            {
                tpl->is_explicit_specialization = 1;
                tpl = tpl->enclosing;
            }
        }

        while (tpl != NULL)
        {
            tpl->is_explicit_instantiation = 1;
            tpl = tpl->enclosing;
        }

        if (symbol_entity_specs_get_is_member(entry)
                && symbol_entity_specs_get_is_defined_inside_class_specifier(entry)
                && !symbol_entity_specs_get_is_defaulted(entry))
        {
            symbol_entity_specs_set_is_defined_inside_class_specifier(entry, 0);
        }

        entry->defined = 1;
    }

    diagnostic_context_pop_and_commit();

    num_being_instantiated_now--;
    being_instantiated_now[num_being_instantiated_now] = NULL;

    return was_instantiated;
}


static void instantiate_template_function_and_add_to_instantiation_units(scope_entry_t* entry,
        const locus_t* locus UNUSED_PARAMETER)
{
    char was_instantiated = instantiate_template_function_internal(entry, locus);
    if (was_instantiated)
    {
        nodecl_t instantiated_entities = flush_instantiated_entities();

        instantiated_entities = nodecl_append_to_list(
                instantiated_entities,
                symbol_entity_specs_get_function_code(entry));

        nodecl_instantiation_units = nodecl_concat_lists(
                nodecl_instantiation_units,
                instantiated_entities);
    }
}

// This function eventually may have to "instantiate" data, for the moment
// it only instantiates functions
static void instantiate_every_symbol(scope_entry_t* entry,
        const locus_t* locus)
{
    if (entry != NULL
            && entry->kind == SK_FUNCTION)
    {
        if (function_may_be_instantiated(entry))
        {
            instantiate_template_function_and_add_to_instantiation_units(entry, locus);
        }
    }
}

void instantiate_template_function(scope_entry_t* entry, const locus_t* locus)
{
    instantiate_template_function_internal(entry, locus);
}

void instantiate_template_function_and_integrate_in_translation_unit(
        scope_entry_t* entry UNUSED_PARAMETER,
        const locus_t* locus UNUSED_PARAMETER)
{
    internal_error("This function should not be called anymore", 0);
}

// Instantiation map
struct instantiation_symbol_map_tag
{
    struct instantiation_symbol_map_tag *parent;
    rb_red_blk_tree *tree;
};

scope_entry_t* instantiation_symbol_do_map(instantiation_symbol_map_t* map, scope_entry_t* orig)
{
    if (map == NULL)
        return NULL;

    rb_red_blk_node *n = rb_tree_query(map->tree, orig);
    if (n == NULL)
    {
        return instantiation_symbol_do_map(map->parent, orig);
    }
    else
    {
        scope_entry_t* result = (scope_entry_t*)rb_node_get_info(n);
        return result;
    }
}


scope_entry_t* instantiation_symbol_try_to_map(instantiation_symbol_map_t* map, scope_entry_t* orig)
{
    scope_entry_t* mapped = instantiation_symbol_do_map(map, orig);
    if (mapped == NULL)
        return orig;
    else
        return mapped;
}

void instantiation_symbol_map_add(instantiation_symbol_map_t* map, scope_entry_t* orig, scope_entry_t* new_sym)
{
    ERROR_CONDITION(map == NULL || map->tree == NULL, "Missing map", 0);

    rb_tree_insert(map->tree, orig, new_sym);
}

static int intptr_t_comp(const void *v1, const void *v2)
{
    intptr_t p1 = (intptr_t)(v1);
    intptr_t p2 = (intptr_t)(v2);

    if (p1 < p2)
        return -1;
    else if (p1 > p2)
        return 1;
    else
        return 0;
}

instantiation_symbol_map_t* instantiation_symbol_map_push(instantiation_symbol_map_t* parent)
{
    instantiation_symbol_map_t* result = NEW0(instantiation_symbol_map_t);

    result->parent = parent;
    result->tree = rb_tree_create(intptr_t_comp, NULL, NULL);

    return result;
}

instantiation_symbol_map_t* instantiation_symbol_map_pop(instantiation_symbol_map_t* map)
{
    instantiation_symbol_map_t* result = map->parent;

    rb_tree_destroy(map->tree);
    DELETE(map);

    return result;
}
