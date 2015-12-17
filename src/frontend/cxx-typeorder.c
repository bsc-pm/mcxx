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




#include "cxx-typeorder.h"
#include "cxx-typededuc.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-limits.h"
#include "cxx-diagnostic.h"

static type_t* extend_function_with_implicit_parameter(scope_entry_t* member_function)
{
    // Build a new function type
    int num_parameters = function_type_get_num_parameters(member_function->type_information);
    char has_ellipsis = function_type_get_has_ellipsis(member_function->type_information);

    int num_transformed_parameters = 1 + num_parameters;
    parameter_info_t param_info[num_transformed_parameters + 1];
    memset(param_info, 0, sizeof(param_info));

    if (has_ellipsis)
        num_parameters--;

    param_info[0].type_info = symbol_entity_specs_get_class_type(member_function);
    param_info[0].type_info = get_cv_qualified_type(
            param_info[0].type_info,
            get_cv_qualifier(member_function->type_information));
    if (function_type_get_ref_qualifier(member_function->type_information) == REF_QUALIFIER_RVALUE)
    {
        param_info[0].type_info = get_rvalue_reference_type(param_info[0].type_info);
    }
    else
    {
        param_info[0].type_info = get_rvalue_reference_type(param_info[0].type_info);
    }

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        param_info[1 + i].type_info = function_type_get_parameter_type_num(member_function->type_information, i);
    }

    if (has_ellipsis)
    {
        param_info[1 + num_parameters].is_ellipsis = 1;
    }

    return get_new_function_type(
            function_type_get_return_type(member_function->type_information),
            param_info,
            num_transformed_parameters,
            REF_QUALIFIER_NONE);
}

static char all_used_parameters_have_values_expr(nodecl_t n,
        template_parameter_list_t* updated_template_parameters)
{
    if (nodecl_is_null(n))
        return 1;

    if (nodecl_get_kind(n) == NODECL_SYMBOL
            && (nodecl_get_symbol(n)->kind == SK_TEMPLATE_NONTYPE_PARAMETER
            || nodecl_get_symbol(n)->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK))
    {
        scope_entry_t* param = nodecl_get_symbol(n);
        int i;
        for (i = 0; i < updated_template_parameters->num_parameters; i++)
        {
            if ((symbol_entity_specs_get_template_parameter_nesting(updated_template_parameters->parameters[i]->entry) ==
                        symbol_entity_specs_get_template_parameter_nesting(param))
                    && (symbol_entity_specs_get_template_parameter_position(updated_template_parameters->parameters[i]->entry) ==
                        symbol_entity_specs_get_template_parameter_position(param)))
            {
                return (updated_template_parameters->arguments[i] != NULL);
            }
        }

        // Not found (?)
        return 0;
    }

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (!all_used_parameters_have_values_expr(
                    nodecl_get_child(n, i),
                    updated_template_parameters))
            return 0;
    }

    return 1;
}

static char all_used_parameters_have_values(type_t* t,
        template_parameter_list_t* updated_template_parameters);

static char all_used_parameters_have_values_tpl_arg(
        template_parameter_list_t* template_arguments,
        template_parameter_list_t* updated_template_parameters)
{
    if (template_arguments == NULL)
        return 1;

    int i;
    for (i = 0; i < template_arguments->num_parameters; i++)
    {
        if (template_arguments->arguments[i]->kind == TPK_NONTYPE
                || template_arguments->arguments[i]->kind == TPK_NONTYPE_PACK)
        {
            if (!all_used_parameters_have_values_expr(
                        template_arguments->arguments[i]->value,
                        updated_template_parameters))
                return 0;
        }
        else if (template_arguments->arguments[i]->kind == TPK_TYPE
                || template_arguments->arguments[i]->kind == TPK_TEMPLATE
                || template_arguments->arguments[i]->kind == TPK_TYPE_PACK
                || template_arguments->arguments[i]->kind == TPK_TEMPLATE_PACK)
        {
            if (!all_used_parameters_have_values(
                        template_arguments->arguments[i]->type,
                        updated_template_parameters))
                return 0;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    return 1;
}

static char all_used_parameters_have_values(type_t* t,
        template_parameter_list_t* updated_template_parameters)
{
    if (t == NULL)
        return 1;

    if (is_named_type(t)
            && (named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
    {
        scope_entry_t* param = named_type_get_symbol(t);

        int i;
        for (i = 0; i < updated_template_parameters->num_parameters; i++)
        {
            if ((symbol_entity_specs_get_template_parameter_nesting(updated_template_parameters->parameters[i]->entry) ==
                        symbol_entity_specs_get_template_parameter_nesting(param))
                    && (symbol_entity_specs_get_template_parameter_position(updated_template_parameters->parameters[i]->entry) ==
                        symbol_entity_specs_get_template_parameter_position(param)))
            {
                return (updated_template_parameters->arguments[i] != NULL);
            }
        }

        // Not found (?)
        return 0;
    }
    else if (is_named_class_type(t)
            && is_template_specialized_type(
                named_type_get_symbol(t)->type_information))
    {
        type_t* named_template_type =
            get_user_defined_type(
                    template_type_get_related_symbol(
                        template_specialized_type_get_related_template_type(
                            named_type_get_symbol(t)->type_information)));
        if (!all_used_parameters_have_values(named_template_type, updated_template_parameters))
            return 0;

        template_parameter_list_t* template_arguments = template_specialized_type_get_template_arguments(
                named_type_get_symbol(t)->type_information);

        return all_used_parameters_have_values_tpl_arg(
                template_arguments,
                updated_template_parameters);

    }
    else if (is_pointer_type(t))
    {
        return all_used_parameters_have_values(
                pointer_type_get_pointee_type(t),
                updated_template_parameters);
    }
    else if (is_pointer_to_member_type(t))
    {
        return all_used_parameters_have_values(
                pointer_to_member_type_get_class_type(t),
                updated_template_parameters)
            && all_used_parameters_have_values(
                    pointer_type_get_pointee_type(t),
                    updated_template_parameters);
    }
    else if (is_any_reference_type(t))
    {
        return all_used_parameters_have_values(
                no_ref(t),
                updated_template_parameters);
    }
    else if (is_function_type(t))
    {
        char c = all_used_parameters_have_values(
                function_type_get_return_type(t),
                updated_template_parameters);

        if (!c)
            return 0;

        int num_arguments = function_type_get_num_parameters(t);
        if (function_type_get_has_ellipsis(t))
            num_arguments--;

        int i;
        for (i = 0; i < num_arguments; i++)
        {
            if (!all_used_parameters_have_values(
                        function_type_get_parameter_type_num(t, i),
                        updated_template_parameters))
                return 0;
        }
    }
    else if (is_array_type(t))
    {
        return all_used_parameters_have_values(
                array_type_get_element_type(t),
                updated_template_parameters)
            && all_used_parameters_have_values_expr(
                    array_type_get_array_size_expr(t),
                    updated_template_parameters);
    }
    else if (is_vector_type(t))
    {
        return all_used_parameters_have_values(
                vector_type_get_element_type(t),
                updated_template_parameters);
    }
    else if (is_dependent_typename_type(t))
    {
        scope_entry_t* entry = NULL;
        nodecl_t parts = nodecl_null();
        dependent_typename_get_components(t,
                &entry,
                &parts);

        if ((entry->kind == SK_TEMPLATE_TYPE_PARAMETER
                    || entry->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
                && !all_used_parameters_have_values(
                    get_user_defined_type(entry),
                    updated_template_parameters))
            return 0;

        int n = 0;
        nodecl_t* list = nodecl_unpack_list(
                nodecl_get_child(parts, 0), &n);

        int i;
        for (i = 0; i < n; i++)
        {
            if (nodecl_get_kind(list[i]) == NODECL_CXX_DEP_TEMPLATE_ID)
            {
                if (!all_used_parameters_have_values_tpl_arg(
                            nodecl_get_template_parameters(list[i]),
                            updated_template_parameters))
                    return 0;
            }
        }
    }
    else if (is_typeof_expr(t))
    {
        return all_used_parameters_have_values_expr(
                typeof_expr_type_get_expression(t),
                updated_template_parameters);
    }
    else if (is_pack_type(t))
    {
        return all_used_parameters_have_values(
                pack_type_get_packed_type(t),
                updated_template_parameters);
    }

    return 1;
}

static void transform_type_for_ordering(
        scope_entry_t* function1,
        type_t** transformed_type_t1,
        scope_entry_t* function2,
        type_t** transformed_type_t2)
{
    char is_nonstatic_member1 = 
        symbol_entity_specs_get_is_member(function1)
        && !symbol_entity_specs_get_is_static(function1);
    char is_nonstatic_member2 = 
        symbol_entity_specs_get_is_member(function2)
        && !symbol_entity_specs_get_is_static(function2);

    if (is_nonstatic_member1 == is_nonstatic_member2)
    {
        *transformed_type_t1 = function1->type_information;
        *transformed_type_t2 = function2->type_information;
    }
    else if (is_nonstatic_member1)
    {
        *transformed_type_t1 = extend_function_with_implicit_parameter(function1);
        *transformed_type_t2 = function2->type_information;
    }
    else // if (is_nonstatic_member2)
    {
        *transformed_type_t1 = function1->type_information;
        *transformed_type_t2 = extend_function_with_implicit_parameter(function2);
    }
}

typedef
enum specialization_comparison_tag
{
    COMPARISON_INVALID = 0,
    COMPARISON_AT_LEAST_AS_SPECIALIZED, // >=
    COMPARISON_NONE, // <>
} specialization_comparison_t;

typedef
enum comparison_extra_tag
{
    COMPARISON_EXTRA_NONE = 0,
    COMPARISON_EXTRA_BOTH_REFERENCES = 1 << 0,
    COMPARISON_EXTRA_LVALUE_ARGUMENT = 1 << 1,
    COMPARISON_EXTRA_LVALUE_PARAMETER = 1 << 2,
    COMPARISON_EXTRA_ARG_IS_MORE_CV = 1 << 3,
} comparison_extra_t;

// Returns COMPARISON_AT_LEAST_AS_SPECIALIZED if argument is a at least as specialized as parameter
// Returns COMPARISON_NONE if neither is more specialized than the other
static specialization_comparison_t compare_type_specialization(
        type_t* parameter,
        type_t* argument,
        template_parameter_list_t* template_parameters UNUSED_PARAMETER,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        int pack_length,
        // flags
        char is_overload,
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        // out
        deduction_set_t* deduction_set)
{
    // See 14.8.2.4 temp.deduct.partial

    if (is_any_reference_type(parameter))
    {
        parameter = no_ref(parameter);
    }

    if (is_any_reference_type(argument))
    {
        argument = no_ref(argument);
    }

    parameter = get_unqualified_type(parameter);
    argument = get_unqualified_type(argument);

    // If A was transformed from a function parameter pack and P is not a
    // parameter pack, type deduction fails
    if (is_overload
            && is_pack_type(argument)
            && !is_pack_type(parameter))
    {
        return COMPARISON_NONE;
    }

    deduction_set_t* deduction_set_current = NEW0(deduction_set_t);
    deduction_result_t deduction_result =
        deduce_template_arguments_from_a_type(
                parameter,
                argument,
                explicit_template_arguments,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                /* is_partial_ordering */ 1,
                /* ignore_cv_qualifiers */ 0,
                deduction_set_current);

    if (deduction_result == DEDUCTION_FAILURE)
    {
        deduction_set_free(deduction_set_current);
        return COMPARISON_NONE;
    }

    deduction_result = deduction_combine_to_second(
            deduction_set_current,
            deduction_set);
    deduction_set_free(deduction_set_current);
    if (deduction_result == DEDUCTION_FAILURE)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: Combining deduction fails\n");
        }
        return COMPARISON_NONE;
    }

    // The deduction has not failed
    return COMPARISON_AT_LEAST_AS_SPECIALIZED;
}


static specialization_comparison_t break_tie(
        char both_references,
        char lvalue_argument,
        char lvalue_parameter,
        char argument_is_more_cv_qualified)
{
    specialization_comparison_t cmp = COMPARISON_AT_LEAST_AS_SPECIALIZED;

    if (both_references)
    {
        cmp = COMPARISON_NONE;
        if (lvalue_argument && !lvalue_parameter)
        {
            cmp = COMPARISON_AT_LEAST_AS_SPECIALIZED;
        }
        else if (argument_is_more_cv_qualified)
        {
            cmp = COMPARISON_AT_LEAST_AS_SPECIALIZED;
        }
    }

    return cmp;
}

static comparison_extra_t compute_cmp_extra(type_t* parameter, type_t* argument)
{
    comparison_extra_t cmp_extra = COMPARISON_EXTRA_NONE;

    if (is_any_reference_type(parameter)
            && is_any_reference_type(argument))
    {
        cmp_extra |= COMPARISON_EXTRA_BOTH_REFERENCES;

        if (is_lvalue_reference_type(argument))
            cmp_extra |= COMPARISON_EXTRA_LVALUE_ARGUMENT;
        if (is_lvalue_reference_type(parameter))
            cmp_extra |= COMPARISON_EXTRA_LVALUE_PARAMETER;

        if (is_more_cv_qualified_type(
                    no_ref(argument),
                    no_ref(parameter)))
            cmp_extra |= COMPARISON_EXTRA_ARG_IS_MORE_CV;
    }

    return cmp_extra;
}

static void compute_single_deduction(int *i,
        int num_types,
        type_t* original_type_template,
        type_t* transformed_type_template,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        char is_conversion,
        char is_overload,
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        // out
        deduction_set_t* deduction_set,
        specialization_comparison_t *cmp_out,
        comparison_extra_t *cmp_extra)
{
    type_t* parameter = NULL;
    type_t* argument = NULL;

    if (is_conversion)
    {
        parameter = function_type_get_return_type(original_type_template);
        argument = function_type_get_return_type(transformed_type_template);
    }
    else if (!is_overload)
    {
        parameter = original_type_template;
        argument = transformed_type_template;
    }
    else
    {
        parameter = function_type_get_parameter_type_num(original_type_template, *i);
        argument = function_type_get_parameter_type_num(transformed_type_template, *i);
    }

    if (is_pack_type(argument)
            && !is_pack_type(parameter))
    {
        // if Ai was originally a pack and Pi is not a pack, the deduction fails
        // Fail everything because we won't go beyond this parameter
        int j;
        for (j = *i; j < num_types; j++)
        {
            cmp_extra[j] = COMPARISON_EXTRA_NONE;
            cmp_out[j] = COMPARISON_NONE;
        }
        *i = num_types - 1;

        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Pair #%d\n", *i);
        fprintf(stderr, "TYPEORDER:   Parameter type: %s\n", print_declarator(parameter));
        fprintf(stderr, "TYPEORDER:   Argument type: %s\n", print_declarator(argument));
    }

    if (is_pack_type(parameter))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: Parameter type is a pack\n");
        }

        // Note that we do not use num_types, only arguments since there might be more arguments
        // in the call than parameter types in the argument template
        int num_args = function_type_get_num_parameters(transformed_type_template);
        if (function_type_get_has_ellipsis(transformed_type_template))
            num_args--;

        int pack_length = num_args - *i;

        int j;
        for (j = *i; j < num_args; j++)
        {
            int pack_index = j - *i;

            type_t* current_parameter = pack_type_get_packed_type(parameter);
            type_t* current_argument = function_type_get_parameter_type_num(transformed_type_template, j);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Pair #%d#%d\n", *i, j);
                fprintf(stderr, "TYPEORDER:   Parameter type: %s\n", print_declarator(current_parameter));
                fprintf(stderr, "TYPEORDER:   Argument type: %s\n", print_declarator(current_argument));
            }

            cmp_extra[j] = compute_cmp_extra(
                    current_parameter,
                    current_argument);

            specialization_comparison_t cmp = compare_type_specialization(
                    current_parameter,
                    current_argument,
                    template_specialized_type_get_template_parameters(
                        original_type_template),
                    explicit_template_arguments,
                    decl_context,
                    locus,
                    pack_index,
                    pack_length,
                    // flags
                    is_overload,
                    is_computing_address_of_function,
                    is_deducing_arguments_from_function_declaration,
                    // inout
                    deduction_set);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Result of comparison: '%s'\n",
                        cmp == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least as specialized" : "not comparable");
            }

            cmp_out[j] = cmp;
        }
        for (j = num_args; j < num_types; j++)
        {
            // Ignore the remaining arguments that do not have an explicit pack here
            cmp_out[j] = COMPARISON_AT_LEAST_AS_SPECIALIZED;
        }
        *i = num_types - 1;
    }
    else
    {
        cmp_extra[*i] = compute_cmp_extra(parameter, argument);

        specialization_comparison_t cmp = compare_type_specialization(
                parameter,
                argument,
                template_specialized_type_get_template_parameters(
                    original_type_template),
                explicit_template_arguments,
                decl_context,
                locus,
                // flags
                /* pack_index */ -1,
                /* pack_length */ -1,
                is_overload,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                // inout
                deduction_set);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: Result of comparison: '%s'\n",
                    cmp == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least as specialized" : "not comparable");
        }
        cmp_out[*i] = cmp;
    }
}

// Returns 1 if template_1 is more specialized than template_2
char is_more_specialized_template_function(
        scope_entry_t* template_1,
        scope_entry_t* template_2,
        template_parameter_list_t* raw_explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        char is_overload, int num_actual_arguments,
        char is_conversion,
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_requiring_exact_match,
        // out
        template_parameter_list_t** out_deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Determining if template function '%s' is more specialized than '%s'\n",
                print_decl_type_str(template_1->type_information,
                    template_1->decl_context,
                    get_qualified_symbol_name(template_1, decl_context)),
                print_decl_type_str(template_2->type_information,
                    template_2->decl_context,
                    get_qualified_symbol_name(template_2, template_2->decl_context)));
    }
    if (out_deduced_template_arguments != NULL)
        *out_deduced_template_arguments = NULL;

    type_t* original_type_template_1 = template_1->type_information;
    type_t* transformed_type_template_1 = NULL;

    type_t* original_type_template_2 = template_2->type_information;
    type_t* transformed_type_template_2 = NULL;

    transform_type_for_ordering(
            template_1, &transformed_type_template_1,
            template_2, &transformed_type_template_2);

    template_parameter_list_t* explicit_template_arguments = NULL;
    deduction_result_t explicit_template_arguments_deduction =
        handle_explicit_template_arguments(
                // Note that since we are going to determine if template_1 is more specialized
                // then template_2, if it is the case it means that we are going to deduce
                // arguments for template_2
                //
                // primary-template: template <typename T> struct A;
                // template1: template <> struct A<int (*)(float)>;
                // template2: template <typename T, typename S> struct A<T(*)(S)>;
                //
                // template1 is more specialized than template2, thus T <- int, S <- float
                // which are template arguments of template2
                template_specialized_type_get_template_arguments(original_type_template_2),
                raw_explicit_template_arguments,
                decl_context,
                locus,
                &explicit_template_arguments);
    if (explicit_template_arguments_deduction == DEDUCTION_FAILURE)
        return 0;

    int num_types = -1;

    if (is_conversion
            || !is_overload)
    {
        num_types = 1;
    }
    else
    {
        num_types = num_actual_arguments;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: We have to compare %d types\n", num_types);
    }

    deduction_set_t* deduction_set = NEW0(deduction_set_t);
    deduction_set_t* inverse_deduction_set = NEW0(deduction_set_t);

    specialization_comparison_t cmp_1[num_types];
    comparison_extra_t cmp_1_extra[num_types];
    memset(cmp_1_extra, 0, sizeof(cmp_1_extra));

    specialization_comparison_t cmp_2[num_types];
    comparison_extra_t cmp_2_extra[num_types];
    memset(cmp_2_extra, 0, sizeof(cmp_2_extra));

    int i;
    for (i = 0; i < num_types; i++)
    {
        compute_single_deduction(&i,
                num_types,
                original_type_template_2,
                transformed_type_template_1,
                explicit_template_arguments,
                decl_context,
                locus,
                // flags
                is_conversion,
                is_overload,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                // out
                deduction_set,
                cmp_1,
                cmp_1_extra);
    }

    for (i = 0; i < num_types; i++)
    {
        compute_single_deduction(&i,
                num_types,
                original_type_template_1,
                transformed_type_template_2,
                explicit_template_arguments,
                decl_context,
                locus,
                // flags
                is_conversion,
                is_overload,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                // out
                inverse_deduction_set,
                cmp_2,
                cmp_2_extra);
    }

    for (i = 0; i < num_types; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEORDER: First comparison returns '%s'\n",
                    cmp_1[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least specialized" : "not comparable");
            fprintf(stderr, "TYPEORDER: Second comparison returns '%s'\n",
                    cmp_2[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least specialized" : "not comparable");
        }
        if (cmp_1[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED
                && cmp_2[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Trying to break tie because both return at least as specialized\n");
            }
            cmp_1[i] = break_tie(
                    !!(cmp_1_extra[i] & COMPARISON_EXTRA_BOTH_REFERENCES),
                    !!(cmp_1_extra[i] & COMPARISON_EXTRA_LVALUE_ARGUMENT),
                    !!(cmp_1_extra[i] & COMPARISON_EXTRA_LVALUE_PARAMETER),
                    !!(cmp_1_extra[i] & COMPARISON_EXTRA_ARG_IS_MORE_CV));
            cmp_2[i] = break_tie(
                    !!(cmp_2_extra[i] & COMPARISON_EXTRA_BOTH_REFERENCES),
                    !!(cmp_2_extra[i] & COMPARISON_EXTRA_LVALUE_ARGUMENT),
                    !!(cmp_2_extra[i] & COMPARISON_EXTRA_LVALUE_PARAMETER),
                    !!(cmp_2_extra[i] & COMPARISON_EXTRA_ARG_IS_MORE_CV));
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: After break, first comparison returns '%s'\n",
                        cmp_1[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least specialized" : "not comparable");
                fprintf(stderr, "TYPEORDER: After break, second comparison returns '%s'\n",
                        cmp_2[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED ? "at least specialized" : "not comparable");
            }
        }
    }

    template_parameter_list_t* updated_template_parameters_1 =
        build_template_parameter_list_from_deduction_set(
                template_specialized_type_get_template_parameters(
                    original_type_template_2),
                deduction_set);
    decl_context_t* updated_context_1 = decl_context_clone(decl_context);
    updated_context_1->template_parameters = updated_template_parameters_1;

    template_parameter_list_t* updated_template_parameters_2 =
        build_template_parameter_list_from_deduction_set(
                template_specialized_type_get_template_parameters(
                    original_type_template_1),
                inverse_deduction_set);
    decl_context_t* updated_context_2 = decl_context_clone(decl_context);
    updated_context_2->template_parameters = updated_template_parameters_2;

    for (i = 0; i < num_types; i++)
    {
        type_t* parameter1;
        type_t* parameter2;
        if (is_conversion)
        {
            parameter1 = function_type_get_return_type(original_type_template_2);
            parameter2 = function_type_get_return_type(original_type_template_1);
        }
        else if (!is_overload)
        {
            parameter1 = original_type_template_2;
            parameter2 = original_type_template_1;
        }
        else
        {
            if (i >= function_type_get_num_parameters(original_type_template_2))
                continue;
            parameter1 = function_type_get_parameter_type_num(original_type_template_2, i);
            if (i >= function_type_get_num_parameters(transformed_type_template_1))
                continue;
            parameter2 = function_type_get_parameter_type_num(transformed_type_template_1, i);
        }

        if (cmp_1[i] == COMPARISON_NONE)
        {
            // Do nothing
        }
        else if (!all_used_parameters_have_values(parameter1, updated_template_parameters_1))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Not all parameters of type %s have been deduced\n",
                        print_declarator(parameter1));
            }
            cmp_1[i] = COMPARISON_NONE;
        }
        else if (is_requiring_exact_match)
        {
            type_t* updated_type = update_type(parameter1, updated_context_1, locus);
            if (updated_type == NULL
                    || !equivalent_types(updated_type, parameter2))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEORDER: Update of type '%s' is '%s' and does not match '%s'\n",
                            print_declarator(parameter1),
                            print_declarator(updated_type),
                            print_declarator(parameter2));
                }
                cmp_1[i] = COMPARISON_NONE;
            }
        }

        if (cmp_2[i] == COMPARISON_NONE)
        {
            // Do nothing
        }
        else if (!all_used_parameters_have_values(parameter2, updated_template_parameters_2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: Not all parameters of type %s have been deduced\n",
                        print_declarator(parameter2));
            }
            cmp_2[i] = COMPARISON_NONE;
        }
        else if (is_requiring_exact_match)
        {
            type_t* updated_type = update_type(parameter2, updated_context_2, locus);
            if (updated_type == NULL
                    || !equivalent_types(updated_type, parameter1))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEORDER: Update of type '%s' is '%s' and does not match '%s'\n",
                            print_declarator(parameter2),
                            print_declarator(updated_type),
                            print_declarator(parameter1));
                }
                cmp_2[i] = COMPARISON_NONE;
            }
        }
    }

    free_template_parameter_list(updated_template_parameters_2);
    free_template_parameter_list(updated_template_parameters_1);

    char at_least_one_is_worse = 0;
    char at_least_one_is_strictly_more = 0;
    for (i = 0; i < num_types; i++)
    {
        if (cmp_1[i] == COMPARISON_NONE
                && cmp_2[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: First comparison of pair %d yields 'not comparable'\n", i);
            }
            at_least_one_is_strictly_more = 0;
            at_least_one_is_worse = 1;
            break;
        }
        if (cmp_1[i] == COMPARISON_AT_LEAST_AS_SPECIALIZED
                && cmp_2[i] != COMPARISON_AT_LEAST_AS_SPECIALIZED)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEORDER: First comparison of pair %d yields 'at least as specialized' "
                        "and the second yields 'not comparable'\n", i);
            }
            at_least_one_is_strictly_more = 1;
        }
    }

    if (!at_least_one_is_strictly_more
            && !at_least_one_is_worse
            && !is_conversion
            && is_overload)
    {
        // DR1395
        // So far the two functions look the same, but let's still make a final
        // attempt to distinguish this case.
        //
        // template <typename T>
        //     void f(T);            // #1
        // template <typename T, typename Q...>
        //     void f(T, Q...);      // #2
        //
        // f(1); // Calls #1
        //
        // The standard says that this call is ambiguous but DR1395 acknowledges that
        // it makes sense to prioritize the non-pack version. Note that if instead of
        // a function parameter pack, #2 had an ellipsis, this would be considered an
        // ambiguity

        // The exact behaviour in this case is still being drafted by the
        // Standard Committee, but this should do
        if (function_type_get_num_parameters(original_type_template_1)
                < function_type_get_num_parameters(original_type_template_2))
        {
            char may_be_better = 1;

            i = num_types;
            for (i = num_types;
                    may_be_better &&
                    (i < function_type_get_num_parameters(original_type_template_2));
                    i++)
            {
                // Note that ellipsis is not better or worse than a pack
                if (is_pack_type(function_type_get_parameter_type_num(original_type_template_2, i)))
                {
                    may_be_better = 1;
                }
                else
                {
                    may_be_better = 0;
                }
            }

            if (may_be_better)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEORDER: The first function is more specialized because it does not have "
                            "more parameters where the second expects a pack\n");
                }
                at_least_one_is_strictly_more = 1;
            }
        }
    }

    char result = at_least_one_is_strictly_more;
    if (result
            && out_deduced_template_arguments != NULL)
    {
        template_parameter_list_t* deduced_template_arguments = explicit_template_arguments;
        deduction_result_t deduction_finish_result =
            finish_deduced_template_arguments(
                    template_type_get_template_parameters(
                        template_specialized_type_get_related_template_type(
                            // See comment above in the invocation of
                            // handle_explicit_template_arguments
                            // why this is 2 and not 1
                            original_type_template_2)),
                    deduction_set,
                    decl_context,
                    locus,
                    /* inout */ deduced_template_arguments);

        if (deduction_finish_result == DEDUCTION_FAILURE)
        {
            result = 0;
        }
        else
        {
            *out_deduced_template_arguments = deduced_template_arguments;
        }
    }

    deduction_set_free(inverse_deduction_set);
    deduction_set_free(deduction_set);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Template function '%s' %s more specialized than '%s'\n",
                print_decl_type_str(template_1->type_information,
                    template_1->decl_context,
                    get_qualified_symbol_name(template_1, decl_context)),
                result ? "IS" : "IS NOT",
                print_decl_type_str(template_2->type_information,
                    template_2->decl_context,
                    get_qualified_symbol_name(template_2, template_2->decl_context)));
    }

    return result;
}

static char compare_template_classes(
        type_t* c1, type_t* c2,
        const decl_context_t* decl_context,
        const locus_t* locus,
        /* flags */
        char is_requiring_exact_match,
        /* out */
        template_parameter_list_t** deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Determining whether class '%s' is more specialized than '%s'\n",
                print_declarator(c1),
                print_declarator(c2));
    }
    ERROR_CONDITION(!is_named_class_type(c1)
            || !is_named_class_type(c2)
            || !is_template_specialized_type(get_actual_class_type(c1))
            || !is_template_specialized_type(get_actual_class_type(c2)),
            "Specialized classes are not", 0);

    parameter_info_t c1_parameters[1] =
    {
        { .is_ellipsis = 0, .type_info = c1, .nonadjusted_type_info = NULL }
    };
    parameter_info_t c2_parameters[1] =
    {
        { .is_ellipsis = 0, .type_info = c2, .nonadjusted_type_info = NULL }
    };

    // Symbol 1
    type_t* fake_primary_type_1 = get_new_function_type(get_void_type(), 
            c1_parameters, 1, REF_QUALIFIER_NONE);

    template_parameter_list_t* template_parameters1 = 
        duplicate_template_argument_list(
                template_specialized_type_get_template_parameters(get_actual_class_type(c1)
                    ));
    // Remove arguments from template parameters
    int i;
    for (i = 0; i < template_parameters1->num_parameters; i++)
    {
        template_parameters1->arguments[i] = NULL;
    }

    type_t* fake_template_type_1 = get_new_template_type(template_parameters1,
            fake_primary_type_1,
            "fake_template_name",
            named_type_get_symbol(c1)->decl_context,
            locus);
    scope_entry_t* fake_template_name_1 = NEW0(scope_entry_t);
    fake_template_name_1->symbol_name = "<<fake_template_function>>";
    fake_template_name_1->kind = SK_TEMPLATE;
    fake_template_name_1->decl_context = named_type_get_symbol(c1)->decl_context;
    fake_template_name_1->type_information = fake_template_type_1;
    template_type_set_related_symbol(fake_template_type_1,
            fake_template_name_1);

    type_t* fake_type_1 = named_type_get_symbol(
            template_type_get_primary_type(fake_template_type_1)
            )->type_information;

    scope_entry_t* fake_sym_1 = NEW0(scope_entry_t);
    fake_sym_1->symbol_name = "<<fake_template_function>>";
    fake_sym_1->kind = SK_FUNCTION;
    fake_sym_1->type_information = fake_type_1;
    fake_sym_1->decl_context = named_type_get_symbol(c1)->decl_context;

    // Symbol 2
    type_t* fake_primary_type_2 = get_new_function_type(get_void_type(), 
            c2_parameters, 1, REF_QUALIFIER_NONE);

    template_parameter_list_t* template_parameters2 = 
        duplicate_template_argument_list(
                template_specialized_type_get_template_parameters(get_actual_class_type(c2)
                    ));
    // Remove arguments from template parameters
    for (i = 0; i < template_parameters2->num_parameters; i++)
    {
        template_parameters2->arguments[i] = NULL;
    }

    type_t* fake_template_type_2 = get_new_template_type(template_parameters2,
            fake_primary_type_2,
            "fake_template_name",
            named_type_get_symbol(c2)->decl_context,
            locus);
    scope_entry_t* fake_template_name_2 = NEW0(scope_entry_t);
    fake_template_name_2->symbol_name = "<<fake_template_function>>";
    fake_template_name_2->kind = SK_TEMPLATE;
    fake_template_name_2->decl_context = named_type_get_symbol(c2)->decl_context;
    fake_template_name_2->type_information = fake_template_type_2;
    template_type_set_related_symbol(fake_template_type_2,
            fake_template_name_2);

    type_t* fake_type_2 = named_type_get_symbol(
            template_type_get_primary_type(fake_template_type_2)
            )->type_information;

    scope_entry_t* fake_sym_2  = NEW0(scope_entry_t);
    fake_sym_2->symbol_name = "<<fake_template_function>>";
    fake_sym_2->kind = SK_FUNCTION;
    fake_sym_2->type_information = fake_type_2;
    fake_sym_2->decl_context = named_type_get_symbol(c2)->decl_context;

    char result = is_more_specialized_template_function(
            fake_sym_1,
            fake_sym_2,
            /* explicit_template_argument */ NULL,
            decl_context,
            locus,
            // flags
            /* is_overload */ 0, /* num_actual_arguments */ -1,
            /* is_conversion */ 0,
            /* is_computing_address_of_function */ 0,
            /* is_deducing_arguments_from_function_declaration */ 0,
            is_requiring_exact_match,
            // out
            deduced_template_arguments
            );

    DELETE(fake_sym_2);
    DELETE(fake_sym_1);

    DELETE(fake_template_name_2);
    DELETE(fake_template_name_1);

    free_template_parameter_list(template_parameters1);
    free_template_parameter_list(template_parameters2);

    free_temporary_template_type(fake_template_type_2);
    free_temporary_template_type(fake_template_type_1);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEORDER: Class '%s' %s more specialized than '%s'\n",
                print_declarator(c1),
                result ? "IS" : "IS NOT",
                print_declarator(c2));
    }
    return result;
}

// States if c1 is more specialized than c2
char is_more_specialized_template_class(
        type_t* c1, type_t* c2,
        const decl_context_t* decl_context,
        const locus_t* locus)
{
    return compare_template_classes(
            c1,
            c2,
            decl_context,
            locus,
            // flags
            /* is_requiring_exact_match */ 1,
            // out
            /* deduced_template_arguments */ NULL);
}

char class_template_specialization_matches(
        type_t* c1, type_t* c2,
        const decl_context_t* decl_context,
        const locus_t* locus,
        /* out */
        template_parameter_list_t** deduced_template_arguments)
{
    return compare_template_classes(
            c1,
            c2,
            decl_context,
            locus,
            // flags
            /* is_requiring_exact_match */ 1,
            // out
            deduced_template_arguments);
}

char is_more_specialized_template_function_in_overload(
        scope_entry_t* f1,
        scope_entry_t* f2,
        const decl_context_t* decl_context,
        template_parameter_list_t* explicit_template_arguments,
        const locus_t* locus,
        // Flags
        int num_actual_arguments,
        char is_conversion)
{
    char result = is_more_specialized_template_function(
            f1,
            f2,
            explicit_template_arguments,
            decl_context,
            locus,
            // flags
            /* is_overload */ 1, num_actual_arguments,
            is_conversion,
            /* is_computing_address_of_function */ 0,
            /* is_deducing_arguments_from_function_declaration */ 0,
            /* is_requiring_exact_match */ 0,
            // out
            /* deduced_template_arguments */ NULL);
    return result;
}

char is_more_specialized_template_function_in_function_address(
        scope_entry_t* f1,
        scope_entry_t* f2,
        const decl_context_t* decl_context,
        template_parameter_list_t* explicit_template_arguments,
        const locus_t* locus,
        // Flags
        char is_conversion)
{
    char result = is_more_specialized_template_function(
            f1,
            f2,
            explicit_template_arguments,
            decl_context,
            locus,
            // flags
            /* is_overload */ 0, -1,
            is_conversion,
            /* is_computing_address_of_function */ 1,
            /* is_deducing_arguments_from_function_declaration */ 0,
            /* is_requiring_exact_match */ 0,
            // out
            /* deduced_template_arguments */ NULL);

    return result;
}
