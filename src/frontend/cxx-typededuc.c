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
#include <stdint.h>
#include <limits.h>

#include "cxx-typededuc.h"
#include "cxx-typeorder.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-overload.h"
#include "cxx-cexpr.h"
#include "cxx-instantiation.h"
#include "cxx-entrylist.h"
#include "cxx-codegen.h"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"
#include "cxx-buildscope.h"

static void print_deduction_set(deduction_set_t* deduction_set)
{
    int i_deductions;
    for (i_deductions = 0; i_deductions < deduction_set->num_deductions; i_deductions++)
    {
        if (i_deductions > 0)
            fprintf(stderr, "TYPEDEDUC:\n");

        deduction_t* current_deduction = deduction_set->deduction_list[i_deductions];

        fprintf(stderr, "TYPEDEDUC:    Name:     %s\n", current_deduction->parameter_name);
        fprintf(stderr, "TYPEDEDUC:    Position: %d\n", current_deduction->parameter_position);
        fprintf(stderr, "TYPEDEDUC:    Nesting:  %d\n", current_deduction->parameter_nesting);

        switch (current_deduction->kind)
        {
            case TPK_TYPE:
                {
                    fprintf(stderr, "TYPEDEDUC:    Type template parameter\n");
                    break;
                }
            case TPK_TEMPLATE:
                {
                    fprintf(stderr, "TYPEDEDUC:    Template template parameter\n");
                    break;
                }
            case TPK_NONTYPE:
                {
                    fprintf(stderr, "TYPEDEDUC:    Nontype template parameter\n");
                    break;
                }
            case TPK_TYPE_PACK:
                {
                    fprintf(stderr, "TYPEDEDUC:    Type template parameter pack\n");
                    break;
                }
            case TPK_TEMPLATE_PACK:
                {
                    fprintf(stderr, "TYPEDEDUC:    Template template parameter pack\n");
                    break;
                }
            case TPK_NONTYPE_PACK:
                {
                    fprintf(stderr, "TYPEDEDUC:    Nontype template parameter pack\n");
                    break;
                }
            default:
                {
                    fprintf(stderr, "TYPEDEDUC:    ??? Unknown template parameter kind ???\n");
                    break;
                }
        }

        int j;
        for (j = 0; j < current_deduction->num_deduced_parameters; j++)
        {
            switch (current_deduction->kind)
            {
                case TPK_TYPE:
                case TPK_TYPE_PACK:
                    {
                        fprintf(stderr, "TYPEDEDUC:    [%d] Deduced type: %s\n", j,
                                print_declarator(current_deduction->deduced_parameters[j]->type));
                        break;
                    }
                case TPK_TEMPLATE:
                case TPK_TEMPLATE_PACK:
                    {
                        fprintf(stderr, "TYPEDEDUC:    [%d] Deduced type: %s\n", j,
                                print_declarator(current_deduction->deduced_parameters[j]->type));
                        break;
                    }
                case TPK_NONTYPE:
                case TPK_NONTYPE_PACK:
                    {
                        fprintf(stderr, "TYPEDEDUC:    [%d] Deduced expression: %s\n", j,
                                nodecl_is_null(current_deduction->deduced_parameters[j]->value) ? "<<NULL>>" :
                                codegen_to_str(current_deduction->deduced_parameters[j]->value,
                                    nodecl_retrieve_context(current_deduction->deduced_parameters[j]->value)));
                        fprintf(stderr, "TYPEDEDUC:    [%d] (Deduced) Type: %s\n", j,
                                print_declarator(current_deduction->deduced_parameters[j]->type));
                        break;
                    }
                default:
                    internal_error("Invalid template parameter kind", 0);
            }
        }
    }
}

template_parameter_list_t* build_template_parameter_list_from_deduction_set(
        template_parameter_list_t* template_parameters,
        deduction_set_t* deduction_set)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Creating template argument list from deduction set\n");
    }
    template_parameter_list_t* result = duplicate_template_argument_list(template_parameters);

    int nesting = get_template_nesting_of_template_parameters(template_parameters);

    int i;
    for (i = 0; i < deduction_set->num_deductions; i++)
    {
        deduction_t* current_deduction = deduction_set->deduction_list[i];

        template_parameter_value_t* argument = NEW0(template_parameter_value_t);

        switch (current_deduction->kind)
        {
            case TPK_TYPE:
            case TPK_TEMPLATE:
            case TPK_TYPE_PACK:
            case TPK_TEMPLATE_PACK:
                {
                    argument->kind = template_parameter_kind_get_base_kind(current_deduction->kind);
                    if (current_deduction->kind == TPK_TYPE_PACK
                            || current_deduction->kind == TPK_TEMPLATE_PACK)
                    {
                        type_t* seq_type[current_deduction->num_deduced_parameters + 1];
                        int j;
                        for (j = 0; j < current_deduction->num_deduced_parameters; j++)
                        {
                            if (current_deduction->deduced_parameters[j] != NULL)
                            {
                                seq_type[j] = current_deduction->deduced_parameters[j]->type;
                            }
                            else
                            {
                                seq_type[j] = get_error_type();
                            }
                        }

                        argument->type = get_sequence_of_types(
                                current_deduction->num_deduced_parameters,
                                seq_type);
                    }
                    else
                    {
                        ERROR_CONDITION(current_deduction->num_deduced_parameters != 1,
                                "Bad deduction num_deduced_parameters != 1 (%d)",
                                current_deduction->num_deduced_parameters);

                        argument->type = current_deduction->deduced_parameters[0]->type;
                    }
                    DEBUG_CODE()
                    {
                        const char* template_parameter_kind_name = "<<unknown>>";
                        switch (current_deduction->kind)
                        {
                            case TPK_TYPE: template_parameter_kind_name = "type template parameter"; break;
                            case TPK_TYPE_PACK: template_parameter_kind_name = "type template parameter pack"; break;
                            case TPK_TEMPLATE: template_parameter_kind_name = "template template parameter"; break;
                            case TPK_TEMPLATE_PACK: template_parameter_kind_name = "template template parameter pack"; break;
                            default: break;
                        }
                        fprintf(stderr, "TYPEDEDUC: Position '%d' and nesting '%d' %s updated to %s\n",
                                current_deduction->parameter_position,
                                nesting,
                                template_parameter_kind_name,
                                print_declarator(argument->type));
                    }
                }
                break;
            case TPK_NONTYPE:
            case TPK_NONTYPE_PACK:
                {
                    argument->kind = template_parameter_kind_get_base_kind(current_deduction->kind);
                    if (current_deduction->kind == TPK_NONTYPE_PACK)
                    {
                        type_t* seq_type[current_deduction->num_deduced_parameters + 1];
                        argument->value = nodecl_null();

                        int j;
                        for (j = 0; j < current_deduction->num_deduced_parameters; j++)
                        {
                            if (current_deduction->deduced_parameters[j] != NULL)
                            {
                                seq_type[j] = current_deduction->deduced_parameters[j]->type;

                                argument->value = nodecl_append_to_list(
                                        argument->value,
                                        nodecl_shallow_copy(current_deduction->deduced_parameters[j]->value));
                            }
                            else
                            {
                                seq_type[j] = get_error_type();

                                argument->value = nodecl_append_to_list(
                                        argument->value,
                                        nodecl_shallow_copy(
                                            nodecl_make_err_expr(make_locus("", 0, 0))));
                            }
                        }

                        argument->type = get_sequence_of_types(
                                current_deduction->num_deduced_parameters,
                                seq_type);
                    }
                    else
                    {
                        ERROR_CONDITION(current_deduction->num_deduced_parameters != 1,
                                "Bad deduction num_deduced_parameters != 1 (%d)",
                                current_deduction->num_deduced_parameters);

                        argument->type = current_deduction->deduced_parameters[0]->type;
                        argument->value = nodecl_shallow_copy(current_deduction->deduced_parameters[0]->value);
                    }

                    DEBUG_CODE()
                    {
                        const char* template_parameter_kind_name = "<<unknown>>";
                        switch (current_deduction->kind)
                        {
                            case TPK_NONTYPE: template_parameter_kind_name = "non-type template parameter"; break;
                            case TPK_NONTYPE_PACK: template_parameter_kind_name = "non-type template parameter pack"; break;
                            default: break;
                        }
                        fprintf(stderr, "TYPEDEDUC: Position '%d' and nesting '%d' %s updated to %s\n",
                                current_deduction->parameter_position,
                                nesting,
                                template_parameter_kind_name,
                                nodecl_is_null(argument->value) ? "<<NULL>>" : 
                                codegen_to_str(argument->value, nodecl_retrieve_context(argument->value)));
                    }
                }
                break;
            default:
                {
                    internal_error("Invalid template parameter type", 0);
                }
        }

        result->arguments[current_deduction->parameter_position] = argument;
    }
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Template parameters building after deduction ended\n");
    }

    return result;
}

static void deduction_free(deduction_t* deduction)
{
    int i;
    for (i = 0; i < deduction->num_deduced_parameters; i++)
    {
        if (deduction->deduced_parameters[i] != NULL)
        {
            nodecl_free(deduction->deduced_parameters[i]->value);
        }
        DELETE(deduction->deduced_parameters[i]);
    }

    DELETE(deduction->deduced_parameters);
    deduction->deduced_parameters = 0;

    deduction->num_deduced_parameters = 0;

    DELETE(deduction);
}

static void deduction_set_clear(deduction_set_t* deduction_set)
{
    int i;
    for (i = 0; i < deduction_set->num_deductions; i++)
    {
        deduction_free(deduction_set->deduction_list[i]);
    }

    DELETE(deduction_set->deduction_list);
    deduction_set->deduction_list = 0;

    deduction_set->num_deductions = 0;
}

void deduction_set_free(deduction_set_t* deduction_set)
{
    if (deduction_set == NULL)
        return;

    deduction_set_clear(deduction_set);
    DELETE(deduction_set);
}

static char same_expression_value(
        nodecl_t arg1,
        nodecl_t arg2)
{
    if (nodecl_is_constant(arg1)
            && nodecl_is_constant(arg2))
    {
        return const_value_is_nonzero(
                const_value_eq(
                    nodecl_get_constant(arg1),
                    nodecl_get_constant(arg2)));
    }
    else
    {
        return same_functional_expression(
                arg1,
                arg2);
    }
}

static char deduced_argument_is_the_same(
        enum template_parameter_kind kind,
        deduced_argument_t* arg1,
        deduced_argument_t* arg2)
{
    switch (kind)
    {
        case TPK_NONTYPE:
            return same_expression_value(
                    arg1->value,
                    arg2->value);
            break;
        case TPK_TEMPLATE:
        case TPK_TYPE:
            return equivalent_types(arg1->type, arg2->type);
            break;
        default:
            internal_error("Code unreachable", 0);
    }
}

static deduction_result_t deduction_combine_aux(
        deduction_set_t* current_deduction,
        // out
        deduction_set_t* previous_deduction)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Combining deductions\n");
    }
    int i, num_previous_deductions = previous_deduction->num_deductions;
    for (i = 0; i < current_deduction->num_deductions; i++)
    {
        deduction_t* current = current_deduction->deduction_list[i];
        // Check if a deduction in the current_deduction set appears
        // in previous_deduction set. If it does not, append
        int j;
        char found = 0;
        for (j = 0; j < num_previous_deductions && !found; j++)
        {
            deduction_t* previous = previous_deduction->deduction_list[j];

            found = (current->kind == previous->kind
                    && current->parameter_position == previous->parameter_position
                    && current->parameter_nesting == previous->parameter_nesting);
        }

        if (!found)
        {
            // Add deduction
            deduction_t* new_deduction = NEW0(deduction_t);
            new_deduction->kind = current->kind;
            new_deduction->parameter_position = current->parameter_position;
            new_deduction->parameter_nesting = current->parameter_nesting;
            new_deduction->parameter_name = current->parameter_name;

            new_deduction->num_deduced_parameters = current->num_deduced_parameters;
            new_deduction->deduced_parameters = NEW_VEC0(
                    deduced_argument_t*,
                    current->num_deduced_parameters);
            for (j = 0; j < current->num_deduced_parameters; j++)
            {
                if (current->deduced_parameters[j] != NULL)
                {
                    deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
                    *new_deduced_argument = *(current->deduced_parameters[j]);
                    new_deduced_argument->value = nodecl_shallow_copy(current->deduced_parameters[j]->value);
                    new_deduction->deduced_parameters[j] = new_deduced_argument;
                }
            }

            P_LIST_ADD(previous_deduction->deduction_list,
                    previous_deduction->num_deductions,
                    new_deduction);
        }
        else
        {
            deduction_t* previous = previous_deduction->deduction_list[j - 1];
            if (current->num_deduced_parameters !=
                    previous->num_deduced_parameters)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deductions cannot be combined because "
                            "we have a mismatch in the number of values\n");
                    fprintf(stderr, "TYPEDEDUC: Mismatch happens for template parameter: '%s'\n",
                            previous->parameter_name);
                }
                return DEDUCTION_FAILURE;
            }

            for (j = 0;
                    j < current->num_deduced_parameters;
                    j++)
            {
                if ((previous->deduced_parameters[j] == NULL) ==
                        (current->deduced_parameters[j] == NULL))
                {
                    if (previous->deduced_parameters[j] == NULL)
                    {
                        // Both positions are still placeholders, do nothing
                    }
                    else if (!deduced_argument_is_the_same(
                                template_parameter_kind_get_base_kind(previous->kind),
                                previous->deduced_parameters[j],
                                current->deduced_parameters[j]))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: Deductions cannot be combined because "
                                    "we have a mismatch in a value\n");
                            fprintf(stderr, "TYPEDEDUC: Mismatch happens for template parameter: '%s'\n",
                                    previous->parameter_name);
                        }
                        return DEDUCTION_FAILURE;
                    }
                }
                else if (previous->deduced_parameters[j] == NULL)
                {
                    // Copy in the placeholder of previous
                    deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);

                    *new_deduced_argument = *(current->deduced_parameters[j]);
                    new_deduced_argument->value = nodecl_shallow_copy(current->deduced_parameters[j]->value);
                    previous->deduced_parameters[j] = new_deduced_argument;
                }
            }
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deductions successfully combined\n");
    }
    return DEDUCTION_OK;
}


deduction_result_t deduction_combine_to_second(
        deduction_set_t* current_deduction,
        // out
        deduction_set_t* previous_deduction)
{
    return deduction_combine_aux(
            current_deduction,
            previous_deduction);
}

static template_parameter_list_t* flatten_template_arguments(template_parameter_list_t* argument_template_argument_list)
{
    template_parameter_list_t* flattened_template_arguments =
        NEW0(template_parameter_list_t);
    int i_arg = 0;

    // Expand all the arguments
    while (i_arg < argument_template_argument_list->num_parameters)
    {
        switch (argument_template_argument_list->arguments[i_arg]->kind)
        {
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    if (is_sequence_of_types(argument_template_argument_list->arguments[i_arg]->type))
                    {
                        int k, num_items = sequence_of_types_get_num_types(
                                argument_template_argument_list->arguments[i_arg]->type);
                        for (k = 0; k < num_items; k++)
                        {
                            template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                            new_value->kind = argument_template_argument_list->arguments[i_arg]->kind;
                            new_value->type =
                                sequence_of_types_get_type_num(argument_template_argument_list->arguments[i_arg]->type, k);

                            P_LIST_ADD(flattened_template_arguments->arguments,
                                    flattened_template_arguments->num_parameters,
                                    new_value);
                        }
                    }
                    else
                    {
                        template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                        *new_value = *(argument_template_argument_list->arguments[i_arg]);
                        P_LIST_ADD(flattened_template_arguments->arguments,
                                flattened_template_arguments->num_parameters,
                                new_value);
                    }
                    break;
                }
            case TPK_NONTYPE:
                {
                    if (nodecl_is_list_or_null(argument_template_argument_list->arguments[i_arg]->value))
                    {
                        int num_items = 0;
                        nodecl_t* list = nodecl_unpack_list(argument_template_argument_list->arguments[i_arg]->value, &num_items);

                        int k;
                        for (k = 0; k < num_items; k++)
                        {
                            template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                            new_value->kind = argument_template_argument_list->arguments[i_arg]->kind;
                            if (is_sequence_of_types(argument_template_argument_list->arguments[i_arg]->type))
                            {
                                new_value->type = sequence_of_types_get_type_num(
                                        argument_template_argument_list->arguments[i_arg]->type,
                                        k);
                            }
                            else
                            {
                                new_value->type = argument_template_argument_list->arguments[i_arg]->type;
                            }
                            new_value->value = nodecl_shallow_copy(list[k]);

                            P_LIST_ADD(flattened_template_arguments->arguments,
                                    flattened_template_arguments->num_parameters,
                                    new_value);
                        }
                        DELETE(list);
                    }
                    else
                    {
                        template_parameter_value_t *new_value = NEW0(template_parameter_value_t);
                        *new_value = *(argument_template_argument_list->arguments[i_arg]);
                        new_value->value = nodecl_shallow_copy(argument_template_argument_list->arguments[i_arg]->value);
                        P_LIST_ADD(flattened_template_arguments->arguments,
                                flattened_template_arguments->num_parameters,
                                new_value);
                    }
                    break;
                }
            default: internal_error("Code unreachable", 0);
        }
        i_arg++;
    }

    return flattened_template_arguments;
}

static void expression_enumerate_template_parameter_packs(
        nodecl_t n,
        char only_deduced_contexts,
        // out
        scope_entry_t*** parameter_packs,
        int *num_parameter_packs)
{
    if (nodecl_is_null(n))
        return;

    if (only_deduced_contexts)
    {
        if (nodecl_get_kind(n) == NODECL_SYMBOL
                && nodecl_get_symbol(n)->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
        {
            P_LIST_ADD(*parameter_packs, *num_parameter_packs, nodecl_get_symbol(n));
        }
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            expression_enumerate_template_parameter_packs(
                    nodecl_get_child(n, i),
                    only_deduced_contexts,
                    parameter_packs,
                    num_parameter_packs);
        }
    }
}

static void type_enumerate_template_parameter_packs(
        type_t* t,
        char only_deduced_contexts,
        // out
        scope_entry_t*** parameter_packs,
        int *num_parameter_packs)
{
    if (t == NULL)
        return;

    if (!is_dependent_type(t))
        return;

    if (is_pack_type(t)
            && !only_deduced_contexts)
    {
        type_enumerate_template_parameter_packs(
                pack_type_get_packed_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
    }
    else if (is_named_type(t)
            && (named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK))
    {
        P_LIST_ADD(*parameter_packs, *num_parameter_packs, named_type_get_symbol(t));
    }
    else if (is_template_type(t)
            && template_type_get_related_symbol(t) != NULL
            && template_type_get_related_symbol(t)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
    {
        P_LIST_ADD(*parameter_packs, *num_parameter_packs,
                template_type_get_related_symbol(t));
    }
    else if (is_named_class_type(t)
            && is_template_specialized_type(
                named_type_get_symbol(t)->type_information))
    {
        // Check TT and arguments
        int i;
        template_parameter_list_t* template_arguments =
            template_specialized_type_get_template_arguments(
                    named_type_get_symbol(t)->type_information);
        for (i = 0; i < template_arguments->num_parameters; i++)
        {
            switch (template_arguments->arguments[i]->kind)
            {
                case TPK_TYPE:
                case TPK_TEMPLATE:
                    type_enumerate_template_parameter_packs(
                                template_arguments->arguments[i]->type,
                                only_deduced_contexts,
                                parameter_packs,
                                num_parameter_packs);
                    break;
                case TPK_NONTYPE:
                    expression_enumerate_template_parameter_packs(
                                template_arguments->arguments[i]->value,
                                only_deduced_contexts,
                                parameter_packs,
                                num_parameter_packs);
                    break;
                default:
                    internal_error("Invalid template argument kind", 0);
            }
        }

        type_enumerate_template_parameter_packs(
                    template_specialized_type_get_related_template_type(
                        named_type_get_symbol(t)->type_information),
                only_deduced_contexts,
                    parameter_packs,
                    num_parameter_packs);
    }
    else if (is_pointer_type(t))
    {
        type_enumerate_template_parameter_packs(
                pointer_type_get_pointee_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        type_enumerate_template_parameter_packs(
                no_ref(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
    }
    else if (is_pointer_to_member_type(t))
    {
        type_enumerate_template_parameter_packs(
                pointer_to_member_type_get_class_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
        type_enumerate_template_parameter_packs(
                pointer_type_get_pointee_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
    }
    else if (is_array_type(t))
    {
        type_enumerate_template_parameter_packs(
                array_type_get_element_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
        expression_enumerate_template_parameter_packs(
                array_type_get_array_size_expr(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);
    }
    else if (is_function_type(t))
    {
        type_enumerate_template_parameter_packs(
                function_type_get_return_type(t),
                only_deduced_contexts,
                parameter_packs,
                num_parameter_packs);

        int num_parameters = function_type_get_num_parameters(t);
        if (function_type_get_has_ellipsis(t))
            num_parameters--;

        int i;
        for (i = 0; i < num_parameters; i++)
        {
            type_enumerate_template_parameter_packs(
                        function_type_get_parameter_type_num(t, i),
                        only_deduced_contexts,
                        parameter_packs,
                        num_parameter_packs);
        }
    }
    else if (is_vector_type(t))
    {
        type_enumerate_template_parameter_packs(
                    vector_type_get_element_type(t),
                    only_deduced_contexts,
                    parameter_packs,
                    num_parameter_packs);
    }
}

static deduction_result_t deduce_empty_parameter_packs(
        scope_entry_t** parameter_packs,
        int num_parameter_packs,
        deduction_set_t* deduction_result)
{
    int i;
    for (i = 0; i < num_parameter_packs; i++)
    {
        scope_entry_t* current_pack = parameter_packs[i];

        deduction_set_t* deduction_current_parameter =
            NEW0(deduction_set_t);

        deduction_t* new_deduction = NEW0(deduction_t);

        switch (current_pack->kind)
        {
            case SK_TEMPLATE_TEMPLATE_PARAMETER_PACK:
                new_deduction->kind = TPK_TEMPLATE_PACK;
                break;
            case SK_TEMPLATE_TYPE_PARAMETER_PACK:
                new_deduction->kind = TPK_TYPE_PACK;
                break;
            case SK_TEMPLATE_NONTYPE_PARAMETER_PACK:
                new_deduction->kind = TPK_NONTYPE_PACK;
                break;
            default:
                internal_error("Code unreachable", 0);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Template parameter pack '%s' has been deduced the empty value sequence\n",
                    current_pack->symbol_name);
        }

        new_deduction->parameter_position = symbol_entity_specs_get_template_parameter_position(current_pack);
        new_deduction->parameter_nesting = symbol_entity_specs_get_template_parameter_nesting(current_pack);
        new_deduction->parameter_name = current_pack->symbol_name;
        // new_deduction->num_deduced_parameters = 0;

        P_LIST_ADD(deduction_current_parameter->deduction_list,
                deduction_current_parameter->num_deductions,
                new_deduction);

        deduction_result_t deduction_result_value = deduction_combine_to_second(
                deduction_current_parameter,
                deduction_result);

        deduction_set_free(deduction_current_parameter);
        if (deduction_result_value == DEDUCTION_FAILURE)
        {
            return DEDUCTION_FAILURE;
        }
    }

    return DEDUCTION_OK;
}

static char template_parameter_participates_in_deduction(
        scope_entry_t* sym,
        template_parameter_list_t* explicit_template_arguments,
        int pack_index)
{
    int i;
    for (i = 0; i < explicit_template_arguments->num_parameters; i++)
    {
        if ((explicit_template_arguments->parameters[i]->entry->kind
                    == sym->kind)
                && (symbol_entity_specs_get_template_parameter_position(explicit_template_arguments->parameters[i]->entry)
                    == symbol_entity_specs_get_template_parameter_position(sym))
                && (symbol_entity_specs_get_template_parameter_nesting(explicit_template_arguments->parameters[i]->entry)
                    == symbol_entity_specs_get_template_parameter_nesting(sym)))
        {
            if (sym->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                    || sym->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK
                    || sym->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
            {
                if (pack_index == -1
                        || explicit_template_arguments->arguments[i] == NULL)
                {
                    // The pack does participate
                    return 1;
                }
                // The pack participates only if the explicit template arguments do not already give a value
                // for it in this expansion
                else if (sym->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
                {
                    return !nodecl_is_list_or_null(explicit_template_arguments->arguments[i]->value)
                        || (pack_index >= nodecl_list_length(explicit_template_arguments->arguments[i]->value));
                }
                else
                {
                    return !is_sequence_of_types(explicit_template_arguments->arguments[i]->type)
                        || (pack_index >= sequence_of_types_get_num_types(explicit_template_arguments->arguments[i]->type));
                }
            }
            else
            {
                // This (nonpack) template parameter participates if it has not
                // already been given any value
                return (explicit_template_arguments->arguments[i] == NULL);
            }
        }
    }

    return 0;
}

static char expression_is_a_participating_nontype_parameter(nodecl_t n,
        template_parameter_list_t* explicit_template_arguments,
        int pack_index)
{
    if (nodecl_get_kind(n) == NODECL_SYMBOL
            && nodecl_get_symbol(n)->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
    {
        return template_parameter_participates_in_deduction(
                nodecl_get_symbol(n),
                explicit_template_arguments,
                /* pack_index */ -1);
    }
    else if (pack_index != -1
            && nodecl_get_kind(n) == NODECL_SYMBOL
            && nodecl_get_symbol(n)->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
    {
        return template_parameter_participates_in_deduction(
                nodecl_get_symbol(n),
                explicit_template_arguments,
                pack_index);
    }

    return 0;
}

static char expression_contains_participating_template_parameters(
        nodecl_t n,
        template_parameter_list_t* explicit_template_arguments,
        int pack_index)
{
    if (nodecl_is_null(n))
        return 0;

    if (expression_is_a_participating_nontype_parameter(n,
                explicit_template_arguments,
                pack_index))
        return 1;

    if (nodecl_get_kind(n) == NODECL_CXX_VALUE_PACK)
        return expression_is_a_participating_nontype_parameter(
                nodecl_get_child(n, 0),
                explicit_template_arguments,
                pack_index);

    return 0;
}

static char type_contains_participating_template_parameters(type_t* t,
        template_parameter_list_t* explicit_template_arguments,
        int pack_index)
{
    if (t == NULL)
        return 0;

    if (!is_dependent_type(t))
        return 0;

    if (is_named_type(t)
            && (named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                || named_type_get_symbol(t)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
    {
        return template_parameter_participates_in_deduction(
                named_type_get_symbol(t),
                explicit_template_arguments,
                pack_index);
    }
    else if (is_named_class_type(t)
            && is_template_specialized_type(
                named_type_get_symbol(t)->type_information))
    {
        // Check TT and arguments
        int i;
        template_parameter_list_t* template_arguments =
            flatten_template_arguments(
                    template_specialized_type_get_template_arguments(
                        named_type_get_symbol(t)->type_information));
        char template_arg_is_undeduced = 0;
        for (i = 0; i < template_arguments->num_parameters; i++)
        {
            switch (template_arguments->arguments[i]->kind)
            {
                case TPK_TYPE:
                case TPK_TEMPLATE:
                    if (is_pack_type(template_arguments->arguments[i]->type))
                    {
                        if (is_pack_type(template_arguments->arguments[i]->type)
                                && (i + 1) != template_arguments->num_parameters)
                        {
                            template_arg_is_undeduced = 1;
                            break;
                        }
                    }
                    break;
                case TPK_NONTYPE:
                    {
                        if (nodecl_is_list_or_null(template_arguments->arguments[i]->value)
                                && (i + 1) != template_arguments->num_parameters)
                        {
                            template_arg_is_undeduced = 1;
                            break;
                        }
                        break;
                    }
                default:
                    internal_error("Invalid template argument kind", 0);
            }
        }

        if (!template_arg_is_undeduced)
        {
            for (i = 0; i < template_arguments->num_parameters; i++)
            {
                switch (template_arguments->arguments[i]->kind)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        if (is_pack_type(template_arguments->arguments[i]->type))
                        {
                            if (type_contains_participating_template_parameters(
                                        pack_type_get_packed_type(
                                            template_arguments->arguments[i]->type),
                                        explicit_template_arguments,
                                        // This would start a new expansion level
                                        /* pack_index */ -1))
                            {
                                free_template_parameter_list(template_arguments);
                                return 1;
                            }
                        }
                        else if (type_contains_participating_template_parameters(
                                    template_arguments->arguments[i]->type,
                                    explicit_template_arguments,
                                    pack_index))
                        {
                            free_template_parameter_list(template_arguments);
                            return 1;
                        }
                        break;
                    case TPK_NONTYPE:
                        if (expression_contains_participating_template_parameters(
                                    template_arguments->arguments[i]->value,
                                    explicit_template_arguments,
                                    pack_index))
                        {
                            free_template_parameter_list(template_arguments);
                            return 1;
                        }
                        break;
                    default:
                        internal_error("Invalid template argument kind", 0);
                }
            }
        }

        if (type_contains_participating_template_parameters(
                    get_user_defined_type(
                        template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(
                                named_type_get_symbol(t)->type_information))),
                    explicit_template_arguments,
                    pack_index))
        {
            free_template_parameter_list(template_arguments);
            return 1;
        }

        free_template_parameter_list(template_arguments);
    }
    else if (is_pointer_type(t))
    {
        return type_contains_participating_template_parameters(
                pointer_type_get_pointee_type(t),
                explicit_template_arguments,
                pack_index);
    }
    else if (is_lvalue_reference_type(t)
            || is_rvalue_reference_type(t))
    {
        return type_contains_participating_template_parameters(
                no_ref(t),
                explicit_template_arguments,
                pack_index);
    }
    else if (is_pointer_to_member_type(t))
    {
        return type_contains_participating_template_parameters(
                pointer_to_member_type_get_class_type(t),
                explicit_template_arguments,
                pack_index)
            || type_contains_participating_template_parameters(
                    pointer_type_get_pointee_type(t),
                    explicit_template_arguments,
                    pack_index);
    }
    else if (is_array_type(t))
    {
        return type_contains_participating_template_parameters(
                array_type_get_element_type(t),
                explicit_template_arguments,
                pack_index)
            || expression_contains_participating_template_parameters(
                    array_type_get_array_size_expr(t),
                    explicit_template_arguments,
                    pack_index);
    }
    else if (is_function_type(t))
    {
        if (type_contains_participating_template_parameters(
                    function_type_get_return_type(t),
                    explicit_template_arguments,
                    pack_index))
            return 1;

        int num_parameters = function_type_get_num_parameters(t);
        if (function_type_get_has_ellipsis(t))
            num_parameters--;

        int i;
        for (i = 0; i < num_parameters; i++)
        {
            // function packs that does not occur at the end of a
            // parameter-declaration-list is not deduced
            if (is_pack_type(function_type_get_parameter_type_num(t, i)))
            {
                if ((i+1) != function_type_get_num_parameters(t))
                    continue;

                return type_contains_participating_template_parameters(
                        pack_type_get_packed_type(
                            function_type_get_parameter_type_num(t, i)),
                        explicit_template_arguments,
                        // This would start a new expansion level
                        /* pack_index */ -1);
            }
            else if (type_contains_participating_template_parameters(
                        function_type_get_parameter_type_num(t, i),
                        explicit_template_arguments,
                        pack_index))
                return 1;
        }
    }
    else if (is_vector_type(t))
    {
        return (type_contains_participating_template_parameters(
                    vector_type_get_element_type(t),
                    explicit_template_arguments,
                    pack_index));
    }
    else if (is_sequence_of_types(t))
    {
        internal_error("Invalid type here", 0);
    }
    else if (is_pack_type(t))
    {
        internal_error("Invalid type here", 0);
    }

    return 0;
}

static deduction_result_t deduce_template_arguments_from_a_value(
        nodecl_t parameter,
        nodecl_t argument,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context UNUSED_PARAMETER,
        const locus_t* locus UNUSED_PARAMETER,
        int pack_index,
        int pack_length,
        // flags
        char is_computing_address_of_function UNUSED_PARAMETER,
        char is_deducing_arguments_from_function_declaration UNUSED_PARAMETER,
        char is_partial_ordering UNUSED_PARAMETER,
        char is_array_size,
        char is_template_argument_list,
        // out
        deduction_set_t* deduction_result)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from a value expression\n");
        fprintf(stderr, "TYPEDEDUC:   '%s' <- '%s'\n",
                codegen_to_str(parameter, decl_context),
                codegen_to_str(argument, decl_context));
    }

    const_value_t* param_const = NULL;
    const_value_t* arg_const = NULL;
    if (nodecl_is_constant(parameter)
            && nodecl_is_constant(argument)

            && !const_value_is_object((param_const = nodecl_get_constant(parameter)))
            && !const_value_is_address(param_const)
            && !const_value_is_object((arg_const = nodecl_get_constant(argument)))
            && !const_value_is_address(arg_const))
    {
        if (const_value_is_nonzero(
                    const_value_eq(
                        param_const,
                        arg_const)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Expressions '%s' and '%s' evaluate to the same constant\n",
                        codegen_to_str(parameter, decl_context),
                        codegen_to_str(argument, decl_context));
            }
            return DEDUCTION_OK;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Expressions '%s' and '%s' evaluate to different constants. Deduction fails\n",
                        codegen_to_str(parameter, decl_context),
                        codegen_to_str(argument, decl_context));
            }
            return DEDUCTION_OK;
        }
    }
    else if (!expression_contains_participating_template_parameters(
                parameter,
                explicit_template_arguments,
                pack_index))
    {
        if (nodecl_expr_is_value_dependent(parameter))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Expression '%s' is a non-deduced context\n",
                        codegen_to_str(parameter, decl_context));
            }
            return DEDUCTION_OK;
        }
        else if (same_functional_expression(parameter, argument))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Expression '%s' and '%s' are functionally equivalent\n",
                        codegen_to_str(parameter, decl_context),
                        codegen_to_str(argument, decl_context));
            }
            return DEDUCTION_OK;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Deduction fails because the two expressions are not functionally equivalent\n");
            }
            return DEDUCTION_FAILURE;
        }
    }
    else if (expression_is_a_participating_nontype_parameter(parameter,
                explicit_template_arguments,
                pack_index))
    {
        scope_entry_t* template_parameter = nodecl_get_symbol(parameter);
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Expression is a nontype template parameter that can be deduced\n");
        }

        type_t* parameter_type = nodecl_get_type(parameter);
        if (template_parameter->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                && is_pack_type(parameter_type))
            parameter_type = pack_type_get_packed_type(parameter_type);

        argument = nodecl_shallow_copy(argument);

        nodecl_t nodecl_dummy = nodecl_null();
        if (is_template_argument_list
                && !is_dependent_type(parameter_type)
                && !is_dependent_type(nodecl_get_type(argument))
                && !check_nodecl_template_argument_can_be_converted_to_parameter_type(
                    argument,
                    parameter_type,
                    decl_context,
                    &nodecl_dummy))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: But the expression appears in a template-argument list "
                        "and its type '%s' cannot be converted to the corresponding nontype "
                        "template-parameter'%s'\n",
                        print_declarator(nodecl_get_type(argument)),
                        print_declarator(parameter_type));
                fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
            }
            // If P has a form that contains <i> and if the type of the corresponding value of A
            // differs from the type of i, deduction fails
            return DEDUCTION_FAILURE;
        }
        nodecl_free(nodecl_dummy);

        if (is_array_size
                && !is_dependent_type(nodecl_get_type(argument))
                && !is_integral_type(no_ref(nodecl_get_type(argument))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: But the expression appears in an array size "
                        "and its type is not an integral type\n");
                fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
            }
            // If P has a form that contains [i] and if the type of i is not an
            // integral type, deduction fails
            return DEDUCTION_FAILURE;
        }

        deduction_t* new_deduction = NEW0(deduction_t);
        new_deduction->kind =
            (template_parameter->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
            ? TPK_NONTYPE
            : TPK_NONTYPE_PACK;

        new_deduction->parameter_position = symbol_entity_specs_get_template_parameter_position(template_parameter);
        new_deduction->parameter_nesting = symbol_entity_specs_get_template_parameter_nesting(template_parameter);
        new_deduction->parameter_name = template_parameter->symbol_name;

        P_LIST_ADD(deduction_result->deduction_list,
                deduction_result->num_deductions,
                new_deduction);

        if (template_parameter->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)
        {
            ERROR_CONDITION(nodecl_is_list_or_null(argument), "This is not an acceptable value here", 0);

            new_deduction->deduced_parameters = NEW_VEC0(deduced_argument_t*, pack_length);
            new_deduction->num_deduced_parameters = pack_length;

            ERROR_CONDITION(pack_length <= 0, "Invalid pack length", 0);
            ERROR_CONDITION(pack_index >= pack_length, "Invalid pack index", 0);

            deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
            new_deduced_argument->value = argument;
            new_deduced_argument->type = parameter_type;
            new_deduction->deduced_parameters[pack_index] = new_deduced_argument;

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Nontype template parameter pack '%s' at position %d of %d has been deduced the value '%s'\n",
                        codegen_to_str(parameter, decl_context),
                        pack_index,
                        pack_length,
                        codegen_to_str(new_deduced_argument->value, decl_context));
            }
        }
        else
        {
            deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
            new_deduced_argument->type = parameter_type;
            new_deduced_argument->value = argument;

            P_LIST_ADD(new_deduction->deduced_parameters,
                    new_deduction->num_deduced_parameters,
                    new_deduced_argument);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Nontype template parameter '%s' has been deduced the value '%s'\n",
                        codegen_to_str(parameter, decl_context),
                        codegen_to_str(argument, decl_context));
            }
        }

        return DEDUCTION_OK;
    }

    internal_error("Code unreachable", 0);
}


static deduction_result_t deduce_template_arguments_from_a_template_argument_list(
        template_parameter_list_t* orig_parameter_template_argument_list,
        template_parameter_list_t* orig_argument_template_argument_list,
        template_parameter_list_t* explicit_template_argument_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        int pack_length,
        // flags
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_partial_ordering,
        // out
        deduction_set_t* deduction_result)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments using a template argument list\n");
        fprintf(stderr, "TYPEDEDUC: (Parameter) Template argument list: %s\n",
                template_arguments_to_str(orig_parameter_template_argument_list,
                    /* first_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    decl_context));
        fprintf(stderr, "TYPEDEDUC: (Argument) Template argument list: %s\n",
                template_arguments_to_str(orig_argument_template_argument_list,
                    /* first_argument_to_be_printed */ 0,
                    /* print_first_level_bracket */ 1,
                    decl_context));
    }

    // Expand arguments if needed
    template_parameter_list_t* parameter_template_argument_list = flatten_template_arguments(
            orig_parameter_template_argument_list);

    int i;
    for (i = 0; i < parameter_template_argument_list->num_parameters; i++)
    {
        // If the template argument list contains a pack expansion that is
        // not the last template argument, the entire template argument
        // list is a non-deduced context
        if ((((parameter_template_argument_list->arguments[i]->kind == TPK_TYPE
                            || parameter_template_argument_list->arguments[i]->kind == TPK_TEMPLATE)
                        && is_pack_type(parameter_template_argument_list->arguments[i]->type))
                    || (parameter_template_argument_list->arguments[i]->kind == TPK_NONTYPE
                        && nodecl_get_kind(parameter_template_argument_list->arguments[i]->value)
                        == NODECL_CXX_VALUE_PACK))
                && i != (parameter_template_argument_list->num_parameters - 1))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: A pack expansion occurs not in the last template argument, "
                        "the entire (parameter) template argument list is nondeduced\n");
            }
            free_template_parameter_list(parameter_template_argument_list);
            return DEDUCTION_OK;
        }
    }

    template_parameter_list_t* argument_template_argument_list = flatten_template_arguments(
            orig_argument_template_argument_list);

    // Each argument Pi of the respective template argument list P is compared with the corresponding
    // argument Ai of the corresponding template-argument list of A
    for (i = 0;
            (i < parameter_template_argument_list->num_parameters)
            && (i < argument_template_argument_list->num_parameters);
            i++)
    {
        deduction_set_t* deduction_for_template_arg = NEW0(deduction_set_t);

        deduction_result_t result_for_template_arg = DEDUCTION_OK;
        if (parameter_template_argument_list->arguments[i]->kind == TPK_NONTYPE)
        {
            if (nodecl_get_kind(parameter_template_argument_list->arguments[i]->value)
                    == NODECL_CXX_VALUE_PACK)
            {
                // This shall occur as the last argument
                // <int ...N>
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template argument %d is a template nontype argument pack\n", i);
                }
                int j;
                int current_pack_length = argument_template_argument_list->num_parameters - i;
                for (j = i; j < argument_template_argument_list->num_parameters; j++)
                {
                    int current_pack_index = j - i;
                    // if (is_partial_ordering)
                    // {
                    //     if (nodecl_get_kind(argument_template_argument_list->arguments[j]->value) ==
                    //             NODECL_CXX_VALUE_PACK)
                    //     {
                    //         if (j >= parameter_template_argument_list->num_parameters)
                    //         {
                    //             DEBUG_CODE()
                    //             {
                    //                 fprintf(stderr, "TYPEDEDUC: During partial ordering, this (argument) nontype template "
                    //                         "argument is a pack expansion but there is no a (parameter) template "
                    //                         "argument for it, so it is ignored\n");
                    //             }
                    //             // During partial ordering, if Ai was originally a pack expansion
                    //             // - if P does not contain a template argument corresponding to Ai then Ai is ignored
                    //             continue;
                    //         }
                    //     }
                    // }

                    deduction_set_t* deduction_for_pack_expansion = NEW0(deduction_set_t);
                    deduction_result_t deduction_result_pack_expansion =
                        deduce_template_arguments_from_a_value(
                                nodecl_get_child(
                                    parameter_template_argument_list->arguments[i]->value,
                                    0),
                                argument_template_argument_list->arguments[j]->value,
                                explicit_template_argument_list,
                                decl_context,
                                locus,
                                current_pack_index,
                                current_pack_length,
                                is_computing_address_of_function,
                                is_deducing_arguments_from_function_declaration,
                                is_partial_ordering,
                                /* is_array_size */ 0,
                                /* is_template_argument_list */ 1,
                                deduction_for_pack_expansion);

                    if (deduction_result_pack_expansion == DEDUCTION_FAILURE)
                    {
                        deduction_set_free(deduction_for_template_arg);
                        deduction_set_free(deduction_for_pack_expansion);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }

                    deduction_result_pack_expansion = deduction_combine_to_second(
                            deduction_for_pack_expansion,
                            deduction_for_template_arg);
                    deduction_set_free(deduction_for_pack_expansion);

                    if (deduction_result_pack_expansion == DEDUCTION_FAILURE)
                    {
                        deduction_set_free(deduction_for_template_arg);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }
                }

                i = argument_template_argument_list->num_parameters;
            }
            else
            {
                // <int N>
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template argument %d is a nontype template argument\n", i);
                }
                if (is_partial_ordering)
                {
                    if (nodecl_get_kind(argument_template_argument_list->arguments[i]->value) ==
                            NODECL_CXX_VALUE_PACK)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: During deduction for partial ordering, the "
                                    "(argument) nontype template argument is a pack expansion "
                                    "but the (parameter) template argument is not\n");
                            fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                        }
                        // During partial ordering, if Ai was originally a pack expansion
                        // - if Pi is not a pack expansion, template argument fails
                        deduction_set_free(deduction_for_template_arg);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }
                }

                result_for_template_arg =
                    deduce_template_arguments_from_a_value(
                            parameter_template_argument_list->arguments[i]->value,
                            argument_template_argument_list->arguments[i]->value,
                            explicit_template_argument_list,
                            decl_context,
                            locus,
                            pack_index,
                            pack_length,
                            is_computing_address_of_function,
                            is_deducing_arguments_from_function_declaration,
                            is_partial_ordering,
                            /* is_array_size */ 0,
                            /* is_template_argument_list */ 1,
                            deduction_for_template_arg);

                if (result_for_template_arg == DEDUCTION_FAILURE)
                {
                    deduction_set_free(deduction_for_template_arg);
                    free_template_parameter_list(argument_template_argument_list);
                    free_template_parameter_list(parameter_template_argument_list);
                    return DEDUCTION_FAILURE;
                }

            }

            result_for_template_arg = deduction_combine_to_second(
                    deduction_for_template_arg,
                    // out
                    deduction_result);
            deduction_set_free(deduction_for_template_arg);

            if (result_for_template_arg == DEDUCTION_FAILURE)
            {
                free_template_parameter_list(argument_template_argument_list);
                free_template_parameter_list(parameter_template_argument_list);
                return DEDUCTION_FAILURE;
            }
        }
        else if (parameter_template_argument_list->arguments[i]->kind == TPK_TYPE
                || parameter_template_argument_list->arguments[i]->kind == TPK_TEMPLATE)
        {
            if (is_pack_type(parameter_template_argument_list->arguments[i]->type))
            {
                // This shall occur as the last argument
                // <typename ...T>
                // <template < _ > class ...W>
                DEBUG_CODE()
                {
                    if (parameter_template_argument_list->arguments[i]->kind == TPK_TYPE)
                        fprintf(stderr, "TYPEDEDUC: Template argument %d is a template type argument pack\n", i);
                    else
                        fprintf(stderr, "TYPEDEDUC: Template argument %d is a template template argument pack\n", i);
                }

                int j;
                int current_pack_length = argument_template_argument_list->num_parameters - i;
                for (j = i; j < argument_template_argument_list->num_parameters; j++)
                {
                    int current_pack_index = j - i;

                    deduction_set_t* deduction_for_pack_expansion = NEW0(deduction_set_t);
                    deduction_result_t deduction_result_pack_expansion =
                        deduce_template_arguments_from_a_type(
                                pack_type_get_packed_type(
                                    parameter_template_argument_list->arguments[i]->type),
                                argument_template_argument_list->arguments[j]->type,
                                explicit_template_argument_list,
                                decl_context,
                                locus,
                                current_pack_index,
                                current_pack_length,
                                is_computing_address_of_function,
                                is_deducing_arguments_from_function_declaration,
                                is_partial_ordering,
                                /* ignore_cv_qualifiers */ 0,
                                deduction_for_pack_expansion);

                    if (deduction_result_pack_expansion == DEDUCTION_FAILURE)
                    {
                        deduction_set_free(deduction_for_template_arg);
                        deduction_set_free(deduction_for_pack_expansion);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }

                    deduction_result_pack_expansion = deduction_combine_to_second(
                            deduction_for_pack_expansion,
                            deduction_for_template_arg);
                    deduction_set_free(deduction_for_pack_expansion);

                    if (deduction_result_pack_expansion == DEDUCTION_FAILURE)
                    {
                        deduction_set_free(deduction_for_template_arg);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }
                }

                i = argument_template_argument_list->num_parameters;
            }
            else
            {
                // <typename T>
                // <template < _ > class W>
                DEBUG_CODE()
                {
                    if (parameter_template_argument_list->arguments[i]->kind == TPK_TYPE)
                        fprintf(stderr, "TYPEDEDUC: Template argument %d is a template type argument\n", i);
                    else
                        fprintf(stderr, "TYPEDEDUC: Template argument %d is a template template argument\n", i);
                }
                if (is_partial_ordering)
                {
                    // During partial ordering, if Ai was originally a pack expansion
                    // - if Pi is not a pack expansion, template deduction fails
                    if (is_pack_type(argument_template_argument_list->arguments[i]->type))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: During deduction for partial ordering, the "
                                    "(argument) type/template template argument is a pack expansion "
                                    "but the (parameter) template argument is not\n");
                            fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                        }
                        deduction_set_free(deduction_for_template_arg);
                        free_template_parameter_list(argument_template_argument_list);
                        free_template_parameter_list(parameter_template_argument_list);
                        return DEDUCTION_FAILURE;
                    }
                }

                result_for_template_arg =
                    deduce_template_arguments_from_a_type(
                            parameter_template_argument_list->arguments[i]->type,
                            argument_template_argument_list->arguments[i]->type,
                            explicit_template_argument_list,
                            decl_context,
                            locus,
                            pack_index,
                            pack_length,
                            is_computing_address_of_function,
                            is_deducing_arguments_from_function_declaration,
                            is_partial_ordering,
                            /* ignore_cv_qualifiers */ 0,
                            deduction_for_template_arg);

                if (result_for_template_arg == DEDUCTION_FAILURE)
                {
                    deduction_set_free(deduction_for_template_arg);
                    free_template_parameter_list(argument_template_argument_list);
                    free_template_parameter_list(parameter_template_argument_list);
                    return DEDUCTION_FAILURE;
                }
            }

            result_for_template_arg = deduction_combine_to_second(
                    deduction_for_template_arg,
                    // out
                    deduction_result);
            deduction_set_free(deduction_for_template_arg);

            if (result_for_template_arg == DEDUCTION_FAILURE)
            {
                free_template_parameter_list(argument_template_argument_list);
                free_template_parameter_list(parameter_template_argument_list);
                return DEDUCTION_FAILURE;
            }
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    // There are more parameters than arguments
    while (i < argument_template_argument_list->num_parameters)
    {
        if (is_partial_ordering
                && ((argument_template_argument_list->arguments[i]->kind == TPK_NONTYPE
                        && (nodecl_get_kind(argument_template_argument_list->arguments[i]->value)
                            == NODECL_CXX_VALUE_PACK))
                    || ((argument_template_argument_list->arguments[i]->kind == TPK_TYPE
                            || argument_template_argument_list->arguments[i]->kind == TPK_TEMPLATE)
                        && (is_pack_type(argument_template_argument_list->arguments[i]->type)))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: During deduction for partial ordering, ignoring argument because "
                        "it is a pack expansion there is not a corresponding parameter\n");
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: There are more template-arguments than template-parameters. Deduction fails\n");
            }
            free_template_parameter_list(argument_template_argument_list);
            free_template_parameter_list(parameter_template_argument_list);
            return DEDUCTION_FAILURE;
        }

        i++;
    }

    if (i < parameter_template_argument_list->num_parameters)
    {
        // The last parameter is a pack (note that if there are packs in the parameter not in the last
        // position, then everything was already discarded as an undeduced context)
        if ((parameter_template_argument_list->arguments[i]->kind == TPK_NONTYPE
                    && (nodecl_get_kind(parameter_template_argument_list->arguments[i]->value)
                        == NODECL_CXX_VALUE_PACK))
                || ((parameter_template_argument_list->arguments[i]->kind == TPK_TYPE
                        || parameter_template_argument_list->arguments[i]->kind == TPK_TEMPLATE)
                    && (is_pack_type(parameter_template_argument_list->arguments[i]->type))))
        {
            // Deduce empty packs
            scope_entry_t** parameter_packs = NULL;
            int num_parameter_packs = 0;

            if (parameter_template_argument_list->arguments[i]->kind == TPK_NONTYPE)
            {
                expression_enumerate_template_parameter_packs(
                        nodecl_get_child(
                            parameter_template_argument_list->arguments[i]->value,
                            0),
                        /* only_deduced_contexts */ 1,
                        &parameter_packs,
                        &num_parameter_packs);
            }
            else
            {
                type_enumerate_template_parameter_packs(
                        pack_type_get_packed_type(parameter_template_argument_list->arguments[i]->type),
                        /* only_deduced_contexts */ 1,
                        &parameter_packs,
                        &num_parameter_packs);
            }

            deduction_set_t* deduction_empty_packs = NEW0(deduction_set_t);
            deduction_result_t deduction_result_empty_packs =
                deduce_empty_parameter_packs(
                        parameter_packs,
                        num_parameter_packs,
                        deduction_empty_packs);

            if (deduction_result_empty_packs == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_empty_packs);
                free_template_parameter_list(argument_template_argument_list);
                free_template_parameter_list(parameter_template_argument_list);
                return DEDUCTION_FAILURE;
            }

            deduction_result_empty_packs = deduction_combine_to_second(
                    deduction_empty_packs,
                    deduction_result);
            deduction_set_free(deduction_empty_packs);

            if (deduction_result_empty_packs == DEDUCTION_FAILURE)
            {
                free_template_parameter_list(argument_template_argument_list);
                free_template_parameter_list(parameter_template_argument_list);
                return DEDUCTION_FAILURE;
            }

            DELETE(parameter_packs);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: More template-parameters than template-arguments "
                        "and the last is not a pack. Deduction fails\n");
            }
            free_template_parameter_list(argument_template_argument_list);
            free_template_parameter_list(parameter_template_argument_list);
            return DEDUCTION_FAILURE;
        }
    }
    

    free_template_parameter_list(argument_template_argument_list);
    free_template_parameter_list(parameter_template_argument_list);
    return DEDUCTION_OK;
}

static deduction_result_t deduce_template_arguments_from_a_function_parameter_list(
        type_t* function_parameter,
        type_t* function_argument,
        template_parameter_list_t* explicit_template_argument_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        int pack_length,
        // flags
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_partial_ordering,
        // out
        deduction_set_t* deduction_result)
{
    ERROR_CONDITION(!is_function_type(function_parameter), "Invalid type", 0);
    ERROR_CONDITION(!is_function_type(function_argument), "Invalid type", 0);

    int parameter_num_parameters = function_type_get_num_parameters(function_parameter);
    int argument_num_parameters = function_type_get_num_parameters(function_argument);

    if (function_type_get_has_ellipsis(function_parameter)
            != function_type_get_has_ellipsis(function_argument))
        return DEDUCTION_FAILURE;

    if (function_type_get_has_ellipsis(function_parameter))
        parameter_num_parameters--;
    if (function_type_get_has_ellipsis(function_argument))
        argument_num_parameters--;

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments using a function parameter list\n");
        int i;
        for (i = 0; (i < parameter_num_parameters) || (i < argument_num_parameters); i++)
        {
            fprintf(stderr, "TYPEDEDUC: Parameter type: %d ", i);
            if (i < parameter_num_parameters)
            {
                fprintf(stderr, "%s",
                        print_declarator(function_type_get_parameter_type_num(function_parameter, i)));
            }
            else
            {
                fprintf(stderr, "<<no-parameter-type>>");
            }
            fprintf(stderr, " <- ");
            if (i < argument_num_parameters)
            {
                fprintf(stderr, "%s",
                        print_declarator(function_type_get_parameter_type_num(function_argument, i)));
            }
            else
            {
                fprintf(stderr, "<<no-argument-type>>");
            }
            fprintf(stderr, "\n");
        }
    }

    int i;
    for (i = 0; i < parameter_num_parameters
            && i < argument_num_parameters;
            i++)
    {
        type_t* parameter = function_type_get_parameter_type_num(function_parameter, i);
        type_t* argument = function_type_get_parameter_type_num(function_argument, i);


        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Deducing using parameter %d of type '%s'\n", i,
                    print_declarator(parameter));
        }

        if (is_pack_type(parameter))
        {
            int j;
            int current_pack_length = argument_num_parameters - i;
            for (j = i; j < argument_num_parameters; j++)
            {
                int current_pack_index = j - i;
                argument = function_type_get_parameter_type_num(function_argument, j);
                deduction_set_t* deduction_for_current_pack
                    = NEW0(deduction_set_t);
                deduction_result_t deduction_result_for_current_pack = DEDUCTION_OK;

                type_t* unpacked_parameter = pack_type_get_packed_type(parameter);

                if (is_computing_address_of_function
                        || is_deducing_arguments_from_function_declaration)
                {
                    // If P and A are function types that originated from deduction
                    // when taking the address of a function or when deducting template
                    // arguments from a function declaration and Pi and Ai are
                    // parameters of the top level parameter-type-list of P and A,
                    // respectively, Pi is adjusted if it is an rvalue reference to a
                    // cv-unqualified template parameter and Ai is an lvalue reference,
                    // in which case the type of Pi is changed to be the template
                    // parameter type

                    if (is_rvalue_reference_type(unpacked_parameter)
                            && is_named_type(no_ref(unpacked_parameter))
                            && (named_type_get_symbol(no_ref(unpacked_parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER
                                || named_type_get_symbol(no_ref(unpacked_parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
                            && is_unqualified_type(no_ref(unpacked_parameter))
                            && is_lvalue_reference_type(argument))
                    {
                        unpacked_parameter = no_ref(unpacked_parameter);
                    }
                }

                deduction_result_for_current_pack =
                    deduce_template_arguments_from_a_type(
                            unpacked_parameter,
                            argument,
                            explicit_template_argument_list,
                            decl_context,
                            locus,
                            current_pack_index,
                            current_pack_length,
                            is_computing_address_of_function,
                            is_deducing_arguments_from_function_declaration,
                            is_partial_ordering,
                            /* ignore_cv_qualifiers */ 0,
                            deduction_for_current_pack);

                if (deduction_result_for_current_pack == DEDUCTION_FAILURE)
                {
                    deduction_set_free(deduction_for_current_pack);
                    return DEDUCTION_FAILURE;
                }

                deduction_result_for_current_pack = deduction_combine_to_second(
                        deduction_for_current_pack,
                        deduction_result);
                deduction_set_free(deduction_for_current_pack);

                if (deduction_result_for_current_pack == DEDUCTION_FAILURE)
                {
                    return DEDUCTION_FAILURE;
                }
            }

            // We have consumed all the arguments
            i = argument_num_parameters;
        }
        else
        {
            if (is_partial_ordering)
            {
                if (is_pack_type(argument))
                {
                    // During partial ordering if Ai was originally a function parameter pack
                    // - if Pi is not a function parameter pack, template argument deduction fails
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: During partial ordering the argument type is a pack expansion "
                                "but the parameter type is not a pack expansion, deduction fails\n");
                    }
                    return DEDUCTION_FAILURE;
                }
            }

            if (is_computing_address_of_function
                    || is_deducing_arguments_from_function_declaration)
            {
                // If P and A are function types that originated from deduction
                // when taking the address of a function or when deducting template
                // arguments from a function declaration and Pi and Ai are
                // parameters of the top level parameter-type-list of P and A,
                // respectively, Pi is adjusted if it is an rvalue reference to a
                // cv-unqualified template parameter and Ai is an lvalue reference,
                // in which case the type of Pi is changed to be the template
                // parameter type

                if (is_rvalue_reference_type(parameter)
                        && is_named_type(no_ref(parameter))
                        && (named_type_get_symbol(no_ref(parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER
                            || named_type_get_symbol(no_ref(parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
                        && is_unqualified_type(no_ref(parameter))
                        && is_lvalue_reference_type(argument))
                {
                    parameter = no_ref(parameter);
                }
            }

            deduction_set_t* deduction_for_current_parameter
                = NEW0(deduction_set_t);
            deduction_result_t deduction_result_for_current_parameter = DEDUCTION_OK;

            deduction_result_for_current_parameter =
                deduce_template_arguments_from_a_type(
                        parameter,
                        argument,
                        explicit_template_argument_list,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        is_computing_address_of_function,
                        is_deducing_arguments_from_function_declaration,
                        is_partial_ordering,
                        /* ignore_cv_qualifiers */ 0,
                        deduction_for_current_parameter);

            if (deduction_result_for_current_parameter == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_for_current_parameter);
                return DEDUCTION_FAILURE;
            }

            deduction_result_for_current_parameter = deduction_combine_to_second(
                    deduction_for_current_parameter,
                    deduction_result);
            deduction_set_free(deduction_for_current_parameter);

            if (deduction_result_for_current_parameter == DEDUCTION_FAILURE)
            {
                return DEDUCTION_FAILURE;
            }
        }
    }

    while (i < argument_num_parameters)
    {
        // There are more arguments than parameters
        type_t* argument = function_type_get_parameter_type_num(function_argument, i);

        if (is_partial_ordering
                && is_pack_type(argument))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: During partial ordering the argument type is a pack expansion "
                        "but there is no corresponding argument type, ignoring\n");
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: There are more function argument types than function parameter types\n");
            }
            return DEDUCTION_FAILURE;
        }

        i++;
    }

    while (i < parameter_num_parameters)
    {
        type_t* parameter = function_type_get_parameter_type_num(function_parameter, i);

        if (!is_pack_type(parameter))
        {
            // There are more parameters than arguments
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: There are more function parameter types than function argument types\n");
            }
            return DEDUCTION_FAILURE;
        }
        else if ((i + 1) != function_type_get_num_parameters(function_parameter))
        {
            // Not the last pack, ignore
            i++;
            continue;
        }

        // Deduce empty packs
        scope_entry_t** parameter_packs = NULL;
        int num_parameter_packs = 0;

        type_enumerate_template_parameter_packs(
                pack_type_get_packed_type(parameter),
                /* only_deduced_contexts */ 1,
                &parameter_packs,
                &num_parameter_packs);

        deduction_set_t* deduction_empty_packs =
            NEW0(deduction_set_t);
        deduction_result_t deduction_result_empty_packs =
            deduce_empty_parameter_packs(
                    parameter_packs,
                    num_parameter_packs,
                    deduction_empty_packs);

        if (deduction_result_empty_packs == DEDUCTION_FAILURE)
        {
            deduction_set_free(deduction_empty_packs);
            return DEDUCTION_FAILURE;
        }

        deduction_result_empty_packs = deduction_combine_to_second(
                deduction_empty_packs,
                deduction_result);
        deduction_set_free(deduction_empty_packs);

        if (deduction_result_empty_packs == DEDUCTION_FAILURE)
        {
            return DEDUCTION_FAILURE;
        }

        DELETE(parameter_packs);
        i++;
    }

    return DEDUCTION_OK;
}

// 14.8.2.5 [temp.deduct.type]
deduction_result_t deduce_template_arguments_from_a_type(
        type_t* parameter,
        type_t* argument,
        template_parameter_list_t* explicit_template_argument_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        int pack_index, // -1 if not expanding
        int pack_length, // only used if pack_index != -1, note that pack_length may be zero!
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_partial_ordering,
        char ignore_cv_qualifiers,
        // out
        deduction_set_t* deduction_result)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments using a type: %s <- %s\n",
                print_declarator(parameter),
                print_declarator(argument));
    }

    cv_qualifier_t cv_qualif_param = CV_NONE;
    parameter = advance_over_typedefs_with_cv_qualif(parameter, &cv_qualif_param);

    cv_qualifier_t cv_qualif_argument = CV_NONE;
    type_t *orig_argument = argument;
    argument = advance_over_typedefs_with_cv_qualif(argument, &cv_qualif_argument);

    if (is_named_type(parameter)
            && (named_type_get_symbol(parameter)->kind == SK_TEMPLATE_TYPE_PARAMETER
                || named_type_get_symbol(parameter)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Parameter type is a type template parameter\n");
        }

        scope_entry_t* template_parameter = named_type_get_symbol(parameter);

        cv_qualifier_t new_cv_qualifier = cv_qualif_argument & (~cv_qualif_param);
        if (!ignore_cv_qualifiers)
        {
            if (!is_less_or_equal_cv_qualified(cv_qualif_param, cv_qualif_argument))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Discarding type template parameter because is more cv-qualified\n");
                }
                // This is a case like const T <- volatile int
                return DEDUCTION_FAILURE;
            }
        }

        // T
        // cv-list T
        deduction_t* new_deduction = NEW0(deduction_t);
        new_deduction->kind =
            (named_type_get_symbol(parameter)->kind == SK_TEMPLATE_TYPE_PARAMETER)
            ? TPK_TYPE
            : TPK_TYPE_PACK;

        new_deduction->parameter_position = symbol_entity_specs_get_template_parameter_position(template_parameter);
        new_deduction->parameter_nesting = symbol_entity_specs_get_template_parameter_nesting(template_parameter);
        new_deduction->parameter_name = template_parameter->symbol_name;

        P_LIST_ADD(deduction_result->deduction_list,
                deduction_result->num_deductions,
                new_deduction);

        if (template_parameter->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
        {
            ERROR_CONDITION(is_sequence_of_types(argument), "This is not an acceptable type here", 0);

            ERROR_CONDITION(pack_length <= 0, "Invalid pack length", 0);
            ERROR_CONDITION(pack_index >= pack_length, "Invalid pack index", 0);

            new_deduction->deduced_parameters = NEW_VEC0(
                    deduced_argument_t*,
                    pack_length);
            new_deduction->num_deduced_parameters = pack_length;

            deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
            new_deduced_argument->type
                = get_cv_qualified_type(argument, new_cv_qualifier);

            type_t *orig_argument_deduced_cv
                = get_cv_qualified_type(orig_argument, new_cv_qualifier);
            if (equivalent_types(new_deduced_argument->type,
                                 orig_argument_deduced_cv))
            {
                new_deduced_argument->type = orig_argument_deduced_cv;
            }

            new_deduction->deduced_parameters[pack_index] = new_deduced_argument;

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Type template parameter pack '%s' at position %d of %d has been deduced the type '%s'\n",
                        print_declarator(parameter),
                        pack_index,
                        pack_length,
                        print_declarator(new_deduced_argument->type));
            }
        }
        else
        {
            deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
            new_deduced_argument->type
                = get_cv_qualified_type(argument, new_cv_qualifier);

            type_t *orig_argument_deduced_cv
                = get_cv_qualified_type(orig_argument, new_cv_qualifier);
            if (equivalent_types(new_deduced_argument->type,
                                 orig_argument_deduced_cv))
            {
                new_deduced_argument->type = orig_argument_deduced_cv;
            }

            P_LIST_ADD(new_deduction->deduced_parameters,
                    new_deduction->num_deduced_parameters,
                    new_deduced_argument);
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Type template parameter '%s' "
                        "has been deduced the type '%s'\n",
                        print_declarator(parameter),
                        print_declarator(new_deduced_argument->type));
            }
        }

        return DEDUCTION_OK;
    }
    else if (is_named_type(parameter)
            && is_template_type(named_type_get_symbol(parameter)->type_information)
            && template_type_get_related_symbol(named_type_get_symbol(parameter)->type_information) != NULL
            && ((template_type_get_related_symbol(named_type_get_symbol(parameter)->type_information)->kind
                    == SK_TEMPLATE_TEMPLATE_PARAMETER)
                || (template_type_get_related_symbol(named_type_get_symbol(parameter)->type_information)->kind
                    == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)))
    {
        parameter = named_type_get_symbol(parameter)->type_information;
        // These appear here because we handle them as types
        // TT
        scope_entry_t* template_parameter = template_type_get_related_symbol(parameter);
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Parameter type is the template template parameter '%s'\n",
                    template_parameter->symbol_name);
        }
        ERROR_CONDITION(template_type_get_related_symbol(
                    named_type_get_symbol(argument)->type_information) == NULL,
                "Invalid related template symbol", 0);

        deduction_t* new_deduction = NEW0(deduction_t);
        new_deduction->kind =
            (template_parameter->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            ? TPK_TEMPLATE
            : TPK_TEMPLATE_PACK;
        new_deduction->parameter_position = symbol_entity_specs_get_template_parameter_position(template_parameter);
        new_deduction->parameter_nesting = symbol_entity_specs_get_template_parameter_nesting(template_parameter);
        new_deduction->parameter_name = template_parameter->symbol_name;

        P_LIST_ADD(deduction_result->deduction_list,
                deduction_result->num_deductions,
                new_deduction);

        if (template_parameter->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
        {
            ERROR_CONDITION(is_sequence_of_types(argument), "This is not an acceptable type here", 0);

            new_deduction->deduced_parameters = NEW_VEC0(deduced_argument_t*, pack_length);
            new_deduction->num_deduced_parameters = pack_length;

            if (pack_length > 0)
            {
                ERROR_CONDITION(pack_index >= pack_length, "This should not happen", 0);

                deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
                new_deduced_argument->type = argument;
                new_deduction->deduced_parameters[pack_index] = new_deduced_argument;

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template template parameter pack '%s' "
                            "at position %d of %d has been deduced the type '%s'\n",
                            print_declarator(parameter),
                            pack_index,
                            pack_length,
                            print_declarator(new_deduced_argument->type));
                }
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template template parameter pack '%s' "
                            "at has been deduced an empty sequence\n",
                            print_declarator(parameter));
                }
            }
        }
        else
        {
            deduced_argument_t* new_deduced_argument = NEW0(deduced_argument_t);
            new_deduced_argument->type = argument;

            P_LIST_ADD(new_deduction->deduced_parameters,
                    new_deduction->num_deduced_parameters,
                    new_deduced_argument);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Template template parameter '%s' has been deduced the type '%s'\n",
                        template_parameter->symbol_name,
                        template_type_get_related_symbol(
                            named_type_get_symbol(argument)->type_information)
                        ->symbol_name);
            }
        }

        return DEDUCTION_OK;
    }
    else if (is_pointer_type(parameter)
            && is_pointer_type(argument))
    {
        // T*
        return deduce_template_arguments_from_a_type(
                pointer_type_get_pointee_type(parameter),
                pointer_type_get_pointee_type(argument),
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                is_partial_ordering,
                ignore_cv_qualifiers,
                deduction_result);
    }
    else if (is_lvalue_reference_type(parameter)
            && is_lvalue_reference_type(argument))
    {
        // T&
        return deduce_template_arguments_from_a_type(
                reference_type_get_referenced_type(parameter),
                reference_type_get_referenced_type(argument),
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                is_partial_ordering,
                ignore_cv_qualifiers,
                deduction_result);
    }
    else if (is_rvalue_reference_type(parameter)
            && is_rvalue_reference_type(argument))
    {
        // T&&
        return deduce_template_arguments_from_a_type(
                no_ref(parameter),
                no_ref(argument),
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                is_partial_ordering,
                ignore_cv_qualifiers,
                deduction_result);
    }
    else if (is_array_type(parameter)
            && is_array_type(argument)
            && (array_type_is_unknown_size(parameter)
                == array_type_is_unknown_size(argument)))
    {
        // T[integer-constant]
        if (!array_type_is_unknown_size(parameter)
                /* && !array_type_is_unknown_size(argument) */)
        {
            deduction_set_t* deduction_for_array_size = NEW0(deduction_set_t);
            deduction_result_t deduction_result_for_array_size =
                deduce_template_arguments_from_a_value(
                        array_type_get_array_size_expr(parameter),
                        array_type_get_array_size_expr(argument),
                        explicit_template_argument_list,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        is_computing_address_of_function,
                        is_deducing_arguments_from_function_declaration,
                        is_partial_ordering,
                        /* is_array_size */ 1,
                        /* is_template_argument_list */ 0,
                        deduction_for_array_size);
            if (deduction_result_for_array_size == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_for_array_size);
                return DEDUCTION_FAILURE;
            }

            deduction_result_for_array_size =
                deduction_combine_to_second(
                        deduction_for_array_size,
                        deduction_result);
            deduction_set_free(deduction_for_array_size);

            if (deduction_result_for_array_size == DEDUCTION_FAILURE)
            {
                return DEDUCTION_FAILURE;
            }
        }

        // type[i]
        deduction_set_t* deduction_for_element_type = NEW0(deduction_set_t);
        deduction_result_t deduction_result_for_element_type =
            deduce_template_arguments_from_a_type(
                    array_type_get_element_type(parameter),
                    array_type_get_element_type(argument),
                    explicit_template_argument_list,
                    decl_context,
                    locus,
                    pack_index,
                    pack_length,
                    is_computing_address_of_function,
                    is_deducing_arguments_from_function_declaration,
                    is_partial_ordering,
                    ignore_cv_qualifiers,
                    deduction_for_element_type);
        if (deduction_result_for_element_type == DEDUCTION_FAILURE)
        {
            deduction_set_free(deduction_for_element_type);
            return DEDUCTION_FAILURE;
        }

        deduction_result_for_element_type =
            deduction_combine_to_second(
                    deduction_for_element_type,
                    deduction_result);
        deduction_set_free(deduction_for_element_type);

        return deduction_result_for_element_type;
    }
    else if (is_named_class_type(parameter)
            && is_template_specialized_type(named_type_get_symbol(parameter)->type_information))
    {
        if (!is_named_class_type(argument)
                || !is_template_specialized_type(named_type_get_symbol(argument)->type_information))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Deduction fails because the parameter is a "
                        "template-specialized type but the argument is not\n");
            }
            return DEDUCTION_FAILURE;
        }
        type_t* template_parameter = named_type_get_symbol(parameter)->type_information;
        type_t* template_argument = named_type_get_symbol(argument)->type_information;

        type_t* template_name_of_parameter = template_specialized_type_get_related_template_type(template_parameter);
        type_t* template_name_of_argument = template_specialized_type_get_related_template_type(template_argument);

        if (equivalent_types(template_name_of_parameter, template_name_of_argument)
                || (template_type_get_related_symbol(template_name_of_parameter)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                || (template_type_get_related_symbol(template_name_of_parameter)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK))
        {
            if (template_type_get_related_symbol(template_name_of_parameter)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER
                    || template_type_get_related_symbol(template_name_of_parameter)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                // TT<T>
                // TT<i>
                // TT<>
                deduction_set_t* deduction_for_template_template
                    = NEW0(deduction_set_t);

                deduce_template_arguments_from_a_type(
                        get_user_defined_type(
                            template_type_get_related_symbol(template_name_of_parameter)),
                        get_user_defined_type(
                            template_type_get_related_symbol(template_name_of_argument)),
                        explicit_template_argument_list,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        is_computing_address_of_function,
                        is_deducing_arguments_from_function_declaration,
                        is_partial_ordering,
                        ignore_cv_qualifiers,
                        deduction_for_template_template);

                deduction_result_t deduction_result_for_template_template =
                    deduction_combine_to_second(
                            deduction_for_template_template,
                            deduction_result);
                deduction_set_free(deduction_for_template_template);

                if (deduction_result_for_template_template == DEDUCTION_FAILURE)
                    return DEDUCTION_FAILURE;
            }

            // template-name<T>
            // template-name<i>
            template_parameter_list_t* parameter_template_argument_list =
                template_specialized_type_get_template_arguments(template_parameter);
            template_parameter_list_t* argument_template_argument_list =
                template_specialized_type_get_template_arguments(template_argument);

            deduction_set_t* deduction_for_template_argument_list
                = NEW0(deduction_set_t);
            deduction_result_t deduction_result_for_template_argument_list =
                deduce_template_arguments_from_a_template_argument_list(
                        parameter_template_argument_list,
                        argument_template_argument_list,
                        explicit_template_argument_list,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        is_computing_address_of_function,
                        is_deducing_arguments_from_function_declaration,
                        is_partial_ordering,
                        deduction_for_template_argument_list);

            if (deduction_result_for_template_argument_list == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_for_template_argument_list);
                return DEDUCTION_FAILURE;
            }

            deduction_result_for_template_argument_list =
                deduction_combine_to_second(
                        deduction_for_template_argument_list,
                        deduction_result);
            deduction_set_free(deduction_for_template_argument_list);

            return deduction_result_for_template_argument_list;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Deduction fails both specialized template types will never match\n");
            }
            return DEDUCTION_FAILURE;
        }
    }
    else if (is_function_type(parameter)
            && is_function_type(argument))
    {
        // type(T)
        // T()
        // T(T)
        {
            type_t* parameter_return_type = function_type_get_return_type(parameter);
            type_t* argument_return_type = function_type_get_return_type(argument);

            if (parameter_return_type != NULL
                    && argument_return_type != NULL)
            {
                deduction_set_t* deduction_for_return_type =
                    NEW0(deduction_set_t);
                deduction_result_t deduction_result_for_return =
                    deduce_template_arguments_from_a_type(
                            parameter_return_type,
                            argument_return_type,
                            explicit_template_argument_list,
                            decl_context,
                            locus,
                            pack_index,
                            pack_length,
                            is_computing_address_of_function,
                            is_deducing_arguments_from_function_declaration,
                            is_partial_ordering,
                            ignore_cv_qualifiers,
                            deduction_for_return_type);

                if (deduction_result_for_return == DEDUCTION_FAILURE)
                {
                    deduction_set_free(deduction_for_return_type);
                    return DEDUCTION_FAILURE;
                }

                deduction_result_for_return = deduction_combine_to_second(
                        deduction_for_return_type,
                        deduction_result);
                deduction_set_free(deduction_for_return_type);
                if (deduction_result_for_return == DEDUCTION_FAILURE)
                {
                    return DEDUCTION_FAILURE;
                }
            }
        }

        deduction_set_t* deduction_for_parameter_types = NEW0(deduction_set_t);
        deduction_result_t deduction_result_for_parameter_types =
            deduce_template_arguments_from_a_function_parameter_list(
                    parameter, // to be unpacked by the callee
                    argument, // to be unpacked by the callee
                    explicit_template_argument_list,
                    decl_context,
                    locus,
                    pack_index,
                    pack_length,
                    is_computing_address_of_function,
                    is_deducing_arguments_from_function_declaration,
                    is_partial_ordering,
                    deduction_for_parameter_types);

        if (deduction_result_for_parameter_types == DEDUCTION_FAILURE)
        {
            deduction_set_free(deduction_for_parameter_types);
            return DEDUCTION_FAILURE;
        }

        deduction_result_for_parameter_types = deduction_combine_to_second(
                deduction_for_parameter_types,
                deduction_result);
        deduction_set_free(deduction_for_parameter_types);

        return deduction_result_for_parameter_types;
    }
    else if (is_pointer_to_member_type(parameter)
            && is_pointer_to_member_type(argument))
    {
        // T type::*
        // type T::*
        // T T::*

        // T (type::*)()
        // type (T::*)()

        // type (type::*)(T)
        // type (T::*)(T)
        // T (type::*)(T)

        // T (T::*)()
        // T (T::*)(T)
        type_t* parameter_class_type = pointer_to_member_type_get_class_type(parameter);
        type_t* argument_class_type = pointer_to_member_type_get_class_type(argument);

        {
            deduction_set_t* deduction_for_class_type = NEW0(deduction_set_t);

            deduction_result_t deduction_result_for_class_type =
                deduce_template_arguments_from_a_type(
                        parameter_class_type,
                        argument_class_type,
                        explicit_template_argument_list,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        is_computing_address_of_function,
                        is_deducing_arguments_from_function_declaration,
                        is_partial_ordering,
                        ignore_cv_qualifiers,
                        deduction_for_class_type);

            if (deduction_result_for_class_type == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_for_class_type);
                return DEDUCTION_FAILURE;
            }

            deduction_result_for_class_type = deduction_combine_to_second(
                    deduction_for_class_type,
                    deduction_result);
            deduction_set_free(deduction_for_class_type);

            if (deduction_result_for_class_type == DEDUCTION_FAILURE)
            {
                return DEDUCTION_FAILURE;
            }
        }

        type_t* parameter_pointee = pointer_type_get_pointee_type(parameter);
        type_t* argument_pointee = pointer_type_get_pointee_type(argument);

        deduction_set_t* deduction_for_pointee = NEW0(deduction_set_t);

        deduction_result_t deduction_result_for_pointee =
            deduce_template_arguments_from_a_type(
                    parameter_pointee,
                    argument_pointee,
                    explicit_template_argument_list,
                    decl_context,
                    locus,
                    pack_index,
                    pack_length,
                    is_computing_address_of_function,
                    is_deducing_arguments_from_function_declaration,
                    is_partial_ordering,
                    ignore_cv_qualifiers,
                    deduction_for_pointee);

        if (deduction_result_for_pointee == DEDUCTION_FAILURE)
        {
            deduction_set_free(deduction_for_pointee);
            return DEDUCTION_FAILURE;
        }

        deduction_result_for_pointee = deduction_combine_to_second(
                deduction_for_pointee,
                deduction_result);
        deduction_set_free(deduction_for_pointee);

        return deduction_result_for_pointee;
    }
    else if (is_vector_type(parameter)
            && is_vector_type(argument))
    {
        // Note that g++ does not deduce the size either
        return deduce_template_arguments_from_a_type(
                vector_type_get_element_type(parameter),
                vector_type_get_element_type(argument),
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                is_partial_ordering,
                ignore_cv_qualifiers,
                deduction_result);
    }
    else if (is_pack_type(parameter)
            && is_pack_type(argument))
    {
        // This is only for partial order of templates
        return deduce_template_arguments_from_a_type(
                pack_type_get_packed_type(parameter),
                pack_type_get_packed_type(argument),
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                is_computing_address_of_function,
                is_deducing_arguments_from_function_declaration,
                is_partial_ordering,
                ignore_cv_qualifiers,
                deduction_result);
    }
    else if (!type_contains_participating_template_parameters(parameter,
                explicit_template_argument_list,
                pack_index))
    {
        if (is_dependent_type(parameter))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Dependent type does not participate in deduction. "
                        "Nothing is deduced\n");
            }
            return DEDUCTION_OK;
        }
        else if (equivalent_types(parameter, argument))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Nondependent type does not participate in deduction but the argument type is equivalent. "
                        "Nothing is deduced\n");
            }
            return DEDUCTION_OK;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deduction using types fails: %s <- %s\n",
                print_declarator(parameter),
                print_declarator(argument));
    }
    return DEDUCTION_FAILURE;
}


// 14.8.2.1 [temp.deduct.call]
static deduction_result_t deduce_template_arguments_function_call_single_argument(
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* explicit_template_argument_list,
        type_t* orig_parameter,
        type_t* orig_argument,
        const decl_context_t* decl_context,
        const locus_t* locus,
        int pack_index,
        int pack_length,
        // out
        deduction_set_t* deduction_result)
{
    type_t* parameter = orig_parameter;
    // At the eyes of the standard no expression has lvalue reference of type T
    // but we do to designate xvalues/lvalues
    type_t* argument = no_ref(orig_argument);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from a single argument type of a function call\n");
        fprintf(stderr, "TYPEDEDUC: parameter-type: %s <- argument-type: %s\n",
                print_declarator(parameter), print_declarator(argument));
    }

    if (is_braced_list_type(argument))
    {
        scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(
                decl_context,
                locus,
                /* mandatory */ 0);
        type_t* plain_parameter = get_unqualified_type(no_ref(parameter));
        if (std_initializer_list_template != NULL
                && is_named_class_type(plain_parameter)
                && is_template_specialized_type(named_type_get_symbol(plain_parameter)->type_information)
                && equivalent_types(
                    std_initializer_list_template->type_information,
                    template_specialized_type_get_related_template_type(
                        named_type_get_symbol(plain_parameter)->type_information)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: The parameter is an initializer-list and the argument is a "
                        "std::initializer_list<T> is a non-deduced context. Moving onto deducing T\n");
            }
            // If removing references and cv-qualifiers from P gives
            // std::initializer_list<P'> for some P' and the argument is an
            // initializer-list, then deduction is performed instead for each
            // element of the initializer list, taking P' as a function
            // template parameter type and the initializer element as its
            // argument.
            int i, N = braced_list_type_get_num_types(argument);
            for (i = 0; i < N; i++)
            {
                deduction_set_t* deduction_for_list_item = NEW0(deduction_set_t);
                deduction_result_t deduction_result_for_list_item =
                    deduce_template_arguments_from_a_type(
                            // FIXME: this will crash when the type does not have the expected shape
                            template_specialized_type_get_template_arguments(
                                named_type_get_symbol(plain_parameter)->type_information
                                )->arguments[0]->type,
                            braced_list_type_get_type_num(argument, i),
                            explicit_template_argument_list,
                            decl_context,
                            locus,
                            pack_index,
                            pack_length,
                            /* is_computing_address_of_function */ 0,
                            /* is_deducing_arguments_from_function_declaration */ 0,
                            /* is_partial_ordering */ 0,
                            /* ignore_cv_qualifiers */ 0,
                            deduction_for_list_item);

                if (deduction_result_for_list_item == DEDUCTION_FAILURE)
                {
                    deduction_set_free(deduction_for_list_item);
                    return DEDUCTION_FAILURE;
                }

                deduction_result_for_list_item = deduction_combine_to_second(
                        deduction_for_list_item,
                        deduction_result);
                deduction_set_free(deduction_for_list_item);

                if (deduction_result_for_list_item == DEDUCTION_FAILURE)
                {
                    return DEDUCTION_FAILURE;
                }
            }
        }

        // Otherwise an initializer list argument causes the parameter to
        // be considered a non-deduced context
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: An initializer-list that is not an argument of a "
                    "std::initializer_list<T> is a non-deduced context, ignoring\n");
        }
        return DEDUCTION_OK;
    }

    if (!is_any_reference_type(parameter))
    {
        if (is_array_type(argument))
        {
            argument = get_pointer_type(array_type_get_element_type(argument));
        }
        else if (is_function_type(argument))
        {
            argument = get_pointer_type(argument);
        }
        else
        {
            argument = get_unqualified_type(argument);
        }
    }

    char ignore_cv_qualifiers_during_deduction =
        is_any_reference_type(orig_parameter)
        || is_pointer_type(parameter)
        || is_pointer_to_member_type(parameter);

    parameter = get_unqualified_type(parameter);
    /*
     * If P is an rvalue reference to a cv-unqualifie template parameter and the argument
     * is an lvalue, the type lvalue reference to A is used in place of A for type deduction
     */
    if (is_rvalue_reference_type(parameter)
            && is_named_type(no_ref(parameter))
            && (named_type_get_symbol(no_ref(parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER
                || named_type_get_symbol(no_ref(parameter))->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)
            && is_unqualified_type(no_ref(parameter))
            && is_lvalue_reference_type(orig_argument))
    {
        argument = orig_argument;
    }

    if (is_any_reference_type(parameter))
    {
        parameter = no_ref(parameter);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: During deduction of template arguments from a single argument type, "
                "types have been transformed for deduction into:\n");
        fprintf(stderr, "TYPEDEDUC: parameter-type: %s <- argument-type: %s\n",
                print_declarator(parameter),
                print_declarator(argument));
    }

    // When P is a function type, pointer to function type or pointer to member
    // function type
    if ((is_function_type(parameter)
                || is_pointer_to_function_type(parameter)
                || (is_pointer_to_member_type(parameter)
                    && is_function_type(pointer_type_get_pointee_type(parameter))))
            && is_unresolved_overloaded_type(argument))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: The parameter type is a "
                    "function, pointer-to function or pointer-to-member function "
                    "and the argument is an unresolved overloaded type\n");
        }

        scope_entry_list_t* overload_set = unresolved_overloaded_type_get_overload_set(argument);
        scope_entry_list_t* aux_overload_set = NULL;
        if (unresolved_overloaded_type_get_explicit_template_arguments(argument) != NULL)
        {
            // Since deduction is not done here, we have to try to simplify it
            scope_entry_t* simplified = unresolved_overloaded_type_simplify(
                    argument,
                    decl_context,
                    locus);
            if (simplified != NULL)
            {
                aux_overload_set = entry_list_new(simplified);
                overload_set = aux_overload_set;
            }
        }

        // If the argument is an overloaded set containing one or more function templates, the parameter
        // is treated as a non-deduced context
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(overload_set);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current = entry_list_iterator_current(it);

            if (current->kind == SK_TEMPLATE)
            {
                entry_list_iterator_free(it);
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: The unresolved overloaded type is a non-deduced "
                            "context because it contains a template function\n");
                }
                entry_list_free(aux_overload_set);
                return DEDUCTION_OK;
            }
        }
        entry_list_iterator_free(it);

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Performing trial deduction with every item in the unresolved overloaded type\n");
        }

        // If the argument is an overload set (not containing function templates), trial argument
        // deduction is attempted using each of the members of the set. If deduction succeeds for only one
        // of the overload set members, that member is used as the argument value for the deduction.
        // If deduction succeeds for more than one member of the overload set, the parameter is treated
        // as a non deduced context
        scope_entry_t* chosen_function = NULL;
        deduction_set_t* deduction_overload_function = NEW0(deduction_set_t);
        for (it = entry_list_iterator_begin(overload_set);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_function = entry_advance_aliases(
                entry_list_iterator_current(it)
                );
            type_t* function_type = current_function->type_information;

            if (symbol_entity_specs_get_is_member(current_function)
                    && !symbol_entity_specs_get_is_static(current_function))
            {
                function_type =
                    get_pointer_to_member_type(
                            current_function->type_information,
                            symbol_entity_specs_get_class_type(current_function));
            }
            else
            {
                function_type = get_lvalue_reference_type(function_type);
            }

            deduction_set_t* deduction_trial = NEW0(deduction_set_t);
            deduction_result_t deduction_result_trial =
                deduce_template_arguments_function_call_single_argument(
                        template_parameters,
                        explicit_template_argument_list,
                        orig_parameter,
                        function_type,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        deduction_trial);
            if (deduction_result_trial == DEDUCTION_OK)
            {
                if (chosen_function == NULL)
                {
                    chosen_function = current_function;
                    *deduction_overload_function = *deduction_trial;
                }
                else
                {
                    deduction_set_free(deduction_trial);
                    deduction_set_free(deduction_overload_function);

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Trial deduction finds more than one "
                                "match rendering the argument a non-deduced context\n");
                    }

                    // Non-deduced context, more than one matches
                    entry_list_free(aux_overload_set);
                    return DEDUCTION_OK;
                }
            }
            else
            {
                deduction_set_free(deduction_trial);
            }
        }
        entry_list_iterator_free(it);

        if (chosen_function != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Trial deduction succeeds and uses function '%s' for deduction\n",
                        print_decl_type_str(
                            chosen_function->type_information,
                            decl_context,
                            get_qualified_symbol_name(
                                chosen_function,
                                decl_context)));
            }
            *deduction_result = *deduction_overload_function;
            entry_list_free(aux_overload_set);
            return DEDUCTION_OK;
        }
        else
        {
            entry_list_free(aux_overload_set);
            deduction_set_free(deduction_overload_function);
        }
    }
    else
    {
        if (is_unresolved_overloaded_type(argument))
        {
            // We do not perform deduction, so attempt to simplify the type if possible
            // Since deduction is not done here, we have to try to simplify it
            scope_entry_t* simplified = unresolved_overloaded_type_simplify(
                    argument,
                    decl_context,
                    locus);
            if (simplified != NULL)
            {
                argument = simplified->type_information;

                if (symbol_entity_specs_get_is_member(simplified)
                        && !symbol_entity_specs_get_is_static(simplified))
                {
                    argument = get_pointer_to_member_type(
                            simplified->type_information,
                            symbol_entity_specs_get_class_type(simplified));
                }
                else
                {
                    argument = get_lvalue_reference_type(argument);
                }

                return deduce_template_arguments_function_call_single_argument(
                        template_parameters,
                        explicit_template_argument_list,
                        orig_parameter,
                        argument,
                        decl_context,
                        locus,
                        pack_index,
                        pack_length,
                        deduction_result);
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: An unresolved overload in a non-deduced context cannot be simplified\n");
                    fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                }
                return DEDUCTION_FAILURE;
            }
        }

        deduction_result_t deduction_result_value = deduce_template_arguments_from_a_type(
                parameter,
                argument,
                explicit_template_argument_list,
                decl_context,
                locus,
                pack_index,
                pack_length,
                /* is_computing_address_of_function */ 0,
                /* is_deducing_arguments_from_function_declaration */ 0,
                /* is_partial_ordering */ 0,
                ignore_cv_qualifiers_during_deduction,
                deduction_result);

        if (deduction_result_value != DEDUCTION_FAILURE)
        {
            template_parameter_list_t* updated_template_parameters =
                build_template_parameter_list_from_deduction_set(
                    explicit_template_argument_list,
                    // template_parameters,
                    deduction_result);

            decl_context_t* updated_context = decl_context_clone(decl_context);
            updated_context->template_parameters = updated_template_parameters;

            diagnostic_context_push_buffered();
            type_t* deduced_argument = update_type_with_pack_index(
                    parameter,
                    updated_context,
                    locus,
                    pack_index);
            diagnostic_context_pop_and_discard();

            free_template_parameter_list(updated_template_parameters);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Deduced argument type is '%s'\n", print_declarator(deduced_argument));
                fprintf(stderr, "TYPEDEDUC: (Transformed) Argument type is '%s'\n", print_declarator(argument));
            }

            if (deduced_argument == NULL)
            {
                // Substitution fails
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Substitution of the deduced template arguments into the parameter type has failed\n");
                    fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                }
                deduction_result_value = DEDUCTION_FAILURE;
            }

            if (deduction_result_value != DEDUCTION_FAILURE)
            {

                // In general the deduction process attempts to find template argument
                // values that will make the deduced argument identical to A (after the
                // type A is transformed as described above)
                if (equivalent_types(deduced_argument, argument))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Successful deduction of template arguments from "
                                "a single argument type of a function call "
                                "because there is an exact match\n");
                    }
                    return DEDUCTION_OK;
                }
                // if the original P is a reference type, the deduced A can be more
                // cv-qualified than the transformed A
                else if (is_any_reference_type(orig_parameter)
                        && is_more_cv_qualified_type(deduced_argument, argument))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Successful deduction of template arguments from "
                                "a single argument type of a function call "
                                "because deduced argument '%s' is more cv-qualified than '%s' and "
                                "the original parameter '%s' is a reference type\n",
                                print_declarator(deduced_argument),
                                print_declarator(argument),
                                print_declarator(orig_parameter));
                    }
                    return DEDUCTION_OK;
                }
                // the transformed A can be another pointer or pointer to member that can be
                // converted to the deduced A via a qualification conversion
                else if (((is_pointer_type(argument)
                                && is_pointer_type(deduced_argument))
                            || (is_pointer_to_member_type(argument)
                                && is_pointer_to_member_type(deduced_argument)))
                        // FIXME - Is this a qualification conversion?
                        && pointer_types_can_be_converted(argument, deduced_argument))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Successful deduction of template arguments from "
                                "a single argument type of a function call "
                                "because argument '%s' is a pointer/pointer-to-member that can be "
                                "converted to the deduced argument '%s' via a qualification conversion\n",
                                print_declarator(argument),
                                print_declarator(deduced_argument));
                    }
                    return DEDUCTION_OK;
                }
                // If P is a class and P has the form simple-template-id then the
                // transformed A can be a derived class of the deduce A.
                else if (is_named_class_type(parameter)
                        && is_template_specialized_type(named_type_get_symbol(parameter)->type_information)
                        && is_named_class_type(argument)
                        && is_named_class_type(deduced_argument)
                        && class_type_is_derived_instantiating(argument, deduced_argument, locus))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Successful deduction of template arguments from "
                                "a single argument type of a function call "
                                "because the parameter '%s' is a specialized type "
                                "and the deduced argument '%s' is (non-ambiguosly) derived "
                                "from the argument '%s'\n",
                                print_declarator(parameter),
                                print_declarator(deduced_argument),
                                print_declarator(argument));
                    }
                    return DEDUCTION_OK;
                }
                // Likewise, if P is a pointer to a class of the form simple-template-id,
                // the transformed A can be a pointer to a derived class pointed to by the
                // deduced A
                else if (is_pointer_to_class_type(parameter)
                        && is_template_specialized_type(
                            named_type_get_symbol(pointer_type_get_pointee_type(parameter))->type_information)
                        && is_pointer_type(argument)
                        && is_pointer_type(deduced_argument)
                        && is_named_class_type(pointer_type_get_pointee_type(argument))
                        && is_named_class_type(pointer_type_get_pointee_type(deduced_argument))
                        && class_type_is_derived_instantiating(
                            pointer_type_get_pointee_type(argument),
                            pointer_type_get_pointee_type(deduced_argument),
                            locus))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Successful deduction of template arguments from "
                                "a single argument type of a function call "
                                "because the parameter '%s' is a pointer to a specialized type "
                                "and the pointee class of the deduced argument '%s' is a (non-ambiguosly) derived "
                                "from the pointee class of the argument '%s'\n",
                                print_declarator(parameter),
                                print_declarator(deduced_argument),
                                print_declarator(argument));
                    }
                    return DEDUCTION_OK;
                }

                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deduction fails because deduced argument type '%s' "
                            "cannot be matched to the argument type '%s'\n",
                            print_declarator(deduced_argument),
                            print_declarator(argument));
                }
                deduction_result_value = DEDUCTION_FAILURE;
            }
        }
        
        if (deduction_result_value == DEDUCTION_FAILURE)
        {
            if ((is_named_class_type(parameter)
                        && is_template_specialized_type(named_type_get_symbol(parameter)->type_information)
                        && is_named_class_type(argument))
                    || (is_pointer_type(parameter)
                        && is_pointer_type(argument)
                        && is_named_class_type(
                            pointer_type_get_pointee_type(parameter))
                        && is_template_specialized_type(
                            named_type_get_symbol(
                                pointer_type_get_pointee_type(parameter)
                                )->type_information)
                        && is_named_class_type(
                            pointer_type_get_pointee_type(argument))))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deduction has failed but we will attempt deduction "
                            "with bases because the parameter is a specialized template-id\n");
                }
                // Try bases because of the following case. See 14.8.2.1p4
                //
                // template <typename T>
                // struct A { };
                //
                // template <typename T>
                // void f(A<T>);
                //
                // struct C : A<int> { };
                //
                // C c;
                // f(c); // deduces T <- int

                deduction_set_clear(deduction_result);

                type_t* class_type_used = argument;
                if (is_pointer_type(argument))
                    class_type_used = pointer_type_get_pointee_type(class_type_used);

                int num_bases = class_type_get_num_bases(class_type_used);
                int i;
                for (i = 0; i < num_bases; i++)
                {
                    char is_virtual, is_dependent, is_expansion;
                    access_specifier_t access_spec;
                    scope_entry_t *base = class_type_get_base_num(
                            class_type_used,
                            i,
                            &is_virtual,
                            &is_dependent,
                            &is_expansion,
                            &access_spec);

                    if (is_dependent) // Should not happen
                        continue;

                    type_t* base_argument = get_user_defined_type(base);
                    if (is_pointer_type(argument))
                        base_argument = get_pointer_type(base_argument);

                    deduction_set_t* deduction_set_base = NEW0(deduction_set_t);
                    deduction_result_t deduction_result_base =
                        deduce_template_arguments_function_call_single_argument(
                                template_parameters,
                                explicit_template_argument_list,
                                parameter,
                                base_argument,
                                decl_context,
                                locus,
                                pack_index,
                                pack_length,
                                deduction_set_base);

                    if (deduction_result_base == DEDUCTION_OK)
                    {
                        if (deduction_result_value == DEDUCTION_FAILURE)
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEDEDUC: A base allows a successful deduction\n");
                            }
                            deduction_result_base = deduction_combine_to_second(deduction_set_base,
                                    deduction_result);
                            deduction_set_free(deduction_set_base);
                            if (deduction_result_base == DEDUCTION_OK)
                            {
                                deduction_result_value = DEDUCTION_OK;
                            }
                            else
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEDEDUC: But the deduced arguments cnnot be combined\n");
                                }
                                break; // It is not worth continuing
                            }
                        }
                        else if (deduction_result_value == DEDUCTION_OK)
                        {
                            DEBUG_CODE()
                            {
                                fprintf(stderr, "TYPEDEDUC: More than one base can be used for deduction\n");
                            }
                            // If more than one base is OK, deduction fails
                            deduction_set_free(deduction_set_base);
                            deduction_result_value = DEDUCTION_FAILURE;
                            break; // It is not worth continuing
                        }
                        else
                        {
                            internal_error("Code unreachable", 0);
                        }
                    }
                }
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deduction with bases ended with %s\n",
                            deduction_result_value == DEDUCTION_FAILURE ? "failure" : "success");
                }
                return deduction_result_value;
            }
        } 
    }

    // Deduction fails
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Cannot deduce template argument from function call\n");
    }
    return DEDUCTION_FAILURE;
}

// 14.8.2 [temp.deduct]
deduction_result_t handle_explicit_template_arguments(
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* raw_explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t** explicit_template_arguments)
{
    // Copy template parameters but clear the arguments
    *explicit_template_arguments = NEW0(template_parameter_list_t);
    **explicit_template_arguments = *template_parameters;
    (*explicit_template_arguments)->arguments = NEW_VEC0(template_parameter_value_t*,
            ((*explicit_template_arguments)->num_parameters));

    if (raw_explicit_template_arguments == NULL)
        return DEDUCTION_OK;

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Processing explicit template arguments\n");
    }

    decl_context_t* context_for_updating = decl_context_clone(decl_context);
    context_for_updating->template_parameters = *explicit_template_arguments;

    int current_arg = 0, current_param = 0;
    while (current_arg < raw_explicit_template_arguments->num_parameters)
    {
        if (current_arg >= template_parameters->num_parameters)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: There are too many explicit template arguments\n");
                fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
            }
            // Too many explicit template arguments
            free_template_parameter_list((*explicit_template_arguments));
            return DEDUCTION_FAILURE;
        }
        // Note, in contrast to what happens with functions and template
        // specializations during deduction, where only a final pack is
        // relevant, explicit template arguments are "engulfed" by the first pack
        // found in the template-parameter list
        if (template_parameter_kind_is_pack(template_parameters->parameters[current_param]->kind))
        {
            template_parameter_value_t* new_template_argument = NEW0(template_parameter_value_t);
            new_template_argument->kind = template_parameter_kind_get_base_kind(
                    template_parameters->parameters[current_param]->kind);
            new_template_argument->type = get_sequence_of_types(0, NULL);

            (*explicit_template_arguments)->arguments[current_param] = new_template_argument;
            int pack_index = 0;
            while (current_arg < raw_explicit_template_arguments->num_parameters)
            {
                if (template_parameter_kind_get_base_kind(template_parameters->parameters[current_param]->kind)
                        != raw_explicit_template_arguments->arguments[current_arg]->kind)
                {
                    // Mismatch in kind of template-parameter
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Mismatch in the kind of template-parameter "
                                "and kind of template-argument inside a pack\n");
                        fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                    }
                    free_template_parameter_list((*explicit_template_arguments));
                    return DEDUCTION_FAILURE;
                }

                if (raw_explicit_template_arguments->arguments[current_arg]->kind == TPK_NONTYPE)
                {
                    diagnostic_context_push_buffered();
                    type_t* template_argument_type =
                        update_type(
                                template_parameters->parameters[current_param]->entry->type_information,
                                context_for_updating,
                                locus);
                    diagnostic_context_pop_and_discard();

                    if (template_argument_type == NULL)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: Substution of the type of a nontype template parameter has failed\n");
                            fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                        }
                        // Failure in substitution
                        free_template_parameter_list((*explicit_template_arguments));
                        return DEDUCTION_FAILURE;
                    }

                    if (!check_nontype_template_argument_type(template_argument_type))
                    {
                        // Failure in the deduced type
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: The substituted type of the nontype template parameter, "
                                    "'%s' is not acceptable\n",
                                    print_declarator(template_argument_type));
                            fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                        }
                        free_template_parameter_list((*explicit_template_arguments));
                        return DEDUCTION_FAILURE;
                    }

                    new_template_argument->type = 
                        get_sequence_of_types_append_type(
                                new_template_argument->type,
                                template_argument_type);

                    new_template_argument->value = 
                        nodecl_append_to_list(
                                new_template_argument->value,
                                nodecl_shallow_copy(
                                raw_explicit_template_arguments->arguments[current_arg]->value));
                }
                else
                {
                    new_template_argument->type = 
                        get_sequence_of_types_append_type(
                                new_template_argument->type,
                                raw_explicit_template_arguments->arguments[current_arg]->type);
                }

                current_arg++;
                pack_index++;
            }

            (*explicit_template_arguments)->arguments[current_param] = new_template_argument;
            current_param++;
        }
        else
        {
            if (template_parameter_kind_get_base_kind(template_parameters->parameters[current_param]->kind)
                    != template_parameter_kind_get_base_kind(raw_explicit_template_arguments->arguments[current_arg]->kind))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Mismatch in the kind of template-parameter and kind of template-argument\n");
                    fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                }
                // Mismatch in kind of template-parameter
                free_template_parameter_list((*explicit_template_arguments));
                return DEDUCTION_FAILURE;
            }

            template_parameter_value_t* new_template_argument = NEW0(template_parameter_value_t);
            new_template_argument->kind = template_parameters->parameters[current_param]->kind;
            new_template_argument->type = raw_explicit_template_arguments->arguments[current_arg]->type;
            if (new_template_argument->kind == TPK_NONTYPE)
            {
                new_template_argument->type = template_parameters->parameters[current_param]->entry->type_information;
                diagnostic_context_push_buffered();
                new_template_argument->type = update_type(
                        new_template_argument->type,
                        context_for_updating,
                        locus);
                diagnostic_context_pop_and_discard();
                if (new_template_argument->type == NULL)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Substution of the type of a nontype template parameter has failed\n");
                        fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                    }
                    // Failure in substitution
                    free_template_parameter_list((*explicit_template_arguments));
                    return DEDUCTION_FAILURE;
                }

                if (!check_nontype_template_argument_type(new_template_argument->type))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: The substituted type of the nontype template parameter is not acceptable\n");
                        fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                    }
                    // Failure in the deduced type
                    free_template_parameter_list((*explicit_template_arguments));
                    return DEDUCTION_FAILURE;
                }

                new_template_argument->value = nodecl_shallow_copy(
                        raw_explicit_template_arguments->arguments[current_arg]->value);
            }

            (*explicit_template_arguments)->arguments[current_param] = new_template_argument;

            current_arg++;
            current_param++;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Explicit template arguments successfully processed\n");
    }
    return DEDUCTION_OK;
}

// 14.8.2 [temp.deduct]
deduction_result_t finish_deduced_template_arguments(
        template_parameter_list_t* type_template_parameters,
        deduction_set_t* deduction_result,
        const decl_context_t* decl_context,
        const locus_t* locus,
        /* inout */ template_parameter_list_t* deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Finishing template argument deduction\n");
        fprintf(stderr, "TYPEDEDUC: Current deduction set:\n");
        print_deduction_set(deduction_result);
        fprintf(stderr, "TYPEDEDUC: No more deductions\n");
    }
    int i;
    for (i = 0; i < deduced_template_arguments->num_parameters; i++)
    {
        template_parameter_value_t* explicit_argument = NULL;
        if (deduced_template_arguments->arguments[i] != NULL)
        {
            if  (deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_TYPE_PARAMETER_PACK
                    && deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                    && deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template parameter '%s' already has an explicit specified value\n",
                            deduced_template_arguments->parameters[i]->entry->symbol_name);
                }
                continue;
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Template parameter pack '%s' has already some values that will be prepended\n",
                            deduced_template_arguments->parameters[i]->entry->symbol_name);
                }
                // Keep the explicit argument, this is used by packs
                explicit_argument = deduced_template_arguments->arguments[i];
            }
        }

        // The parameter does not have an argument. Try to see if it has been deduced
        deduction_t* deduction = NULL;
        int j;
        for (j = 0; j < deduction_result->num_deductions; j++)
        {
            if ((deduction_result->deduction_list[j]->parameter_nesting
                        == symbol_entity_specs_get_template_parameter_nesting(deduced_template_arguments->parameters[i]->entry))
                    && (deduction_result->deduction_list[j]->parameter_position
                        == symbol_entity_specs_get_template_parameter_position(deduced_template_arguments->parameters[i]->entry)))
            {
                deduction = deduction_result->deduction_list[j];
                break;
            }
        }

        if (deduction != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Template parameter '%s' does not have a value but one has been deduced for it\n",
                        deduced_template_arguments->parameters[i]->entry->symbol_name);
            }
            // We have deduction for it, use that one
            template_parameter_value_t* value = NEW0(template_parameter_value_t);
            value->kind = template_parameter_kind_get_base_kind(deduction->kind);
            switch (deduction->kind)
            {
                case TPK_TYPE:
                case TPK_TEMPLATE:
                    value->type = deduction->deduced_parameters[0]->type;
                    break;
                case TPK_TYPE_PACK:
                case TPK_TEMPLATE_PACK:
                    {
                        // Note: packs may be deduced zero values!
                        int num_deduced = deduction->num_deduced_parameters;

                        char there_are_explicit_args =
                            (explicit_argument != NULL
                             && is_sequence_of_types(explicit_argument->type));

                        if (there_are_explicit_args)
                        {
                            int N = sequence_of_types_get_num_types(explicit_argument->type);
                            num_deduced = N > num_deduced ? N : num_deduced;
                        }

                        type_t *seq[num_deduced + 1];
                        memset(seq, 0, sizeof(seq));

                        if (there_are_explicit_args)
                        {
                            int N = sequence_of_types_get_num_types(explicit_argument->type);
                            for (j = 0; j < N; j++)
                            {
                                seq[j] = sequence_of_types_get_type_num(explicit_argument->type, j);
                            }
                        }

                        for (j = 0; j < deduction->num_deduced_parameters; j++)
                        {
                            if ((seq[j] == NULL)
                                    == (deduction->deduced_parameters[j] == NULL))
                            {
                                if (seq[j] == NULL)
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "TYPEDEDUC: A template pack is missing a value in position %d!\n", j);
                                    }
                                    return DEDUCTION_FAILURE;
                                }
                                else if (!equivalent_types(seq[j], deduction->deduced_parameters[j]->type))
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "TYPEDEDUC: An explicit template pack value in position %d is '%s'"
                                                " but it does not match that of the deduction '%s'\n", j,
                                                print_declarator(seq[j]),
                                                print_declarator(deduction->deduced_parameters[j]->type));
                                    }
                                    return DEDUCTION_FAILURE;
                                }
                            }
                            else if (seq[j] == NULL
                                    && deduction->deduced_parameters[j] != NULL)
                            {
                                seq[j] = deduction->deduced_parameters[j]->type;
                            }
                        }

                        value->type = get_sequence_of_types(
                                num_deduced,
                                seq);
                    }
                    break;
                case TPK_NONTYPE:
                    value->type = deduction->deduced_parameters[0]->type;
                    value->value = nodecl_shallow_copy(deduction->deduced_parameters[0]->value);
                    break;
                case TPK_NONTYPE_PACK:
                    {
                        // Note: packs may be deduced zero values!
                        nodecl_t* explicit_list = NULL;
                        int num_explicit_list = 0;
                        int num_deduced = deduction->num_deduced_parameters;

                        if (explicit_argument != NULL)
                        {
                            ERROR_CONDITION(nodecl_is_list_or_null(explicit_argument->value)
                                    != is_sequence_of_types(explicit_argument->type),
                                    "Inconsistent explicit argument", 0);
                            if (is_sequence_of_types(explicit_argument->type))
                            {
                                ERROR_CONDITION(nodecl_list_length(explicit_argument->value)
                                        != sequence_of_types_get_num_types(explicit_argument->type),
                                        "Mismatch in sequence of explicit argument", 0);
                                explicit_list = nodecl_unpack_list(
                                        explicit_argument->value,
                                        &num_explicit_list);

                                num_deduced = num_explicit_list > num_deduced ? num_explicit_list : num_deduced;
                            }
                        }

                        type_t *seq[num_deduced + 1];
                        memset(seq, 0, sizeof(seq));

                        nodecl_t list[num_deduced + 1];
                        memset(list, 0, sizeof(list));

                        if (explicit_list != NULL)
                        {
                            for (j = 0; j < num_explicit_list; j++)
                            {
                                seq[j] = sequence_of_types_get_type_num(explicit_argument->type, j);
                                list[j] = nodecl_shallow_copy(explicit_list[j]);
                            }
                        }
                        DELETE(explicit_list);

                        for (j = 0; j < deduction->num_deduced_parameters; j++)
                        {
                            if ((deduction->deduced_parameters[j] == NULL)
                                    == nodecl_is_null(list[j]))
                            {
                                if (nodecl_is_null(list[j]))
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "TYPEDEDUC: A template pack is missing a value in position %d!\n", j);
                                    }
                                    return DEDUCTION_FAILURE;
                                }
                                else if (!same_expression_value(
                                            list[j],
                                            deduction->deduced_parameters[j]->value))
                                {
                                    DEBUG_CODE()
                                    {
                                        fprintf(stderr, "TYPEDEDUC: An explicit template pack value in position %d is '%s'"
                                                " but it does not match that of the deduction '%s'\n", j,
                                                codegen_to_str(list[j], decl_context),
                                                codegen_to_str(deduction->deduced_parameters[j]->value, decl_context));
                                    }
                                    return DEDUCTION_FAILURE;
                                }
                            }
                            else if (nodecl_is_null(list[j])
                                    && deduction->deduced_parameters[j] != NULL)
                            {
                                seq[j] = deduction->deduced_parameters[j]->type;
                                list[j] = nodecl_shallow_copy(deduction->deduced_parameters[j]->value);
                            }
                        }

                        value->type = get_sequence_of_types(num_deduced, seq);
                        value->value = nodecl_make_list_n(num_deduced, list);
                    }
                    break;
                default:
                    internal_error("Code unreachable", 0);
            }

            deduced_template_arguments->arguments[i] = value;
        }
        else if (explicit_argument != NULL)
        {
            // Only packs can get here
            ERROR_CONDITION(
                    (deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_TYPE_PARAMETER_PACK
                     && deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                     && deduced_template_arguments->parameters[i]->entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER_PACK),
                    "Invalid parameter type", 0);
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Template parameter '%s' does not have a deduced value but happens to "
                        "have an explicit specified value\n",
                        deduced_template_arguments->parameters[i]->entry->symbol_name);
            }
        }
        else if (type_template_parameters->arguments[i] != NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Template parameter '%s' does not have a value but happens to "
                        "be a default template argument for it\n",
                        deduced_template_arguments->parameters[i]->entry->symbol_name);
            }
            // This is not a pack but the template had default arguments
            ERROR_CONDITION(!type_template_parameters->arguments[i]->is_default,
                    "Invalid default argument", 0);

            template_parameter_value_t* default_template_argument
                = type_template_parameters->arguments[i];

            decl_context_t* context_for_updating = decl_context_clone(decl_context);
            context_for_updating->template_parameters = deduced_template_arguments;
            // Update the default argument
            diagnostic_context_push_buffered();
            template_parameter_value_t* value = update_template_parameter_value(default_template_argument,
                    context_for_updating,
                    /* instantiation_symbol_map */ NULL,
                    locus,
                    /* pack_index */ -1);
            diagnostic_context_pop_and_discard();

            if (value == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Substitution of the default template argument for template parameter '%s' failed\n",
                            deduced_template_arguments->parameters[i]->entry->symbol_name);
                    fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
                }
                return DEDUCTION_FAILURE;
            }

            deduced_template_arguments->arguments[i] = value;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Template parameter '%s' does not have a value\n",
                        deduced_template_arguments->parameters[i]->entry->symbol_name);
                fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
            }
            // This template parameter would be left without argument
            return DEDUCTION_FAILURE;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Template argument finished successfully\n");
    }

    return DEDUCTION_OK;
}

deduction_result_t give_explicit_empty_value_to_stray_template_parameter_packs(
        template_parameter_list_t* type_template_parameters,
        type_t* parameter_function_type,
        /* inout */
        deduction_set_t* deduction_result)
{
    scope_entry_t** parameter_packs = NULL;
    int num_parameter_packs = 0;
    type_enumerate_template_parameter_packs(
            parameter_function_type,
            /* only_deduced_contexts */ 0,
            &parameter_packs,
            &num_parameter_packs);

    int i;
    for (i = 0; i < type_template_parameters->num_parameters; i++)
    {
        if (!template_parameter_kind_is_pack(type_template_parameters->parameters[i]->kind))
            continue;

        scope_entry_t* current_pack = type_template_parameters->parameters[i]->entry;

        // Check if there is a deduction for it, if it is, ignore this pack
        char found = 0;
        int j;
        for (j = 0; j < deduction_result->num_deductions && !found; j++)
        {
            found = ((deduction_result->deduction_list[j]->parameter_position
                        == symbol_entity_specs_get_template_parameter_position(current_pack))
                    && (deduction_result->deduction_list[j]->parameter_nesting
                        == symbol_entity_specs_get_template_parameter_nesting(current_pack)));
        }

        if (found)
            continue;

        // Now check if this template appers in the list of gathered template
        // parameter packs.  If it does, ignore it as well
        found = 0;
        for (j = 0; j < num_parameter_packs; j++)
        {
            found = ((symbol_entity_specs_get_template_parameter_position(parameter_packs[j])
                        == symbol_entity_specs_get_template_parameter_position(current_pack))
                    && (symbol_entity_specs_get_template_parameter_nesting(parameter_packs[j])
                        == symbol_entity_specs_get_template_parameter_nesting(current_pack)));
        }

        if (found)
            continue;

        // """deduce""" an empty sequence for this pack that is not mentioned anywhere in the function
        deduction_t* new_deduction = NEW0(deduction_t);
        switch (current_pack->kind)
        {
            case SK_TEMPLATE_TEMPLATE_PARAMETER_PACK:
                new_deduction->kind = TPK_TEMPLATE_PACK;
                break;
            case SK_TEMPLATE_TYPE_PARAMETER_PACK:
                new_deduction->kind = TPK_TYPE_PACK;
                break;
            case SK_TEMPLATE_NONTYPE_PARAMETER_PACK:
                new_deduction->kind = TPK_NONTYPE_PACK;
                break;
            default:
                internal_error("Code unreachable", 0);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Template parameter pack '%s' has been deduced the empty value sequence\n",
                    current_pack->symbol_name);
        }

        new_deduction->parameter_position = symbol_entity_specs_get_template_parameter_position(current_pack);
        new_deduction->parameter_nesting = symbol_entity_specs_get_template_parameter_nesting(current_pack);
        new_deduction->parameter_name = current_pack->symbol_name;
        // new_deduction->num_deduced_parameters = 0;

        P_LIST_ADD(deduction_result->deduction_list,
                deduction_result->num_deductions,
                new_deduction);
    }

    DELETE(parameter_packs);
    return DEDUCTION_OK;
}


// 14.8.2.1 [temp.deduct.call]
static deduction_result_t deduce_template_arguments_from_call_function_aux(
        type_t** call_argument_types,
        int num_arguments,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        deduction_set_t* deduction_result,
        template_parameter_list_t **out_deduced_template_arguments)
{
    *out_deduced_template_arguments = NULL;

    type_t* parameter_function_type =
        named_type_get_symbol(specialized_named_type)->type_information;
    int num_parameters = function_type_get_num_parameters(parameter_function_type);

    char has_ellipsis = function_type_get_has_ellipsis(parameter_function_type);
    if (has_ellipsis)
        num_parameters--;

    template_parameter_list_t* explicit_template_arguments = NULL;
    deduction_result_t explicit_template_arguments_deduction =
        handle_explicit_template_arguments(template_parameters,
                raw_explicit_template_arguments,
                decl_context,
                locus,
                &explicit_template_arguments);
    if (explicit_template_arguments_deduction == DEDUCTION_FAILURE)
        return DEDUCTION_FAILURE;

    int current_arg = 0;
    int current_param = 0;

    while (current_arg < num_arguments)
    {
        type_t* argument = call_argument_types[current_arg];
        // Too many parameters for a successful deduction
        if (current_arg >= num_parameters)
        {
            if (has_ellipsis)
                // We are done
                break;

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: There are too many arguments when deducing "
                        "template arguments from a function call\n");
                fprintf(stderr, "TYPEDEDUC: Deduction fails\n");
            }
            return DEDUCTION_FAILURE;
        }

        type_t* parameter = function_type_get_parameter_type_num(parameter_function_type, current_arg);
        if (is_pack_type(parameter))
        {
            // When a function parameter pack appears in a non-deduced context, the type of that
            // parameter is never deduced. [A function parameter pack is a non-deduced context
            // if it does not occur at the end of the parameter-declaration-list]

            // Note: we do not use num_parameters because we want it to be in the final position,
            // not just before the ellipsis
            if ((current_param + 1) != function_type_get_num_parameters(parameter_function_type))
            {
                // Nondeduced
                current_param++;
            }
            else
            {
                int pack_index = 0;
                int pack_length = num_arguments - current_arg;
                while (current_arg < num_arguments)
                {
                    argument = call_argument_types[current_arg];

                    if (!type_contains_participating_template_parameters(
                                pack_type_get_packed_type(parameter),
                                explicit_template_arguments,
                                pack_index))
                    {
                        // Does not participate in the deduction
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: Type '%s' does not contain any participating template parameter, "
                                    "so it does no participate in the type deduction\n",
                                    print_declarator(parameter));
                        }
                        current_arg++;
                        pack_index++;
                        continue;
                    }


                    deduction_set_t *deduction_single_parameter
                        = NEW0(deduction_set_t);

                    deduction_result_t deduction_result_of_simple_parameter =
                        deduce_template_arguments_function_call_single_argument(
                                template_parameters,
                                explicit_template_arguments,
                                pack_type_get_packed_type(parameter),
                                argument,
                                decl_context,
                                locus,
                                pack_index,
                                pack_length,
                                deduction_single_parameter);

                    if (deduction_result_of_simple_parameter == DEDUCTION_FAILURE)
                    {
                        deduction_set_free(deduction_single_parameter);
                        return DEDUCTION_FAILURE;
                    }

                    deduction_result_of_simple_parameter =
                        deduction_combine_to_second(
                                deduction_single_parameter,
                                deduction_result);
                    deduction_set_free(deduction_single_parameter);
                    if (deduction_result_of_simple_parameter == DEDUCTION_FAILURE)
                    {
                        return DEDUCTION_FAILURE;
                    }

                    current_arg++;
                    pack_index++;
                }
                current_param++;
            }
        }
        else
        {
            // template <typename T>
            // void f(T x, int y);
            //
            //    f('a', 2.3f); // T <- char
            //
            // is OK since 'int' does not have any participating template parameter for 'y'.
            //
            // Maybe we should verify that there is a SCS but overload will check this for us
            // so it does not seem to make sense to early check it here.
            if (!type_contains_participating_template_parameters(
                        parameter,
                        explicit_template_arguments,
                        -1))
            {
                // Does not participate in the deduction
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Type '%s' does not contain any participating template parameter, "
                            "so it does no participate in the type deduction\n",
                            print_declarator(parameter));
                }
                current_arg++;
                current_param++;
                continue;
            }

            deduction_set_t *deduction_single_parameter
                = NEW0(deduction_set_t);

            deduction_result_t deduction_result_of_simple_parameter =
                deduce_template_arguments_function_call_single_argument(
                        template_parameters,
                        explicit_template_arguments,
                        parameter,
                        argument,
                        decl_context,
                        locus,
                        /* pack_index */ -1,
                        /* pack_index */ -1,
                        deduction_single_parameter);

            if (deduction_result_of_simple_parameter == DEDUCTION_FAILURE)
            {
                deduction_set_free(deduction_single_parameter);
                return DEDUCTION_FAILURE;
            }

            deduction_result_of_simple_parameter =
                deduction_combine_to_second(
                    deduction_single_parameter,
                    deduction_result);
            deduction_set_free(deduction_single_parameter);

            if (deduction_result_of_simple_parameter == DEDUCTION_FAILURE)
            {
                return DEDUCTION_FAILURE;
            }

            current_arg++;
            current_param++;
        }
    }

    while (current_arg < num_parameters)
    {
        type_t* parameter = function_type_get_parameter_type_num(parameter_function_type, current_arg);

        if (!is_pack_type(parameter))
        {
            // Not a pack, ignore
            current_arg++;
            continue;
        }
        else if ((current_arg + 1) != function_type_get_num_parameters(parameter_function_type))
        {
            // Not the last pack, ignore
            current_arg++;
            continue;
        }

        // Deduce empty packs
        scope_entry_t** parameter_packs = NULL;
        int num_parameter_packs = 0;

        type_enumerate_template_parameter_packs(
                pack_type_get_packed_type(parameter),
                /* only_deduced_contexts */ 1,
                &parameter_packs,
                &num_parameter_packs);

        deduction_set_t* deduction_empty_packs = NEW0(deduction_set_t);
        deduction_result_t deduction_result_empty_packs =
            deduce_empty_parameter_packs(
                    parameter_packs,
                    num_parameter_packs,
                    deduction_empty_packs);

        if (deduction_result_empty_packs == DEDUCTION_FAILURE)
        {
            deduction_set_free(deduction_empty_packs);
            return DEDUCTION_FAILURE;
        }

        deduction_result_empty_packs = deduction_combine_to_second(
                deduction_empty_packs,
                deduction_result);
        deduction_set_free(deduction_empty_packs);

        if (deduction_result_empty_packs == DEDUCTION_FAILURE)
        {
            return DEDUCTION_FAILURE;
        }

        DELETE(parameter_packs);
        current_arg++;
    }

    // Nobody knows how really should this be done, so we will try to make our
    // best based on the common experience using existing C++ compilers.
    //
    // template <typename ...T>
    // void f();
    //
    // f();
    //
    // This is a fine call since the template parameter pack does not appear in any nondeduced context,
    // so let's make it as if it was explicitly specified as empty. This way finish_deduced_template_arguments
    // will be happy to see that T has some value.
    deduction_result_t deduction_finish_result =
        give_explicit_empty_value_to_stray_template_parameter_packs(
                type_template_parameters,
                parameter_function_type,
                /* inout */
                deduction_result);
    if (deduction_finish_result == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(explicit_template_arguments);
        return DEDUCTION_FAILURE;
    }

    // Finish the deduction
    template_parameter_list_t* deduced_template_arguments = explicit_template_arguments;
    deduction_finish_result =
        finish_deduced_template_arguments(
            type_template_parameters,
            deduction_result,
            named_type_get_symbol(specialized_named_type)->decl_context,
            locus,
            /* inout */ deduced_template_arguments);
    if (deduction_finish_result == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(explicit_template_arguments);
        return DEDUCTION_FAILURE;
    }

    *out_deduced_template_arguments = deduced_template_arguments;
    return DEDUCTION_OK;
}

// 14.8.2.1 [temp.deduct.call]
deduction_result_t deduce_template_arguments_from_function_call(
        type_t** call_argument_types,
        int num_arguments,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from function call\n");
        if (num_arguments == 0)
        {
            fprintf(stderr, "TYPEDEDUC: Zero argument types\n");
        }
        else
        {
            int i;
            for (i = 0; i < num_arguments; i++)
            {
                fprintf(stderr, "TYPEDEDUC: Argument type %d: %s\n", i,
                        print_declarator(call_argument_types[i]));
            }
            fprintf(stderr, "TYPEDEDUC: No more argument types\n");
        }
    }

    deduction_set_t* deduction_set = NEW0(deduction_set_t);

    deduction_result_t deduction_result = deduce_template_arguments_from_call_function_aux(
            call_argument_types,
            num_arguments,
            specialized_named_type,
            template_parameters,
            type_template_parameters,
            raw_explicit_template_arguments,
            decl_context,
            locus,
            // out
            deduction_set,
            out_deduced_template_arguments);

    deduction_set_free(deduction_set);

    return deduction_result;
}

// 14.8.2.2 [temp.deduct.funcaddr]
deduction_result_t deduce_template_arguments_from_address_of_a_function_template(
        type_t* specified_type, /* A */
        type_t* specialized_named_type, /* P */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from address of function template\n");
        fprintf(stderr, "TYPEDEDUC: %s <- %s\n",
                print_declarator(specialized_named_type),
                print_declarator(specified_type));
    }

    *out_deduced_template_arguments = NULL;

    deduction_set_t* deduction_set = NEW0(deduction_set_t);

    template_parameter_list_t* explicit_template_arguments = NULL;
    deduction_result_t explicit_template_arguments_deduction =
        handle_explicit_template_arguments(template_parameters,
                raw_explicit_template_arguments,
                decl_context,
                locus,
                &explicit_template_arguments);
    if (explicit_template_arguments_deduction == DEDUCTION_FAILURE)
    {
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    deduction_result_t deduction_using_type = deduce_template_arguments_from_a_type(
            specialized_named_type,
            specified_type,
            explicit_template_arguments,
            decl_context,
            locus,
            /* pack_index */ -1,
            /* pack_length */ -1,
            /* is_computing_address_of_function */ 1,
            /* is_deducing_arguments_from_function_declaration */ 0,
            /* is_partial_ordering */ 0,
            /* ignore_cv_qualifiers */ 0,
            deduction_set);

    if (deduction_using_type == DEDUCTION_FAILURE)
    {
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    // Finish the deduction
    template_parameter_list_t* deduced_template_arguments = explicit_template_arguments;
    deduction_result_t deduction_finish_result =
        finish_deduced_template_arguments(
            type_template_parameters,
            deduction_set,
            decl_context,
            locus,
            /* inout */ deduced_template_arguments);
    deduction_set_free(deduction_set);
    if (deduction_finish_result == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(deduced_template_arguments);
        return DEDUCTION_FAILURE;
    }

    *out_deduced_template_arguments = deduced_template_arguments;
    return DEDUCTION_OK;
}


// 14.8.2.3 [temp.deduct.conv]
deduction_result_t deduce_template_arguments_for_conversion_function(
        scope_entry_t* conversion_function,
        type_t* required_type, /* A */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments)
{
    *out_deduced_template_arguments = NULL;

    ERROR_CONDITION(!symbol_entity_specs_get_is_conversion(conversion_function),
            "This is not a conversion", 0);

    type_t* original_required_type = required_type;
    type_t* return_type /* P */ =
        function_type_get_return_type(conversion_function->type_information);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from conversion function\n");
        fprintf(stderr, "TYPEDEDUC: Return types: %s <- %s\n",
                print_declarator(return_type),
                print_declarator(required_type));
    }

    if (is_any_reference_type(return_type))
    {
        return_type = no_ref(return_type);
    }

    if (!is_any_reference_type(required_type))
    {
        if (is_array_type(return_type))
        {
            return_type = get_pointer_type(array_type_get_element_type(return_type));
        }
        else if (is_function_type(return_type))
        {
            return_type = get_pointer_type(return_type);
        }
        return_type = get_unqualified_type(return_type);
    }

    deduction_set_t* deduction_set = NEW0(deduction_set_t);

    required_type = get_unqualified_type(required_type);
    required_type = no_ref(required_type);

    template_parameter_list_t* explicit_template_arguments = NULL;
    deduction_result_t explicit_template_arguments_deduction =
        handle_explicit_template_arguments(template_parameters,
                raw_explicit_template_arguments,
                decl_context,
                locus,
                &explicit_template_arguments);
    // This should not fail, though
    if (explicit_template_arguments_deduction == DEDUCTION_FAILURE)
    {
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    char ignore_cv_qualifiers_during_deduction =
            is_pointer_type(required_type)
            || is_pointer_to_member_type(required_type);

    deduction_result_t deduction_using_type = deduce_template_arguments_from_a_type(
            return_type,
            required_type,
            explicit_template_arguments,
            decl_context,
            locus,
            /* pack_index */ -1,
            /* pack_length */ -1,
            /* is_computing_address_of_function */ 0,
            /* is_deducing_arguments_from_function_declaration */ 0,
            /* is_partial_ordering */ 0,
            ignore_cv_qualifiers_during_deduction,
            deduction_set);
    if (deduction_using_type == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(explicit_template_arguments);
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    // Finish the deduction
    template_parameter_list_t* deduced_template_arguments = explicit_template_arguments;
    deduction_result_t deduction_finish_result =
        finish_deduced_template_arguments(
            type_template_parameters,
            deduction_set,
            decl_context,
            locus,
            /* inout */ deduced_template_arguments);
    deduction_set_free(deduction_set);
    if (deduction_finish_result == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(deduced_template_arguments);
        return DEDUCTION_FAILURE;
    }

    // Now update the type
    decl_context_t* updating_context = decl_context_clone(decl_context);
    updating_context->template_parameters = deduced_template_arguments;

    diagnostic_context_push_buffered();
    type_t* deduced_required_type = update_type(return_type,
            updating_context,
            locus);
    diagnostic_context_pop_and_discard();

    if (equivalent_types(deduced_required_type, required_type))
    {
        // Fine
    }
    else if (is_any_reference_type(original_required_type)
            && is_more_or_equal_cv_qualified_type(no_ref(deduced_required_type), required_type))
    {
        // Fine
    }
    else if (is_pointer_type(required_type)
            && is_pointer_type(deduced_required_type)
            && pointer_types_can_be_converted(deduced_required_type, required_type))
    {
        // Fine
    }
    else
    {
        // Deduction fails
        return DEDUCTION_FAILURE;
    }

    *out_deduced_template_arguments = deduced_template_arguments;
    return DEDUCTION_OK;
}

// 14.8.2.6 [temp.deduct.decl]
deduction_result_t deduce_template_arguments_from_function_declaration(
        type_t* potential_match, /* P */
        type_t* function_type_from_declaration, /* A */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments from function declaration\n");
        fprintf(stderr, "TYPEDEDUC: %s <- %s\n",
                print_declarator(potential_match),
                print_declarator(function_type_from_declaration));
    }

    deduction_set_t* deduction_set = NEW0(deduction_set_t);

    template_parameter_list_t* explicit_template_arguments = NULL;
    deduction_result_t explicit_template_arguments_deduction =
        handle_explicit_template_arguments(template_parameters,
                raw_explicit_template_arguments,
                decl_context,
                locus,
                &explicit_template_arguments);
    if (explicit_template_arguments_deduction == DEDUCTION_FAILURE)
    {
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    deduction_result_t deduction_using_type = deduce_template_arguments_from_a_type(
            potential_match,
            function_type_from_declaration,
            explicit_template_arguments,
            decl_context,
            locus,
            /* pack_index */ -1,
            /* pack_length */ -1,
            /* is_computing_address_of_function */ 0,
            /* is_deducing_arguments_from_function_declaration */ 1,
            /* is_partial_ordering */ 0,
            /* ignore_cv_qualifiers */ 0,
            deduction_set);
    if (deduction_using_type == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(explicit_template_arguments);
        deduction_set_free(deduction_set);
        return DEDUCTION_FAILURE;
    }

    // Finish the deduction
    template_parameter_list_t* deduced_template_arguments = explicit_template_arguments;
    deduction_result_t deduction_finish_result =
        finish_deduced_template_arguments(
            type_template_parameters,
            deduction_set,
            decl_context,
            locus,
            /* inout */ deduced_template_arguments);
    deduction_set_free(deduction_set);
    if (deduction_finish_result == DEDUCTION_FAILURE)
    {
        free_template_parameter_list(deduced_template_arguments);
        return DEDUCTION_FAILURE;
    }

    // Now verify the deduction
    decl_context_t* updating_context = decl_context_clone(decl_context);
    updating_context->template_parameters = deduced_template_arguments;

    diagnostic_context_push_buffered();
    type_t* deduced_type = update_type(potential_match,
            updating_context,
            locus);
    diagnostic_context_pop_and_discard();

    if (deduced_type == NULL
            || !equivalent_types(deduced_type, function_type_from_declaration))
    {
        free_template_parameter_list(deduced_template_arguments);
        return DEDUCTION_FAILURE;
    }

    *out_deduced_template_arguments = deduced_template_arguments;

    return DEDUCTION_OK;
}

char deduce_arguments_of_auto_initialization(
        type_t* destination_type,
        type_t* initializer_type,
        const decl_context_t* decl_context,
        template_parameter_list_t** deduced_template_arguments,
        char is_braced_array,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template arguments for auto initialization\n");
        fprintf(stderr, "TYPEDEDUC: %s <- %s\n",
                print_declarator(destination_type),
                print_declarator(initializer_type));
    }
    // Fake type template parameter
    scope_entry_t* fake_template_parameter_symbol = NEW0(scope_entry_t);
    fake_template_parameter_symbol->symbol_name = UNIQUESTR_LITERAL("FakeTypeTemplateParameter");
    fake_template_parameter_symbol->kind = SK_TEMPLATE_TYPE_PARAMETER;
    symbol_entity_specs_set_is_template_parameter(fake_template_parameter_symbol, 1);
    fake_template_parameter_symbol->locus = locus;
    symbol_entity_specs_set_template_parameter_nesting(fake_template_parameter_symbol, 1);
    symbol_entity_specs_set_template_parameter_position(fake_template_parameter_symbol, 0);

    // Fake template parameter list
    template_parameter_list_t *fake_template_parameter_list = NEW0(template_parameter_list_t);
    template_parameter_t* fake_template_parameter = NEW0(template_parameter_t);
    fake_template_parameter->kind = TPK_TYPE;
    fake_template_parameter->entry = fake_template_parameter_symbol;

    P_LIST_ADD(fake_template_parameter_list->parameters,
            fake_template_parameter_list->num_parameters,
            fake_template_parameter);
    int num_args = 0;
    P_LIST_ADD(fake_template_parameter_list->arguments,
            num_args,
            NULL);

    // Create a suitable type for the fake function using the fake type template parameter
    // const auto& -> const FakeTypeTemplateParameter&
    type_t* type_in_place_of_auto = NULL;
    if (!is_braced_array)
    {
        type_in_place_of_auto = get_user_defined_type(fake_template_parameter_symbol);
    }
    else
    {
        scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(
                decl_context,
                locus,
                /* mandatory */ 1);
        if (std_initializer_list_template == NULL)
            return 0;

        template_parameter_list_t* fake_template_argument_list = duplicate_template_argument_list(
                template_type_get_template_parameters(std_initializer_list_template->type_information));
        template_parameter_value_t* fake_template_argument = NEW0(template_parameter_value_t);
        fake_template_argument->kind = TPK_TYPE;
        fake_template_argument->type = get_user_defined_type(fake_template_parameter_symbol);
        fake_template_argument_list->arguments[0] = fake_template_argument;

        // Now get a silly specialization using our fake template parameter
        type_in_place_of_auto = template_type_get_specialized_type(std_initializer_list_template->type_information,
                fake_template_argument_list,
                decl_context,
                locus);
        free_template_parameter_list(fake_template_argument_list);
    }
    ERROR_CONDITION(type_in_place_of_auto == NULL, "Invalid type in place for auto", 0);

    type_t* fake_parameter_type = update_type_for_auto(destination_type, type_in_place_of_auto);

    // Create a function type
    parameter_info_t parameter_types[1];
    memset(&parameter_types, 0, sizeof(parameter_types));

    parameter_types[0].type_info = fake_parameter_type;

    type_t* fake_function_type = get_new_function_type(get_void_type(),
            parameter_types, 1, REF_QUALIFIER_NONE);

    // Fake template type
    type_t* fake_template_type = get_new_template_type(fake_template_parameter_list,
            fake_function_type,
            "FakeTemplate",
            decl_context,
            locus);

    type_t* primary_specialization = template_type_get_primary_type(fake_template_type);

    return deduce_template_arguments_from_function_call(
            &initializer_type, 1,
            primary_specialization,
            fake_template_parameter_list,
            fake_template_parameter_list,
            /* explicit_template_argument_list */ NULL,
            decl_context,
            locus,
            deduced_template_arguments);
}
