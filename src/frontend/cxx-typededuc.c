/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-overload.h"
#include "cxx-cexpr.h"
#include "cxx-instantiation.h"
#include "cxx-entrylist.h"
#include "cxx-codegen.h"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"

unsigned long long int _bytes_typededuc = 0;

unsigned long long int typededuc_used_memory(void)
{
    return _bytes_typededuc;
}

static template_parameter_list_t* build_template_parameter_list_from_deduction_set(
        template_parameter_list_t* template_parameters,
        deduction_set_t* deduction_set);


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
                internal_error("Invalid template parameter kind", 0);
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

static void deduction_free(deduction_t* deduction)
{
    int i;
    for (i = 0; i < deduction->num_deduced_parameters; i++)
    {
        xfree(deduction->deduced_parameters[i]);
    }

    xfree(deduction->deduced_parameters);
    deduction->deduced_parameters = 0;

    deduction->num_deduced_parameters = 0;

    xfree(deduction);
}

void deduction_set_free(deduction_set_t* deduction_set)
{
    if (deduction_set == NULL)
        return;

    int i;
    for (i = 0; i < deduction_set->num_deductions; i++)
    {
        deduction_free(deduction_set->deduction_list[i]);
    }

    xfree(deduction_set->deduction_list);
    deduction_set->deduction_list = 0;

    deduction_set->num_deductions = 0;

    xfree(deduction_set);
}

static deduced_parameter_t* deduced_parameter_copy(deduced_parameter_t* deduced_parameter)
{
    deduced_parameter_t* result = xcalloc(1, sizeof(*result));
    *result = *deduced_parameter;
    result->value = nodecl_shallow_copy(deduced_parameter->value);

    return result;
}

static deduction_t* deduction_copy(deduction_t* deduction)
{
    deduction_t* result = xcalloc(1, sizeof(*result));

    result->kind = deduction->kind;
    result->parameter_position = deduction->parameter_position;
    result->parameter_nesting = deduction->parameter_nesting;
    result->parameter_name = deduction->parameter_name;
    result->num_deduced_parameters = deduction->num_deduced_parameters;

    result->deduced_parameters = xcalloc(result->num_deduced_parameters, sizeof(*result->deduced_parameters));

    int i;
    for (i = 0; i < result->num_deduced_parameters; i++)
    {
        result->deduced_parameters[i] = deduced_parameter_copy(deduction->deduced_parameters[i]);
    }

    return result;
}

static deduction_set_t* deduction_set_copy(deduction_set_t* deduction_set)
{
    deduction_set_t* result = xcalloc(1, sizeof(*result));

    result->num_deductions = deduction_set->num_deductions;
    result->deduction_list = xcalloc(result->num_deductions, sizeof(*result->deduction_list));
    int i;
    for (i = 0; i < result->num_deductions; i++)
    {
        result->deduction_list[i] = deduction_copy(deduction_set->deduction_list[i]);
    }

    return result;
}

deduction_t* deduction_set_get_unification_item_for_template_parameter(deduction_set_t* deduction_set, scope_entry_t* s1)
{
    ERROR_CONDITION(!s1->entity_specs.is_template_parameter,
            "This must be a template-parameter", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Getting unification item for template parameter '%s'\n",
                s1->symbol_name);
    }

    int position = s1->entity_specs.template_parameter_position;
    int nesting = s1->entity_specs.template_parameter_nesting;

    enum template_parameter_kind kind = TPK_UNKNOWN;
    switch (s1->kind)
    {
        case SK_TEMPLATE_NONTYPE_PARAMETER:
            {
                kind = TPK_NONTYPE;
                break;
            }
        case SK_TEMPLATE_TYPE_PARAMETER :
            {
                kind = TPK_TYPE;
                break;
            }
        case SK_TEMPLATE_TEMPLATE_PARAMETER:
            {
                kind = TPK_TEMPLATE;
                break;
            }
        case SK_TEMPLATE_NONTYPE_PARAMETER_PACK:
            {
                kind = TPK_NONTYPE_PACK;
                break;
            }
        case SK_TEMPLATE_TYPE_PARAMETER_PACK :
            {
                kind = TPK_TYPE_PACK;
                break;
            }
        case SK_TEMPLATE_TEMPLATE_PARAMETER_PACK:
            {
                kind = TPK_TEMPLATE_PACK;
                break;
            }
        default:
            internal_error("Invalid symbol kind %s", symbol_kind_to_str(s1->kind));
    }


    return deduction_set_get_unification_item(deduction_set, position, nesting, kind, s1->symbol_name);
}

char deduction_set_add_type_parameter_deduction(deduction_t* deduction,
        deduced_parameter_t* current_deduced_parameter)
{
    char found = 0;
    int i;
    for (i = 0; i < deduction->num_deduced_parameters; i++)
    {
        deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

        type_t* previous_deduced_type = previous_deduced_parameter->type;

        if (equivalent_types(previous_deduced_type, 
                    current_deduced_parameter->type))
        {
            found = 1;
            break;
        }
    }

    if (!found)
    {

        deduced_parameter_t* new_deduced_parameter = xcalloc(1, sizeof(*new_deduced_parameter));
        *new_deduced_parameter = *current_deduced_parameter;

        P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
    }

    return !found;
}

char deduction_set_add_nontype_parameter_deduction(deduction_t* deduction,
        deduced_parameter_t* current_deduced_parameter)
{
    nodecl_t current_deduced_value = current_deduced_parameter->value;
    scope_entry_t* current_deduced_symbol = nodecl_get_symbol(current_deduced_value);

    char found = 0;
    int i;
    for (i = 0; i < deduction->num_deduced_parameters; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Checking previous deductions\n");
        }
        deduced_parameter_t* previous_deduced_parameter = deduction->deduced_parameters[i];

        nodecl_t previous_deduced_value = previous_deduced_parameter->value;

        scope_entry_t* previous_unified_symbol = nodecl_get_symbol(previous_deduced_value);

        if (previous_unified_symbol != NULL
                && current_deduced_symbol != NULL
                && ((previous_unified_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER
                        && current_deduced_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
                    || (previous_unified_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                        && current_deduced_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Both previous deduction tree '%s' and new deduction tree '%s'"
                        " are nontype template parameters. Checking if they are the same\n", 
                        codegen_to_str(previous_deduced_value, nodecl_retrieve_context(previous_deduced_value)),
                        codegen_to_str(current_deduced_value, nodecl_retrieve_context(current_deduced_value)));
            }

            int previous_unified_expr_parameter_position = 
                previous_unified_symbol->entity_specs.template_parameter_position;
            int previous_unified_expr_parameter_nesting = 
                previous_unified_symbol->entity_specs.template_parameter_nesting;

            int currently_unified_template_param_position = 
                current_deduced_symbol->entity_specs.template_parameter_position;
            int currently_unified_template_param_nesting = 
                current_deduced_symbol->entity_specs.template_parameter_nesting;

            if ((currently_unified_template_param_position == previous_unified_expr_parameter_position)
                    && (currently_unified_template_param_nesting == previous_unified_expr_parameter_nesting))
            {
                found = 1;
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: They are the same\n");
                }
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: They are different\n");
                }
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Checking if previous deduced tree '%s' and current deduced tree '%s'"
                        " are the same\n", 
                        codegen_to_str(previous_deduced_value, nodecl_retrieve_context(previous_deduced_value)),
                        codegen_to_str(current_deduced_value, nodecl_retrieve_context(current_deduced_value)));
            }

            if (same_functional_expression(previous_deduced_value, current_deduced_value))
            {
                found = 1;
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: They are the same\n");
                }
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: They are different\n");
                }
            }
        }
    }

    if (!found)
    {
        deduced_parameter_t* new_deduced_parameter = xcalloc(1, sizeof(*new_deduced_parameter));
        *new_deduced_parameter = *current_deduced_parameter;

        P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
    }

    return !found;
}


deduction_t* deduction_set_get_unification_item(
        deduction_set_t* deduction_set,
        int position, int nesting,
        enum template_parameter_kind kind,
        const char* name)
{
    int i;
    for (i = 0; i < deduction_set->num_deductions; i++)
    {
        if (deduction_set->deduction_list[i]->parameter_position == position
                && deduction_set->deduction_list[i]->parameter_nesting == nesting)
        {
            return deduction_set->deduction_list[i];
        }
    }

    deduction_t* result = xcalloc(1, sizeof(*result));
    result->kind = kind;
    result->parameter_position = position;
    result->parameter_nesting = nesting;
    result->parameter_name = name;

    P_LIST_ADD(deduction_set->deduction_list, deduction_set->num_deductions, result);

    return result;
}

static deduction_t* deduction_set_deduction_set_get_unification_item_from_item(
        deduction_set_t* deduction_set,
        deduction_t* deduction)
{
    return deduction_set_get_unification_item(deduction_set,
            deduction->parameter_position,
            deduction->parameter_nesting,
            deduction->kind,
            deduction->parameter_name);
}

// Merges the second deduction set inside the first
void deduction_set_merge(deduction_set_t* dest,
        deduction_set_t* source,
        char combine_sequence)
{
    // Now we have to merge these single values into the final
    // deduction.  So, for template-packs we have to add value in their
    // existing deduction, for template that are not packs, just add
    // another deduction
    int j;
    for (j = 0; j < source->num_deductions; j++)
    {
        // The current deduction, will contain only single values
        deduction_t* current_deduction = source->deduction_list[j];

        // The merged deduction, if this deduction is for a template-pack will contain
        // several values (otherwise just single values)
        deduction_t* merged_deduction = deduction_set_deduction_set_get_unification_item_from_item(dest, current_deduction);

        enum {
              NON_COMBINING_SEQUENCE = 0,
              COMBINING_SEQUENCE = 1000
        };

        int kind = current_deduction->kind;
        if ((kind == TPK_TYPE_PACK
                    || kind == TPK_NONTYPE_PACK
                    || kind == TPK_TEMPLATE_PACK)
                && combine_sequence)
        {
            kind += COMBINING_SEQUENCE;
        }

        switch (kind)
        {
            case TPK_TYPE_PACK     + COMBINING_SEQUENCE:
            case TPK_TEMPLATE_PACK + COMBINING_SEQUENCE:
                {
                    // Since this is a deduction for a template-pack,
                    // existing deductions may contain several values
                    // already and we have to append the values deduced
                    // above to each of them, but as usual avoiding
                    // repeated deductions
                    int k1;
                    // Note that if current_deduction->num_deduced_parameters > 1
                    // merged_deduction->num_deduced_parameters will grow, so we
                    // keep it here
                    int num_merged_deductions = merged_deduction->num_deduced_parameters;
                    for (k1 = 0; k1 < num_merged_deductions; k1++)
                    {
                        deduced_parameter_t* current_merged_deduced_parameter = merged_deduction->deduced_parameters[k1];

                        // If only one has been deduced it is just a matter of extending
                        // the existing deduction
                        if (current_deduction->num_deduced_parameters > 0)
                        {
                            // We copy the current_merged_deduced_parameter except for the first
                            // that will update it in place (but we extend it the last) to avoid
                            // breaking the copy of the current_merged_deduced_parameter
                            int k2;
                            // Note that we start from 1
                            for (k2 = 1; k2 < current_deduction->num_deduced_parameters; k2++)
                            {
                                // Extend the current deduction with the new value
                                deduced_parameter_t* extended_deduced_parameter = deduced_parameter_copy(
                                        current_merged_deduced_parameter);

                                extended_deduced_parameter->type = get_sequence_of_types_append_type(
                                        extended_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);

                                /* char added = */ deduction_set_add_type_parameter_deduction(merged_deduction,
                                        extended_deduced_parameter);
                            }
                            // First iteration (updates in-place instead of copy)
                            k2 = 0;
                            current_merged_deduced_parameter->type =
                                get_sequence_of_types_append_type(
                                        current_merged_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);
                        }
                    }
                    // There were no deductions, simply add ours
                    if (merged_deduction->num_deduced_parameters == 0)
                    {
                        int k2;
                        for (k2 = 0; k2 < current_deduction->num_deduced_parameters; k2++)
                        {
                            type_t* deduced_type = current_deduction->deduced_parameters[k2]->type;
                            current_deduction->deduced_parameters[k2]->type = get_sequence_of_types(1, &deduced_type);
                            deduction_set_add_type_parameter_deduction(merged_deduction,
                                    current_deduction->deduced_parameters[k2]);
                        }
                    }
                    break;
                }
            case TPK_NONTYPE_PACK + COMBINING_SEQUENCE:
                {
                    // Since this is a deduction for a template-pack,
                    // existing deductions may contain several values
                    // already and we have to append the values deduced
                    // above to each of them, but as usual avoiding
                    // repeated deductions
                    int k1;
                    // Note that if current_deduction->num_deduced_parameters > 1
                    // merged_deduction->num_deduced_parameters will grow, so we
                    // keep it here
                    int num_merged_deductions = merged_deduction->num_deduced_parameters;
                    for (k1 = 0; k1 < num_merged_deductions; k1++)
                    {
                        deduced_parameter_t* current_merged_deduced_parameter = merged_deduction->deduced_parameters[k1];

                        // If only one has been deduced it is just a matter of extending
                        // the existing deduction
                        if (current_deduction->num_deduced_parameters > 0)
                        {
                            // We copy the current_merged_deduced_parameter except for the first
                            // that will update it in place (but we extend it the last) to avoid
                            // breaking the copy of the current_merged_deduced_parameter
                            int k2;
                            // Note that we start from 1
                            for (k2 = 1; k2 < current_deduction->num_deduced_parameters; k2++)
                            {
                                // Extend the current deduction with the new value
                                deduced_parameter_t* extended_deduced_parameter = deduced_parameter_copy(
                                        current_merged_deduced_parameter);

                                extended_deduced_parameter->value = nodecl_append_to_list(
                                        extended_deduced_parameter->value,
                                        current_deduction->deduced_parameters[k2]->value);
                                extended_deduced_parameter->type = get_sequence_of_types_append_type(
                                        extended_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);

                                /* char added = */ deduction_set_add_nontype_parameter_deduction(merged_deduction,
                                        extended_deduced_parameter);
                            }
                            // First iteration (updates in-place instead of copy)
                            k2 = 0;
                            current_merged_deduced_parameter->value = nodecl_append_to_list(
                                    current_merged_deduced_parameter->value,
                                    current_deduction->deduced_parameters[k2]->value);
                            current_merged_deduced_parameter->type =
                                get_sequence_of_types_append_type(
                                        current_merged_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);
                        }
                    }
                    // There were no deductions, simply add ours
                    if (merged_deduction->num_deduced_parameters == 0)
                    {
                        int k2;
                        for (k2 = 0; k2 < current_deduction->num_deduced_parameters; k2++)
                        {
                            type_t* deduced_type = current_deduction->deduced_parameters[k2]->type;
                            nodecl_t deduced_value = nodecl_shallow_copy(current_deduction->deduced_parameters[k2]->value);
                            current_deduction->deduced_parameters[k2]->type = get_sequence_of_types(1, &deduced_type);
                            current_deduction->deduced_parameters[k2]->value = nodecl_make_list_1(deduced_value);
                            deduction_set_add_nontype_parameter_deduction(merged_deduction,
                                    current_deduction->deduced_parameters[k2]);
                        }
                    }
                    break;
                }
            case TPK_TYPE:
            case TPK_TEMPLATE:
            case TPK_TYPE_PACK     + NON_COMBINING_SEQUENCE:
            case TPK_TEMPLATE_PACK + NON_COMBINING_SEQUENCE:
                {
                    int k;
                    for (k = 0; k < current_deduction->num_deduced_parameters; k++)
                    {
                        deduction_set_add_type_parameter_deduction(merged_deduction,
                                current_deduction->deduced_parameters[k]);
                    }
                    break;
                }
            case TPK_NONTYPE:
            case TPK_NONTYPE_PACK + NON_COMBINING_SEQUENCE:
                {
                    int k;
                    for (k = 0; k < current_deduction->num_deduced_parameters; k++)
                    {
                        deduction_set_add_nontype_parameter_deduction(merged_deduction,
                                current_deduction->deduced_parameters[k]);
                    }
                    break;
                }
            default:
                {
                    internal_error("Code unreachable", 0);
                }
        }
    }
}

static void deduction_set_slots_free(deduction_set_t** deductions, int num_deduction_slots)
{
    int i;
    for (i = 0; i < num_deduction_slots; i++)
    {
        deduction_set_free(deductions[i]);
    }
}

static char deduction_set_contains_incompatible_deduction(deduction_set_t* deduction_set)
{
    char intra_more_than_one_deduction = 0;
    int j;
    for (j = 0; j < deduction_set->num_deductions; j++)
    {
        deduction_t *current_deduction = deduction_set->deduction_list[j];
        // From one argument we deduced to 'A' for one same 'P'
        //
        // E.g.:
        //
        // void f(void (*a)(T, T));
        //
        // void k(int, float);
        //
        // void g()
        // {
        //    f(k); <-- // For parameter 'a' we deduce 'T <- int' and 'T <- float'
        // }
        intra_more_than_one_deduction = intra_more_than_one_deduction
           || (current_deduction->num_deduced_parameters > 1);
    }

    return intra_more_than_one_deduction;
}

static char finish_deduced_template_arguments(
        deduction_set_t* deduction_set,
        decl_context_t updated_context,
        template_parameter_list_t* type_template_parameters,
        template_parameter_list_t* deduced_template_arguments,
        char is_trial_deduction,
        const locus_t* locus)
{
    // Check that all parameters have been deduced an argument
    char c[MCXX_MAX_TEMPLATE_PARAMETERS];
    memset(c, 0, sizeof(c));

    int j;
    for (j = 0; j < deduction_set->num_deductions; j++)
    {
        deduction_t *current_deduction = deduction_set->deduction_list[j];

        ERROR_CONDITION(current_deduction->num_deduced_parameters > 1,
                "Error a parameter is deduced more than one argument here", 0);

        ERROR_CONDITION(current_deduction->parameter_position > MCXX_MAX_TEMPLATE_PARAMETERS,
                "Too many template parameters", 0);

        c[current_deduction->parameter_position] =
            (current_deduction->num_deduced_parameters == 1);
    }

    // C++11: Now complete with default deduced template arguments and template packs
    // and update nontype template parameters
    int i_tpl_parameters;
    for (i_tpl_parameters = 0; i_tpl_parameters < type_template_parameters->num_parameters; i_tpl_parameters++)
    {
        // Argument i_tpl_parameters-th was not deduced a template argument
        if (!c[i_tpl_parameters])
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: No template argument was deduced for template parameter '%s'\n",
                        deduced_template_arguments->parameters[i_tpl_parameters]->entry->symbol_name);
            }
            if (template_parameter_kind_is_pack(type_template_parameters->parameters[i_tpl_parameters]->kind))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: But nondeduced template parameter %d "
                            " is a pack so it will have an empty value\n", i_tpl_parameters);
                }
                // Packs are deduced their empty values
                template_parameter_value_t* new_template_argument = xcalloc(1, sizeof(*new_template_argument));
                switch (type_template_parameters->parameters[i_tpl_parameters]->kind)
                {
                    case TPK_TEMPLATE_PACK:
                    case TPK_TYPE_PACK:
                        {
                            new_template_argument->kind = template_parameter_kind_get_base_kind(
                                    type_template_parameters->parameters[i_tpl_parameters]->kind);
                            new_template_argument->type = get_sequence_of_types(0, NULL);

                            // // We update this for debugging purposes
                            // deduction_set->deduction_list[i_tpl_parameters]->kind = new_template_argument->kind;
                            // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->type =
                            //     new_template_argument->type;
                            break;
                        }
                    case TPK_NONTYPE_PACK:
                        {
                            new_template_argument->kind = TPK_NONTYPE;
                            // Warning: A NULL value for a TPK_NONTYPE means an empty list
                            new_template_argument->value = nodecl_null();
                            new_template_argument->type = update_type(
                                    type_template_parameters->parameters[i_tpl_parameters]->entry->type_information,
                                    updated_context,
                                    locus);

                            // // We update this for debugging purposes
                            // deduction_set->deduction_list[i_tpl_parameters]->kind = new_template_argument->kind;
                            // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->value =
                            //     new_template_argument->value;
                            // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->type =
                            //     new_template_argument->type;
                            break;
                        }
                    default:
                        {
                            internal_error("Unexpected kind", 0);
                        }
                }
                deduced_template_arguments->arguments[i_tpl_parameters] = new_template_argument;
            }
            else // default argument
            {
                template_parameter_value_t* default_template_argument
                    = type_template_parameters->arguments[i_tpl_parameters];
                if (default_template_argument == NULL
                        || !default_template_argument->is_default)
                {
                    if (!is_trial_deduction)
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: Template parameter '%s' does not have a "
                                    "deduced template argument and causes deduction to fail\n",
                                    deduced_template_arguments->parameters[i_tpl_parameters]->entry->symbol_name);
                        }
                        return 0;
                    }
                    else
                    {
                        continue;
                    }
                }
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: But nondeduced template parameter %d "
                            "has a default template argument\n", i_tpl_parameters);
                }

                diagnostic_context_push_buffered();
                template_parameter_value_t* new_template_argument = update_template_parameter_value(default_template_argument,
                        updated_context,
                        /* instantiation_symbol_map */ NULL,
                        locus,
                        /* index_pack */ -1);
                diagnostic_context_pop_and_discard();

                if (new_template_argument == NULL)
                {
                    // SFINAE
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Deduction fails because default template argument "
                                "%d failed to be updated\n",
                                i_tpl_parameters);
                    }
                    return 0;
                }

                deduced_template_arguments->arguments[i_tpl_parameters] = new_template_argument;

                // // We update this for debugging purposes
                // deduction_set->deduction_list[i_tpl_parameters]->kind = new_template_argument->kind;
                // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->value =
                //     new_template_argument->value;
                // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->type =
                //     new_template_argument->type;
            }
        }
        else if (deduced_template_arguments->arguments[i_tpl_parameters]->kind == TPK_NONTYPE)
        {
            deduced_template_arguments->arguments[i_tpl_parameters]->type =
                update_type(
                        type_template_parameters->parameters[i_tpl_parameters]->entry->type_information,
                        updated_context,
                        locus);

            type_t* t = deduced_template_arguments->arguments[i_tpl_parameters]->type;

            // // We update this for debugging purposes
            // deduction_set->deduction_list[i_tpl_parameters]->deduced_parameters[0]->type =
            //     deduced_template_arguments->arguments[i_tpl_parameters]->type;

            if (is_sequence_of_types(t))
            {
                int value_length = 0;
                int i, N = sequence_of_types_get_num_types(t);

                if (!nodecl_is_null(deduced_template_arguments->arguments[i_tpl_parameters]->value))
                {
                    if (nodecl_is_list(deduced_template_arguments->arguments[i_tpl_parameters]->value))
                        value_length = nodecl_list_length(deduced_template_arguments->arguments[i_tpl_parameters]->value);
                    else
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: There is a mismatch in nontype template argument %d, "
                                    "I expected a list as a value because its type is a sequence, "
                                    "but the value is not a list\n", i_tpl_parameters);
                        }
                        return 0;
                    }
                }

                if (value_length != N)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: There is a mismatch in nontype template argument %d, "
                                "the length of the list values and the sequence type associated do not match",
                                i_tpl_parameters);
                    }
                    return 0;
                }

                for (i = 0; i < N; i++)
                {
                    if (!check_nontype_template_argument_type(sequence_of_types_get_type_num(t, i)))
                    {
                        DEBUG_CODE()
                        {
                            fprintf(stderr, "TYPEDEDUC: Deduction fails because nontype template parameter "
                                    "%d was deduced a sequence type containing an invalid type '%s'\n",
                                    i_tpl_parameters, print_declarator(sequence_of_types_get_type_num(t, i)));
                        }
                        return 0;
                    }
                }
            }
            else if (!check_nontype_template_argument_type(t))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deduction fails because nontype template parameter "
                            "%d was deduced an invalid type '%s'\n",
                            i_tpl_parameters, print_declarator(t));
                }
                return 0;
            }
        }
    }

    return 1;
}

static char type_is_nondeduced_context(
        type_t* adjusted_parameter_type,
        type_t* argument_type,
        decl_context_t decl_context,
        const locus_t* locus)
{
    if (is_dependent_typename_type(adjusted_parameter_type) // typename T :: Type
            || is_typeof_expr(adjusted_parameter_type)) // decltype/__typeof__
    {
        return 1;
    }

    scope_entry_t* std_initializer_type = get_std_initializer_list_template(decl_context, locus,
            /* mandatory */ 0);

    // The argument is a braced list but the parameter is not a std::initializer_list<T>
    if (is_braced_list_type(argument_type)
            && !(std_initializer_type != NULL
                && is_named_class_type(adjusted_parameter_type)
                && is_template_specialized_type(get_actual_class_type(adjusted_parameter_type))
                && equivalent_types(
                    std_initializer_type->type_information,
                    template_specialized_type_get_related_template_type(
                        get_actual_class_type(no_ref(adjusted_parameter_type)))))
       )
    {
        return 1;
    }

    return 0;
}

static char deduction_of_argument_type_may_succeed(
        type_t* deduced_argument_type,
        type_t* argument_type,
        type_t* adjusted_parameter_type,
        type_t* original_parameter_type,
        decl_context_t decl_context,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Checking if the deduced argument type '%s' "
                "is acceptable for the argument type '%s' (adjusted parameter type is '%s', original parameter type is '%s')\n",
                print_declarator(deduced_argument_type),
                print_declarator(argument_type),
                print_declarator(adjusted_parameter_type),
                print_declarator(original_parameter_type));
    }

    if (type_is_nondeduced_context(adjusted_parameter_type,
                argument_type,
                decl_context,
                locus))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: But the original parameter type '%s' is a nonduced context "
                    "so it did not play any role in the overall deduction\n",
                    print_declarator(original_parameter_type));
        }
        return 1;
    }

    if (equivalent_types(argument_type, deduced_argument_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Deduced argument type '%s' and argument type '%s' match\n",
                    print_declarator(deduced_argument_type),
                    print_declarator(argument_type));
        }
        return 1;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deduced argument type '%s' and argument type '%s' are not exactly the same\n",
                print_declarator(deduced_argument_type),
                print_declarator(argument_type));
    }

    // This case must be valid and overload will stop if not
    //
    // template <typename _T>
    // void f(_T a, int b);
    //
    // void g()
    // {
    //    int a = 0;
    //    unsigned int b = 1;
    //
    //    f(a, b);
    // }
    //
    // Here we would see that 'unsigned int' (argument type) is not exactly 'int' (parameter type)
    // and fail. So if the parameter type (parameter_type) is not dependent, allow this case
    if (!is_dependent_type(original_parameter_type))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: But the original parameter type '%s' was not dependent so "
                    "it did not play any role in the overall deduction\n",
                    print_declarator(original_parameter_type));
        }
        return 1;
    }
    else if (is_any_reference_type(original_parameter_type)
            && equivalent_types(
                get_unqualified_type(no_ref(deduced_argument_type)),
                get_unqualified_type(no_ref(argument_type)))
            && is_more_or_equal_cv_qualified_type(no_ref(deduced_argument_type), no_ref(argument_type)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: But original parameter type is reference and"
                    " deduced parameter type '%s' is more qualified than argument type '%s'\n",
                    print_declarator(deduced_argument_type),
                    print_declarator(argument_type));
        }
        return 1;
    }
    /*
     * This case is valid
     *
     * struct A { };
     * struct B : A { };
     *
     * template<typename _T>
     * void f(_T t, _T* t);
     *
     * struct C { };
     *
     * void g()
     * {
     *   A a;
     *   B b;
     *   C c;
     *
     *   f(a, &b); // valid
     *   f(a, &c); // error
     * }
     *
     */
    else if (is_pointer_type(argument_type)
            && is_pointer_type(deduced_argument_type))
    {
        standard_conversion_t standard_conversion;
        if (standard_conversion_between_types(&standard_conversion, argument_type, deduced_argument_type, locus))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: But argument type '%s' is a convertible pointer "
                        "to deduced parameter type '%s'\n",
                        print_declarator(argument_type),
                        print_declarator(deduced_argument_type));
            }
            return 1;
        }
    }
    /*
     * This case is wrongly handled by all the compilers out, so we will too
     *
     * Consider the following case
     *
     * template <typename _T>
     * struct A
     * {
     *     typedef A<_T> M;
     * 
     *     template <typename _Q>
     *     void f(A a, _Q q)
     *     {
     *     }
     * 
     *     template <typename _Q>
     *     void g(A<_Q> a, _Q q)
     *     {
     *     }
     * 
     *     template <typename _Q>
     *     void h(M m, _Q q)
     *     {
     *     }
     * };
     * 
     * template <typename _T>
     * struct B : A<_T>
     * {
     * };
     * 
     * int main(int argc, char* argv[])
     * {
     *     A<int> a;
     *     B<int> b;
     * 
     *     a.f(b, 3); // This case should be wrong
     *     a.g(b, 3); // This case should be right
     *     a.h(b, 3); // This case should be wrong
     * }
     *
     * But nobody checks this and allow all three calls. We should
     * check that the parameter is *actually* a template-id (if we
     * follow strictly the Standard).
     *
     * We allow all these three cases. But we check that it is actually
     * a derived type.
     *
     * For the case of a pointer it was already checked in the previous
     * case.
     */
    else if (is_named_class_type(deduced_argument_type)
            && is_template_specialized_type(get_actual_class_type(deduced_argument_type))
            && is_named_class_type(argument_type))
    {
        if (class_type_is_base_instantiating(deduced_argument_type, argument_type, locus))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: But argument type '%s' is a derived class type of "
                        "the deduced parameter type '%s'\n",
                        print_declarator(argument_type),
                        print_declarator(deduced_argument_type));
            }
            return 1;
        }
    }
    else if (is_braced_list_type(argument_type))
    {
        scope_entry_t* std_initializer_type = get_std_initializer_list_template(decl_context, locus,
                /* mandatory */ 0);

        if (std_initializer_type != NULL
                && is_named_class_type(no_ref(deduced_argument_type))
                && is_template_specialized_type(get_actual_class_type(no_ref(deduced_argument_type)))
                && equivalent_types(
                    std_initializer_type->type_information,
                    template_specialized_type_get_related_template_type(
                        get_actual_class_type(no_ref(deduced_argument_type)))))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: But the deduced argument type is a std::initializer_type<T>\n");
            }
            return 1;
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deduction fails because the deduced "
                "argument type '%s' is not acceptable for the argument type '%s'\n",
                print_declarator(argument_type),
                print_declarator(deduced_argument_type));
    }

    return 0;
}

static char deduction_of_base_type_may_succeed(
        type_t* parameter_type,
        type_t* original_parameter_type,
        type_t* argument_type,
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* type_template_parameters,
        deduction_set_t* trial_deduction_set,
        decl_context_t updated_context,
        const locus_t* locus)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Checking if the deduction using a base class may succeed or not\n");
    }

    if (deduction_set_contains_incompatible_deduction(trial_deduction_set))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Deduction fails because we deduced"
                    " more than one value for the same template parameter\n");
        }
        return 0;
    }

    template_parameter_list_t* current_deduced_template_arguments
        = build_template_parameter_list_from_deduction_set(
                template_parameters,
                trial_deduction_set);
    updated_context.template_parameters = current_deduced_template_arguments;

    char ok = finish_deduced_template_arguments(
            trial_deduction_set,
            updated_context,
            type_template_parameters,
            current_deduced_template_arguments,
            /* is_trial_deduction */ 1,
            locus);

    if (!ok)
    {
        free_template_parameter_list(current_deduced_template_arguments);
        return 0;
    }

    diagnostic_context_push_buffered();
    type_t* deduced_argument_type = update_type(parameter_type, updated_context, locus);
    diagnostic_context_pop_and_discard();

    if (deduced_argument_type == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Deduction fails because it could not deduce any argument type\n");
        }
        free_template_parameter_list(current_deduced_template_arguments);
        return 0;
    }

    char result = deduction_of_argument_type_may_succeed(
            deduced_argument_type,
            argument_type,
            parameter_type,
            original_parameter_type,
            updated_context,
            locus);
    free_template_parameter_list(current_deduced_template_arguments);

    return result;
}

static void deduce_template_arguments_from_types(
        type_t* parameter_type,
        type_t* original_parameter_type UNUSED_PARAMETER,
        type_t* argument_type,
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* type_template_parameters,
        deduction_set_t* current_deduction_set,
        decl_context_t updated_context,
        const locus_t* locus,
        char is_function_call)
{
    if (is_function_call
            && ((is_named_class_type(parameter_type)
                    // parameter_type is a template-id
                    && is_template_specialized_type(get_actual_class_type(parameter_type))
                    && is_named_class_type(argument_type))
                || (is_pointer_type(parameter_type)
                    && is_pointer_type(argument_type)
                    && is_named_class_type(pointer_type_get_pointee_type(parameter_type))
                    // parameter_type is a pointer to template-id
                    && is_template_specialized_type(get_actual_class_type(pointer_type_get_pointee_type(parameter_type)))
                    && is_named_class_type(pointer_type_get_pointee_type(argument_type))))
            && !is_dependent_type(argument_type))
    {
        type_t *class_parameter_type = parameter_type;
        type_t *class_argument_type = argument_type;

        if (is_pointer_type(class_parameter_type))
        {
            class_parameter_type = pointer_type_get_pointee_type(class_parameter_type);
            class_argument_type = pointer_type_get_pointee_type(class_argument_type);
        }

        // Do a trial deduction
        deduction_set_t* orig_deduction_set = deduction_set_copy(current_deduction_set);
        deduction_set_t* trial_deduction_set = deduction_set_copy(current_deduction_set);

        unificate_two_types(class_parameter_type, class_argument_type, trial_deduction_set, updated_context, locus);

        if (deduction_of_base_type_may_succeed(
                    class_parameter_type,
                    class_parameter_type,
                    class_argument_type,
                    template_parameters,
                    type_template_parameters,
                    trial_deduction_set,
                    updated_context,
                    locus))
        {
            *current_deduction_set = *trial_deduction_set;
        }
        else
        {
            deduction_set_free(trial_deduction_set);
            // Try with bases
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Instantiating class '%s' since we will try to deduce against bases\n",
                        print_declarator(class_argument_type));
            }
            instantiate_template_class_if_possible(named_type_get_symbol(class_argument_type), updated_context, locus);

            scope_entry_list_t* all_bases = class_type_get_all_bases(get_actual_class_type(class_argument_type),
                    /* include_dependent */ 0);

            // Keep the original deduction because we'll extend it for every base
            orig_deduction_set = deduction_set_copy(current_deduction_set);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(all_bases);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                trial_deduction_set = deduction_set_copy(orig_deduction_set);

                scope_entry_t* entry = entry_list_iterator_current(it);
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Trial deduction using base class '%s'\n",
                            get_qualified_symbol_name(entry, entry->decl_context));
                }

                unificate_two_types(class_parameter_type, get_user_defined_type(entry), trial_deduction_set, updated_context, locus);

                if (deduction_of_base_type_may_succeed(
                            class_parameter_type,
                            class_parameter_type,
                            get_user_defined_type(entry),
                            template_parameters,
                            type_template_parameters,
                            trial_deduction_set,
                            updated_context,
                            locus))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Trial deduction using base class '%s' succeeded\n",
                                get_qualified_symbol_name(entry, entry->decl_context));
                    }
                    deduction_set_merge(current_deduction_set, trial_deduction_set, /* combine_sequence */ 0);
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Trial deduction using base class '%s' failed\n",
                                get_qualified_symbol_name(entry, entry->decl_context));
                    }
                }

                deduction_set_free(trial_deduction_set);
            }
            entry_list_iterator_free(it);
            entry_list_free(all_bases);

        }
        deduction_set_free(orig_deduction_set);
    }
    else
    {
        unificate_two_types(parameter_type, argument_type, current_deduction_set, updated_context, locus);
    }
}

char deduce_template_arguments_common(
        // These are the template adjusted_parameters of this function specialization
        template_parameter_list_t* template_parameters,
        // These are the template adjusted_parameters of template-type
        // We need these because of default template arguments for template
        // functions (they are not kept in each specialization)
        template_parameter_list_t* type_template_parameters,
        type_t** adjusted_arguments, int num_arguments,
        type_t** adjusted_parameters, int num_parameters,
        type_t** original_parameters,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus,
        template_parameter_list_t* explicit_template_parameters,
        char is_function_call)
{
    // This is used for debugging
    char * kind_name[] =
    {
        [TPK_TYPE] = "type-template parameter",
        [TPK_NONTYPE] = "nontype-template parameter",
        [TPK_TEMPLATE] = "template-template parameter",

        // Should not appear here
        [TPK_TYPE_PACK] = "type-template parameter pack",
        [TPK_NONTYPE_PACK] = "nontype-template parameter pack",
        [TPK_TEMPLATE_PACK] = "template-template parameter pack",
    };
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Trying to deduce template adjusted_arguments for template\n");
        int i;
        if (template_parameters != NULL)
        {
            fprintf(stderr, "TYPEDEDUC: Template adjusted_parameters of the template type\n");
            for (i = 0; i < template_parameters->num_parameters; i++)
            {
                template_parameter_t* current_template_parameter = template_parameters->parameters[i];

                fprintf(stderr, "TYPEDEDUC:   [%d] %s - %s\n", i, 
                        kind_name[current_template_parameter->kind],
                        current_template_parameter->entry->symbol_name);
            }
            fprintf(stderr, "TYPEDEDUC: End of template adjusted_parameters involved\n");
            if (explicit_template_parameters == NULL)
            {
                fprintf(stderr, "TYPEDEDUC: No explicit template adjusted_arguments available\n");
            }
            else
            {
                fprintf(stderr, "TYPEDEDUC: There are %d explicit template adjusted_arguments\n", 
                        explicit_template_parameters->num_parameters);
                for (i = 0; i < explicit_template_parameters->num_parameters; i++)
                {
                    template_parameter_value_t* current_template_argument = explicit_template_parameters->arguments[i];

                    const char* value = "<<<UNKNOWN>>>";
                    if (current_template_argument != NULL)
                    {
                        switch (current_template_argument->kind)
                        {
                            case TPK_TEMPLATE:
                            case TPK_TYPE:
                                value = print_declarator(current_template_argument->type);
                                break;
                            case TPK_NONTYPE:
                                value = nodecl_is_null(current_template_argument->value)
                                    ? "<<NULL>>"
                                    : codegen_to_str(current_template_argument->value,
                                            nodecl_retrieve_context(current_template_argument->value));
                                break;
                            default:
                                internal_error("Code unreachable", 0);
                        }
                        fprintf(stderr, "TYPEDEDUC:   [%d] %s <- %s\n", i, 
                                kind_name[current_template_argument->kind],
                                value);
                    }
                    else
                    {
                        fprintf(stderr, "TYPEDEDUC:   [%d] <<NULL!!!>>\n", i);
                    }
                }
                fprintf(stderr, "TYPEDEDUC: End of explicit template adjusted_arguments available\n");
            }
        }
        else
        {
            fprintf(stderr, "TYPEDEDUC: There are no template adjusted_parameters\n");
        }

        fprintf(stderr, "TYPEDEDUC: For this deduction we have %d parameter types\n", num_parameters);

        for (i = 0; i < num_parameters; i++)
        {
            fprintf(stderr, "TYPEDEDUC:    Parameter %d: Type: '%s'\n", i,
                    print_declarator(adjusted_parameters[i]));
        }

        fprintf(stderr, "TYPEDEDUC: For this deduction we have %d argument types\n", num_arguments);

        for (i = 0; i < num_arguments; i++)
        {
            fprintf(stderr, "TYPEDEDUC:    Argument %d: Type: '%s'\n", i,
                    print_declarator(adjusted_arguments[i]));
        }
    }

    *deduced_template_arguments = NULL;

    deduction_set_t *deductions[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(deductions, 0, sizeof(deductions));

    if (template_parameters == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Type deduction successes trivially because there are no template adjusted_parameters\n");
        }

        internal_error("Not yet implemented", 0);
        return 1;
    }

    decl_context_t updated_context = decl_context;

    deduction_set_t *explicit_deductions = NULL;
    int num_deduction_slots = 0;
    if (explicit_template_parameters != NULL
            && explicit_template_parameters->num_parameters > 0)
    {
        /* If we are given explicit template adjusted_arguments register them in the deduction result */
        updated_context.template_parameters = xcalloc(1, sizeof(*updated_context.template_parameters));
        updated_context.template_parameters->enclosing = decl_context.template_parameters->enclosing;
        updated_context.template_parameters->is_explicit_specialization =
            decl_context.template_parameters->is_explicit_specialization;

        int i_param = 0;
        int i_arg = 0;
        int expanding_index = -1;
        char in_pack = 0;
        template_parameter_value_t* last_arg = NULL;
        while (i_param < template_parameters->num_parameters
                && i_arg < explicit_template_parameters->num_parameters)
        {
            if (template_parameter_kind_get_base_kind(explicit_template_parameters->arguments[i_arg]->kind)
                    != template_parameter_kind_get_base_kind(template_parameters->parameters[i_param]->kind))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Deduction fails because mismatch in template argument/template parameter "
                            "(we expected a %s but we got a %s)\n",
                            kind_name[ template_parameters->parameters[i_param]->kind ],
                            kind_name[ explicit_template_parameters->arguments[i_arg]->kind ]);
                }
                deduction_set_slots_free(deductions, num_deduction_slots);
                return 0;
            }

            if (template_parameter_kind_is_pack(template_parameters->parameters[i_param]->kind))
            {
                if (!in_pack)
                {
                    in_pack = 1;
                    int num = updated_context.template_parameters->num_parameters;
                    P_LIST_ADD(updated_context.template_parameters->parameters,
                            num,
                            template_parameters->parameters[i_param]);
                    // Empty value
                    last_arg = xcalloc(1, sizeof(*last_arg));
                    last_arg->kind = template_parameter_kind_get_base_kind(template_parameters->parameters[i_param]->kind);
                    P_LIST_ADD(updated_context.template_parameters->arguments,
                            updated_context.template_parameters->num_parameters,
                            last_arg);
                }

                type_t* t = explicit_template_parameters->arguments[i_arg]->type;
                switch (explicit_template_parameters->arguments[i_arg]->kind)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        {
                            last_arg->type = get_sequence_of_types_append_type(last_arg->type, t);
                        }
                        break;
                    case TPK_NONTYPE:
                        {
                            nodecl_t value = explicit_template_parameters->arguments[i_arg]->value;
                            last_arg->type = get_sequence_of_types_append_type(last_arg->type, t);

                            if (nodecl_is_list(value))
                                last_arg->value = nodecl_concat_lists(last_arg->value, value);
                            else
                                last_arg->value = nodecl_append_to_list(last_arg->value, value);
                        }
                        break;
                    default:
                        internal_error("Invalid argument kind", 0);
                }

                // We stay in the same i_param
                i_arg++;
            }
            else
            {
                switch (explicit_template_parameters->arguments[i_arg]->kind)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                        {
                            type_t* t = explicit_template_parameters->arguments[i_arg]->type;
                            if (is_sequence_of_types(t))
                            {
                                // Several values for consecutive adjusted_parameters
                                int num_types = sequence_of_types_get_num_types(t);

                                if (num_types > 0)
                                {
                                    if (expanding_index < 0)
                                    {
                                        expanding_index = 0;
                                    }

                                    template_parameter_value_t* new_value = xcalloc(1, sizeof(*new_value));
                                    new_value->kind = explicit_template_parameters->arguments[i_arg]->kind;
                                    new_value->type = sequence_of_types_get_type_num(t, expanding_index);

                                    int num =
                                        updated_context.template_parameters->num_parameters;
                                    P_LIST_ADD(updated_context.template_parameters->parameters,
                                            num,
                                            template_parameters->parameters[i_param]);
                                    P_LIST_ADD(updated_context.template_parameters->arguments,
                                            updated_context.template_parameters->num_parameters,
                                            new_value);

                                    expanding_index++;
                                    if (expanding_index == num_types)
                                        expanding_index = -1;
                                }

                                i_arg++;
                                // Note that we will stay in the same i_param unless we ended with this
                                // expanded value
                                if (expanding_index < 0)
                                    i_param++;
                            }
                            else
                            {
                                // One value for one parameter
                                int num =
                                        updated_context.template_parameters->num_parameters;
                                P_LIST_ADD(updated_context.template_parameters->parameters,
                                        num,
                                        template_parameters->parameters[i_param]);
                                P_LIST_ADD(updated_context.template_parameters->arguments,
                                        updated_context.template_parameters->num_parameters,
                                        explicit_template_parameters->arguments[i_arg]);
                                i_arg++;
                                i_param++;
                            }
                        }
                        break;
                    case TPK_NONTYPE:
                        {
                            nodecl_t value = explicit_template_parameters->arguments[i_arg]->value;
                            type_t* t = explicit_template_parameters->arguments[i_arg]->type;
                            if (nodecl_is_list(value))
                            {
                                // Several values for consecutive adjusted_parameters
                                if (expanding_index < 0)
                                {
                                    expanding_index = 0;
                                }

                                template_parameter_value_t* new_value = xcalloc(1, sizeof(*new_value));
                                int num_items = 0;
                                nodecl_t* list = nodecl_unpack_list(value, &num_items);
                                new_value->kind = explicit_template_parameters->arguments[i_arg]->kind;
                                new_value->value = list[expanding_index];
                                new_value->type = sequence_of_types_get_type_num(t, expanding_index);

                                int num = updated_context.template_parameters->num_parameters;
                                P_LIST_ADD(updated_context.template_parameters->parameters,
                                        num,
                                        template_parameters->parameters[i_param]);
                                P_LIST_ADD(updated_context.template_parameters->arguments,
                                        updated_context.template_parameters->num_parameters,
                                        new_value);

                                xfree(list);

                                expanding_index++;
                                if (expanding_index == num_items)
                                    expanding_index = -1;

                                i_arg++;
                                // Note that we will stay in the same i_param unless we ended with this
                                // expanded value
                                if (expanding_index < 0)
                                    i_param++;
                            }
                            else
                            {
                                // One value for one parameter
                                int num = updated_context.template_parameters->num_parameters;
                                P_LIST_ADD(updated_context.template_parameters->parameters,
                                        num,
                                        template_parameters->parameters[i_param]);
                                P_LIST_ADD(updated_context.template_parameters->arguments,
                                        updated_context.template_parameters->num_parameters,
                                        explicit_template_parameters->arguments[i_arg]);
                                i_arg++;
                                i_param++;
                            }
                        }
                        break;
                    default:
                        internal_error("Invalid argument kind", 0);
                }
            }
        }

        if (i_param >= template_parameters->num_parameters
                && i_arg < explicit_template_parameters->num_parameters)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Deduction fails because there are too many template adjusted_arguments for this template function\n");
            }
            deduction_set_slots_free(deductions, num_deduction_slots);
            return 0;
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Parameter types updated with explicit template adjusted_arguments\n");
        }

        int j;
        explicit_deductions = counted_xcalloc(1, sizeof(*explicit_deductions), &_bytes_typededuc);
        for (j = 0; j < updated_context.template_parameters->num_parameters; j++)
        {
            template_parameter_t* current_template_parameter = updated_context.template_parameters->parameters[j];
            template_parameter_value_t* current_explicit_template_argument = updated_context.template_parameters->arguments[j];

            deduction_t* deduction_item = deduction_set_get_unification_item_for_template_parameter(explicit_deductions,
                    current_template_parameter->entry);

            deduced_parameter_t* current_deduced_parameter = counted_xcalloc(1, sizeof(*current_deduced_parameter), &_bytes_typededuc);

            switch (template_parameter_kind_get_base_kind(current_template_parameter->kind))
            {
                case TPK_TEMPLATE:
                case TPK_TYPE :
                    {
                        current_deduced_parameter->type = current_explicit_template_argument->type;
                        break;
                    }
                case TPK_NONTYPE:
                    {
                        current_deduced_parameter->value = current_explicit_template_argument->value;
                        current_deduced_parameter->type = current_template_parameter->entry->type_information;
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                    }
            }

            P_LIST_ADD(deduction_item->deduced_parameters, deduction_item->num_deduced_parameters, current_deduced_parameter);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Updating parameter types with explicit template adjusted_arguments\n");
        }
        deductions[0] = explicit_deductions;
        num_deduction_slots++;
        // Update adjusted_parameters with the explicit given template adjusted_arguments
        // deduction machinery would try to match them deducing template
        // adjusted_parameters explicitly given (yielding to potential different values)
        //
        // e.g.
        //
        // template <typename _T, typename _Q>
        // void f(_T, _Q);
        //
        // void g()
        // {
        //    f<int>(3.2f, 4.5f);
        // }
        //
        // This calls 'f<int, float>(int, float)' although if types were deduced
        // without considering what is given, _T would be 'float' too. So when
        // doing deduction, deduction machinery has to see something like 
        // (invalid C++ code)
        //
        //  template <int, typename _Q> 
        //  void f(int, _Q); <-- this is what we solve now
        //

        for (j = 0; j < num_parameters
                && j < num_arguments; j++)
        {
            // Update the parameter
            type_t* updated_parameter = NULL;
            diagnostic_context_push_buffered();
            updated_parameter = update_type(adjusted_parameters[j],
                    updated_context,
                    locus);
            diagnostic_context_pop_and_discard();

            if (updated_parameter == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Update of parameter [%d] (with original type '%s') "
                            "with explicitly given template adjusted_arguments failed.",
                            j, print_declarator(adjusted_parameters[j]));

                    if (updated_parameter != NULL)
                    {
                        fprintf(stderr, " Type '%s' is not sound\n",
                                print_declarator(updated_parameter));
                    }
                    else
                    {
                        fprintf(stderr, " No type was actually computed\n");
                    }
                }
                deduction_set_slots_free(deductions, num_deduction_slots);
                return 0;
            }

            adjusted_parameters[j] = updated_parameter;

            // Likewise for the original parameter
            diagnostic_context_push_buffered();
            updated_parameter = update_type(original_parameters[j],
                    updated_context,
                    locus);
            diagnostic_context_pop_and_discard();

            if (updated_parameter == NULL)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEDEDUC: Update of original parameter [%d] (with original type '%s') "
                            "with explicitly given template adjusted_arguments failed.",
                            j, print_declarator(original_parameters[j]));

                    if (updated_parameter != NULL)
                    {
                        fprintf(stderr, " Type '%s' is not sound\n",
                                print_declarator(updated_parameter));
                    }
                    else
                    {
                        fprintf(stderr, " No type was actually computed\n");
                    }
                }
                deduction_set_slots_free(deductions, num_deduction_slots);
                return 0;
            }

            original_parameters[j] = updated_parameter;
        }
    }

    int i_param = 0;
    int i_arg = 0;
    while (i_arg < num_arguments
            && i_param < num_parameters)
    {
        ERROR_CONDITION(num_deduction_slots >= MCXX_MAX_FUNCTION_CALL_ARGUMENTS, "Too many adjusted_arguments\n", 0);

        type_t* argument_type = adjusted_arguments[i_arg];
        type_t* parameter_type = adjusted_parameters[i_param];
        type_t* original_parameter_type = original_parameters[i_param];

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: Computing deduction for argument %d\n", i_arg);
            fprintf(stderr, "TYPEDEDUC:   Argument type  : %s\n", print_declarator(argument_type));
            fprintf(stderr, "TYPEDEDUC:   Parameter type : %s\n", print_declarator(parameter_type));
        }

        deduction_set_t *current_deduction = counted_xcalloc(1, sizeof(*current_deduction), &_bytes_typededuc);
        if (explicit_deductions != NULL)
        {
            deduction_set_merge(current_deduction,
                    explicit_deductions,
                    /* combine_sequence */ 0);
        }

        if (is_pack_type(parameter_type))
        {
            if ((i_param + 1) == num_parameters)
            {
                int num_types = num_arguments - i_arg;
                type_t* types[num_types + 1];
                int k;
                for (k = 0; k < num_types; k++)
                    types[k] = adjusted_arguments[k + i_arg];

                deduce_template_arguments_from_types(parameter_type,
                        original_parameter_type,
                        get_sequence_of_types(num_types, types),
                        template_parameters,
                        type_template_parameters,
                        current_deduction,
                        updated_context,
                        locus,
                        is_function_call);

                // All the adjusted_arguments have been used to deduce this template-pack
                i_arg = num_arguments;
            }
            else
            {
                // Not at the last position, skip this template-pack as it will not be deduced at all
                i_param++;
            }
        }
        else
        {
            // Not a template pack
            deduce_template_arguments_from_types(
                    parameter_type,
                    original_parameter_type,
                    argument_type,
                    template_parameters,
                    type_template_parameters,
                    current_deduction,
                    updated_context, locus,
                    is_function_call);

            i_param++;
            i_arg++;
        }

        deductions[num_deduction_slots] = current_deduction;
        num_deduction_slots++;
    }


    // Several checks must be performed here when deducing P/A
    int i_deductions;
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Computed deduction sets (may still be incorrect)\n");

        for (i_deductions = 0; i_deductions < num_deduction_slots; i_deductions++)
        {
            print_deduction_set(deductions[i_deductions]);
        }

        fprintf(stderr, "TYPEDEDUC: No more deduction sets\n");
    }

    // 2.1 If any pair P/A leads to different deduced types, deduction fails
    //    "intra" deduction check
    char intra_more_than_one_deduction = 0;
    for (i_deductions = 0; (i_deductions < num_deduction_slots)
            && !intra_more_than_one_deduction;
            i_deductions++)
    {
        intra_more_than_one_deduction = intra_more_than_one_deduction
            || deduction_set_contains_incompatible_deduction(deductions[i_deductions]);
    }

    if (intra_more_than_one_deduction)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEDEDUC: We deduced more than one value for the same template parameter "
                    "using the same parameter-argument pair\n");
        }
        deduction_set_slots_free(deductions, num_deduction_slots);
        return 0;
    }

    int num_deduced_template_arguments = 0;
    if (template_parameters != NULL)
    {
        num_deduced_template_arguments = type_template_parameters->num_parameters;
    }

    deduction_set_t deduced_arguments;
    memset(&deduced_arguments, 0, sizeof(deduced_arguments));

    deduced_arguments.num_deductions = num_deduced_template_arguments;

    // It might happen that nothing is actually deduced and everything is by
    // deduced by default template argument
    deduced_parameter_t _deduced_parameters_values[num_deduced_template_arguments + 1];
    memset(_deduced_parameters_values, 0, sizeof(_deduced_parameters_values));

    deduced_parameter_t *_deduced_parameters[num_deduced_template_arguments + 1];
    memset(_deduced_parameters, 0, sizeof(_deduced_parameters));

    deduction_t _deduction_list_values[num_deduced_template_arguments + 1];
    memset(_deduction_list_values, 0, sizeof(_deduction_list_values));

    deduction_t *_deduction_list[num_deduced_template_arguments + 1];
    deduced_arguments.deduction_list = _deduction_list;

    for (i_deductions = 0; i_deductions < deduced_arguments.num_deductions; i_deductions++)
    {
        _deduction_list[i_deductions] = &(_deduction_list_values[i_deductions]);
        _deduced_parameters[i_deductions] = &_deduced_parameters_values[i_deductions];
        deduced_arguments.deduction_list[i_deductions]->deduced_parameters = &(_deduced_parameters[i_deductions]);

        deduced_arguments.deduction_list[i_deductions]->parameter_name =
            template_parameters->parameters[i_deductions]->entry->symbol_name;
        deduced_arguments.deduction_list[i_deductions]->parameter_position =
            template_parameters->parameters[i_deductions]->entry->entity_specs.template_parameter_position;
        deduced_arguments.deduction_list[i_deductions]->parameter_nesting =
            template_parameters->parameters[i_deductions]->entry->entity_specs.template_parameter_nesting;
    }

    // Merge all the deductions in a single one
    for (i_deductions = 0; i_deductions < num_deduction_slots; i_deductions++)
    {
        int j;
        for (j = 0; j < deductions[i_deductions]->num_deductions; j++)
        {
            deduction_t *current_deduction = deductions[i_deductions]->deduction_list[j];

            ERROR_CONDITION(current_deduction->parameter_position >= deduced_arguments.num_deductions,
                    "Invalid parameter position %d >= %d ",
                    current_deduction->parameter_position,
                    deduced_arguments.num_deductions);

            deduction_t* result_deduction =
                deduced_arguments.deduction_list[current_deduction->parameter_position];

            if (current_deduction->num_deduced_parameters == 0)
            {
                // Skip empty deductions
                continue;
            }
            else if (current_deduction->num_deduced_parameters == 1)
            {
                result_deduction->num_deduced_parameters = 1;
            }
            else
            {
                internal_error("Error a parameter is deduced more than one argument here", 0);
            }

            deduced_parameter_t* current_deduced_parameter = current_deduction->deduced_parameters[0];

            // 2.2 If different pairs P/A lead to different deduced adjusted_arguments for the same parameter
            //     deduction fails
            if (result_deduction->kind == TPK_UNKNOWN)
            {
                result_deduction->kind = current_deduction->kind;

                *(result_deduction->deduced_parameters[0]) = *current_deduced_parameter;
            }
            else
            {
                // Have to check that they are equivalents
                if (result_deduction->kind
                        != current_deduction->kind)
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEDEDUC: Type deduction fails because deduced template adjusted_arguments are not of the same kind\n");
                    }
                    deduction_set_slots_free(deductions, num_deduction_slots);
                    return 0;
                }

                deduced_parameter_t* result_deduced_parameter = result_deduction->deduced_parameters[0];

                switch (result_deduction->kind)
                {
                    case TPK_TYPE:
                    case TPK_TEMPLATE:
                    case TPK_TYPE_PACK:
                    case TPK_TEMPLATE_PACK:
                        {
                            if (!equivalent_types(result_deduced_parameter->type,
                                        current_deduced_parameter->type))
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEDEDUC: Type deduction fails because previous deduction (%s) "
                                            "for type template argument does not match current one (%s)\n",
                                            print_declarator(result_deduced_parameter->type),
                                            print_declarator(current_deduced_parameter->type));
                                }
                                deduction_set_slots_free(deductions, num_deduction_slots);
                                return 0;
                            }
                            break;
                        }
                    case TPK_NONTYPE:
                    case TPK_NONTYPE_PACK:
                        {

                            if (!same_functional_expression(
                                        result_deduced_parameter->value,
                                        current_deduced_parameter->value)
                                    || !same_functional_expression(
                                        current_deduced_parameter->value,
                                        result_deduced_parameter->value))
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEDEDUC: Type deduction fails because previous "
                                            "deduction for nontype template argument does not match\n");
                                }
                                deduction_set_slots_free(deductions, num_deduction_slots);
                                return 0;
                            }
                            break;
                        }
                    default:
                        {
                            internal_error("Invalid deduction kind\n", 0);
                        }
                }
            }
        }
    }

    // For nontype template adjusted_arguments and default deduced template adjusted_arguments
    // their types may have to be updated
    template_parameter_list_t* current_deduced_template_arguments
        = build_template_parameter_list_from_deduction_set(
                template_parameters,
                &deduced_arguments);
    updated_context.template_parameters = current_deduced_template_arguments;

    char ok = finish_deduced_template_arguments(
            &deduced_arguments,
            updated_context,
            type_template_parameters,
            current_deduced_template_arguments,
            /* is_trial_deduction */ 0,
            locus);

    if (!ok)
    {
        deduction_set_slots_free(deductions, num_deduction_slots);
        return 0;
    }

    *deduced_template_arguments = current_deduced_template_arguments;

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deduction seems fine here\n");

        fprintf(stderr, "TYPEDEDUC: Results of the deduction\n");

        print_deduction_set(&deduced_arguments);

        fprintf(stderr, "TYPEDEDUC: No more deduced template adjusted_parameters\n");
    }

    deduction_set_slots_free(deductions, num_deduction_slots);

    // Seems a fine deduction
    return 1;
}

char deduce_arguments_of_conversion(
        type_t* destination_type,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* type_template_parameters,
        decl_context_t decl_context,
        template_parameter_list_t **deduced_template_arguments,
        const locus_t* locus)
{
    scope_entry_t* specialized_symbol = named_type_get_symbol(specialized_named_type);

    ERROR_CONDITION(specialized_symbol->kind != SK_FUNCTION, 
            "This is not a template specialized function", 0);

    ERROR_CONDITION((!specialized_symbol->entity_specs.is_conversion),
            "This is not a conversion function", 0);

    type_t* specialized_type = specialized_symbol->type_information;

    type_t* result_from_conversion_type = 
        function_type_get_return_type(specialized_type);

    type_t* parameter_types[1] = { result_from_conversion_type };
    type_t* original_parameter_types[1] = { result_from_conversion_type };
    type_t* argument_types[1] = { destination_type };

    // Adjustments done to argument and parameter types
    //
    // We do not want referenced types here as arguments
    (*argument_types) = no_ref((*argument_types));

    // If P is not a reference type
    if (!is_any_reference_type((*parameter_types)))
    {
        // if A is an array type the pointer type produced by the array to pointer conversion
        // is used in place of A
        if (is_array_type((*argument_types)))
        {
            (*argument_types) = get_pointer_type(array_type_get_element_type((*argument_types)));
        }
        // otherwise, if A is a function type the pointer type produced by the array to pointer conversion
        // is used in place of A
        else if (is_function_type((*argument_types)))
        {
            (*argument_types) = get_pointer_type((*argument_types));
        }
        // otherwise, if A is a cv-qualified type, top-level cv qualification for A is ignored for type deduction
        else
        {
            (*argument_types) = get_unqualified_type((*argument_types));
        }
    }

    if (is_any_reference_type((*parameter_types)))
    {
        (*parameter_types) = reference_type_get_referenced_type((*parameter_types));
    }

    (*parameter_types) = get_unqualified_type((*parameter_types));

    // Deduce template arguments
    if (!deduce_template_arguments_common(template_parameters,
                type_template_parameters,
                argument_types, /* relevant arguments */ 1,
                parameter_types, /* relevant parameters */ 1,
                original_parameter_types,
                decl_context,
                deduced_template_arguments, locus,
                /* explicit_template_parameters */ NULL,
                /* is_function_call */ 0))
    {
        return 0;
    }

    // Now check that the updated types match exactly or can be converted
    // according to the standard for the case of function calls
    decl_context_t updated_context = specialized_symbol->decl_context;
    updated_context.template_parameters = *deduced_template_arguments;

    type_t* original_parameter_type = (*parameter_types);
    type_t* updated_type = 
        update_type(original_parameter_type, 
                updated_context,
                locus);

    if (!equivalent_types((*argument_types), updated_type))
    {
        // We have to check several things
        type_t* original_parameter = function_type_get_return_type(specialized_type);

        char ok = 0;
        if (is_any_reference_type(original_parameter)
                && is_more_or_equal_cv_qualified_type(reference_type_get_referenced_type(updated_type),
                    (*argument_types)))
        {
            ok = 1;
        }
        else if (is_pointer_type((*argument_types))
                && is_pointer_type(updated_type))
        {
            standard_conversion_t standard_conversion;
            if (standard_conversion_between_types(&standard_conversion, (*argument_types), updated_type, locus))
            {
                ok = 1;
            }
        }

        if (!ok)
        {
            return 0;
        }
    }

    return 1;
}

char deduce_arguments_from_call_to_specific_template_function(
        type_t** call_argument_types,
        int num_arguments, type_t* specialized_named_type, 
        template_parameter_list_t* template_parameters, 
        template_parameter_list_t* type_template_parameters, 
        decl_context_t decl_context,
        template_parameter_list_t **deduced_template_arguments, 
        const locus_t* locus,
        template_parameter_list_t* explicit_template_parameters)
{
    scope_entry_t* specialized_symbol = named_type_get_symbol(specialized_named_type);

    ERROR_CONDITION(specialized_symbol->kind != SK_FUNCTION, 
            "This is not a template specialized function", 0);

    type_t* specialized_type = specialized_symbol->type_information;

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Deducing template parameters using arguments of call\n");
        fprintf(stderr, "TYPEDEDUC: Called function : '%s'\n", print_declarator(specialized_type));

        fprintf(stderr, "TYPEDEDUC: Number of arguments: %d\n", num_arguments);
        int i;
        for (i = 0; i < num_arguments; i++)
        {
            fprintf(stderr, "TYPEDEDUC:    Argument %d: Type: '%s'\n", i,
                    print_declarator(call_argument_types[i]));
        }
    }

    int num_parameters = function_type_get_num_parameters(specialized_type);
    if (function_type_get_has_ellipsis(specialized_type))
        num_parameters--;

    int relevant_arguments = num_arguments;
    int relevant_parameters = num_parameters;

    // This one will keep P's adjusted
    type_t** adjusted_parameter_types = counted_xcalloc(relevant_parameters,
            sizeof(*adjusted_parameter_types), &_bytes_typededuc);
    // This one will keep original types but will be modified if there are explicit template arguments
    // These are modified in deduce_template_arguments_common
    type_t** original_parameter_types = counted_xcalloc(relevant_parameters,
            sizeof(*original_parameter_types), &_bytes_typededuc);
    // This one keeps A's adjusted
    type_t** adjusted_argument_types = counted_xcalloc(relevant_arguments,
            sizeof(*adjusted_argument_types), &_bytes_typededuc);

    int i_arg = 0;
    int i_param = 0;
    // Some changes must be introduced to the types
    while (i_arg < relevant_arguments
            && i_param < relevant_parameters)
    {
        type_t* original_parameter_type =
            function_type_get_parameter_type_num(specialized_type, i_param);
        type_t* current_parameter_type = original_parameter_type;
        type_t* current_argument_type =
            call_argument_types[i_arg];

        if (is_pack_type(current_parameter_type))
        {
            if ((i_param + 1) == num_parameters)
            {
                current_parameter_type = pack_type_get_packed_type(current_parameter_type);
            }
            else
            {
                // If it is not the last ignore this parameter pack
                // but keep the same argument
                i_param++;
                continue;
            }
        }

        // We do not want referenced types here as arguments
        current_argument_type = no_ref(current_argument_type);

        // If P is not a reference type
        if (!is_any_reference_type(current_parameter_type))
        {
            // if A is an array type the pointer type produced by the array to pointer conversion
            // is used in place of A
            if (is_array_type(current_argument_type))
            {
                current_argument_type = get_pointer_type(array_type_get_element_type(current_argument_type));
            }
            // otherwise, if A is a function type the pointer type produced by the array to pointer conversion
            // is used in place of A
            else if (is_function_type(current_argument_type)
                    || (is_unresolved_overloaded_type(current_argument_type)
                        && !is_pointer_to_member_type(current_parameter_type)))
            {
                // unresolved overloaded type always represents a reference to any of its functions 
                // but only if the original parameter is not already a pointer to member type 
                // because it is not possible to have a lvalue of a member function (only
                // lvalue of _pointer to member function_, while we can have a lvalue of
                // a nonstatic member or nonmember function)
                //
                // template <typename _Ret, typename _Class, typename _P1>
                // void f(_Ret (_Class::* T)(_P1));
                //
                // struct A
                // {
                //    float g(int);
                // };
                //
                // void h()
                // {
                //    f(&A::g);
                // }
                //
                // Parameter 'T' is not a reference, but a pointer to member
                // function, and '&A::g' has computed type unresolved, in
                // this case, do not convert the whole thing into a pointer
                // while we would do in the following one
                //
                // template <typename _Ret, typename _P1>
                // void f(_Ret (*T)(_P1));
                //
                // float g(int);
                //
                // void h()
                // {
                //    f(g);
                // }
                //
                // Because 'T' is not a reference (nor a pointer to member
                // function) and 'g' is an unresolved type we will convert it
                // into a pointer type
                //

                if (is_function_type(current_argument_type))
                {
                    current_argument_type = get_pointer_type(current_argument_type);
                }
                else if (is_unresolved_overloaded_type(current_argument_type))
                {
                    // Simplify an unresolved overload of singleton, if possible
                    scope_entry_t* solved_function = unresolved_overloaded_type_simplify(current_argument_type,
                            decl_context, locus);

                    if (solved_function != NULL
                            && (!solved_function->entity_specs.is_member
                                || solved_function->entity_specs.is_static))
                    {
                        current_argument_type = get_pointer_type(solved_function->type_information);
                    }
                }
            }
            // otherwise, if A is a cv-qualified type, top-level cv qualification for A is ignored for type deduction
            else
            {
                current_argument_type = get_unqualified_type(current_argument_type);
            }
        }

        original_parameter_types[i_param] = current_parameter_type;

        // If P is a qualified type the top level cv-qualifiers of P are ignored for type deduction
        current_parameter_type = get_unqualified_type(current_parameter_type);
        if (is_any_reference_type(current_parameter_type))
        {
            // If P is a reference type the type referred to by P is used for type deducton
            current_parameter_type = reference_type_get_referenced_type(current_parameter_type);
        }

        if (is_rvalue_reference_type(original_parameter_type)
                && get_cv_qualifier(no_ref(original_parameter_type)) == CV_NONE
                && is_named_type(no_ref(original_parameter_type))
                && named_type_get_symbol(no_ref(original_parameter_type))->kind == SK_TEMPLATE_TYPE_PARAMETER
                && is_lvalue_reference_type(call_argument_types[i_arg]))
        {
            // If P is an rvalue reference to a cv-unqualified template parameter and the
            // argument is an lvalue, the type “lvalue reference to A” is used in place of
            // A for type deduction.
            current_argument_type = get_lvalue_reference_type(current_argument_type);
        }

        adjusted_parameter_types[i_param] = current_parameter_type;
        adjusted_argument_types[i_arg] = current_argument_type;

        i_arg++;
        if (!is_pack_type(original_parameter_type))
        {
            i_param++;
        }
        else
        {
            // Once we have modified the expanded type, convert it back to an
            // expansion type otherwise we would lose track of it
            adjusted_parameter_types[i_param] = get_pack_type(adjusted_parameter_types[i_param]);
            original_parameter_types[i_param] = get_pack_type(original_parameter_types[i_param]);
        }
    }

    // This function modifies adjusted_parameter_types using the
    // explicit_template_parameters
    if (!deduce_template_arguments_common(template_parameters,
                type_template_parameters,
                adjusted_argument_types, relevant_arguments,
                adjusted_parameter_types, relevant_parameters,
                original_parameter_types,
                specialized_symbol->decl_context,
                deduced_template_arguments,
                locus,
                explicit_template_parameters,
                /* is_function_call */ 1))
    {
        xfree(adjusted_argument_types);
        xfree(adjusted_parameter_types);
        xfree(original_parameter_types);

        return 0;
    }

    // Now check that the updated types match exactly or can be converted
    // accordingly to the standard for the case of function calls
    decl_context_t updated_context = specialized_symbol->decl_context;
    updated_context.template_parameters = *deduced_template_arguments;

    i_arg = 0;
    i_param = 0;

    while (i_arg < relevant_arguments
            && i_param < relevant_parameters)
    {
        type_t* original_parameter_type = original_parameter_types[i_param];
        type_t* adjusted_parameter_type = adjusted_parameter_types[i_param];

        int number_of_args = 1;

        if (is_pack_type(original_parameter_type))
        {
            if ((i_param + 1) == num_parameters)
            {
                number_of_args = relevant_arguments - i_arg;
            }
            else
            {
                // If it is not the last ignore this parameter pack
                // but keep the same argument
                i_param++;
                continue;
            }
        }

        diagnostic_context_push_buffered();
        type_t* deduced_argument_type =
            update_type(adjusted_parameter_type,
                    updated_context,
                    locus);
        diagnostic_context_pop_and_discard();

        if (deduced_argument_type == NULL)
        {
            xfree(adjusted_argument_types);
            xfree(adjusted_parameter_types);
            xfree(original_parameter_types);

            return 0;
        }

        int current_arg = 0;
        for (current_arg = i_arg; current_arg < (i_arg + number_of_args); current_arg++)
        {
            type_t* argument_type = adjusted_argument_types[current_arg];

            type_t* current_deduced_argument_type = deduced_argument_type;
            type_t* current_original_parameter_type = original_parameter_type;
            type_t* current_adjusted_parameter_type = adjusted_parameter_type;

            if (is_sequence_of_types(current_deduced_argument_type))
            {
                // Unpack this type
                current_deduced_argument_type =
                    sequence_of_types_get_type_num(current_deduced_argument_type,
                            current_arg - i_arg);
            }

            if (is_pack_type(current_original_parameter_type))
            {
                current_original_parameter_type = pack_type_get_packed_type(current_original_parameter_type);
            }
            if (is_pack_type(current_adjusted_parameter_type))
            {
                current_adjusted_parameter_type = pack_type_get_packed_type(current_adjusted_parameter_type);
            }

             if (is_unresolved_overloaded_type(argument_type)
                     || (is_pointer_type(argument_type)
                         && is_unresolved_overloaded_type(
                             pointer_type_get_pointee_type(argument_type)))
                )
             {
                 type_t* unresolved_type = argument_type;

                 if (is_pointer_type(argument_type))
                     unresolved_type = pointer_type_get_pointee_type(argument_type);

                 scope_entry_list_t* unresolved_set =
                     unresolved_overloaded_type_get_overload_set(unresolved_type);

                 scope_entry_t* solved_function = address_of_overloaded_function(
                         unresolved_set,
                         unresolved_overloaded_type_get_explicit_template_arguments(unresolved_type),
                         current_deduced_argument_type,
                         updated_context,
                         locus);
                 entry_list_free(unresolved_set);

                 if (solved_function != NULL)
                 {
                     // Some adjustment goes here so the equivalent_types check below works.
                     // We mimic the adjustments performed before
                     //
                     if (!is_any_reference_type(current_original_parameter_type))
                     {
                         // If it is not a reference convert from function to pointer
                         if (!solved_function->entity_specs.is_member
                                 || solved_function->entity_specs.is_static)
                         {
                             argument_type = get_pointer_type(solved_function->type_information);
                         }
                         else
                         {
                             argument_type = get_pointer_to_member_type(solved_function->type_information,
                                     solved_function->entity_specs.class_type);
                         }
                     }
                     else
                     {
                         // Otherwise keep exactly the type
                         argument_type = solved_function->type_information;
                     }
                 }
             }

            if (!deduction_of_argument_type_may_succeed(
                        /* deduced_argument_type */ current_deduced_argument_type,
                        /* argument_type */ argument_type,
                        /* adjusted_parameter_type */ current_adjusted_parameter_type,
                        /* original_parameter_type */ current_original_parameter_type,
                        decl_context,
                        locus))
            {
                xfree(adjusted_argument_types);
                xfree(adjusted_parameter_types);
                xfree(original_parameter_types);

                return 0;
            }
        }

        i_arg += number_of_args;
        if (!is_pack_type(original_parameter_type))
        {
            i_param++;
        }
    }

    xfree(adjusted_argument_types);
    xfree(adjusted_parameter_types);
    xfree(original_parameter_types);

    return 1;
}


char deduce_arguments_of_auto_initialization(
        type_t* destination_type,
        type_t* initializer_type,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        char is_braced_array,
        const locus_t* locus)
{

    // Fake type template parameter
    scope_entry_t* fake_template_parameter_symbol = xcalloc(1, sizeof(*fake_template_parameter_symbol));
    fake_template_parameter_symbol->symbol_name = UNIQUESTR_LITERAL("FakeTypeTemplateParameter");
    fake_template_parameter_symbol->kind = SK_TEMPLATE_TYPE_PARAMETER;
    fake_template_parameter_symbol->entity_specs.is_template_parameter = 1;
    fake_template_parameter_symbol->locus = locus;
    fake_template_parameter_symbol->entity_specs.template_parameter_nesting = 1;
    fake_template_parameter_symbol->entity_specs.template_parameter_position = 0;

    // Fake template parameter list
    template_parameter_list_t *fake_template_parameter_list = xcalloc(1, sizeof(*fake_template_parameter_list));
    template_parameter_t* fake_template_parameter = xcalloc(1, sizeof(*fake_template_parameter));
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
        template_parameter_value_t* fake_template_argument = xcalloc(1, sizeof(*fake_template_argument));
        fake_template_argument->kind = TPK_TYPE;
        fake_template_argument->type = get_user_defined_type(fake_template_parameter_symbol);
        fake_template_argument_list->arguments[0] = fake_template_argument;

        // Now get a silly specialization using our fake template parameter
        type_in_place_of_auto = template_type_get_specialized_type(std_initializer_list_template->type_information,
                fake_template_argument_list,
                decl_context,
                locus);
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

    return deduce_arguments_from_call_to_specific_template_function(
            &initializer_type, 1,
            primary_specialization,
            fake_template_parameter_list,
            fake_template_parameter_list,
            decl_context,
            deduced_template_arguments,
            locus,
            NULL);
}

static template_parameter_list_t* build_template_parameter_list_from_deduction_set(
        template_parameter_list_t* template_parameters,
        deduction_set_t* deduction_set)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEDEDUC: Creating template argument list after deduction set\n");
    }
    template_parameter_list_t* result = duplicate_template_argument_list(template_parameters);

    int nesting = get_template_nesting_of_template_parameters(template_parameters);

    int i;
    for (i = 0; i < deduction_set->num_deductions; i++)
    {
        deduction_t* current_deduction = deduction_set->deduction_list[i];

        // Skip empty deductions
        if (current_deduction->num_deduced_parameters == 0)
            continue;

        ERROR_CONDITION(current_deduction->num_deduced_parameters != 1,
                "Bad deduction num_deduced_parameters != 1 (%d)",
                current_deduction->num_deduced_parameters);

        template_parameter_value_t* argument = counted_xcalloc(1, sizeof(*argument), &_bytes_typededuc);

        switch (current_deduction->kind)
        {
            case TPK_UNKNOWN:
                {
                    // This will have to be filled later
                    break;
                }
            case TPK_TYPE:
            case TPK_TYPE_PACK:
            case TPK_TEMPLATE:
            case TPK_TEMPLATE_PACK:
                {
                    argument->kind = template_parameter_kind_get_base_kind(current_deduction->kind);
                    argument->type = current_deduction->deduced_parameters[0]->type;
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
                    argument->type = current_deduction->deduced_parameters[0]->type;
                    argument->value = current_deduction->deduced_parameters[0]->value;

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
