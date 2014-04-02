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




#include <stdio.h>
#include <string.h>
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"
#include "cxx-typededuc.h"
#include "cxx-exprtype.h"
#include "cxx-cexpr.h"
#include "cxx-buildscope.h"
#include "cxx-scope.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-solvetemplate.h"
#include "cxx-prettyprint.h"
#include "cxx-instantiation.h"
#include "cxx-entrylist.h"
#include "cxx-codegen.h"

unsigned long long int _bytes_typeunif = 0;

long long int typeunif_used_memory(void)
{
    return _bytes_typeunif;
}

typedef
enum tribool_tag
{
    DEFINITELY_FALSE = 0,
    DEFINITELY_TRUE = 1,
    NOT_SURE = 2
} tribool_t;

static deduction_t* get_unification_item(
        deduction_set_t** deduction_set,
        int position, int nesting,
        enum template_parameter_kind kind,
        const char* name)
{
    int i;
    for (i = 0; i < (*deduction_set)->num_deductions; i++)
    {
        if ((*deduction_set)->deduction_list[i]->parameter_position == position
                && (*deduction_set)->deduction_list[i]->parameter_nesting == nesting)
        {
            return (*deduction_set)->deduction_list[i];
        }
    }

    deduction_t* result = counted_xcalloc(1, sizeof(*result), &_bytes_typeunif);
    result->kind = kind;
    result->parameter_position = position;
    result->parameter_nesting = nesting;
    result->parameter_name = name;

    P_LIST_ADD((*deduction_set)->deduction_list, (*deduction_set)->num_deductions, result);

    return result;
}

static deduction_t* get_unification_item_from_item(
        deduction_set_t** deduction_set,
        deduction_t* deduction)
{
    return get_unification_item(deduction_set,
            deduction->parameter_position,
            deduction->parameter_nesting,
            deduction->kind,
            deduction->parameter_name);
}

deduction_t* get_unification_item_for_template_parameter(deduction_set_t** deduction_set, scope_entry_t* s1)
{
    ERROR_CONDITION(!s1->entity_specs.is_template_parameter,
            "This must be a template-parameter", 0);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Getting unification item for template parameter '%s'\n",
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


    return get_unification_item(deduction_set, position, nesting, kind, s1->symbol_name);
}


static char deduction_add_type_parameter_deduction(deduction_t* deduction,
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

        deduced_parameter_t* new_deduced_parameter = counted_xcalloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
        *new_deduced_parameter = *current_deduced_parameter;

        P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
    }

    return !found;
}

static char deduction_add_nontype_parameter_deduction(deduction_t* deduction,
        deduced_parameter_t* current_deduced_parameter,
        deduction_flags_t flags)
{
    nodecl_t current_deduced_value = current_deduced_parameter->value;
    scope_entry_t* current_deduced_symbol = nodecl_get_symbol(current_deduced_value);

    char found = 0;
    int i;
    for (i = 0; i < deduction->num_deduced_parameters; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Checking previous deductions\n");
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
                fprintf(stderr, "TYPEUNIF: Both previous deduction tree '%s' and new deduction tree '%s'"
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
                    fprintf(stderr, "TYPEUNIF: They are the same\n");
                }
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: They are different\n");
                }
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Checking if previous deduced tree '%s' and current deduced tree '%s'"
                        " are the same\n", 
                        codegen_to_str(previous_deduced_value, nodecl_retrieve_context(previous_deduced_value)),
                        codegen_to_str(current_deduced_value, nodecl_retrieve_context(current_deduced_value)));
            }

            if (same_functional_expression(previous_deduced_value, current_deduced_value, flags))
            {
                found = 1;
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: They are the same\n");
                }
            }
            else
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: They are different\n");
                }
            }
        }
    }

    if (!found)
    {
        deduced_parameter_t* new_deduced_parameter = counted_xcalloc(1, sizeof(*new_deduced_parameter), &_bytes_typeunif);
        *new_deduced_parameter = *current_deduced_parameter;

        P_LIST_ADD(deduction->deduced_parameters, deduction->num_deduced_parameters, new_deduced_parameter);
    }

    return !found;
}

static char equivalent_dependent_expressions(nodecl_t left_tree, 
        nodecl_t right_tree, 
        deduction_set_t** unif_set,
        deduction_flags_t flags);

static tribool_t equivalent_expression_trees(nodecl_t left_tree, nodecl_t right_tree);

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context, 
        const locus_t* locus, deduction_flags_t flags);

deduction_t* get_unification_item_for_template_parameter(deduction_set_t** deduction_set, scope_entry_t* s1);

static deduced_parameter_t* copy_deduced_parameter(deduced_parameter_t* deduced_parameter)
{
    deduced_parameter_t* result = xcalloc(1, sizeof(*result));
    *result = *deduced_parameter;
    result->value = nodecl_shallow_copy(deduced_parameter->value);

    return result;
}

static void free_deduced_parameter(deduced_parameter_t* deduced_parameter UNUSED_PARAMETER)
{
}

// Merges the second deduction set inside the first
static void merge_deduction_set(deduction_set_t** dest, deduction_set_t* source, deduction_flags_t flags)
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
        deduction_t* merged_deduction = get_unification_item_from_item(dest, current_deduction);

        switch (current_deduction->kind)
        {
            case TPK_TYPE_PACK:
            case TPK_TEMPLATE_PACK:
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
                                deduced_parameter_t* extended_deduced_parameter = copy_deduced_parameter(
                                        current_merged_deduced_parameter);

                                extended_deduced_parameter->type = get_sequence_of_types_append_type(
                                        extended_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);

                                char added = deduction_add_type_parameter_deduction(merged_deduction,
                                        extended_deduced_parameter);

                                if (!added)
                                {
                                    free_deduced_parameter(extended_deduced_parameter);
                                }
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
                            deduction_add_type_parameter_deduction(merged_deduction,
                                    current_deduction->deduced_parameters[k2]);
                        }
                    }
                    break;
                }
            case TPK_NONTYPE_PACK:
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
                                deduced_parameter_t* extended_deduced_parameter = copy_deduced_parameter(
                                        current_merged_deduced_parameter);

                                extended_deduced_parameter->value = nodecl_append_to_list(
                                        extended_deduced_parameter->value,
                                        current_deduction->deduced_parameters[k2]->value);
                                extended_deduced_parameter->type = get_sequence_of_types_append_type(
                                        extended_deduced_parameter->type,
                                        current_deduction->deduced_parameters[k2]->type);

                                char added = deduction_add_nontype_parameter_deduction(merged_deduction,
                                        extended_deduced_parameter, flags);

                                if (!added)
                                {
                                    free_deduced_parameter(extended_deduced_parameter);
                                }
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
                            deduction_add_nontype_parameter_deduction(merged_deduction,
                                    current_deduction->deduced_parameters[k2],
                                    flags);
                        }
                    }
                    break;
                }
            case TPK_TYPE:
            case TPK_TEMPLATE:
                {
                    int k;
                    for (k = 0; k < current_deduction->num_deduced_parameters; k++)
                    {
                        deduction_add_type_parameter_deduction(merged_deduction,
                                current_deduction->deduced_parameters[k]);
                    }
                    break;
                }
            case TPK_NONTYPE:
                {
                    int k;
                    for (k = 0; k < current_deduction->num_deduced_parameters; k++)
                    {
                        deduction_add_nontype_parameter_deduction(merged_deduction,
                                current_deduction->deduced_parameters[k], flags);
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

void unificate_two_types(type_t* t1,
        type_t* t2,
        deduction_set_t** deduction_set, 
        decl_context_t decl_context, const locus_t* locus,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Trying to unificate type '%s' <- '%s'\n",
                print_declarator(t1),
                print_declarator(t2));
    }

#define UNIFICATION_ENDED \
    DEBUG_CODE() \
    { \
        fprintf(stderr, "TYPEUNIF: Ended unification of types '%s' <- '%s'\n", \
                print_declarator(t1), \
                print_declarator(t2)); \
    } \


    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;
    t1 = advance_over_typedefs_with_cv_qualif(t1, &cv_qualif_1);
    t2 = advance_over_typedefs_with_cv_qualif(t2, &cv_qualif_2);

    if (is_unresolved_overloaded_type(t2))
    {
        // Special case for unresolved overloaded function types:
        //  - try all cases in the hope that any will match
        unificate_unresolved_overloaded(t1, t2, deduction_set, decl_context, locus, flags);
        // Nothing else must be done with this t2
        UNIFICATION_ENDED;
        return;
    }

    // No unification is ever possible for this type
    if (is_braced_list_type(t2))
    {
        // { 1, 2, 3 } can only be unificated with std::initializer_list<T>
        // Unification with a plain type template parameter is not possible
        scope_entry_t* std_initializer_list = get_std_initializer_list_template(decl_context, locus,
                /* mandatory */ 0);

        if (std_initializer_list == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Cannot unify braced initializer list because std::initializer_list has not been declared\n");
            }
            UNIFICATION_ENDED;
            return;
        }

        if (!is_named_class_type(t1)
                || !is_template_specialized_type(get_actual_class_type(t1))
                || template_specialized_type_get_related_template_type(
                    get_actual_class_type(t1)) != std_initializer_list->type_information)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Cannot unify braced initializer list to something that is not a specialization "
                        "of std::initializer_list<T>\n");
            }
            return;
        }

        // Now we know this is std::initializer_list<T1> <- {T2[0], T2[1], ... , }
        // so proceed to unificate T1 with each T2[i]
        template_parameter_list_t *targ_list_1
            = template_specialized_type_get_template_arguments(get_actual_class_type(t1));

        if (targ_list_1->num_parameters < 1
                || targ_list_1->arguments[0] == NULL
                || targ_list_1->arguments[0]->kind != TPK_TYPE)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEDEDUC: Cannot unify braced initializer list because "
                        "std::initializer_list template specialization is wrong\n");
            }
            UNIFICATION_ENDED;
            return;
        }

        int i, n = braced_list_type_get_num_types(t2);

        for (i = 0; i < n; i++)
        {
            unificate_two_types(targ_list_1->arguments[0]->type,
                    braced_list_type_get_type_num(t2, i),
                    deduction_set,
                    decl_context,
                    locus, flags);
        }

        UNIFICATION_ENDED;
        return;
    }

    // 1. If it is a template parameter (or a user defined type pointing to it)
    // then save a deduction
    if (is_named_type(t1) 
            && ((named_type_get_symbol(t1)->kind == SK_TEMPLATE_TYPE_PARAMETER)
                || (named_type_get_symbol(t1)->kind == SK_TEMPLATE_TYPE_PARAMETER_PACK)))
    {
        scope_entry_t* s1 = named_type_get_symbol(t1);
        if (is_sequence_of_types(t2)
                && s1->kind == SK_TEMPLATE_TYPE_PARAMETER)
        {
            if (sequence_of_types_get_num_types(t2) == 1)
            {
                // T <- {int}
                t2 = sequence_of_types_get_type_num(t2, 0);
            }
            else
            {
                // Cannot be unified
                UNIFICATION_ENDED;
                return;
            }
        }

        if (named_type_get_symbol(t1)->kind == SK_TEMPLATE_TYPE_PARAMETER
                && is_pack_type(t2))
        {
            // Cannot unify a type template parameter with a template pack
            UNIFICATION_ENDED;
            return;
        }

        deduction_t* deduction = get_unification_item_for_template_parameter(
                deduction_set, s1);

        deduced_parameter_t current_deduced_parameter;
        memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

        current_deduced_parameter.type = get_cv_qualified_type(t2, cv_qualif_2 & (~cv_qualif_1));

        char added = deduction_add_type_parameter_deduction(deduction, &current_deduced_parameter);
        DEBUG_CODE()
        {
            if (added)
            {
                fprintf(stderr, "TYPEUNIF: Type deduction for type template parameter (%s) (%d,%d) with value '%s' \n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position,
                        print_declarator(current_deduced_parameter.type));
            }
        }
    }
    // template template parameters are handled a bit different
    else if (is_named_type(t1)
            && ((named_type_get_symbol(t1)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                || (named_type_get_symbol(t1)->kind == SK_TEMPLATE_TEMPLATE_PARAMETER_PACK)))
    {
        // Only other template-template parameters are valid here or template-names of
        // classes
        if (!is_named_type(t2)
                || (named_type_get_symbol(t2)->kind != SK_TEMPLATE
                    && named_type_get_symbol(t2)->kind != SK_TEMPLATE_TEMPLATE_PARAMETER))
        {
            // Nothing else to do
            UNIFICATION_ENDED;
            return;
        }

        scope_entry_t* s1 = named_type_get_symbol(t1);

        deduction_t* deduction = get_unification_item_for_template_parameter(
                deduction_set, s1);

        deduced_parameter_t current_deduced_parameter;
        memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

        current_deduced_parameter.type = t2;

        char added = deduction_add_type_parameter_deduction(deduction, &current_deduced_parameter);

        DEBUG_CODE()
        {
            if (added)
            {
                fprintf(stderr, "TYPEUNIF: Type deduction for template template parameter (%s) (%d,%d) with value '%s' \n", 
                        s1->symbol_name,
                        s1->entity_specs.template_parameter_nesting,
                        s1->entity_specs.template_parameter_position,
                        print_declarator(current_deduced_parameter.type));
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Unificating template template parameter\n");
        }

    }
    // Structural unification, one of them must succeed
    else if (is_non_derived_type(t1)
            && is_non_derived_type(t2))
    {
        // Unificate template types
        //
        // A<_T> <- A<int>
        //
        // or
        //
        // _V<_T> <- A<int>
        //
        if (is_named_class_type(t1)
                && is_named_class_type(t2)
                && is_template_specialized_type(get_actual_class_type(t1))
                && is_template_specialized_type(get_actual_class_type(t2))
                && ((template_specialized_type_get_related_template_type(get_actual_class_type(t1)) 
                        == template_specialized_type_get_related_template_type(get_actual_class_type(t2)))
                    // Allow t1 to be a template-template parameter
                    || (template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(
                                get_actual_class_type(t1)))->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
                    )
           )
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Class type is a specialized one, unificating arguments\n");
            }

            scope_entry_t* t1_related_symbol =
                template_type_get_related_symbol(
                        template_specialized_type_get_related_template_type(
                            get_actual_class_type(t1)));

            // If t1 is a template-template parameter it means that t1 can be
            // unified to the template type of t2
            if (t1_related_symbol != NULL
                    && t1_related_symbol->kind == SK_TEMPLATE_TEMPLATE_PARAMETER)
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: This is a template-id built after a template-template parameter\n");
                }

                // Now we request an unification item for this template-parameter
                deduction_t* deduction = get_unification_item_for_template_parameter(
                        deduction_set, t1_related_symbol);

                deduced_parameter_t current_deduced_parameter;
                memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

                scope_entry_t* t2_related_symbol = 
                    template_type_get_related_symbol(
                            template_specialized_type_get_related_template_type(
                                get_actual_class_type(t2)));

                current_deduced_parameter.type = get_user_defined_type(t2_related_symbol);

                char added = deduction_add_type_parameter_deduction(deduction, &current_deduced_parameter);
                DEBUG_CODE()
                {
                    if (added) 
                    {

                        fprintf(stderr, "TYPEUNIF: Type deduction for template template parameter (%s) (%d,%d) with value '%s' \n", 
                                t1_related_symbol->symbol_name,
                                t1_related_symbol->entity_specs.template_parameter_nesting,
                                t1_related_symbol->entity_specs.template_parameter_position,
                                print_declarator(current_deduced_parameter.type));
                    }

                    fprintf(stderr, "TYPEUNIF: Unificating template template parameter against template-name in a template-id\n");
                }
            }

            template_parameter_list_t *targ_list_1 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t1));
            template_parameter_list_t *targ_list_2 
                = template_specialized_type_get_template_arguments(get_actual_class_type(t2));

            char template_args_are_deduced = 1;

            int i;
            for (i = 0; i < targ_list_1->num_parameters; i++)
            {
                template_parameter_value_t* current_arg_1 = targ_list_1->arguments[i];
                if (template_argument_is_pack(current_arg_1)
                        && (i+1) != targ_list_1->num_parameters)
                {
                    template_args_are_deduced = 0;
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: Template arguments will not be deduced because we found a template pack not at the last position\n");
                    }
                    break;
                }
            }

            if (template_args_are_deduced)
            {
                for (i = 0;
                        i < targ_list_1->num_parameters
                        && i < targ_list_2->num_parameters;
                        i++)
                {
                    template_parameter_value_t* current_arg_1 = targ_list_1->arguments[i];
                    template_parameter_value_t* current_arg_2 = targ_list_2->arguments[i];

                    switch (current_arg_1->kind)
                    {
                        case TPK_TYPE:
                        case TPK_TEMPLATE:
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: Unificating template/type-template argument %d\n", i);
                                }

                                // If we are unificating T... <- S... and S is
                                // the last pack, do not unificate T... <- {S...}
                                if (is_pack_type(current_arg_1->type))
                                {
                                    int num_t2_types = 0;
                                    int k;
                                    for (k = i; k < targ_list_2->num_parameters; k++)
                                    {
                                        if (is_sequence_of_types(targ_list_2->arguments[k]->type))
                                        {
                                            num_t2_types += sequence_of_types_get_num_types(targ_list_2->arguments[k]->type);
                                        }
                                        else
                                        {
                                            num_t2_types++;
                                        }
                                    }

                                    type_t* types[num_t2_types + 1];
                                    int type_idx = 0;
                                    for (k = i; k < targ_list_2->num_parameters; k++)
                                    {
                                        if (is_sequence_of_types(targ_list_2->arguments[k]->type))
                                        {
                                            int z, num_seq_types = sequence_of_types_get_num_types(targ_list_2->arguments[k]->type);
                                            for (z = 0; z < num_seq_types; z++)
                                            {
                                                types[type_idx] = sequence_of_types_get_type_num(
                                                        targ_list_2->arguments[k]->type,
                                                        z);
                                                type_idx++;
                                            }
                                        }
                                        else
                                        {
                                            types[type_idx] = targ_list_2->arguments[k]->type;
                                            type_idx++;
                                        }
                                    }

                                    unificate_two_types(current_arg_1->type,
                                            get_sequence_of_types(num_t2_types, types),
                                            deduction_set,
                                            decl_context,
                                            locus,
                                            flags);
                                }
                                else
                                {
                                    unificate_two_types(current_arg_1->type,
                                            current_arg_2->type,
                                            deduction_set, decl_context, locus, flags);
                                }
                            }
                            break;
                        case TPK_NONTYPE:
                            {
                                DEBUG_CODE()
                                {
                                    fprintf(stderr, "TYPEUNIF: Unificating nontype-template argument %d\n",
                                            i);
                                }
                                unificate_two_expressions(deduction_set, 
                                        current_arg_1->value,
                                        current_arg_2->value,
                                        flags);
                            }
                            break;
                        default:
                            {
                                internal_error("Invalid template argument kind", 0);
                            }
                    }
                }
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Arguments unificated\n");
                }
            }
            // Nothing else to do
            UNIFICATION_ENDED;
            return;
        }

        // Special case where we unificate with all the bases of a given class
        if (!flags.do_not_allow_conversions
                && is_named_class_type(t1)
                // t1 is a template-id
                && is_template_specialized_type(get_actual_class_type(t1))
                && is_named_class_type(t2)
                && !is_dependent_type(t2))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Unificating against bases\n");
            }

            /*
             * Here we have to instantiate t2 but only if it is an incomplete independent class.
             *
             * Note that this code is invalid (obviously)
             *
             *    template <typename _T>
             *    struct A 
             *    { 
             *    };
             *    
             *    template <typename _T>
             *    struct B;
             *    
             *    template <typename _T>
             *    void f(A<_T>*);
             *    
             *    void g(void)
             *    {
             *        B<float> *b;
             *    
             *        f(b);
             *    }
             *
             * while the following is valid
             *
             *    template <typename _T>
             *    struct A 
             *    { 
             *    };
             *    
             *    template <typename _T>
             *    struct B : A<_T> { };
             *    
             *    template <typename _T>
             *    void f(A<_T>*);
             *    
             *    void g(void)
             *    {
             *        B<float> *b;
             *    
             *        f(b);  // will call f(A<int>*)
             *    }
             */

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Instantiating class '%s' since we will try to unificate against bases\n",
                        print_declarator(t2));
            }

            instantiate_template_class_if_possible(named_type_get_symbol(t2), decl_context, locus);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Class '%s' instantiated, now we can proceed to check bases\n",
                        print_declarator(t2));
            }

            scope_entry_list_t* all_bases = class_type_get_all_bases(get_actual_class_type(t2), /* include_dependent */ 0);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(all_bases);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                unificate_two_types(t1, get_user_defined_type(entry), deduction_set, decl_context, locus, flags);
            }
            entry_list_iterator_free(it);
            entry_list_free(all_bases);

            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Bases unificated\n");
            }
        }
    }
    else if (is_sequence_of_types(t1)
            && is_sequence_of_types(t2))
    {
        int i, n1 = sequence_of_types_get_num_types(t1), n2 = sequence_of_types_get_num_types(t2);
        for (i = 0; i < n1 && i < n2; i++)
        {
            type_t* item_t1 = sequence_of_types_get_type_num(t1, i);

            if (is_pack_type(item_t1))
            {
                if (i == (n1 - 1))
                {
                    // Craft a sequence type with the remaining types (if any)
                    type_t* remaining_sequence = get_sequence_of_types(0, NULL);
                    int j;
                    for (j = 0; j < n2; j++)
                    {
                        remaining_sequence = get_sequence_of_types_append_type(remaining_sequence,
                                sequence_of_types_get_type_num(t2, j));
                    }

                    unificate_two_types(
                            item_t1,
                            remaining_sequence,
                            deduction_set,
                            decl_context,
                            locus, flags);
                }
                else
                {
                    // We cannot deduce this pack here
                }
            }
            else
            {
                unificate_two_types(
                        item_t1,
                        sequence_of_types_get_type_num(t2, i),
                        deduction_set,
                        decl_context,
                        locus, flags);
            }
        }
    }
    else if (is_pack_type(t1))
    {
        type_t* packed_type = pack_type_get_packed_type(t1);
        if (is_sequence_of_types(t2))
        {
            int num_t2_types = sequence_of_types_get_num_types(t2);

            int i;
            for (i = 0; i < num_t2_types; i++)
            {
                deduction_set_t *current_deduction_set = counted_xcalloc(1, sizeof(*current_deduction_set), &_bytes_typeunif);

                unificate_two_types(
                        packed_type,
                        sequence_of_types_get_type_num(t2, i),
                        &current_deduction_set, decl_context, locus, flags);

                merge_deduction_set(deduction_set, current_deduction_set, flags);
            }
        }
        else if (is_pack_type(t2))
        {
            unificate_two_types(packed_type,
                    pack_type_get_packed_type(t2),
                    deduction_set, decl_context, locus, flags);
        }
        else
        {
            UNIFICATION_ENDED;
            return;
        }
    }
    else if (is_lvalue_reference_type(t1)
            && is_lvalue_reference_type(t2))
    {
        unificate_two_types(reference_type_get_referenced_type(t1), reference_type_get_referenced_type(t2), deduction_set,
                decl_context, locus, flags);
    }
    else if (is_rvalue_reference_type(t1)
            && is_rvalue_reference_type(t2))
    {
        unificate_two_types(reference_type_get_referenced_type(t1), reference_type_get_referenced_type(t2), deduction_set,
                decl_context, locus, flags);
    }
    else if (is_pointer_type(t1)
            && is_pointer_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, locus, flags);
    }
    else if (is_pointer_to_member_type(t1)
            && is_pointer_to_member_type(t2))
    {
        unificate_two_types(pointer_type_get_pointee_type(t1), pointer_type_get_pointee_type(t2), deduction_set,
                decl_context, locus, flags);
        unificate_two_types(pointer_to_member_type_get_class_type(t1),
                pointer_to_member_type_get_class_type(t2), deduction_set, decl_context, locus,
                flags);
    }
    else if (is_array_type(t1)
            && is_array_type(t2))
    {

        if (!nodecl_is_null(array_type_get_array_size_expr(t1))
                && !nodecl_is_null(array_type_get_array_size_expr(t2)))
        {
            unificate_two_expressions(deduction_set, 
                    array_type_get_array_size_expr(t1),
                    array_type_get_array_size_expr(t2),
                    flags);
        }

        unificate_two_types(array_type_get_element_type(t1),
                array_type_get_element_type(t2),
                deduction_set,
                decl_context,
                locus, flags);
    }
    else if (is_function_type(t1)
            && is_function_type(t2))
    {
        unificate_two_types(function_type_get_return_type(t1), 
                function_type_get_return_type(t2), 
                deduction_set, 
                decl_context,
                locus, flags);

        int num_parameters = function_type_get_num_parameters(t1);
        if (function_type_get_has_ellipsis(t1))
            num_parameters--;

        int num_arguments = function_type_get_num_parameters(t2);
        if (function_type_get_has_ellipsis(t2))
            num_arguments--;

        int i_param = 0;
        int i_arg = 0;
        while (i_param < num_parameters
                && i_arg < num_arguments)
        {
            // Fix this should ignore outermost cv qualifier
            type_t* par = function_type_get_parameter_type_num(t1, i_param);

            if (is_pack_type(par))
            {
                int remaining_args = num_arguments - i_arg;
                type_t* arg_types[remaining_args];
                int j;
                for (j = 0; j < remaining_args; j++)
                    arg_types[j] = function_type_get_parameter_type_num(t2, i_arg + j);

                type_t* seq_type = get_sequence_of_types(remaining_args, arg_types);

                unificate_two_types(par, seq_type, deduction_set, decl_context, locus, flags);
                // All arguments have been unified now
                i_arg = num_arguments;
            }
            else
            {
                type_t* arg = function_type_get_parameter_type_num(t2, i_arg);
                unificate_two_types(par, arg, deduction_set,
                        decl_context, locus, flags);
                i_param++;
                i_arg++;
            }
        }
    }
    UNIFICATION_ENDED;
}

void unificate_two_expressions(deduction_set_t **deduction_set, 
        nodecl_t left_tree, 
        nodecl_t right_tree, 
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Attempting to unificate expression '%s' <- '%s'\n",
                codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
    }

    equivalent_dependent_expressions(left_tree, right_tree,
            deduction_set, flags);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of expression '%s' <- '%s' ended\n",
                codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
    }
}


static char equivalent_nodecl_expressions(nodecl_t left_tree,
        nodecl_t right_tree,
        deduction_set_t** unif_set,
        deduction_flags_t flags);

static char equivalent_dependent_expressions(nodecl_t left_tree,
        nodecl_t right_tree,
        deduction_set_t** unif_set,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether %s and %s are equivalent\n",
                codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
    }
    if (nodecl_expr_is_value_dependent(left_tree)
            || nodecl_expr_is_value_dependent(right_tree))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Some of the expressions is dependent, falling back to structural equivalence\n");
        }
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Trying to match %s and %s by means of plain constant evaluation\n",
                    codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                    codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
        }
        tribool_t tribool = equivalent_expression_trees(left_tree, right_tree);
        if (tribool == NOT_SURE)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Plain evaluation was not possible. Trying structural equivalence\n");
            }
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Constant values comparison %s. They have the %s values\n",
                        tribool ? "succeeded" : "failed",
                        tribool ? "equivalent" : "different");
            }
            return tribool;
        }
    }

    scope_entry_t* left_symbol = nodecl_get_symbol(left_tree);
    scope_entry_t* right_symbol = nodecl_get_symbol(right_tree);

    if (left_symbol != NULL
            && right_symbol != NULL)
    {
        if (left_symbol == right_symbol)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Left tree '%s' and right tree '%s'"
                        " are the same symbol. They are trivially equivalent\n", 
                        codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                        codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
            }
            return 1;
        }
        else if (left_symbol->kind == SK_DEPENDENT_ENTITY
                && right_symbol->kind == SK_DEPENDENT_ENTITY)
        {
            return equivalent_types(left_symbol->type_information, right_symbol->type_information);
        }
    }

    if (right_symbol != NULL)
    {
        // Advance right value except for enumerators or a function parameter name
        if (right_symbol->kind != SK_ENUMERATOR
                && !symbol_is_parameter_of_function(right_symbol, get_function_declaration_proxy())
                && !nodecl_is_null(right_symbol->value))
            return equivalent_dependent_expressions(left_tree, right_symbol->value, unif_set, flags);
    }

    if (left_symbol != NULL)
    {
        // Advance left value only if it is not a nontype template parameter or a function parameter name
        if (left_symbol->kind != SK_TEMPLATE_NONTYPE_PARAMETER 
                && left_symbol->kind != SK_TEMPLATE_NONTYPE_PARAMETER_PACK
                && !symbol_is_parameter_of_function(left_symbol, get_function_declaration_proxy())
                && !nodecl_is_null(left_symbol->value))
            return equivalent_dependent_expressions(left_symbol->value, right_tree, unif_set, flags);

        // Try to unify using this template parameter
        if ((left_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
                || (left_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "TYPEUNIF: Left part '%s' found to be a nontype template parameter\n", 
                        codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)));
            }
            deduction_t* deduction = get_unification_item_for_template_parameter(unif_set,
                    left_symbol);

            deduced_parameter_t current_deduced_parameter;
            memset(&current_deduced_parameter, 0, sizeof(current_deduced_parameter));

            current_deduced_parameter.value = right_tree;
            current_deduced_parameter.type = left_symbol->type_information;

            // Fold if possible (except for enums)
            if (nodecl_is_constant(right_tree)
                    && !is_enum_type(left_symbol->type_information))
            {
                current_deduced_parameter.value =
                    const_value_to_nodecl(nodecl_get_constant(right_tree));
            }

            char added = deduction_add_nontype_parameter_deduction(deduction, &current_deduced_parameter, flags);

            DEBUG_CODE()
            {
                if (added)
                {
                    fprintf(stderr, "TYPEUNIF: Added unification for '%s' <- '%s'\n",
                            codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                            codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
                }
            }

            char equivalent = 0;

            if (right_symbol != NULL
                    && ((right_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER)
                        || (right_symbol->kind == SK_TEMPLATE_NONTYPE_PARAMETER_PACK)))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "TYPEUNIF: Checking if it is exactly the same template parameter '%s' and '%s'\n",
                            codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                            codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
                }
                if ((right_symbol->entity_specs.template_parameter_nesting 
                            == left_symbol->entity_specs.template_parameter_nesting)
                        && (right_symbol->entity_specs.template_parameter_position 
                            == left_symbol->entity_specs.template_parameter_position))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: They are the same template parameter\n");
                    }
                    equivalent = 1;
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "TYPEUNIF: They are different template parameters\n");
                    }
                    equivalent = 0;
                }
            }

            return equivalent;
        }
        // If both a parameters of a function, check if they might mean the same entity
        else if (left_symbol->kind == SK_VARIABLE
                && symbol_is_parameter_of_function(left_symbol, get_function_declaration_proxy())
                && right_symbol != NULL
                && right_symbol->kind == SK_VARIABLE
                && symbol_is_parameter_of_function(right_symbol, get_function_declaration_proxy()))
        {
            int left_nesting = symbol_get_parameter_nesting_in_function(left_symbol, get_function_declaration_proxy());
            int left_position = symbol_get_parameter_position_in_function(left_symbol, get_function_declaration_proxy());

            int right_nesting = symbol_get_parameter_nesting_in_function(right_symbol, get_function_declaration_proxy());
            int right_position = symbol_get_parameter_position_in_function(right_symbol, get_function_declaration_proxy());

            return (left_nesting == right_nesting
                    && left_position == right_position
                    // Check also type identity of the parameters themselves
                    && equivalent_types(left_symbol->type_information,
                        right_symbol->type_information));
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Unification through symbols failed %s != %s\n", 
                    codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                    codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
        }

        return 0;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Nodecls are not comparable\n");
    }

    // Best effort
    return equivalent_nodecl_expressions(left_tree, right_tree, unif_set, flags);
}

static char equivalent_nodecl_expressions(nodecl_t left_tree, nodecl_t right_tree, 
        deduction_set_t** unif_set,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether nodecls %s and %s are structurally equivalent\n",
                codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
    }

    if (nodecl_is_null(left_tree)
             || nodecl_is_null(right_tree))
        return nodecl_is_null(left_tree)
            && nodecl_is_null(right_tree);

    if (nodecl_get_kind(left_tree) == NODECL_CXX_VALUE_PACK
            && nodecl_get_kind(right_tree) != NODECL_CXX_VALUE_PACK
            && nodecl_is_list(right_tree))
    {
        // This case
        //
        // template <int ...N>
        // struct A
        // {
        // };
        //
        // A<1, 2, 3>; // N... <- {1, 2, 3}

        nodecl_t pack_expr = nodecl_get_child(left_tree, 0);

        int num_items = 0;
        char result = 1;
        nodecl_t* list = nodecl_unpack_list(right_tree, &num_items);

        int i;
        for (i = 0; i < num_items; i++)
        {
            deduction_set_t *current_unif_set = counted_xcalloc(1, sizeof(*current_unif_set), &_bytes_typeunif);

            if (!equivalent_dependent_expressions(pack_expr, list[i], &current_unif_set, flags))
                result = 0;

            merge_deduction_set(unif_set, current_unif_set, flags);
        }

        return result;
    }

    if (nodecl_get_kind(left_tree) != nodecl_get_kind(right_tree))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Their kinds are different %s != %s\n",
                    ast_print_node_type(nodecl_get_kind(left_tree)),
                    ast_print_node_type(nodecl_get_kind(right_tree)));
        }
        return 0;
    }

    if (nodecl_get_kind(left_tree) == NODECL_TYPE)
    {
        // This is a special node just holding a type, check its types instead
        return equivalent_types(
                nodecl_get_type(left_tree),
                nodecl_get_type(right_tree));
    }

    // FIXME - This can be done better
    // This is a crude implementation that applies equivalent_dependent_expressions to each nodecl
    char ok = 1;
    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN && ok; i++)
    {
        nodecl_t left_child = nodecl_get_child(left_tree, i);
        nodecl_t right_child = nodecl_get_child(right_tree, i);

        if (nodecl_is_null(left_child) != nodecl_is_null(right_child))
        {
            return 0;
        }

        if (!nodecl_is_null(left_child))
        {
            ok = ok && equivalent_dependent_expressions(left_child, right_child, unif_set, flags);
        }
    }

    return ok;
}

static tribool_t equivalent_expression_trees(nodecl_t left_tree, nodecl_t right_tree)
{
    if (nodecl_is_constant(left_tree)
            && nodecl_is_constant(right_tree))
    {
        if (const_value_is_nonzero(
                const_value_eq(
                    nodecl_get_constant(left_tree),
                    nodecl_get_constant(right_tree))))
        {
            return DEFINITELY_TRUE;
        }
        else
        {
            return DEFINITELY_FALSE;
        }
    }
    return NOT_SURE;
}

// Defined in cxx-typededuc.c
void deduction_set_free(deduction_set_t* deduction_set);

char same_functional_expression(nodecl_t left_tree, nodecl_t right_tree, 
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Checking whether '%s' and '%s' are functionally equivalent\n",
                nodecl_is_null(left_tree) ? "<<NULL>>" : codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                nodecl_is_null(right_tree) ? "<<NULL>>" : codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)));
    }
    deduction_set_t* deduction_set = counted_xcalloc(1, sizeof(*deduction_set), &_bytes_typeunif);

    char c = 0;
   
    // We do not compare trees if any of them is null
    if (!nodecl_is_null(left_tree)
            && !nodecl_is_null(right_tree))
    {
        c = equivalent_dependent_expressions(left_tree, right_tree,  
                &deduction_set, flags);
    }

    // Free it, it is unused after this
    deduction_set_free(deduction_set);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: '%s' and '%s' %s functionally equivalent\n",
                nodecl_is_null(left_tree) ? "<<NULL>>" : codegen_to_str(left_tree, nodecl_retrieve_context(left_tree)),
                nodecl_is_null(right_tree) ? "<<NULL>>" : codegen_to_str(right_tree, nodecl_retrieve_context(right_tree)),
                c ? "ARE" : "are NOT");
    }

    return c;
}

static void unificate_unresolved_overloaded(type_t* t1, type_t* t2, 
        deduction_set_t** deduction_set, decl_context_t decl_context,
        const locus_t* locus,
        deduction_flags_t flags)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unifying with an unresolved overloaded type. Unfolding it\n");
    }
    scope_entry_list_t* overloaded_set = unresolved_overloaded_type_get_overload_set(t2);

    char contains_templates = 0;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(overloaded_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));

        if (entry->kind == SK_TEMPLATE)
        {
            contains_templates = 1;
        }
    }

    if (contains_templates)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: The overloaded set contains one or more templates, not using it for deduction\n");
        }
        entry_list_free(overloaded_set);
        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: There are %d overloads\n",
                entry_list_size(overloaded_set));
    }

    // template_parameter_list_t* explicit_template_parameters 
    //     = unresolved_overloaded_type_get_explicit_template_arguments(t2);

    // FIXME - The standard says that this context is deduced if only one function matches
    // the type, but we are not able to assert this yet

    for (it = entry_list_iterator_begin(overloaded_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
        // char is_template = 0;

        type_t* function_type = NULL;

        if (entry->kind == SK_FUNCTION)
        {
            function_type = entry->type_information;
        }
        else
        {
            internal_error("Code unreachable", 0);
        }

        if (entry->entity_specs.is_member
                && !entry->entity_specs.is_static)
        {
            function_type = get_pointer_to_member_type(function_type,
                    entry->entity_specs.class_type);
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "TYPEUNIF: Trying unification with '%s' at '%s' of type '%s'\n",
                    // is_template ? "[template]" : "",
                    entry->symbol_name,
                    locus_to_str(entry->locus),
                    print_declarator(function_type));
        }

        // Now perform deduction
        unificate_two_types(t1, function_type, deduction_set, decl_context, locus, flags);
    }
    entry_list_iterator_free(it);
    entry_list_free(overloaded_set);

    DEBUG_CODE()
    {
        fprintf(stderr, "TYPEUNIF: Unification of unresolved overloaded types ended\n");
    }
}

deduction_flags_t deduction_flags_empty()
{
    deduction_flags_t flags;
    memset(&flags, 0, sizeof(flags));

    return flags;
}
