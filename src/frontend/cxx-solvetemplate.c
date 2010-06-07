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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-solvetemplate.h"
#include "cxx-typededuc.h"
#include "cxx-typeutils.h"
#include "cxx-typeorder.h"
#include "cxx-prettyprint.h"
#include "cxx-driver.h"
#include "cxx-instantiation.h"

static type_t* determine_most_specialized_template_class(type_t* template_type, 
        type_t** matching_specializations, int num_specializations,
        decl_context_t decl_context,
        const char *filename, int line);

type_t* solve_class_template(decl_context_t decl_context,
        type_t* template_type,
        type_t* specialized_type,
        deduction_set_t** unification_set,
        const char *filename,
        int line)
{
    template_argument_list_t* specialized
        = template_specialized_type_get_template_arguments(
                get_actual_class_type(specialized_type));

    int i;
    int num_specializations = template_type_get_num_specializations(template_type);

    type_t** matching_set = NULL;
    int num_matching_set = 0;

    deduction_set_t **deduction_results = NULL;
    int num_deductions = 0;

    for (i = 0; i < num_specializations; i++)
    {
        type_t* current_specialized_type = 
            template_type_get_specialization_num(template_type, i);

        DEBUG_CODE()
        {
            scope_entry_t* entry = named_type_get_symbol(current_specialized_type);
            fprintf(stderr, "SOLVETEMPLATE: Checking with specialization defined in '%s:%d'\n",
                    entry->file, entry->line);
        }

        // We do not want these for instantiation purposes
        if (class_type_is_incomplete_independent(
                    get_actual_class_type(current_specialized_type))
                || class_type_is_incomplete_dependent(
                    get_actual_class_type(current_specialized_type)))
        {
            DEBUG_CODE()
            {
                scope_entry_t* entry = named_type_get_symbol(current_specialized_type);
                fprintf(stderr, "SOLVETEMPLATE: Discarding '%s:%d' since it is incomplete\n",
                        entry->file, entry->line);
            }
            continue;
        }

        template_argument_list_t *arguments = 
            template_specialized_type_get_template_arguments(
                    get_actual_class_type(current_specialized_type));

        // It is supposed that this will hold in correct code
        ERROR_CONDITION((arguments->num_arguments != specialized->num_arguments),
            "Template argument lists are not of equal length", 0);

        deduction_set_t* deduction_result = NULL;

        if (is_less_or_equal_specialized_template_class(
                    current_specialized_type,
                    specialized_type,
                    decl_context,
                    &deduction_result,
                    filename, line))
        {
            P_LIST_ADD(matching_set, num_matching_set, current_specialized_type);
            P_LIST_ADD(deduction_results, num_deductions, deduction_result);
        }
    }

    if (num_matching_set >= 1)
    {
        type_t* more_specialized = 
            determine_most_specialized_template_class(template_type, matching_set, 
                num_matching_set, decl_context, filename, line);

        if (more_specialized == NULL)
            return NULL;

        if (is_unresolved_overloaded_type(more_specialized))
        {
            fprintf(stderr, "%s:%d: note: template specialization candidate list\n", filename, line);

            scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(more_specialized);
            while (entry_list != NULL)
            {
                fprintf(stderr, "%s:%d: note:   %s\n",
                        entry_list->entry->file,
                        entry_list->entry->line,
                        print_type_str(get_user_defined_type(entry_list->entry), decl_context));

                entry_list = entry_list->next;
            }

            running_error("%s:%d: error: ambiguous template type for '%s'\n", filename, line,
                    print_type_str(specialized_type, decl_context));
        }

        for (i = 0; i < num_matching_set; i++)
        {
            if (matching_set[i] == more_specialized)
            {
                *unification_set = deduction_results[i];
            }
        }

        return more_specialized;
    }
    else
    {
        return NULL;
    }
}

// This function assumes that only one minimum will exist
static type_t* determine_most_specialized_template_class(
        type_t* template_type UNUSED_PARAMETER, 
        type_t** matching_specializations, int num_specializations,
        decl_context_t decl_context,
        const char *filename, int line)
{
    int current_i = 0;
    type_t* current_most_specialized = matching_specializations[0];

    deduction_set_t* deduce_result = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Unification has found '%d' matching template specializations, "
                " looking for the best one among the following:\n", num_specializations
                );
        int j;
        for (j = 0; j < num_specializations; j++)
        {
            scope_entry_t* current = named_type_get_symbol(matching_specializations[j]);
            fprintf(stderr, "SOLVETEMPLATE:     Matching specialization: [%d] '%s:%d'\n",
                    j,
                    current->file, current->line);
        }
        fprintf(stderr, "SOLVETEMPLATE: No more specializations matched\n");
    }

    int i;
    for (i = 1; i < num_specializations; i++)
    {
        DEBUG_CODE()
        {
            scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
            scope_entry_t* current = named_type_get_symbol(matching_specializations[i]);
            fprintf(stderr, "SOLVETEMPLATE: Checking current most specialized template [%d] '%s:%d' against [%d] '%s:%d'\n",
                    current_i,
                    minimum->file, minimum->line, 
                    i,
                    current->file, current->line);
        }
        if (!is_less_or_equal_specialized_template_class(
                    matching_specializations[i],
                    current_most_specialized,
                    decl_context,
                    &deduce_result, filename, line))
        {
            // It is more specialized
            DEBUG_CODE()
            {
                scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
                scope_entry_t* current = named_type_get_symbol(matching_specializations[i]);
                fprintf(stderr, "SOLVETEMPLATE: Template specialization '%s:%d' is more specialized than '%s:%d'\n",
                        current->file, current->line,
                        minimum->file, minimum->line);
            }
            current_i = i;
            current_most_specialized = matching_specializations[i];
        }
    }

    DEBUG_CODE()
    {
        scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
        fprintf(stderr, "SOLVETEMPLATE: Checking that [%d] %s:%d is actually the most specialized template class\n", 
                current_i,
                minimum->file, minimum->line);
    }

    // Now check it is actually the minimum
    for (i = 0; i < num_specializations; i++)
    {
        if (current_most_specialized == matching_specializations[i])
            continue;

        if (!is_less_or_equal_specialized_template_class(
                    matching_specializations[i],
                    current_most_specialized,
                    decl_context,
                    &deduce_result, filename, line))
        {
            DEBUG_CODE()
            {
                scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
                scope_entry_t* current = named_type_get_symbol(matching_specializations[i]);
                fprintf(stderr, "SOLVETEMPLATE: There is not a most specialized template since '%s:%d' is not less "
                        "specialized as '%s:%d'\n", 
                        current->file, current->line,
                        minimum->file, minimum->line);
            }
            
            // Return the ambiguity as a list
            scope_entry_list_t* ambiguous_result = calloc(sizeof(*ambiguous_result), 2);
            ambiguous_result->next = &ambiguous_result[1];
            ambiguous_result->entry = named_type_get_symbol(current_most_specialized);
            ambiguous_result[1].entry = named_type_get_symbol(matching_specializations[i]);

            return get_unresolved_overloaded_type(ambiguous_result, NULL);
        }
    }

    DEBUG_CODE()
    {
        scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
        fprintf(stderr, "SOLVETEMPLATE: Most specialized template is [%d] '%s:%d'\n",
                current_i, minimum->file, minimum->line);
    }

    return current_most_specialized;
}

static type_t* extend_function_with_return_type(type_t* funct_type);

static
type_t* determine_most_specialized_template_function(int num_feasible_templates, 
        type_t** feasible_templates, decl_context_t decl_context,
        const char *filename, int line)
{
    if (num_feasible_templates == 0)
        return NULL;

    // Now select the best one among all feasible_templates
    type_t* most_specialized = feasible_templates[0];

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Starting with '%s' at '%s:%d' as the most specialized template-function\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    named_type_get_symbol(most_specialized)->file,
                    named_type_get_symbol(most_specialized)->line);
    }

    int i;
    for (i = 1; i < num_feasible_templates; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SOLVETEMPLATE: Comparing '%s' at '%s:%d' against '%s' at '%s:%d'\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    named_type_get_symbol(most_specialized)->file,
                    named_type_get_symbol(most_specialized)->line,
                    named_type_get_symbol(feasible_templates[i])->symbol_name,
                    named_type_get_symbol(feasible_templates[i])->file,
                    named_type_get_symbol(feasible_templates[i])->line);
        }

        char is_conversion = 
            named_type_get_symbol(feasible_templates[i])->entity_specs.is_conversion;
        type_t* f = named_type_get_symbol(feasible_templates[i])->type_information;
        type_t* g = named_type_get_symbol(most_specialized)->type_information;

        f = extend_function_with_return_type(f);
        g = extend_function_with_return_type(g);

        if (!is_less_or_equal_specialized_template_function(
                    f,
                    g,
                    decl_context, 
                    /* deduction_set */ NULL,
                    /* explicit_template_arguments */ NULL, 
                    filename, line,
                    is_conversion))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SOLVETEMPLATE: Found that '%s' at '%s:%d' is "
                        "more specialized than '%s' at '%s:%d'\n",
                        named_type_get_symbol(feasible_templates[i])->symbol_name,
                        named_type_get_symbol(feasible_templates[i])->file,
                        named_type_get_symbol(feasible_templates[i])->line,
                        named_type_get_symbol(most_specialized)->symbol_name,
                        named_type_get_symbol(most_specialized)->file,
                        named_type_get_symbol(most_specialized)->line);
            }

            most_specialized = feasible_templates[i];
        }
    }

    // Now check it is actually the best one
    for (i = 0; i < num_feasible_templates; i++)
    {
        if (most_specialized == feasible_templates[i])
            continue;

        DEBUG_CODE()
        {
            fprintf(stderr, "SOLVETEMPLATE: Checking that '%s' at '%s:%d' is "
                    "more specialized than '%s' at '%s:%d'\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    named_type_get_symbol(most_specialized)->file,
                    named_type_get_symbol(most_specialized)->line,
                    named_type_get_symbol(feasible_templates[i])->symbol_name,
                    named_type_get_symbol(feasible_templates[i])->file,
                    named_type_get_symbol(feasible_templates[i])->line);
        }

        char is_conversion = 
            named_type_get_symbol(most_specialized)->entity_specs.is_conversion;
        type_t* f = named_type_get_symbol(feasible_templates[i])->type_information;
        type_t* g = named_type_get_symbol(most_specialized)->type_information;

        f = extend_function_with_return_type(f);
        g = extend_function_with_return_type(g);

        if (!is_less_or_equal_specialized_template_function(
                    f,
                    g,
                    decl_context, 
                    /* deduction_set */ NULL,
                    /* explicit_template_arguments */ NULL, 
                    filename, line,
                    is_conversion))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SOLVETEMPLATE: Found that '%s' at '%s:%d' is "
                        "not the most specialized. '%s' at '%s:%d' is not less specialized\n",
                        named_type_get_symbol(most_specialized)->symbol_name,
                        named_type_get_symbol(most_specialized)->file,
                        named_type_get_symbol(most_specialized)->line,
                        named_type_get_symbol(feasible_templates[i])->symbol_name,
                        named_type_get_symbol(feasible_templates[i])->file,
                        named_type_get_symbol(feasible_templates[i])->line);
            }

            // Return the ambiguity as a list
            scope_entry_list_t* ambiguous_result = calloc(sizeof(*ambiguous_result), 2);
            ambiguous_result->next = &ambiguous_result[1];
            ambiguous_result->entry = named_type_get_symbol(most_specialized);
            ambiguous_result[1].entry = named_type_get_symbol(feasible_templates[i]);

            return get_unresolved_overloaded_type(ambiguous_result, NULL);
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Determined '%s' at '%s:%d' as the most specialized template-function\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    named_type_get_symbol(most_specialized)->file,
                    named_type_get_symbol(most_specialized)->line);
    }

    return most_specialized;
}

static type_t* extend_function_with_return_type(type_t* funct_type)
{
    int num_params = function_type_get_num_parameters(funct_type);
    parameter_info_t params[num_params + 1];
    memset(params, 0, sizeof(params));

    char has_ellipsis = function_type_get_has_ellipsis(funct_type);

    if (has_ellipsis)
        num_params--;

    int i;
    for (i = 0; i < num_params; i++)
    {
        params[i].type_info = function_type_get_parameter_type_num(funct_type, i);
        params[i].nonadjusted_type_info = function_type_get_nonadjusted_parameter_type_num(funct_type, i);
    }

    params[i].type_info = function_type_get_return_type(funct_type);
    params[i].nonadjusted_type_info = function_type_get_return_type(funct_type);

    if (has_ellipsis)
    {
        i++;
        params[i].is_ellipsis = 1;

        num_params = function_type_get_num_parameters(funct_type);
    }

    type_t* result_type = get_new_function_type(get_void_type(), params, num_params + 1);

    if (is_template_specialized_type(funct_type))
    {
        type_t* template_type = template_specialized_type_get_related_template_type(funct_type);
        type_t* primary_type = template_type_get_primary_type(template_type);

        type_t* new_template = get_new_template_type(
                template_type_get_template_parameters(template_type),
                result_type, 
                named_type_get_symbol(primary_type)->symbol_name,
                named_type_get_symbol(primary_type)->decl_context,
                named_type_get_symbol(primary_type)->line,
                named_type_get_symbol(primary_type)->file);

        result_type = named_type_get_symbol(template_type_get_primary_type(new_template))->type_information;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Type has been extended from '%s' to '%s'\n", 
                print_declarator(funct_type),
                print_declarator(result_type));
    }

    return result_type;
}

scope_entry_t* solve_template_function(scope_entry_list_t* template_set,
        template_argument_list_t* explicit_template_arguments,
        type_t* function_type, decl_context_t decl_context,
        const char *filename, int line)
{
    scope_entry_list_t *it = template_set;

#define MAX_FEASIBLE_SPECIALIZATIONS (256)
    type_t* feasible_templates[MAX_FEASIBLE_SPECIALIZATIONS];
    deduction_set_t *feasible_deductions[MAX_FEASIBLE_SPECIALIZATIONS];
    int num_feasible_templates = 0;

    type_t* extended_function_type = extend_function_with_return_type(function_type);

    while (it != NULL)
    {
        scope_entry_t* entry = it->entry;
        if (entry->kind != SK_TEMPLATE)
        {
            // Skip nontemplates
            it = it->next;
            continue;
        }

        type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
        scope_entry_t* primary_symbol = named_type_get_symbol(primary_named_type);
        type_t* primary_type = primary_symbol->type_information;

        type_t* extended_primary_type = extend_function_with_return_type(primary_type);

        deduction_set_t *feasible_deduction = NULL;
        if (is_less_or_equal_specialized_template_function(
                    extended_primary_type,
                    extended_function_type,
                    decl_context,
                    &feasible_deduction,
                    explicit_template_arguments,
                    filename, line, 
                    primary_symbol->entity_specs.is_conversion))
        {
            ERROR_CONDITION(num_feasible_templates >= MAX_FEASIBLE_SPECIALIZATIONS,
                    "Too many feasible deductions", 0);
            feasible_templates[num_feasible_templates] = primary_named_type;
            feasible_deductions[num_feasible_templates] = feasible_deduction;
            num_feasible_templates++;
        }

        it = it->next;
    }

    type_t* result = determine_most_specialized_template_function(num_feasible_templates,
            feasible_templates, decl_context, filename, line);

    if (result == NULL)
        return NULL;

    if (is_unresolved_overloaded_type(result))
    {
        char is_dependent = 0;
        int max_qualif = 0;

        const char* full_name = get_fully_qualified_symbol_name(
                template_set->entry,
                decl_context, &is_dependent, &max_qualif);

        scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(result);

        fprintf(stderr, "%s:%d: note: ambiguous template functions list\n", filename, line);
        while (entry_list != NULL)
        {
            fprintf(stderr, "%s:%d: note:   %s\n",
                    entry_list->entry->file,
                    entry_list->entry->line,
                    print_decl_type_str(entry_list->entry->type_information, decl_context, full_name));

            entry_list = entry_list->next;
        }

        running_error("%s:%d: error: ambiguous template specialization '%s' for '%s'\n",
                filename, line,
                template_set->entry->symbol_name,
                print_decl_type_str(function_type, decl_context, full_name));
    }

    deduction_set_t* selected_deduction = NULL;
    // Now we have to find which one is the selected deduction
    int i;
    for (i = 0; i < num_feasible_templates; i++)
    {
        if (feasible_templates[i] == result)
        {
            selected_deduction = feasible_deductions[i];
            break;
        }
    }

    ERROR_CONDITION((selected_deduction == NULL), "Selected deduction cannot be NULL", 0);

    // Build the specialized type
    template_argument_list_t* template_arguments = 
        build_template_argument_list_from_deduction_set(selected_deduction);

    type_t* result_specialized = template_type_get_specialized_type(
            template_specialized_type_get_related_template_type(
                named_type_get_symbol(result)->type_information
                ),
            template_arguments, /* no template parameters */ NULL,
            decl_context, line, filename);

    return named_type_get_symbol(result_specialized);
}
