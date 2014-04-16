/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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
#include "cxx-entrylist.h"
#include "cxx-diagnostic.h"

static type_t* determine_most_specialized_template_class(type_t* template_type, 
        type_t** matching_specializations, int num_specializations,
        decl_context_t decl_context,
        const locus_t* locus);

type_t* solve_class_template(type_t* template_type,
        type_t* specialized_type,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus)
{
    int i;
    int num_specializations = template_type_get_num_specializations(template_type);

    type_t** matching_set = NULL;
    int num_matching_set = 0;

    template_parameter_list_t **deduction_results = NULL;
    int num_deductions = 0;

    for (i = 0; i < num_specializations; i++)
    {
        type_t* current_specialized_type = 
            template_type_get_specialization_num(template_type, i);

        DEBUG_CODE()
        {
            scope_entry_t* entry = named_type_get_symbol(current_specialized_type);
            fprintf(stderr, "SOLVETEMPLATE: Checking with specialization defined in '%s' (%s)\n",
                    print_declarator(current_specialized_type),
                    locus_to_str(entry->locus));
        }

        // We do not want these for instantiation purposes
        if (!named_type_get_symbol(current_specialized_type)->entity_specs.is_instantiable)
        {
            DEBUG_CODE()
            {
                scope_entry_t* entry = named_type_get_symbol(current_specialized_type);
                fprintf(stderr, "SOLVETEMPLATE: Discarding '%s' (%s) since it has been created by the typesystem\n",
                        print_declarator(current_specialized_type),
                        locus_to_str(entry->locus));
            }
            continue;
        }

        // We do not want aliases for instantiation purposes either
        if (named_type_get_symbol(current_specialized_type)->entity_specs.alias_to != NULL)
        {
            DEBUG_CODE()
            {
                scope_entry_t* entry = named_type_get_symbol(current_specialized_type);
                fprintf(stderr, "SOLVETEMPLATE: Discarding '%s' (%s) since it is actually "
                        "an alias to another specialized type\n",
                        print_declarator(current_specialized_type),
                        locus_to_str(entry->locus));
            }
            continue;
        }

        if (equivalent_types(current_specialized_type, specialized_type))
        {
            // Ourselves is not an option
            continue;
        }

        // template_parameter_list_t *arguments =
        //     template_specialized_type_get_template_arguments(
        //             get_actual_class_type(current_specialized_type));

        // It is supposed that this will hold in correct code
        // ERROR_CONDITION((arguments->num_parameters != specialized->num_parameters),
        //     "Template argument lists are not of equal length", 0);

        template_parameter_list_t* current_deduced_template_arguments = NULL;

        if (is_less_or_equal_specialized_template_class(
                    current_specialized_type,
                    specialized_type,
                    named_type_get_symbol(current_specialized_type)->decl_context,
                    &current_deduced_template_arguments,
                    locus))
        {
            P_LIST_ADD(matching_set, num_matching_set, current_specialized_type);
            P_LIST_ADD(deduction_results, num_deductions, current_deduced_template_arguments);
        }
    }

    if (num_matching_set >= 1)
    {
        type_t* more_specialized = 
            determine_most_specialized_template_class(template_type, matching_set, 
                num_matching_set, named_type_get_symbol(specialized_type)->decl_context, 
                locus);

        if (more_specialized == NULL)
            return NULL;

        if (is_unresolved_overloaded_type(more_specialized))
        {
            fprintf(stderr, "%s: note: template specialization candidate list\n", locus_to_str(locus));

            scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(more_specialized);

            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                fprintf(stderr, "%s: note:   %s\n",
                        locus_to_str(entry->locus),
                        print_type_str(get_user_defined_type(entry), entry->decl_context));
            }
            entry_list_iterator_free(it);
            entry_list_free(entry_list);

            error_printf("%s: error: ambiguous template type for '%s'\n", 
                    locus_to_str(locus),
                    print_type_str(specialized_type, named_type_get_symbol(specialized_type)->decl_context));
            return NULL;
        }

        for (i = 0; i < num_matching_set; i++)
        {
            if (matching_set[i] == more_specialized)
            {
                *deduced_template_arguments = deduction_results[i];
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
        const locus_t* locus)
{
    int current_i = 0;
    type_t* current_most_specialized = matching_specializations[0];

    template_parameter_list_t* deduced_template_arguments = NULL;

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Unification has found '%d' matching template specializations, "
                " looking for the best one among the following:\n", num_specializations
                );
        int j;
        for (j = 0; j < num_specializations; j++)
        {
            scope_entry_t* current = named_type_get_symbol(matching_specializations[j]);
            fprintf(stderr, "SOLVETEMPLATE:     Matching specialization: [%d] '%s'\n",
                    j,
                    locus_to_str(current->locus));
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
            fprintf(stderr, "SOLVETEMPLATE: Checking current most specialized template [%d] '%s' against [%d] '%s'\n",
                    current_i,
                    locus_to_str(minimum->locus),
                    i,
                    locus_to_str(current->locus));
        }
        if (!is_less_or_equal_specialized_template_class(
                    matching_specializations[i],
                    current_most_specialized,
                    decl_context,
                    &deduced_template_arguments, locus))
        {
            // It is more specialized
            DEBUG_CODE()
            {
                scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
                scope_entry_t* current = named_type_get_symbol(matching_specializations[i]);
                fprintf(stderr, "SOLVETEMPLATE: Template specialization '%s' is more specialized than '%s'\n",
                        locus_to_str(current->locus),
                        locus_to_str(minimum->locus));
            }
            current_i = i;
            current_most_specialized = matching_specializations[i];
        }
    }

    DEBUG_CODE()
    {
        scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
        fprintf(stderr, "SOLVETEMPLATE: Checking that [%d] %s is actually the most specialized template class\n", 
                current_i,
                locus_to_str(minimum->locus));
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
                    &deduced_template_arguments, locus))
        {
            DEBUG_CODE()
            {
                scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
                scope_entry_t* current = named_type_get_symbol(matching_specializations[i]);
                fprintf(stderr, "SOLVETEMPLATE: There is not a most specialized template since '%s' is not less "
                        "specialized as '%s'\n", 
                        locus_to_str(current->locus),
                        locus_to_str(minimum->locus));
            }

            // Return the ambiguity as a list
            scope_entry_list_t* ambiguous_result = 
                entry_list_new(named_type_get_symbol(current_most_specialized));
            ambiguous_result = entry_list_add(ambiguous_result, named_type_get_symbol(matching_specializations[i]));

            return get_unresolved_overloaded_type(ambiguous_result, NULL);
        }
    }

    DEBUG_CODE()
    {
        scope_entry_t* minimum = named_type_get_symbol(current_most_specialized);
        fprintf(stderr, "SOLVETEMPLATE: Most specialized template is [%d] '%s'\n",
                current_i, locus_to_str(minimum->locus));
    }

    return current_most_specialized;
}

static type_t* extend_function_with_return_type(type_t* funct_type);

static
type_t* determine_most_specialized_template_function(int num_feasible_templates, 
        type_t** feasible_templates, 
        const locus_t* locus)
{
    if (num_feasible_templates == 0)
        return NULL;

    // Now select the best one among all feasible_templates
    type_t* most_specialized = feasible_templates[0];

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Starting with '%s' at '%s' as the most specialized template-function\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    locus_to_str(named_type_get_symbol(most_specialized)->locus));
    }

    int i;
    for (i = 1; i < num_feasible_templates; i++)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "SOLVETEMPLATE: Comparing '%s' at '%s' against '%s' at '%s'\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    locus_to_str(named_type_get_symbol(most_specialized)->locus),
                    named_type_get_symbol(feasible_templates[i])->symbol_name,
                    locus_to_str(named_type_get_symbol(feasible_templates[i])->locus));
        }

        char is_conversion = 
            named_type_get_symbol(feasible_templates[i])->entity_specs.is_conversion;
        scope_entry_t* f_sym = named_type_get_symbol(feasible_templates[i]);
        type_t* f = f_sym->type_information;
        scope_entry_t* g_sym = named_type_get_symbol(most_specialized);
        type_t* g = g_sym->type_information;

        f = extend_function_with_return_type(f);
        g = extend_function_with_return_type(g);

        template_parameter_list_t* deduced_template_arguments = NULL;
        if (!is_less_or_equal_specialized_template_function(
                    f,
                    g,
                    f_sym->decl_context,
                    &deduced_template_arguments,
                    /* explicit_template_parameters */ NULL, 
                    locus,
                    is_conversion))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SOLVETEMPLATE: Found that '%s' at '%s' is "
                        "more specialized than '%s' at '%s'\n",
                        named_type_get_symbol(feasible_templates[i])->symbol_name,
                        locus_to_str(named_type_get_symbol(feasible_templates[i])->locus),
                        named_type_get_symbol(most_specialized)->symbol_name,
                        locus_to_str(named_type_get_symbol(most_specialized)->locus));
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
            fprintf(stderr, "SOLVETEMPLATE: Checking that '%s' at '%s' is "
                    "more specialized than '%s' at '%s'\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    locus_to_str(named_type_get_symbol(most_specialized)->locus),
                    named_type_get_symbol(feasible_templates[i])->symbol_name,
                    locus_to_str(named_type_get_symbol(feasible_templates[i])->locus));
        }

        char is_conversion = 
            named_type_get_symbol(most_specialized)->entity_specs.is_conversion;
        scope_entry_t* f_sym = named_type_get_symbol(feasible_templates[i]);
        type_t* f = f_sym->type_information;
        scope_entry_t* g_sym = named_type_get_symbol(most_specialized);
        type_t* g = g_sym->type_information;

        f = extend_function_with_return_type(f);
        g = extend_function_with_return_type(g);

        template_parameter_list_t* deduced_template_arguments = NULL;
        if (!is_less_or_equal_specialized_template_function(
                    f,
                    g,
                    f_sym->decl_context,
                    &deduced_template_arguments,
                    /* explicit_template_parameters */ NULL, 
                    locus,
                    is_conversion))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "SOLVETEMPLATE: Found that '%s' at '%s' is "
                        "not the most specialized. '%s' at '%s' is not less specialized\n",
                        named_type_get_symbol(most_specialized)->symbol_name,
                        locus_to_str(named_type_get_symbol(most_specialized)->locus),
                        named_type_get_symbol(feasible_templates[i])->symbol_name,
                        locus_to_str(named_type_get_symbol(feasible_templates[i])->locus));
            }

            // Return the ambiguity as a list
            scope_entry_list_t* ambiguous_result = entry_list_new(named_type_get_symbol(most_specialized));
            ambiguous_result = entry_list_add(ambiguous_result, named_type_get_symbol(feasible_templates[i]));

            return get_unresolved_overloaded_type(ambiguous_result, NULL);
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "SOLVETEMPLATE: Determined '%s' at '%s' as the most specialized template-function\n",
                    named_type_get_symbol(most_specialized)->symbol_name,
                    locus_to_str(named_type_get_symbol(most_specialized)->locus));
    }

    return most_specialized;
}

static type_t* extend_function_with_return_type(type_t* funct_type)
{
    // Some functions do not return anything, they are already extended
    if (function_type_get_return_type(funct_type) == NULL)
        return funct_type;

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
        params[i].type_info = get_ellipsis_type();

        num_params = function_type_get_num_parameters(funct_type);
    }

    type_t* result_type = get_new_function_type(get_void_type(), params, num_params + 1, REF_QUALIFIER_NONE);

    if (is_template_specialized_type(funct_type))
    {
        type_t* template_type = template_specialized_type_get_related_template_type(funct_type);
        type_t* primary_type = template_type_get_primary_type(template_type);

        type_t* new_template = get_new_template_type(
                template_type_get_template_parameters(template_type),
                result_type, 
                named_type_get_symbol(primary_type)->symbol_name,
                named_type_get_symbol(primary_type)->decl_context,
                named_type_get_symbol(primary_type)->locus);

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

scope_entry_list_t* solve_template_function(scope_entry_list_t* template_set,
        template_parameter_list_t* explicit_template_parameters,
        type_t* function_type, const locus_t* locus)
{
    type_t* feasible_templates[MCXX_MAX_FEASIBLE_SPECIALIZATIONS];
    template_parameter_list_t *feasible_deductions[MCXX_MAX_FEASIBLE_SPECIALIZATIONS];
    int num_feasible_templates = 0;

    type_t* extended_function_type = extend_function_with_return_type(function_type);

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(template_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        if (entry->kind != SK_TEMPLATE)
        {
            // Skip nontemplates
            continue;
        }

        type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
        scope_entry_t* primary_symbol = named_type_get_symbol(primary_named_type);
        type_t* primary_type = primary_symbol->type_information;

        type_t* extended_primary_type = extend_function_with_return_type(primary_type);

        template_parameter_list_t *feasible_deduction = NULL;
        if (is_less_or_equal_specialized_template_function(
                    extended_primary_type,
                    extended_function_type,
                    primary_symbol->decl_context,
                    &feasible_deduction,
                    explicit_template_parameters,
                    locus, 
                    primary_symbol->entity_specs.is_conversion))
        {
            ERROR_CONDITION(num_feasible_templates >= MCXX_MAX_FEASIBLE_SPECIALIZATIONS,
                    "Too many feasible deductions", 0);
            feasible_templates[num_feasible_templates] = primary_named_type;
            feasible_deductions[num_feasible_templates] = feasible_deduction;
            num_feasible_templates++;
        }
    }
    entry_list_iterator_free(it);

    type_t* result = determine_most_specialized_template_function(num_feasible_templates,
            feasible_templates, locus);

    if (result == NULL)
    {
        // Not found
        return NULL;
    }

    if (is_unresolved_overloaded_type(result))
    {
        // Ambiguous case
        scope_entry_list_t* entry_list = unresolved_overloaded_type_get_overload_set(result);
        return entry_list;
    }

    template_parameter_list_t* selected_deduction = NULL;
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
    type_t* template_type = template_specialized_type_get_related_template_type(
            named_type_get_symbol(result)->type_information);
    scope_entry_t* primary_template = template_type_get_related_symbol(template_type);

    type_t* result_specialized = template_type_get_specialized_type(
            template_type,
            selected_deduction, 
            primary_template->decl_context, 
            locus);

    return entry_list_new(named_type_get_symbol(result_specialized));
}
