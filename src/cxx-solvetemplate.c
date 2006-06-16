#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-solvetemplate.h"
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"

char match_one_template(template_argument_list_t* arguments, template_argument_list_t* specialized, scope_t* st);

static scope_entry_t* determine_more_specialized(int num_matching_set, scope_entry_t** matching_set, scope_t* st);

scope_entry_t* solve_template(scope_entry_list_t* candidate_templates, template_argument_list_t* arguments, scope_t* st,
		char give_exact_match)
{
	scope_entry_t* result = NULL;

	// In the worst of the cases the chosen template will be the primary one
	scope_entry_list_t* iter = candidate_templates;

	while (iter != NULL)
	{
		if (iter->entry->kind == SK_TEMPLATE_PRIMARY_CLASS)
		{
			result = iter->entry;
		}
		iter = iter->next;
	}

	if (result == NULL)
	{
		internal_error("No primary template was found", 0);
	}

	// Now, for every specialization try to unificate its template_argument_list with ours
	iter = candidate_templates;

	int num_matching_set = 0;
	scope_entry_t** matching_set = NULL;

	while (iter != NULL)
	{
		if (iter->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
		{
			scope_entry_t* entry = iter->entry;

			template_argument_list_t* specialized = entry->type_information->type->template_arguments;

			// It is supposed that this will hold in correct code
			if (arguments->num_arguments != specialized->num_arguments)
			{
				internal_error("Template argument lists are not of equal length", 0);
			}

			if (match_one_template(arguments, specialized, st))
			{
				P_LIST_ADD(matching_set, num_matching_set, entry);
			}
		}

		iter = iter->next;
	}

	// There is no more than one candidate
	if (num_matching_set == 1)
	{
		result = matching_set[0];
	}
	else if (num_matching_set > 0)
	{
		result = determine_more_specialized(num_matching_set, matching_set, st);
	}

	if (give_exact_match 
			&& result != NULL)
	{
		// Result will be an exact match if it can be unified with the original
		if (result->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
		{

			template_argument_list_t* specialized = result->type_information->type->template_arguments;

			fprintf(stderr, "Checking match %p %p\n", specialized, arguments);

			if (!match_one_template(specialized, arguments, st))
			{
				return NULL;
			}
		}
		else
		{
			// A primary template cannot be exactly matched to something that
			// has invoked a template-id selection
			return NULL;
		}
	}

	return result;
}

// This function assumes that only one minimum will exist
static scope_entry_t* determine_more_specialized(int num_matching_set, scope_entry_t** matching_set, scope_t* st)
{
	scope_entry_t* min = matching_set[0];

	int i;
	for (i = 1; i < num_matching_set; i++)
	{
		scope_entry_t* current_entry = matching_set[i];

		template_argument_list_t* min_args = min->type_information->type->template_arguments;
		template_argument_list_t* current_args = current_entry->type_information->type->template_arguments;

		if (!match_one_template(min_args, current_args, st))
		{
			min = current_entry;

			if (!match_one_template(current_args, min_args, st))
			{
				internal_error("Ambiguous specialization instantiation\n", 0);
			}
		}
	}

	return min;
}

char match_one_template(template_argument_list_t* arguments, 
		template_argument_list_t* specialized, scope_t* st)
{
	int i;
	unification_set_t* unif_set = GC_CALLOC(1, sizeof(*unif_set));

	for (i = 0; i < arguments->num_arguments; i++)
	{
		template_argument_t* spec_arg = specialized->argument_list[i];
		template_argument_t* arg = arguments->argument_list[i];

		if (spec_arg->kind == arg->kind)
		{
			switch (spec_arg->kind)
			{
				case TAK_TYPE :
					{
						// unification_set_t* unif_set = GC_CALLOC(1, sizeof(*unif_set));
						if (!unificate_two_types(spec_arg->type, arg->type, st, &unif_set))
						{
							return 0;
						}
						break;
					}
				case TAK_NONTYPE :
					{
						// The key here is evaluating both expressions and checking they are the same
						if (spec_arg->expression == NULL)
						{
							internal_error("Expected an expression value for specialized argument", 0);
						}
						literal_value_t spec_arg_value = evaluate_constant_expression(spec_arg->expression, st);

						if (arg->expression == NULL)
						{
							internal_error("Expected an expression value for argument", 0);
						}

						literal_value_t arg_value = evaluate_constant_expression(arg->expression, st);

						if (!equal_literal_values(spec_arg_value, arg_value, st))
						{
							fprintf(stderr, "==> They are different\n");
							return 0;
						}
						else
						{
							fprintf(stderr, "==> They are the same!\n");
						}
						break;
					}
				default :
					{
						internal_error("Unknown template argument type %s", spec_arg->kind);
					}
			}
		}
		else
		{
			return 0;
		}
	}

	return 1;
}
