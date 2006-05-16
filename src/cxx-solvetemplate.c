#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include "cxx-utils.h"
#include "cxx-symtab.h"
#include "cxx-solvetemplate.h"
#include "cxx-typeunif.h"

char match_one_template(template_argument_list_t* arguments, template_argument_list_t* specialized, symtab_t* st);

static symtab_entry_t* determine_more_specialized(int num_matching_set, symtab_entry_t** matching_set, symtab_t* st);

symtab_entry_t* solve_template(symtab_entry_list_t* candidate_templates, template_argument_list_t* arguments, symtab_t* st)
{
	symtab_entry_t* result = NULL;

	// In the worst of the cases the chosen template will be the primary one
	symtab_entry_list_t* iter = candidate_templates;

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
	// TODO - Consider the case several specializations matching -> we have to sort them
	// at least finding the more defined specialization
	
	iter = candidate_templates;

	int num_matching_set = 0;
	symtab_entry_t** matching_set = NULL;

	while (iter != NULL)
	{
		if (iter->entry->kind == SK_TEMPLATE_SPECIALIZED_CLASS)
		{
			symtab_entry_t* entry = iter->entry;

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
		// internal_error("Partial ordering of matching specializations not done yet", 0);
		result = determine_more_specialized(num_matching_set, matching_set, st);
	}

	return result;
}

// This function assumes that only one minimum will exist
static symtab_entry_t* determine_more_specialized(int num_matching_set, symtab_entry_t** matching_set, symtab_t* st)
{
	symtab_entry_t* min = matching_set[0];

	int i;
	for (i = 1; i < num_matching_set; i++)
	{
		symtab_entry_t* current_entry = matching_set[i];

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
		template_argument_list_t* specialized, symtab_t* st)
{
	int i;
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
						unification_set_t* unif_set = GC_CALLOC(1, sizeof(*unif_set));
						if (!unificate_two_types(spec_arg->type, arg->type, st, &unif_set))
						{
							return 0;
						}
						break;
					}
				case TAK_NONTYPE :
					{
						internal_error("Nontype parameter matching not implemented yet!\n", 0);
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
