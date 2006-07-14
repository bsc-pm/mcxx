#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-scope.h"
#include "cxx-solvetemplate.h"
#include "cxx-typeunif.h"
#include "cxx-typeutils.h"
#include "cxx-prettyprint.h"

char match_one_template(template_argument_list_t* arguments, 
		template_argument_list_t* specialized, scope_entry_t* specialized_entry, 
		scope_t* st, unification_set_t* unif_set);

static matching_pair_t* determine_more_specialized(int num_matching_set, matching_pair_t** matching_set, scope_t* st);

matching_pair_t* solve_template(scope_entry_list_t* candidate_templates, template_argument_list_t* arguments, scope_t* st, 
		char give_exact_match)
{
	matching_pair_t* result = NULL;

	// In the worst of the cases the chosen template will be the primary one
	scope_entry_list_t* iter = candidate_templates;

	char seen_primary_template = 0;
	while (iter != NULL && !seen_primary_template)
	{
		seen_primary_template |= (iter->entry->kind == SK_TEMPLATE_PRIMARY_CLASS);
		iter = iter->next;
	}

	if (!seen_primary_template)
	{
		internal_error("No primary template was found", 0);
	}

	// Now, for every specialization try to unificate its template_argument_list with ours
	iter = candidate_templates;

	int num_matching_set = 0;

	matching_pair_t** matching_set = NULL;

	while (iter != NULL)
	{
		scope_entry_t* entry = iter->entry;

		template_argument_list_t* specialized = entry->type_information->type->template_arguments;

		// It is supposed that this will hold in correct code
		if (arguments->num_arguments != specialized->num_arguments)
		{
			internal_error("Template argument lists are not of equal length", 0);
		}

		unification_set_t* unification_set = GC_CALLOC(1, sizeof(*unification_set));
		if (match_one_template(arguments, specialized, entry, st, unification_set))
		{
			matching_pair_t* match_pair = GC_CALLOC(1, sizeof(*match_pair));

			match_pair->entry = entry;
			match_pair->unif_set = unification_set;

			P_LIST_ADD(matching_set, num_matching_set, match_pair);
		}

		iter = iter->next;
	}

	// There is no more than one candidate
	if (num_matching_set == 1)
	{
		result = matching_set[0];
		if (give_exact_match)
		{
			template_argument_list_t* specialized = result->entry->type_information->type->template_arguments;

			fprintf(stderr, "Checking match with the unique %p %p\n", specialized, arguments);

			unification_set_t* unification_set = GC_CALLOC(1, sizeof(*unification_set));
			if (!match_one_template(specialized, arguments, NULL, st, unification_set))
			{
				return NULL;
			}
		}
		else
		{
			result = matching_set[0];
		}
	}
	else if (num_matching_set > 0)
	{
		fprintf(stderr, "More than one template can be selected, determining more specialized\n");
		result = determine_more_specialized(num_matching_set, matching_set, st);
		fprintf(stderr, "More specialized determined result=%p\n", result->entry);
	}

	if (give_exact_match 
			&& result != NULL)
	{
		// Result will be an exact match if it can be unified with the original
		template_argument_list_t* specialized = result->entry->type_information->type->template_arguments;

		fprintf(stderr, "Checking match %p %p\n", specialized, arguments);

		unification_set_t* unification_set = GC_CALLOC(1, sizeof(*unification_set));
		if (!match_one_template(specialized, arguments, NULL, st, unification_set))
		{
			return NULL;
		}
	}

	return result;
}

// This function assumes that only one minimum will exist
static matching_pair_t* determine_more_specialized(int num_matching_set, matching_pair_t** matching_set, scope_t* st)
{
	matching_pair_t* min = matching_set[0];

	fprintf(stderr, "Have to select the best template among %d templates\n", num_matching_set);

	int i;
	for (i = 1; i < num_matching_set; i++)
	{
		matching_pair_t* current_entry = matching_set[i];

		template_argument_list_t* min_args =
			min->entry->type_information->type->template_arguments;
		template_argument_list_t* current_args =
			current_entry->entry->type_information->type->template_arguments;

		unification_set_t* unification_set = GC_CALLOC(1, sizeof(*unification_set));

		if (!match_one_template(min_args, current_args, current_entry->entry, st, unification_set))
		{
			fprintf(stderr, "Template %p is more specialized than %p\n", current_entry->entry, min->entry);
			min = current_entry;

			unification_set_t* unification_set_check = GC_CALLOC(1, sizeof(*unification_set_check));
			if (!match_one_template(current_args, min_args, min->entry, st, unification_set_check))
			{
				internal_error("Ambiguous specialization instantiation\n", 0);
			}
		}
	}

	fprintf(stderr, "Best template selected\n");

	return min;
}

char match_one_template(template_argument_list_t* arguments, 
		template_argument_list_t* specialized, scope_entry_t* specialized_entry, 
		scope_t* st, unification_set_t* unif_set)
{
	int i;
	// unification_set_t* unif_set = GC_CALLOC(1, sizeof(*unif_set));
	
	if (specialized_entry != NULL)
	{
		fprintf(stderr, "=== Starting unification with %p\n", specialized_entry);
	}
	else
	{
		// For reverse unifications
		fprintf(stderr, "=== Starting unification\n");
	}

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
						if (!unificate_two_types(spec_arg->type, arg->type, st, &unif_set))
						{
							fprintf(stderr, "=== Unification failed\n");
							return 0;
						}
						break;
					}
				case TAK_NONTYPE :
					{
						// The key here is evaluating both expressions and checking they are the same
						if (spec_arg->argument_tree == NULL)
						{
							internal_error("Expected an expression value for specialized argument", 0);
						}
						literal_value_t spec_arg_value = evaluate_constant_expression(spec_arg->argument_tree, spec_arg->scope);

						if (arg->argument_tree == NULL)
						{
							internal_error("Expected an expression value for argument", 0);
						}

						literal_value_t arg_value = evaluate_constant_expression(arg->argument_tree, arg->scope);

						if (spec_arg_value.kind != LVK_DEPENDENT_EXPR)
						{
							if (!equal_literal_values(spec_arg_value, arg_value, st))
							{
								fprintf(stderr, "==> They are different\n");
								fprintf(stderr, "=== Unification failed\n");
								return 0;
							}
							else
							{
								fprintf(stderr, "==> They are the same!\n");
							}
						}
						else
						{
							unification_item_t* unif_item = GC_CALLOC(1, sizeof(*unif_item));
							// Get the name. This assumes that the expression
							// will be solely AST_EXPRESSION -> AST_SYMBOL
							
							if (ASTType(spec_arg->argument_tree) == AST_EXPRESSION)
							{
								AST symbol = ASTSon0(spec_arg->argument_tree);
								if (ASTType(symbol) == AST_SYMBOL)
								{
									char* name = ASTText(symbol);

									unification_item_t* unif_item = GC_CALLOC(1, sizeof(*unif_item));
									unif_item->parameter_name = name;
									unif_item->expression = arg->argument_tree;
									// Set to -1 to early detect errors
									unif_item->parameter_num = -1;
									unif_item->parameter_nesting = -1;

									P_LIST_ADD(unif_set->unif_list, unif_set->num_elems, unif_item);
								}
								else
								{
									internal_error("Expecting an AST_SYMBOL and found '%s'\n", ast_print_node_type(ASTType(symbol)));
								}
							}
							else
							{
								prettyprint(stderr, spec_arg->argument_tree);
								fprintf(stderr, "\n");
								internal_error("Expecting an AST_EXPRESSION and found '%s'\n", 
										ast_print_node_type(ASTType(spec_arg->argument_tree)));
							}
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
			fprintf(stderr, "=== Unification failed\n");
			return 0;
		}
	}

	fprintf(stderr, "=== Unification succeeded!\n");
	fprintf(stderr, "=== Unification details\n");
	for (i = 0; i < unif_set->num_elems; i++)
	{
		unification_item_t* unif_item = unif_set->unif_list[i];
		fprintf(stderr, "Parameter num: %d || Parameter nesting: %d || Parameter name: %s <- ",
				unif_item->parameter_num, unif_item->parameter_nesting, unif_item->parameter_name);
		if (unif_item->value != NULL)
		{
			fprintf(stderr, "[type] ");
			print_declarator(unif_item->value, st);
		}
		else if (unif_item->expression != NULL)
		{
			fprintf(stderr, "[expr] ");
			prettyprint(stderr, unif_item->expression);
		}
		else
		{
			fprintf(stderr, "(unknown)");
		}
		fprintf(stderr, "\n");
	}
	fprintf(stderr, "=== End of unification details\n");

	return 1;
}
