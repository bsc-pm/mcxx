#include "cxx-overload.h"
#include "cxx-utils.h"

static int count_argument_list(AST argument_list);
static char function_is_viable(scope_entry_t* entry, int num_args);

scope_entry_t* resolve_overload(scope_t* st, AST argument_list, 
		scope_entry_list_t* candidate_functions)
{
	// Early out for common cases
	if (candidate_functions == NULL
			|| candidate_functions->next == NULL)
	{
		return candidate_functions;
	}
}

scope_entry_list_t* calculate_viable_functions(scope_entry_list_t* candidate_functions, int num_args)
{
	scope_entry_list_t* iter = candidate_functions;

	scope_entry_list_t* result = NULL;

	while (iter != NULL)
	{
		if (function_is_viable(iter->entry, num_args))
		{
			scope_entry_list_t* current = GC_CALLOC(1, sizeof(*current));

			current->entry = iter->entry;
			current->next = result;

			result = current;
		}
		iter = iter->next;
	}

	return result;
}

static char function_is_viable(scope_entry_t* entry, int num_args)
{
	if (entry->kind != SK_FUNCTION)
	{
		internal_error("Expecting a symbol function here!", 0);
	}

	function_info_t* function_info = entry->type_information->function;
	if (num_args != function_info->num_parameters)
	{
		// Number of arguments is different to number of parameters
		if (num_args < function_info->num_parameters)
		{
#warning Search for the ellipsis
		}
		else // num_args > function_info
		{
			// Ensure parameters have default initializer
		}
	}
	else
	{
		// Every parameter will have an argument
		return 1;
	}
}

static int count_argument_list(AST argument_list)
{
	if (argument_list == NULL)
	{
		return 0;
	}
	else
	{
		int i = 0;
		AST iter;
		for_each_element(argument_list, iter)
		{
			i++;
		}
		return i;
	}
}
