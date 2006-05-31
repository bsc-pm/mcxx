#include "cxx-overload.h"
#include "cxx-utils.h"
#include "cxx-typecalc.h"
#include "cxx-typeutils.h"

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


static char function_is_argument_viable(scope_entry_t* entry, 
		int num_args, scope_t* st)
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
			// Ensure parameters have default initializer
			parameter_info_t* param_for_last_arg = function_info->parameter_list[num_args];

			if (param_for_last_arg->default_argument == NULL)
			{
				// This is not a viable function because
				// it does not have a default argument here
				return 0;
			}
		}
		else // num_args > function_info->num_parameters
		{
			// The last one should be an ellipsized argument
			parameter_info_t* param_for_last_arg = function_info->parameter_list[function_info->num_parameters-1];

			if (!param_for_last_arg->is_ellipsis)
			{
				// This is not a viable function because
				// it does not have an ellipsized argument
				return 0;
			}
		}
	}

	// It looks callable with "num_arg" arguments
	return 1;
}

// That's a bit inefficient but argument lists are in general rather short
// (at most less than ten parameters)
static AST get_argument_i(AST argument_list, int i)
{
	AST iter;
	int j = 0;
	for_each_element(argument_list, iter)
	{
		if (i == j)
		{
			return ASTSon1(iter);
		}
		j++;
	}

	return NULL;
}

void build_standard_conversion_sequence(type_t* argument_type, type_t* parameter_type,
		one_implicit_conversion_sequence_t* sequence, scope_t* st)
{
	sequence->kind = ICS_STANDARD;

	// Do not consider cv-qualification
	if (equivalent_types(argument_type, parameter_type, st, CVE_IGNORE))
	{
		sequence->scs_category |= SCS_IDENTITY;
		return;
	}

	// type_t* argument_basic_type = base_type(argument_type);
	// type_t* parameter_basic_type = base_type(parameter_type);

	if ((sequence->scs_category & SCS_LVALUE_TRANSFORMATION) != SCS_LVALUE_TRANSFORMATION)
	{
		// Check array to pointer
		if ((argument_type->kind == TK_POINTER
					&& parameter_type->kind == TK_ARRAY)
				|| (argument_type->kind == TK_ARRAY
					&& parameter_type->kind == TK_POINTER))
		{
			// Normalize both to pointer
			if (argument_type->kind == TK_ARRAY)
			{
				argument_type->kind = TK_POINTER;
				argument_type->pointer = GC_CALLOC(1, sizeof(*(argument_type->pointer)));
				argument_type->pointer->pointee = argument_type->array->element_type;
			}
			else
			{
				parameter_type->kind = TK_POINTER;
				parameter_type->pointer = GC_CALLOC(1, sizeof(*(parameter_type->pointer)));
				parameter_type->pointer->pointee = parameter_type->array->element_type;
			}

			// And construct the proper conversion
			build_standard_conversion_sequence(argument_type, parameter_type, sequence, st);
			sequence->scs_category |= SCS_LVALUE_TRANSFORMATION;
			return;
		}

		// Check function to pointer
		if ((argument_type->kind == TK_FUNCTION
					&& parameter_type->kind == TK_POINTER
					&& parameter_type->pointer->pointee->kind == TK_FUNCTION)
				|| (parameter_type->kind == TK_FUNCTION
					&& argument_type->kind == TK_POINTER
					&& argument_type->pointer->pointee->kind == TK_FUNCTION))
		{
			// Normalize both to the function
			if (argument_type->kind == TK_POINTER)
			{
				argument_type = argument_type->pointer->pointee;
			}
			else
			{
				parameter_type = parameter_type->pointer->pointee;
			}

			sequence->scs_category |= SCS_LVALUE_TRANSFORMATION;
			// Construct the proper conversion
			build_standard_conversion_sequence(argument_type, parameter_type, sequence, st);
			return;
		}
	}

	if ((sequence->scs_category & SCS_PROMOTION) != SCS_PROMOTION
			&& (sequence->scs_category & SCS_CONVERSION) != SCS_CONVERSION)
	{
		// Check for integral promotions/conversions
		if (is_fundamental_type(argument_type)
				&& is_fundamental_type(parameter_type))
		{
			if (can_be_promoted_to_dest(argument_type, parameter_type))
			{
				sequence->scs_category |= SCS_PROMOTION;
			}
			else if (can_be_converted_to_dest(argument_type, parameter_type))
			{
				sequence->scs_category |= SCS_CONVERSION;
			}

			return;
		}

		// Check for pointer conversion
		if (argument_type->kind == TK_POINTER
				&& parameter_type->kind == TK_POINTER)
		{
			if (pointer_can_be_converted_to_dest(argument_type, parameter_type, st))
			{
				sequence->scs_category |= SCS_CONVERSION;
			}

			return;
		}
	}

	// No valid SCS found
	sequence->scs_category = SCS_UNKNOWN;
}

static one_implicit_conversion_sequence_t* 
build_one_implicit_conversion_sequence(scope_entry_t* entry, int n_arg, AST argument_list, scope_t* st)
{
	// Now compute an ICS for this argument.
	// First get the type of the argument expression
	AST argument = get_argument_i(argument_list, n_arg);
	
	type_set_t* type_result_set = calculate_expression_type(argument, st);

	if (type_result_set->num_types != 1)
	{
#warning Additional overload unsupported
		internal_error("Additional overload unsupported", 0);
	}

	one_implicit_conversion_sequence_t* result = GC_CALLOC(1, sizeof(*result));

	type_t* argument_type = type_result_set->types[0];
	type_t* parameter_type = entry->type_information->function->parameter_list[n_arg]->type_info;

	// Copy the types since this functio will modify them
	build_standard_conversion_sequence(copy_type(argument_type), 
			copy_type(parameter_type), result, st);

	if (result->scs_category == SCS_UNKNOWN)
	{
		// It looks that a standard conversion sequence is not possible
	}
}

static implicit_conversion_sequence_t* build_implicit_conversion_sequence(scope_entry_t* entry, int num_args, 
		AST argument_list, scope_t* st)
{
	int num_pars = entry->type_information->function->num_parameters;
	int i;

	implicit_conversion_sequence_t* result = GC_CALLOC(1, sizeof(*result));

	for (i = 0; i < num_args; i++)
	{
		one_implicit_conversion_sequence_t* one_ics;
		if (num_args >= num_pars)
		{
			one_ics = GC_CALLOC(1, sizeof(*one_ics));
			// This can only be by ellipsis nature
			one_ics->kind = ICS_ELLIPSIS;
			P_LIST_ADD(result->conversion, result->num_arg, one_ics);
		}
		else
		{
			one_implicit_conversion_sequence_t* one_ics = build_one_implicit_conversion_sequence(entry, i, argument_list, st);

			if (one_ics == NULL)
			{
				return NULL;
			}

			P_LIST_ADD(result->conversion, result->num_arg, one_ics);
		}
	}
}

static viable_function_list_t* calculate_viable_functions(scope_entry_list_t* candidate_functions, 
		int num_args, AST argument_list, scope_t* st)
{
	scope_entry_list_t* iter = candidate_functions;

	viable_function_list_t* result = NULL;

	while (iter != NULL)
	{
		if (function_is_argument_viable(iter->entry, num_args, st))
		{
			// Right. Now we have to build an ICS (implicit conversion sequence)
			// for this function
			implicit_conversion_sequence_t* ics = 
				build_implicit_conversion_sequence(iter->entry, num_args, argument_list, st);
			
			// viable_function_list_t* current_funct = GC_CALLOC(1, sizeof(*current_funct));
			// current_funct->entry = iter->entry;
			// current_funct->next = result;
			// result = current_funct;
		}
		iter = iter->next;
	}

	return result;
}

scope_entry_t* resolve_overload(scope_t* st, AST argument_list, 
		scope_entry_list_t* candidate_functions)
{
	// Early out for common cases
	if (candidate_functions == NULL
			|| candidate_functions->next == NULL)
	{
		return candidate_functions->entry;
	}

	int num_args = count_argument_list(argument_list);

	viable_function_list_t* viable_functions;

	viable_functions = calculate_viable_functions(candidate_functions, num_args, argument_list, st);
}

