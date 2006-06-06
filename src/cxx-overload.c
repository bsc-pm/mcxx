#include <string.h>
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

	// Outermost cv-qualification should be disregarded
	type_t* base_argument_type = base_type(argument_type);
	type_t* base_parameter_type = base_type(parameter_type);
	cv_qualifier_t cv_qualif_argument = base_argument_type->type->cv_qualifier;
	cv_qualifier_t cv_qualif_parameter = base_parameter_type->type->cv_qualifier;

#warning Consider references to const binding to const
	// If the outermost cv-qualification of arg is contained in outermost
	// cv-qualification of parameter it can be disregarded
	if ((((cv_qualif_argument & CV_CONST) != CV_CONST) 
				|| ((cv_qualif_parameter & CV_CONST) == CV_CONST))
			&& (((cv_qualif_argument & CV_VOLATILE) != CV_VOLATILE) 
				|| ((cv_qualif_parameter & CV_VOLATILE) == CV_VOLATILE)))
	{
		base_argument_type->type->cv_qualifier = CV_NONE;
		base_parameter_type->type->cv_qualifier = CV_NONE;
	}

	if (equivalent_types(argument_type, parameter_type, st, CVE_CONSIDER))
	{
		sequence->scs_category |= SCS_IDENTITY;
		return;
	}

	// Restore the cv_qualifiers
	base_argument_type->type->cv_qualifier = cv_qualif_argument;
	base_parameter_type->type->cv_qualifier = cv_qualif_parameter;

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

		/*
		 * T1 *a;
		 * T2 *b;
		 *
		 *   a = b;
		 *
		 * is valid (provided T1 and T2 are different types) only if T1 is a base class of T2
		 */
		if (argument_type->kind == TK_POINTER
				&& parameter_type->kind == TK_POINTER)
		{
			if (pointer_can_be_converted_to_dest(argument_type, parameter_type, st))
			{
				sequence->scs_category |= SCS_CONVERSION;
			}

			return;
		}

		/*
		 * T A::* p1;
		 * T B::* p2;
		 *
		 *   p2 = p1;
		 *
		 * is valid if A is a base class of B (just the opposite of the previous case)
		 */
		if (argument_type->kind == TK_POINTER_TO_MEMBER
				&& parameter_type->kind == TK_POINTER_TO_MEMBER)
		{
#warning Check that outermost cv-qualifiers are right here
			if (equivalent_types(argument_type->pointer->pointee, 
						parameter_type->pointer->pointee,
						st, CVE_CONSIDER))
			{
				if (argument_type->pointer->pointee_class != 
						parameter_type->pointer->pointee_class)
				{
					if (is_base_class_of(argument_type->pointer->pointee_class->type_information,
								parameter_type->pointer->pointee_class->type_information))
					{
						sequence->scs_category |= SCS_CONVERSION;
					}
				}
			}
		}
	}

	// No valid SCS found
	sequence->scs_category = SCS_UNKNOWN;
}

void build_user_defined_conversion_sequence(type_t* argument_type, type_t* parameter_type,
		one_implicit_conversion_sequence_t* sequence, scope_t* st)
{
	sequence->kind = ICS_USER_DEFINED;
	// First case,
	//
	// a) argument is of type class and has an operator that yields a value that can be
	//    SCS converted to parameter type
	
	if (is_class_type(argument_type))
	{
		// Check for conversion functions
#warning TODO - At the moment assume this conversion operator can be called now (if the \
		 object where it is applied is const it is not feasible to call \
		 non-const operator).
#warning TODO - Check that parameter_type is not a base of argument_type
		type_t* class_type = get_class_type(argument_type);

		one_implicit_conversion_sequence_t attempt_scs;

		int i;
		for (i = 0; i < class_type->type->class_info->num_conversion_functions; i++)
		{
			conversion_function_t* conv_funct = class_type->type->class_info->conversion_function_list[i];

			// Clear this attempt SCS
			memset(&attempt_scs, 0, sizeof(attempt_scs));

			// It is possible to build a SCS from the converted type to the parameter type ?
			build_standard_conversion_sequence(conv_funct->conversion_type, parameter_type, 
					&attempt_scs, st);

			if (attempt_scs.scs_category != SCS_UNKNOWN)
			{
				// It is possible
				if (sequence->udc_category != UDC_VALID)
				{
					sequence->udc_category = UDC_VALID;
					sequence->scs_category = attempt_scs.scs_category;
					sequence->udc_conv_funct = conv_funct;
				}
				else
				{
					// There is more than one conversion possible (it is a bit
					// strange this could happen with just conversion operators
					// but let's consider this case anyway)
					sequence->udc_category = UDC_AMBIGUOUS;
					sequence->scs_category = SCS_UNKNOWN;
					sequence->udc_constr_funct = NULL;
					sequence->udc_conv_funct = NULL;
				}
			}
		}
	}

	// Second case, it is possible to convert via a SCS the argument to the
	// parameter of a constructor of the parameter type (provided the parameter
	// is of class type) ?
	if (is_named_class_type(parameter_type))
	{
		type_t* class_type = get_class_type(parameter_type);
		one_implicit_conversion_sequence_t attempt_scs;

		int i;
		for (i = 0; i < class_type->type->class_info->num_constructors; i++)
		{
			scope_entry_t* constructor = class_type->type->class_info->constructor_list[i];
			type_t* constructor_type = constructor->type_information;
			
			// Clear this attempt SCS
			memset(&attempt_scs, 0, sizeof(attempt_scs));

			if (constructor_type->kind != TK_FUNCTION)
			{
				internal_error("The constructor has no functional type", 0);
			}

			// If this is a conversor constructor try to convert the argument to its
			// argument
			if (constructor_type->function->num_parameters == 1
					&& !constructor_type->function->is_explicit)
			{
				// It is possible to build a SCS from the converted type to the parameter type ?
				build_standard_conversion_sequence(argument_type, 
						constructor_type->function->parameter_list[0]->type_info, 
						&attempt_scs, st);

				if (attempt_scs.scs_category != SCS_UNKNOWN)
				{
					// It is possible an SCS to this parameter
					if (sequence->udc_category != UDC_VALID)
					{
						sequence->udc_category = UDC_VALID;
						sequence->scs_category = attempt_scs.scs_category;
						sequence->udc_constr_funct = constructor;
					}
					else
					{
						// There is more than one conversion possible
						sequence->udc_category = UDC_AMBIGUOUS;
						sequence->scs_category = SCS_UNKNOWN;
						sequence->udc_constr_funct = NULL;
						sequence->udc_conv_funct = NULL;
					}
				}
			}
		}
	}
}

static one_implicit_conversion_sequence_t* 
build_one_implicit_conversion_sequence(scope_entry_t* entry, int n_arg, AST argument_list, scope_t* st)
{
	// Now compute an ICS for this argument.
	// First get the type of the argument expression
	AST argument = get_argument_i(argument_list, n_arg);

	calculated_type_t* type_result_set = calculate_expression_type(argument, st);

	if (type_result_set->num_types != 1)
	{
#warning Additional overload unsupported
		internal_error("Additional overload unsupported", 0);
	}

	one_implicit_conversion_sequence_t* result = GC_CALLOC(1, sizeof(*result));

	type_t* argument_type = type_result_set->types[0];
	type_t* parameter_type = entry->type_information->function->parameter_list[n_arg]->type_info;

	// Copy the types since this function will modify them
	build_standard_conversion_sequence(copy_type(argument_type), 
			copy_type(parameter_type), result, st);

	if (result->scs_category != SCS_UNKNOWN)
	{
		return result;
	}

	// There might be a user defined conversion sequence provided that
	//
	//  a) the argument is of class type and has an operator that yields a type
	//     that can be SCS converted to the parameter type.
	//
	//     struct A
	//     {
	//        operator T&();
	//     };
	//
	//     void f(T k);
	//     void f(char* d);
	//
	//     void g()
	//     {
	//        A b;
	//        f(b); <-- Will call f(T);
	//     }
	//
	//  b) or otherwise, the parameter is of class type that and has am implicit constructor that 
	//     has an argument of a type that can be converted to with a SCS from the argument
	//
	//     struct A
	//     {
	//        A(const char* c);
	//     };
	//
	//     void f(A a);
	//     void f(double d);
	//
	//     void g()
	//     {
	//        char* c;
	//        f(c); <-- Will call f(A);
	//     }
	//
	//  It is possible to find an ambiguous user defined conversions in the
	//  case where is possible to convert a type to T1 and T2 and there are
	//  f(T1) and f(T2)
	//
	//     struct A
	//     {
	//        operator T1();
	//     };
	//
	//     struct T2
	//     {
	//        T2(A a);
	//     };
	//
	//     void f(T1 t1);
	//     void f(T2 t2);
	//
	//     void g()
	//     {
	//        A a;
	//        f(a); <-- f(T1) or f(T2) ?
	//     }

	// Clear the result
	memset(result, 0, sizeof(*result));

	build_user_defined_conversion_sequence(copy_type(argument_type), 
			copy_type(parameter_type), result, st);

	if (result->udc_category == UDC_UNKNOWN)
	{
		// There is no valid UDC nor SCS
		return NULL;
	}
	else
	{
		return result;
	}
}

static implicit_conversion_sequence_t* build_implicit_conversion_sequence(scope_entry_t* entry, int num_args, 
		AST argument_list, scope_t* st)
{
	int num_pars = entry->type_information->function->num_parameters;
	int i;

	implicit_conversion_sequence_t* result = GC_CALLOC(1, sizeof(*result));

	// Consider "this" pseudoargument and its associated pseudo parameter
	// If the considered function is member
#if 0
	if (entry->type_information->function->is_member)
	{
		// Search "this"
		scope_entry_list_t* this_query = query_unqualified_name(st, "this");
		if (this_query == NULL)
		{
			if (!entry->type_information->function->is_static)
			{
				// The function we are trying to call is a non static member
				// that cannot be called in this context
				return NULL;
			}
			else
			{
				// Otherwise we are trying to call a static member, this is fine
				// implicit argument is ignored here
			}
		}
		else
		{
			scope_entry_t* this_variable = this_query->entry;
			type_t* this_variable_type = this_variable->type_information;
			type_t* this_value_type = this_variable_type->pointer->pointee;
			type_t* considered_function_type = entry->type_information;
			// This is a pointer
			// If it points to a const object then the function should be const too,
			// otherwise it is not viable
			if ((this_value_type->type->cv_qualifier & CV_CONST) == CV_CONST)
			{
				if ((considered_function_type->function->cv_qualifier & CV_CONST) != CV_CONST)
				{
					return NULL;
				}
			}

			if ((this_value_type->type->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
			{
				if ((considered_function_type->function->cv_qualifier & CV_VOLATILE) != CV_VOLATILE)
				{
					return NULL;
				}
			}
		}
	}
#endif

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

	return result;
}

static char is_better_standard_conversion_sequence(one_implicit_conversion_sequence_t* s1,
		one_implicit_conversion_sequence_t* s2)
{
	if (s1->kind != ICS_STANDARD
			|| s2->kind != ICS_STANDARD)
	{
		internal_error("This function only operates on standard sequences", 0);
	}

	// If s1 is a proper subsequence of s2 then s1 is better
	if (s1->scs_category != s2->scs_category)
	{
		// The identity always is a subset of any non-identity standard
		// conversion
		if (s1->scs_category == SCS_IDENTITY)
		{
			return 1;
		}

		// Every conversion in s2 is in s1. Thus s1 is better
		if ((s1->scs_category & s2->scs_category) == s1->scs_category)
		{
			return 1;
		}

		// The rank of s1 is better than s2 one
		// if s2 has conversion rank
		if ((s2->scs_category & SCS_CONVERSION) == SCS_CONVERSION)
		{
			// If s1 does not have conversion rank, it is better
			if ((s1->scs_category & SCS_CONVERSION) != SCS_CONVERSION)
			{
				return 1;
			}
		}
		// if s2 has promotion rank
		else if ((s2->scs_category & SCS_PROMOTION) == SCS_PROMOTION)
		{
			// If s1 is an exact match, it is better
			if (((s1->scs_category & SCS_IDENTITY) == SCS_IDENTITY)
					|| ((s1->scs_category & SCS_LVALUE_TRANSFORMATION) == SCS_LVALUE_TRANSFORMATION)
					|| ((s1->scs_category & SCS_QUALIFICATION_ADJUSTMENT) == SCS_QUALIFICATION_ADJUSTMENT))
			{
				return 1;
			}
		}
#warning Missing case for less cv-qualified priority in references? [13.3.3.2/3]
	}
#warning Missing case for standard conversion sequences [13.3.3.2/4]

	return 0;
}

static char is_better_conversion_sequence(one_implicit_conversion_sequence_t* s1,
		one_implicit_conversion_sequence_t* s2)
{
	// A standard conversions equence is a better conversion sequence than an
	// ellipsis conversion
	if (s1->kind == ICS_STANDARD
			&& (s2->kind == ICS_USER_DEFINED
				|| s2->kind == ICS_ELLIPSIS))
	{
		return 1;
	}

	// A user-defined conversion sequence is a better conversion sequence than
	// an ellipsis conversion sequence
	if (s1->kind == ICS_USER_DEFINED
			&& s2->kind == ICS_ELLIPSIS)
	{
		return 1;
	}

	if (s1->kind == s2->kind)
	{
		if (s1->kind == ICS_STANDARD)
		{
			return is_better_standard_conversion_sequence(s1, s2);
		}
		else if (s1->kind == ICS_USER_DEFINED)
		{
			
			if (s1->udc_category != UDC_AMBIGUOUS
					&& s2->udc_category != UDC_AMBIGUOUS)
			{
				// A user defined conversion is better than another user defined
				// conversion if they contain the same user-defined conversion
				// function or constructor and if the second standard conversion
				// sequence of U1 is better than the second standard conversion
				// sequence.
				if ((s1->udc_conv_funct != NULL && s2->udc_conv_funct != NULL
							&& s1->udc_conv_funct == s2->udc_conv_funct)
						|| (s1->udc_constr_funct != NULL && s2->udc_constr_funct != NULL
							&& s1->udc_constr_funct == s2->udc_constr_funct))
				{
					one_implicit_conversion_sequence_t scs1 = *s1;
					one_implicit_conversion_sequence_t scs2 = *s2;

					scs1.kind = ICS_STANDARD;
					scs2.kind = ICS_STANDARD;

					return is_better_standard_conversion_sequence(&scs1, &scs2);
				}
			}
		}
	}

	return 0;
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
			
			if (ics != NULL)
			{
				viable_function_list_t* current_funct = GC_CALLOC(1, sizeof(*current_funct));
				current_funct->entry = iter->entry;
				current_funct->ics_num_args = num_args;
				current_funct->ics = ics;
				current_funct->next = result;
				result = current_funct;
			}
		}
		iter = iter->next;
	}

	return result;
}


static char is_better_viable_function(viable_function_list_t* f1,
		viable_function_list_t* f2)
{
	/*
	 * A viable function f1 is defined to be a better function than another viable
	 * function f2 if for all arguments "i", ics[i](f1) is not a worse conversion
	 * sequence than ics[i](f2) and
	 *
	 * a) for some argument j, ics[j](f1) is better than ics[j](f2), or if not
	 * that
	 *
	 * b) f1 is a non template function and f2 is a function template
	 * specialization, or if not that
	 *
	 * c) f1 and f2 are function template specializations, and the function
	 * template for f1 is more specialized than the template for f2, according
	 * to the partial ordering rules described in 14.5.5.2, or if not that
	 *
	 * d) the context is an initialization by user-defined conversion and the
	 * standard conversion sequence from the return type of F1 to the
	 * destination type (the entity being initialized) is a better conversion
	 * sequence than the standard conversion sequence from the return type of
	 * f2 to the destination type. (rofi: I think this one is subsumed by the
	 * order of a user defined conversion)
	 *
	 */

	char some_is_worse = 0;
	char some_is_better = 0;
	int i;
	for (i = 0; i < f1->ics_num_args; i++)
	{
		if (is_better_conversion_sequence(f1->ics->conversion[i], f2->ics->conversion[i]))
		{
			some_is_better = 1;
		}
		else
		{
			some_is_worse = 1;
		}
	}

	if (some_is_worse)
	{
		// Should be no worse for any of the arguments
		return 0;
	}

	// This is a)
	if (some_is_better)
	{
		return 1;
	}

	// This is b)
	if (f1->entry->kind == SK_FUNCTION
			&& f2->entry->kind == SK_TEMPLATE_FUNCTION)
	{
		return 1;
	}

	// This is c)
#warning Missing partial template specialization ordering [13.3.3/1 and 14.5.5.2]
#if 0
	if (f1->entry->kind == SK_TEMPLATE_FUNCTION
			&& f2->entry->kind == SK_TEMPLATE_FUNCTION
			&& is_function_template_more_specialized(f1, f2))
	{
		return 1;
	}
#endif 

	// This is d)
	/*
	    struct A {
	      A();
	      operator int();
	      operator double();
	    };
	    
	    A a
	    
	    int i = a; // A::operator int() will be used to convert "a" to int since A::operator double()
	               // would imply a conversion from double to int and A::operator int() does not
	               // requires this conversion
	    float x = a; // Ambiguity, both A::operator int() and A::operator double() can be
	                 // applied to yield a value that will be further converted to float.
	 */
#warning Missing ICS best viable definition in the context of an initialization [13.3.3/1]

	// Otherwise, this is not better
	return 0;
}

static scope_entry_t* choose_best_viable_function(viable_function_list_t* viable_functions)
{
	/*
	 * The algorithm is as follows.
	 * a) Find a candidate to be the best one. This is a classical maximum search.
	 * b) Recheck this candidate is really the best with all functions.
	 *
	 * According to the standard a) will yield a F function that might be the best one
	 * but we have to ensure that there is no G that will be better than G, otherwise
	 * no best viable function exists
	 */
	if (viable_functions == NULL)
	{
		return NULL;
	}

	viable_function_list_t* result = viable_functions;
	viable_function_list_t* iter = viable_functions->next;

	while (iter != NULL)
	{
		if (is_better_viable_function(result, iter))
		{
			iter = result;
		}

		iter = iter->next;
	}

	// Now check is really the best
	iter = viable_functions;
	char is_still_the_best = 1;
	while ((iter != NULL) && is_still_the_best)
	{
		if (iter != result)
		{
			is_still_the_best = is_better_viable_function(result, iter);
		}

		iter = iter->next;
	}

	if (!is_still_the_best)
	{
		return NULL;
	}
	else
	{
		return result->entry;
	}
}

// This function returns the unique overload or NULL if it is unavailable or it
// is ambiguous
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

	scope_entry_t* best_viable_function = choose_best_viable_function(viable_functions);

	return best_viable_function;
}

