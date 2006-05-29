#include "cxx-typecalc.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"

/*
 * Calculates the type of an expression
 */
static type_set_t* create_type_set(type_t* t);
static type_t* usual_arithmetic_conversions(type_t* t1, type_t* t2, scope_t* st);

type_t* new_fundamental_type(temporary_status_t temporary_status)
{
	type_t* result = GC_CALLOC(1, sizeof(*result));

	result->kind = TK_DIRECT;
	result->type = GC_CALLOC(1, sizeof(*(result->type)));

	result->type->kind = STK_BUILTIN_TYPE;

	result->temporary_status = temporary_status;

	return result;
}

type_t* new_bool_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);

	result->type->builtin_type = BT_BOOL;

	return result;
}

type_t* new_double_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);

	result->type->builtin_type = BT_DOUBLE;

	return result;
}

type_t* new_float_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);

	result->type->builtin_type = BT_FLOAT;

	return result;
}

type_t* new_char_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);
	
	result->type->builtin_type = BT_CHAR;

	return result;
}

type_t* new_wchar_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);
	
	result->type->builtin_type = BT_WCHAR;

	return result;
}

type_t* new_const_wchar_pointer_type(temporary_status_t temporary_status)
{
	type_t* const_char = new_wchar_type(temporary_status);
	
	const_char->type->cv_qualifier |= CV_CONST;

	type_t* pointer = GC_CALLOC(1, sizeof(*pointer));
	pointer->kind = TK_POINTER;
	pointer->pointer = GC_CALLOC(1, sizeof(*(pointer->pointer)));
	pointer->pointer->pointee = const_char;
	
	return pointer;
}

type_t* new_const_char_pointer_type(temporary_status_t temporary_status)
{
	type_t* const_char = new_char_type(temporary_status);
	
	const_char->type->cv_qualifier |= CV_CONST;

	type_t* pointer = GC_CALLOC(1, sizeof(*pointer));
	pointer->kind = TK_POINTER;
	pointer->pointer = GC_CALLOC(1, sizeof(*(pointer->pointer)));
	pointer->pointer->pointee = const_char;
	
	return pointer;
}

type_t* new_int_type(temporary_status_t temporary_status)
{
	type_t* result = new_fundamental_type(temporary_status);

	result->type->builtin_type = BT_INT;

	return result;
}

char is_fundamental_type(type_t* t)
{
	return (t->kind == TK_DIRECT
			&& t->type->kind == STK_BUILTIN_TYPE);
}

type_set_t* calculate_expression_type(AST a, scope_t* st)
{
	switch (ASTType(a))
	{
		// Primaries
		case AST_BOOLEAN_LITERAL :
			{
				return create_type_set(new_bool_type(IS_TEMPORARY));
				break;
			}
		case AST_OCTAL_LITERAL :
		case AST_HEXADECIMAL_LITERAL :
		case AST_DECIMAL_LITERAL :
			{
				char* text = ASTText(a);
				char is_unsigned;
				char is_long;

				gather_integer_literal_suffix(text, &is_long, &is_unsigned);

				type_t* result = new_int_type(IS_TEMPORARY);

				result->type->is_unsigned = is_unsigned;
				result->type->is_long = is_long;

				return create_type_set(result);
				break;
			}
		case AST_FLOATING_LITERAL :
			{
				char* text = ASTText(a);
				char is_float;
				char is_long_double;
				gather_float_literal_suffix(text, &is_float, &is_long_double);

				type_t* result = NULL;
				if (!is_float)
				{
					result = new_double_type(IS_TEMPORARY);
					
					if (is_long_double)
					{
						result->type->is_long = is_long_double;
					}
				}
				else 
				{
					result = new_float_type(IS_TEMPORARY);
				}

				return create_type_set(result);
				break;
			}
		case AST_THIS_VARIABLE :
			{
#warning We need a way to fetch "this" symbol
				return NULL;
			}
		case AST_PARENTHESIZED_EXPRESSION :
			{
				return calculate_expression_type(ASTSon0(a), st);
			}
		case AST_SYMBOL :
		case AST_QUALIFIED_ID :
		case AST_DESTRUCTOR_ID :
		case AST_OPERATOR_FUNCTION_ID :
		case AST_QUALIFIED_OPERATOR_FUNCTION_ID :
			{
				scope_entry_list_t* result_list = query_id_expression(st, a, FULL_UNQUALIFIED_LOOKUP);

				if (result_list == NULL)
				{
					internal_error("Unknown symbol", 0);
				}

				type_set_t* result = NULL;

				// If this is a function name return all types associated with it
				if (result_list->entry->kind == SK_FUNCTION)
				{
					result = GC_CALLOC(1, sizeof(*result));
					scope_entry_list_t* iter = result_list;

					while (iter != NULL)
					{
						P_LIST_ADD(result->types, result->num_types, iter->entry->type_information);
						iter = iter->next;
					}
				}
				else
				{
					// Otherwise this should have only one type
					result = create_type_set(result_list->entry->type_information);
				}

				return result;
				break;
			}
		case AST_ARRAY_SUBSCRIPT :
			{
				type_set_t* array_type_set = calculate_expression_type(ASTSon0(a), st);

				if (array_type_set->num_types != 1)
				{
					internal_error("Unsupported set of types for this array subscript", 0);
				}

				type_t* array_type = array_type_set->types[0];

				if (array_type->kind != TK_ARRAY)
				{
					internal_error("Expected an array type at the left of the array subscript!\n", 0);
				}

				return create_type_set(array_type->array->element_type);
				break;
			}
		case AST_CLASS_MEMBER_ACCESS :
			{
				break;
			}
			// Binary operators
		case AST_LOGICAL_OR :
		case AST_LOGICAL_AND :
		case AST_BITWISE_OR :
		case AST_BITWISE_XOR :
		case AST_BITWISE_AND :
		case AST_DIFFERENT_OP :
		case AST_EQUAL_OP :
		case AST_LOWER_THAN :
		case AST_GREATER_THAN :
		case AST_GREATER_OR_EQUAL_THAN :
		case AST_LOWER_OR_EQUAL_THAN :
		case AST_SHL_OP :
		case AST_SHR_OP :
		case AST_ADD_OP :
		case AST_MINUS_OP :
		case AST_MULT_OP :
		case AST_MOD_OP :
		case AST_DIV_OP :
			{
				type_set_t* type_left = calculate_expression_type(ASTSon0(a), st);
				type_set_t* type_right = calculate_expression_type(ASTSon1(a), st);

				if (type_left->num_types == 1
						&& type_right->num_types == 1)
				{
					type_t* t1 = type_left->types[0];
					type_t* t2 = type_right->types[0];
					// Check if this may be a builtin operator invocation
					if (t1->kind == TK_DIRECT
							&& t1->type->kind == STK_BUILTIN_TYPE
							&& t2->kind == TK_DIRECT
							&& t2->type->kind == STK_BUILTIN_TYPE)
					{
						// This is a simple builtin invocation
						return usual_arithmetic_conversions(t1, t2, st);
					}
				}
#warning Missing overload support here
				internal_error("Unsupported overloading of binary operators", 0);
			}
		case AST_CAST_EXPRESSION :
			{
				internal_error("TODO", 0);
			}
		case AST_CONDITIONAL_EXPRESSION :
			{
				internal_error("TODO", 0);
			}
			// unary operators
		case AST_PLUS_OP :
			{
				internal_error("TODO", 0);
			}
		case AST_NOT_OP :
			{
				internal_error("TODO", 0);
			}
		case AST_NEG_OP :
			{
				internal_error("TODO", 0);
			}
		case AST_COMPLEMENT_OP :
			{
				internal_error("TODO", 0);
			}
		case AST_SIZEOF :
		case AST_SIZEOF_TYPEID :
			{
				internal_error("TODO", 0);
			}
		default :
			{
				internal_error("Unknown node '%s' in expression", ast_print_node_type(ASTType(a)));
			}
	}
}

static type_set_t* create_type_set(type_t* t)
{
	type_set_t* result = GC_CALLOC(1, sizeof(*result));

	result->num_types = 1;

	result->types = GC_CALLOC(1, sizeof(*result));
	result->types[0] = t;

	return result;
}

/*
 * Implements 5.9 [expr] "usual arithmetic conversions"
 */
static type_t* usual_arithmetic_conversions(type_t* t1, type_t* t2, scope_t* st)
{
	if (t1->kind != TK_DIRECT
			|| t2->kind != TK_DIRECT)
	{
		internal_error("Types in arithmetic conversions should be direct ones", 0);
	}

	if (equivalent_types(t1, t2, st))
	{
		return t1;
	}

#warning TODO - What about references to builtin types ?
	simple_type_t* st1 = t1->type;
	simple_type_t* st2 = t2->type;

	if (st1->kind != STK_BUILTIN_TYPE
			|| st2->kind != STK_BUILTIN_TYPE)
	{
#warning Deal the case for enumerators
		internal_error("Types in arithmetic conversions should be of builtin type", 0);
	}
			

	// - If either operand is of type long double, the other shall be converted to long double
	// Thus the result is of long double type
	if (st1->builtin_type == BT_DOUBLE
			&& st1->is_long)
	{
		return t1;
	}
	if (st2->builtin_type == BT_DOUBLE
			&& st2->is_long)
	{
		return t2;
	}

	// - otherwise, if either operand is double the other should be converted to double
	// Thus the result is of double type
	if (st1->builtin_type == BT_DOUBLE)
	{
		return t1;
	}
	if (st2->builtin_type == BT_DOUBLE)
	{
		return t2;
	}

	// - otherwise, if either operand is float the other shall be converted to float
	// Thus the result is of float type
	if (st1->builtin_type == BT_FLOAT)
	{
		return t1;
	}
	if (st2->builtin_type == BT_FLOAT)
	{
		return t2;
	}

	// - otherwise, integral promotions shall be performed on both operands
	// Integral promotions
	// Everything can be converted onto an int 
	// FIX - This may not hold for all architectures

	// - then if either operand is unsigned long the other shall be converted to unsigned long
	// Thus, this will yield an unsigned long value
	if (st1->builtin_type == BT_INT
			&& st1->is_long
			&& st1->is_unsigned)
	{
		return t1;
	}

	if (st2->builtin_type == BT_INT
			&& st2->is_long
			&& st2->is_unsigned)
	{
		return t2;
	}

	// - otherwise if one operand is a long int and the other unsigned int then if a long int can
	// represent all the values of an unsigned int the unsigned int shall be converted to a long int
	// otherwise both operands shall be converted to unsigned long int
	
	// Assume that a long int can hold any unsigned int. Otherwise
	// they should be converted to unsigned long int
	if (st1->builtin_type == BT_INT
			&& st1->is_long
			&& st2->builtin_type == BT_INT
			&& st2->is_unsigned)
	{
		return t1;
	}

	if (st2->builtin_type == BT_INT
			&& st2->is_long
			&& st1->builtin_type == BT_INT
			&& st1->is_unsigned)
	{
		return t2;
	}

	// - otherwise if either operand is long, the other shall be converted to long
	// Thus the result is long
	if (st1->builtin_type == BT_INT
			&& st1->is_long)
	{
		return t1;
	}

	if (st2->builtin_type == BT_INT
			&& st2->is_long)
	{
		return t1;
	}

	// - otherwise if either operand is unsigned, the other shall be converted to unsigned
	// Thus the result is unsigned
	if (st1->builtin_type == BT_INT
			&& st1->is_unsigned)
	{
		return t1;
	}
	
	if (st2->builtin_type == BT_INT
			&& st2->is_unsigned)
	{
		return t2;
	}

	// - the remaining case is when both operands are of type int
	return new_int_type(IS_TEMPORARY);
}

