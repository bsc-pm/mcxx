#include <stdlib.h>
#include <string.h>
#include "cxx-typecalc.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"

/*
 * Calculates the type of an expression
 */
static type_set_t* create_type_set(type_t* t);
static type_t* usual_arithmetic_conversions(type_t* t1, type_t* t2, scope_t* st);

type_t* new_fundamental_type(void)
{
	type_t* result = GC_CALLOC(1, sizeof(*result));

	result->kind = TK_DIRECT;
	result->type = GC_CALLOC(1, sizeof(*(result->type)));

	result->type->kind = STK_BUILTIN_TYPE;

	return result;
}

type_t* new_bool_type(void)
{
	type_t* result = new_fundamental_type();

	result->type->builtin_type = BT_BOOL;

	return result;
}

type_t* new_double_type(void)
{
	type_t* result = new_fundamental_type();

	result->type->builtin_type = BT_DOUBLE;

	return result;
}

type_t* new_float_type(void)
{
	type_t* result = new_fundamental_type();

	result->type->builtin_type = BT_FLOAT;

	return result;
}

type_t* new_char_type(void)
{
	type_t* result = new_fundamental_type();
	
	result->type->builtin_type = BT_CHAR;

	return result;
}

type_t* new_wchar_type(void)
{
	type_t* result = new_fundamental_type();
	
	result->type->builtin_type = BT_WCHAR;

	return result;
}

type_t* new_const_wchar_pointer_type(void)
{
	type_t* const_char = new_wchar_type();
	
	const_char->type->cv_qualifier |= CV_CONST;

	type_t* pointer = GC_CALLOC(1, sizeof(*pointer));
	pointer->kind = TK_POINTER;
	pointer->pointer = GC_CALLOC(1, sizeof(*(pointer->pointer)));
	pointer->pointer->pointee = const_char;
	
	return pointer;
}

type_t* new_const_char_pointer_type(void)
{
	type_t* const_char = new_char_type();
	
	const_char->type->cv_qualifier |= CV_CONST;

	type_t* pointer = GC_CALLOC(1, sizeof(*pointer));
	pointer->kind = TK_POINTER;
	pointer->pointer = GC_CALLOC(1, sizeof(*(pointer->pointer)));
	pointer->pointer->pointee = const_char;
	
	return pointer;
}

type_t* new_int_type(void)
{
	type_t* result = new_fundamental_type();

	result->type->builtin_type = BT_INT;

	return result;
}

char is_fundamental_type(type_t* t)
{
	// Advance over typedefs
	while (t->kind == TK_DIRECT
			&& t->type->kind == STK_TYPEDEF)
	{
		t = t->type->aliased_type;
	}

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
				return create_type_set(new_bool_type());
			}
		case AST_OCTAL_LITERAL :
		case AST_HEXADECIMAL_LITERAL :
		case AST_DECIMAL_LITERAL :
			{
				char* text = ASTText(a);
				char is_unsigned;
				char is_long;

				gather_integer_literal_suffix(text, &is_long, &is_unsigned);

				type_t* result = new_int_type();

				result->type->is_unsigned = is_unsigned;
				result->type->is_long = is_long;

				return create_type_set(result);
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
					result = new_double_type();
					
					if (is_long_double)
					{
						result->type->is_long = is_long_double;
					}
				}
				else 
				{
					result = new_float_type();
				}

				return create_type_set(result);
			}
		case AST_THIS_VARIABLE :
			{
				scope_entry_list_t* this_symbol;

				this_symbol = query_in_symbols_of_scope(st, "this");

				return create_type_set(this_symbol->entry->type_information);
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
			}
			// Postfix expressions
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
			}
		case AST_POINTER_CLASS_MEMBER_ACCESS :
		case AST_CLASS_MEMBER_ACCESS :
			{
				type_set_t* class_type_set = calculate_expression_type(ASTSon0(a), st);

				if (class_type_set->num_types != 1)
				{
					internal_error("Unsupported set of types for this member access", 0);
				}

				type_t* class_type = class_type_set->types[0];
				
				// Jump over one pointer type
				if (ASTType(a) == AST_POINTER_CLASS_MEMBER_ACCESS)
				{
					// Advance over typedefs
					while (class_type->kind == TK_DIRECT
							&& class_type->type->kind == STK_TYPEDEF)
					{
						class_type = class_type->type->aliased_type;
					}

					if (class_type->kind != TK_POINTER)
					{
						internal_error("Postfix expression does not denote a pointer", 0);
					}

					class_type = class_type->pointer->pointee;
				}
				
				// Advance over typedefs
				while (class_type->kind == TK_DIRECT
						&& class_type->type->kind == STK_TYPEDEF)
				{
					class_type = class_type->type->aliased_type;
				}

				// If this names a user defined type get the original type
				if (class_type->kind == TK_DIRECT
						&& class_type->type->kind == STK_USER_DEFINED)
				{
					class_type = class_type->type->user_defined_type->type_information;
				}

				if (class_type->kind != TK_DIRECT
						|| class_type->type->kind != STK_CLASS)
				{
					internal_error("This expression does not denote a class", 0);
				}

				scope_t* inner_scope = class_type->type->class_info->inner_scope;

				AST id_expression = ASTSon1(a);
				scope_entry_list_t* result_list = query_id_expression(inner_scope, id_expression, NOFULL_UNQUALIFIED_LOOKUP);

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
			}
		case AST_FUNCTION_CALL : 
			{
				AST function_expr = ASTSon0(a);
				// AST argument_list = ASTSon1(a);

				type_set_t* function_expr_type = calculate_expression_type(function_expr, st);

				if (function_expr_type->num_types > 1)
				{
					internal_error("Overload resolution still not implemented", 0);
				}

				type_t* function_type = function_expr_type->types[0];

				if (function_type->kind != TK_FUNCTION)
				{
					internal_error("Expression does not denote a function\n", 0);
				}

				return create_type_set(function_type->function->return_type);
			}
		case AST_DYNAMIC_CAST :
		case AST_STATIC_CAST :
		case AST_REINTERPRET_CAST :
		case AST_CONST_CAST :
		case AST_CAST_EXPRESSION :
			{
				AST type_id = ASTSon0(a);

				// A type_id is a type_specifier_seq followed by an optional abstract
				// declarator
				AST type_specifier_seq = ASTSon0(type_id);
				AST abstract_decl = ASTSon1(type_id);

				// A type_specifier_seq is essentially a subset of a
				// declarator_specifier_seq so we can reuse existing functions
				simple_type_t* type_info = NULL;
				gather_decl_spec_t gather_info;
				memset(&gather_info, 0, sizeof(gather_info));

				build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info);

				type_t* declarator_type;
				if (abstract_decl != NULL)
				{
					build_scope_declarator(abstract_decl, st, &gather_info, type_info, &declarator_type);
				}
				else
				{
					declarator_type = simple_type_to_type(type_info);
				}

				return create_type_set(declarator_type);
			}
			// Binary operators that by default return bool values
		case AST_DIFFERENT_OP :
		case AST_EQUAL_OP :
		case AST_LOWER_THAN :
		case AST_GREATER_THAN :
		case AST_GREATER_OR_EQUAL_THAN :
		case AST_LOWER_OR_EQUAL_THAN :
		case AST_LOGICAL_OR :
		case AST_LOGICAL_AND :
			{
				type_set_t* type_left = calculate_expression_type(ASTSon0(a), st);
				type_set_t* type_right = calculate_expression_type(ASTSon1(a), st);

				if (type_left->num_types == 1
						&& type_right->num_types == 1)
				{
					type_t* t1 = type_left->types[0];
					type_t* t2 = type_right->types[0];
					// Check if this may be a builtin operator invocation
					if (is_fundamental_type(t1)
							&& is_fundamental_type(t2))
					{
						// This is a simple builtin invocation
						return create_type_set(new_bool_type());
					}

					internal_error("Unsupported overloading of binary operators", 0);
				}

				internal_error("Unsupported overloading of binary operators", 0);
			}
		case AST_NEW_EXPRESSION :
		case AST_NEW_TYPE_ID :
			{
				AST type_id = ASTSon2(a);

				// A type_id is a type_specifier_seq followed by an optional abstract
				// declarator
				AST type_specifier_seq = ASTSon0(type_id);
				AST abstract_decl = ASTSon1(type_id);

				// A type_specifier_seq is essentially a subset of a
				// declarator_specifier_seq so we can reuse existing functions
				simple_type_t* type_info = NULL;
				gather_decl_spec_t gather_info;
				memset(&gather_info, 0, sizeof(gather_info));

				build_scope_decl_specifier_seq(type_specifier_seq, st, &gather_info, &type_info);

				type_t* declarator_type;
				if (abstract_decl != NULL)
				{
					build_scope_declarator(abstract_decl, st, &gather_info, type_info, &declarator_type);
				}
				else
				{
					declarator_type = simple_type_to_type(type_info);
				}

				// If innermost type is an array, do not convert it, otherwise
				// create a pointer to the declared type
				if (declarator_type->kind == TK_ARRAY)
				{
					return create_type_set(declarator_type);
				}
				else
				{
					type_t* pointer_to = GC_CALLOC(1, sizeof(*pointer_to));
					pointer_to->kind = TK_POINTER;
					pointer_to->pointer = GC_CALLOC(1, sizeof(*(pointer_to->pointer)));
					pointer_to->pointer->pointee = declarator_type;

					return create_type_set(pointer_to);
				}
				break;
			}
			// Binary operators
		case AST_BITWISE_OR :
		case AST_BITWISE_XOR :
		case AST_BITWISE_AND :
		case AST_SHL_OP :
		case AST_SHR_OP :
		case AST_ADD_OP :
		case AST_MINUS_OP :
		case AST_MULT_OP :
		case AST_MOD_OP :
		case AST_DIV_OP :
			{
#warning Missing overload support here
				type_set_t* type_left = calculate_expression_type(ASTSon0(a), st);
				type_set_t* type_right = calculate_expression_type(ASTSon1(a), st);

				if (type_left->num_types == 1
						&& type_right->num_types == 1)
				{
					type_t* t1 = type_left->types[0];
					type_t* t2 = type_right->types[0];
					// Check if this may be a builtin operator invocation
					if (is_fundamental_type(t1)
							&& is_fundamental_type(t2))
					{
						// This is a simple builtin invocation
						return create_type_set(usual_arithmetic_conversions(t1, t2, st));
					}

					internal_error("Unsupported overloading of binary operators", 0);
				}
				internal_error("Unsupported overloading of binary operators", 0);
			}
		case AST_ASSIGNMENT :
		case AST_MUL_ASSIGNMENT :
		case AST_DIV_ASSIGNMENT :
		case AST_ADD_ASSIGNMENT :
		case AST_SUB_ASSIGNMENT :
		case AST_SHL_ASSIGNMENT :
		case AST_SHR_ASSIGNMENT :
		case AST_AND_ASSIGNMENT :
		case AST_OR_ASSIGNMENT :
		case AST_XOR_ASSIGNMENT :
		case AST_MOD_ASSIGNMENT :
			{
#warning Missing overload support here
				type_set_t* type_left = calculate_expression_type(ASTSon0(a), st);
				type_set_t* type_right = calculate_expression_type(ASTSon1(a), st);

				if (type_left->num_types == 1
						&& type_right->num_types == 1)
				{
					type_t* t1 = type_left->types[0];
					type_t* t2 = type_right->types[0];
					// Check if this may be a builtin operator invocation
					if (is_fundamental_type(t1)
							&& is_fundamental_type(t2))
					{
						return create_type_set(usual_arithmetic_conversions(t1, t2, st));
					}

					internal_error("Unsupported overloading of assignment expressions", 0);
				}

				internal_error("Unsupported overloading of assignment expressions", 0);
			}
		case AST_PREINCREMENT :
		case AST_POSTINCREMENT :
		case AST_PREDECREMENT :
		case AST_POSTDECREMENT :
			{
#warning Missing overload support here
				type_set_t* result = calculate_expression_type(ASTSon0(a), st);

				if (result->num_types == 1)
				{
					if (is_fundamental_type(result->types[0]))
					{
						return result;
					}
					else
					{
						internal_error("Unsupported overloading of ++ or --", 0);
					}
				}

				internal_error("Unsupported overloading of ++ or --", 0);
			}
		case AST_CONDITIONAL_EXPRESSION :
			{
				// Assume ASTSon1 and ASTSon2 will yield the same type
#warning Add support for standard conversions
				type_set_t* result = calculate_expression_type(ASTSon1(a), st);
				return result;
			}
			// Special unary operators
		case AST_DERREFERENCE :
			{
#warning Missing overload support here
				AST cast_expr = ASTSon1(a);

				type_set_t* cast_expr_set = calculate_expression_type(cast_expr, st);

				if (cast_expr_set->num_types != 1)
				{
					internal_error("Overloaded function pointer derreference unsupported", 0);
				}

				type_t* pointer_type = cast_expr_set->types[0];

				if (pointer_type->kind != TK_POINTER)
				{
					internal_error("Overloading of operator* still unsupported", 0);
				}
				else
				{
					return create_type_set(pointer_type->pointer->pointee);
				}
				break;
			}
		case AST_REFERENCE :
			{
#warning Missing overload support here
#warning Handle the case where a function is "over-referenced"
				AST cast_expr = ASTSon1(a);

				type_set_t* cast_expr_set = calculate_expression_type(cast_expr, st);

				if (cast_expr_set->num_types != 1)
				{
					internal_error("Overloaded function pointer reference unsupported", 0);
				}

				type_t* object_type = cast_expr_set->types[0];

				if (is_fundamental_type(object_type))
				{
					type_t* pointer_to = GC_CALLOC(1, sizeof(*pointer_to));
					pointer_to->kind = TK_POINTER;
					pointer_to->pointer = GC_CALLOC(1, sizeof(*(pointer_to->pointer)));
					pointer_to->pointer->pointee = object_type;

					return create_type_set(pointer_to);
				}
				
				internal_error("Overloading of operator& unsupported", 0);
				break;
			}
			// Unary operators
		case AST_COMPLEMENT_OP :
		case AST_NEG_OP :
		case AST_NOT_OP :
		case AST_PLUS_OP :
			{
#warning Missing overload support here
				type_set_t* result = calculate_expression_type(ASTSon0(a), st);

				if (result->num_types == 1)
				{
					if (is_fundamental_type(result->types[0]))
					{
						return result;
					}
					else
					{
						internal_error("Unsupported overloading of unary operators", 0);
					}
				}

				internal_error("Unsupported overloading of unary operators", 0);
			}
		case AST_TYPEID_EXPR :
		case AST_TYPEID_TYPE :
			{
				// TODO - This can be used only when #include <typeinfo> has been specified
				internal_error("TODO", 0);
			}
		case AST_SIZEOF :
		case AST_SIZEOF_TYPEID :
			{
				return create_type_set(new_int_type());
			}
		default :
			{
				internal_error("Unknown node '%s' in expression", ast_print_node_type(ASTType(a)));
			}
	}

	internal_error("Unreachable code", 0);
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
	// ADvance over typedefs
	while (t1->kind == TK_DIRECT
			&& t1->type->kind == STK_TYPEDEF)
	{
		t1 = t1->type->aliased_type;
	}

	while (t2->kind == TK_DIRECT
			&& t2->type->kind == STK_TYPEDEF)
	{
		t2 = t2->type->aliased_type;
	}

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
	return new_int_type();
}

