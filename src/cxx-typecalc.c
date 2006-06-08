#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-typecalc.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"
#include "cxx-cexpr.h"
#include "cxx-overload.h"
#include "cxx-ambiguity.h"
#include "cxx-koenig.h"

/*
 * Calculates the type of an expression
 */
static calculated_type_t* create_type_set(type_t* t, value_type_t value_type);
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

calculated_type_t* calculate_functional_expression_type(AST a, AST arguments, scope_t* st)
{
	switch (ASTType(a))
	{
		case AST_PARENTHESIZED_EXPRESSION :
			{
				return calculate_functional_expression_type(ASTSon0(a), arguments, st);
			}
		case AST_SYMBOL :
			{
				calculated_type_t* result = NULL;
				
				// Koenig lookup here!
				scope_entry_list_t* function_lookup = NULL;
				function_lookup = lookup_unqualified_function(st, ASTText(a), arguments);

				enum cxx_symbol_kind filter_funct[2] =
				{ 
					SK_FUNCTION,
					SK_TEMPLATE_FUNCTION
				};
				function_lookup = filter_symbol_kind_set(function_lookup, 2, filter_funct);

				if (function_lookup == NULL)
				{
					internal_error("Function '%s' not found\n", ASTText(a));
				}

				if (function_lookup->entry->kind == SK_FUNCTION)
				{
					result = GC_CALLOC(1, sizeof(*result));
					scope_entry_list_t* iter = function_lookup;

					// This is rather ugly
					while (iter != NULL)
					{
						result->num_types++;
						iter = iter->next;
					}

					result->overloaded_functions = function_lookup;

				}

				return result;
				break;
			}
		default :
			return calculate_expression_type(a, st);
	}
}

calculated_type_t* calculate_expression_type(AST a, scope_t* st)
{
	switch (ASTType(a))
	{
		case AST_AMBIGUITY :
			{
				solve_possibly_ambiguous_expression(a, st);
				if (ASTType(a) == AST_AMBIGUITY)
				{
					internal_error("Still ambiguous", 0);
				}
				// Restart 
				return calculate_expression_type(a, st);
				break;
			}
		// Primaries
		case AST_BOOLEAN_LITERAL :
			{
				type_t* result = new_bool_type();

				return create_type_set(result, VT_RVALUE);
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

				return create_type_set(result, VT_RVALUE);
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

				return create_type_set(result, VT_RVALUE);
			}
		case AST_THIS_VARIABLE :
			{
				scope_entry_list_t* this_symbol;

				this_symbol = query_in_symbols_of_scope(st, "this");

				return create_type_set(this_symbol->entry->type_information, VT_RVALUE);
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

				calculated_type_t* result = NULL;

				// If this is a function name return all types associated with it
				if (result_list->entry->kind == SK_FUNCTION)
				{
					result = GC_CALLOC(1, sizeof(*result));
					scope_entry_list_t* iter = result_list;

					// This is rather ugly
					while (iter != NULL)
					{
						result->num_types++;
						iter = iter->next;
					}

					result->overloaded_functions = result_list;
				}
				else // This is not a function
				{
					scope_entry_t* entry = result_list->entry;
					value_type_t value_type = VT_UNDEFINED;
					// Otherwise this should have only one type
					if (entry->kind == SK_VARIABLE)
					{
						cv_qualifier_t cv_qualifier = get_cv_qualifier(result_list->entry->type_information);

						if ((cv_qualifier & CV_CONST) == CV_CONST)
						{
							value_type = VT_RVALUE;
						}
						else
						{
							value_type = VT_LVALUE;
						}
					}
					else if (entry->kind == SK_ENUMERATOR)
					{
						value_type = VT_RVALUE;
					}
					else
					{
						internal_error("Unexpected symbol kind '%d'", entry->kind);
					}

					result = create_type_set(entry->type_information, value_type);
				}

				return result;
			}
			// Postfix expressions
		case AST_ARRAY_SUBSCRIPT :
			{
				calculated_type_t* array_type_set = calculate_expression_type(ASTSon0(a), st);

				if (array_type_set->num_types != 1)
				{
					internal_error("Unsupported set of types for this array subscript", 0);
				}

				type_t* array_type = array_type_set->types[0];

				value_type_t value_type = VT_LVALUE;
				if (array_type->kind != TK_ARRAY)
				{
					internal_error("Expected an array type at the left of the array subscript!\n", 0);
				}

				cv_qualifier_t cv_qualifier = get_cv_qualifier(array_type->array->element_type);

				if ((cv_qualifier & CV_CONST) == CV_CONST)
				{
					value_type = VT_RVALUE;
				}

				return create_type_set(array_type->array->element_type, value_type);
			}
		case AST_POINTER_CLASS_MEMBER_ACCESS :
		case AST_CLASS_MEMBER_ACCESS :
			{
				cv_qualifier_t cv_qualifier = CV_NONE;
				value_type_t value_type = VT_RVALUE;
				calculated_type_t* class_type_set = calculate_expression_type(ASTSon0(a), st);

				if (class_type_set->num_types != 1)
				{
					internal_error("Unsupported set of types for this member access (%d)", class_type_set->num_types);
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
					cv_qualifier |= get_cv_qualifier(class_type);
					class_type = class_type->type->aliased_type;
				}

				// If this names a user defined type get the original type
				if (class_type->kind == TK_DIRECT
						&& class_type->type->kind == STK_USER_DEFINED)
				{
					cv_qualifier |= get_cv_qualifier(class_type);
					class_type = class_type->type->user_defined_type->type_information;
				}
				else if (class_type->kind == TK_DIRECT
						&& class_type->type->kind == STK_CLASS)
				{
					// This is an unnamed class, the cv_qualifier will be in
					// the class itself (this is a very odd case, left as
					// exercise for the reader to understand why)
					cv_qualifier |= get_cv_qualifier(class_type);
				}

				if (!is_class_type(class_type))
				{
					internal_error("This expression does not denote a class", 0);
				}

				scope_t* inner_scope = class_type->type->class_info->inner_scope;

				AST id_expression = ASTSon1(a);
				scope_entry_list_t* result_list = query_id_expression(inner_scope, id_expression, NOFULL_UNQUALIFIED_LOOKUP);

				calculated_type_t* result = NULL;

				// If this is a function name return all types associated with it
				if (result_list->entry->kind == SK_FUNCTION)
				{
					result = GC_CALLOC(1, sizeof(*result));
					scope_entry_list_t* iter = result_list;

					// This is rather ugly
					while (iter != NULL)
					{
						result->num_types++;
						iter = iter->next;
					}

					result->overloaded_functions = result_list;
					result->object_type = class_type;
				}
				else
				{
					// Otherwise this should have only one type
					if ((cv_qualifier & CV_CONST) == CV_CONST)
					{
						value_type = VT_RVALUE;
					}
					result = create_type_set(result_list->entry->type_information, value_type);
					result->object_type = class_type;
				}

				return result;
			}
		case AST_FUNCTION_CALL : 
			{
				AST function_expr = ASTSon0(a);
				AST argument_list = ASTSon1(a);

				calculated_type_t* function_expr_type = calculate_functional_expression_type(function_expr, argument_list, st);

				type_t* function_type;
				if (function_expr_type->num_types > 1)
				{
					scope_entry_t* overloaded_funct = resolve_overload(st, argument_list, function_expr_type->overloaded_functions, 
							function_expr_type->object_type);
					function_type = overloaded_funct->type_information;
				}
				else
				{
					function_type = function_expr_type->overloaded_functions->entry->type_information;
				}

				if (function_type->kind != TK_FUNCTION)
				{
					internal_error("Expression does not denote a function\n", 0);
				}

				value_type_t value_type = VT_RVALUE;

				if (function_type->function->return_type->kind == TK_REFERENCE)
				{
					value_type = VT_LVALUE;
				}

				return create_type_set(function_type->function->return_type, value_type);
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

				calculated_type_t* casted_expr_type = calculate_expression_type(ASTSon1(a), st);

				value_type_t value_type = casted_expr_type->value_type;

				return create_type_set(declarator_type, value_type);
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
				calculated_type_t* type_left = calculate_expression_type(ASTSon0(a), st);
				calculated_type_t* type_right = calculate_expression_type(ASTSon1(a), st);

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
						return create_type_set(new_bool_type(), VT_RVALUE);
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
					return create_type_set(declarator_type, VT_RVALUE);
				}
				else
				{
					type_t* pointer_to = GC_CALLOC(1, sizeof(*pointer_to));
					pointer_to->kind = TK_POINTER;
					pointer_to->pointer = GC_CALLOC(1, sizeof(*(pointer_to->pointer)));
					pointer_to->pointer->pointee = declarator_type;

					return create_type_set(pointer_to, VT_RVALUE);
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
				calculated_type_t* type_left = calculate_expression_type(ASTSon0(a), st);
				calculated_type_t* type_right = calculate_expression_type(ASTSon1(a), st);

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
						return create_type_set(usual_arithmetic_conversions(t1, t2, st), VT_RVALUE);
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
				calculated_type_t* type_left = calculate_expression_type(ASTSon0(a), st);
				calculated_type_t* type_right = calculate_expression_type(ASTSon1(a), st);

				if (type_left->num_types == 1
						&& type_right->num_types == 1)
				{
					type_t* t1 = type_left->types[0];
					type_t* t2 = type_right->types[0];
					// Check if this may be a builtin operator invocation
					if (is_fundamental_type(t1)
							&& is_fundamental_type(t2))
					{
						return create_type_set(usual_arithmetic_conversions(t1, t2, st), VT_RVALUE);
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
				calculated_type_t* result = calculate_expression_type(ASTSon0(a), st);

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
				calculated_type_t* result = calculate_expression_type(ASTSon1(a), st);
				return result;
			}
			// Special unary operators
		case AST_DERREFERENCE :
			{
#warning Missing overload support here
				AST cast_expr = ASTSon1(a);

				calculated_type_t* cast_expr_set = calculate_expression_type(cast_expr, st);

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
					cv_qualifier_t cv_qualifier = get_cv_qualifier(pointer_type->pointer->pointee);
					value_type_t value_type = VT_LVALUE;

					if ((cv_qualifier & CV_CONST) == CV_CONST)
					{
						value_type = VT_RVALUE;
					}
					return create_type_set(pointer_type->pointer->pointee, value_type);
				}
				break;
			}
		case AST_REFERENCE :
			{
#warning Missing overload support here
#warning Handle the case where a function is "over-referenced"
				AST cast_expr = ASTSon1(a);

				calculated_type_t* cast_expr_set = calculate_expression_type(cast_expr, st);

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

					return create_type_set(pointer_to, VT_RVALUE);
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
				calculated_type_t* result = calculate_expression_type(ASTSon0(a), st);

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
				internal_error("TODO - typeid not really supported", 0);
			}
		case AST_SIZEOF :
		case AST_SIZEOF_TYPEID :
			{
				return create_type_set(new_int_type(), VT_RVALUE);
			}
		default :
			{
				internal_error("Unknown node '%s' in expression", ast_print_node_type(ASTType(a)));
			}
	}

	internal_error("Unreachable code", 0);
}

static calculated_type_t* create_type_set(type_t* t, value_type_t value_type)
{
	if (t == NULL)
	{
		internal_error("Should not be creating a type set from a null type", 0);
	}

	calculated_type_t* result = GC_CALLOC(1, sizeof(*result));

	result->num_types = 1;

	result->types = GC_CALLOC(1, sizeof(*result));
	result->types[0] = t;

	result->value_type = value_type;

	return result;
}

/*
 * Implements 5.9 [expr] "usual arithmetic conversions"
 */
static type_t* usual_arithmetic_conversions(type_t* t1, type_t* t2, scope_t* st)
{
	// Advance over typedefs
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

	if (equivalent_types(t1, t2, st, CVE_IGNORE_OUTERMOST))
	{
		return t1;
	}

#warning What about references to builtin types ?
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

