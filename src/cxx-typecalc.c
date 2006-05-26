#include "cxx-typecalc.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-typeutils.h"
#include "cxx-utils.h"


type_t* new_fundamental_type(temporary_status_t temporary_status)
{
	type_t* result = GC_CALLOC(1, sizeof(*result));

	result->kind = TK_DIRECT;
	result->type = GC_CALLOC(1, sizeof(*(result->type)));

	result->type->kind = STK_BUILTIN_TYPE;

	result->temporary_status = temporary_status;

	return result;
}

type_t* new_boolean_type(temporary_status_t temporary_status)
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

type_t* calculate_expression_type(AST a, scope_t* st)
{
	switch (ASTType(a))
	{
		// Primaries
		case AST_BOOLEAN_LITERAL :
			{
				return new_bool_type(IS_TEMPORARY);
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

				return result;
				break;
			}
		case AST_FLOATING_LITERAL :
			{
				char* text = ASTText(a);
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

				return result;
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
				scope_entry_list_t* result = query_id_expression(st, a);
				break;
			}
		default :
			{
				internal_error("Unknown node '%s' in expression", ast_print_node_type(ASTType(a)));
			}
	}
}
