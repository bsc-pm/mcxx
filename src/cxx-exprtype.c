/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include <ctype.h>
#include <string.h>

static
type_t *compute_expression_type_rec(AST expr, scope_t *sc, decl_context_t decl_context, char *is_lvalue);

type_t *compute_expression_type(AST expr, scope_t *sc, decl_context_t decl_context, char *is_lvalue)
{
    if (expr == NULL)
    {
        internal_error("Invalid null node\n", 0);
    }

    *is_lvalue = 0;
    type_t* result = compute_expression_type_rec(expr, sc, decl_context, is_lvalue);

    return result;
}

static type_t *decimal_literal_type(AST expr);
static type_t *character_literal(AST expr);
static type_t *floating_literal(AST expr);
static type_t *string_literal(AST expr);
static type_t *pointer_to_type(type_t* t);

static
type_t *compute_expression_type_rec(AST expr, scope_t *sc, decl_context_t decl_context, char* is_lvalue)
{
    type_t* result = NULL;

    switch (ASTType(expr))
    {
        case AST_EXPRESSION : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);
                // No change in the "lvalueness"
                break;
            }
        case AST_SYMBOL :
            {
                scope_entry_list_t* list = query_id_expression(sc, expr, FULL_UNQUALIFIED_LOOKUP, decl_context);
                if (list == NULL)
                    break;

                scope_entry_t* entry = list->entry;
                result = entry->type_information;

                // This is always an lvalue except for functions and arrays
                if (!is_function_type(result)
                        && !is_array_type(result))
                {
                    (*is_lvalue) = 1;
                }
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {
                result = decimal_literal_type(expr);
                // Literals are not lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_CHARACTER_LITERAL :
            {
                result = character_literal(expr);
                // Literals are not lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_FLOATING_LITERAL :
        case AST_HEXADECIMAL_FLOAT :
            {
                result = floating_literal(expr);
                // Literals are not lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_STRING_LITERAL :
            {
                result = string_literal(expr);
                // Literals are not lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_ARRAY_SUBSCRIPT :
            {
                type_t* subscripted_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);

                if (subscripted_type == NULL)
                {
                    break;
                }

                subscripted_type = advance_over_typedefs(subscripted_type);

                if (subscripted_type->kind == TK_POINTER)
                {
                    result = subscripted_type->pointer->pointee;
                }
                else if (subscripted_type->kind == TK_ARRAY)
                {
                    result = subscripted_type->array->element_type;
                }

                // This is always a lvalue
                (*is_lvalue) = 1;

                break;
            }
        case AST_FUNCTION_CALL :
            {
                type_t* function_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);

                if (function_type == NULL)
                    break;

                function_type = advance_over_typedefs(function_type);

                if (function_type->kind == TK_FUNCTION)
                {
                    result = function_type->function->return_type;
                }
                else if (function_type->kind == TK_POINTER
                        && function_type->pointer->pointee->kind == TK_FUNCTION)
                {
                    result = function_type->pointer->pointee;
                }

                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_CLASS_MEMBER_ACCESS :
        case AST_POINTER_CLASS_MEMBER_ACCESS :
            {
                // We have to lookup in the class scope
                type_t* class_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);

                if (class_type == NULL)
                    break;

                class_type = advance_over_typedefs(class_type);

                // An additional indirection here
                if (ASTType(expr) == AST_POINTER_CLASS_MEMBER_ACCESS)
                {
                    if (class_type->kind == TK_POINTER)
                    {
                        class_type = class_type->pointer->pointee;
                    }
                    else break;

                    class_type = advance_over_typedefs(class_type);
                }

                scope_t *class_scope = NULL;
                if (is_named_class_type(class_type))
                {
                    // Get the class itself
                    class_scope = 
                        class_type->type->user_defined_type // The user defined type
                        ->type_information // its type information TK_DIRECT and STK_CLASS
                        ->type->class_info->inner_scope;
                }
                else if (is_unnamed_class_type(class_type))
                {
                    // class_type->type->kind == STK_CLASS
                    class_scope = class_type->type->class_info->inner_scope;
                }
                else 
                    break;

                // Now get the member of the class
                scope_entry_list_t* list = query_id_expression(class_scope, ASTSon1(expr), NOFULL_UNQUALIFIED_LOOKUP, decl_context);
                if (list == NULL)
                    break;

                scope_entry_t* entry = list->entry;

                result = entry->type_information;

                // This is always a lvalue
                (*is_lvalue) = 1;
                break;
            }
        case AST_POSTINCREMENT :
        case AST_POSTDECREMENT :
        case AST_PREINCREMENT :
        case AST_PREDECREMENT :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);
                // This is always a lvalue
                (*is_lvalue) = 1;
                break;
            }
        case AST_SIZEOF :
            {
                // Technically this is size_t
                result = unsigned_integer_type();
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_SIZEOF_TYPEID :
            {
                // Technically this is size_t
                result = unsigned_integer_type();
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_DERREFERENCE :
            {
                type_t* referenced_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);

                if (referenced_type == NULL)
                    break;

                if (referenced_type->kind == TK_POINTER)
                {
                    result = referenced_type->pointer->pointee;
                }
                else if (referenced_type->type->kind == TK_ARRAY)
                {
                    result = referenced_type->array->element_type;
                }
                else if (referenced_type->type->kind == TK_FUNCTION)
                {
                    // Stupid case since here (*f) is the same as (f)
                    result = referenced_type;
                }

                // This is always an lvalue
                (*is_lvalue) = 1;
                break;
            }
        case AST_REFERENCE :
            {
                type_t* referenced_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);

                if (referenced_type == NULL)
                    break;

                result = pointer_to_type(referenced_type);
                
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);
                
                // These should not be lvalues
                (*is_lvalue) = 0;
                break;
            }
        case AST_CAST_EXPRESSION :
            {
                AST type_id = ASTSon0(expr);

                AST type_specifier = ASTSon0(type_id);
                AST abstract_declarator = ASTSon1(type_id);

                gather_decl_spec_t gather_info;
                memset(&gather_info, 0, sizeof(gather_info));

                decl_context.decl_flags |= DF_NO_FAIL;

                type_t* simple_type_info = NULL;
                build_scope_decl_specifier_seq(type_specifier, sc, &gather_info, &simple_type_info, 
                        decl_context);

                if (abstract_declarator != NULL)
                {
                    type_t* declarator_type = NULL;
                    build_scope_declarator(abstract_declarator, sc, &gather_info, simple_type_info, 
                            &declarator_type, decl_context);

                    result = declarator_type;
                }
                else
                {
                    result = simple_type_info;
                }

                // Technically something casted is not an lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_MULT_OP :
        case AST_DIV_OP :
        case AST_MOD_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
        case AST_SHL_OP :
        case AST_SHR_OP :
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
            {
                type_t* result_lhs = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);
                type_t* result_rhs = compute_expression_type_rec(ASTSon1(expr), sc, decl_context, is_lvalue);

                if (result_lhs == NULL
                        || result_rhs == NULL)
                    break;

                // This is never a lvalue
                (*is_lvalue) = 0;

                // Pointer arithmetic
                if ((is_pointer_type(result_lhs) &&
                            is_integral_type(result_rhs)))
                {
                    result = result_lhs;
                    break;
                }
                if (is_integral_type(result_lhs) &&
                        is_pointer_type(result_rhs))
                {
                    result = result_rhs;
                    break;
                }

                // Order of arithmetic promotions
                char (*ptr_is_type[])(type_t*) =
                { 
                    is_long_double,
                    is_float,
                    is_unsigned_long_long_int,
                    is_long_long_int,
                    is_unsigned_long_int,
                    is_long_int
                };
                int num_ptr_functs = sizeof(ptr_is_type) / sizeof(*ptr_is_type);

                int i;
                char conversion_made = 0;
                for (i = 0; i < num_ptr_functs; i++)
                {
                    char (*ptr_func)(type_t*) = ptr_is_type[i];

                    if (ptr_func(result_lhs)
                            || ptr_func(result_rhs))
                    {
                        result = ptr_func(result_lhs) ? result_lhs : result_rhs;
                        conversion_made = 1;
                        break;
                    }
                }

                if (conversion_made)
                    break;

                // As we were supposed to do integral promotions only int remains here
                result = integer_type();
                break;
            }
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
            {
                // In C this is always an int
                result = integer_type();
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
                // In C this is always an int
                result = integer_type();
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                // Assume is the first one
                char is_lvalue_1 = 0;
                char is_lvalue_2 = 0;
                type_t* true_type = compute_expression_type_rec(ASTSon1(expr), sc, decl_context, &is_lvalue_1);
                /* type_t* false_type = */ compute_expression_type_rec(ASTSon2(expr), sc, decl_context, &is_lvalue_2);

                // Let's assume the first one, even if we should promote
                result = true_type;

                // This is a lvalue if both are
                (*is_lvalue) = (is_lvalue_1 && is_lvalue_2);
                break;
            }
        case AST_ASSIGNMENT :
        case AST_ADD_ASSIGNMENT :
        case AST_SUB_ASSIGNMENT :
        case AST_SHL_ASSIGNMENT :
        case AST_SHR_ASSIGNMENT :
        case AST_MUL_ASSIGNMENT :
        case AST_DIV_ASSIGNMENT :
        case AST_AND_ASSIGNMENT :
        case AST_OR_ASSIGNMENT :
        case AST_XOR_ASSIGNMENT :
        case AST_MOD_ASSIGNMENT :
            {
                // The type is always the one of the left part of the assignment
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context, is_lvalue);
                // This is never a lvalue
                (*is_lvalue) = 0;
                break;
            }
        default:
            internal_error("Unknown node type %s at %s\n", ast_print_node_type(ASTType(expr)), node_information(expr));
    }

    return result;
}


// Signed
type_t *integer_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_DIRECT;

        result->type = calloc(1, sizeof(*result->type));
        result->type->kind = STK_BUILTIN_TYPE;
        result->type->builtin_type = BT_INT;
    }

    return result;
}

static type_t *long_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(integer_type());
        result->type->is_long = 1;
    }

    return result;
}

static type_t *long_long_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(integer_type());
        result->type->is_long = 2;
    }

    return result;
}

static type_t *short_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(integer_type());
        result->type->is_short = 1;
    }

    return result;
}

// Unsigned

type_t *unsigned_integer_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = copy_type(integer_type());
        result->type->is_unsigned = 1;
    }

    return result;
}

static type_t *unsigned_long_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(long_integer_type());
        result->type->is_unsigned = 1;
    }

    return result;
}

static type_t *unsigned_long_long_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(long_long_integer_type());
        result->type->is_unsigned = 1;
    }

    return result;
}

static type_t *unsigned_short_integer_type(void)
{
    static type_t* result = NULL;

    if (result == NULL)
    {
        result = copy_type(short_integer_type());
        result->type->is_unsigned = 1;
    }

    return result;
}

static type_t* character_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_DIRECT;

        result->type = calloc(1, sizeof(*result->type));
        result->type->kind = STK_BUILTIN_TYPE;
        result->type->builtin_type = BT_CHAR;
    }

    return result;
}

static type_t* wide_character_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_DIRECT;

        result->type = calloc(1, sizeof(*result->type));
        result->type->kind = STK_BUILTIN_TYPE;
        result->type->builtin_type = BT_WCHAR;
    }

    return result;
}

static type_t* float_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_DIRECT;

        result->type = calloc(1, sizeof(*result->type));
        result->type->kind = STK_BUILTIN_TYPE;
        result->type->builtin_type = BT_FLOAT;
    }

    return result;
}

static type_t* double_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_DIRECT;

        result->type = calloc(1, sizeof(*result->type));
        result->type->kind = STK_BUILTIN_TYPE;
        result->type->builtin_type = BT_DOUBLE;
    }

    return result;
}

static type_t* long_double_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = copy_type(double_type());
        result->type->is_long = 1;
    }

    return result;
}

static type_t *pointer_to_char_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_POINTER;
        result->pointer = calloc(1, sizeof(*result->pointer));
        result->pointer->pointee = character_type();
    }

    return result;
}

static type_t *pointer_to_wchar_type(void)
{
    static type_t* result = NULL;
    if (result == NULL)
    {
        result = calloc(1, sizeof(*result));
        result->kind = TK_POINTER;
        result->pointer = calloc(1, sizeof(*result->pointer));
        result->pointer->pointee = wide_character_type();
    }

    return result;
}

// Compute type of literals

static type_t* decimal_literal_type(AST expr)
{
    char *literal = ASTText(expr);
    char *last = literal + strlen(literal) - 1;

    char is_unsigned = 0;
    char is_long = 0;

    while (toupper(*last) == 'L' 
            || toupper(*last) == 'U')
    {
        switch (*last)
        {
            case 'l' :
            case 'L' :
                is_long++;
                break;
            case 'u' :
            case 'U' :
                is_unsigned = 1;
                break;
            default:
                break;
        }
        last--;
    }

    switch (is_long)
    {
        case 0 :
            {
                return is_unsigned == 0 ? integer_type() : unsigned_integer_type();
            }
        case 1 : 
            {
                return is_unsigned == 0 ? long_integer_type() : unsigned_long_integer_type();
            }
        default :
            {
                return is_unsigned == 0 ? long_long_integer_type() : unsigned_long_long_integer_type();
            }
    }
}

static type_t *character_literal(AST expr)
{
    char *literal = ASTText(expr);

    if (*literal != 'L')
    {
        return character_type();
    }
    else
    {
        return wide_character_type();
    }
}

static type_t *floating_literal(AST expr)
{
    char *literal = ASTText(expr);
    char *last = literal + strlen(literal) - 1;

    char is_float = 0;
    char is_long_double = 0;

    while (toupper(*last) == 'F' 
            || toupper(*last) == 'L')
    {
        switch (*last)
        {
            case 'l' :
            case 'L' :
                is_long_double++;
                break;
            case 'F' :
            case 'f' :
                is_float = 1;
                break;
            default:
                break;
        }
        last--;
    }

    if (is_long_double)
    {
        return long_double_type();
    }
    else if (is_float)
    {
        return float_type();
    }
    else 
        return double_type();
}

static type_t *string_literal(AST expr)
{
    char *literal = ASTText(expr);

    if (*literal != 'L')
    {
        return pointer_to_char_type();
    }
    else
    {
        return pointer_to_wchar_type();
    }
}

static type_t *pointer_to_type(type_t* t)
{
    type_t* result = calloc(1, sizeof(*result));

    result->kind = TK_POINTER;
    result->pointer = calloc(1, sizeof(*result->pointer));
    result->pointer->pointee = t;

    return result;
}
