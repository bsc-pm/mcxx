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
#include "cxx-koenig.h"
#include <ctype.h>
#include <string.h>

typedef
struct type_set_tag
{
    int num_types;
    argument_type_info_t** types;

    // This is the qualification type of a member access
    argument_type_info_t* qualifier;
} type_set_t;

static type_t* get_type_from_set(type_set_t type_set);
static char get_lvalueness_from_set(type_set_t type_set);
static int number_of_types(type_set_t type_set);

static type_set_t compute_expression_type_rec(AST expr, decl_context_t decl_context);

type_t *compute_expression_type(AST expr, decl_context_t decl_context, char *is_lvalue)
{
    if (expr == NULL)
    {
        internal_error("Invalid null node\n", 0);
    }

    *is_lvalue = 0;

    type_t* result = NULL;
    
    type_set_t type_set = compute_expression_type_rec(expr, decl_context);

    if (number_of_types(type_set) > 0)
    {
        // FIXME - What to do when type_set.num_types > 1?
        // It can happen only with expressions
        // referring to overloaded/templated functions
        result = type_set.types[0]->type;
        *is_lvalue = type_set.types[0]->is_lvalue;
    }

    return result;
}

static type_t *decimal_literal_type(AST expr);
static type_t *character_literal(AST expr);
static type_t *floating_literal(AST expr);
static type_t *string_literal(AST expr);

static int number_of_types(type_set_t type_set)
{
    return type_set.num_types;
}

static type_t* get_type_from_set(type_set_t type_set)
{
    if (type_set.num_types == 1)
    {
        return type_set.types[0]->type;
    }
    else return NULL;
}

static char get_lvalueness_from_set(type_set_t type_set)
{
    if (type_set.num_types == 1)
    {
        return type_set.types[0]->is_lvalue;
    }
    else internal_error("This function cannot be used in type sets that are not singletons", 0);
}

argument_type_info_t* new_argument_type(type_t* type, char is_lvalue)
{
    argument_type_info_t* result = calloc(1, sizeof(*result));

    result->type = type;
    result->is_lvalue = is_lvalue;

    return result;
}

static
type_set_t compute_expression_type_rec(AST expr, decl_context_t decl_context)
{
    // Clear the stack debris
    type_set_t result;
    memset(&result, 0, sizeof(result));

    switch (ASTType(expr))
    {
        case AST_EXPRESSION : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                // lvalueness is preserved
                result = compute_expression_type_rec(ASTSon0(expr), decl_context);
                break;
            }
        case AST_THIS_VARIABLE :
            {
                scope_entry_list_t* list = query_unqualified_name_str(decl_context, "this");
                if (list == NULL)
                    break;

                scope_entry_t* entry = list->entry;

                // 'this' is never a lvalue
                argument_type_info_t* result_type = new_argument_type(
                        /* type */entry->type_information, 
                        /* is_lvalue */0);
                P_LIST_ADD(result.types, result.num_types, result_type);
                break;
            }
        case AST_SYMBOL :
            {
                scope_entry_list_t* list = query_id_expression(decl_context, expr);
                if (list == NULL)
                    break;

                scope_entry_list_t* iter = list;

                while (iter != NULL)
                {
                    scope_entry_t* entry = iter->entry;
                    type_t* type_info = entry->type_information;

                    cv_qualifier_t cv_qualif = CV_NONE;
                    advance_over_typedefs_with_cv_qualif(type_info, &cv_qualif);

                    // Arrays, enumerated, function types
                    // and const objects are not lvalues
                    char is_lvalue = !is_function_type(type_info) 
                        && !is_enumerated_type(type_info)
                        && !is_array_type(type_info)
                        && ((cv_qualif & CV_CONST) != CV_CONST);

                    argument_type_info_t* result_type = new_argument_type(
                            /* type */entry->type_information, 
                            /* is_lvalue */is_lvalue);
                    P_LIST_ADD(result.types, result.num_types, result_type);
                    iter = iter->next;
                }
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {
                P_LIST_ADD(result.types, result.num_types,
                        // Literals are never lvalue
                        new_argument_type(decimal_literal_type(expr), 0)
                        );
                break;
            }
        case AST_CHARACTER_LITERAL :
            {
                P_LIST_ADD(result.types, result.num_types,
                        // Literals are never lvalue
                        new_argument_type(character_literal(expr), 0)
                        );
                break;
            }
        case AST_FLOATING_LITERAL :
        case AST_HEXADECIMAL_FLOAT :
            {
                P_LIST_ADD(result.types, result.num_types,
                        // Literals are never lvalue
                        new_argument_type(floating_literal(expr), 0)
                        );
                break;
            }
        case AST_STRING_LITERAL :
            {
                P_LIST_ADD(result.types, result.num_types,
                        // Literals are never lvalue
                        new_argument_type(string_literal(expr), 0)
                        );
                break;
            }
        case AST_ARRAY_SUBSCRIPT :
            {
// #warning operator[] not considered for C++
                // FIXME - This might involve calling 'operator[]' of the subscripted expression
                // is a class
                type_set_t subscripted_expr_type = compute_expression_type_rec(ASTSon0(expr), decl_context);

                if (number_of_types(subscripted_expr_type) == 0)
                    break;

                ERROR_CONDITION((number_of_types(subscripted_expr_type) > 1), 
                        "A subscripted expression cannot have multiple types!", 0);

                type_t* subscripted_type = get_type_from_set(subscripted_expr_type);

                subscripted_type = advance_over_typedefs(subscripted_type);

                type_t* element_type = NULL;
                if (is_pointer_type(subscripted_type))
                {
                    element_type = pointer_type_get_pointee_type(subscripted_type);
                }
                else if (is_array_type(subscripted_type))
                {
                    element_type = array_type_get_element_type(subscripted_type);
                }

                cv_qualifier_t cv_qualif = CV_NONE;
                advance_over_typedefs_with_cv_qualif(element_type, &cv_qualif);

                char is_lvalue = ((cv_qualif & CV_CONST) != CV_CONST);

                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(element_type, is_lvalue));
                break;
            }
        case AST_FUNCTION_CALL :
            {
                C_LANGUAGE()
                {
                    // This is easy in C
                    type_set_t function_type_set = compute_expression_type_rec(ASTSon0(expr), decl_context);
                    if (number_of_types(function_type_set) == 0)
                        break;

                    type_t* function_type = get_type_from_set(function_type_set);
                    function_type = advance_over_typedefs(function_type);

                    type_t* result_type = NULL;
                    if (is_function_type(function_type))
                    {
                        result_type = function_type_get_return_type(function_type);
                    }
                    else if (is_pointer_type(function_type)
                            && is_function_type(pointer_type_get_pointee_type(function_type)))
                    {
                        result_type = function_type_get_return_type(
                                pointer_type_get_pointee_type(function_type)
                                );
                    }

                    // In C this is never a lvalue
                    P_LIST_ADD(result.types, result.num_types,
                            new_argument_type(result_type, 0));
                }

                CXX_LANGUAGE()
                {
                    // This is hard in C++
                    AST called_expression = ASTSon0(expr);
                    // First get all the types of all the parameters involved in the call
                    AST expression_list = ASTSon1(expr);
                    AST iter;
                    type_set_t argument_types;
                    memset(&argument_types, 0, sizeof(argument_types));

                    char argument_type_error = 0;

                    for_each_element(expression_list, iter)
                    {
                        AST argument = ASTSon1(iter);

                        type_set_t argument_type_set = compute_expression_type_rec(argument, decl_context);

                        if (number_of_types(argument_type_set) == 0) 
                        {
                            // Quit everything
                            argument_type_error = 1;
                            break;
                        }

                        ERROR_CONDITION((number_of_types(argument_type_set) > 1),
                                "Arguments only can have one type!", 0);

                        P_LIST_ADD(argument_types.types, argument_types.num_types,
                                argument_type_set.types[0]);
                    }

                    // Quit if any argument was unknown
                    if (argument_type_error)
                        break;

                    /*
                     * Algorithm:
                     *
                     * 1. If Koenig must be used, use it to get a list of callable functions
                     * 2. If Koenig cannot be used
                     *    2.1. Compute the type of the called expression
                     *    2.2. Use it to get a list of callable functions
                     * 3. If template functions are involved, solve their exact types to get
                     * a list of callable functions free of template functions.
                     * 4. Apply overloading to the list obtained in 3
                     *
                     * When Koenig must be used ?
                     *
                     *   - When the called expression is an unqualified one and it does not
                     *   designate any member function visible in the current scope.
                     */

                    if (koenig_can_be_used(called_expression, decl_context))
                    {
                        /* scope_entry_list_t* found_by_koenig = */ koenig_lookup(
                                argument_types.num_types,
                                argument_types.types, 
                                decl_context);
                    }
                }

                break;
            }
        case AST_CLASS_MEMBER_ACCESS :
        case AST_POINTER_CLASS_MEMBER_ACCESS :
            {
                // We have to lookup in the class scope
                type_set_t class_type_set = compute_expression_type_rec(ASTSon0(expr), decl_context);

                if (number_of_types(class_type_set) == 0)
                    break;

                ERROR_CONDITION( (number_of_types(class_type_set) > 1),
                        "In a class or pointer to class member access only one type is valid in the postfix", 0);
                    
                type_t* class_type = get_type_from_set(class_type_set);

                cv_qualifier_t cv_qualif = CV_NONE;
                class_type = advance_over_typedefs_with_cv_qualif(class_type, &cv_qualif);

                // An additional indirection here
                if (ASTType(expr) == AST_POINTER_CLASS_MEMBER_ACCESS)
                {
                    if (is_pointer_type(class_type))
                    {
                        class_type = pointer_type_get_pointee_type(class_type);
                    }
                    else break;

                    cv_qualif = CV_NONE;
                    class_type = advance_over_typedefs_with_cv_qualif(class_type, &cv_qualif);
                }

                decl_context_t class_context;
                if (is_named_class_type(class_type))
                {
                    // Get the class itself
                    class_context = 
                        class_type_get_inner_context(
                                named_type_get_symbol(class_type)->type_information 
                                );
                }
                else if (is_unnamed_class_type(class_type))
                {
                    class_context = class_type_get_inner_context(class_type);
                }
                else 
                    break;

                // Now get the member of the class
                scope_entry_list_t* list = query_id_expression(class_context, ASTSon1(expr));
                if (list == NULL)
                    break;

                scope_entry_list_t* iter = list;

                while (iter != NULL)
                {
                    scope_entry_t* entry = iter->entry;
                    type_t* type_info = entry->type_information;

                    // Arrays are not lvalues in members
                    char is_lvalue = !is_array_type(type_info);

                    argument_type_info_t* result_type = new_argument_type(
                            /* type */entry->type_information, 
                            /* is_lvalue */is_lvalue);
                    P_LIST_ADD(result.types, result.num_types, result_type);
                    iter = iter->next;
                }

                char qualifier_is_lvalue 
                    = (cv_qualif & CV_CONST) != CV_CONST;
                result.qualifier = new_argument_type(
                        class_type,
                        qualifier_is_lvalue);
                break;
            }
        case AST_POSTINCREMENT :
        case AST_POSTDECREMENT :
        case AST_PREINCREMENT :
        case AST_PREDECREMENT :
            {
// #warning operator++/--() and operator++/--(int) not considered
                result = compute_expression_type_rec(ASTSon0(expr), decl_context);
                // It preserves the lvalueness as the original expression should be
                // already an lvalue

                ERROR_CONDITION((number_of_types(result) > 1), 
                        "A post/preincrement cannot have more than one type", 0);
                break;
            }
        case AST_SIZEOF :
            {
                // Technically this is size_t and it is not a lvalue
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_unsigned_int_type(), 0));
                break;
            }
        case AST_SIZEOF_TYPEID :
            {
                // Technically this is size_t and it is not a lvalue
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_unsigned_int_type(), 0));
                break;
            }
        case AST_DERREFERENCE :
            {
// #warning operator* not considered for C++
                type_set_t referenced_type_set = compute_expression_type_rec(ASTSon0(expr), decl_context);

                if (number_of_types(referenced_type_set) == 0)
                    break;

                ERROR_CONDITION((number_of_types(referenced_type_set) > 1), 
                        "A derreference cannot have more than one type", 0);

                type_t* referenced_type = get_type_from_set(referenced_type_set);
                referenced_type = advance_over_typedefs(referenced_type);

                type_t* result_type = NULL;
                if (is_pointer_type(referenced_type))
                {
                    result_type = pointer_type_get_pointee_type(referenced_type);
                }
                else if (is_array_type(referenced_type))
                {
                    result_type = array_type_get_element_type(referenced_type);
                }
                else if (is_function_type(referenced_type))
                {
                    // Stupid case since here (*f) is the same as (f)
                    result_type = referenced_type;
                }

                cv_qualifier_t cv_qualif = CV_NONE;
                advance_over_typedefs_with_cv_qualif(result_type, &cv_qualif);

                char is_lvalue = !is_function_type(result_type)
                    && ((cv_qualif & CV_CONST) != CV_CONST);

                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(result_type, is_lvalue));
                break;
            }
        case AST_REFERENCE :
            {
                type_set_t referenced_type_set = compute_expression_type_rec(ASTSon0(expr), decl_context);

                if (number_of_types(referenced_type_set) == 0)
                    break;

                ERROR_CONDITION(number_of_types(referenced_type_set) > 1,
                        "Error, the types of a reference cannot be more than one", 0);

                type_t* pointer_type = get_pointer_type(get_type_from_set(referenced_type_set));
                
                // This is never a lvalue
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(pointer_type, 0));
                break;
            }
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
// #warning C++ unary operators not considered
                result = compute_expression_type_rec(ASTSon0(expr), decl_context);
                if (number_of_types(result) == 0)
                    break;

                ERROR_CONDITION((number_of_types(result) > 1),
                        "An unary expression cannot have more than one type", 0);

                // This is not a lvalue in C
                result.types[0]->is_lvalue = 0;

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
                build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                        decl_context);

                type_t* result_type = NULL;
                type_t* declarator_type = NULL;
                compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                        &declarator_type, decl_context);

                // A casted entity is not an lvalue
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(result_type, 0));
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
// #warning C++ operator overloading not implemented
                type_set_t result_lhs_set = compute_expression_type_rec(ASTSon0(expr), decl_context);
                type_set_t result_rhs_set = compute_expression_type_rec(ASTSon1(expr), decl_context);

                if ((number_of_types(result_lhs_set) == 0)
                        || (number_of_types(result_rhs_set) == 0))
                    break;

                ERROR_CONDITION(((number_of_types(result_lhs_set) > 1)
                        || (number_of_types(result_rhs_set) > 1)),
                        "A binary operation cannot have multiple types in any of the operands", 0);

                type_t* result_lhs = get_type_from_set(result_lhs_set);
                type_t* result_rhs = get_type_from_set(result_rhs_set);

                type_t* result_type;
                // Pointer arithmetic, mark it not being lvalue
                if ((is_pointer_type(result_lhs) &&
                            is_integral_type(result_rhs)))
                {
                    result_type = result_lhs;
                    break;
                }
                if (is_integral_type(result_lhs) &&
                        is_pointer_type(result_rhs))
                {
                    result_type = result_rhs;
                    break;
                }

                // Order of arithmetic promotions
                char (*ptr_is_type[])(type_t*) =
                { 
                    is_long_double_type,
                    is_float_type,
                    is_unsigned_long_long_int_type,
                    is_signed_long_long_int_type,
                    is_unsigned_long_int_type,
                    is_signed_long_int_type
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
                        result_type = ptr_func(result_lhs) ? result_lhs : result_rhs;
                        conversion_made = 1;
                        break;
                    }
                }

                if (conversion_made)
                    break;

                // As we were supposed to do integral promotions only int remains here
                result_type = get_signed_int_type();

                P_LIST_ADD(result.types, result.num_types, 
                        new_argument_type(result_type, 0));
                break;
            }
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
            {
// #warning Overload of relational operators not considered
                // In C++ should be bool for builtin ones
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_signed_int_type(), 0));
                break;
            }
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
// #warning Overload of logical operators not considered
                // In C++ should be bool for builtin ones
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_signed_int_type(), 0));
                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
                /*
                 * Note, the condition expression is not checked
                 */
                type_set_t true_type_set = compute_expression_type_rec(ASTSon1(expr), decl_context);
                type_set_t false_type_set = compute_expression_type_rec(ASTSon2(expr), decl_context);

                if ((number_of_types(true_type_set) == 0)
                        || (number_of_types(false_type_set) == 0))
                    break;

                ERROR_CONDITION(((number_of_types(true_type_set) > 1)
                        || (number_of_types(false_type_set) > 1)),
                        "Conditional expression cannot have several types in any of the branches", 0);

                char is_lvalue_1 = get_lvalueness_from_set(true_type_set);
                char is_lvalue_2 = get_lvalueness_from_set(false_type_set);

                char is_lvalue = (is_lvalue_1 && is_lvalue_2);

                // It is lvalue if both expressions are
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_type_from_set(true_type_set), is_lvalue));
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
// #warning Operator assignments not considered yet in C++
                // The type is always the one of the left part of the assignment
                type_set_t result_set = compute_expression_type_rec(ASTSon0(expr), decl_context);

                if (number_of_types(result_set) == 0)
                    break;

                ERROR_CONDITION((number_of_types(result_set) > 1), 
                        "Assignment operation cannot have more than one type", 0);

                // Not lvalue
                P_LIST_ADD(result.types, result.num_types,
                        new_argument_type(get_type_from_set(result_set), 0));
                break;
            }
        default:
            internal_error("Unknown node type %s at %s\n", ast_print_node_type(ASTType(expr)), node_information(expr));
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
                return is_unsigned == 0 ? get_signed_int_type() : get_unsigned_int_type();
            }
        case 1 : 
            {
                return is_unsigned == 0 ? get_signed_long_int_type() : get_unsigned_long_int_type();
            }
        default :
            {
                return is_unsigned == 0 ? get_signed_long_long_int_type() : get_unsigned_long_long_int_type();
            }
    }
}

static type_t *character_literal(AST expr)
{
    char *literal = ASTText(expr);

    type_t* result = NULL;
    if (*literal != 'L')
    {
        result = get_char_type();
    }
    else
    {
        result = get_wchar_t_type();
    }

    return result;
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
        return get_long_double_type();
    }
    else if (is_float)
    {
        return get_float_type();
    }
    else 
        return get_double_type();
}

static type_t *string_literal(AST expr)
{
    char *literal = ASTText(expr);

    if (*literal != 'L')
    {
        return get_pointer_type(get_char_type());
    }
    else
    {
        return get_pointer_type(get_wchar_t_type());
    }

    
}
