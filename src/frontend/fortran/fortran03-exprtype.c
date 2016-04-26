/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/



#include "fortran03-exprtype.h"
#include "fortran03-buildscope.h"
#include "fortran03-scope.h"
#include "fortran03-prettyprint.h"
#include "fortran03-typeutils.h"
#include "fortran03-intrinsics.h"
#include "fortran03-codegen.h"
#include "fortran03-cexpr.h"
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-ast.h"
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include "cxx-tltype.h"
#include "cxx-nodecl.h"
#include "cxx-nodecl-output.h"
#include "cxx-diagnostic.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

static void fortran_check_expression_impl_(AST expression, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static void check_symbol_of_called_name(AST sym, const decl_context_t* decl_context, scope_entry_list_t** symbol_list, char is_call_stmt);

static void check_symbol_of_argument(AST sym, const decl_context_t* decl_context, nodecl_t* nodecl_output);


static nodecl_t fortran_nodecl_adjust_function_argument(
        type_t* parameter_type,
        nodecl_t argument);

char fortran_check_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    fortran_check_expression_impl_(a, decl_context, nodecl_output);

    return !nodecl_is_err_expr(*nodecl_output);
}

static int _checking_initializer = 0;
static void fortran_push_checking_initializer(void)
{
    _checking_initializer++;
}

static void fortran_pop_checking_initializer(void)
{
    _checking_initializer--;
    ERROR_CONDITION(_checking_initializer < 0, 
            "Invalid value %d for _checking_initializer", _checking_initializer);
}

static char fortran_checking_initializer(void)
{
    return _checking_initializer != 0;
}

static int _checking_array_expression = 0;
static void fortran_push_checking_array_expression(void)
{
    _checking_array_expression++;
}

static void fortran_pop_checking_array_expression(void)
{
    _checking_array_expression--;
    ERROR_CONDITION(_checking_array_expression < 0, 
            "Invalid value %d for _checking_array_expression", _checking_array_expression);
}

static char fortran_checking_array_expression(void)
{
    return _checking_array_expression != 0;
}

char fortran_check_array_bounds_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    fortran_push_checking_array_expression();
    fortran_check_expression_impl_(a, decl_context, nodecl_output);
    *nodecl_output = fortran_expression_as_value(*nodecl_output);
    fortran_pop_checking_array_expression();

    return !nodecl_is_err_expr(*nodecl_output);
}

typedef void (*check_expression_function_t)(AST statement, const decl_context_t*, nodecl_t* nodecl_output);
typedef struct check_expression_handler_tag
{
    node_t ast_kind;
    check_expression_function_t handler;
} check_expression_handler_t;

#define STATEMENT_HANDLER_TABLE \
 STATEMENT_HANDLER(AST_ADD, check_add_op) \
 STATEMENT_HANDLER(AST_ARRAY_CONSTRUCTOR, check_array_constructor) \
 STATEMENT_HANDLER(AST_ARRAY_SUBSCRIPT, check_array_ref) \
 STATEMENT_HANDLER(AST_BINARY_LITERAL, check_binary_literal) \
 STATEMENT_HANDLER(AST_BOOLEAN_LITERAL, check_boolean_literal) \
 STATEMENT_HANDLER(AST_COMPLEX_LITERAL, check_complex_literal) \
 STATEMENT_HANDLER(AST_CLASS_MEMBER_ACCESS, check_component_ref) \
 STATEMENT_HANDLER(AST_CONCAT, check_concat_op) \
 STATEMENT_HANDLER(AST_DECIMAL_LITERAL, check_decimal_literal) \
 STATEMENT_HANDLER(AST_DERIVED_TYPE_CONSTRUCTOR, check_derived_type_constructor) \
 STATEMENT_HANDLER(AST_DIFFERENT, check_different_op) \
 STATEMENT_HANDLER(AST_DIV, check_div_op) \
 STATEMENT_HANDLER(AST_EQUAL, check_equal_op) \
 STATEMENT_HANDLER(AST_FLOATING_LITERAL, check_floating_literal) \
 STATEMENT_HANDLER(AST_FUNCTION_CALL, check_function_call) \
 STATEMENT_HANDLER(AST_GREATER_OR_EQUAL_THAN, check_greater_or_equal_than) \
 STATEMENT_HANDLER(AST_GREATER_THAN, check_greater_than) \
 STATEMENT_HANDLER(AST_HEXADECIMAL_LITERAL, check_hexadecimal_literal) \
 STATEMENT_HANDLER(AST_HOLLERITH_CONSTANT, check_hollerith_constant) \
 STATEMENT_HANDLER(AST_IMAGE_REF, check_image_ref) \
 STATEMENT_HANDLER(AST_LOGICAL_AND, check_logical_and) \
 STATEMENT_HANDLER(AST_LOGICAL_EQUAL, check_logical_equal) \
 STATEMENT_HANDLER(AST_LOGICAL_DIFFERENT, check_logical_different) \
 STATEMENT_HANDLER(AST_LOGICAL_OR, check_logical_or) \
 STATEMENT_HANDLER(AST_LOWER_OR_EQUAL_THAN, check_lower_or_equal_than) \
 STATEMENT_HANDLER(AST_LOWER_THAN, check_lower_than) \
 STATEMENT_HANDLER(AST_MINUS, check_minus_op) \
 STATEMENT_HANDLER(AST_MUL, check_mult_op) \
 STATEMENT_HANDLER(AST_NEG, check_neg_op) \
 STATEMENT_HANDLER(AST_LOGICAL_NOT, check_not_op) \
 STATEMENT_HANDLER(AST_OCTAL_LITERAL, check_octal_literal) \
 STATEMENT_HANDLER(AST_PARENTHESIZED_EXPRESSION, check_parenthesized_expression) \
 STATEMENT_HANDLER(AST_PLUS, check_plus_op) \
 STATEMENT_HANDLER(AST_POWER, check_power_op) \
 STATEMENT_HANDLER(AST_STRING_LITERAL, check_string_literal) \
 STATEMENT_HANDLER(AST_USER_DEFINED_UNARY_OP, check_user_defined_unary_op) \
 STATEMENT_HANDLER(AST_SYMBOL, check_symbol_of_variable) \
 STATEMENT_HANDLER(AST_ASSIGNMENT, check_assignment) \
 STATEMENT_HANDLER(AST_PTR_ASSIGNMENT, check_ptr_assignment) \
 STATEMENT_HANDLER(AST_AMBIGUITY, disambiguate_expression) \
 STATEMENT_HANDLER(AST_USER_DEFINED_BINARY_OP, check_user_defined_binary_op) \
 STATEMENT_HANDLER(AST_NODECL_LITERAL, check_nodecl_literal) \
 STATEMENT_HANDLER(AST_SYMBOL_LITERAL_REF, check_symbol_literal) \
 STATEMENT_HANDLER(AST_MULTIEXPRESSION, check_multiexpression) \

// Enable this if you really need extremely verbose typechecking
// #define VERBOSE_DEBUG_EXPR 1

#ifdef VERBOSE_DEBUG_EXPR
  // Prototypes
  #define STATEMENT_HANDLER(_kind, _handler) \
      static void _handler(AST, const decl_context_t*); \
      static void _handler##_(AST a, const decl_context_t* d) \
      { \
          DEBUG_CODE() \
          { \
              fprintf(stderr, "%s: -> %s\n", ast_location(a), #_handler); \
          } \
          _handler(a, d); \
          DEBUG_CODE() \
          { \
              fprintf(stderr, "%s: <- %s\n", ast_location(a), #_handler); \
          } \
      }
#else
  #define STATEMENT_HANDLER(_kind, _handler) \
      static void _handler(AST, const decl_context_t*, nodecl_t* nodecl_output); 
#endif

STATEMENT_HANDLER_TABLE
#undef STATEMENT_HANDLER

// Table
#ifdef VERBOSE_DEBUG_EXPR
  #define STATEMENT_HANDLER(_kind, _handler) \
     { .ast_kind = _kind, .handler = _handler##_ },
#else
  #define STATEMENT_HANDLER(_kind, _handler) \
     { .ast_kind = _kind, .handler = _handler },
#endif
static check_expression_handler_t check_expression_function[] = 
{
  STATEMENT_HANDLER_TABLE
};
#undef STATEMENT_HANDLER

static char is_call_to_null(nodecl_t node, type_t** ptr_type)
{
    if (ptr_type != NULL)
        *ptr_type = NULL;

    scope_entry_t* function_called = NULL;
    char ok = 0;
    if (!nodecl_is_null(node)
            && is_zero_type(nodecl_get_type(node)))
    {
        ok = 1;
    }
    else if (!nodecl_is_null(node)
            && nodecl_get_kind(node) == NODECL_FUNCTION_CALL
            && ((function_called = nodecl_get_symbol(nodecl_get_child(node, 0))) != NULL)
            && strcasecmp(function_called->symbol_name, "null") == 0
            && symbol_entity_specs_get_is_builtin(function_called))
    {
        ok = 1;
    }

    if (!ok)
        return 0;

    if (ptr_type != NULL)
        *ptr_type = function_type_get_return_type(function_called->type_information);
    return 1;
}

static int check_expression_function_init = 0;

static int check_expression_function_compare(const void *a, const void *b)
{
    check_expression_handler_t *pa = (check_expression_handler_t*)a;
    check_expression_handler_t *pb = (check_expression_handler_t*)b;

    if (pa->ast_kind < pb->ast_kind)
        return -1;
    else if (pa->ast_kind > pb->ast_kind)
        return 1;
    else
        return 0;
}

// Like const_value_to_nodecl but does not fold arrays or structures
static nodecl_t fortran_const_value_to_nodecl(const_value_t* v)
{
    if (const_value_is_array(v) 
            || const_value_is_structured(v))
    {
        return nodecl_null();
    }
    else
    {
        return const_value_to_nodecl(v);
    }
}

static void fortran_check_expression_impl_(AST expression, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    ERROR_CONDITION(expression == NULL, "Invalid tree for expression", 0);
    ERROR_CONDITION(nodecl_output == NULL, "Nodecl cannot be NULL here", 0);

    // Sort the array if needed
    if (!check_expression_function_init)
    {
        // void qsort(void *base, size_t nmemb, size_t size,
        //    int(*compar)(const void *, const void *));
        qsort(check_expression_function, 
                sizeof(check_expression_function) / sizeof(check_expression_function[0]),
                sizeof(check_expression_function[0]),
                check_expression_function_compare);
        check_expression_function_init = 1;
    }

    check_expression_handler_t key = { .ast_kind = ASTKind(expression) };
    check_expression_handler_t *handler = NULL;

    // void *bsearch(const void *key, const void *base,
    //       size_t nmemb, size_t size,
    //       int (*compar)(const void *, const void *));
    handler = (check_expression_handler_t*)bsearch(&key, check_expression_function, 
            sizeof(check_expression_function) / sizeof(check_expression_function[0]),
            sizeof(check_expression_function[0]),
            check_expression_function_compare);
    if (handler == NULL 
            || handler->handler == NULL)
    {
        fatal_printf_at(ast_get_locus(expression), "unhandled expression %s\n",
                ast_print_node_type(ASTKind(expression)));
    }
    (handler->handler)(expression, decl_context, nodecl_output);

    ERROR_CONDITION(nodecl_is_null(*nodecl_output), "Nodecl cannot be NULL here", 0);

    ERROR_CONDITION(
            (!nodecl_is_err_expr(*nodecl_output)
             && (nodecl_get_type(*nodecl_output) == NULL ||
                 is_error_type(nodecl_get_type(*nodecl_output)))),
            "This should be an error expression", 0);

    DEBUG_CODE()
    {
        if (!nodecl_is_constant(*nodecl_output))
        {
            fprintf(stderr, "EXPRTYPE: %s: '%s' has type '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression),
                    print_declarator(nodecl_get_type(*nodecl_output)));
        }
        else
        {
            const_value_t* konst = nodecl_get_constant(*nodecl_output);
            fprintf(stderr, "EXPRTYPE: %s: '%s' has type '%s' with a constant value of '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression),
                    print_declarator(nodecl_get_type(*nodecl_output)),
                    const_value_to_str(konst));
        }
    }

    // if (CURRENT_CONFIGURATION->strict_typecheck)
    // {
    //     if (nodecl_get_type(*nodecl_output) == NULL
    //             || nodecl_is_err_expr(*nodecl_output))
    //     {
    //         internal_error("%s: invalid expression '%s'\n",
    //                 ast_location(expression),
    //                 fortran_prettyprint_in_buffer(expression));
    //     }
    // }
}

static type_t* compute_result_of_intrinsic_operator(AST expr, const decl_context_t*,
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output);

static void common_binary_check(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output);
static void common_unary_check(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output);

static void check_add_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static char is_intrinsic_assignment(type_t* lvalue_type, type_t* rvalue_type);

static void check_ac_value_list(
        AST ac_value_list,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output,
        type_t** current_type,
        const_value_t** ac_value_const)
{
    int ac_value_length = 0;
    AST it;
    for_each_element(ac_value_list, it)
    {
        ac_value_length++;
    }

    const_value_t** ac_constant_values = NEW_VEC0(const_value_t*, ac_value_length);
    int item_position = 0;
    for_each_element(ac_value_list, it)
    {
        AST ac_value = ASTSon1(it);

        if (ASTKind(ac_value) == AST_IMPLIED_DO)
        {
            AST implied_do_ac_value = ASTSon0(ac_value);

            AST implied_do_control = ASTSon1(ac_value);
            AST ac_do_variable = ASTSon0(implied_do_control);
            AST lower_bound = ASTSon1(implied_do_control);
            AST upper_bound = ASTSon2(implied_do_control);
            AST stride = ASTSon3(implied_do_control);

            nodecl_t nodecl_lower = nodecl_null();
            fortran_check_expression_impl_(lower_bound, decl_context, &nodecl_lower);
            nodecl_lower = fortran_expression_as_value(nodecl_lower);
            nodecl_t nodecl_upper = nodecl_null();
            fortran_check_expression_impl_(upper_bound, decl_context, &nodecl_upper);
            nodecl_upper = fortran_expression_as_value(nodecl_upper);
            nodecl_t nodecl_stride = nodecl_null();
            if (stride != NULL)
            {
                fortran_check_expression_impl_(stride, decl_context, &nodecl_stride);
                nodecl_stride = fortran_expression_as_value(nodecl_stride);
            }
            else
            {
                nodecl_stride = const_value_to_nodecl(
                        const_value_get_one(/* bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1));
                nodecl_set_locus_as(nodecl_stride, nodecl_upper);
            }

            scope_entry_t* do_variable = fortran_get_variable_with_locus(decl_context, ac_do_variable, ASTText(ac_do_variable));

            if (do_variable == NULL)
            {
                error_printf_at(ast_get_locus(ac_do_variable), "unknown symbol '%s' in ac-implied-do\n", ASTText(ac_do_variable));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(ac_do_variable));
                *current_type = get_error_type();
                return;
            }

            if (do_variable->kind == SK_UNDEFINED)
            {
                do_variable->kind = SK_VARIABLE;
                remove_unknown_kind_symbol(decl_context, do_variable);
            }
            else if (do_variable->kind != SK_VARIABLE)
            {
                error_printf_at(ast_get_locus(ac_do_variable), "invalid name '%s' for ac-implied-do\n", ASTText(ac_do_variable));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(ac_do_variable));
                *current_type = get_error_type();
                return;
            }

            nodecl_t nodecl_ac_value = nodecl_null();
            const_value_t* whole_range_constant = NULL;
            check_ac_value_list(implied_do_ac_value, decl_context,
                    &nodecl_ac_value, current_type, &whole_range_constant);

            if (nodecl_is_err_expr(nodecl_ac_value))
            {
                *nodecl_output = nodecl_ac_value;
                return;
            }

            const_value_t* implied_do_cval = NULL;
            // Compute the constant if possible
            if (nodecl_is_constant(nodecl_lower)
                    && nodecl_is_constant(nodecl_upper)
                    && nodecl_is_constant(nodecl_stride))
            {
                int val_lower = const_value_cast_to_signed_int(nodecl_get_constant(nodecl_lower));
                int val_upper = const_value_cast_to_signed_int(nodecl_get_constant(nodecl_upper));
                int val_stride = const_value_cast_to_signed_int(nodecl_get_constant(nodecl_stride));

                if (val_stride == 0)
                {
                    error_printf_at(ast_get_locus(stride), "step of implied-do is zero\n");
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(stride));
                    return;
                }

                int trip = (val_upper
                        - val_lower
                        + val_stride)
                    / val_stride;

                if (trip < 0)
                {
                    trip = 0;
                }

                // Save information of the symbol
                type_t* original_type = do_variable->type_information;
                nodecl_t original_value = do_variable->value;

                // Set it as a PARAMETER of kind INTEGER (so we will effectively use its value)
                do_variable->type_information = get_const_qualified_type(original_type);

                if (trip == 0)
                {
                    // empty range
                    implied_do_cval = const_value_make_array(0, NULL);
                }
                else
                {
                    int stride_sign = 1;
                    if (val_stride < 0)
                        stride_sign = -1;

                    const_value_t** const_value_list = NEW_VEC0(const_value_t*, trip);
                    const_value_t** const_value_list_it = const_value_list;

                    char all_constant = 1;
                    int i;
                    for (i = val_lower;
                            ((stride_sign * i) <= (stride_sign * val_upper)) && all_constant;
                            i+= val_stride)
                    {
                        // Set the value of the variable
                        do_variable->value = const_value_to_nodecl(const_value_get_signed_int(i));
                        nodecl_set_locus_as(do_variable->value, nodecl_lower);

                        nodecl_t nodecl_current_item = nodecl_null();

                        const_value_t* current_constant = NULL;
                        check_ac_value_list(implied_do_ac_value, decl_context,
                                &nodecl_current_item,
                                current_type,
                                &current_constant);

                        if (nodecl_is_err_expr(nodecl_current_item))
                        {
                            *nodecl_output = nodecl_current_item;
                            return;
                        }

                        if (current_constant == NULL)
                        {
                            all_constant = 0;
                        }
                        else
                        {
                            *const_value_list_it = current_constant;
                            const_value_list_it++;
                        }

                        // This node is useless now
                        nodecl_free(nodecl_current_item);
                    }

                    if (all_constant)
                    {
                        implied_do_cval = const_value_make_array(trip, const_value_list);
                        // this constant must always be rank-1
                        implied_do_cval = fortran_flatten_array(implied_do_cval);
                    }

                    DELETE(const_value_list);
                }

                // Restore the variable used for the expansion
                do_variable->type_information = original_type;
                do_variable->value = original_value;
            }

            nodecl_t nodecl_implied_do =
                nodecl_make_fortran_implied_do(
                        nodecl_make_symbol(do_variable, ast_get_locus(ac_do_variable)),
                        nodecl_make_range(
                            nodecl_lower,
                            nodecl_upper,
                            nodecl_stride,
                            fortran_get_default_integer_type(),
                            ast_get_locus(implied_do_control)),
                        nodecl_ac_value,
                        ast_get_locus(implied_do_control));

            nodecl_set_constant(nodecl_implied_do, implied_do_cval);
            ac_constant_values[item_position] = implied_do_cval;

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_implied_do);
        }
        else
        {
            nodecl_t nodecl_expr = nodecl_null();
            fortran_check_expression_impl_(ac_value, decl_context, &nodecl_expr);
            nodecl_expr = fortran_expression_as_value(nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
            {
                *nodecl_output = nodecl_expr;
                return;
            }
            else if (*current_type == NULL)
            {
                *current_type = fortran_get_rank0_type(nodecl_get_type(nodecl_expr));
            }
            else if (*current_type != NULL)
            {
                if (!is_intrinsic_assignment(*current_type,
                            fortran_get_rank0_type(nodecl_get_type(nodecl_expr))))
                {
                    error_printf_at(nodecl_get_locus(nodecl_expr), "expression of type '%s' is not conformable in an array constructor of type '%s'\n",
                            fortran_print_type_str(fortran_get_rank0_type(nodecl_get_type(nodecl_expr))),
                            fortran_print_type_str(*current_type));
                    *nodecl_output = nodecl_make_err_expr(nodecl_get_locus(nodecl_expr));
                    *current_type = get_error_type();
                    return;
                }
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_expr);
            if (nodecl_is_constant(nodecl_expr))
            {
                const_value_t* value = nodecl_get_constant(nodecl_expr);
                if (!const_value_is_array(value))
                {
                    value = const_value_make_array(1, &value);
                }

                ac_constant_values[item_position] = value;
            }
        }

        item_position++;
    }

    char all_constant = 1;
    for (item_position = 0; (item_position < ac_value_length) && all_constant; item_position++)
    {
        if (ac_constant_values[item_position] == NULL)
            all_constant = 0;
    }

    if (all_constant)
    {
        *ac_value_const = const_value_make_array(ac_value_length, ac_constant_values);
        *ac_value_const = fortran_flatten_array(*ac_value_const);
    }
    else
    {
        *ac_value_const = NULL;
    }

    DELETE(ac_constant_values);
}

static void check_array_constructor(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST ac_spec = ASTSon0(expr);
    AST type_spec = ASTSon0(ac_spec);

    type_t* ac_value_type = NULL;
    if (type_spec != NULL)
    {
        ac_value_type = fortran_gather_type_from_declaration_type_spec(type_spec, decl_context,
                /* character_length_out */ NULL);

        if (is_error_type(ac_value_type))
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }

    AST ac_value_list = ASTSon1(ac_spec);
    nodecl_t nodecl_ac_value = nodecl_null();
    const_value_t* ac_value_const = NULL;
    if (ac_value_list != NULL)
    {
        check_ac_value_list(ac_value_list, decl_context, &nodecl_ac_value, &ac_value_type, &ac_value_const);
        if (is_error_type(ac_value_type))
        {
            // Empty ranges may return null trees
            if (nodecl_is_null(nodecl_ac_value))
                nodecl_ac_value = nodecl_make_err_expr(ast_get_locus(expr));

            *nodecl_output = nodecl_ac_value;
            return;
        }
    }
    else
    {
        if (ac_value_type == NULL)
        {
            error_printf_at(ast_get_locus(expr), "invalid empty array-constructor without type-specifier\n");
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }

    if (ac_value_const != NULL)
    {
        int num_items = const_value_get_num_elements(ac_value_const);
        ac_value_type = get_array_type_bounds(ac_value_type,
                const_value_to_nodecl(const_value_get_one(fortran_get_default_integer_type_kind(), /* signed */ 1)),
                const_value_to_nodecl(const_value_get_signed_int(num_items)),
                decl_context);
        ac_value_type = get_const_qualified_type(ac_value_type);
    }
    else
    {
        // Maybe the values of the array are not constant but the size is
        int n;
        nodecl_t* list = nodecl_unpack_list(nodecl_ac_value, &n);

        int num_items = 0;

        int i;
        for (i = 0; (i < n) && (num_items >= 0); i++)
        {
            if (nodecl_get_kind(list[i]) != NODECL_FORTRAN_IMPLIED_DO)
            {
                // A single element
                num_items++;
            }
            else if (nodecl_is_constant(list[i])) // nodecl_get_kind(list[i]) == NODECL_FORTRAN_IMPLIED_DO
            {
                // Well, this is an ac-implied-do but we know how many elements it has
                num_items += const_value_get_num_elements(nodecl_get_constant(list[i]));
            }
            else // nodecl_get_kind(list[i]) == NODECL_FORTRAN_IMPLIED_DO && !nodecl_is_constant(list[i])
            {
                num_items = -1;
            }
        }

        if (num_items < 0)
        {
            ac_value_type = get_array_type_bounds(ac_value_type, nodecl_null(), nodecl_null(), decl_context);
        }
        else
        {
            ac_value_type = get_array_type_bounds(ac_value_type,
                    const_value_to_nodecl(const_value_get_one(fortran_get_default_integer_type_kind(), /* signed */ 1)),
                    const_value_to_nodecl(const_value_get_signed_int(num_items)),
                    decl_context);
        }
    }

    nodecl_t nodecl_array_constructor_form = nodecl_null();

    if (type_spec != NULL)
    {
        nodecl_array_constructor_form =
            nodecl_make_structured_value_fortran_typespec_array_constructor(
                ast_get_locus(type_spec));
    }

    *nodecl_output = nodecl_make_structured_value(nodecl_ac_value,
            nodecl_array_constructor_form,
            ac_value_type,
            ast_get_locus(expr));

    nodecl_set_constant(*nodecl_output, ac_value_const);
}

static void check_substring(AST expr, const decl_context_t* decl_context, nodecl_t nodecl_subscripted, nodecl_t* nodecl_output)
{
    type_t* lhs_type = no_ref(nodecl_get_type(nodecl_subscripted));

    AST subscript_list = ASTSon1(expr);

    int num_subscripts = 0;
    AST it;
    for_each_element(subscript_list, it)
    {
        num_subscripts++;
    }

    if (num_subscripts != 1)
    {
        error_printf_at(ast_get_locus(expr), "invalid number of subscripts (%d) in substring expression\n",
                num_subscripts);
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    AST subscript = ASTSon1(subscript_list);

    AST lower = ASTSon0(subscript);
    AST upper = ASTSon1(subscript);
    AST stride = ASTSon2(subscript);

    if (stride != NULL)
    {
        error_printf_at(ast_get_locus(expr), "a stride is not valid in a substring expression\n");
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    nodecl_t nodecl_lower = nodecl_null();
    if (lower != NULL)
    {
        fortran_check_expression_impl_(lower, decl_context, &nodecl_lower);
        nodecl_lower = fortran_expression_as_value(nodecl_lower);
    }

    nodecl_t nodecl_upper = nodecl_null();
    if (upper != NULL)
    {
        fortran_check_expression_impl_(upper, decl_context, &nodecl_upper);
        nodecl_upper = fortran_expression_as_value(nodecl_upper);
    }

    type_t* string_type = fortran_get_rank0_type(lhs_type);
    ERROR_CONDITION(!fortran_is_character_type(string_type), "Bad string type", 0);

    // Rebuild string type
    type_t* synthesized_type = get_array_type_bounds(array_type_get_element_type(string_type), nodecl_lower, nodecl_upper, decl_context);
    if (fortran_is_array_type(lhs_type))
    {
        ERROR_CONDITION(!fortran_is_scalar_type(synthesized_type)
                && !fortran_is_character_type(synthesized_type), "Invalid synthesized_type type", 0);

        synthesized_type = fortran_rebuild_array_type(synthesized_type, lhs_type);
    }

    nodecl_t nodecl_stride = const_value_to_nodecl(
            const_value_get_one(/* bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1));
    nodecl_set_locus(nodecl_stride, ast_get_locus(expr));

    type_t* data_type = synthesized_type;
    if (is_any_reference_type(nodecl_get_type(nodecl_subscripted)))
    {
        data_type = lvalue_ref(data_type);
    }

    *nodecl_output = nodecl_make_array_subscript(
            nodecl_subscripted,
            nodecl_make_list_1(
                nodecl_make_range(nodecl_lower, nodecl_upper, nodecl_stride, fortran_get_default_integer_type(), ast_get_locus(expr))),
            data_type,
            ast_get_locus(expr));
    // FIXME - We should compute a constant
}


static const_value_t* compute_subconstant_of_array_rec(
        const_value_t* current_rank_value,
        type_t* current_array_type,
        nodecl_t* all_subscripts,
        int current_subscript, 
        int total_subscripts)
{
    nodecl_t current_nodecl_subscript = all_subscripts[(total_subscripts - 1) - current_subscript];
    const_value_t* const_of_subscript = nodecl_get_constant(current_nodecl_subscript);

    const_value_t* result_value = NULL;

    int array_rank_base = const_value_cast_to_signed_int(
            nodecl_get_constant(array_type_get_array_lower_bound(current_array_type)));

    if (const_value_is_range(const_of_subscript))
    {
        int lower = const_value_cast_to_signed_int(const_value_get_element_num(const_of_subscript, 0));
        int upper = const_value_cast_to_signed_int(const_value_get_element_num(const_of_subscript, 1));
        int stride = const_value_cast_to_signed_int(const_value_get_element_num(const_of_subscript, 2));
        int trip = (upper - lower + stride) / stride;

        const_value_t* result_array[trip];
        memset(result_array, 0, sizeof(result_array));

        int i, item = 0;
        if (stride > 0)
        {
            for (i = lower; i <= upper; i += stride, item++)
            {
                if ((current_subscript + 1) == total_subscripts)
                {
                    result_array[item] = const_value_get_element_num(current_rank_value, i - array_rank_base);
                }
                else
                {
                    result_array[item] = compute_subconstant_of_array_rec(
                            const_value_get_element_num(current_rank_value, i - array_rank_base),
                            array_type_get_element_type(current_array_type),
                            all_subscripts,
                            current_subscript + 1,
                            total_subscripts);
                }
            }
        }
        else
        {
            for (i = lower; i >= upper; i += stride, item++)
            {
                if ((current_subscript + 1) == total_subscripts)
                {
                    result_array[item] = const_value_get_element_num(current_rank_value, i - array_rank_base);
                }
                else
                {
                    result_array[item] = compute_subconstant_of_array_rec(
                            const_value_get_element_num(current_rank_value, i - array_rank_base),
                            array_type_get_element_type(current_array_type),
                            all_subscripts,
                            current_subscript + 1,
                            total_subscripts);
                }
            }
        }

        result_value = const_value_make_array(trip, result_array);
    }
    else if (const_value_is_array(const_of_subscript))
    {
        int trip = const_value_get_num_elements(const_of_subscript);

        const_value_t* result_array[trip];
        memset(result_array, 0, sizeof(result_array));

        int p;
        for (p = 0; p < trip; p++)
        {
            int i = const_value_cast_to_signed_int(
                    const_value_get_element_num(const_of_subscript, p));

            if ((current_subscript + 1) == total_subscripts)
            {
                result_array[p] = const_value_get_element_num(current_rank_value, i - array_rank_base);
            }
            else
            {
                result_array[p] = compute_subconstant_of_array_rec(
                        const_value_get_element_num(current_rank_value, i - array_rank_base),
                        array_type_get_element_type(current_array_type),
                        all_subscripts,
                        current_subscript + 1,
                        total_subscripts);
            }
        }

        result_value = const_value_make_array(trip, result_array);
    }
    else
    {
        int i = const_value_cast_to_signed_int(const_of_subscript);
        if ((current_subscript + 1) == total_subscripts)
        {
            result_value = const_value_get_element_num(current_rank_value, i - array_rank_base);
        }
        else
        {
            result_value = compute_subconstant_of_array_rec(
                    const_value_get_element_num(current_rank_value, i - array_rank_base),
                    array_type_get_element_type(current_array_type),
                    all_subscripts,
                    current_subscript + 1,
                    total_subscripts);
        }
    }

    ERROR_CONDITION(result_value == NULL, "This is not possible", 0);
    return result_value;
}

static const_value_t* compute_subconstant_of_array(
        const_value_t* current_rank_value,
        type_t* array_type,
        nodecl_t* all_subscripts,
        int total_subscripts)
{
    return compute_subconstant_of_array_rec(current_rank_value, 
            array_type,
            all_subscripts,
            0, total_subscripts);
}

scope_entry_t* fortran_data_ref_get_symbol(nodecl_t n)
{
    switch (nodecl_get_kind(n))
    {
        case NODECL_SYMBOL:
            return nodecl_get_symbol(n);
        case NODECL_DEREFERENCE:
            return fortran_data_ref_get_symbol(nodecl_get_child(n, 0));
        case NODECL_ARRAY_SUBSCRIPT:
            return fortran_data_ref_get_symbol(nodecl_get_child(n, 0));
        case NODECL_CLASS_MEMBER_ACCESS:
            return fortran_data_ref_get_symbol(nodecl_get_child(n, 1));
        case NODECL_CONVERSION:
            return fortran_data_ref_get_symbol(nodecl_get_child(n, 0));
        default:
            return NULL;
    }
}

static void check_array_ref_(
        AST expr,
        const decl_context_t* decl_context,
        nodecl_t nodecl_subscripted,
        nodecl_t whole_expression,
        nodecl_t* nodecl_output,
        char do_complete_array_ranks,
        char require_lower_bound)
{
    char symbol_is_invalid = 0;

    type_t* array_type = NULL;
    type_t* synthesized_type = NULL;

    int rank_of_type = -1;

    scope_entry_t* symbol = fortran_data_ref_get_symbol(nodecl_subscripted);
    if (symbol == NULL
            || (!fortran_is_array_type(no_ref(symbol->type_information))
                && !fortran_is_pointer_to_array_type(no_ref(symbol->type_information))))
    {
        symbol_is_invalid = 1;
    }
    else
    {
        array_type = no_ref(symbol->type_information);
        if (fortran_is_pointer_to_array_type(array_type))
            array_type = pointer_type_get_pointee_type(array_type);

        synthesized_type = fortran_get_rank0_type(array_type);
        rank_of_type = fortran_get_rank_of_type(array_type);
    }

    AST subscript_list = ASTSon1(expr);

    int num_subscripts = 0;
    AST it;
    for_each_element(subscript_list, it)
    {
        num_subscripts++;
    }

    type_t* dimension_type = array_type;
    nodecl_t nodecl_indexes[num_subscripts];
    memset(nodecl_indexes, 0, sizeof(nodecl_indexes));

    nodecl_t nodecl_lower_dim[num_subscripts];
    memset(nodecl_lower_dim, 0, sizeof(nodecl_lower_dim));

    nodecl_t nodecl_upper_dim[num_subscripts];
    memset(nodecl_upper_dim, 0, sizeof(nodecl_upper_dim));
    int i;
    for (i = 0; i < num_subscripts; i++)
    {
        int current_idx = (num_subscripts - 1) - i;

        nodecl_lower_dim[current_idx] = nodecl_null();
        nodecl_upper_dim[current_idx] = nodecl_null();
        if (is_array_type(dimension_type))
        {
            nodecl_lower_dim[current_idx] = array_type_get_array_lower_bound(dimension_type);
            nodecl_upper_dim[current_idx] = array_type_get_array_upper_bound(dimension_type);

            dimension_type = array_type_get_element_type(dimension_type);
        }

        nodecl_indexes[i] = nodecl_null();
    }

    num_subscripts = 0;
    for_each_element(subscript_list, it)
    {
        AST subscript = ASTSon1(it);

        if (ASTKind(subscript) == AST_SUBSCRIPT_TRIPLET)
        {
            AST lower = ASTSon0(subscript);
            AST upper = ASTSon1(subscript);
            AST stride = ASTSon2(subscript);
            nodecl_t nodecl_lower = nodecl_null();
            nodecl_t nodecl_upper = nodecl_null();
            nodecl_t nodecl_stride = nodecl_null();

            if (lower != NULL)
            {
                fortran_check_expression_impl_(lower, decl_context, &nodecl_lower);
                nodecl_lower = fortran_expression_as_value(nodecl_lower);
            }
            else
            {
                if (require_lower_bound)
                {
                    error_printf_at(ast_get_locus(subscript), "lower bound is mandatory\n");
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }

                nodecl_lower = nodecl_shallow_copy(nodecl_lower_dim[num_subscripts]);
            }

            if (upper != NULL)
            {
                fortran_check_expression_impl_(upper, decl_context, &nodecl_upper);
                nodecl_upper = fortran_expression_as_value(nodecl_upper);
            }
            else
            {
                nodecl_upper = nodecl_shallow_copy(nodecl_upper_dim[num_subscripts]);
                if (nodecl_is_null(nodecl_upper)
                        && (it == subscript_list) // This is the last subscript
                        && !symbol_is_invalid
                        && symbol_is_parameter_of_function(symbol, decl_context->current_scope->related_entry)
                        && is_array_type(no_ref(symbol->type_information))
                        && !array_type_with_descriptor(no_ref(symbol->type_information)))
                {
                    error_printf_at(ast_get_locus(subscript), "array-section of assumed-size array '%s' lacks the upper bound\n",
                            symbol->symbol_name);
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }
            }

            if (stride != NULL)
            {
                fortran_check_expression_impl_(stride, decl_context, &nodecl_stride);
                nodecl_stride = fortran_expression_as_value(nodecl_stride);
            }
            else
            {
                nodecl_stride = const_value_to_nodecl(const_value_get_one(/* bytes */ fortran_get_default_integer_type_kind(), /* signed */ 1));
                nodecl_set_locus(nodecl_stride, ast_get_locus(subscript));
            }

            if (!nodecl_is_null(nodecl_lower)
                    && nodecl_is_err_expr(nodecl_lower))
            {
                *nodecl_output = nodecl_lower;
                return;
            }

            if (!nodecl_is_null(nodecl_upper)
                    && nodecl_is_err_expr(nodecl_upper))
            {
                *nodecl_output = nodecl_upper;
                return;
            }

            if (nodecl_is_err_expr(nodecl_stride))
            {
                *nodecl_output = nodecl_stride;
                return;
            }

            if (!symbol_is_invalid)
            {
                // Make ranges explicit through the usage of LBOUND and UBOUND
                if (do_complete_array_ranks
                        && nodecl_is_null(nodecl_lower))
                {
                    nodecl_t nodecl_actual_arguments[2] =
                    {
                        nodecl_make_fortran_actual_argument(
                                nodecl_shallow_copy(whole_expression),
                                ast_get_locus(subscript)),
                        nodecl_make_fortran_actual_argument(
                                const_value_to_nodecl(const_value_get_signed_int(num_subscripts + 1)),
                                ast_get_locus(subscript))
                    };

                    scope_entry_t* intrinsic_lbound = fortran_solve_generic_intrinsic_call(
                            fortran_query_intrinsic_name_str(decl_context, "lbound"),
                            nodecl_actual_arguments,
                            /* explicit_num_actual_arguments */ 2,
                            /* is_call */ 0);

                    ERROR_CONDITION(
                            intrinsic_lbound == NULL,
                            "Failure while doing an internal call to lbound at %s", 
                            locus_to_str(ast_get_locus(subscript)));

                    fortran_simplify_specific_intrinsic_call(intrinsic_lbound,
                            nodecl_actual_arguments,
                            /* explicit_num_actual_arguments */ 2,
                            &nodecl_lower,
                            ast_get_locus(subscript));

                    if (nodecl_is_null(nodecl_lower))
                    {
                        nodecl_t nodecl_called = nodecl_make_symbol(intrinsic_lbound, ast_get_locus(subscript));
                        nodecl_set_type(nodecl_called, lvalue_ref(intrinsic_lbound->type_information));

                        nodecl_lower = nodecl_make_function_call(
                                nodecl_called,
                                nodecl_make_list_2(
                                    nodecl_get_child(nodecl_actual_arguments[0], 0),
                                    nodecl_get_child(nodecl_actual_arguments[1], 0)),
                                /* generic spec */ nodecl_null(),
                                /* function form*/ nodecl_null(),
                                fortran_get_default_integer_type(),
                                ast_get_locus(subscript));
                    }
                }

                if (do_complete_array_ranks
                        && nodecl_is_null(nodecl_upper))
                {
                    nodecl_t nodecl_actual_arguments[2] =
                    {
                        nodecl_make_fortran_actual_argument(
                                nodecl_shallow_copy(whole_expression),
                                ast_get_locus(subscript)),
                        nodecl_make_fortran_actual_argument(
                                const_value_to_nodecl(const_value_get_signed_int(num_subscripts + 1)),
                                ast_get_locus(subscript))
                    };

                    scope_entry_t* intrinsic_ubound = fortran_solve_generic_intrinsic_call(
                            fortran_query_intrinsic_name_str(decl_context, "ubound"),
                            nodecl_actual_arguments,
                            /* explicit_num_actual_arguments */ 2,
                            /* is_call */ 0);

                    ERROR_CONDITION(
                            intrinsic_ubound == NULL,
                            "Failure while doing an internal call to ubound at %s", 
                            locus_to_str(ast_get_locus(subscript)));

                    fortran_simplify_specific_intrinsic_call(intrinsic_ubound,
                            nodecl_actual_arguments,
                            /* explicit_num_actual_arguments */ 2,
                            &nodecl_upper,
                            ast_get_locus(subscript));

                    if (nodecl_is_null(nodecl_upper))
                    {
                        nodecl_t nodecl_called = nodecl_make_symbol(intrinsic_ubound, ast_get_locus(subscript));
                        nodecl_set_type(nodecl_called, lvalue_ref(intrinsic_ubound->type_information));

                        nodecl_upper = nodecl_make_function_call(
                                nodecl_called,
                                nodecl_make_list_2(
                                    nodecl_get_child(nodecl_actual_arguments[0], 0),
                                    nodecl_get_child(nodecl_actual_arguments[1], 0)),
                                /* generic spec */ nodecl_null(),
                                /* function form*/ nodecl_null(),
                                fortran_get_default_integer_type(),
                                ast_get_locus(subscript));
                    }
                }

                nodecl_indexes[num_subscripts] = nodecl_make_range(
                        nodecl_lower,
                        nodecl_upper,
                        nodecl_stride,
                        fortran_get_default_integer_type(),
                        ast_get_locus(subscript));

                synthesized_type = get_array_type_bounds_with_regions(synthesized_type, 
                        // Original array
                        nodecl_lower_dim[num_subscripts], 
                        nodecl_upper_dim[num_subscripts],
                        decl_context,
                        // Range
                        nodecl_indexes[num_subscripts], 
                        decl_context);

                if (!nodecl_is_null(nodecl_lower)
                        && !nodecl_is_null(nodecl_upper)
                        && !nodecl_is_null(nodecl_stride)
                        && nodecl_is_constant(nodecl_lower)
                        && nodecl_is_constant(nodecl_upper)
                        && nodecl_is_constant(nodecl_stride))
                {
                    // This range is constant
                    nodecl_set_constant(nodecl_indexes[num_subscripts],
                            const_value_make_range(nodecl_get_constant(nodecl_lower),
                                nodecl_get_constant(nodecl_upper),
                                nodecl_get_constant(nodecl_stride)));
                }
            }

        }
        else
        {
            fortran_check_expression_impl_(subscript, decl_context, &nodecl_indexes[num_subscripts]);
            nodecl_indexes[num_subscripts] = fortran_expression_as_value(nodecl_indexes[num_subscripts]);

            if (nodecl_is_err_expr(nodecl_indexes[num_subscripts]))
            {
                *nodecl_output = nodecl_indexes[num_subscripts];
                return;
            }

            type_t* t = nodecl_get_type(nodecl_indexes[num_subscripts]);

            type_t* rank_0 = fortran_get_rank0_type(t);

            if (!is_any_int_type(rank_0))
            {
                warn_printf_at(ast_get_locus(subscript), "subscript of array should be of type INTEGER\n");
            }

            if (is_pointer_type(no_ref(t)))
                t = pointer_type_get_pointee_type(no_ref(t));

            t = no_ref(t);

            if (!symbol_is_invalid)
            {
                if (fortran_is_array_type(t))
                {
                    // We do not really know the range here
                    synthesized_type = get_array_type_bounds(
                            synthesized_type,
                            nodecl_null(),
                            nodecl_null(),
                            array_type_get_array_size_expr_context(t));
                }
            }
        }
        num_subscripts++;
    }

    if (symbol_is_invalid)
    {
        error_printf_at(ast_get_locus(expr), "data reference '%s' does not designate an array name\n", fortran_prettyprint_in_buffer(ASTSon0(expr)));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (num_subscripts != rank_of_type)
    {
        error_printf_at(ast_get_locus(expr), "mismatch in subscripts of array reference, expecting %d got %d\n",
                fortran_get_rank_of_type(symbol->type_information),
                num_subscripts);
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    char all_subscripts_const = 1;
    nodecl_t nodecl_list = nodecl_null();
    for (i = num_subscripts-1; i >= 0; i--)
    {
        if (!nodecl_is_constant(nodecl_indexes[i]))
            all_subscripts_const = 0;
        nodecl_list = nodecl_append_to_list(nodecl_list, nodecl_indexes[i]);
    }

    type_t* data_type = synthesized_type;
    if (is_any_reference_type(nodecl_get_type(nodecl_subscripted)))
    {
        data_type = lvalue_ref(data_type);
    }

    *nodecl_output = nodecl_make_array_subscript(nodecl_subscripted, 
            nodecl_list,
            data_type,
            ast_get_locus(expr));

    if (nodecl_is_constant(nodecl_subscripted)
            && all_subscripts_const)
    {
        const_value_t* subconstant = compute_subconstant_of_array(
                nodecl_get_constant(nodecl_subscripted),
                array_type,
                nodecl_indexes,
                num_subscripts);

        nodecl_set_constant(*nodecl_output, subconstant);
    }
}

static void check_array_ref(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_subscripted = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_subscripted);

    if (nodecl_is_err_expr(nodecl_subscripted))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    type_t* subscripted_type = nodecl_get_type(nodecl_subscripted);

    // This ordering is important to preserve the proper meaning of the subscript
    // A(1:2) where 'A' is an array
    if (ASTKind(ASTSon0(expr)) != AST_ARRAY_SUBSCRIPT
            && (fortran_is_array_type(no_ref(subscripted_type))
                || fortran_is_pointer_to_array_type(no_ref(subscripted_type))))
    {
        if (is_any_reference_type(subscripted_type)
                && fortran_is_pointer_to_array_type(no_ref(subscripted_type)))
        {
            nodecl_subscripted = fortran_expression_as_variable(nodecl_subscripted);
        }
        check_array_ref_(expr, decl_context, nodecl_subscripted, nodecl_subscripted, nodecl_output,
                /* do_complete_array_ranks */ 1, /* require_lower_bound */ 0);
        return;
    }
    // C(1:2) where 'C' is a scalar CHARACTER
    else if (fortran_is_character_type(no_ref(subscripted_type))
            || fortran_is_pointer_to_character_type(no_ref(subscripted_type)))
    {
        if (is_any_reference_type(subscripted_type)
                && fortran_is_pointer_to_character_type(no_ref(subscripted_type)))
        {
            nodecl_subscripted = fortran_expression_as_variable(nodecl_subscripted);
        }
        check_substring(expr, decl_context, nodecl_subscripted, nodecl_output);
        return;
    }
    // A(1:2)(3:4) where 'A' is an array of CHARACTER and 'A(1:2)' yields an array type (i.e. an array-section)
    else if (
            ASTKind(ASTSon0(expr)) == AST_ARRAY_SUBSCRIPT
            && (// An array of CHARACTER
                (fortran_is_array_type(no_ref(subscripted_type))
                 && fortran_is_character_type(fortran_get_rank0_type(no_ref(subscripted_type))))
                // A pointer to array of CHARACTER
                || (fortran_is_pointer_to_array_type(no_ref(subscripted_type))
                    && fortran_is_pointer_to_character_type(fortran_get_rank0_type(no_ref(subscripted_type))))))
    {
        check_substring(expr, decl_context, nodecl_subscripted, nodecl_output);
        return;
    }

    error_printf_at(ast_get_locus(expr), "invalid entity '%s' for subscript expression\n",
            fortran_prettyprint_in_buffer(ASTSon0(expr)));
    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
}

static char in_string_set(char c, const char* char_set)
{
    int i;
    int len = strlen(char_set);
    for (i = 0; i < len; i++)
    {
        if (tolower(c) == tolower(char_set[i]))
            return 1;
    }

    return 0;
}

static unsigned int get_kind_of_unsigned_value(unsigned long long v)
{
    const unsigned long long b[] = {0xFFUL, 0xFFFFULL, 0xFFFFFFFFULL, 0xFFFFFFFFFFFFFFFFULL }; 
    const unsigned long long S[] = {1ULL,   2ULL,      4ULL,          8ULL };

    int MAX = (sizeof(S) / sizeof(S[0]));

    int i;
    for (i = 0; i < MAX; i++)
    {
        if (v <= b[i])
        {
            return S[i];
        } 
    }

    return S[MAX-1];
}

static void compute_boz_literal(AST expr, const char *valid_prefix, int base, nodecl_t* nodecl_output)
{
    const char* literal_token = ASTText(expr);

    char literal_text[strlen(literal_token) + 1];
    memset(literal_text, 0, sizeof(literal_text));

    char *q = literal_text;

    char had_prefix = 0;
    if (in_string_set(*literal_token, valid_prefix))
    {
        literal_token++;
        had_prefix = 1;
    }

    ERROR_CONDITION(*literal_token != '\''
            && *literal_token != '\"', "Invalid expr token '%s'!", literal_token);

    const char delim = *literal_token;

    // Jump delimiter
    literal_token++;

    while (*literal_token != delim)
    {
        *q = *literal_token;
        literal_token++;
        q++;
    }

    if (!had_prefix)
    {
        literal_token++;
        if (!in_string_set(*literal_token, valid_prefix))
        {
            ERROR_CONDITION(*literal_token != '\''
                    && *literal_token != '\"', "Invalid expr token!", 0);
        }
    }


    unsigned long long int value = strtoull(literal_text, NULL, base);

    unsigned int kind_size = get_kind_of_unsigned_value(value);

    // We need a nodecl just for diagnostic purposes
    nodecl_t loc = nodecl_make_text(ASTText(expr), ast_get_locus(expr));
    type_t* integer_type = choose_int_type_from_kind(loc, kind_size);
    nodecl_free(loc);

    const_value_t* const_value = const_value_get_integer(value, kind_size, /* signed */ 1);

    *nodecl_output = nodecl_make_fortran_boz_literal(
            integer_type, ASTText(expr), const_value,
            ast_get_locus(expr));
}


static void check_binary_literal(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    compute_boz_literal(expr, "b", 2, nodecl_output);
}

static char kind_is_integer_literal(const char* c)
{
    while (*c != '\0')
    {
        if (!isdigit(*c))
            return 0;
        c++;
    }
    return 1;
}

static int compute_kind_from_literal(const char* p, AST expr, const decl_context_t* decl_context)
{
    if (kind_is_integer_literal(p))
    {
        return atoi(p);
    }
    else
    {
        scope_entry_t* sym = fortran_get_variable_with_locus(decl_context, expr, p);
        if (sym == NULL
                || sym->kind != SK_VARIABLE
                || !is_const_qualified_type(no_ref(sym->type_information)))
        {
            fprintf(stderr, "%s: invalid kind '%s'\n", 
                    ast_location(expr), 
                    p);
            return 0;
        }

        ERROR_CONDITION(nodecl_is_null(sym->value),
                "Invalid constant for kind '%s'", sym->symbol_name);

        ERROR_CONDITION(!nodecl_is_constant(sym->value),
                "Invalid nonconstant expression for kind '%s'", 
                codegen_to_str(sym->value, nodecl_retrieve_context(sym->value)));

        return const_value_cast_to_4(nodecl_get_constant(sym->value));
    }
}

static void check_boolean_literal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    const_value_t* const_value = NULL;

    char* literal = xstrdup(ASTText(expr));

    type_t* logical_type = fortran_get_default_logical_type();
    int kind = fortran_get_default_logical_type_kind();

    char* kind_str = strchr(literal, '_');
    if (kind_str != NULL)
    {
       *kind_str = '\0';
       kind_str++;

       kind = compute_kind_from_literal(kind_str, expr, decl_context);
       if (kind == 0)
       {
           *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
           return;
       }
       nodecl_t nodecl_fake = nodecl_make_text(literal, ast_get_locus(expr));
       type_t* int_type = choose_int_type_from_kind(nodecl_fake, kind);
       logical_type = get_bool_of_integer_type(int_type);
    }

    if (strcasecmp(literal, ".true.") == 0)
    {
        const_value = const_value_get_one(kind, 1);
    }
    else if (strcasecmp(literal, ".false.") == 0)
    {
        const_value = const_value_get_zero(kind, 1);
    }
    else
    {
        internal_error("Invalid boolean literal", 0);
    }

    DELETE(literal);

    *nodecl_output = nodecl_make_boolean_literal(
            logical_type, 
            const_value, 
            ast_get_locus(expr));
}

static void check_complex_literal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST real_part = ASTSon0(expr);
    AST imag_part = ASTSon1(expr);

    nodecl_t nodecl_real = nodecl_null();
    fortran_check_expression_impl_(real_part, decl_context, &nodecl_real);
    nodecl_real = fortran_expression_as_value(nodecl_real);

    if (nodecl_is_err_expr(nodecl_real))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    nodecl_t nodecl_imag = nodecl_null();
    fortran_check_expression_impl_(imag_part, decl_context, &nodecl_imag);
    nodecl_imag = fortran_expression_as_value(nodecl_imag);

    if (nodecl_is_err_expr(nodecl_imag))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    type_t* real_part_type = no_ref(nodecl_get_type(nodecl_real));
    type_t* imag_part_type = no_ref(nodecl_get_type(nodecl_imag));

    char is_integer_complex = 0;

    type_t* element_type = NULL;
    type_t* result_type = NULL;
    if (is_integer_type(real_part_type)
            && is_integer_type(imag_part_type))
    {
        is_integer_complex = 1;
        result_type = get_complex_type(fortran_get_default_real_type());
    }
    else if (is_floating_type(real_part_type)
            || is_floating_type(imag_part_type))
    {
        if (is_floating_type(real_part_type))
        {
            element_type = real_part_type;
        }

        if (is_floating_type(imag_part_type))
        {
            if (element_type == NULL)
            {
                element_type = imag_part_type;
            }
            else
            {
                // We will choose the bigger one (note that element_type here
                // is already real_part_type, no need to check that case)
                if (type_get_size(imag_part_type) > type_get_size(real_part_type))
                {
                    element_type = imag_part_type;
                }
            }
        }

        result_type = get_complex_type(element_type);
    }
    else
    {
        error_printf_at(ast_get_locus(expr), "invalid complex constant '%s'\n",
                fortran_prettyprint_in_buffer(expr));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (!nodecl_is_constant(nodecl_real))
    {
        error_printf_at(ast_get_locus(real_part), "real part '%s' of complex constant is not constant\n",
            fortran_prettyprint_in_buffer(real_part));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }
    if (!nodecl_is_constant(nodecl_imag))
    {
        error_printf_at(ast_get_locus(imag_part), "imaginary part '%s' of complex constant is not constant\n",
            fortran_prettyprint_in_buffer(imag_part));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    const_value_t* cval_real_part = nodecl_get_constant(nodecl_real);
    const_value_t* cval_imag_part = nodecl_get_constant(nodecl_imag);

    if (is_integer_complex)
    {
        if (equivalent_types(fortran_get_default_real_type(), get_float_type()))
        {
            cval_real_part = const_value_cast_to_float_value(cval_real_part);
            cval_imag_part = const_value_cast_to_float_value(cval_imag_part);
        }
        else if (equivalent_types(fortran_get_default_real_type(), get_double_type()))
        {
            cval_real_part = const_value_cast_to_double_value(cval_real_part);
            cval_imag_part = const_value_cast_to_double_value(cval_imag_part);
        }
        else if (equivalent_types(fortran_get_default_real_type(), get_long_double_type()))
        {
            cval_real_part = const_value_cast_to_long_double_value(cval_real_part);
            cval_imag_part = const_value_cast_to_long_double_value(cval_imag_part);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }
    else
    {
        if (equivalent_types(get_unqualified_type(element_type), get_float_type()))
        {
            cval_real_part = const_value_cast_to_float_value(cval_real_part);
            cval_imag_part = const_value_cast_to_float_value(cval_imag_part);
        }
        else if (equivalent_types(get_unqualified_type(element_type), get_double_type()))
        {
            cval_real_part = const_value_cast_to_double_value(cval_real_part);
            cval_imag_part = const_value_cast_to_double_value(cval_imag_part);
        }
        else if (equivalent_types(get_unqualified_type(element_type), get_long_double_type()))
        {
            cval_real_part = const_value_cast_to_long_double_value(cval_real_part);
            cval_imag_part = const_value_cast_to_long_double_value(cval_imag_part);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
    }

    const_value_t* complex_constant = const_value_make_complex(
            cval_real_part,
            cval_imag_part);

    *nodecl_output = nodecl_make_complex_literal(
            result_type,
            complex_constant,
            ast_get_locus(expr));
}

static void check_component_ref_(AST expr,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output,
        char do_complete_array_ranks,
        char require_lower_bound)
{
    // Left hand side first
    nodecl_t nodecl_lhs = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_lhs);

    if (nodecl_is_err_expr(nodecl_lhs))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (is_any_reference_type(nodecl_get_type(nodecl_lhs)))
    {
        nodecl_lhs = fortran_expression_as_variable(nodecl_lhs);
    }

    // The type of the lhs_type (may be a class type or array of class type)
    type_t* lhs_type = no_ref(nodecl_get_type(nodecl_lhs));

    ERROR_CONDITION(is_pointer_type(lhs_type), "Invalid type", 0);

    type_t* class_type = fortran_get_rank0_type(lhs_type);
    if (!is_class_type(class_type))
    {
        error_printf_at(ast_get_locus(expr), "'%s' does not denote a derived type\n",
                fortran_prettyprint_in_buffer(ASTSon0(expr)));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    ERROR_CONDITION(is_pointer_to_class_type(class_type), "Invalid type", 0);

    const decl_context_t* class_context = class_type_get_inner_context(get_actual_class_type(class_type));

    // Right hand side
    AST rhs = ASTSon1(expr);
    AST name = rhs;

    switch (ASTKind(name))
    {
        case AST_SYMBOL:
            {
                // Do nothing
                break;
            }
        case AST_ARRAY_SUBSCRIPT:
            {
                name = ASTSon0(name);
                break;
            }
        default:
            {
                internal_error("Unexpected tree '%s' at right hand side of '%%'\n", ast_print_node_type(ASTKind(name)));
            }
    }

    const char* field = ASTText(name);
    scope_entry_t* component_symbol = query_name_in_class(class_context, field, ast_get_locus(name));

    if (component_symbol == NULL)
    {
        error_printf_at(ast_get_locus(expr), "'%s' is not a component of '%s'\n",
                field,
                fortran_print_type_str(class_type));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    type_t* rhs_type = component_symbol->type_information;

    nodecl_t nodecl_rhs = nodecl_make_symbol(component_symbol, ast_get_locus(name));
    type_t* component_type = no_ref(component_symbol->type_information);

    if (ASTKind(rhs) == AST_ARRAY_SUBSCRIPT
            && !fortran_is_array_type(component_type)
            && !fortran_is_pointer_to_array_type(component_type)
            && !fortran_is_character_type(no_ref(component_type))
            && !fortran_is_pointer_to_character_type(no_ref(component_type)))
    {
        error_printf_at(ast_get_locus(rhs), "component '%s' of '%s' is not an array or character\n",
                component_symbol->symbol_name,
                fortran_print_type_str(class_type));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    nodecl_set_type(nodecl_rhs, component_type);

    type_t* synthesized_type = NULL;
    if (fortran_is_array_type(lhs_type))
    {
        if (is_pointer_type(lhs_type))
        {
            error_printf_at(ast_get_locus(expr), "nonzero rank data-reference has a component of pointer type\n");
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }

        synthesized_type = fortran_rebuild_array_type(
                rhs_type,
                lhs_type);
    }
    else if (fortran_is_array_type(rhs_type))
    {
        synthesized_type = rhs_type;
    }
    else
    {
        synthesized_type = rhs_type;
    }

    if (is_lvalue_reference_type(nodecl_get_type(nodecl_lhs)))
    {
        synthesized_type = get_lvalue_reference_type(synthesized_type);
    }

    const_value_t* const_value = NULL;
    if (nodecl_is_constant(nodecl_lhs))
    {
        // The base is const, thus this component reference is const as well
        const_value = nodecl_get_constant(nodecl_lhs);
        ERROR_CONDITION(!const_value_is_structured(const_value), "Invalid constant value for data-reference of part", 0);

        // First figure the index inside the const value
        int i = 0;
        scope_entry_list_t* components = class_type_get_nonstatic_data_members(class_type);

        scope_entry_list_iterator_t* iter = NULL;
        for (iter = entry_list_iterator_begin(components);
                !entry_list_iterator_end(iter);
                entry_list_iterator_next(iter), i++)
        {
            scope_entry_t* current_member = entry_list_iterator_current(iter);
            if (current_member == component_symbol)
            {
                break;
            }
        }
        entry_list_iterator_free(iter);

        ERROR_CONDITION((i == entry_list_size(components)), "This should not happen", 0);

        const_value = const_value_get_element_num(const_value, i);
    }

    *nodecl_output =
        nodecl_make_class_member_access(
                nodecl_lhs,
                nodecl_rhs,
                /* member form */ nodecl_null(),
                synthesized_type,
                ast_get_locus(expr));
    nodecl_set_constant(*nodecl_output, const_value);

#if 0
    if (is_pointer_type(component_type))
    {
        *nodecl_output =
            nodecl_make_dereference(
                    *nodecl_output,
                    lvalue_ref(pointer_type_get_pointee_type(component_type)),
                    ast_get_locus(expr));
    }
#endif

    if (ASTKind(rhs) == AST_ARRAY_SUBSCRIPT)
    {
        if (fortran_is_array_type(component_type)
                || fortran_is_pointer_to_array_type(component_type))
        {
            if (is_any_reference_type(synthesized_type)
                    && fortran_is_pointer_to_array_type(no_ref(synthesized_type)))
            {
                *nodecl_output = fortran_expression_as_variable(*nodecl_output);
            }
            check_array_ref_(rhs, decl_context, *nodecl_output, *nodecl_output, nodecl_output,
                    do_complete_array_ranks, require_lower_bound);

            if (nodecl_is_err_expr(*nodecl_output))
                return;

            if (fortran_is_array_type(lhs_type)
                    && fortran_is_array_type(no_ref(nodecl_get_type(*nodecl_output))))
            {
                error_printf_at(ast_get_locus(expr), "nonzero rank data-reference has a component of nonzero rank\n");
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return;
            }

            synthesized_type = fortran_rebuild_array_type(
                    no_ref(nodecl_get_type(*nodecl_output)),
                    lhs_type);

            if (is_lvalue_reference_type(nodecl_get_type(nodecl_lhs)))
            {
                synthesized_type = get_lvalue_reference_type(synthesized_type);
            }

            nodecl_set_type(*nodecl_output, synthesized_type);
        }
        else if (fortran_is_character_type(no_ref(component_type))
                || fortran_is_pointer_to_character_type(no_ref(component_type)))
        {
            if (is_any_reference_type(synthesized_type)
                    && fortran_is_pointer_to_character_type(no_ref(synthesized_type)))
            {
                *nodecl_output = fortran_expression_as_variable(*nodecl_output);
            }
            check_substring(rhs, decl_context, *nodecl_output, nodecl_output);
        }
        else
        {
            internal_error("code unreachable", 0);
        }
    }
}

static void check_component_ref(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    check_component_ref_(expr, decl_context, nodecl_output,
            /* do_complete_array_ranks */ 1, /* require_lower_bound */ 0);
}

static void check_concat_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_decimal_literal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    const char* c = ASTText(expr);

    char decimal_text[strlen(c) + 1];
    memset(decimal_text, 0, sizeof(decimal_text));

    char *q = decimal_text;
    const char* p = c;

    while (*p != '\0'
            && *p != '_')
    {
        *q = *p;
        p++;
        q++;
    }

    int kind = fortran_get_default_integer_type_kind();
    if (*p == '_')
    {
        p++;
        kind = compute_kind_from_literal(p, expr, decl_context);
        if (kind == 0)
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }

    long long int value = strtoll(decimal_text, NULL, 10);

    const_value_t* const_value = const_value_get_integer(value, kind, 1);
    nodecl_t nodecl_fake = nodecl_make_text(decimal_text, ast_get_locus(expr));
    type_t* t = choose_int_type_from_kind(nodecl_fake, 
            kind);

    *nodecl_output = nodecl_make_integer_literal(t, const_value, 
            ast_get_locus(expr));
}

static void check_derived_type_constructor(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST derived_type_spec = ASTSon0(expr);
    AST component_spec_list = ASTSon1(expr);

    AST type_param_spec_list = ASTSon1(derived_type_spec);
    if (type_param_spec_list != NULL)
    {
        error_printf_at(ast_get_locus(expr), "sorry: derived types with type parameters not supported\n");
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    AST derived_name = ASTSon0(derived_type_spec);
    scope_entry_t* entry = fortran_get_variable_with_locus(decl_context, derived_name, ASTText(derived_name));

    if (entry == NULL
            || entry->kind != SK_CLASS)
    {
        error_printf_at(ast_get_locus(expr), "'%s' is not a derived-type-name\n",
                strtolower(ASTText(derived_name)));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    char all_components_are_const = 1;
    char all_components_are_const_type = 1;

    scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(entry->type_information);

    nodecl_t initialization_expressions[entry_list_size(nonstatic_data_members) + 1];
    memset(initialization_expressions, 0, sizeof(initialization_expressions));

    int member_index = 0;
    int component_position = 1;
    if (component_spec_list != NULL)
    {
        AST it;
        for_each_element(component_spec_list, it)
        {
            AST component_spec = ASTSon1(it);
            AST component_name = ASTSon0(component_spec);
            AST component_data_source = ASTSon1(component_spec);

            int current_member_index = 0;

            scope_entry_t* member = NULL;
            if (component_name == NULL)
            {
                if (member_index < 0)
                {
                    error_printf_at(ast_get_locus(component_spec), "component specifier at position %d lacks a component name\n",
                            component_position);
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }

                if (member_index > entry_list_size(nonstatic_data_members))
                {
                    error_printf_at(ast_get_locus(component_spec), "too many specifiers in derived-type constructor\n");
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }

                scope_entry_list_iterator_t* iter = entry_list_iterator_begin(nonstatic_data_members);
                int i;
                for (i = 0; i < member_index; i++)
                {
                    entry_list_iterator_next(iter);
                }
                member = entry_list_iterator_current(iter);

                entry_list_iterator_free(iter);

                current_member_index = member_index;

                member_index++;
            }
            else
            {
                const decl_context_t* class_context = class_type_get_inner_context(get_actual_class_type(entry->type_information));

                const char* field = ASTText(component_name);
                member = query_name_in_class(class_context, field, ast_get_locus(component_name));
                if (member == NULL)
                {
                    error_printf_at(ast_get_locus(expr), "component specifier '%s' is not a component of '%s'\n",
                            field,
                            fortran_print_type_str(entry->type_information));
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }

                current_member_index = 0;
                scope_entry_list_iterator_t* iter = NULL;
                for (iter = entry_list_iterator_begin(nonstatic_data_members);
                        !entry_list_iterator_end(iter);
                        entry_list_iterator_next(iter), current_member_index++)
                {
                    scope_entry_t* current_member = entry_list_iterator_current(iter);
                    if (current_member == member)
                        break;
                }

                ERROR_CONDITION((current_member_index == entry_list_size(nonstatic_data_members)), "This should never happen", 0);

                member_index = -1;
            }

            if (!nodecl_is_null(initialization_expressions[current_member_index]))
            {
                error_printf_at(ast_get_locus(expr), "component '%s' initialized more than once\n",
                        member->symbol_name);
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return;
            }

            nodecl_t nodecl_expr = nodecl_null();
            fortran_check_expression_impl_(component_data_source, decl_context, &nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
            {
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return;
            }

            if (!nodecl_is_constant(nodecl_expr))
            {
                all_components_are_const = 0;
            }

            initialization_expressions[current_member_index] = nodecl_expr;

            component_position++;
        }
    }

    nodecl_t nodecl_initializer_list = nodecl_null();
    scope_entry_list_iterator_t* iter = NULL;

    // Now review components not initialized yet
    int i = 0;
    for (iter = entry_list_iterator_begin(nonstatic_data_members);
            !entry_list_iterator_end(iter);
            entry_list_iterator_next(iter), i++)
    {
        scope_entry_t* member = entry_list_iterator_current(iter);

        if (nodecl_is_null(initialization_expressions[i]))
        {
            if (nodecl_is_null(member->value))
            {
                error_printf_at(ast_get_locus(expr), "component '%s' lacks an initializer\n",
                        member->symbol_name);
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return;
            }
            else
            {
                // This should be const, shouldn't it?
                if (!nodecl_is_constant(member->value))
                {
                    all_components_are_const = 0;
                }

                initialization_expressions[i] = nodecl_shallow_copy(member->value);
            }
        }

        nodecl_initializer_list = nodecl_append_to_list(nodecl_initializer_list, 
                nodecl_make_field_designator(
                    nodecl_make_symbol(member, ast_get_locus(expr)),
                    initialization_expressions[i],
                    no_ref(member->type_information),
                    ast_get_locus(expr)));
    }
    entry_list_iterator_free(iter);

    *nodecl_output = nodecl_make_structured_value(nodecl_initializer_list, 
            nodecl_null(),
            get_user_defined_type(entry), 
            ast_get_locus(expr));

    if (all_components_are_const)
    {
        const_value_t* items[nodecl_list_length(nodecl_initializer_list) + 1];
        memset(items, 0, sizeof(items));

        int num_items = entry_list_size(nonstatic_data_members);

        for (i = 0; i < num_items; i++)
        {
            items[i] = nodecl_get_constant(initialization_expressions[i]);
        }

        const_value_t* value = const_value_make_struct(num_items, items, get_user_defined_type(entry));
        nodecl_set_constant(*nodecl_output, value);
    }
    else if (all_components_are_const_type)
    {
        nodecl_set_type(*nodecl_output, get_const_qualified_type(nodecl_get_type(*nodecl_output)));
    }

    entry_list_free(nonstatic_data_members);
}

static void check_different_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_div_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_equal_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

#define check_range_of_floating(expr, text, value, kind, isfun) \
    do { \
        if (value == 0 && errno == ERANGE) \
        { \
            error_printf_at(ast_get_locus(expr), "value '%s' underflows REAL(KIND=%d)\n", \
                    text, kind); \
            value = 0.0; \
        } \
        else if (isfun(value)) \
        { \
            error_printf_at(ast_get_locus(expr), "value '%s' overflows REAL(KIND=%d)\n", \
                    text, kind); \
            value = 0.0; \
        } \
    } while (0) 

static void check_floating_literal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
   char* floating_text = xstrdup(strtolower(ASTText(expr)));

   unsigned int kind = fortran_get_default_real_type_kind();
   char *q = NULL; 
   if ((q = strchr(floating_text, '_')) != NULL)
   {
       *q = '\0';
       q++;
       kind = compute_kind_from_literal(q, expr, decl_context);
       if (kind == 0)
       {
           *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
           return;
       }
   }
   else if ((q = strchr(floating_text, 'd')) != NULL)
   {
       *q = 'e';
       kind = fortran_get_doubleprecision_type_kind();
   }

   nodecl_t nodecl_fake = nodecl_make_text(floating_text, ast_get_locus(expr));
   type_t* t = choose_float_type_from_kind(nodecl_fake, kind);

   const_value_t *value = NULL;
   if (kind == (floating_type_get_info(get_float_type())->bits / 8))
   {
       errno = 0;
       float f = strtof(floating_text, NULL);
       check_range_of_floating(expr, floating_text, f, kind, isinf);

       value = const_value_get_float(f);
   }
   else if (kind == (floating_type_get_info(get_double_type())->bits / 8))
   {
       errno = 0;
       double d = strtod(floating_text, NULL);
       check_range_of_floating(expr, floating_text, d, kind, isinf);

       value = const_value_get_double(d);
   }
   else if (kind == (floating_type_get_info(get_long_double_type())->bits / 8))
   {
       errno = 0;
       long double ld = strtold(floating_text, NULL);
       check_range_of_floating(expr, floating_text, ld, kind, isinf);

       value = const_value_get_long_double(ld);
   }
   else if (is_other_float_type(t))
   {
#ifdef HAVE_QUADMATH_H
       // __float128
       if (kind == 16)
       {
           errno = 0;
           __float128 f128 = strtoflt128(floating_text, NULL);
           check_range_of_floating(expr, floating_text, f128, kind, isinfq);

           value = const_value_get_float128(f128);
       }
       else
#endif
       {
           fatal_printf_at(ast_get_locus(expr), "literals of KIND=%d not supported\n", kind);
       }
   }
   else
   {
       fatal_error("Code unreachable, invalid floating literal");
   }

   *nodecl_output = nodecl_make_floating_literal(t, value, ast_get_locus(expr));

   DELETE(floating_text);
}

static char is_assumed_shape_or_pointer_array(scope_entry_t* entry)
{
    return (entry != NULL
            && (fortran_is_pointer_to_array_type(no_ref(entry->type_information))
                || (fortran_is_array_type(no_ref(entry->type_information))
                    && array_type_with_descriptor(no_ref(entry->type_information))
                    // allocatable arrays have descriptors but are not assumed shape
                    && !symbol_entity_specs_get_is_allocatable(entry))));
}

static char check_argument_association(
        scope_entry_t* function,
        type_t* formal_type,
        type_t* real_type,
        nodecl_t real_argument,

        char ranks_must_agree,

        char diagnostic,
        int argument_num,
        const locus_t* locus)
{
    formal_type = no_ref(formal_type);
    real_type = no_ref(real_type);

    if (is_pointer_type(formal_type)
            && is_call_to_null(real_argument, NULL))
    {
        // NULL() is OK with any pointer
        return 1;
    }

    // If the actual argument is not a pointer type,
    // the pointer type itself is not relevant anymore
    if (is_pointer_type(real_type)
            && !is_pointer_type(formal_type))
    {
        real_type = pointer_type_get_pointee_type(real_type);
    }

    if (is_function_type(formal_type)
            && is_function_type(real_type))
    {
        scope_entry_t* entry = nodecl_get_symbol(real_argument);

        if (entry != NULL
                && symbol_entity_specs_get_is_implicit_basic_type(entry))
            // We cannot reliably check this case
            return 1;

        scope_entry_t* dummy_argument =
            symbol_entity_specs_get_related_symbols_num(function, argument_num);

        if (dummy_argument != NULL
                && symbol_entity_specs_get_is_implicit_basic_type(dummy_argument))
            // We cannot reliably check this case
            return 1;
    }

    if (!fortran_equivalent_tk_types(formal_type, real_type))
    {
        if (diagnostic)
        {
            error_printf_at(locus, "type or kind '%s' of actual argument %d does not agree type or kind '%s' of dummy argument\n",
                    fortran_print_type_str(real_type),
                    argument_num + 1,
                    fortran_print_type_str(formal_type));
        }
        return 0;
    }

    if (// If both types are pointers or ...
            ((is_pointer_type(formal_type)
              && is_pointer_type(real_type))
             // ... the dummy argument is an array requiring descriptor ...
             || (fortran_is_array_type(formal_type)
                 && array_type_with_descriptor(formal_type))
             // Or we explicitly need ranks to agree
             || ranks_must_agree
             )
            // then their ranks should match
            && fortran_get_rank_of_type(formal_type) != fortran_get_rank_of_type(real_type))
    {
        if (diagnostic)
        {
            error_printf_at(locus, "rank %d of actual argument %d does not agree rank %d of dummy argument\n",
                    fortran_get_rank_of_type(real_type),
                    argument_num + 1,
                    fortran_get_rank_of_type(formal_type));
        }
        return 0;
    }

    // If the actual argument is a scalar, ...
    if (!fortran_is_array_type(real_type))
    {
        char is_assumed_or_pointer = 0;
        // ... the dummy argument should be a scalar ...
        if (fortran_is_array_type(formal_type))
        {
            char ok = 0;
            // (Fortran 2003) unless the actual argument is a character...
            if (fortran_is_character_type(real_type))
            {
                // ... of default kind
                if (equivalent_types(get_unqualified_type(array_type_get_element_type(real_type)),
                            fortran_get_default_character_type()))
                {
                    ok = 1;
                    // ... and the character is not a subscring is not an assumed shape or pointer to array
                    scope_entry_t* array = fortran_data_ref_get_symbol(real_argument);
                    if (is_assumed_shape_or_pointer_array(array))
                    {
                        is_assumed_or_pointer = 1;
                        ok = 0;
                    }
                }

            }
            // ... or the actual argument is an element of an array ...
            else if (nodecl_get_kind(real_argument) == NODECL_ARRAY_SUBSCRIPT)
            {
                ok = 1;

                // ... and the array is not an assumed shape or pointer to array
                scope_entry_t* array = fortran_data_ref_get_symbol(real_argument);
                if (is_assumed_shape_or_pointer_array(array))
                {
                    is_assumed_or_pointer = 1;
                    ok = 0;
                }
            }

            if (!ok)
            {
                if (diagnostic)
                {
                    error_printf_at(locus, "scalar type '%s' of actual argument %d cannot "
                            "be associated to non-scalar type '%s' of dummy argument%s\n",
                            fortran_print_type_str(real_type),
                            argument_num + 1,
                            fortran_print_type_str(formal_type),
                            is_assumed_or_pointer ?
                                " because it would associate a pointer or assumed array"
                                : "");
                }
                return 0;
            }
        }
    }

    // Everything looks fine here
    return 1;
}

typedef
struct actual_argument_info_tag
{
    const char* keyword;
    type_t* type;
    char not_present;
    nodecl_t argument;
} actual_argument_info_t;

static scope_entry_list_t* get_specific_interface_aux(scope_entry_t* symbol, 
        int num_arguments, 
        nodecl_t* nodecl_actual_arguments,
        char ignore_elementals)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Getting specific interface of '%s' called with the following argument types\n",
                symbol->symbol_name);

        int i;
        for (i = 0; i < num_arguments; i++)
        {
            fprintf(stderr, "EXPRTYPE:    Name: %s\n",
                    nodecl_get_text(nodecl_actual_arguments[i]) != NULL ? nodecl_get_text(nodecl_actual_arguments[i]) : "<<no-name>>");
            fprintf(stderr, "EXPRTYPE:    Argument: %s\n",
                    fortran_print_type_str(nodecl_get_type(nodecl_get_child(nodecl_actual_arguments[i], 0))));
        }
    }

    scope_entry_list_t* result = NULL;
    int k;
    for (k = 0; k < symbol_entity_specs_get_num_related_symbols(symbol); k++)
    {
        scope_entry_t* specific_symbol = symbol_entity_specs_get_related_symbols_num(symbol, k);

        if (symbol_entity_specs_get_is_elemental(specific_symbol)
                && ignore_elementals)
            continue;

        char ok = 1;

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
        memset(argument_types, 0, sizeof(argument_types));

        int num_parameters = function_type_get_num_parameters(specific_symbol->type_information);

        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Checking with specific interface %s\n",
                    locus_to_str(specific_symbol->locus));
            int i;
            for (i = 0; (i < num_parameters) && ok; i++)
            {
                type_t* formal_type = no_ref(function_type_get_parameter_type_num(specific_symbol->type_information, i));

                fprintf(stderr, "EXPRTYPE:    %sName: %s\n", 
                        (symbol_entity_specs_get_is_optional(
                                 symbol_entity_specs_get_related_symbols_num(specific_symbol, i))
                         && !symbol_entity_specs_get_is_stmt_function(specific_symbol)) ? "Optional " : "",
                        symbol_entity_specs_get_related_symbols_num(specific_symbol, i) != NULL ? 
                        symbol_entity_specs_get_related_symbols_num(specific_symbol, i)->symbol_name : 
                        "<<no-name>>");
                fprintf(stderr, "EXPRTYPE:    Parameter: %s\n", 
                        fortran_print_type_str(formal_type));
            }
        }

        int current_num_arguments = num_arguments;

        // This is not possible with generic specifiers
        if (current_num_arguments > num_parameters)
            ok = 0;

        int i;
        for (i = 0; (i < current_num_arguments) && ok; i++)
        {
            int position = -1;
            if (nodecl_get_text(nodecl_actual_arguments[i]) == NULL)
            {
                position = i;
            }
            else
            {
                int j;
                for (j = 0; j < symbol_entity_specs_get_num_related_symbols(specific_symbol); j++)
                {
                    scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(specific_symbol, j);

                    if (!symbol_is_parameter_of_function(related_sym, specific_symbol))
                        continue;

                    if (strcasecmp(related_sym->symbol_name,
                                nodecl_get_text(nodecl_actual_arguments[i])) == 0)
                    {
                        position = j;
                    }
                }
                if (position < 0)
                {
                    ok = 0;
                    break;
                }
            }
            if (argument_types[position].type != NULL)
            {
                ok = 0;
                break;
            }
            argument_types[position].type = nodecl_get_type(nodecl_get_child(nodecl_actual_arguments[i], 0));
            argument_types[position].argument = nodecl_get_child(nodecl_actual_arguments[i], 0);
        }

        if (ok)
        {
            // Now complete with the optional ones
            for (i = 0; (i < symbol_entity_specs_get_num_related_symbols(specific_symbol)) && ok; i++)
            {
                scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(specific_symbol, i);
                ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, specific_symbol),
                        "Related symbol must be a parameter of the function", 0 );

                if (argument_types[i].type == NULL)
                {
                    if (symbol_entity_specs_get_is_optional(related_sym)
                            && !symbol_entity_specs_get_is_stmt_function(specific_symbol))
                    {
                        argument_types[i].type = related_sym->type_information;
                        argument_types[i].not_present = 1;
                        current_num_arguments++;
                    }
                    else
                    {
                        ok = 0;
                        break;
                    }
                }
            }
        }

        if (ok)
        {
            if (current_num_arguments != function_type_get_num_parameters(specific_symbol->type_information))
                ok = 0;
        }

        if (ok)
        {
            // Now check that every type matches, otherwise error
            for (i = 0; (i < current_num_arguments) && ok; i++)
            {
                scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(specific_symbol, i);
                ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, specific_symbol),
                        "Related symbol must be a parameter of the function", 0 );

                type_t* formal_type = no_ref(function_type_get_parameter_type_num(specific_symbol->type_information, i));
                type_t* real_type = no_ref(argument_types[i].type);

                // Note that for ELEMENTAL some more checks should be done
                if (symbol_entity_specs_get_is_elemental(specific_symbol)) 
                {
                    real_type = fortran_get_rank0_type(real_type);
                }

                if (is_pointer_type(formal_type)
                        && !argument_types[i].not_present)
                {
                    // If the actual argument is a pointer type and the dummy argument is a derreference,
                    // get the pointer type being derreferenced
                    if (!is_pointer_type(real_type))
                    {
                        ok = 0;
                        break;
                    }
                }

                if (symbol_entity_specs_get_is_allocatable(related_sym)
                        && !argument_types[i].not_present)
                {
                    scope_entry_t* current_arg_sym = NULL; 
                    if (nodecl_get_kind(argument_types[i].argument) == NODECL_SYMBOL)
                    {
                        current_arg_sym = nodecl_get_symbol(argument_types[i].argument);
                    }
                    else if (nodecl_get_kind(argument_types[i].argument) == NODECL_CLASS_MEMBER_ACCESS)
                    {
                        current_arg_sym = nodecl_get_symbol(
                                nodecl_get_child(argument_types[i].argument, 1)
                                );
                    }

                    if (current_arg_sym == NULL
                            || !symbol_entity_specs_get_is_allocatable(current_arg_sym))
                    {
                        ok = 0;
                        break;
                    }
                }

                if (!check_argument_association(
                            specific_symbol,
                            formal_type, 
                            real_type, 
                            argument_types[i].argument,

                            /* ranks_must_agree only if non-elemental */ !symbol_entity_specs_get_is_elemental(specific_symbol),

                            /* do_diagnostic */ 0,
                            /* argument_num */ i,
                            make_locus("", 0, 0)))
                {
                    ok = 0;
                    break;
                }
            }
        }

        if (ok)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Current specifier DOES match\n");
            }
            result = entry_list_add(result, specific_symbol);
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Current specifier does NOT match\n");
            }
        }
    }

    return result;
}

static scope_entry_list_t* get_specific_interface(scope_entry_t* symbol,
        int num_arguments,
        nodecl_t* nodecl_actual_arguments)
{
    scope_entry_list_t* exact_match_nonelemental
        = get_specific_interface_aux(symbol, num_arguments, nodecl_actual_arguments,
            /* ignore_elementals */ 1);

    if (exact_match_nonelemental != NULL)
        return exact_match_nonelemental;

    return get_specific_interface_aux(symbol, num_arguments, nodecl_actual_arguments,
            /* ignore_elementals */ 0);
}

static char inside_context_of_symbol(const decl_context_t* decl_context, scope_entry_t* entry)
{
    scope_t* sc = decl_context->current_scope;
    while (sc != NULL)
    {
        if (sc->related_entry == entry)
            return 1;
        sc = sc->contained_in;
    }
    return 0;
}


static void check_called_symbol_list(
        scope_entry_list_t* symbol_list,
        const decl_context_t* decl_context, 
        AST location,
        AST procedure_designator,
        int *num_actual_arguments,
        nodecl_t* nodecl_actual_arguments,
        char is_call_stmt,
        // out
        type_t** result_type, // Result type of this call
        scope_entry_t** called_symbol, // This is set to the real function called
        scope_entry_t** generic_specifier_symbol, // This will be non-NULL if the function call goes through a generic specifier name
        nodecl_t* nodecl_simplify)
{
    scope_entry_t* symbol = NULL;
    char intrinsic_name_can_be_called = 0;
    // char generic_name_can_be_called = 0;
    char hide_intrinsics = 0;

    int explicit_num_actual_arguments = *num_actual_arguments;

    // First solve the generic specifier
    if (entry_list_size(symbol_list) > 1
            || entry_list_head(symbol_list)->kind == SK_GENERIC_NAME)
    {
        scope_entry_list_t* specific_symbol_set = NULL;
        scope_entry_list_iterator_t* it = NULL;
        for (it = entry_list_iterator_begin(symbol_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_generic_spec = entry_list_iterator_current(it);

            if (symbol_entity_specs_get_is_builtin(current_generic_spec)
                    && is_computed_function_type(current_generic_spec->type_information))
            {
                scope_entry_t* specific_intrinsic = fortran_solve_generic_intrinsic_call(current_generic_spec,
                        nodecl_actual_arguments,
                        explicit_num_actual_arguments,
                        is_call_stmt);

                if (specific_intrinsic != NULL)
                {
                    intrinsic_name_can_be_called = 1;
                    specific_symbol_set = entry_list_merge(entry_list_new(specific_intrinsic),
                            specific_symbol_set);
                }
            }
            else if (current_generic_spec->kind == SK_GENERIC_NAME)
            {
                scope_entry_list_t* current_specific_symbol_set = get_specific_interface(current_generic_spec,
                        explicit_num_actual_arguments,
                        nodecl_actual_arguments);

                if (current_specific_symbol_set != NULL)
                {
                    // generic_name_can_be_called = 1;
                    // This may be overwritten when more than one generic specifier
                    // can match, which is wrong
                    *generic_specifier_symbol = current_generic_spec;

                    // If we find a USE consistent call to a associated name
                    // then INTRINSICS must be ignored to prioritize USEs
                    if (symbol_entity_specs_get_from_module(current_generic_spec) != NULL)
                    {
                        hide_intrinsics = 1;
                    }
                }

                specific_symbol_set = entry_list_merge(current_specific_symbol_set,
                        specific_symbol_set);
            }
            else
            {
                internal_error("Unexpected symbol '%s' when solving function reference at a '%s'",
                        current_generic_spec->symbol_name,
                        ast_location(location));
            }
        }

        entry_list_iterator_free(it);

        if (entry_list_size(specific_symbol_set) > 1)
        {
            scope_entry_list_t* filtered_specific_symbol_set = NULL;

            for (it = entry_list_iterator_begin(specific_symbol_set);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                if (hide_intrinsics && symbol_entity_specs_get_is_builtin(entry))
                    continue;

                filtered_specific_symbol_set = entry_list_add_once(filtered_specific_symbol_set, entry);
            }
            entry_list_iterator_free(it);

            entry_list_free(specific_symbol_set);
            specific_symbol_set = filtered_specific_symbol_set;
        }

        if (specific_symbol_set == NULL)
        {
            error_printf_at(ast_get_locus(location), "no specific interfaces%s match the generic interface '%s' in function reference\n",
                    intrinsic_name_can_be_called ? " or intrinsic procedures" : "",
                    fortran_prettyprint_in_buffer(procedure_designator));
            *result_type = get_error_type();
            return;
        }
        else if (entry_list_size(specific_symbol_set) > 1)
        {
            error_printf_at(ast_get_locus(location), "several specific interfaces%s match generic interface '%s' in function reference\n",
                    intrinsic_name_can_be_called ? " or intrinsic procedures" : "",
                    fortran_prettyprint_in_buffer(procedure_designator));
            for (it = entry_list_iterator_begin(specific_symbol_set);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* current_generic_spec = entry_list_iterator_current(it);
                if (current_generic_spec->kind == SK_GENERIC_NAME)
                {
                    info_printf_at(current_generic_spec->locus, "specific interface '%s' matches\n",
                            current_generic_spec->symbol_name);
                }
                else if (symbol_entity_specs_get_is_builtin(current_generic_spec))
                {
                    info_printf_at(current_generic_spec->locus, "intrinsic '%s' matches\n",
                            current_generic_spec->symbol_name);
                }
            }
            entry_list_iterator_free(it);

            *result_type = get_error_type();
            entry_list_free(specific_symbol_set);
            return;
        }
        else
        {
            symbol = entry_list_head(specific_symbol_set);
            entry_list_free(specific_symbol_set);
        }
    }
    else if (entry_list_size(symbol_list) == 1)
    {
        symbol = entry_list_head(symbol_list);
    }

    ERROR_CONDITION(symbol == NULL, "Symbol function not set", 0);

    if (!symbol_entity_specs_get_is_recursive(symbol)
            && inside_context_of_symbol(decl_context, symbol))
    {
        error_printf_at(ast_get_locus(location), "cannot recursively call '%s'\n",
                symbol->symbol_name);
    }

    type_t* return_type = NULL;
    // This is a generic procedure reference
    if (symbol_entity_specs_get_is_builtin(symbol)
            && is_computed_function_type(symbol->type_information))
    {
        scope_entry_t* entry = fortran_solve_generic_intrinsic_call(symbol,
                nodecl_actual_arguments,
                explicit_num_actual_arguments,
                is_call_stmt);

        if (entry == NULL)
        {
            error_printf_at(ast_get_locus(location), "call to intrinsic %s failed\n",
                    strtoupper(symbol->symbol_name));

            if (explicit_num_actual_arguments > 0)
            {
                info_printf_at(ast_get_locus(location), "actual arguments and their types follow\n");
                int i;
                for (i = 0; i < explicit_num_actual_arguments; i++)
                {
                    nodecl_t expr = nodecl_get_child(nodecl_actual_arguments[i], 0);
                    type_t* t = nodecl_get_type(expr);
                    info_printf_at(ast_get_locus(location), "'%s' of type %s\n",
                            codegen_to_str(nodecl_actual_arguments[i],
                                nodecl_retrieve_context(nodecl_actual_arguments[i])),
                            fortran_print_type_str(t));
                }
            }
            else
            {
                info_printf_at(ast_get_locus(location), "no arguments passed\n");
            }
            *result_type = get_error_type();
            return;
        }

        if (!nodecl_is_null(*nodecl_simplify))
        {
            return_type = nodecl_get_type(*nodecl_simplify);
        }
        else if (symbol_entity_specs_get_is_elemental(entry))
        {
            // Try to come up with a common_rank
            int common_rank = -1;
            int i;
            for (i = 0; i < explicit_num_actual_arguments; i++)
            {
                nodecl_t expr = nodecl_get_child(nodecl_actual_arguments[i], 0);
                type_t* t = nodecl_get_type(expr);
                int current_rank = fortran_get_rank_of_type(t);
                if (common_rank <= 0)
                {
                    common_rank = current_rank;
                }
                else if (current_rank != common_rank
                        && current_rank != 0)
                {
                    common_rank = -1;
                    break;
                }
            }

            if (common_rank == 0)
            {
                return_type = function_type_get_return_type(entry->type_information);
            }
            else if (common_rank > 0)
            {
                return_type = function_type_get_return_type(entry->type_information);

                if (!is_void_type(return_type))
                {
                    return_type = fortran_get_n_ranked_type(
                            function_type_get_return_type(entry->type_information),
                            common_rank, decl_context);
                }
            }
            else
            {
                error_printf_at(ast_get_locus(location), "mismatch of ranks in call to elemental intrinsic '%s'\n",
                        strtoupper(symbol->symbol_name));
                *result_type = get_error_type();
                return;
            }
        }
        else
        {
            return_type = function_type_get_return_type(entry->type_information);
        }

        // We are calling the deduced intrinsic
        symbol = entry;
    }
    else
    {
        type_t* function_type = no_ref(symbol->type_information);
        if (is_pointer_to_function_type(function_type))
        {
            function_type = pointer_type_get_pointee_type(function_type);
        }

        // This is now a specfic procedure reference
        ERROR_CONDITION (!is_function_type(function_type), "We need a function type!", 0);

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_info_items[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
        memset(argument_info_items, 0, sizeof(argument_info_items));

        int i;
        for (i = 0; i < explicit_num_actual_arguments; i++)
        {
            ERROR_CONDITION(nodecl_get_kind(nodecl_actual_arguments[i]) != NODECL_FORTRAN_ACTUAL_ARGUMENT,
                    "This node must be a NODECL_FORTRAN_ACTUAL_ARGUMENT", 0);

            int position = -1;
            if (nodecl_get_text(nodecl_actual_arguments[i]) == NULL)
            {
                position = i;
            }
            else
            {
                int j;
                for (j = 0; j < symbol_entity_specs_get_num_related_symbols(symbol); j++)
                {
                    scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(symbol, j);
                    ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, symbol),
                            "Related symbol must be a parameter of the function", 0 );

                    if (strcasecmp(related_sym->symbol_name, nodecl_get_text(nodecl_actual_arguments[i])) == 0)
                    {
                        position = j;
                    }
                }
                if (position < 0)
                {
                    error_printf_at(ast_get_locus(location), "keyword '%s' is not a dummy argument of function '%s'\n", 
                            nodecl_get_text(nodecl_actual_arguments[i]),
                            symbol->symbol_name);
                    *result_type = get_error_type();
                    return;
                }
            }

            if (argument_info_items[position].type != NULL)
            {
                error_printf_at(ast_get_locus(location), "argument keyword '%s' specified more than once\n", nodecl_get_text(nodecl_actual_arguments[i]));
                *result_type = get_error_type();
                return;
            }
            argument_info_items[position].type = nodecl_get_type(nodecl_get_child(nodecl_actual_arguments[i], 0));
            argument_info_items[position].argument = nodecl_get_child(nodecl_actual_arguments[i], 0);
        }

        int num_completed_arguments = explicit_num_actual_arguments;

        // Now complete with the optional ones
        for (i = 0; i < symbol_entity_specs_get_num_related_symbols(symbol); i++)
        {
            scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(symbol, i);
            ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, symbol),
                    "Related symbol must be a parameter of the function", 0 );

            if (argument_info_items[i].type == NULL)
            {
                if (symbol_entity_specs_get_is_optional(related_sym)
                        && !symbol_entity_specs_get_is_stmt_function(symbol))
                {
                    argument_info_items[i].type = related_sym->type_information;
                    argument_info_items[i].not_present = 1;
                    num_completed_arguments++;
                }
                else
                {
                    error_printf_at(ast_get_locus(location), "dummy argument '%s' of function '%s' has not been specified in function reference\n",
                            related_sym->symbol_name,
                            symbol->symbol_name);
                    *result_type = get_error_type();
                    return;
                }
            }
        }

        if (!function_type_get_lacking_prototype(function_type)
                && num_completed_arguments > function_type_get_num_parameters(function_type))
        {
            error_printf_at(ast_get_locus(location), "too many actual arguments in function reference to '%s'\n",
                    symbol->symbol_name);
            *result_type = get_error_type();
            return;
        }

        ERROR_CONDITION(!function_type_get_lacking_prototype(function_type) 
                && (num_completed_arguments != function_type_get_num_parameters(function_type)), 
                "Mismatch between arguments and the type of the function %d != %d", 
                num_completed_arguments,
                function_type_get_num_parameters(function_type));

        char argument_type_mismatch = 0;
        int common_rank = -1;
        if (!function_type_get_lacking_prototype(function_type))
        {
            actual_argument_info_t fixed_argument_info_items[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
            memcpy(fixed_argument_info_items, argument_info_items, sizeof(fixed_argument_info_items));

            if (symbol_entity_specs_get_is_elemental(symbol))
            {
                // We may have to adjust the ranks, first check that all the
                // ranks match
                char ok = 1;
                for (i = 0; i < num_completed_arguments && ok; i++)
                {
                    int current_rank = fortran_get_rank_of_type(fixed_argument_info_items[i].type); 
                    if (common_rank <= 0)
                    {
                        common_rank = current_rank;
                    }
                    else if ((common_rank != current_rank)
                            && current_rank != 0)
                    {
                        ok = 0;
                    }
                }

                if (ok)
                {
                    // Remove rank if they match, otherwise let it fail later
                    for (i = 0; i < num_completed_arguments && ok; i++)
                    {
                        fixed_argument_info_items[i].type = fortran_get_rank0_type(fixed_argument_info_items[i].type);
                    }
                }
            }

            for (i = 0; i < num_completed_arguments; i++)
            {
                scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(symbol, i);
                ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, symbol),
                        "Related symbol must be a parameter of the function", 0 );

                type_t* formal_type = no_ref(function_type_get_parameter_type_num(function_type, i));
                type_t* real_type = no_ref(fixed_argument_info_items[i].type);

                if (is_pointer_type(formal_type)
                        && !fixed_argument_info_items[i].not_present)
                {
                    // If the actual argument is a pointer type and the dummy argument is a derreference,
                    // get the pointer type being derreferenced
                    if (!is_pointer_type(real_type))
                    {
                        error_printf_at(nodecl_get_locus(fixed_argument_info_items[i].argument), "cannot associate non-POINTER actual argument to POINTER dummy argument\n");
                        // This is not a derreferenced pointer?
                        argument_type_mismatch = 1;
                        continue;
                    }
                }

                if (symbol_entity_specs_get_is_allocatable(related_sym)
                        && !fixed_argument_info_items[i].not_present)
                {
                    scope_entry_t* current_arg_sym = NULL; 
                    if (nodecl_get_kind(fixed_argument_info_items[i].argument) == NODECL_SYMBOL)
                    {
                        current_arg_sym = nodecl_get_symbol(fixed_argument_info_items[i].argument);
                    }
                    else if (nodecl_get_kind(fixed_argument_info_items[i].argument) == NODECL_CLASS_MEMBER_ACCESS)
                    {
                        current_arg_sym = nodecl_get_symbol(
                                nodecl_get_child(fixed_argument_info_items[i].argument, 1)
                                );
                    }

                    if (current_arg_sym == NULL
                            || !symbol_entity_specs_get_is_allocatable(current_arg_sym))
                    {
                        error_printf_at(nodecl_get_locus(fixed_argument_info_items[i].argument), "cannot associate non-ALLOCATABLE actual argument to ALLOCATABLE dummy argument\n");
                        argument_type_mismatch = 1;
                        break;
                    }
                }

                if (!check_argument_association(
                            symbol,
                            formal_type, 
                            real_type, 
                            fixed_argument_info_items[i].argument,

                            /* ranks_must_agree */ 0,

                            /* diagnostics */ 1,
                            /* argument_num */ i,
                            ast_get_locus(location)))
                {
                    argument_type_mismatch = 1;
                }
            }
        }

        if (argument_type_mismatch)
        {
            *result_type = get_error_type();
            return;
        }

        return_type = function_type_get_return_type(function_type);

        if (symbol_entity_specs_get_is_elemental(symbol)
                && !is_void_type(return_type))
        {
            if (common_rank > 0)
            {
                return_type = fortran_get_n_ranked_type(return_type, common_rank, decl_context);
            }
        }
    }

    // Simplify intrinsics
    if (symbol_entity_specs_get_is_builtin(symbol))
    {
        fortran_simplify_specific_intrinsic_call(symbol,
                nodecl_actual_arguments,
                explicit_num_actual_arguments,
                nodecl_simplify,
                ast_get_locus(procedure_designator));
    }

    if (symbol_entity_specs_get_is_implicit_basic_type(symbol))
    {
        if (!is_call_stmt)
        {
            // From now it is a FUNCTION
            symbol_entity_specs_set_is_implicit_basic_type(symbol, 0);
        }
        else
        {
            // From now it is a SUBROUTINE
            symbol->type_information = fortran_update_basic_type_with_type(
                    symbol->type_information,
                    get_void_type());
        }
        symbol_entity_specs_set_is_implicit_basic_type(symbol, 0);
    }
    else
    {
        if (is_void_type(return_type))
        {
            if (!is_call_stmt)
            {
                error_printf_at(ast_get_locus(location), "invalid function reference to a SUBROUTINE\n");
                *result_type = get_error_type();
                return;
            }
        }
        else
        {
            if (is_call_stmt)
            {
                error_printf_at(ast_get_locus(location), "invalid CALL statement to a FUNCTION\n");
                *result_type = get_error_type();
                return;
            }
        }
    }

    // Now make the code explicitly positional and adjust the pointer arguments
    nodecl_t nodecl_actual_positional_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(nodecl_actual_positional_arguments, 0, sizeof(nodecl_actual_positional_arguments));

    int num_parameter_types = function_type_get_num_parameters(no_ref(symbol->type_information));

    int last_argument = -1;
    int i;
    for (i = 0; i < explicit_num_actual_arguments; i++)
    {
        ERROR_CONDITION(nodecl_get_kind(nodecl_actual_arguments[i]) != NODECL_FORTRAN_ACTUAL_ARGUMENT, "Invalid node", 0);
        const char* keyword_name = nodecl_get_text(nodecl_actual_arguments[i]);

        int position = -1;
        if (keyword_name == NULL)
        {
            position = i;
        }
        else
        {
            int j;
            for (j = 0; j < symbol_entity_specs_get_num_related_symbols(symbol); j++)
            {
                scope_entry_t* related_sym = symbol_entity_specs_get_related_symbols_num(symbol, j);
                ERROR_CONDITION(!symbol_is_parameter_of_function(related_sym, symbol),
                        "Related symbol must be a parameter of the function", 0 );

                if (strcasecmp(related_sym->symbol_name, keyword_name) == 0)
                {
                    position = j;
                }
            }
            ERROR_CONDITION(position < 0, "Invalid dummy argument name", 0);
            ERROR_CONDITION(position >= MCXX_MAX_FUNCTION_CALL_ARGUMENTS, "Invalid position", 0);
        }

        nodecl_t nodecl_argument = nodecl_actual_arguments[i];

        if (position < num_parameter_types)
        {
            type_t* parameter_type = function_type_get_parameter_type_num(
                    no_ref(symbol->type_information), position);
            nodecl_argument = fortran_nodecl_adjust_function_argument(
                    parameter_type, nodecl_argument);
        }

        nodecl_actual_positional_arguments[position] = nodecl_argument;
        last_argument = last_argument < position ? position : last_argument;
    }

    if (!function_type_get_lacking_prototype(no_ref(symbol->type_information)))
    {
        last_argument = function_type_get_num_parameters(no_ref(symbol->type_information)) - 1;
    }

    // Copy back to nodecl_actual_arguments
    for (i = 0; i <= last_argument; i++)
    {
        if (nodecl_is_null(nodecl_actual_positional_arguments[i]))
        {
            nodecl_actual_arguments[i] = nodecl_make_fortran_not_present(ast_get_locus(location));
        }
        else
        {
            nodecl_actual_arguments[i] = nodecl_actual_positional_arguments[i];
        }
    }

    *num_actual_arguments = (last_argument + 1);

    *result_type = return_type;
    *called_symbol = symbol;
}

static void check_function_call(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    char is_call_stmt = (ASTText(expr) != NULL
            && (strcmp(ASTText(expr), "call") == 0));

    AST procedure_designator = ASTSon0(expr);
    AST actual_arg_spec_list = ASTSon1(expr);

    scope_entry_list_t* symbol_list = NULL;
    check_symbol_of_called_name(procedure_designator, decl_context, &symbol_list, is_call_stmt);

    if (symbol_list == NULL)
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    int num_actual_arguments = 0;

    nodecl_t nodecl_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(nodecl_arguments, 0, sizeof(nodecl_arguments));

    // Check arguments
    if (actual_arg_spec_list != NULL)
    {
        char with_keyword = 0;
        char wrong_arg_spec_list = 0;
        AST it;
        for_each_element(actual_arg_spec_list, it)
        {
            AST actual_arg_spec = ASTSon1(it);
            AST keyword = ASTSon0(actual_arg_spec);
            const char* keyword_str = NULL;
            if (keyword != NULL)
            {
                with_keyword = 1;
                keyword_str = strtolower(ASTText(keyword));
            }
            else if (with_keyword) // keyword == NULL
            {
                error_printf_at(ast_get_locus(actual_arg_spec), "in function call, '%s' argument requires a keyword\n",
                        fortran_prettyprint_in_buffer(actual_arg_spec));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(actual_arg_spec));
                return;
            }

            AST actual_arg = ASTSon1(actual_arg_spec);

            if (ASTKind(actual_arg) != AST_ALTERNATE_RESULT_SPEC)
            {
                nodecl_t nodecl_argument = nodecl_null();

                // If the actual_arg is a symbol, we'll do a special checking
                // The reason: detect intrinsic functions in arguments
                if (ASTKind(actual_arg) == AST_SYMBOL)
                {
                    check_symbol_of_argument(actual_arg, decl_context, &nodecl_argument);
                }
                else
                {
                    fortran_check_expression_impl_(actual_arg, decl_context, &nodecl_argument);
                }

                if (nodecl_is_err_expr(nodecl_argument))
                {
                    wrong_arg_spec_list = 1;
                }

                nodecl_arguments[num_actual_arguments] = nodecl_make_fortran_actual_argument(nodecl_argument,
                        ast_get_locus(actual_arg));
                nodecl_set_text(nodecl_arguments[num_actual_arguments], keyword_str);
            }
            else
            {
                if (!is_call_stmt)
                {
                    error_printf_at(ast_get_locus(actual_arg_spec), "only CALL statement allows an alternate return\n");
                    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                    return;
                }

                scope_entry_t* label = fortran_query_label(ASTSon0(actual_arg), 
                        decl_context, /* is_definition */ 0);

                nodecl_arguments[num_actual_arguments] =
                    nodecl_make_fortran_actual_argument(
                            nodecl_make_fortran_alternate_return_argument(
                                label, get_void_type(),
                                ast_get_locus(actual_arg)),
                            ast_get_locus(actual_arg));
            }

            num_actual_arguments++;
        }

        if (wrong_arg_spec_list)
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }


    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    scope_entry_t* generic_specifier_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol_list(symbol_list, 
            decl_context, 
            expr, 
            procedure_designator, 
            &num_actual_arguments,
            nodecl_arguments,
            is_call_stmt,
            // out
            &result_type,
            &called_symbol,
            &generic_specifier_symbol,
            &nodecl_simplify
            );

    ERROR_CONDITION(result_type == NULL, "Invalid type returned by check_called_symbol_list", 0);

    if (is_error_type(result_type))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (nodecl_is_null(nodecl_simplify))
    {
        nodecl_t nodecl_argument_list = nodecl_null();
        int i;
        for (i = 0; i < num_actual_arguments; i++)
        {
            nodecl_argument_list = nodecl_append_to_list(nodecl_argument_list, nodecl_arguments[i]);
        }

        nodecl_t nodecl_generic_spec = nodecl_null();

        if (generic_specifier_symbol != NULL)
        {
            nodecl_generic_spec = nodecl_make_symbol(generic_specifier_symbol, 
                    ast_get_locus(procedure_designator));
        }

        nodecl_t nodecl_called = 
                nodecl_make_symbol(called_symbol, ast_get_locus(procedure_designator));
        if (called_symbol->kind == SK_VARIABLE)
        {
            if (is_pointer_to_function_type(no_ref(called_symbol->type_information)))
            {
                nodecl_called = nodecl_make_dereference(
                        nodecl_called,
                        lvalue_ref(called_symbol->type_information),
                        ast_get_locus(procedure_designator));
            }
        }

        *nodecl_output = nodecl_make_function_call(
                nodecl_called,
                nodecl_argument_list,
                nodecl_generic_spec,
                /* function_form */ nodecl_null(),
                result_type,
                ast_get_locus(expr));

#if 0
        if (is_pointer_type(no_ref(result_type)))
        {
            *nodecl_output = nodecl_make_dereference(*nodecl_output, 
                    lvalue_ref(pointer_type_get_pointee_type(no_ref(result_type))),
                    ast_get_locus(expr));
        }
#endif
    }
    else
    {
        *nodecl_output = nodecl_simplify;
    }
}

static void check_greater_or_equal_than(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_greater_than(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_hexadecimal_literal(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    // We allow X and Z
    compute_boz_literal(expr, "xz", 16, nodecl_output);
}

static void check_hollerith_constant(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    const char* str = ast_get_text(expr);
    *nodecl_output = nodecl_make_fortran_hollerith(get_hollerith_type(),
            ASTText(expr),
            const_value_make_string(str, strlen(str)),
            ast_get_locus(expr));
}

static void check_image_ref(AST expr UNUSED_PARAMETER, const decl_context_t* decl_context UNUSED_PARAMETER,
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    error_printf_at(ast_get_locus(expr), "sorry: image references not supported\n");
    *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
}

static void check_logical_and(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_logical_equal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_logical_different(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_logical_or(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_lower_or_equal_than(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_lower_than(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_minus_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_mult_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_neg_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_not_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_octal_literal(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    compute_boz_literal(expr, "o", 8, nodecl_output);
}

static void check_parenthesized_expression(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_expr);
    nodecl_expr = fortran_expression_as_value(nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        return;
    }

    *nodecl_output = nodecl_make_parenthesized_expression(
            nodecl_expr,
            nodecl_get_type(nodecl_expr),
            ast_get_locus(expr));

    if (nodecl_is_constant(nodecl_expr))
    {
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_expr));
    }
}

static void check_plus_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_power_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void common_binary_intrinsic_check(AST expr, const decl_context_t*,
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output);
static void common_binary_check(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expr);
    AST rhs = ASTSon1(expr);
    nodecl_t nodecl_lhs = nodecl_null();
    fortran_check_expression_impl_(lhs, decl_context, &nodecl_lhs);
    nodecl_t nodecl_rhs = nodecl_null();
    fortran_check_expression_impl_(rhs, decl_context, &nodecl_rhs);

    if (nodecl_is_err_expr(nodecl_lhs)
            || nodecl_is_err_expr(nodecl_rhs))
    {
       *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr)); 
       return;
    }

    common_binary_intrinsic_check(expr, decl_context, nodecl_lhs, nodecl_rhs, nodecl_output);
}

static void common_binary_intrinsic_check(AST expr, const decl_context_t* decl_context,
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output)
{
    compute_result_of_intrinsic_operator(expr, decl_context, nodecl_lhs, nodecl_rhs, nodecl_output);
}

static void common_unary_intrinsic_check(AST expr, const decl_context_t* decl_context,
        nodecl_t nodecl_rhs, nodecl_t* nodecl_output);

static void common_unary_check(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output) 
{
    AST rhs = ASTSon0(expr);
    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(rhs, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
       *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr)); 
       return;
    }

    common_unary_intrinsic_check(expr, decl_context, nodecl_expr, nodecl_output);
}

static void common_unary_intrinsic_check(AST expr, const decl_context_t* decl_context,
        nodecl_t nodecl_rhs, nodecl_t* nodecl_output)
{
    compute_result_of_intrinsic_operator(expr, decl_context, nodecl_null(), nodecl_rhs, nodecl_output);
}

static void check_string_literal(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    const char* literal = ASTText(expr);

    enum { MAX_KIND_LENGTH = 31 };
    char kind_str[MAX_KIND_LENGTH + 1] = { 0 };
    char *kind_last = kind_str;

    type_t* character_type = fortran_get_default_character_type();

    if ((literal[0] != '"'
                    && literal[0] != '\''))
    {
        // There is KIND, check it
        // First gather the characters that make up the kind
        while (*literal != '"'
                && *literal != '\''
                && ((unsigned int)(kind_last - kind_str) < MAX_KIND_LENGTH))
        {
            *kind_last = *literal;
            literal++;
            kind_last++;
        }

        if (*literal != '"'
                && *literal != '\'')
        {
            error_printf_at(ast_get_locus(expr), "KIND specifier is too long\n");
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }

        ERROR_CONDITION(kind_last == kind_str, "No characters were consumed", 0);
        kind_last--;

        ERROR_CONDITION(*kind_last != '_', "Wrong delimiter '%c'", *kind_last);
        *kind_last = '\0';

        // Compute the value
        int kind = compute_kind_from_literal(kind_str, expr, decl_context);
        if (kind == 0)
        {
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }

        nodecl_t loc = nodecl_make_text(ASTText(expr), ast_get_locus(expr));
        character_type = choose_character_type_from_kind(loc, kind);
        nodecl_free(loc);
    }

    const char* whole_literal = literal;

    int length = strlen(literal);

    char real_string[length + 1];
    int real_length = 0;
    char delim = literal[0];
    literal++; // Jump delim

    while (*literal != delim
                || *(literal+1) == delim)
    {
        ERROR_CONDITION(real_length >= (length+1), "Wrong construction of string literal '%s'", whole_literal);

        if (*literal !=  delim)
        {
            real_string[real_length] = *literal;
            literal++;
        }
        else
        {
            real_string[real_length] = *literal;
            // Jump both '' or ""
            literal += 2;
        }

        real_length++;
    }
    real_string[real_length] = '\0';

    nodecl_t one = nodecl_make_integer_literal(
            fortran_get_default_integer_type(),
            const_value_get_signed_int(1),
            ast_get_locus(expr));
    nodecl_t length_tree = nodecl_make_integer_literal(
            character_type,
            const_value_get_signed_int(real_length),
            ast_get_locus(expr));

    type_t* t = get_array_type_bounds(fortran_get_default_character_type(), one, length_tree, decl_context);

    const_value_t* value = const_value_make_string(real_string, real_length);

    *nodecl_output = nodecl_make_string_literal(t, value, ast_get_locus(expr));

    // Also keep the string itself for codegen
    nodecl_set_text(*nodecl_output, whole_literal);
}

static void check_user_defined_unary_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    // This is an AST_NAMED_PAIR_SPEC with no name. This way it is easier to
    // reuse common function call code
    AST operand = ASTSon1(expr);
    AST operand_expr = ASTSon1(operand);

    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(operand_expr, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    AST operator = ASTSon0(expr);
    const char* operator_name = strtolower(strappend(".operator.", ASTText(operator)));
    scope_entry_list_t* call_list = fortran_query_name_str_for_function(decl_context, operator_name,
            ast_get_locus(operator));

    if (call_list == NULL)
    {
        error_printf_at(ast_get_locus(expr), "unknown user-defined operator '%s'\n", ASTText(operator));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    int num_actual_arguments = 1;
    nodecl_t nodecl_arguments[1] = {
        nodecl_make_fortran_actual_argument(nodecl_expr, nodecl_get_locus(nodecl_expr))
    };

    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    scope_entry_t* generic_specifier_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol_list(call_list,
            decl_context,
            expr,
            operator,
            &num_actual_arguments,
            nodecl_arguments,
            /* is_call_stmt */ 0,
            // out
            &result_type,
            &called_symbol,
            &generic_specifier_symbol,
            &nodecl_simplify);

    ERROR_CONDITION(result_type == NULL, "Invalid type returned by check_called_symbol_list", 0);

    if (is_error_type(result_type))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    nodecl_t nodecl_generic_spec = nodecl_null();
    if (generic_specifier_symbol != NULL)
    {
        nodecl_generic_spec = nodecl_make_symbol(generic_specifier_symbol, ast_get_locus(expr));
    }

    nodecl_t nodecl_called = nodecl_make_symbol(called_symbol, ast_get_locus(expr));

    if (called_symbol->kind == SK_VARIABLE)
    {
        // This must be a pointer to function
        ERROR_CONDITION(!is_pointer_to_function_type(no_ref(called_symbol->type_information)), "Invalid symbol", 0);

        nodecl_called = nodecl_make_dereference(
                nodecl_called,
                lvalue_ref(called_symbol->type_information),
                ast_get_locus(expr));
    }

    *nodecl_output = nodecl_make_function_call(
            nodecl_called,
            nodecl_make_list_1(nodecl_arguments[0]),
            nodecl_generic_spec,
            /* function_form */ nodecl_null(),
            // Fortran has not template arguments
            result_type,
            ast_get_locus(expr));

#if 0
    if (is_pointer_type(no_ref(result_type)))
    {
        *nodecl_output = nodecl_make_dereference(*nodecl_output,
                lvalue_ref(pointer_type_get_pointee_type(no_ref(result_type))),
                ast_get_locus(expr));
    }
#endif
}

static void check_user_defined_binary_op(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    // This is an AST_NAMED_PAIR_SPEC with no name. This way it is easier to
    // reuse common function call code
    AST lhs = ASTSon1(expr);
    AST lhs_expr = ASTSon1(lhs);

    nodecl_t nodecl_lhs = nodecl_null();
    fortran_check_expression_impl_(lhs_expr, decl_context, &nodecl_lhs);

    if (nodecl_is_err_expr(nodecl_lhs))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    AST rhs = ASTSon2(expr);
    AST rhs_expr = ASTSon1(rhs);

    nodecl_t nodecl_rhs = nodecl_null();
    fortran_check_expression_impl_(rhs_expr, decl_context, &nodecl_rhs);

    if (nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    AST operator = ASTSon0(expr);
    const char* operator_name = strtolower(strappend(".operator.", ASTText(operator)));
    scope_entry_list_t* call_list = fortran_query_name_str_for_function(decl_context, operator_name,
            ast_get_locus(operator));

    if (call_list == NULL)
    {
        error_printf_at(ast_get_locus(expr), "unknown user-defined operator '%s'\n", ASTText(operator));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    int num_actual_arguments = 2;
    nodecl_t nodecl_arguments[2] = {
        nodecl_make_fortran_actual_argument(nodecl_lhs, nodecl_get_locus(nodecl_lhs)),
        nodecl_make_fortran_actual_argument(nodecl_rhs, nodecl_get_locus(nodecl_rhs))
    };

    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    scope_entry_t* generic_specifier_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol_list(call_list,
            decl_context,
            /* location */ expr,
            operator,
            &num_actual_arguments,
            nodecl_arguments,
            /* is_call_stmt */ 0,
            // out
            &result_type,
            &called_symbol,
            &generic_specifier_symbol,
            &nodecl_simplify);

    ERROR_CONDITION(result_type == NULL, "Invalid type returned by check_called_symbol_list", 0);

    if (is_error_type(result_type))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    nodecl_t nodecl_generic_spec = nodecl_null();
    if (generic_specifier_symbol != NULL)
    {
        nodecl_generic_spec = nodecl_make_symbol(generic_specifier_symbol, ast_get_locus(expr));
    }

    nodecl_t nodecl_called = nodecl_make_symbol(called_symbol, ast_get_locus(expr));

    if (called_symbol->kind == SK_VARIABLE)
    {
        // This must be a pointer to function
        ERROR_CONDITION(!is_pointer_to_function_type(no_ref(called_symbol->type_information)), "Invalid symbol", 0);

        nodecl_called = nodecl_make_dereference(
                nodecl_called,
                lvalue_ref(called_symbol->type_information),
                ast_get_locus(expr));
    }

    *nodecl_output = nodecl_make_function_call(
            nodecl_called,
            nodecl_make_list_2(
                nodecl_arguments[0],
                nodecl_arguments[1]),
            nodecl_generic_spec,
            /* function_form */ nodecl_null(),
            // Fortran has not template arguments
            result_type,
            ast_get_locus(expr));

#if 0
    if (is_pointer_type(no_ref(result_type)))
    {
        *nodecl_output = nodecl_make_dereference(*nodecl_output, 
                lvalue_ref(pointer_type_get_pointee_type(no_ref(result_type))),
                ast_get_locus(expr));
    }
#endif
}

static void check_nodecl_literal(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    // Make sure we copy it, otherwise under ambiguity
    // the same trees would be wrongly handled
    *nodecl_output = nodecl_shallow_copy(nodecl_make_from_ast_nodecl_literal(expr));
}

static void check_symbol_literal(AST expr, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    const char *prefix = NULL;
    void *p = NULL;
    const char *tmp = ASTText(ASTSon0(expr));
    unpack_pointer(tmp, &prefix, &p);

    ERROR_CONDITION(prefix == NULL || p == NULL || strcmp(prefix, "symbol") != 0,
            "Failure during unpack of symbol literal", 0);

    scope_entry_t* entry = (scope_entry_t*)p;

    *nodecl_output = nodecl_make_symbol(
            entry,
            ast_get_locus(expr));

    if (entry->kind == SK_VARIABLE)
    {
        if (!is_const_qualified_type(no_ref(entry->type_information)))
        {
            nodecl_set_type(*nodecl_output, lvalue_ref(entry->type_information));
        }
        else
        {
            nodecl_set_type(*nodecl_output, entry->type_information);
        }

#if 0
        if (is_pointer_type(entry->type_information))
        {
            *nodecl_output = nodecl_make_dereference(
                    *nodecl_output,
                    lvalue_ref(pointer_type_get_pointee_type(entry->type_information)),
                    nodecl_get_locus(*nodecl_output));
        }
#endif
    }
    else
    {
        // Best effort
        nodecl_set_type(*nodecl_output, entry->type_information);
    }
}

#if 0
static char is_name_of_funtion_call(AST expr)
{
    return ASTParent(expr) != NULL
        && ASTKind(ASTParent(expr)) == AST_FUNCTION_CALL;
}
#endif

static void check_symbol_of_called_name(AST sym,
        const decl_context_t* decl_context, 
        scope_entry_list_t** call_list, 
        char is_call_stmt)
{
    if (ASTKind(sym) != AST_SYMBOL
            && ASTKind(sym) != AST_SYMBOL_LITERAL_REF)
    {
        error_printf_at(ast_get_locus(sym), "expression is not a valid procedure designator\n");
        *call_list = NULL;
        return;
    }

    if (ASTKind(sym) == AST_SYMBOL_LITERAL_REF)
    {
        nodecl_t nodecl_output = nodecl_null();
        check_symbol_literal(sym, decl_context, &nodecl_output);
        if (nodecl_get_symbol(nodecl_output) != NULL)
        {
            *call_list = entry_list_new(nodecl_get_symbol(nodecl_output));
        }
        return;
    }

    // Look the symbol up. This will ignore INTRINSIC names not known to be generic/specific
    // (not declared at all in the scope)
    scope_entry_list_t* entry_list = fortran_query_name_str_for_function(decl_context, ASTText(sym),
            ast_get_locus(sym));

    if (entry_list != NULL)
    {
        scope_entry_t* intrinsic = fortran_query_intrinsic_name_str(decl_context, ASTText(sym));
        if (intrinsic != NULL)
        {
            scope_entry_list_t* new_entry_list = NULL;

            scope_entry_list_iterator_t* iter = NULL;
            for (iter = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(iter);
                    entry_list_iterator_next(iter))
            {
                scope_entry_t* symbol = entry_list_iterator_current(iter);
                if (symbol->decl_context->current_scope == symbol->decl_context->global_scope)
                {
                    // This is a global name that matches the name of an intrinsic, hide the global name.
                    //
                    // Global names (but intrinsics) do not exist in Fortran, thus this symbol came elsewhere
                    // (likely from a C header)
                    continue;
                }
                new_entry_list = entry_list_add(new_entry_list, symbol);
            }

            entry_list_free(entry_list);
            entry_list = new_entry_list;
        }
    }

    if (entry_list == NULL)
    {
        char entry_is_an_intrinsic = 0;

        // We did not find anything.
        //
        // Does this name match the name of an INTRINSIC?
        scope_entry_t* entry = fortran_query_intrinsic_name_str(decl_context, ASTText(sym));
        if (entry != NULL)
        {
            // It names an intrinsic
            entry_is_an_intrinsic = 1;

            // Make sure this intrinsic can be invoked as we intend to do
            if (is_call_stmt != symbol_entity_specs_get_is_intrinsic_subroutine(entry)
                    && (!is_call_stmt) != symbol_entity_specs_get_is_intrinsic_function(entry))
            {
                entry_is_an_intrinsic = 0;
                entry = NULL;
            }

            if (entry_is_an_intrinsic)
            {
                if (!fortran_checking_initializer())
                {
                    // Inserting the intrinsic as an alias is okay here since there
                    // is no doubt about the name being the INTRINSIC symbol
                    //
                    // We do not insert intrinsics from initializations just in case
                    // an INTRINSIC or EXTERNAL statement appears later
                    insert_alias(decl_context->current_scope, entry, strtolower(ASTText(sym)));
                }
                else
                {
                    // if (decl_context->current_scope->related_entry != NULL
                    //         && decl_context->current_scope->related_entry->kind == SK_MODULE)
                    // {
                    //         warn_printf("%s: warning: procedure reference to intrinsic '%s' has been stablished "
                    //                 "to unspecified at this module scoping unit\n", ast_location(sym), entry->symbol_name);
                    //         info_printf("%s: info: you may want to add an INTRINSIC or EXTERNAL statement prior to this use "
                    //                 "to avoid lookup problems later when using name '%s'\n", ast_location(sym), entry->symbol_name);
                    // }
                }

                // We are done, this is the single name being called
                *call_list = entry_list_new(entry);
                return;
            }
        }

        if (!entry_is_an_intrinsic)
        {
            // Well, it does not name an intrinsic either (_or_ it does name
            // one but it does not match its usage) (i.e. CALLing an INTRINSIC
            // FUNCTION)
            if (is_call_stmt)
            {
                // We did not find the symbol. But this is okay since this is a CALL.
                // CALL does not need a type, thus IMPLICIT plays no role here
                // Just sign in the symbol and give it an unprototyped type (= implicit interface)
                const decl_context_t* program_unit_context = decl_context->current_scope->related_entry->related_decl_context;
                entry = new_fortran_symbol(program_unit_context, ASTText(sym));
                entry->kind = SK_FUNCTION;
                entry->locus = ast_get_locus(sym);
                entry->type_information = get_nonproto_function_type(get_void_type(), 0);

                remove_unknown_kind_symbol(decl_context, entry);
            }
            else
            {
                if (is_implicit_none(decl_context))
                {
                    // This is not a CALL and we are under IMPLICIT NONE. Something is amiss
                    error_printf_at(ast_get_locus(sym), "'%s' is not a known function name\n", ASTText(sym));
                    *call_list = NULL;
                    return;
                }
                else if (!is_implicit_none(decl_context))
                {
                    // This a new function brought to you by IMPLICIT after a function reference
                    entry = new_fortran_implicit_symbol(decl_context, sym, strtolower(ASTText(sym)));
                    entry->kind = SK_FUNCTION;
                    entry->type_information =
                        get_nonproto_function_type(get_implicit_type_for_symbol(decl_context, entry->symbol_name), 0);

                    remove_unknown_kind_symbol(decl_context, entry);
                }
            }

            // Do not allow its type be redefined anymore
            symbol_entity_specs_set_is_implicit_basic_type(entry, 0);

            // And we are done
            *call_list = entry_list_new(entry);
            return;
        }
    }
    else
    {
        // fortran_query_name_str_for_function returns a single item if a non generic name was found
        // if more than one generic name is found, all the visible ones in the current scope are returned
        // thus we do not have to check anything
        if (entry_list_size(entry_list) == 1
                && entry_list_head(entry_list)->kind != SK_GENERIC_NAME
                && !symbol_entity_specs_get_is_builtin(entry_list_head(entry_list)))
        {
            scope_entry_t* entry = entry_list_head(entry_list);
            if (entry->kind == SK_UNDEFINED)
            {
                // Make it a function
                entry->kind = SK_FUNCTION;
                remove_unknown_kind_symbol(decl_context, entry);

                scope_entry_t * intrinsic_sym = NULL;

                if (!symbol_is_parameter_of_function(entry, decl_context->current_scope->related_entry))
                {
                    intrinsic_sym = fortran_query_intrinsic_name_str(decl_context, entry->symbol_name);
                }

                if (symbol_entity_specs_get_alias_to(entry) != NULL
                        && symbol_entity_specs_get_is_builtin(
                            symbol_entity_specs_get_alias_to(entry)))
                {
                    /*
                     * Heads up here!
                     *
                     * PROGRAM P
                     *   IMPLICIT NONE
                     *
                     *   CALL F1(SQRT)      !!! (1)
                     *   CALL F2(SQRT(1.2)) !!! (2)
                     * END PROGRAM P
                     *
                     * Initially in (1) we created an SK_UNDEFINED with an alias to the intrinsic SQRT because
                     * we were not 100% sure if this was going to be a variable or the called intrinsic. Then in (2)
                     * our suspicions get confirmed: SQRT was indeed an INTRINSIC, but we created a fake symbol
                     * which now we want it to behave like the intrinsic.
                     *
                     * Note 1. If (2) were removed, then SQRT usage is wrong. 
                     *
                     * Note 2. Nothing of this happens if SQRT is stated as an
                     * intrinsic using an INTRINSIC :: SQRT statement.
                     */

                    remove_untyped_symbol(decl_context, entry);

                    scope_entry_t* intrinsic_symbol = symbol_entity_specs_get_alias_to(entry);
                    copy_intrinsic_function_info(entry, intrinsic_symbol);
                }
                else if (intrinsic_sym != NULL)
                {
                    // From now, the symbol is an intrinsic
                    copy_intrinsic_function_info(entry, intrinsic_sym);
                }
                else
                {
                    // Implicitly declared function due to its usage
                    char was_ref = is_lvalue_reference_type(entry->type_information);
                    if (!is_call_stmt)
                    {
                        entry->type_information = get_nonproto_function_type(entry->type_information, 0);

                        // This symbol is not untyped anymore
                        remove_untyped_symbol(decl_context, entry);
                        // nor its type can be redefined (this would never happen in real Fortran because of statement ordering)
                        symbol_entity_specs_set_is_implicit_basic_type(entry, 0);
                    }
                    else
                    {
                        // This is a call but do not change its status of untyped type
                        entry->type_information = get_nonproto_function_type(get_void_type(), 0);
                    }

                    if (was_ref)
                        entry->type_information = lvalue_ref(entry->type_information);
                }
            }

            if (entry->kind == SK_FUNCTION)
            {
                // OK
                if (symbol_entity_specs_get_is_implicit_basic_type(entry))
                {
                    // Case for
                    //
                    // EXTERNAL :: FOO, BAR
                    //
                    // CALL FOO()
                    // A = BAR(3.5)

                    if (is_call_stmt)
                    {
                        char was_ref = is_lvalue_reference_type(entry->type_information);
                        entry->type_information = get_nonproto_function_type(get_void_type(), 0);

                        if (was_ref)
                            entry->type_information = lvalue_ref(entry->type_information);
                    }

                    // This symbol is not untyped anymore
                    remove_untyped_symbol(decl_context, entry);
                    // nor its type can be redefined (this would never happen in real Fortran because of statement ordering)
                    symbol_entity_specs_set_is_implicit_basic_type(entry, 0);
                }
            }
            else if (entry->kind == SK_VARIABLE
                    && (is_pointer_to_function_type(no_ref(entry->type_information))
                        || /* dummy procedures */ is_function_type(no_ref(entry->type_information))))
            {
                // OK
            }
            else
            {
                error_printf_at(ast_get_locus(sym), "name '%s' %s\n", entry->symbol_name,
                        is_call_stmt ? "is not a valid subroutine name for a CALL statement" : "cannot appear in a function reference");
                *call_list = NULL;
                return;
            }

            // And we are done
            *call_list = entry_list_new(entry);
        }
        else
        {
            *call_list = entry_list;
        }
    }
}

// Common function when we finally understand that a name must be a variable-name
static void check_symbol_name_as_a_variable(
        AST sym,
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(entry->kind != SK_VARIABLE, 
            "Symbol must be a SK_VARIABLE but it is a %s", 
            symbol_kind_name(entry));

    // It might happen that dummy arguments/result do not have any implicit
    // type here (because the input code is wrong)
    if (is_error_type(entry->type_information))
    {
        // This error should have already been signaled elsewhere
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(sym));
        return;
    }

    if (is_void_type(no_ref(entry->type_information))
            && symbol_entity_specs_get_is_implicit_basic_type(entry)
            && is_implicit_none(decl_context))
    {
        if (symbol_is_parameter_of_function(entry, 
                    decl_context->current_scope->related_entry)
                && fortran_checking_array_expression())
        {
            // Special case for this (incorrect but tolerated) case
            //
            // SUBROUTINE S(A, N)
            //    IMPLICIT NONE
            //
            //    INTEGER :: A(N+1)
            //    INTEGER :: N
            //    ...

            // We currently do not have any mean to remember that the usage of this
            // variable involves some information for it. What we can do, though
            // is set it a type and mark it as implicitly defined (even though we are under
            // IMPLICIT NONE)
            entry->type_information = get_lvalue_reference_type(fortran_get_default_integer_type());
            symbol_entity_specs_set_is_implicit_basic_type(entry, 1);

            // Being unable to remember that this must be an integer hinders us to detect
            // the following (100% wrong) case
            //
            // SUBROUTINE S(A, N)
            //    IMPLICIT NONE
            //
            //    INTEGER :: A(N+1)
            //    REAL :: N         ! Error: N is used in a way that cannot have this type
            //                      ! We will not detect this case!
            //
            // Note that this extension is utterly broken since we could write this
            //
            // SUBROUTINE S(N)
            //    IMPLICIT NONE
            //
            //    INTEGER :: A(KIND(N))   ! Aha...
            //    INTEGER(KIND=8) :: N    ! What now?
            //
            // Every compiler will react on a different way. Some will assume a default kind for N (i.e. 4)
            // some others will complain of KIND(N) (though they do not in the case of 'N + 1', etc)
            //
            // Any approach is not driven by the standard, so anything we do is
            // both OK and wrong at the same time
        }
        else
        {
            error_printf_at(ast_get_locus(sym), "symbol '%s' has no IMPLICIT type\n",
                    entry->symbol_name);
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(sym));
            return;
        }
    }


    *nodecl_output = nodecl_make_symbol(entry, ast_get_locus(sym));
    if (!is_const_qualified_type(no_ref(entry->type_information)))
    {
        nodecl_set_type(*nodecl_output, lvalue_ref(entry->type_information));
    }
    else
    {
        nodecl_set_type(*nodecl_output, entry->type_information);
    }

    if (is_const_qualified_type(no_ref(entry->type_information))
            && !nodecl_is_null(entry->value)
            && nodecl_is_constant(entry->value))
    {
        // Use the constant value instead
        nodecl_t nodecl_const_val = fortran_const_value_to_nodecl(nodecl_get_constant(entry->value));
        if (!nodecl_is_null(nodecl_const_val)
                // Avoid a constant pointer be folded here
                && !is_pointer_type(entry->type_information)
                // Cruft from ISO_C_BINDING
                && !(symbol_entity_specs_get_from_module(entry) != NULL
                    && strcasecmp(symbol_entity_specs_get_from_module(entry)->symbol_name, "iso_c_binding") == 0))
        {
            nodecl_t nodecl_old = *nodecl_output;

            type_t* orig_type = nodecl_get_type(*nodecl_output);
            *nodecl_output = nodecl_const_val;
            nodecl_set_type(*nodecl_output, orig_type);

            nodecl_set_locus_as(*nodecl_output, nodecl_old);
        }
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(entry->value));
    }

#if 0
    if (is_pointer_type(no_ref(entry->type_information)))
    {
        *nodecl_output = 
            nodecl_make_dereference(
                    *nodecl_output,
                    lvalue_ref(pointer_type_get_pointee_type(no_ref(entry->type_information))),
                    ast_get_locus(sym));
    }
#endif
}

static void check_symbol_of_argument(AST sym, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    // Look the symbol up. This will ignore INTRINSIC names
    scope_entry_t* entry = fortran_query_name_str(decl_context, ASTText(sym),
            ast_get_locus(sym));
    if (entry == NULL)
    {
        // We did not find anything.
        //
        // Does this name match the name of an INTRINSIC?
        entry = fortran_query_intrinsic_name_str(decl_context, ASTText(sym));

        if (entry != NULL)
        {
            scope_entry_t* original_intrinsic = entry;
            // It names an intrinsic
            if (!is_implicit_none(decl_context))
            {
                // If we are _not_ in an IMPLICIT NONE we just create a new
                // symbol. Later it might be promoted to a SK_VARIABLE or
                // SK_FUNCTION
                entry = new_fortran_implicit_symbol(decl_context, sym, ASTText(sym));
            }
            else
            {
                // Under IMPLICIT NONE we are just unsure about this,
                // so just remember the intrinsic
                //
                // See a long comment in check_symbol_of_called_name
                // explaining this case
                entry = new_fortran_symbol(decl_context, ASTText(sym));
                entry->locus = ast_get_locus(sym);
                entry->type_information = get_implicit_none_type();

                // This is actually an implicit none, so it is actually
                // something untyped!
                add_untyped_symbol(decl_context, entry);
            }

            // Remember the intrinsic we named
            symbol_entity_specs_set_alias_to(entry, original_intrinsic);
        }
        else
        {   
            // It is not an intrinsic
            if (!is_implicit_none(decl_context))
            {
                // If we are _not_ in IMPLICIT NONE then just create a
                // SK_UNDEFINED symbol. Later it might be promoted to a
                // SK_VARIABLE or SK_FUNCTION 
                entry = new_fortran_implicit_symbol(decl_context, sym, ASTText(sym));
            }
            else
            {
                // Under IMPLICIT NONE this means something is amiss
                error_printf_at(ast_get_locus(sym), "Symbol '%s' has not IMPLICIT type\n",
                        fortran_prettyprint_in_buffer(sym));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(sym));
                return;
            }
        }
    }
    
    if (entry == NULL || 
           (entry->kind != SK_VARIABLE &&
            entry->kind != SK_FUNCTION && 
            entry->kind != SK_UNDEFINED))
    {
        error_printf_at(ast_get_locus(sym), "'%s' cannot be an argument\n", entry->symbol_name);
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(sym));
        return;
    }
    if (entry->kind == SK_VARIABLE)
    {
        check_symbol_name_as_a_variable(sym, entry, decl_context, nodecl_output);
    }
    else if (entry->kind == SK_UNDEFINED)
    {
        if (is_pointer_type(no_ref(entry->type_information)))
        {
            // Used this way this must become a variable
            //
            // PROGRAM P
            //    INTEGER, POINTER :: X    ! X is SK_UNDEFINED
            //    INTEGER, POINTER :: Y    ! Y is SK_UNDEFINED
            //    EXTERNAL :: Y            ! This will make Y a SK_FUNCTION
            //
            //    CALL S1(X) ! X must be a variable now
            //    CALL S2(Y)
            // END PROGRAM P
            //
            // Note that we cannot make it a variable in the declaration
            // because later an EXTERNAL might have turned it into a SK_FUNCTION 
            // (this is the case of 'Y' shown above)
            entry->kind = SK_VARIABLE;
            remove_unknown_kind_symbol(decl_context, entry);

            check_symbol_name_as_a_variable(sym, entry, decl_context, nodecl_output);
        }
        else
        {
            *nodecl_output = nodecl_make_symbol(entry, ast_get_locus(sym));
            nodecl_set_type(*nodecl_output, lvalue_ref(entry->type_information));
        }
    }
    else if (entry->kind == SK_FUNCTION)
    {
        *nodecl_output = nodecl_make_symbol(entry, ast_get_locus(sym));
        nodecl_set_type(*nodecl_output, lvalue_ref(entry->type_information));
    }
    else
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(sym));
    }
}


static void check_symbol_of_variable(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    // Entry will never be an intrinsic function
    scope_entry_t* entry = fortran_get_variable_with_locus(decl_context, expr, ASTText(expr));

    // When IMPLICIT NONE fortran_query_name_no_builtin_with_locus can return NULL
    if (entry == NULL)
    {
        error_printf_at(ast_get_locus(expr), "unknown entity '%s'\n",
                fortran_prettyprint_in_buffer(expr));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (entry->kind != SK_VARIABLE
             && entry->kind != SK_UNDEFINED)
    {
        error_printf_at(ast_get_locus(expr), "name '%s' is not valid in expression\n",
                entry->symbol_name);
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    entry->kind = SK_VARIABLE;
    remove_unknown_kind_symbol(decl_context, entry);

    // It might happen that dummy arguments/result do not have any implicit
    // type here (because the input code is wrong)
    if (is_error_type(entry->type_information))
    {
        error_printf_at(ast_get_locus(expr), "entity '%s' does not have any IMPLICIT type\n",
                fortran_prettyprint_in_buffer(expr));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    check_symbol_name_as_a_variable(expr, entry, decl_context, nodecl_output);
}

static void conform_types_in_assignment(type_t* lhs_type, type_t* rhs_type, type_t** conf_lhs_type, type_t** conf_rhs_type);

static char is_intrinsic_assignment(type_t* lvalue_type, type_t* rvalue_type)
{
    lvalue_type = no_ref(lvalue_type);
    rvalue_type = no_ref(rvalue_type);

    if (is_pointer_type(lvalue_type))
    {
        lvalue_type = pointer_type_get_pointee_type(lvalue_type);
    }
    if (is_pointer_type(rvalue_type))
    {
        rvalue_type = pointer_type_get_pointee_type(rvalue_type);
    }

    type_t* conf_lhs_type = NULL;
    type_t* conf_rhs_type = NULL;

    conform_types_in_assignment(lvalue_type, rvalue_type, &conf_lhs_type, &conf_rhs_type);

    if (is_enum_type(conf_lhs_type))
        conf_lhs_type = enum_type_get_underlying_type(conf_lhs_type);
    if (is_enum_type(conf_rhs_type))
        conf_rhs_type = enum_type_get_underlying_type(conf_rhs_type);

    if ((is_integer_type(conf_lhs_type)
                || is_floating_type(conf_lhs_type)
                || is_complex_type(conf_lhs_type))
            && (is_integer_type(conf_rhs_type)
                || is_floating_type(conf_rhs_type)
                || is_complex_type(conf_rhs_type)))
        return 1;

    if (fortran_is_character_type(conf_lhs_type)
            && fortran_is_character_type(conf_rhs_type)
            && equivalent_types(
                get_unqualified_type(array_type_get_element_type(conf_lhs_type)), 
                get_unqualified_type(array_type_get_element_type(conf_rhs_type)))) 
    {
        return 1;
    }

    if (is_bool_type(conf_lhs_type)
            && is_bool_type(conf_rhs_type))
        return 1;

    if (is_class_type(conf_lhs_type)
            && is_class_type(conf_rhs_type)
            && equivalent_types(
                get_unqualified_type(conf_lhs_type), 
                get_unqualified_type(conf_rhs_type)))
        return 1;

    return 0;
}

static char is_defined_assignment(AST expr, AST lvalue, 
        AST rvalue UNUSED_PARAMETER,
        nodecl_t nodecl_lvalue, nodecl_t nodecl_rvalue, 
        const decl_context_t* decl_context, 
        scope_entry_t** entry,
        scope_entry_t** generic_specifier_symbol)
{
    const char* operator_name = ".operator.=";
    scope_entry_list_t* call_list = fortran_query_name_str_for_function(decl_context, operator_name,
            ast_get_locus(expr));

    if (call_list == NULL)
        return 0;

    int num_actual_arguments = 2;
    nodecl_t nodecl_arguments[2] = {
        nodecl_make_fortran_actual_argument(nodecl_shallow_copy(nodecl_lvalue),
                nodecl_get_locus(nodecl_lvalue)),
        nodecl_make_fortran_actual_argument(nodecl_shallow_copy(nodecl_rvalue),
                nodecl_get_locus(nodecl_rvalue)) };

    type_t* result_type = NULL;

    AST operator_designation = ASTLeaf(AST_SYMBOL, ast_get_locus(lvalue), "=");

    diagnostic_context_push_buffered();
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol_list(call_list,
            decl_context,
            /* location */ expr,
            operator_designation,
            &num_actual_arguments,
            nodecl_arguments,
            /* is_call_stmt */ 1, // Assignments must be subroutines!
            // out
            &result_type,
            entry,
            generic_specifier_symbol,
            &nodecl_simplify);
    diagnostic_context_pop_and_discard();

    nodecl_free(nodecl_arguments[0]);
    nodecl_free(nodecl_arguments[1]);
    ast_free(operator_designation);

    return !is_error_type(result_type);
}

nodecl_t fortran_expression_as_value(nodecl_t expr)
{
    type_t* t = nodecl_get_type(expr);

    if (is_any_reference_type(t))
    {
        expr = nodecl_make_conversion(
                expr,
                no_ref(t),
                nodecl_get_locus(expr));
        t = no_ref(t);
    }

    if (is_pointer_type(t))
    {
        expr = nodecl_make_dereference(
                expr,
                lvalue_ref(pointer_type_get_pointee_type(t)),
                nodecl_get_locus(expr));
        return fortran_expression_as_value(expr);
    }

    return expr;
}

// Here variable is the Fortran meaning of variable: an lvalue of non-pointer type
nodecl_t fortran_expression_as_variable(nodecl_t expr)
{
    type_t* t = nodecl_get_type(expr);

    if (!is_any_reference_type(t))
    {
        return nodecl_make_err_expr(nodecl_get_locus(expr));
    }

    if (is_pointer_type(no_ref(t)))
    {
        expr = nodecl_make_conversion(
                expr,
                no_ref(t),
                nodecl_get_locus(expr));
        expr = nodecl_make_dereference(
                expr,
                lvalue_ref(pointer_type_get_pointee_type(no_ref(t))),
                nodecl_get_locus(expr));
        return fortran_expression_as_variable(expr);
    }

    return expr;
}


static void check_assignment(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST lvalue = ASTSon0(expr);
    AST rvalue = ASTSon1(expr);
    
    nodecl_t nodecl_lvalue = nodecl_null();
    fortran_check_expression_impl_(lvalue, decl_context, &nodecl_lvalue);
    if (nodecl_is_err_expr(nodecl_lvalue))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }
    type_t* lvalue_type = nodecl_get_type(nodecl_lvalue);

    nodecl_t nodecl_rvalue = nodecl_null();
    fortran_check_expression_impl_(rvalue, decl_context, &nodecl_rvalue);
    if (nodecl_is_err_expr(nodecl_rvalue))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }
    type_t* rvalue_type = nodecl_get_type(nodecl_rvalue);
    
    scope_entry_t* assignment_op = NULL;
    scope_entry_t* generic_specifier_symbol = NULL;
    char is_defined_assig = is_defined_assignment(expr,
            lvalue,
            rvalue,
            nodecl_lvalue,
            nodecl_rvalue,
            decl_context, 
            &assignment_op,
            &generic_specifier_symbol);

    if (!is_defined_assig
            && !is_intrinsic_assignment(lvalue_type, rvalue_type))
    {
        error_printf_at(ast_get_locus(expr), "cannot assign to a variable of type '%s' a value of type '%s'\n",
                fortran_print_type_str(lvalue_type),
                fortran_print_type_str(rvalue_type));
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (!is_defined_assig)
    {
        nodecl_lvalue = fortran_expression_as_variable(nodecl_lvalue);
        nodecl_rvalue = fortran_expression_as_value(nodecl_rvalue);

        if (!equivalent_types(
                    get_unqualified_type(no_ref(nodecl_get_type(nodecl_lvalue))), 
                    get_unqualified_type(nodecl_get_type(nodecl_rvalue))))
        {
            nodecl_rvalue = nodecl_make_conversion(nodecl_rvalue, 
                    lvalue_type,
                    nodecl_get_locus(nodecl_rvalue));
        }

        *nodecl_output = nodecl_make_assignment(nodecl_lvalue, nodecl_rvalue, lvalue_type, ast_get_locus(expr));
    }
    else
    {
        nodecl_t nodecl_generic_spec = nodecl_null();
        if (generic_specifier_symbol != NULL)
        {
            nodecl_generic_spec = nodecl_make_symbol(generic_specifier_symbol, ast_get_locus(expr));
        }

        nodecl_t nodecl_called = nodecl_make_symbol(assignment_op, ast_get_locus(expr));

        if (assignment_op->kind == SK_VARIABLE)
        {
            // This must be a pointer to function
            ERROR_CONDITION(!is_pointer_to_function_type(no_ref(assignment_op->type_information)), "Invalid symbol", 0);

            nodecl_called = nodecl_make_dereference(
                    nodecl_called,
                    lvalue_ref(assignment_op->type_information),
                    ast_get_locus(expr));
        }

        *nodecl_output = nodecl_make_function_call(
                nodecl_called,
                nodecl_make_list_2(
                    nodecl_make_fortran_actual_argument(
                        nodecl_lvalue,
                        nodecl_get_locus(nodecl_lvalue)),
                    nodecl_make_fortran_actual_argument(
                        nodecl_rvalue,
                        nodecl_get_locus(nodecl_rvalue))),
                nodecl_generic_spec,
                /* function_form */ nodecl_null(),
                // Fortran has not template arguments
                get_void_type(),
                ast_get_locus(expr));
    }

    if (nodecl_is_constant(nodecl_rvalue))
    {
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_rvalue));
    }
}

static void cast_initialization(
        type_t* initialized_type,
        const_value_t* init_constant, 

        const_value_t** casted_const,
        // This can be NULL but if it is not, it is updated
        nodecl_t* nodecl_output)
{
    // Now check the constant and the type do square, otherwise fix the
    // constant value
    const_value_t* val = init_constant;

    // The user is initializing an integer with another integer
    if (is_any_int_type(initialized_type)
            && const_value_is_integer(val))
    {
        // Ensure the integer constant is of the size the user declared
        *casted_const = const_value_cast_to_bytes(
                val,
                type_get_size(initialized_type),
                /* signed */ 1);
        if (nodecl_output != NULL)
        {
            *nodecl_output = nodecl_make_integer_literal(
                    initialized_type,
                    *casted_const,
                    nodecl_get_locus(*nodecl_output));
        }
    }
    // The user is initializing an integer using a float. 
    else if (is_any_int_type(initialized_type)
            && const_value_is_floating(val))
    {
        *casted_const = const_value_cast_to_bytes(
                val,
                type_get_size(initialized_type),
                /* signed */ 1);

        if (nodecl_output != NULL)
        {
            *nodecl_output = nodecl_make_integer_literal(
                    initialized_type,
                    *casted_const,
                    nodecl_get_locus(*nodecl_output));
        }
    }
    // The user is initializing a real using an integer
    else if (is_floating_type(initialized_type)
            && const_value_is_integer(val))
    {
        type_t* flt_type = initialized_type;

        const_value_t* flt_val = NULL;

        if (is_float_type(flt_type))
        {
            flt_val = const_value_cast_to_float_value(val);
        }
        else if (is_double_type(flt_type))
        {
            flt_val = const_value_cast_to_double_value(val);
        }
        else if (is_long_double_type(flt_type))
        {
            flt_val = const_value_cast_to_long_double_value(val);
        }
        else if (is_other_float_type(flt_type))
        {
#ifdef HAVE_QUADMATH_H
            if (floating_type_get_info(flt_type)->size_of == 16)
            {
                flt_val = const_value_cast_to_float128_value(val);
            }
#endif
        }

        ERROR_CONDITION(flt_val == NULL, "No conversion was possible", 0);

        *casted_const = flt_val;

        if (nodecl_output != NULL)
        {
            *nodecl_output = nodecl_make_floating_literal(
                    initialized_type,
                    *casted_const,
                    nodecl_get_locus(*nodecl_output));
        }
    }
    // The user is initializing a a complex type using either a float or an integer
    else if (is_complex_type(initialized_type)
            && (const_value_is_floating(val)
                || const_value_is_integer(val)))
    {
        // Cast real part
        const_value_t* real_part = NULL;
        nodecl_t nodecl_real_part = nodecl_null();
        if (nodecl_output != NULL)
        {
            nodecl_real_part = *nodecl_output;
        }
        cast_initialization(complex_type_get_base_type(initialized_type),
                val, &real_part,
                /* nodecl_output */ nodecl_is_null(nodecl_real_part) ? NULL : &nodecl_real_part);

        // Cast imag part (a 0 literal actually)
        const_value_t* imag_part = NULL;
        nodecl_t nodecl_imag_part = nodecl_null();
        if (nodecl_output != NULL)
        {
            nodecl_imag_part = nodecl_make_integer_literal(
                    fortran_get_default_integer_type(),
                    const_value_get_zero(fortran_get_default_integer_type_kind(), 1),
                    nodecl_get_locus(*nodecl_output));
        }
        cast_initialization(complex_type_get_base_type(initialized_type),
                const_value_get_zero(fortran_get_default_integer_type_kind(), 1),
                &imag_part,
                /* nodecl_output */ nodecl_is_null(nodecl_imag_part) ? NULL : &nodecl_imag_part);

        // Build a complex const
        *casted_const = const_value_make_complex(real_part, imag_part);

        if (nodecl_output != NULL)
        {
            // Build a nodecl if needed
            *nodecl_output = nodecl_make_complex_literal(
                    initialized_type,
                    *casted_const,
                    nodecl_get_locus(*nodecl_output));
        }
    }
    else if (fortran_is_character_type(initialized_type)
            && const_value_is_string(val))
    {
        *casted_const = val;

        nodecl_t size_init = array_type_get_array_size_expr(initialized_type);

        if (!nodecl_is_null(size_init)
                && nodecl_is_constant(size_init))
        {
            signed int n = const_value_cast_to_signed_int(nodecl_get_constant(size_init));

            ERROR_CONDITION(n <= 0, "Invalid length", 0);

            if (const_value_get_num_elements(val) < n)
            {
                char c[n];
                int s = const_value_get_num_elements(val);

                int i;
                for (i = 0; i < n; i++)
                {
                    if (i < s)
                    {
                        c[i] = (char)const_value_cast_to_signed_int(const_value_get_element_num(val, i));
                    }
                    else
                    {
                        c[i] = ' ';
                    }
                }

                *casted_const = const_value_make_string(c, n);
            }
        }

        if (nodecl_output != NULL)
        {
            // Build a nodecl if needed
            nodecl_t old_nodecl = *nodecl_output;
            *nodecl_output = const_value_to_nodecl(*casted_const);
            // Try hard to preserve the string literal...
            if (nodecl_get_kind(old_nodecl) == NODECL_STRING_LITERAL)
            {
                nodecl_set_text(*nodecl_output, nodecl_get_text(old_nodecl));
            }
        }
    }
    else if (fortran_is_array_type(initialized_type)
            && !const_value_is_array(val))
    {
        type_t* rank0_type = fortran_get_rank0_type(initialized_type);

        // Avoid transforming initializers of empty arrays
        if (fortran_array_has_zero_size(initialized_type))
        {
            *casted_const = val;
            if (nodecl_output != NULL)
            {
                *nodecl_output = const_value_to_nodecl_with_basic_type(*casted_const, rank0_type);
            }

            return;
        }

        nodecl_t nodecl_size = array_type_get_array_size_expr(initialized_type);
        if (nodecl_is_constant(nodecl_size))
        {
            int i;
            int size = const_value_cast_to_signed_int(nodecl_get_constant(nodecl_size));
            type_t* element_type = array_type_get_element_type(initialized_type);

            const_value_t* const_values[size + 1];
            for (i = 0 ; i < size; i++)
            {
                cast_initialization(element_type,
                        val,
                        &(const_values[i]),
                        /* nodecl_output */ NULL);

            }

            *casted_const = const_value_make_array(size, const_values);

            if (nodecl_output != NULL)
            {
                // We do not expand the full array to a tree, just we keep the original simple tree
                // and modify the const value of the scalar
                *nodecl_output = const_value_to_nodecl_with_basic_type(val, rank0_type);
                nodecl_set_constant(*nodecl_output, *casted_const);
            }
        }
    }
    else
    {
        if (is_floating_type(initialized_type)
                && const_value_is_floating(val))
        {
            *casted_const = const_value_cast_to_floating_type_value(val, initialized_type);

            if (nodecl_output != NULL)
            {
                *nodecl_output = nodecl_make_floating_literal(
                        initialized_type,
                        *casted_const,
                        nodecl_get_locus(*nodecl_output));
            }
        }
        else if (is_integer_type(initialized_type)
                && const_value_is_integer(val))
        {
            *casted_const = const_value_cast_to_bytes(val, type_get_size(initialized_type), /* sign */ 1);

            if (nodecl_output != NULL)
            {
                *nodecl_output = nodecl_make_integer_literal(
                        initialized_type,
                        *casted_const,
                        nodecl_get_locus(*nodecl_output));
            }
        }
        else
        {
            // No cast
            *casted_const = val;
        }
    }
}

void fortran_cast_initialization(
        scope_entry_t* entry,
        nodecl_t *nodecl_init)
{
    ERROR_CONDITION(nodecl_init == NULL, "Cannot be NULL here", 0);

    if (nodecl_is_null(*nodecl_init)
            || !nodecl_is_constant(*nodecl_init))
        return;

    const_value_t* casted_const = NULL;
    cast_initialization(no_ref(entry->type_information),
            nodecl_get_constant(*nodecl_init),
            &casted_const,
            nodecl_init);
}

void fortran_check_initialization(
        scope_entry_t* entry,
        AST expr, 
        const decl_context_t* decl_context, 
        char is_pointer_initialization,
        nodecl_t* nodecl_output)
{
    char ok = 1;
    if (symbol_is_parameter_of_function(entry, entry->decl_context->current_scope->related_entry))
    {
        error_printf_at(ast_get_locus(expr), "a dummy argument cannot have initializer\n");
        ok = 0;
    }

    if (!ok)
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    fortran_push_checking_initializer();
    fortran_check_expression(expr, decl_context, nodecl_output);
    fortran_pop_checking_initializer();

    if (nodecl_is_err_expr(*nodecl_output))
        return;

    if (is_pointer_initialization)
    {
        char wrong_ptr_init = 0;

        // Just check that is => NULL()
        scope_entry_t* function_called = NULL;
        if (nodecl_get_kind(*nodecl_output) != NODECL_FUNCTION_CALL
                || ((function_called = nodecl_get_symbol(nodecl_get_child(*nodecl_output, 0))) == NULL)
                || strcasecmp(function_called->symbol_name, "null") != 0
                || !symbol_entity_specs_get_is_builtin(function_called))
        {
            wrong_ptr_init = 1;
        }

        if (wrong_ptr_init)
        {
            error_printf_at(ast_get_locus(expr), "pointer initializer of '%s' is not '=> NULL()'\n",
                    entry->symbol_name);
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        }
    }
    else
    {
        // Check if the initializer is valid
        if (!is_intrinsic_assignment(entry->type_information, 
                    nodecl_get_type(*nodecl_output)))
        {
            error_printf_at(ast_get_locus(expr), "initializer '%s' of type '%s' is not valid to initialize '%s' of type '%s'\n",
                    codegen_to_str(*nodecl_output, nodecl_retrieve_context(*nodecl_output)),
                    fortran_print_type_str(nodecl_get_type(*nodecl_output)),
                    entry->symbol_name,
                    fortran_print_type_str(entry->type_information));
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }

        if (!nodecl_is_constant(*nodecl_output))
        {
            // FIXME -- ???
            // if (!is_const_qualified_type(no_ref(nodecl_get_type(*nodecl_output))))
            {
                error_printf_at(ast_get_locus(expr), "initializer '%s' is not a constant expression\n",
                        codegen_to_str(*nodecl_output, nodecl_retrieve_context(*nodecl_output)));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            }
            return;
        }

        const_value_t* casted_const = NULL;
        cast_initialization(no_ref(entry->type_information),
                nodecl_get_constant(*nodecl_output),
                &casted_const,
                nodecl_output);
    }
}

static void check_ptr_assignment(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    AST lvalue = ASTSon0(expr);
    AST rvalue = ASTSon1(expr);

    nodecl_t nodecl_lvalue = nodecl_null();
    // Special handling for array subscripts
    if (ASTKind(lvalue) == AST_ARRAY_SUBSCRIPT)
    {
        // A(1:, 2:) => ...
        nodecl_t nodecl_subscripted = nodecl_null();
        fortran_check_expression_impl_(ASTSon0(lvalue), decl_context, &nodecl_subscripted);
        type_t* subscripted_type = nodecl_get_type(nodecl_subscripted);

        if (fortran_is_array_type(no_ref(subscripted_type))
                || fortran_is_pointer_to_array_type(no_ref(subscripted_type)))
        {

            // This function returns an expression whose type is an array
            // instead of a pointer to array.  Thus, we have to fix it later!
            check_array_ref_(lvalue, decl_context, nodecl_subscripted, nodecl_subscripted, &nodecl_lvalue,
                    /* do_complete_array_ranks */ 0, /* require_lower_bound */ 1);

        }
        else
        {
            error_printf_at(ast_get_locus(expr), "subscripted left hand side of pointer assignment is not a pointer to array\n");
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }
    else if (ASTKind(lvalue) == AST_CLASS_MEMBER_ACCESS)
    {
        // X % A(1:, 2:) => ...

        // This function returns an expression whose type is an array
        // instead of a pointer to array. Thus, we have to fix it later!
        check_component_ref_(lvalue, decl_context, &nodecl_lvalue,
                /* do_complete_array_ranks */ 0,
                /* require_lower_bound */ 1);
    }
    else
    {
        // X => ...
        fortran_check_expression_impl_(lvalue, decl_context, &nodecl_lvalue);
    }

    if (nodecl_is_err_expr(nodecl_lvalue))
    {
        *nodecl_output = nodecl_lvalue;
        return;
    }

    nodecl_t nodecl_rvalue = nodecl_null();
    fortran_check_expression_impl_(rvalue, decl_context, &nodecl_rvalue);

    if (nodecl_is_err_expr(nodecl_rvalue))
    {
        *nodecl_output = nodecl_rvalue;
        return;
    }

    type_t* rvalue_type = nodecl_get_type(nodecl_rvalue);
    if (is_error_type(rvalue_type))
    {
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    scope_entry_t* lvalue_sym = fortran_data_ref_get_symbol(nodecl_lvalue);
    if (lvalue_sym == NULL
            || lvalue_sym->kind != SK_VARIABLE
            || !is_pointer_type(no_ref(lvalue_sym->type_information)))
    {
        error_printf_at(ast_get_locus(expr), "left hand of pointer assignment is not a POINTER variable\n");
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    char is_target = 0;
    char is_pointer = 0;
    scope_entry_t* rvalue_sym = fortran_data_ref_get_symbol(nodecl_rvalue);
    if (rvalue_sym != NULL)
    {
        nodecl_t auxiliar = nodecl_rvalue;

        // If a named type variable is declared target, all its fields are target too.
        while (1)
        {
            if (nodecl_get_kind(auxiliar) == NODECL_SYMBOL)
            {
                scope_entry_t* sym = nodecl_get_symbol(auxiliar);
                if (sym != NULL)
                {
                    is_target = is_target
                        || symbol_entity_specs_get_is_target(sym);
                    is_pointer = is_pointer
                        || is_pointer_type(no_ref(sym->type_information));
                }
                break;
            }
            else if (nodecl_get_kind(auxiliar) == NODECL_CLASS_MEMBER_ACCESS)
            {
                scope_entry_t* component = nodecl_get_symbol(nodecl_get_child(auxiliar, 1));
                if (component != NULL)
                {
                    is_target = is_target
                        || symbol_entity_specs_get_is_target(component);
                    is_pointer = is_pointer
                        || is_pointer_type(no_ref(component->type_information));
                }
                auxiliar = nodecl_get_child(auxiliar, 0);
            }
            else if (nodecl_get_kind(auxiliar) == NODECL_ARRAY_SUBSCRIPT)
            {
                auxiliar = nodecl_get_child(auxiliar, 0);
            }
            else if (nodecl_get_kind(auxiliar) == NODECL_DEREFERENCE)
            {
                auxiliar = nodecl_get_child(auxiliar, 0);
            }
            else if (nodecl_get_kind(auxiliar) == NODECL_CONVERSION)
            {
                auxiliar = nodecl_get_child(auxiliar, 0);
            }
            else
            {
                break;
            }
        }

        // if (nodecl_get_kind(auxiliar) == NODECL_CLASS_MEMBER_ACCESS)
        // {
        //     // We don't want the accessed fields, we want the variable
        //     while (nodecl_get_kind(auxiliar) == NODECL_CLASS_MEMBER_ACCESS)
        //     {
        //         scope_entry_t* component = nodecl_get_symbol(nodecl_get_child(auxiliar, 1));

        //         if (component != NULL
        //                 && is_pointer_type(component->type_information))
        //             is_transitively_a_pointer = 1;

        //         auxiliar = nodecl_get_child(auxiliar, 0);
        //     }

        //     scope_entry_t* sym = nodecl_get_symbol(auxiliar);
        //     if (sym != NULL)
        //     {
        //         if (symbol_entity_specs_get_is_target(sym))
        //             target_is_subobject_of_target = 1;
        //         if (is_pointer_type(no_ref(sym->type_information)))
        //             is_transitively_a_pointer = 1;
        //     }
        // }
    }

    if (rvalue_sym != NULL
            && rvalue_sym->kind == SK_VARIABLE)
    {
        if (!is_pointer
                && !is_target)
        {
            // If the variable is not a POINTER, not a TARGET or not a subobject of a TARGET, error
            error_printf_at(ast_get_locus(expr), "symbol name in right hand side of pointer assignment is not a POINTER or TARGET data-reference\n");
            *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
            return;
        }
    }
    else if (is_call_to_null(nodecl_rvalue, NULL))
    {
        // This is OK
    }
    // If the right part is not a symbol name, but an expression of type
    // POINTER (currently only possible if we call a FUNCTION returning
    // POINTER), then it must be have been derreferenced
    else if (!is_pointer_type(no_ref(nodecl_get_type(nodecl_rvalue))))
    {
        error_printf_at(ast_get_locus(expr), "right hand side of pointer assignment does not yield a POINTER data-reference\n");
        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
        return;
    }

    if (is_pointer_type(no_ref(nodecl_get_type(nodecl_rvalue))))
    {
        // lvalue pointer to rvalue pointer
        if (is_any_reference_type(nodecl_get_type(nodecl_rvalue)))
        {
            nodecl_rvalue = nodecl_make_conversion(
                    nodecl_rvalue,
                    no_ref(nodecl_get_type(nodecl_rvalue)),
                    nodecl_get_locus(nodecl_rvalue));
        }
    }
    else if (nodecl_get_kind(nodecl_rvalue) == NODECL_SYMBOL
            || nodecl_get_kind(nodecl_rvalue) == NODECL_CLASS_MEMBER_ACCESS
            || nodecl_get_kind(nodecl_rvalue) == NODECL_ARRAY_SUBSCRIPT)
    {
        // Build a reference here
        nodecl_rvalue = fortran_expression_as_variable(nodecl_rvalue);
        nodecl_rvalue = nodecl_make_reference(nodecl_rvalue,
                get_pointer_type(no_ref(rvalue_sym->type_information)),
                nodecl_get_locus(nodecl_rvalue));
    }
    else if (is_call_to_null(nodecl_rvalue, NULL))
    {
        // This is OK
    }
    else
    {
        internal_error("Code unreachable", 0);
    }

    if (nodecl_get_kind(nodecl_lvalue) == NODECL_ARRAY_SUBSCRIPT)
    {
        // Fixing the type of the lvalue expression. If we don't fix the type
        // of the expression, we will not generate a pointer assignment
        type_t* nodecl_lvalue_type = nodecl_get_type(nodecl_lvalue);
        nodecl_set_type(nodecl_lvalue, get_pointer_type(no_ref(nodecl_lvalue_type)));
    }

    *nodecl_output = nodecl_make_assignment(
            nodecl_lvalue,
            nodecl_rvalue,
            no_ref(lvalue_sym->type_information),
            ast_get_locus(expr));
}

static void disambiguate_expression(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    int num_ambig = ast_get_num_ambiguities(expr);

    int i;
    int correct_option = -1;

    struct { AST t; int idx; } prioritize[4] = { { NULL, 0 } };

    // Due to a glitch we have to check the ambiguities in a very specific order
    //
    // * function call
    // * array reference
    // * derived type constructors
    // * class member accesses
    //
    for (i = 0; i < num_ambig; i++)
    {
        AST current_expr = ast_get_ambiguity(expr, i);

        switch (ASTKind(current_expr))
        {
            case AST_FUNCTION_CALL:
                {
                    ERROR_CONDITION(prioritize[0].t != NULL, "More than one ambiguity!\n", 0);
                    prioritize[0].t = current_expr;
                    prioritize[0].idx = i;
                    break;
                }
            case AST_ARRAY_SUBSCRIPT:
                {
                    ERROR_CONDITION(prioritize[1].t != NULL, "More than one ambiguity!\n", 0);
                    prioritize[1].t = current_expr;
                    prioritize[1].idx = i;
                    break;
                }
            case AST_DERIVED_TYPE_CONSTRUCTOR:
                {
                    ERROR_CONDITION(prioritize[2].t != NULL, "More than one ambiguity!\n", 0);
                    prioritize[2].t = current_expr;
                    prioritize[2].idx = i;
                    break;
                }
            case AST_CLASS_MEMBER_ACCESS:
                {
                    ERROR_CONDITION(prioritize[3].t != NULL, "More than one ambiguity!\n", 0);
                    prioritize[3].t = current_expr;
                    prioritize[3].idx = i;
                    break;
                }
                break;
            default:
                {
                    internal_error("%s: unexpected node '%s'\n", 
                            ast_location(current_expr),
                            ast_print_node_type(ASTKind(current_expr)));
                    break;
                }
        }
    }

    diagnostic_context_t* ambig_diag[num_ambig + 1];
    nodecl_t nodecl_check_expr[num_ambig + 1];

    for (i = 0; i < num_ambig; i++)
    {
        ambig_diag[i] = NULL;
        nodecl_check_expr[i] = nodecl_null();
    }

    for (i = 0; i < (int)STATIC_ARRAY_LENGTH(prioritize); i++)
    {
        if (prioritize[i].t != NULL)
        {
            AST current_expr = prioritize[i].t;

            ambig_diag[prioritize[i].idx] = diagnostic_context_push_buffered();
            fortran_check_expression_impl_(current_expr, decl_context,
                    &(nodecl_check_expr[prioritize[i].idx]));
            diagnostic_context_pop();

            if (!nodecl_is_err_expr(nodecl_check_expr[prioritize[i].idx]))
            {
                // Use the first one that works
                correct_option = prioritize[i].idx;
                break;
            }
        }
    }

    for (i = 0; i < num_ambig; i++)
    {
        if (i == correct_option)
        {
            diagnostic_context_commit(ambig_diag[i]);
        }
        else
        {
            nodecl_free(nodecl_check_expr[i]);
            if (ambig_diag[i] != NULL)
            {
                if (correct_option < 0)
                {
                    diagnostic_context_commit(ambig_diag[i]);
                }
                else
                {
                    diagnostic_context_discard(ambig_diag[i]);
                }
            }
        }
    }

    if (correct_option < 0)
    {
        ERROR_CONDITION(prioritize[0].t == NULL && prioritize[3].t == NULL, 
                "Invalid ambiguity", 0);
        if (prioritize[3].t == NULL)
            correct_option = prioritize[0].idx;
        else
            correct_option = prioritize[3].idx;
        ast_replace_with_ambiguity(expr, correct_option);

        *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
    }
    else
    {
        ast_replace_with_ambiguity(expr, correct_option);
        *nodecl_output = nodecl_check_expr[correct_option];
    }
}

static type_t* common_kind(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    ERROR_CONDITION(!fortran_is_scalar_type(t1)
            || !fortran_is_scalar_type(t2), "One of the types is not scalar", 0);

    _size_t s1 = type_get_size(t1);
    _size_t s2 = type_get_size(t2);

    if (s1 > s2)
        return t1;
    else 
        return t2;
}

static type_t* first_type(type_t* t1, type_t* t2 UNUSED_PARAMETER)
{
    return t1;
}

static type_t* second_type(type_t* t1 UNUSED_PARAMETER, type_t* t2)
{
    return t2;
}

static type_t* combine_character_array(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    if (fortran_is_pointer_to_character_type(t1))
        t1 = pointer_type_get_pointee_type(t1);
    if (fortran_is_pointer_to_character_type(t2))
        t1 = pointer_type_get_pointee_type(t2);

    nodecl_t length1 = array_type_get_array_size_expr(t1);
    nodecl_t length2 = array_type_get_array_size_expr(t2);

    type_t* char1 = array_type_get_element_type(t1);
    type_t* char2 = array_type_get_element_type(t2);

    if (!equivalent_types(get_unqualified_type(char1), get_unqualified_type(char2)))
        return NULL;

    type_t* result;
    if (!nodecl_is_null(length1)
            && !nodecl_is_null(length2))
    {
        nodecl_t lower = nodecl_make_integer_literal(
                fortran_get_default_logical_type(), 
                const_value_get_signed_int(1), 
                make_locus("", 0, 0));
        nodecl_t upper;
        if (nodecl_is_constant(length1) 
                && nodecl_is_constant(length2))
        {
            upper = const_value_to_nodecl(
                    const_value_add(nodecl_get_constant(length1),
                        nodecl_get_constant(length2)));
        }
        else
        {
            upper = nodecl_make_add(
                    nodecl_shallow_copy(length1),
                    nodecl_shallow_copy(length2),
                    fortran_get_default_logical_type(),
                    make_locus("", 0, 0));
        }

        result = get_array_type_bounds(char1, 
                lower, 
                upper, 
                array_type_get_array_size_expr_context(t1));
    }
    else
    {
        result = get_array_type(char1, nodecl_null(), array_type_get_array_size_expr_context(t1));
    }

    return result;
}

static type_t* logical_type(type_t* t1 UNUSED_PARAMETER, type_t* t2 UNUSED_PARAMETER)
{
    return fortran_get_default_logical_type();
}

static char is_logical_type(type_t* t1)
{
    t1 = no_ref(t1);
    return is_bool_type(t1);
}

typedef
struct operand_types_tag
{
    char (*lhs_type)(type_t*);
    char (*rhs_type)(type_t*);
    type_t* (*common_type)(type_t*, type_t*);
    char convert_to_common;
} operand_types_t;

enum { DO_CONVERT_TO_RESULT = 1, DO_NOT_CONVERT_TO_RESULT = 0};

static operand_types_t arithmetic_unary[] = 
{
    { NULL, is_integer_type, second_type, DO_NOT_CONVERT_TO_RESULT },
    { NULL, is_floating_type, second_type, DO_NOT_CONVERT_TO_RESULT },
    { NULL, is_complex_type, second_type, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t arithmetic_binary[] =
{
    { is_integer_type, is_integer_type, common_kind, DO_CONVERT_TO_RESULT },
    { is_integer_type, is_floating_type,  second_type, DO_CONVERT_TO_RESULT },
    { is_integer_type, is_complex_type, second_type, DO_CONVERT_TO_RESULT },
    { is_floating_type, is_integer_type, first_type, DO_CONVERT_TO_RESULT },
    { is_floating_type, is_floating_type, common_kind, DO_CONVERT_TO_RESULT },
    { is_floating_type, is_complex_type, second_type, DO_CONVERT_TO_RESULT },
    { is_complex_type, is_integer_type, first_type, DO_CONVERT_TO_RESULT },
    { is_complex_type, is_floating_type, first_type, DO_CONVERT_TO_RESULT },
    { is_complex_type, is_complex_type, common_kind, DO_CONVERT_TO_RESULT },
};

static operand_types_t arithmetic_binary_power[] =
{
    { is_integer_type, is_integer_type, common_kind, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_floating_type,  second_type, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_complex_type, second_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_integer_type, first_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_floating_type, common_kind, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_complex_type, second_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_integer_type, first_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_floating_type, first_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_complex_type, common_kind, DO_NOT_CONVERT_TO_RESULT },
};


static operand_types_t concat_op[] = 
{
    { fortran_is_character_type_or_pointer_to, fortran_is_character_type_or_pointer_to, combine_character_array, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t relational_equality[] =
{
    { is_integer_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_floating_type,  logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_complex_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_floating_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_complex_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_floating_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_complex_type, is_complex_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { fortran_is_character_type, fortran_is_character_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t relational_weak[] =
{
    { is_integer_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_floating_type,  logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_floating_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { fortran_is_character_type, fortran_is_character_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t logical_unary[] =
{
    { NULL, is_logical_type, second_type, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t logical_binary[] =
{
    { is_logical_type, is_logical_type, common_kind, DO_NOT_CONVERT_TO_RESULT }
};

typedef struct operand_map_tag
{
    node_t node_type;
    operand_types_t* operand_types;
    int num_operands;

    const_value_t* (*compute_const)(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);

    const char* op_symbol_name;

    nodecl_t (*compute_nodecl)(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, type_t* t, const locus_t* locus);
} operand_map_t;

#define HANDLER_MAP(_node_op, _operands, _compute_const, _operator_symbol_name, _nodecl_fun) \
{ _node_op, _operands, sizeof(_operands) / sizeof(_operands[0]), _compute_const, _operator_symbol_name, _nodecl_fun }

static const_value_t* const_unary_plus(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_unary_neg(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_add(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_sub(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_mult(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_div(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_power(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_not_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_lt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_lte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_gt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_gte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_unary_not(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_and(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_or(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);
static const_value_t* const_bin_concat(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs);

#define NODECL_FUN_2BIN(x) binary_##x

#define NODECL_FUN_2BIN_DEF(x) \
static nodecl_t binary_##x(nodecl_t nodecl_lhs UNUSED_PARAMETER, nodecl_t nodecl_rhs, type_t* t, const locus_t* locus) \
{ \
    return x(nodecl_rhs, t, locus); \
}

static const_value_t* fortran_str_const_value_eq(const_value_t* v1, const_value_t* v2)
{
    if (const_value_get_num_elements(v1) != const_value_get_num_elements(v2))
    {
        return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
    }

    int i, N = const_value_get_num_elements(v1);
    for (i = 0; i < N; i++)
    {
        const_value_t* equal = const_value_eq(
                const_value_get_element_num(v1, i),
                const_value_get_element_num(v2, i));

        if (const_value_is_zero(equal))
            return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
    }

    return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
}

static const_value_t* fortran_str_const_value_neq(const_value_t* v1, const_value_t* v2)
{
    const_value_t* eq = fortran_str_const_value_eq(v1, v2);

    if (const_value_is_zero(eq))
        return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
    else
        return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
}

static const_value_t* fortran_str_const_value_lt(const_value_t* v1, const_value_t* v2)
{
    int i, N1, N2;

    N1 = const_value_get_num_elements(v1);
    N2 = const_value_get_num_elements(v2);

    int min = N1 < N2 ? N1 : N2;

    for (i = 0; i < min; i++)
    {
        const_value_t* equal = const_value_eq(
                const_value_get_element_num(v1, i),
                const_value_get_element_num(v2, i));

        if (const_value_is_zero(equal))
        {
            // It is not equal, is it lower?
            const_value_t* lt = const_value_lt(
                    const_value_get_element_num(v1, i),
                    const_value_get_element_num(v2, i));

            if (const_value_is_zero(lt))
            {
                return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
            }
            else
            {
                return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
            }
        }
    }

    if (N1 == N2)
        return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
    else
        return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
}

static const_value_t* fortran_str_const_value_lte(const_value_t* v1, const_value_t* v2)
{
    const_value_t* result = fortran_str_const_value_lt(v1, v2);

    // Not lower, might equal
    if (const_value_is_zero(result))
        result = fortran_str_const_value_eq(v1, v2);

    return result;
}

static const_value_t* fortran_str_const_value_gt(const_value_t* v1, const_value_t* v2)
{
    const_value_t* lte = fortran_str_const_value_lte(v1, v2);

    if (const_value_is_zero(lte))
        return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
    else
        return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
}

static const_value_t* fortran_str_const_value_gte(const_value_t* v1, const_value_t* v2)
{
    const_value_t* lte = fortran_str_const_value_lt(v1, v2);

    if (const_value_is_zero(lte))
        return const_value_get_one(fortran_get_default_integer_type_kind(), 1);
    else
        return const_value_get_zero(fortran_get_default_integer_type_kind(), 1);
}

#define NODECL_FORTRAN_RELATIONAL(name) \
static const_value_t* fortran_##name(const_value_t* v1, const_value_t* v2) \
{ \
    if (const_value_is_string(v1) && const_value_is_string(v2)) \
    { \
        return fortran_str_##name(v1, v2); \
    } \
    else \
    { \
        return name(v1, v2); \
    } \
}

NODECL_FORTRAN_RELATIONAL(const_value_eq)
NODECL_FORTRAN_RELATIONAL(const_value_neq)
NODECL_FORTRAN_RELATIONAL(const_value_lt)
NODECL_FORTRAN_RELATIONAL(const_value_lte)
NODECL_FORTRAN_RELATIONAL(const_value_gt)
NODECL_FORTRAN_RELATIONAL(const_value_gte)

NODECL_FUN_2BIN_DEF(nodecl_make_plus)
NODECL_FUN_2BIN_DEF(nodecl_make_neg)
NODECL_FUN_2BIN_DEF(nodecl_make_logical_not)

static operand_map_t operand_map[] =
{
    // Arithmetic unary
    HANDLER_MAP(AST_PLUS, arithmetic_unary, const_unary_plus, ".operator.+", NODECL_FUN_2BIN(nodecl_make_plus)),
    HANDLER_MAP(AST_NEG, arithmetic_unary, const_unary_neg, ".operator.-", NODECL_FUN_2BIN(nodecl_make_neg)),
    // Arithmetic binary
    HANDLER_MAP(AST_ADD, arithmetic_binary, const_bin_add, ".operator.+", nodecl_make_add),
    HANDLER_MAP(AST_MINUS, arithmetic_binary, const_bin_sub, ".operator.-", nodecl_make_minus),
    HANDLER_MAP(AST_MUL, arithmetic_binary, const_bin_mult, ".operator.*", nodecl_make_mul),
    HANDLER_MAP(AST_DIV, arithmetic_binary, const_bin_div, ".operator./", nodecl_make_div),
    HANDLER_MAP(AST_POWER, arithmetic_binary_power, const_bin_power, ".operator.**", nodecl_make_power),
    // String concat
    HANDLER_MAP(AST_CONCAT, concat_op, const_bin_concat, ".operator.//", nodecl_make_concat),
    // Relational strong
    HANDLER_MAP(AST_EQUAL, relational_equality, const_bin_equal, ".operator.==", nodecl_make_equal),
    HANDLER_MAP(AST_DIFFERENT, relational_equality, const_bin_not_equal, ".operator./=", nodecl_make_different),
    // Relational weak
    HANDLER_MAP(AST_LOWER_THAN, relational_weak, const_bin_lt, ".operator.<", nodecl_make_lower_than),
    HANDLER_MAP(AST_LOWER_OR_EQUAL_THAN, relational_weak, const_bin_lte, ".operator.<=", nodecl_make_lower_or_equal_than),
    HANDLER_MAP(AST_GREATER_THAN, relational_weak, const_bin_gt, ".operator.>", nodecl_make_greater_than),
    HANDLER_MAP(AST_GREATER_OR_EQUAL_THAN, relational_weak, const_bin_gte, ".operator.>=", nodecl_make_greater_or_equal_than),
    // Unary logical
    HANDLER_MAP(AST_LOGICAL_NOT, logical_unary, const_unary_not, ".operator..not.", NODECL_FUN_2BIN(nodecl_make_logical_not)),
    // Binary logical
    HANDLER_MAP(AST_LOGICAL_EQUAL, logical_binary, const_bin_equal, ".operator..eqv.", nodecl_make_equal),
    HANDLER_MAP(AST_LOGICAL_DIFFERENT, logical_binary, const_bin_not_equal, ".operator..neqv.", nodecl_make_different),
    HANDLER_MAP(AST_LOGICAL_AND, logical_binary, const_bin_and, ".operator..and.", nodecl_make_logical_and),
    HANDLER_MAP(AST_LOGICAL_OR, logical_binary, const_bin_or, ".operator..or.", nodecl_make_logical_or),
};
static char operand_map_init = 0;

static int compare_map_items(const void* a, const void* b)
{
    const operand_map_t* pa = (const operand_map_t*)a;
    const operand_map_t* pb = (const operand_map_t*)b;

    if (pa->node_type > pb->node_type)
        return 1;
    else if (pa->node_type < pb->node_type)
        return -1;
    else
        return 0;
}

static const char * get_operator_for_expr(AST expr);

static type_t* rerank_type(type_t* rank0_common, type_t* lhs_type, type_t* rhs_type);

static void conform_types(type_t* lhs_type, type_t* rhs_type, 
        type_t** conf_lhs_type, type_t** conf_rhs_type);

static type_t* adjust_type_for_intrinsic_operator(type_t* t)
{
    t = no_ref(t);

    if (is_pointer_type(t))
        t = pointer_type_get_pointee_type(t);

    return t;
}

static type_t* compute_result_of_intrinsic_operator(AST expr, const decl_context_t* decl_context, 
        nodecl_t nodecl_lhs,
        nodecl_t nodecl_rhs,
        nodecl_t* nodecl_output)
{
    type_t* adj_lhs_type = NULL;
    type_t* conf_lhs_type = NULL;

    type_t* adj_rhs_type = NULL;
    type_t* conf_rhs_type = NULL;

    if (!nodecl_is_null(nodecl_lhs))
    {
        adj_lhs_type = adjust_type_for_intrinsic_operator(nodecl_get_type(nodecl_lhs));
    }
    adj_rhs_type = adjust_type_for_intrinsic_operator(nodecl_get_type(nodecl_rhs));

    conform_types(adj_lhs_type, adj_rhs_type, &conf_lhs_type, &conf_rhs_type);

    if (!operand_map_init)
    {
        qsort(operand_map, 
                sizeof(operand_map) / sizeof(operand_map[0]), 
                sizeof(operand_map[0]),
                compare_map_items);

        operand_map_init = 1;
    }

    operand_map_t key = { .node_type = ASTKind(expr) };
    operand_map_t* value = (operand_map_t*)bsearch(&key, operand_map,
                sizeof(operand_map) / sizeof(operand_map[0]), 
                sizeof(operand_map[0]),
                compare_map_items);

    if (value == NULL)
    {
        internal_error("%s: unexpected expression '%s' for intrinsic operation\n", 
                ast_location(expr),
                fortran_prettyprint_in_buffer(expr));
    }

    type_t* result = NULL;
    char convert_to_common = 0;

    // Lookup an intrinsic operator that matches the LHS and RHS types (once conformed)
    operand_types_t* operand_types = value->operand_types;
    int i;
    for (i = 0; i < value->num_operands && result == NULL; i++)
    {
        if (((conf_lhs_type == NULL 
                        && operand_types[i].lhs_type == NULL)
                    || ((operand_types[i].lhs_type)(conf_lhs_type)))
                && ((operand_types[i].rhs_type)(conf_rhs_type)))
        {
            result = (operand_types[i].common_type)(conf_lhs_type, conf_rhs_type);
            convert_to_common = (operand_types[i].convert_to_common == DO_CONVERT_TO_RESULT);
            break;
        }
    }

    if (result == NULL)
    {
        // No intrinsic operator was found, try with a user defined one
        result = get_error_type();

        scope_entry_list_t* call_list = fortran_query_name_str_for_function(decl_context, value->op_symbol_name,
                ast_get_locus(expr));

        // Perform a resolution by means of a call check
        if (call_list != NULL)
        {
            int num_actual_arguments = 0;
            nodecl_t nodecl_arguments[2] = { nodecl_null(), nodecl_null() };
            if (adj_lhs_type == NULL)
            {
                num_actual_arguments = 1;
                nodecl_arguments[0] = nodecl_make_fortran_actual_argument(nodecl_rhs, nodecl_get_locus(nodecl_rhs));
            }
            else
            {
                num_actual_arguments = 2;
                nodecl_arguments[0] = nodecl_make_fortran_actual_argument(nodecl_lhs, nodecl_get_locus(nodecl_lhs));
                nodecl_arguments[1] = nodecl_make_fortran_actual_argument(nodecl_rhs, nodecl_get_locus(nodecl_rhs));
            }

            AST operator_designation = ASTLeaf(AST_SYMBOL, ast_get_locus(expr), get_operator_for_expr(expr));

            scope_entry_t* called_symbol = NULL;
            scope_entry_t* generic_specifier_symbol = NULL;
            nodecl_t nodecl_simplify = nodecl_null();
            check_called_symbol_list(call_list, 
                    decl_context, 
                    /* location */ expr, 
                    operator_designation,
                    &num_actual_arguments,
                    nodecl_arguments,
                    /* is_call_stmt */ 0,
                    // out
                    &result,
                    &called_symbol,
                    &generic_specifier_symbol,
                    &nodecl_simplify);

            ast_free(operator_designation);

            // Restore the rank of the common type
            if (!is_error_type(result))
            {
                nodecl_t nodecl_argument_list;
                if (nodecl_is_null(nodecl_lhs))
                {
                    nodecl_argument_list = nodecl_make_list_1(nodecl_arguments[0]);
                }
                else
                {
                    nodecl_argument_list = nodecl_make_list_2(nodecl_arguments[0], nodecl_arguments[1]);
                }

                nodecl_t nodecl_generic_spec = nodecl_null();
                if (generic_specifier_symbol != NULL)
                {
                    nodecl_generic_spec = nodecl_make_symbol(generic_specifier_symbol, ast_get_locus(expr));
                }

                nodecl_t nodecl_called = nodecl_make_symbol(called_symbol, ast_get_locus(expr));

                if (called_symbol->kind == SK_VARIABLE)
                {
                    // This must be a pointer to function
                    ERROR_CONDITION(!is_pointer_to_function_type(no_ref(called_symbol->type_information)), "Invalid symbol", 0);

                    nodecl_called = nodecl_make_dereference(
                            nodecl_called,
                            lvalue_ref(called_symbol->type_information),
                            ast_get_locus(expr));
                }

                *nodecl_output = nodecl_make_function_call(
                        nodecl_called,
                        nodecl_argument_list,
                        nodecl_generic_spec,
                        /* function_form */ nodecl_null(),
                        // Fortran has not template arguments
                        result,
                        ast_get_locus(expr));

#if 0
                if (is_pointer_type(no_ref(result)))
                {
                    *nodecl_output = nodecl_make_dereference(*nodecl_output, 
                            lvalue_ref(pointer_type_get_pointee_type(no_ref(result))),
                            ast_get_locus(expr));
                }
#endif

                if (!nodecl_is_null(nodecl_simplify)
                        && nodecl_is_constant(nodecl_simplify))
                {
                    nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_simplify));
                }
            }
        }

        if (is_error_type(result))
        {
            if (adj_lhs_type != NULL)
            {
                error_printf_at(ast_get_locus(expr), "invalid operand types %s and %s for intrinsic binary operator '%s'\n",
                        fortran_print_type_str(nodecl_get_type(nodecl_lhs)),
                        fortran_print_type_str(nodecl_get_type(nodecl_rhs)),
                        get_operator_for_expr(expr));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return get_error_type();
            }
            else
            {
                error_printf_at(ast_get_locus(expr), "invalid operand types %s for intrinsic unary operator '%s'\n",
                        fortran_print_type_str(nodecl_get_type(nodecl_rhs)),
                        get_operator_for_expr(expr));
                *nodecl_output = nodecl_make_err_expr(ast_get_locus(expr));
                return get_error_type();
            }
        }
    }
    else
    {
        // Restore the rank of the common type
        result = rerank_type(result, adj_lhs_type, adj_rhs_type);

        const_value_t* val = NULL;
        if (value->compute_const != NULL)
        {
            val = value->compute_const(nodecl_lhs, nodecl_rhs);
        }

        if (!nodecl_is_null(nodecl_lhs))
            nodecl_lhs = fortran_expression_as_value(nodecl_lhs);
        nodecl_rhs = fortran_expression_as_value(nodecl_rhs);

        ERROR_CONDITION(is_any_reference_type(result), "Invalid type", 0);

        // Keep the conversions
        if (convert_to_common)
        {
            if (!nodecl_is_null(nodecl_lhs)
                    && !equivalent_types(
                        get_unqualified_type(result), 
                        get_unqualified_type(nodecl_get_type(nodecl_lhs))))
            {
                nodecl_lhs = nodecl_make_conversion(nodecl_lhs, result, 
                        nodecl_get_locus(nodecl_lhs));
            }
            if (!equivalent_types(
                        get_unqualified_type(result), 
                        get_unqualified_type(nodecl_get_type(nodecl_rhs))))
            {
                nodecl_rhs = nodecl_make_conversion(nodecl_rhs, result, 
                        nodecl_get_locus(nodecl_rhs));
            }
        }

        *nodecl_output = value->compute_nodecl(nodecl_lhs, nodecl_rhs, result, ast_get_locus(expr));

        if (val != NULL)
        {
            nodecl_t nodecl_const_val = fortran_const_value_to_nodecl(val);
            if (!nodecl_is_null(nodecl_const_val))
            {
                nodecl_t nodecl_old = *nodecl_output;

                *nodecl_output = nodecl_const_val;
                nodecl_set_type(*nodecl_output, result);

                nodecl_set_locus_as(*nodecl_output, nodecl_old);
            }
        }

        nodecl_set_constant(*nodecl_output, val);
    }


    return result;
}

const char* operator_names[] =
{
    [AST_PLUS] = "+",
    [AST_NEG] = "-",
    [AST_ADD] = "+",
    [AST_MINUS] = "-",
    [AST_MUL] = "*",
    [AST_DIV] = "/",
    [AST_POWER] = "**",
    [AST_CONCAT] = "//",
    [AST_EQUAL] = "==",
    [AST_DIFFERENT] = "/=",
    [AST_LOWER_THAN] = "<",
    [AST_LOWER_OR_EQUAL_THAN] = "<=",
    [AST_GREATER_THAN] = ">",
    [AST_GREATER_OR_EQUAL_THAN] = ">=",
    [AST_LOGICAL_NOT] = ".NOT.",
    [AST_LOGICAL_EQUAL] = ".EQV.",
    [AST_LOGICAL_DIFFERENT] = ".NEQV.",
    [AST_LOGICAL_AND] = ".AND.",
    [AST_LOGICAL_OR] = ".OR.",
};

static const char * get_operator_for_expr(AST expr)
{
    return operator_names[ASTKind(expr)];
}

static void conform_types_(type_t* lhs_type, type_t* rhs_type, 
        type_t** conf_lhs_type, type_t** conf_rhs_type,
        char conform_only_lhs)
{
    lhs_type = no_ref(lhs_type);
    rhs_type = no_ref(rhs_type);

    if (!fortran_is_array_type(lhs_type)
            && !fortran_is_array_type(rhs_type))
    {
        *conf_lhs_type = lhs_type;
        *conf_rhs_type = rhs_type;
    }
    else if ((fortran_is_array_type(lhs_type)
                && !fortran_is_array_type(rhs_type))
            || (!conform_only_lhs
                && !fortran_is_array_type(lhs_type)
                && fortran_is_array_type(rhs_type)))
    {
        // One is array and the other is scalar
        *conf_lhs_type = fortran_get_rank0_type(lhs_type);
        *conf_rhs_type = fortran_get_rank0_type(rhs_type);
    }
    else 
    {
        // Both are arrays, they only conform if their rank (and ultimately its
        // shape but this is not always checkable) matches
        if (fortran_get_rank_of_type(lhs_type) == fortran_get_rank_of_type(rhs_type))
        {
            *conf_lhs_type = fortran_get_rank0_type(lhs_type);
            *conf_rhs_type = fortran_get_rank0_type(rhs_type);
        }
        else
        // Do not conform
        {
            *conf_lhs_type = lhs_type;
            *conf_rhs_type = rhs_type;
        }
    }
}

static void conform_types_in_assignment(type_t* lhs_type, type_t* rhs_type, 
        type_t** conf_lhs_type, type_t** conf_rhs_type)
{
    conform_types_(lhs_type, rhs_type, conf_lhs_type, conf_rhs_type,
            /* conform_only_left */ 1);
}

static void conform_types(type_t* lhs_type, type_t* rhs_type, 
        type_t** conf_lhs_type, type_t** conf_rhs_type)
{
    conform_types_(lhs_type, rhs_type, conf_lhs_type, conf_rhs_type,
            /* conform_only_left */ 0);
}

static type_t* rerank_type(type_t* rank0_common, type_t* lhs_type, type_t* rhs_type)
{
    lhs_type = no_ref(lhs_type);
    rhs_type = no_ref(rhs_type);

    ERROR_CONDITION(!fortran_is_scalar_type(rank0_common)
            && !fortran_is_character_type(rank0_common), "Invalid rank0 type", 0);

    if (fortran_is_array_type(lhs_type))
    {
        // They should have the same rank and shape so it does not matter very much which one we use, right?
        return fortran_rebuild_array_type(rank0_common, lhs_type);
    }
    else if (fortran_is_array_type(rhs_type))
    {
        return fortran_rebuild_array_type(rank0_common, rhs_type);
    }
    else
    {
        return rank0_common;
    }
}

static const_value_t* const_bin_val_(const_value_t* cval_lhs, const_value_t* cval_rhs,
        const_value_t* (*compute)(const_value_t*, const_value_t*))
{
    if (!const_value_is_array(cval_lhs)
            && !const_value_is_array(cval_rhs))
    {
        return compute(cval_lhs, cval_rhs);
    }
    else
    {
        int num_elements;
        if (const_value_is_array(cval_lhs))
        {
            num_elements = const_value_get_num_elements(cval_lhs);
            if (const_value_is_array(cval_rhs))
            {
                // Should not happen, though
                if (const_value_get_num_elements(cval_rhs) != num_elements)
                    return NULL;
            }
        }
        else
        {
            num_elements = const_value_get_num_elements(cval_rhs);
        }

        if (num_elements == 0)
            return const_value_make_array(0, NULL);

        const_value_t* cvals[num_elements];

        int k;
        for (k = 0; k < num_elements; k++)
        {
            const_value_t* current_lhs = cval_lhs;
            if (const_value_is_array(current_lhs))
                current_lhs = const_value_get_element_num(current_lhs, k);

            const_value_t* current_rhs = cval_rhs;
            if (const_value_is_array(current_rhs))
                current_rhs = const_value_get_element_num(current_rhs, k);

            cvals[k] = const_bin_val_(current_lhs, current_rhs, compute);

            if (cvals[k] == NULL)
                return NULL;
        }

        return const_value_make_array(num_elements, cvals);
    }
}

static const_value_t* const_bin_(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs,
        const_value_t* (*compute)(const_value_t*, const_value_t*))
{
    if (nodecl_is_constant(nodecl_lhs)
            && nodecl_is_constant(nodecl_rhs))
    {
        const_value_t* cval_lhs = nodecl_get_constant(nodecl_lhs);
        const_value_t* cval_rhs = nodecl_get_constant(nodecl_rhs);

        if (cval_lhs != NULL
                && cval_rhs != NULL)
        {
            return const_bin_val_(cval_lhs, cval_rhs, compute);
        }
    }
    return NULL;
}

static const_value_t* const_unary_val_(const_value_t* cval_lhs, const_value_t* (*compute)(const_value_t*))
{
    if (!const_value_is_array(cval_lhs))
    {
        return compute(cval_lhs);
    }
    else
    {
        int num_elements = const_value_get_num_elements(cval_lhs);

        if (num_elements == 0)
            return const_value_make_array(0, NULL);

        const_value_t* cvals[num_elements];
        int k;
        for (k = 0; k < num_elements; k++)
        {
            cvals[k] = const_unary_val_(const_value_get_element_num(cval_lhs, k), compute);
            if (cvals[k] == NULL)
                return NULL;
        }

        return const_value_make_array(num_elements, cvals);
    }
}

static const_value_t* const_unary_(nodecl_t nodecl_lhs, const_value_t* (*compute)(const_value_t*))
{
    if (nodecl_is_constant(nodecl_lhs))
    {
        const_value_t* cval_lhs = nodecl_get_constant(nodecl_lhs);
        if (cval_lhs != NULL)
            return const_unary_val_(cval_lhs, compute);
    }
    return NULL;
}

static const_value_t* const_unary_plus(nodecl_t nodecl_lhs UNUSED_PARAMETER, nodecl_t nodecl_rhs)
{
    return const_unary_(nodecl_rhs, const_value_plus);
}

static const_value_t* const_unary_neg(nodecl_t nodecl_lhs UNUSED_PARAMETER, nodecl_t nodecl_rhs)
{
    return const_unary_(nodecl_rhs, const_value_neg);
}

static const_value_t* const_bin_add(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_add);
}

static const_value_t* const_bin_sub(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_sub);
}

static const_value_t* const_bin_mult(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_mul);
}

static char const_value_is_zero_or_array_contains_zero(const_value_t* cval)
{
    if (const_value_is_array(cval))
    {
        int i;
        for (i = 0; i < const_value_get_num_elements(cval); i++)
        {
            return const_value_is_zero_or_array_contains_zero(
                    const_value_get_element_num(cval, i));
        }
        return false;
    }
    else return const_value_is_zero(cval);
}

static const_value_t* const_bin_div(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    if (nodecl_is_constant(nodecl_lhs)
            && nodecl_is_constant(nodecl_rhs))
    {
        const_value_t* lhs_value = nodecl_get_constant(nodecl_lhs);
        const_value_t* rhs_value = nodecl_get_constant(nodecl_rhs);

        if (const_value_is_zero_or_array_contains_zero(rhs_value))
        {
            error_printf_at(nodecl_get_locus(nodecl_lhs), "right hand side of intrinsic operator / cannot be a zero constant expression\n");
            return NULL;
        }

        return const_value_div(lhs_value, rhs_value);
    }
    return NULL;
}


static const_value_t* const_bin_power(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    if (nodecl_is_constant(nodecl_lhs)
            && nodecl_is_constant(nodecl_rhs))
    {
        const_value_t* lhs_value = nodecl_get_constant(nodecl_lhs);
        const_value_t* rhs_value = nodecl_get_constant(nodecl_rhs);

        if (!const_value_is_array(rhs_value)
                && !const_value_is_array(lhs_value)
                && const_value_is_floating(rhs_value)
                && const_value_is_negative(lhs_value))
        {
            error_printf_at(nodecl_get_locus(nodecl_lhs), "left hand side of intrinsic operator ** cannot be a negative constant expression\n");
            return NULL;
        }

        return const_value_pow(lhs_value, rhs_value);
    }
    return NULL;
}

static const_value_t* const_bin_concat(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_string_concat);
}

static const_value_t* const_bin_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_eq);
}

static const_value_t* const_bin_not_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_neq);
}

static const_value_t* const_bin_lt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_lt);
}

static const_value_t* const_bin_lte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_lte);
}

static const_value_t* const_bin_gt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_gt);
}

static const_value_t* const_bin_gte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, fortran_const_value_gte);
}

static const_value_t* const_unary_not(nodecl_t nodecl_lhs UNUSED_PARAMETER, nodecl_t nodecl_rhs)
{
    return const_unary_(nodecl_rhs, const_value_not);
}

static const_value_t* const_bin_and(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_and);
}

static const_value_t* const_bin_or(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_or);
}

type_t* common_type_of_binary_operation(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    if (is_pointer_type(t1))
        t1 = pointer_type_get_pointee_type(t1);
    if (is_pointer_type(t2))
        t2 = pointer_type_get_pointee_type(t2);

    if ((is_bool_type(t1) && is_bool_type(t2))
            || (is_integer_type(t1) && is_integer_type(t2))
            || (is_floating_type(t1) && is_floating_type(t2))
            || (is_complex_type(t1) && is_complex_type(t2)))
    {
        return common_kind(t1, t2);
    }
    else 
    {
        int i;
        int max = sizeof(arithmetic_binary) / sizeof(arithmetic_binary[0]);
        for (i = 0; i < max; i++)
        {
            if ((arithmetic_binary[i].lhs_type)(t1)
                    && (arithmetic_binary[i].rhs_type)(t2))
            {
                return  (arithmetic_binary[i].common_type)(t1, t2);
            }
        }
    }
    return NULL;
}

type_t* common_type_of_equality_operation(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    if (is_pointer_type(t1))
        t1 = pointer_type_get_pointee_type(t1);
    if (is_pointer_type(t2))
        t2 = pointer_type_get_pointee_type(t2);

    int i;
    int max = sizeof(relational_equality) / sizeof(relational_equality[0]);
    for (i = 0; i < max; i++)
    {
        if ((relational_equality[i].lhs_type)(t1)
                && (relational_equality[i].rhs_type)(t2))
        {
            return  (relational_equality[i].common_type)(t1, t2);
        }
    }
    return NULL;
}

static nodecl_t fortran_nodecl_adjust_function_argument(
        type_t* parameter_type,
        nodecl_t argument)
{
    ERROR_CONDITION(nodecl_get_kind(argument) != NODECL_FORTRAN_ACTUAL_ARGUMENT, "Invalid pointer access", 0);
    nodecl_t expr = nodecl_get_child(argument, 0);
    type_t* argument_type = nodecl_get_type(expr);

    if (is_lvalue_reference_type(parameter_type)
            && !is_lvalue_reference_type(argument_type))
    {
        // Passing a non-variable actual argument to a non-VALUE dummy argument
        // This would create a temporary
        const_value_t* cval = nodecl_get_constant(expr);
        expr = nodecl_make_conversion(
                expr, parameter_type, nodecl_get_locus(expr));
        nodecl_set_constant(expr, cval);
        nodecl_set_child(argument, 0, expr);
    }
    else if (!is_lvalue_reference_type(parameter_type)
            && is_lvalue_reference_type(argument_type))
    {
        // Passing a variable actual argument to a VALUE dummy argument
        const_value_t* cval = nodecl_get_constant(expr);
        expr = nodecl_make_conversion(
                expr, parameter_type, nodecl_get_locus(expr));
        nodecl_set_constant(expr, cval);
        nodecl_set_child(argument, 0, expr);
    }

    return argument;
}

static void multiexpression_check_range(AST range,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output)
{
    switch (ASTKind(range))
    {
        case AST_MULTIEXPRESSION_RANGE_SECTION: // lower : upper
            {
                AST lower = ASTSon0(range);
                nodecl_t nodecl_lower = nodecl_null();

                fortran_check_expression_impl_(lower, decl_context, &nodecl_lower);
                nodecl_lower = fortran_expression_as_value(nodecl_lower);
                if (nodecl_is_err_expr(nodecl_lower))
                {
                    *nodecl_output = nodecl_lower;
                    return;
                }

                AST upper = ASTSon1(range);
                nodecl_t nodecl_upper = nodecl_null();

                fortran_check_expression_impl_(upper, decl_context, &nodecl_upper);
                nodecl_upper = fortran_expression_as_value(nodecl_upper);
                if (nodecl_is_err_expr(nodecl_upper))
                {
                    *nodecl_output = nodecl_upper;
                    return;
                }

                nodecl_t nodecl_stride = nodecl_null();
                AST stride = ASTSon2(range);
                if (stride != NULL)
                {
                    fortran_check_expression_impl_(stride, decl_context, &nodecl_stride);
                    nodecl_stride = fortran_expression_as_value(nodecl_stride);
                    if (nodecl_is_err_expr(nodecl_stride))
                    {
                        *nodecl_output = nodecl_stride;
                        return;
                    }
                }
                else
                {
                    nodecl_stride = const_value_to_nodecl(const_value_get_signed_int(1));
                }

                *nodecl_output = nodecl_make_range(
                        nodecl_lower,
                        nodecl_upper,
                        nodecl_stride,
                        get_signed_int_type(),
                        ast_get_locus(range));

                if (nodecl_is_constant(nodecl_lower)
                        && nodecl_is_constant(nodecl_upper)
                        && nodecl_is_constant(nodecl_stride))
                {
                    nodecl_set_constant(
                            *nodecl_output,
                            const_value_make_range(
                                nodecl_get_constant(nodecl_lower),
                                nodecl_get_constant(nodecl_upper),
                                nodecl_get_constant(nodecl_stride)));
                }

                break;
            }
        case AST_MULTIEXPRESSION_RANGE_DISCRETE:
            {
                internal_error("Not yet implemented", 0);
                break;
            }
        default:
            internal_error("Unexpected node kind '%s'\n", ast_print_node_type(ASTKind(range)));
    }
}

static void check_multiexpression(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    const decl_context_t* iterator_context = new_block_context(decl_context);

    AST ompss_iterator = ASTSon1(expr);
    AST identifier = ASTSon0(ompss_iterator);
    AST range = ASTSon1(ompss_iterator);

    const char* iterator_name = strtolower(ASTText(identifier));

    scope_entry_t* new_iterator = new_symbol(iterator_context,
            iterator_context->current_scope,
            iterator_name);
    new_iterator->kind = SK_VARIABLE;
    new_iterator->type_information = get_signed_int_type();
    new_iterator->locus = ast_get_locus(ompss_iterator);

    nodecl_t nodecl_range = nodecl_null();
    multiexpression_check_range(range, iterator_context, &nodecl_range);

    if (nodecl_is_err_expr(nodecl_range))
    {
        *nodecl_output = nodecl_range;
        return;
    }

    nodecl_t nodecl_subexpr = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr),
            iterator_context,
            &nodecl_subexpr);

    if (nodecl_is_err_expr(nodecl_subexpr))
    {
        *nodecl_output = nodecl_subexpr;
        return;
    }

    *nodecl_output = nodecl_make_multi_expression(nodecl_range,
            nodecl_subexpr,
            new_iterator,
            nodecl_get_type(nodecl_subexpr),
            ast_get_locus(expr));
}
