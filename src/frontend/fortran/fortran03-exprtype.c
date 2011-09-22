/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

static void fortran_check_expression_impl_(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

char fortran_check_expression(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    fortran_check_expression_impl_(a, decl_context, nodecl_output);

    return !nodecl_is_err_expr(*nodecl_output);
}

typedef void (*check_expression_function_t)(AST statement, decl_context_t, nodecl_t* nodecl_output);
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
 STATEMENT_HANDLER(AST_IMAGE_REF, check_image_ref) \
 STATEMENT_HANDLER(AST_LOGICAL_AND, check_logical_and) \
 STATEMENT_HANDLER(AST_LOGICAL_EQUAL, check_logical_equal) \
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
 STATEMENT_HANDLER(AST_SYMBOL, check_symbol) \
 STATEMENT_HANDLER(AST_ASSIGNMENT, check_assignment) \
 STATEMENT_HANDLER(AST_PTR_ASSIGNMENT, check_ptr_assignment) \
 STATEMENT_HANDLER(AST_AMBIGUITY, disambiguate_expression) \
 STATEMENT_HANDLER(AST_USER_DEFINED_BINARY_OP, check_user_defined_binary_op) \

// Enable this if you really need extremely verbose typechecking
// #define VERBOSE_DEBUG_EXPR 1

#ifdef VERBOSE_DEBUG_EXPR
  // Prototypes
  #define STATEMENT_HANDLER(_kind, _handler) \
      static void _handler(AST, decl_context_t); \
      static void _handler##_(AST a, decl_context_t d) \
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
      static void _handler(AST, decl_context_t, nodecl_t* nodecl_output); 
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

#define RETURN_IF_ERROR_2(t1, t2, e) \
{ \
    if (is_error_type(t1) \
        || is_error_type(t2)) \
    { \
       *nodecl_output = nodecl_make_err_expr(ASTFileName(e), ASTLine(e)); \
       return; \
    }\
}

#define RETURN_IF_ERROR_1(t1, e) \
{ \
    if (is_error_type(t1)) \
    { \
       *nodecl_output = nodecl_make_err_expr(ASTFileName(e), ASTLine(e)); \
       return; \
    } \
}

static void fortran_check_expression_impl_(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    check_expression_handler_t key = { .ast_kind = ASTType(expression) };
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
        running_error("%s: sorry: unhandled expression %s\n", 
                ast_location(expression), 
                ast_print_node_type(ASTType(expression)));
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
            fprintf(stderr, "EXPRTYPE: %s: '%s' has type '%s' with a constant value of '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression),
                    print_declarator(nodecl_get_type(*nodecl_output)),
                    fortran_codegen_to_str(*nodecl_output));
        }
    }

    if (!checking_ambiguity() 
            && CURRENT_CONFIGURATION->strict_typecheck)
    {
        if (nodecl_get_type(*nodecl_output) == NULL
                || nodecl_is_err_expr(*nodecl_output))
        {
            internal_error("%s: invalid expression '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression));
        }
    }
}

static type_t* compute_result_of_intrinsic_operator(AST expr, decl_context_t, type_t* lhs_type, type_t* rhs_type, 
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output);

static void common_binary_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void common_unary_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);

static void check_add_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_ac_value_list(AST ac_value_list, decl_context_t decl_context, nodecl_t* nodecl_output, type_t** current_type)
{
    AST it;
    for_each_element(ac_value_list, it)
    {
        AST ac_value = ASTSon1(it);

        if (ASTType(ac_value) == AST_IMPLIED_DO)
        {
            AST implied_do_ac_value = ASTSon0(ac_value);

            AST implied_do_control = ASTSon1(ac_value);
            AST ac_do_variable = ASTSon0(implied_do_control);
            AST lower_bound = ASTSon1(implied_do_control);
            AST upper_bound = ASTSon2(implied_do_control);
            AST stride = ASTSon3(implied_do_control);

            nodecl_t nodecl_lower = nodecl_null();
            fortran_check_expression_impl_(lower_bound, decl_context, &nodecl_lower);
            nodecl_t nodecl_upper = nodecl_null();
            fortran_check_expression_impl_(upper_bound, decl_context, &nodecl_upper);
            nodecl_t nodecl_stride = nodecl_null();
            if (stride != NULL)
                fortran_check_expression_impl_(stride, decl_context, &nodecl_stride);

            scope_entry_t* do_variable = query_name_with_locus(decl_context, ac_do_variable, ASTText(ac_do_variable));

            if (do_variable == NULL)
            {
                running_error("%s: error: unknown symbol '%s' in ac-implied-do\n", ast_location(ac_do_variable), ASTText(ac_do_variable));
            }

            if (do_variable->kind == SK_UNDEFINED)
            {
                do_variable->kind = SK_VARIABLE;
            }
            else if (do_variable->kind != SK_VARIABLE)
            {
                running_error("%s: error: invalid name '%s' for ac-implied-do\n", ast_location(ac_do_variable), ASTText(ac_do_variable));
            }

            nodecl_t nodecl_ac_value = nodecl_null();
            check_ac_value_list(implied_do_ac_value, decl_context, &nodecl_ac_value, current_type);

            nodecl_t nodecl_implied_do = 
                nodecl_make_fortran_implied_do(
                        nodecl_make_symbol(do_variable, ASTFileName(ac_do_variable), ASTLine(ac_do_variable)),
                        nodecl_make_subscript_triplet(nodecl_lower, 
                            nodecl_upper, 
                            nodecl_stride, 
                            ASTFileName(implied_do_control), 
                            ASTLine(implied_do_control)),
                        nodecl_ac_value,
                        ASTFileName(implied_do_control), 
                        ASTLine(implied_do_control));

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_implied_do);
        }
        else
        {
            nodecl_t nodecl_expr = nodecl_null();
            fortran_check_expression_impl_(ac_value, decl_context, &nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
            {
                *nodecl_output = nodecl_expr;
                return;
            }
            else if (*current_type == NULL)
            {
                *current_type = nodecl_get_type(nodecl_expr);
            }

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_expr);
        }
    }
}

static void check_array_constructor(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST ac_spec = ASTSon0(expr);
    AST type_spec = ASTSon0(ac_spec);

    if (type_spec != NULL)
    {
        error_printf("%s: error: type specifier in array constructors not supported\n",
                ast_location(type_spec));
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST ac_value_list = ASTSon1(ac_spec);
    nodecl_t nodecl_ac_value = nodecl_null();
    type_t* ac_value_type = NULL;
    check_ac_value_list(ac_value_list, decl_context, &nodecl_ac_value, &ac_value_type);

    *nodecl_output = nodecl_make_structured_value(nodecl_ac_value,
            ac_value_type,
            ASTFileName(expr), ASTLine(expr));
}

static void check_substring(AST expr, decl_context_t decl_context, nodecl_t nodecl_subscripted, nodecl_t* nodecl_output)
{
    type_t* subscripted_type = no_ref(nodecl_get_type(nodecl_subscripted));

    AST subscript_list = ASTSon1(expr);

    int num_subscripts = 0;
    AST it;
    for_each_element(subscript_list, it)
    {
        num_subscripts++;
    }

    if (num_subscripts != 1)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: invalid number of subscripts (%d) in substring expression\n",
                    ast_location(expr),
                    num_subscripts);
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST subscript = ASTSon1(subscript_list);

    AST lower = ASTSon0(subscript);
    AST upper = ASTSon1(subscript);
    AST stride = ASTSon2(subscript);

    if (stride != NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: a stride is not valid in a substring expression\n",
                    ast_location(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    nodecl_t nodecl_lower = nodecl_null();
    if (lower != NULL)
        fortran_check_expression_impl_(lower, decl_context, &nodecl_lower);

    nodecl_t nodecl_upper = nodecl_null();
    if (upper != NULL)
        fortran_check_expression_impl_(upper, decl_context, &nodecl_upper);

    type_t* synthesized_type = NULL;

    // Do not compute the exact size at the moment
    synthesized_type = get_array_type_bounds(array_type_get_element_type(subscripted_type), nodecl_lower, nodecl_upper, decl_context);

    *nodecl_output = nodecl_make_array_subscript(
            nodecl_subscripted,
            nodecl_make_list_1(nodecl_make_subscript_triplet(nodecl_lower, nodecl_upper, nodecl_null(), ASTFileName(expr), ASTLine(expr))),
            synthesized_type,
            ASTFileName(expr), ASTLine(expr));
    nodecl_set_symbol(*nodecl_output, nodecl_get_symbol(nodecl_subscripted));
}

static void check_array_ref_(AST expr, decl_context_t decl_context, nodecl_t nodecl_subscripted, nodecl_t* nodecl_output)
{
    char symbol_is_invalid = 0;

    type_t* array_type = NULL;
    type_t* synthesized_type = NULL;

    int rank_of_type = -1;

    scope_entry_t* symbol = nodecl_get_symbol(nodecl_subscripted);
    if (symbol == NULL
            || (!is_array_type(no_ref(symbol->type_information))
                && !is_pointer_to_array_type(no_ref(symbol->type_information))))
    {
        symbol_is_invalid = 1;
    }
    else
    {
        array_type = no_ref(symbol->type_information);
        if (is_pointer_to_array_type(no_ref(symbol->type_information)))
            array_type = pointer_type_get_pointee_type(no_ref(symbol->type_information));

        synthesized_type = get_rank0_type(array_type);
        rank_of_type = get_rank_of_type(array_type);
    }

    AST subscript_list = ASTSon1(expr);

    int num_subscripts = 0;
    AST it;
    for_each_element(subscript_list, it)
    {
        num_subscripts++;
    }

    nodecl_t nodecl_indexes[num_subscripts];
    int i;
    for (i = 0; i < num_subscripts; i++)
    {
        nodecl_indexes[i] = nodecl_null();
    }

    num_subscripts = 0;
    for_each_element(subscript_list, it)
    {
        AST subscript = ASTSon1(it);

        if (ASTType(subscript) == AST_SUBSCRIPT_TRIPLET)
        {
            AST lower = ASTSon0(subscript);
            AST upper = ASTSon1(subscript);
            AST stride = ASTSon2(subscript);
            nodecl_t nodecl_lower = nodecl_null();
            nodecl_t nodecl_upper = nodecl_null();
            nodecl_t nodecl_stride = nodecl_null();
            if (lower != NULL)
                fortran_check_expression_impl_(lower, decl_context, &nodecl_lower);
            if (upper != NULL)
                fortran_check_expression_impl_(upper, decl_context, &nodecl_upper);
            if (stride != NULL)
                fortran_check_expression_impl_(stride, decl_context, &nodecl_stride);

            // Do not attempt to compute at the moment the sizes of the bounds
            // maybe we will in the future
            if (!symbol_is_invalid)
            {
                // FIXME - Stride may imply an array with smaller sizer (rank is unaffected)
                synthesized_type = get_array_type_bounds(synthesized_type, nodecl_lower, nodecl_upper, decl_context);
            }

            nodecl_indexes[num_subscripts] = nodecl_make_subscript_triplet(
                    nodecl_lower,
                    nodecl_upper,
                    nodecl_stride,
                    ASTFileName(subscript),
                    ASTLine(subscript));
        }
        else
        {
            fortran_check_expression_impl_(subscript, decl_context, &nodecl_indexes[num_subscripts]);
        }
        num_subscripts++;
    }

    if (symbol_is_invalid)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: data reference '%s' does not designate an array name\n",
                    ast_location(expr), fortran_prettyprint_in_buffer(ASTSon0(expr)));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    if (num_subscripts != rank_of_type)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: mismatch in subscripts of array reference, expecting %d got %d\n",
                    ast_location(expr),
                    get_rank_of_type(symbol->type_information),
                    num_subscripts);
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }


    nodecl_t nodecl_list = nodecl_null();
    for (i = num_subscripts-1; i >= 0; i--)
    {
        nodecl_list = nodecl_append_to_list(nodecl_list, nodecl_indexes[i]);
    }

    *nodecl_output = nodecl_make_array_subscript(nodecl_subscripted, 
            nodecl_list,
            synthesized_type,
            ASTFileName(expr),
            ASTLine(expr));
    nodecl_set_symbol(*nodecl_output, nodecl_get_symbol(nodecl_subscripted));
}

static void check_array_ref(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_subscripted = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_subscripted);

    if (nodecl_is_err_expr(nodecl_subscripted))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    type_t* subscripted_type = nodecl_get_type(nodecl_subscripted);

    if (is_fortran_array_type(no_ref(subscripted_type))
            || is_pointer_to_fortran_array_type(no_ref(subscripted_type)))
    {
        check_array_ref_(expr, decl_context, nodecl_subscripted, nodecl_output);
        return;
    }
    else if (is_fortran_character_type(get_rank0_type(no_ref(subscripted_type)))
            || is_pointer_to_fortran_character_type(get_rank0_type(no_ref(subscripted_type))))
    {
        check_substring(expr, decl_context, nodecl_subscripted, nodecl_output);
        return;
    }

    if (!checking_ambiguity())
    {
        error_printf("%s: error: invalid entity '%s' for subscript expression\n",
                ast_location(expr),
                fortran_prettyprint_in_buffer(ASTSon0(expr)));
    }
    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
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

    long long int value = strtoll(literal_text, NULL, base);

    const_value_t* const_value = const_value_get_signed_int(value);


    *nodecl_output = nodecl_make_integer_literal(fortran_get_default_logical_type(), const_value, ASTFileName(expr), ASTLine(expr));
}


static void check_binary_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    compute_boz_literal(expr, "b", 2, nodecl_output);
}

static void check_boolean_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    const_value_t* const_value = NULL;
    if (strcasecmp(ASTText(expr), ".true.") == 0)
    {
        const_value = const_value_get_one(1, 1);
    }
    else if (strcasecmp(ASTText(expr), ".false.") == 0)
    {
        const_value = const_value_get_zero(1, 1);
    }
    else
    {
        internal_error("Invalid boolean literal", 0);
    }

    *nodecl_output = nodecl_make_boolean_literal(fortran_get_default_logical_type(), const_value, 
            ASTFileName(expr), ASTLine(expr));
}

static void check_complex_literal(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // Const value does not support yet complex numbers, simply compute its
    // type
    AST real_part = ASTSon0(expr);
    AST imag_part = ASTSon1(expr);

    nodecl_t nodecl_real = nodecl_null();
    fortran_check_expression_impl_(real_part, decl_context, &nodecl_real);
    if (nodecl_is_err_expr(nodecl_real))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    nodecl_t nodecl_imag = nodecl_null();
    fortran_check_expression_impl_(imag_part, decl_context, &nodecl_imag);
    if (nodecl_is_err_expr(nodecl_imag))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    type_t* real_part_type = no_ref(nodecl_get_type(nodecl_real));
    type_t* imag_part_type = no_ref(nodecl_get_type(nodecl_imag));

    type_t* result_type = NULL;
    if (is_integer_type(real_part_type)
            && is_integer_type(imag_part_type))
    {
        result_type = get_complex_type(get_float_type());
    }
    else if (is_floating_type(real_part_type)
            || is_floating_type(imag_part_type))
    {
        type_t* element_type = NULL;
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
        if (!checking_ambiguity())
        {
            error_printf("%s: error: invalid complex constant '%s'\n", 
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }


    *nodecl_output = nodecl_make_complex_literal(
            nodecl_real, nodecl_imag, 
            result_type,
            ASTFileName(expr), ASTLine(expr));
}

static void check_component_ref(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_base = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_base);

    if (nodecl_is_err_expr(nodecl_base))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    type_t* t = no_ref(nodecl_get_type(nodecl_base));

    if (!is_pointer_to_class_type(t)
            && !is_class_type(t))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' does not denote a derived type\n",
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(ASTSon0(expr)));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    type_t* class_type = t;
    if (is_pointer_to_class_type(class_type))
    {
        class_type = pointer_type_get_pointee_type(t);
    }

    decl_context_t class_context = class_type_get_inner_context(get_actual_class_type(class_type));

    const char* field = ASTText(ASTSon1(expr));
    scope_entry_t* entry = query_name_in_class(class_context, field);

    if (entry == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' is not a component of '%s'\n",
                    ast_location(expr),
                    field,
                    fortran_print_type_str(class_type));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }



    if (is_pointer_to_class_type(t))
    {
    }
    else
    {
    }

    *nodecl_output = nodecl_make_class_member_access(nodecl_base, 
            nodecl_make_symbol(entry, ASTFileName(ASTSon1(expr)), ASTLine(ASTSon1(expr))),
            entry->type_information,
            ASTFileName(expr), ASTLine(expr));
    nodecl_set_symbol(*nodecl_output, entry);
}

static void check_concat_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
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

static int compute_kind_from_literal(const char* p, AST expr, decl_context_t decl_context)
{
    if (kind_is_integer_literal(p))
    {
        return atoi(p);
    }
    else
    {
        scope_entry_t* sym = query_name_with_locus(decl_context, expr, p);
        if (sym == NULL
                || sym->kind != SK_VARIABLE
                || !is_const_qualified_type(no_ref(sym->type_information)))
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: invalid kind '%s'\n", 
                        ast_location(expr), 
                        p);
            }
            return 0;
        }

        ERROR_CONDITION(nodecl_is_null(sym->value),
                "Invalid constant for kind '%s'", sym->symbol_name);

        ERROR_CONDITION(!nodecl_is_constant(sym->value),
                "Invalid nonconstant expression for kind '%s'", 
                fortran_codegen_to_str(sym->value));

        return const_value_cast_to_4(nodecl_get_constant(sym->value));
    }
}

static void check_decimal_literal(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    int kind = 4;
    if (*p == '_')
    {
        p++;
        kind = compute_kind_from_literal(p, expr, decl_context);
        if (kind == 0)
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
            return;
        }
    }

    long long int value = strtoll(decimal_text, NULL, 10);

    const_value_t* const_value = const_value_get_integer(value, kind, 1);
    nodecl_t nodecl_fake = nodecl_make_text(decimal_text, ASTFileName(expr), ASTLine(expr));
    type_t* t = choose_int_type_from_kind(nodecl_fake, 
            kind);


    *nodecl_output = nodecl_make_integer_literal(t, const_value, 
            ASTFileName(expr), ASTLine(expr));
}

static void check_derived_type_constructor(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST derived_type_spec = ASTSon0(expr);
    AST component_spec_list = ASTSon1(expr);

    AST type_param_spec_list = ASTSon1(derived_type_spec);
    if (type_param_spec_list != NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: sorry: derived types with type parameters not supported\n", ast_location(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST derived_name = ASTSon0(derived_type_spec);
    scope_entry_t* entry = query_name_with_locus(decl_context, derived_name, ASTText(derived_name));

    if (entry == NULL
            || entry->kind != SK_CLASS)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' is not a derived-type-name\n",
                    ast_location(expr),
                    ASTText(derived_name));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    // decl_context_t class_context = class_type_get_inner_context(entry->type_information);

    nodecl_t nodecl_initializer_list = nodecl_null();

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

            scope_entry_t* member = NULL;
            if (component_name == NULL)
            {
                if (member_index < 0)
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: component specifier at position %d lacks a component name", ast_location(component_spec),
                                component_position);
                    }
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                    return;
                }

                int i;
                scope_entry_list_t* nonstatic_data_members = class_type_get_nonstatic_data_members(entry->type_information);

                if (member_index > entry_list_size(nonstatic_data_members))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: too many specifiers in derived-type constructor\n", ast_location(component_spec));
                    }
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                    return;
                }

                scope_entry_list_iterator_t* iter = entry_list_iterator_begin(nonstatic_data_members);
                for (i = 0; i < member_index; i++)
                {
                    entry_list_iterator_next(iter);
                }
                member = entry_list_iterator_current(iter);

                entry_list_iterator_free(iter);
                entry_list_free(nonstatic_data_members);

                member_index++;
            }
            else
            {
                decl_context_t class_context = class_type_get_inner_context(get_actual_class_type(entry->type_information));

                const char* field = ASTText(component_name);
                member = query_name_in_class(class_context, field);
                if (member == NULL)
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: component specifier '%s' is not a component of '%s'\n",
                                ast_location(expr),
                                field,
                                fortran_print_type_str(entry->type_information));
                    }
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                    return;
                }

                member_index = -1;
            }


            nodecl_t nodecl_expr = nodecl_null();
            fortran_check_expression_impl_(component_data_source, decl_context, &nodecl_expr);

            nodecl_t nodecl_field = nodecl_make_symbol(member, ASTFileName(component_spec), ASTLine(component_spec));
            nodecl_t nodecl_field_designator = nodecl_make_field_designator(nodecl_field, nodecl_expr, 
                    ASTFileName(component_spec), ASTLine(component_spec));

            nodecl_initializer_list = nodecl_append_to_list(nodecl_initializer_list, nodecl_field_designator);

            component_position++;
        }
    }


    *nodecl_output = nodecl_make_structured_value(nodecl_initializer_list, 
            entry->type_information, 
            ASTFileName(expr), ASTLine(expr));
}

static void check_different_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_div_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_equal_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_floating_literal(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
   char* floating_text = strdup(strtolower(ASTText(expr)));

   unsigned int kind = 4;
   char *q = NULL; 
   if ((q = strchr(floating_text, '_')) != NULL)
   {
       *q = '\0';
       q++;
       kind = compute_kind_from_literal(q, expr, decl_context);
       if (kind == 0)
       {
           *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
           return;
       }
   }
   else if ((q = strchr(floating_text, 'd')) != NULL)
   {
       *q = '\0';
       kind = 8;
   }

    nodecl_t nodecl_fake = nodecl_make_text(floating_text, ASTFileName(expr), ASTLine(expr));
   type_t* t = choose_float_type_from_kind(nodecl_fake, kind);


   const_value_t *value = NULL;
   if (kind == (floating_type_get_info(get_float_type())->bits / 8))
   {
       float f = strtof(floating_text, NULL);
       value = const_value_get_float(f);
   }
   else if (kind == (floating_type_get_info(get_double_type())->bits / 8))
   {
       double d = strtod(floating_text, NULL);
       value = const_value_get_double(d);
   }
   else if (kind == (floating_type_get_info(get_long_double_type())->bits / 8))
   {
       long double ld = strtold(floating_text, NULL);
       value = const_value_get_long_double(ld);
   }
   else if (is_other_float_type(t))
   {
#ifdef HAVE_QUADMATH_H
       // __float128
       if (kind == 16)
       {
           __float128 f128 = strtoflt128(floating_text, NULL);
           value = const_value_get_float128(f128);
       }
       else
#endif
       {
           running_error("%s: error: literals of KIND=%d not supported yet\n", ast_location(expr), kind);
       }
   }
   else
   {
       running_error("Code unreachable, invalid floating literal", 0);
   }

   *nodecl_output = nodecl_make_floating_literal(t, value, ASTFileName(expr), ASTLine(expr));

   free(floating_text);
}

typedef
struct actual_argument_info_tag
{
    const char* keyword;
    type_t* type;
    char not_present;
} actual_argument_info_t;

static scope_entry_t* get_specific_interface(scope_entry_t* symbol, 
        int num_arguments, 
        const char **keyword_names,
        nodecl_t* nodecl_actual_arguments)
{
    scope_entry_t* result = NULL;
    int k;
    for (k = 0; k < symbol->entity_specs.num_related_symbols; k++)
    {
        scope_entry_t* specific_symbol = symbol->entity_specs.related_symbols[k];

        char ok = 1;

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
        memset(argument_types, 0, sizeof(argument_types));

        int i;
        for (i = 0; (i < num_arguments) && ok; i++)
        {
            int position = -1;
            if (keyword_names[i] == NULL)
            {
                position = i;
            }
            else
            {
                int j;
                for (j = 0; j < specific_symbol->entity_specs.num_related_symbols; j++)
                {
                    scope_entry_t* related_sym = specific_symbol->entity_specs.related_symbols[j];

                    if (!related_sym->entity_specs.is_parameter)
                        continue;

                    if (strcasecmp(related_sym->symbol_name, keyword_names[i]) == 0)
                    {
                        position = related_sym->entity_specs.parameter_position;
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
            argument_types[position].type = nodecl_get_type(nodecl_actual_arguments[i]);
        }

        if (!ok)
            continue;

        // Now complete with the optional ones
        for (i = 0; (i < specific_symbol->entity_specs.num_related_symbols) && ok; i++)
        {
            scope_entry_t* related_sym = specific_symbol->entity_specs.related_symbols[i];

            if (related_sym->entity_specs.is_parameter)
            {
                if (argument_types[related_sym->entity_specs.parameter_position].type == NULL)
                {
                    if (related_sym->entity_specs.is_optional)
                    {
                        argument_types[related_sym->entity_specs.parameter_position].type = related_sym->type_information;
                        argument_types[related_sym->entity_specs.parameter_position].not_present = 1;
                        num_arguments++;
                    }
                    else 
                    {
                        ok = 0;
                        break;
                    }
                }
            }
        }

        if (!ok)
            continue;

        if (num_arguments != function_type_get_num_parameters(specific_symbol->type_information))
            continue;

        // Now check that every type matches, otherwise error
        for (i = 0; (i < num_arguments) && ok; i++)
        {
            type_t* formal_type = function_type_get_parameter_type_num(specific_symbol->type_information, i);
            type_t* real_type = argument_types[i].type;

            // Note that for ELEMENTAL some more checks should be done
            if (specific_symbol->entity_specs.is_elemental)
            {
                real_type = get_rank0_type(real_type);
            }

            if (!equivalent_tkr_types(formal_type, real_type))
            {
                ok = 0;
                break;
            }
        }

        if (ok)
        {
            if (result == NULL)
            {
                result = specific_symbol;
            }
            else
            {
                // More than one match, ambiguity detected
                return NULL;
            }
        }
    }

    return result;
}

static char inside_context_of_symbol(decl_context_t decl_context, scope_entry_t* entry)
{
    scope_t* sc = decl_context.current_scope;
    while (sc != NULL)
    {
        if (sc->related_entry == entry)
            return 1;
        sc = sc->contained_in;
    }
    return 0;
}

static void check_called_symbol(
        scope_entry_t* symbol, 
        decl_context_t decl_context, 
        AST location,
        AST procedure_designator,
        int num_actual_arguments,
        nodecl_t* nodecl_actual_arguments,
        const char** actual_arguments_keywords,
        char is_call_stmt,
        // out
        type_t** result_type,
        scope_entry_t** called_symbol,
        nodecl_t* nodecl_simplify)
{
    if (symbol != NULL
            && symbol->kind == SK_VARIABLE
            && symbol->entity_specs.is_implicit_basic_type)
    {
        // Upgrade the symbol to a function with unknown arguments
        symbol->kind = SK_FUNCTION;

        type_t* return_type = symbol->type_information;
        if (is_call_stmt)
            return_type = NULL;

        symbol->type_information = get_nonproto_function_type(return_type, 0);
        symbol->entity_specs.is_implicit_basic_type = 0;
    }

    if (symbol == NULL
            || symbol->kind != SK_FUNCTION)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: in %s, '%s' does not designate a procedure\n",
                    ast_location(location),
                    !is_call_stmt ? "function reference" : "CALL statement",
                    fortran_prettyprint_in_buffer(procedure_designator));
        }
        *result_type = get_error_type();
        return;
    }

    if (inside_context_of_symbol(decl_context, symbol)
            && !symbol->entity_specs.is_recursive)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: cannot call recursively '%s'\n",
                    ast_location(location),
                    fortran_prettyprint_in_buffer(procedure_designator));
        }
        *result_type = get_error_type();
        return;
    }

    type_t* return_type = NULL; 
    // This is a generic procedure reference
    if (symbol->entity_specs.is_builtin
            && is_computed_function_type(symbol->type_information))
    {
        if (CURRENT_CONFIGURATION->disable_intrinsics)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: call to intrinsic '%s' not implemented\n", 
                        ast_location(location),
                        strtoupper(symbol->symbol_name));
            }
            *result_type = get_error_type();
            return;
        }

        scope_entry_t* entry = fortran_intrinsic_solve_call(symbol, 
                actual_arguments_keywords,
                nodecl_actual_arguments, 
                num_actual_arguments, 
                nodecl_simplify);

        if (entry == NULL)
        {
            const char* actual_arguments_str = "(";

            int i;
            for (i = 0; i < num_actual_arguments; i++)
            {
                char c[256];
                snprintf(c, 255, "%s%s", i != 0 ? ", " : "", 
                        fortran_print_type_str(nodecl_get_type(nodecl_actual_arguments[i])));
                c[255] = '\0';

                actual_arguments_str = strappend(actual_arguments_str, c);
            }
            actual_arguments_str = strappend(actual_arguments_str, ")");
            
            if (!checking_ambiguity())
            {
                error_printf("%s: error: call to intrinsic %s%s failed\n", 
                        ast_location(location),
                        strtoupper(symbol->symbol_name),
                        actual_arguments_str);
            }
            *result_type = get_error_type();
            return;
        }

        if (!nodecl_is_null(*nodecl_simplify))
        {
            return_type = nodecl_get_type(*nodecl_simplify);
        }
        else if (entry->entity_specs.is_elemental)
        {
            // Try to come up with a common_rank
            int common_rank = -1;
            int i;
            for (i = 0; i < num_actual_arguments; i++)
            {
                int current_rank = get_rank_of_type(nodecl_get_type(nodecl_actual_arguments[i]));
                if (common_rank < 0)
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
                return_type = get_n_ranked_type(
                        function_type_get_return_type(entry->type_information),
                        common_rank, decl_context);
            }
            else
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: mismatch of ranks in call to elemental intrinsic '%s'\n",
                            ast_location(location),
                            strtoupper(symbol->symbol_name));
                }

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
        if (symbol->entity_specs.is_generic_spec)
        {
            scope_entry_t* specific_symbol = get_specific_interface(symbol, num_actual_arguments, 
                actual_arguments_keywords,
                nodecl_actual_arguments);
            if (specific_symbol == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: no specific interface matches generic interface '%s' in function reference\n",
                            ast_location(location),
                            fortran_prettyprint_in_buffer(procedure_designator));
                }

                *result_type = get_error_type();
                return;
            }
            symbol = specific_symbol;
        }

        // This is now a specfic procedure reference
        ERROR_CONDITION (!is_function_type(symbol->type_information), "Invalid type for function symbol!\n", 0);

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_info_items[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
        memset(argument_info_items, 0, sizeof(argument_info_items));

        int i;
        for (i = 0; i < num_actual_arguments; i++)
        {
            int position = -1;
            if (actual_arguments_keywords[i] == NULL)
            {
                position = i;
            }
            else
            {
                int j;
                for (j = 0; j < symbol->entity_specs.num_related_symbols; j++)
                {
                    scope_entry_t* related_sym = symbol->entity_specs.related_symbols[j];

                    if (!related_sym->entity_specs.is_parameter)
                        continue;

                    if (strcasecmp(related_sym->symbol_name, actual_arguments_keywords[i]) == 0)
                    {
                        position = related_sym->entity_specs.parameter_position;
                    }
                }
                if (position < 0)
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: keyword '%s' is not a dummy argument of function '%s'\n",
                                ast_location(location), 
                                actual_arguments_keywords[i],
                                symbol->symbol_name);
                    }
                    *result_type = get_error_type();
                    return;
                }
            }

            if (argument_info_items[position].type != NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: argument keyword '%s' specified more than once\n",
                            ast_location(location), actual_arguments_keywords[i]);
                }
                *result_type = get_error_type();
                return;
            }
            argument_info_items[position].type = nodecl_get_type(nodecl_actual_arguments[i]);
        }

        int num_completed_arguments = num_actual_arguments;

        // Now complete with the optional ones
        for (i = 0; i < symbol->entity_specs.num_related_symbols; i++)
        {
            scope_entry_t* related_sym = symbol->entity_specs.related_symbols[i];

            if (related_sym->entity_specs.is_parameter)
            {
                if (argument_info_items[related_sym->entity_specs.parameter_position].type == NULL)
                {
                    if (related_sym->entity_specs.is_optional)
                    {
                        argument_info_items[related_sym->entity_specs.parameter_position].type = related_sym->type_information;
                        argument_info_items[related_sym->entity_specs.parameter_position].not_present = 1;
                        num_completed_arguments++;
                    }
                    else 
                    {
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: error: dummy argument '%s' of function '%s' has not been specified in function reference\n",
                                    ast_location(location),
                                    related_sym->symbol_name,
                                    symbol->symbol_name);
                        }
                        *result_type = get_error_type();
                        return;
                    }
                }
            }
        }

        if (!function_type_get_lacking_prototype(symbol->type_information) 
                && num_completed_arguments > function_type_get_num_parameters(symbol->type_information))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: too many actual arguments in function reference to '%s'\n",
                        ast_location(location),
                        symbol->symbol_name);
            }
            *result_type = get_error_type();
            return;
        }

        ERROR_CONDITION(!function_type_get_lacking_prototype(symbol->type_information) 
                && (num_completed_arguments != function_type_get_num_parameters(symbol->type_information)), 
                "Mismatch between arguments and the type of the function %d != %d", 
                num_completed_arguments,
                function_type_get_num_parameters(symbol->type_information));

        char argument_type_mismatch = 0;
        int common_rank = -1;
        if (!function_type_get_lacking_prototype(symbol->type_information))
        {
            actual_argument_info_t fixed_argument_info_items[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
            memcpy(fixed_argument_info_items, argument_info_items, sizeof(fixed_argument_info_items));

            if (symbol->entity_specs.is_elemental)
            {
                // We may have to adjust the ranks, first check that all the
                // ranks match
                char ok = 1;
                for (i = 0; i < num_completed_arguments && ok; i++)
                {
                    int current_rank = get_rank_of_type(fixed_argument_info_items[i].type); 
                    if (common_rank < 0)
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
                        fixed_argument_info_items[i].type = get_rank0_type(fixed_argument_info_items[i].type);
                    }
                }
            }

            for (i = 0; i < num_completed_arguments; i++)
            {
                type_t* formal_type = function_type_get_parameter_type_num(symbol->type_information, i);
                type_t* real_type = fixed_argument_info_items[i].type;

                if (!equivalent_tkr_types(formal_type, real_type))
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: type mismatch in argument %d between the "
                                "real argument %s and the dummy argument %s\n",
                                ast_location(location),
                                i + 1,
                                fortran_print_type_str(real_type),
                                fortran_print_type_str(formal_type));
                    }
                    argument_type_mismatch = 1;
                }
            }
        }

        if (argument_type_mismatch)
        {
            *result_type = get_error_type();
            return;
        }

        return_type = function_type_get_return_type(symbol->type_information);

        if (symbol->entity_specs.is_elemental
                && return_type != NULL)
        {
            if (common_rank > 0)
            {
                return_type = get_n_ranked_type(return_type, common_rank, decl_context);
            }
        }
    }

    if (return_type == NULL)
    {
        if (!is_call_stmt)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: invalid function reference to a SUBROUTINE\n",
                        ast_location(location));
            }
            *result_type = get_error_type();
            return;
        }
        return_type = get_void_type();
    }
    else
    {
        if (is_call_stmt)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: invalid CALL statement to a FUNCTION\n",
                        ast_location(location));
            }
            *result_type = get_error_type();
            return;
        }
    }

    *result_type = return_type;
    *called_symbol = symbol;
}

static void check_function_call(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    char is_call_stmt = (ASTText(expr) != NULL
            && (strcmp(ASTText(expr), "call") == 0));

    AST procedure_designator = ASTSon0(expr);
    AST actual_arg_spec_list = ASTSon1(expr);

    nodecl_t nodecl_proc_designator = nodecl_null();
    if (ASTType(procedure_designator) == AST_SYMBOL
            && is_call_stmt)
    {
        scope_entry_t* call_sym 
            = query_name_with_locus(decl_context, procedure_designator, ASTText(procedure_designator));
        if (call_sym == NULL
                || (call_sym->entity_specs.is_builtin
                    && !call_sym->entity_specs.is_builtin_subroutine))
        {
            // This should be regarded as an error, but it is not for some
            // obscure reasons
            call_sym = new_fortran_symbol(decl_context, ASTText(procedure_designator));
            call_sym->kind = SK_FUNCTION;
            call_sym->type_information = get_nonproto_function_type(NULL, 0);
            call_sym->entity_specs.is_extern = 1;
        }
        else 
        {
            // We know this function because of an EXTERNAL
            if (call_sym->entity_specs.is_implicit_basic_type)
            {
                call_sym->kind = SK_FUNCTION;
                call_sym->type_information = get_nonproto_function_type(NULL, 0);
                call_sym->entity_specs.is_implicit_basic_type = 0;
                call_sym->entity_specs.is_extern = 1;
            }
        }

        nodecl_proc_designator = nodecl_make_symbol(call_sym, 
                ASTFileName(procedure_designator), ASTLine(procedure_designator));
    }
    else
    {
        fortran_check_expression_impl_(procedure_designator, decl_context, &nodecl_proc_designator);
    }

    int num_actual_arguments = 0;

    nodecl_t nodecl_arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(nodecl_arguments, 0, sizeof(nodecl_arguments));
    const char* argument_keywords[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(argument_keywords, 0, sizeof(argument_keywords));

    // Check arguments
    if (actual_arg_spec_list != NULL)
    {
        char with_keyword = 0;
        char seen_alternate_return = 0;
        char wrong_arg_spec_list = 0;
        AST it;
        for_each_element(actual_arg_spec_list, it)
        {
            AST actual_arg_spec = ASTSon1(it);
            AST keyword = ASTSon0(actual_arg_spec);
            if (keyword != NULL)
            {
                with_keyword = 1;
                argument_keywords[num_actual_arguments] = ASTText(keyword);
            }
            else if (with_keyword) // keyword == NULL
            {
                error_printf("%s: error: in function call, '%s' argument requires a keyword\n",
                        ast_location(actual_arg_spec),
                        fortran_prettyprint_in_buffer(actual_arg_spec));
                return;
            }

            AST actual_arg = ASTSon1(actual_arg_spec);

            if (ASTType(actual_arg) != AST_ALTERNATE_RESULT_SPEC)
            {
                nodecl_t nodecl_argument = nodecl_null();
                fortran_check_expression_impl_(actual_arg, decl_context, &nodecl_argument);

                if (nodecl_is_err_expr(nodecl_argument))
                {
                    wrong_arg_spec_list = 1;
                }

                nodecl_arguments[num_actual_arguments] = nodecl_argument;
            }
            else
            {
                if (!is_call_stmt)
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: only CALL statement allows an alternate return\n",
                                ast_location(actual_arg_spec));
                    }
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                    return;
                }
                if (!seen_alternate_return)
                {
                    seen_alternate_return = 1;

                    nodecl_arguments[num_actual_arguments] = nodecl_make_fortran_alt_return(ASTFileName(actual_arg), ASTLine(actual_arg));
                }
                else
                {
                    if (!checking_ambiguity())
                    {
                        error_printf("%s: error: in a procedure reference an alternate return must be at the last position\n", 
                                ast_location(actual_arg_spec));
                    }
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                    return;
                }
            }

            num_actual_arguments++;
        }

        if (wrong_arg_spec_list)
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
            return;
        }
    }

    if (nodecl_is_err_expr(nodecl_proc_designator))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    scope_entry_t* symbol = nodecl_get_symbol(nodecl_proc_designator);

    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol(symbol, 
            decl_context, 
            expr, 
            procedure_designator, 
            num_actual_arguments,
            nodecl_arguments,
            argument_keywords,
            is_call_stmt,
            // out
            &result_type,
            &called_symbol,
            &nodecl_simplify
            );

    // ERROR_CONDITION(called_symbol == NULL, "Invalid symbol called returned by check_called_symbol", 0);
    ERROR_CONDITION(result_type == NULL, "Invalid type returned by check_called_symbol", 0);

    if (is_error_type(result_type))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }


    // Check arguments again...
    nodecl_t nodecl_argument_list = nodecl_null();
    if (actual_arg_spec_list != NULL)
    {
        int parameter_index = 0;
        AST it;
        for_each_element(actual_arg_spec_list, it)
        {
            AST actual_arg_spec = ASTSon1(it);

            AST keyword = ASTSon0(actual_arg_spec);
            AST actual_arg = ASTSon1(actual_arg_spec);

            nodecl_t nodecl_argument_spec = nodecl_null();
            if (ASTType(actual_arg) != AST_ALTERNATE_RESULT_SPEC)
            {
                char no_argument_info = 0;
                scope_entry_t* parameter = NULL;
                if (keyword == NULL)
                {
                    ERROR_CONDITION(parameter_index < 0, "Invalid index", 0);
                    if (called_symbol->entity_specs.num_related_symbols <= parameter_index)
                    {
                        no_argument_info = 1;
                    }
                    else
                    {
                        parameter = called_symbol->entity_specs.related_symbols[parameter_index];
                    }
                    parameter_index++;
                }
                else
                {
                    const char* param_name = ASTText(keyword);
                    int j;
                    for (j = 0; j < called_symbol->entity_specs.num_related_symbols; j++)
                    {
                        if (strcasecmp(called_symbol->entity_specs.related_symbols[j]->symbol_name, param_name) == 0)
                        {
                            parameter = called_symbol->entity_specs.related_symbols[j];
                            break;
                        }
                    }
                    parameter_index = -1;
                }

                nodecl_t nodecl_argument = nodecl_null();
                fortran_check_expression_impl_(actual_arg, decl_context, &nodecl_argument);

                if (no_argument_info)
                {
                    nodecl_argument_spec = nodecl_argument;
                }
                else
                {
                    ERROR_CONDITION(parameter == NULL, "We did not find the parameter", 0);
                    nodecl_argument_spec = nodecl_make_fortran_named_pair_spec(
                            nodecl_make_symbol(parameter, ASTFileName(actual_arg_spec), ASTLine(actual_arg_spec)),
                            nodecl_argument,
                            ASTFileName(actual_arg_spec), ASTLine(actual_arg_spec));
                }
            }
            else
            {
                internal_error("Alternate return not implemented yet", 0);
                // nodecl_argument_spec = nodecl_make_fortran_named_pair_spec(
                //         nodecl_null(),
                //         nodecl_make_builtin_expr(
                //             nodecl_make_any_list(
                //                 nodecl_make_list_1(
                //                     nodecl_make_string_literal(get_void_type(), 
                //                         /* label */ ASTText(ASTSon0(actual_arg)), 
                //                         ASTFileName(ASTSon0(actual_arg)), 
                //                         ASTLine(ASTSon0(actual_arg)))),
                //                 ASTFileName(ASTSon0(actual_arg)), 
                //                 ASTLine(ASTSon0(actual_arg)) ),
                //             get_void_type(),
                //             "fortran-alternate-result",
                //             ASTFileName(actual_arg),
                //             ASTLine(actual_arg)),
                //         ASTFileName(actual_arg),
                //         ASTLine(actual_arg) );
            }

            nodecl_argument_list = nodecl_append_to_list(nodecl_argument_list, nodecl_argument_spec);
        }
    }

    if (nodecl_is_null(nodecl_simplify))
    {
        *nodecl_output = nodecl_make_function_call(
                nodecl_make_symbol(called_symbol, ASTFileName(procedure_designator), ASTLine(procedure_designator)),
                nodecl_argument_list,
                result_type,
                ASTFileName(expr), ASTLine(expr));
    }
    else
    {
        *nodecl_output = nodecl_simplify;
    }
}

static void check_greater_or_equal_than(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_greater_than(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_hexadecimal_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    // We allow X and Z
    compute_boz_literal(expr, "xz", 16, nodecl_output);
}

static void check_image_ref(AST expr UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    error_printf("%s: sorry: image references not supported\n", 
            ast_location(expr));
    *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
}

static void check_logical_and(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_logical_equal(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_logical_or(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_lower_or_equal_than(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_lower_than(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_minus_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_mult_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void check_neg_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_not_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_octal_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    compute_boz_literal(expr, "o", 8, nodecl_output);
}

static void check_parenthesized_expression(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_expr);


    *nodecl_output = nodecl_make_parenthesized_expression(
            nodecl_expr,
            nodecl_get_type(nodecl_expr),
            ASTFileName(expr), ASTLine(expr));

    if (nodecl_is_constant(nodecl_expr))
    {
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_expr));
    }
}

static void check_plus_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_unary_check(expr, decl_context, nodecl_output);
}

static void check_power_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    common_binary_check(expr, decl_context, nodecl_output);
}

static void common_binary_intrinsic_check(AST expr, decl_context_t, type_t* lhs_type, type_t* rhs_type, 
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output);
static void common_binary_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expr);
    AST rhs = ASTSon1(expr);
    nodecl_t nodecl_lhs = nodecl_null();
    fortran_check_expression_impl_(lhs, decl_context, &nodecl_lhs);
    nodecl_t nodecl_rhs = nodecl_null();
    fortran_check_expression_impl_(rhs, decl_context, &nodecl_rhs);

    type_t* lhs_type = nodecl_get_type(nodecl_lhs);
    type_t* rhs_type = nodecl_get_type(nodecl_rhs);

    RETURN_IF_ERROR_2(lhs_type, rhs_type, expr);

    common_binary_intrinsic_check(expr, decl_context, lhs_type, rhs_type, nodecl_lhs, nodecl_rhs, nodecl_output);

}

static void common_binary_intrinsic_check(AST expr, decl_context_t decl_context, type_t* lhs_type, type_t* rhs_type,
        nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, nodecl_t* nodecl_output)
{
    compute_result_of_intrinsic_operator(expr, decl_context, lhs_type, rhs_type, nodecl_lhs, nodecl_rhs, nodecl_output);
}

static void common_unary_intrinsic_check(AST expr, decl_context_t decl_context, type_t* rhs_type,
        nodecl_t nodecl_rhs, nodecl_t* nodecl_output);

static void common_unary_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output) 
{
    AST rhs = ASTSon0(expr);
    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(rhs, decl_context, &nodecl_expr);

    type_t* rhs_type = nodecl_get_type(nodecl_expr);

    RETURN_IF_ERROR_1(rhs_type, expr);

    common_unary_intrinsic_check(expr, decl_context, rhs_type, nodecl_expr, nodecl_output);

}

static void common_unary_intrinsic_check(AST expr, decl_context_t decl_context, type_t* rhs_type,
        nodecl_t nodecl_rhs, nodecl_t* nodecl_output)
{
    compute_result_of_intrinsic_operator(expr, decl_context, NULL, rhs_type, nodecl_null(), nodecl_rhs, nodecl_output);
}

static void check_string_literal(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    const char* literal = ASTText(expr);

    char kind[31] = { 0 };
    char has_kind = 0;

    if ((has_kind = (literal[0] != '"'
                    && literal[0] != '\'')))
    {
        char *q = kind;
        while (*literal != '_'
                && ((unsigned int)(q - kind) < (sizeof(kind) - 1)))
        {
            literal++;
        }
        if (*literal != '_')
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: KIND specifier is too long\n",
                        ast_location(expr));
            }
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
            return;
        }
        literal++;

        if (!checking_ambiguity())
        {
            warn_printf("%s: warning: ignoring KIND=%s of character-literal, assuming KIND=1\n",
                    kind,
                    ast_location(expr));
        }
    }

    int length = strlen(literal);

    char real_string[length + 1];
    int real_length = 0;
    char delim = literal[0];
    literal++; // Jump delim

    while (*literal != delim
                || *(literal+1) == delim)
    {
        ERROR_CONDITION(real_length >= (length+1), "Wrong construction of real string", 0);

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
            fortran_get_default_logical_type(), 
            const_value_get_signed_int(1), 
            ASTFileName(expr),
            ASTLine(expr));
    nodecl_t length_tree = nodecl_make_integer_literal(fortran_get_default_logical_type(), 
            const_value_get_signed_int(real_length), 
            ASTFileName(expr),
            ASTLine(expr));

    type_t* t = get_array_type_bounds(fortran_get_default_character_type(), one, length_tree, decl_context);


    const_value_t* value = const_value_make_string(real_string, real_length);

    *nodecl_output = nodecl_make_string_literal(t, value, ASTFileName(expr), ASTLine(expr));
}

static void check_user_defined_unary_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // This is an AST_NAMED_PAIR_SPEC with no name. This way it is easier to
    // reuse common function call code
    AST operand = ASTSon1(expr);
    AST operand_expr = ASTSon1(operand);

    nodecl_t nodecl_expr = nodecl_null();
    fortran_check_expression_impl_(operand_expr, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST operator = ASTSon0(expr);
    const char* operator_name = strtolower(strappend(".operator.", ASTText(operator)));
    scope_entry_t* call_sym = query_name_no_implicit(decl_context, operator_name);

    if (call_sym == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: unknown user-defined operator '%s'\n", ast_location(expr), ASTText(operator));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    int num_actual_arguments = 1;
    const char* keyword_arguments[1] = { NULL };
    nodecl_t nodecl_arguments[1] = { nodecl_expr };

    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol(call_sym, 
            decl_context, 
            expr, 
            operator, 
            num_actual_arguments,
            nodecl_arguments,
            keyword_arguments,
            /* is_call_stmt */ 0,
            // out
            &result_type,
            &called_symbol,
            &nodecl_simplify);

    *nodecl_output = nodecl_make_function_call(
            nodecl_make_symbol(called_symbol, ASTFileName(expr), ASTLine(expr)),
            nodecl_make_list_1(
                nodecl_make_fortran_named_pair_spec(nodecl_null(),
                    nodecl_expr,
                    ASTFileName(operand_expr),
                    ASTLine(operand_expr))),
            result_type,
            ASTFileName(expr),
            ASTLine(expr));
}

static void check_user_defined_binary_op(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // This is an AST_NAMED_PAIR_SPEC with no name. This way it is easier to
    // reuse common function call code
    AST lhs = ASTSon1(expr);
    AST lhs_expr = ASTSon1(lhs);

    nodecl_t nodecl_lhs = nodecl_null();
    fortran_check_expression_impl_(lhs_expr, decl_context, &nodecl_lhs);

    if (nodecl_is_err_expr(nodecl_lhs))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST rhs = ASTSon2(expr);
    AST rhs_expr = ASTSon1(rhs);

    nodecl_t nodecl_rhs = nodecl_null();
    fortran_check_expression_impl_(rhs_expr, decl_context, &nodecl_rhs);

    if (nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST operator = ASTSon0(expr);
    const char* operator_name = strtolower(strappend(".operator.", ASTText(operator)));
    scope_entry_t* call_sym = query_name_with_locus(decl_context, operator, operator_name);

    if (call_sym == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: unknown user-defined operator '%s'\n", ast_location(expr), ASTText(operator));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    int num_actual_arguments = 2;
    const char* keywords[2] = { NULL, NULL };
    nodecl_t nodecl_arguments[2] = { nodecl_lhs, nodecl_rhs };

    type_t* result_type = NULL;
    scope_entry_t* called_symbol = NULL;
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol(call_sym, 
            decl_context, 
            /* location */ expr, 
            operator, 
            num_actual_arguments,
            nodecl_arguments,
            keywords,
            /* is_call_stmt */ 0,
            // out
            &result_type,
            &called_symbol,
            &nodecl_simplify);

    *nodecl_output = nodecl_make_function_call(
            nodecl_make_symbol(called_symbol, ASTFileName(expr), ASTLine(expr)),
            nodecl_make_list_2(
                nodecl_make_fortran_named_pair_spec(nodecl_null(),
                    nodecl_lhs,
                    ASTFileName(lhs_expr),
                    ASTLine(lhs_expr)),
                nodecl_make_fortran_named_pair_spec(nodecl_null(),
                    nodecl_rhs,
                    ASTFileName(rhs_expr),
                    ASTLine(rhs_expr))),
            result_type,
            ASTFileName(expr),
            ASTLine(expr));
}

static char function_has_named_result(scope_entry_t* entry)
{
    int i;
    for (i = 0; i < entry->entity_specs.num_related_symbols; i++)
    {
        if (entry->entity_specs.related_symbols[i]->entity_specs.is_result
                && strcasecmp(entry->entity_specs.related_symbols[i]->symbol_name, entry->symbol_name) != 0)
            return 1;
    }
    return 0;
}


static char is_name_of_funtion_call(AST expr)
{
    return ASTParent(expr) != NULL
        && ASTType(ASTParent(expr)) == AST_FUNCTION_CALL;
}

static char is_name_in_actual_arg_spec_list(AST expr)
{
    node_t hierarchy[] = { AST_NAMED_PAIR_SPEC, AST_NODE_LIST, AST_FUNCTION_CALL, AST_INVALID_NODE };

    AST p = ASTParent(expr);
    int i = 0;
    while (hierarchy[i] != AST_INVALID_NODE
            && p != NULL
            && ASTType(p) == hierarchy[i])
    {
        p = ASTParent(p);
        i++;
    }

    return (hierarchy[i] == AST_INVALID_NODE);
}

static void check_symbol(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    scope_entry_t* entry = query_name_with_locus(decl_context, expr, ASTText(expr));

    if (entry == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: unknown entity '%s'\n",
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    if (entry->kind == SK_UNDEFINED)
    {
        if (is_name_of_funtion_call(expr))
        {
            entry->kind = SK_FUNCTION;
            entry->type_information = get_nonproto_function_type(entry->type_information, 0);
        }
        // else if (is_name_in_actual_arg_spec_list(expr))
        // {
        //     // If we are an actual argument do not change our status
        // }
        else
        {
            // Otherwise we are a variable
            entry->kind = SK_VARIABLE;
        }
    }

    if (entry->kind == SK_FUNCTION)
    {
        // This must act as a variable
        if (!is_name_of_funtion_call(expr)
                && !is_name_in_actual_arg_spec_list(expr))
        {
            // FIXME: This is not exactly OK here, we may be passing a function
            // name as a variable to another function
            type_t* return_type = NULL; 
            if (is_function_type(no_ref(entry->type_information)))
                return_type = function_type_get_return_type(entry->type_information);
            if (return_type == NULL
                    || function_has_named_result(entry))
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: error: '%s' is not a variable\n",
                            ast_location(expr),
                            fortran_prettyprint_in_buffer(expr));
                }
                *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                return;
            }

            // Update to the result symbol
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Reference to function name '%s' where a variable is expected, using result symbol\n",
                        entry->symbol_name);
            }
            entry = function_get_result_symbol(entry);
        }
    }

    if (entry->kind == SK_VARIABLE)
    {
        // It might happen that dummy arguments/result do not have any implicit
        // type here (because the input code is wrong)
        if (is_error_type(entry->type_information))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: entity '%s' does not have any IMPLICIT type\n",
                        ast_location(expr),
                        fortran_prettyprint_in_buffer(expr));
            }
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
            return;
        }

        *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expr), ASTLine(expr));
        nodecl_set_type(*nodecl_output, entry->type_information);

        if (is_const_qualified_type(no_ref(entry->type_information))
                && !nodecl_is_null(entry->value)
                && nodecl_is_constant(entry->value))
        {
            // Effectively replace the synthesized nodecl by its constant 
            *nodecl_output = entry->value;
        }

        if (is_pointer_type(no_ref(entry->type_information)))
        {
            *nodecl_output = 
                nodecl_make_derreference(
                        *nodecl_output,
                        pointer_type_get_pointee_type(entry->type_information),
                        ASTFileName(expr), ASTLine(expr));
            nodecl_set_symbol(*nodecl_output, entry);
        }
    }
    else if (entry->kind == SK_UNDEFINED)
    {
        *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expr), ASTLine(expr));
        nodecl_set_type(*nodecl_output, entry->type_information);
    }
    else if (entry->kind == SK_FUNCTION)
    {
        if (!is_name_of_funtion_call(expr) 
                && !is_name_in_actual_arg_spec_list(expr))
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
            return;
        }
        else
        {
            *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expr), ASTLine(expr));
            nodecl_set_type(*nodecl_output, entry->type_information);
        }
    }
    else
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
    }


}

static void conform_types(type_t* lhs_type, type_t* rhs_type, type_t** conf_lhs_type, type_t** conf_rhs_type);

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

    conform_types(lvalue_type, rvalue_type, &conf_lhs_type, &conf_rhs_type);

    if ((is_integer_type(conf_lhs_type)
                || is_floating_type(conf_lhs_type)
                || is_complex_type(conf_lhs_type))
            && (is_integer_type(conf_rhs_type)
                || is_floating_type(conf_rhs_type)
                || is_complex_type(conf_rhs_type)))
        return 1;

    if (is_fortran_character_type(conf_lhs_type)
            && is_fortran_character_type(conf_rhs_type)
            && equivalent_types(array_type_get_element_type(conf_lhs_type), 
                array_type_get_element_type(conf_rhs_type))) 
    {
        return 1;
    }

    if (is_bool_type(conf_lhs_type)
            && is_bool_type(conf_rhs_type))
        return 1;

    if (is_class_type(conf_lhs_type)
            && is_class_type(conf_rhs_type)
            && equivalent_types(conf_lhs_type, conf_rhs_type))
        return 1;

    return 0;
}

static char is_defined_assignment(AST expr, AST lvalue, 
        AST rvalue UNUSED_PARAMETER,
        nodecl_t nodecl_lvalue, nodecl_t nodecl_rvalue, 
        decl_context_t decl_context, scope_entry_t** entry)
{
    const char* operator_name = ".operator.=";
    scope_entry_t* call_sym = query_name_no_implicit(decl_context, operator_name);

    if (call_sym == NULL)
        return 0;

    int num_actual_arguments = 2;
    const char* keyword_names[2] = { NULL, NULL };
    nodecl_t nodecl_arguments[2] = { nodecl_lvalue, nodecl_rvalue };

    type_t* result_type = NULL;

    AST operator_designation = ASTLeaf(AST_SYMBOL, ast_get_filename(lvalue), ast_get_line(lvalue), "=");

    enter_test_expression();
    nodecl_t nodecl_simplify = nodecl_null();
    check_called_symbol(call_sym, 
            decl_context,
            /* location */ expr,
            operator_designation,
            num_actual_arguments,
            nodecl_arguments,
            keyword_names,
            /* is_call_stmt */ 1, // Assignments must be subroutines!
            // out
            &result_type,
            entry,
            &nodecl_simplify);
    leave_test_expression();

    ast_free(operator_designation);

    return !is_error_type(result_type);
}

static void check_assignment(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lvalue = ASTSon0(expr);
    AST rvalue = ASTSon1(expr);

    nodecl_t nodecl_lvalue = nodecl_null();
    fortran_check_expression_impl_(lvalue, decl_context, &nodecl_lvalue);
    if (nodecl_is_err_expr(nodecl_lvalue))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }
    type_t* lvalue_type = nodecl_get_type(nodecl_lvalue);

    nodecl_t nodecl_rvalue = nodecl_null();
    fortran_check_expression_impl_(rvalue, decl_context, &nodecl_rvalue);
    if (nodecl_is_err_expr(nodecl_rvalue))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }
    type_t* rvalue_type = nodecl_get_type(nodecl_rvalue);

    scope_entry_t* assignment_op = NULL;
    char is_defined_assig = is_defined_assignment(expr, lvalue, rvalue, nodecl_lvalue, nodecl_rvalue, decl_context, &assignment_op);

    if (!is_defined_assig
            && !is_intrinsic_assignment(lvalue_type, rvalue_type))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: cannot assign to a variable of type '%s' a value of type '%s'\n",
                    ast_location(expr),
                    fortran_print_type_str(lvalue_type),
                    fortran_print_type_str(rvalue_type));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }


    if (!is_defined_assig)
    {
        if (!equivalent_types(no_ref(lvalue_type), no_ref(rvalue_type)))
        {
            nodecl_rvalue = nodecl_make_conversion(nodecl_rvalue, 
                    lvalue_type,
                    nodecl_get_filename(nodecl_rvalue), nodecl_get_line(nodecl_rvalue));
        }
        *nodecl_output = nodecl_make_assignment(nodecl_lvalue, nodecl_rvalue, get_void_type(), ASTFileName(expr), ASTLine(expr));
    }
    else
    {
        *nodecl_output = nodecl_make_function_call(
                nodecl_make_symbol(assignment_op, ASTFileName(expr), ASTLine(expr)),
                nodecl_make_list_2(
                    nodecl_make_fortran_named_pair_spec(
                        nodecl_null(),
                        nodecl_lvalue,
                        ASTFileName(lvalue), ASTLine(lvalue)),
                    nodecl_make_fortran_named_pair_spec(
                        nodecl_null(),
                        nodecl_rvalue,
                        ASTFileName(rvalue), ASTLine(rvalue))),
                get_void_type(),
                ASTFileName(expr), ASTLine(expr));
    }

    if (nodecl_is_constant(nodecl_rvalue))
    {
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_rvalue));
    }
}

static void check_ptr_assignment(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lvalue = ASTSon0(expr);
    AST rvalue = ASTSon1(expr);

    nodecl_t nodecl_lvalue = nodecl_null();
    fortran_check_expression_impl_(lvalue, decl_context, &nodecl_lvalue);

    type_t* lvalue_type = nodecl_get_type(nodecl_lvalue);
    if (is_error_type(lvalue_type))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    nodecl_t nodecl_rvalue = nodecl_null();
    fortran_check_expression_impl_(rvalue, decl_context, &nodecl_rvalue);

    type_t* rvalue_type = nodecl_get_type(nodecl_rvalue);
    if (is_error_type(rvalue_type))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    scope_entry_t* sym = NULL;
    if (nodecl_get_symbol(nodecl_lvalue) != NULL)
    {
        sym = nodecl_get_symbol(nodecl_lvalue);
    }
    if (sym == NULL
            || sym->kind != SK_VARIABLE
            || !is_pointer_type(no_ref(sym->type_information)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: left hand of pointer assignment is not a pointer data-reference\n",
                    ast_location(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    ERROR_CONDITION(nodecl_get_kind(nodecl_lvalue) != NODECL_DERREFERENCE, 
            "A reference to a pointer entity must be derreferenced", 0);

    // Get the inner part of the derreference
    nodecl_lvalue = nodecl_get_child(nodecl_lvalue, 0);

    *nodecl_output = nodecl_make_assignment(
            nodecl_lvalue,
            nodecl_rvalue,
            get_void_type(),
            ASTFileName(expr),
            ASTLine(expr));
}

static void disambiguate_expression(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    int num_ambig = ast_get_num_ambiguities(expr);

    int i;
    int correct_option = -1;
    int function_call = -1;
    for (i = 0; i < num_ambig; i++)
    {
        AST current_expr = ast_get_ambiguity(expr, i);

        nodecl_t nodecl_check_expr = nodecl_null();
        switch (ASTType(current_expr))
        {
            case AST_FUNCTION_CALL:
            case AST_ARRAY_SUBSCRIPT:
            case AST_DERIVED_TYPE_CONSTRUCTOR:
                {
                    enter_test_expression();
                    fortran_check_expression_impl_(current_expr, decl_context, &nodecl_check_expr);
                    leave_test_expression();

                    if (ASTType(current_expr) == AST_FUNCTION_CALL)
                    {
                        function_call = i;
                    }
                    break;
                }
            default:
                {
                    internal_error("%s: unexpected node '%s'\n", 
                            ast_location(expr),
                            ast_print_node_type(ASTType(expr)));
                    break;
                }
        }

        if (!nodecl_is_err_expr(nodecl_check_expr))
        {
            if (correct_option < 0)
            {
                correct_option = i;
            }
            else
            {
                internal_error("%s: more than one interpretation valid for '%s'\n",
                        fortran_prettyprint_in_buffer(current_expr));
            }
        }
    }

    if (correct_option < 0)
    {
        ERROR_CONDITION(function_call < 0, "Invalid ambiguity", 0);
        // Default to function call as a fallback
        ast_replace_with_ambiguity(expr, function_call);
    }
    else
    {
        ast_replace_with_ambiguity(expr, correct_option);
    }

    // We want the diagnostics again
    fortran_check_expression_impl_(expr, decl_context, nodecl_output);
}

static type_t* common_kind(type_t* t1, type_t* t2)
{
    t1 = no_ref(t1);
    t2 = no_ref(t2);

    ERROR_CONDITION(!is_scalar_type(t1)
            || !is_scalar_type(t2), "One of the types is not scalar", 0);

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

    if (is_pointer_to_fortran_character_type(t1))
        t1 = pointer_type_get_pointee_type(t1);
    if (is_pointer_to_fortran_character_type(t2))
        t1 = pointer_type_get_pointee_type(t2);

    nodecl_t length1 = array_type_get_array_size_expr(t1);
    nodecl_t length2 = array_type_get_array_size_expr(t2);

    type_t* char1 = array_type_get_element_type(t1);
    type_t* char2 = array_type_get_element_type(t2);

    if (!equivalent_types(get_unqualified_type(char1), get_unqualified_type(char2)))
        return NULL;

    type_t* result = NULL;
    if (!nodecl_is_null(length1)
            && !nodecl_is_null(length2))
    {
        nodecl_t lower = nodecl_make_integer_literal(
                fortran_get_default_logical_type(), 
                const_value_get_signed_int(1), 
                NULL, 0);
        nodecl_t upper = nodecl_null();
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
                    nodecl_copy(length1),
                    nodecl_copy(length2),
                    fortran_get_default_logical_type(),
                    NULL, 0);
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

static char is_fortran_character_type_or_pointer_to(type_t* t)
{
    t = no_ref(t);
    return is_pointer_to_fortran_character_type(t)
        || is_fortran_character_type(t);
}

static operand_types_t concat_op[] = 
{
    { is_fortran_character_type_or_pointer_to, is_fortran_character_type_or_pointer_to, combine_character_array, DO_NOT_CONVERT_TO_RESULT },
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
    { is_fortran_character_type, is_fortran_character_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
};

static operand_types_t relational_weak[] =
{
    { is_integer_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_integer_type, is_floating_type,  logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_integer_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_floating_type, is_floating_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
    { is_fortran_character_type, is_fortran_character_type, logical_type, DO_NOT_CONVERT_TO_RESULT },
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

    nodecl_t (*compute_nodecl)(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs, type_t* t, const char* filename, int line);
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
static nodecl_t binary_##x(nodecl_t nodecl_lhs UNUSED_PARAMETER, nodecl_t nodecl_rhs, type_t* t, const char* filename, int line) \
{ \
    return x(nodecl_rhs, t, filename, line); \
}

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
    HANDLER_MAP(AST_POWER, arithmetic_binary, const_bin_power, ".operator.**", nodecl_make_power),
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

static type_t* compute_result_of_intrinsic_operator(AST expr, decl_context_t decl_context, 
        type_t* lhs_type, 
        type_t* rhs_type,
        nodecl_t nodecl_lhs,
        nodecl_t nodecl_rhs,
        nodecl_t* nodecl_output)
{
    lhs_type = no_ref(lhs_type);
    rhs_type = no_ref(rhs_type);

    // Remove pointer, which is actually only used for data refs
    if (is_pointer_type(lhs_type))
        lhs_type = pointer_type_get_pointee_type(lhs_type);
    if (is_pointer_type(rhs_type))
        rhs_type = pointer_type_get_pointee_type(rhs_type);

    type_t* conf_lhs_type = NULL;
    type_t* conf_rhs_type = NULL;

    conform_types(lhs_type, rhs_type, &conf_lhs_type, &conf_rhs_type);

    if (!operand_map_init)
    {
        qsort(operand_map, 
                sizeof(operand_map) / sizeof(operand_map[0]), 
                sizeof(operand_map[0]),
                compare_map_items);

        operand_map_init = 1;
    }

    operand_map_t key = { .node_type = ASTType(expr) };
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

    operand_types_t* operand_types = value->operand_types;
    int i;
    for (i = 0; i < value->num_operands && result == NULL; i++)
    {
        if (((lhs_type == NULL 
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
        result = get_error_type();
        // Now try with a user defined operator
        scope_entry_t* call_sym = query_name_with_locus(decl_context, expr, value->op_symbol_name);

        // Perform a resolution by means of a call check
        if (call_sym != NULL)
        {
            int num_actual_arguments = 0;
            const char* keywords[2] = { NULL, NULL };
            nodecl_t nodecl_arguments[2] = { nodecl_null(), nodecl_null() };
            if (lhs_type == NULL)
            {
                num_actual_arguments = 1;
                nodecl_arguments[0] = nodecl_rhs;
            }
            else
            {
                num_actual_arguments = 2;
                nodecl_arguments[0] = nodecl_lhs;
                nodecl_arguments[1] = nodecl_rhs;
            }

            AST operator_designation = ASTLeaf(AST_SYMBOL, ast_get_filename(expr), ast_get_line(expr), get_operator_for_expr(expr));

            scope_entry_t* called_symbol = NULL;
            nodecl_t nodecl_simplify = nodecl_null();
            check_called_symbol(call_sym, 
                    decl_context, 
                    /* location */ expr, 
                    operator_designation,
                    num_actual_arguments,
                    nodecl_arguments,
                    keywords,
                    /* is_call_stmt */ 0,
                    // out
                    &result,
                    &called_symbol,
                    &nodecl_simplify);

            ast_free(operator_designation);

            // Restore the rank of the common type
            if (!is_error_type(result))
            {
                result = rerank_type(result, lhs_type, rhs_type);

                if (nodecl_is_null(nodecl_simplify))
                {
                    nodecl_t nodecl_argument_list = nodecl_null();

                    if (nodecl_is_null(nodecl_lhs))
                    {
                        nodecl_argument_list = nodecl_make_list_1(nodecl_rhs);
                    }
                    else
                    {
                        nodecl_argument_list = nodecl_make_list_2(nodecl_rhs, nodecl_lhs);
                    }

                    *nodecl_output = nodecl_make_function_call(
                            nodecl_make_symbol(call_sym, ast_get_filename(expr), ast_get_line(expr)),
                            nodecl_argument_list,
                            result,
                            ASTFileName(expr), ASTLine(expr));
                }
                else
                {
                    *nodecl_output = nodecl_simplify;
                }
            }
        }

        if (is_error_type(result))
        {
            if (lhs_type != NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid operand types %s and %s for intrinsic binary operator '%s'\n",
                            ast_location(expr),
                            fortran_print_type_str(lhs_type),
                            fortran_print_type_str(rhs_type),
                            get_operator_for_expr(expr));
                }
                return get_error_type();
            }
            else
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid operand types %s for intrinsic unary operator '%s'\n",
                            ast_location(expr),
                            fortran_print_type_str(rhs_type),
                            get_operator_for_expr(expr));
                }
                return get_error_type();
            }
        }
        
    }
    else
    {
        // Restore the rank of the common type
        result = rerank_type(result, lhs_type, rhs_type);

        const_value_t* val = NULL;
        if (value->compute_const != NULL)
        {
            if (lhs_type != NULL) 
            {
                // Binary
                val = value->compute_const(nodecl_lhs, nodecl_rhs);
            }
            else
            {
                val = value->compute_const(nodecl_null(), nodecl_rhs);
            }
        }

        // Keep the conversions
        if (convert_to_common)
        {
            if (lhs_type != NULL
                    && !equivalent_types(no_ref(result), no_ref(lhs_type)))
            {
                nodecl_lhs = nodecl_make_conversion(nodecl_lhs, result, 
                        nodecl_get_filename(nodecl_lhs), nodecl_get_line(nodecl_lhs));
            }
            if (!equivalent_types(no_ref(result), no_ref(rhs_type)))
            {
                nodecl_rhs = nodecl_make_conversion(nodecl_rhs, result, 
                        nodecl_get_filename(nodecl_rhs), nodecl_get_line(nodecl_rhs));
            }
        }

        *nodecl_output = value->compute_nodecl(nodecl_lhs, nodecl_rhs, result, ASTFileName(expr), ASTLine(expr));
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
    return operator_names[ASTType(expr)];
}

static void conform_types(type_t* lhs_type, type_t* rhs_type, type_t** conf_lhs_type, type_t** conf_rhs_type)
{
    lhs_type = no_ref(lhs_type);
    rhs_type = no_ref(rhs_type);

    if (!is_fortran_array_type(lhs_type)
            && !is_fortran_array_type(rhs_type))
    {
        *conf_lhs_type = lhs_type;
        *conf_rhs_type = rhs_type;
    }
    else if (is_fortran_array_type(lhs_type)
            != is_fortran_array_type(rhs_type))
    {
        // One is array and the other is scalar
        *conf_lhs_type = get_rank0_type(lhs_type);
        *conf_rhs_type = get_rank0_type(rhs_type);
    }
    else 
    {
        // Both are arrays, they only conform if their rank (and ultimately its
        // shape but this is not always checkable) matches
        if (get_rank_of_type(lhs_type) == get_rank_of_type(rhs_type))
        {
            *conf_lhs_type = get_rank0_type(lhs_type);
            *conf_rhs_type = get_rank0_type(rhs_type);
        }
        else
        // Do not conform
        {
            *conf_lhs_type = lhs_type;
            *conf_rhs_type = rhs_type;
        }
    }
}

static type_t* rerank_type(type_t* rank0_common, type_t* lhs_type, type_t* rhs_type)
{
    lhs_type = no_ref(lhs_type);
    rhs_type = no_ref(rhs_type);

    if (is_fortran_array_type(lhs_type))
    {
        // They should have the same rank and shape so it does not matter very much which one we use, right?
        return rebuild_array_type(rank0_common, lhs_type);
    }
    else if (is_fortran_array_type(rhs_type))
    {
        return rebuild_array_type(rank0_common, rhs_type);
    }
    else
    {
        return rank0_common;
    }
}

static const_value_t* const_bin_(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs,
        const_value_t* (*compute)(const_value_t*, const_value_t*))
{
    if (nodecl_is_constant(nodecl_lhs)
            && nodecl_is_constant(nodecl_rhs))
    {
        return compute(nodecl_get_constant(nodecl_lhs),
                nodecl_get_constant(nodecl_rhs));
    }
    return NULL;
}

static const_value_t* const_unary_(nodecl_t nodecl_lhs, const_value_t* (*compute)(const_value_t*))
{
    if (nodecl_is_constant(nodecl_lhs))
    {
        return compute(nodecl_get_constant(nodecl_lhs));
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

static const_value_t* const_bin_div(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_div);
}

static const_value_t* const_bin_power(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_pow);
}

static const_value_t* const_bin_concat(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_string_concat);
}

static const_value_t* const_bin_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_eq);
}

static const_value_t* const_bin_not_equal(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_neq);
}

static const_value_t* const_bin_lt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_lt);
}

static const_value_t* const_bin_lte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_lte);
}

static const_value_t* const_bin_gt(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_gt);
}

static const_value_t* const_bin_gte(nodecl_t nodecl_lhs, nodecl_t nodecl_rhs)
{
    return const_bin_(nodecl_lhs, nodecl_rhs, const_value_gte);
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
