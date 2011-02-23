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
#include "cxx-exprtype.h"
#include "cxx-entrylist.h"
#include "cxx-ast.h"
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include "cxx-tltype.h"
#include "cxx-attrnames.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

static void fortran_check_expression_impl_(AST expression, decl_context_t decl_context);

char fortran_check_expression(AST a, decl_context_t decl_context)
{
    fortran_check_expression_impl_(a, decl_context);
    return (!is_error_type(expression_get_type(a)));
}

typedef void (*check_expression_function_t)(AST statement, decl_context_t);
typedef struct check_expression_handler_tag
{
    node_t ast_kind;
    check_expression_function_t handler;
} check_expression_handler_t;

#define STATEMENT_HANDLER_TABLE \
 STATEMENT_HANDLER(AST_ADD_OP, check_add_op) \
 STATEMENT_HANDLER(AST_ARRAY_CONSTRUCTOR, check_array_constructor) \
 STATEMENT_HANDLER(AST_ARRAY_REF, check_array_ref) \
 STATEMENT_HANDLER(AST_BINARY_LITERAL, check_binary_literal) \
 STATEMENT_HANDLER(AST_BOOLEAN_LITERAL, check_boolean_literal) \
 STATEMENT_HANDLER(AST_COMPLEX_LITERAL, check_complex_literal) \
 STATEMENT_HANDLER(AST_COMPONENT_REF, check_component_ref) \
 STATEMENT_HANDLER(AST_CONCAT_OP, check_concat_op) \
 STATEMENT_HANDLER(AST_DECIMAL_LITERAL, check_decimal_literal) \
 STATEMENT_HANDLER(AST_DERIVED_TYPE_CONSTRUCTOR, check_derived_type_constructor) \
 STATEMENT_HANDLER(AST_DIFFERENT_OP, check_different_op) \
 STATEMENT_HANDLER(AST_DIV_OP, check_div_op) \
 STATEMENT_HANDLER(AST_EQUAL_OP, check_equal_op) \
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
 STATEMENT_HANDLER(AST_MINUS_OP, check_minus_op) \
 STATEMENT_HANDLER(AST_MULT_OP, check_mult_op) \
 STATEMENT_HANDLER(AST_NEG_OP, check_neg_op) \
 STATEMENT_HANDLER(AST_NOT_OP, check_not_op) \
 STATEMENT_HANDLER(AST_OCTAL_LITERAL, check_octal_literal) \
 STATEMENT_HANDLER(AST_PARENTHESIZED_EXPRESSION, check_parenthesized_expression) \
 STATEMENT_HANDLER(AST_PLUS_OP, check_plus_op) \
 STATEMENT_HANDLER(AST_POWER_OP, check_power_op) \
 STATEMENT_HANDLER(AST_STRING_LITERAL, check_string_literal) \
 STATEMENT_HANDLER(AST_USER_DEFINED_UNARY_OP, check_user_defined_unary_op) \
 STATEMENT_HANDLER(AST_SYMBOL, check_symbol) \
 STATEMENT_HANDLER(AST_ASSIGNMENT, check_assignment) \
 STATEMENT_HANDLER(AST_AMBIGUITY, disambiguate_expression) 

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
      static void _handler(AST, decl_context_t); 
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
       expression_set_error(e); \
       return; \
    }\
}

#define RETURN_IF_ERROR_1(t1, e) \
{ \
    if (is_error_type(t1)) \
    { \
       expression_set_error(e); \
       return; \
    } \
}

static void fortran_check_expression_impl_(AST expression, decl_context_t decl_context)
{
    ERROR_CONDITION(expression == NULL, "Invalid tree for expression", 0);
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

    // Do not recalculate again
    if (expression_get_type(expression) != NULL)
        return;

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
    (handler->handler)(expression, decl_context);

    DEBUG_CODE()
    {
        if (!expression_is_constant(expression))
        {
            fprintf(stderr, "EXPRTYPE: %s: '%s' has type '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression),
                    print_declarator(expression_get_type(expression)));
        }
        else
        {
            fprintf(stderr, "EXPRTYPE: %s: '%s' has type '%s' with a constant value of '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression),
                    print_declarator(expression_get_type(expression)),
                    fortran_prettyprint_in_buffer(const_value_to_tree(expression_get_constant(expression))));
        }
    }

    if (CURRENT_CONFIGURATION->strict_typecheck)
    {
        if (expression_get_type(expression) == NULL
                || expression_is_error(expression))
        {
            internal_error("%s: invalid expression '%s'\n",
                    ast_location(expression),
                    fortran_prettyprint_in_buffer(expression));
        }
    }
}

static type_t* compute_result_of_intrinsic_operator(AST expr, type_t* lhs_type, type_t* rhs_type);

static void common_binary_check(AST expr, decl_context_t decl_context);
static void common_unary_check(AST expr, decl_context_t decl_context);

static void check_add_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_ac_value_list(AST ac_value_list, decl_context_t decl_context)
{
    type_t* current_type = NULL;
    AST it;
    for_each_element(ac_value_list, it)
    {
        AST ac_value = ASTSon1(it);

        if (ASTType(ac_value) == AST_IMPLIED_DO)
        {
            AST implied_do_ac_value = ASTSon0(ac_value);

            decl_context_t new_context = fortran_new_block_context(decl_context);

            AST implied_do_control = ASTSon1(ac_value);
            AST ac_do_variable = ASTSon0(implied_do_control);
            AST lower_bound = ASTSon1(implied_do_control);
            AST upper_bound = ASTSon2(implied_do_control);
            AST stride = ASTSon3(implied_do_control);

            fortran_check_expression_impl_(lower_bound, decl_context);
            fortran_check_expression_impl_(upper_bound, decl_context);
            if (stride != NULL)
                fortran_check_expression_impl_(stride, decl_context);

            scope_entry_t* do_variable = new_symbol(new_context, new_context.current_scope,
                    ASTText(ac_do_variable));

            do_variable->kind = SK_VARIABLE;
            do_variable->type_information 
                = get_const_qualified_type(get_signed_int_type());
            do_variable->file = ASTFileName(ac_do_variable);
            do_variable->line = ASTLine(ac_do_variable);

            check_ac_value_list(implied_do_ac_value, new_context);
        }
        else
        {
            fortran_check_expression_impl_(ac_value, decl_context);
        }

        if (is_error_type(expression_get_type(ac_value)))
        {
            expression_set_error(ac_value_list);
            return;
        }
        else if (current_type == NULL)
        {
            current_type = expression_get_type(ac_value);
            expression_set_type(ac_value_list, current_type);
        }
    }
}

static void check_array_constructor(AST expr, decl_context_t decl_context)
{
    AST ac_spec = ASTSon0(expr);
    AST type_spec = ASTSon0(ac_spec);

    if (type_spec != NULL)
    {
        running_error("%s: sorry: type specifier in array constructors not supported\n",
                ast_location(type_spec));
    }

    AST ac_value_list = ASTSon1(ac_spec);
    check_ac_value_list(ac_value_list, decl_context);

    expression_set_type(expr, expression_get_type(ac_value_list));
}

static void check_array_ref(AST expr, decl_context_t decl_context)
{
    fortran_check_expression_impl_(ASTSon0(expr), decl_context);

    if (is_error_type(expression_get_type(ASTSon0(expr))))
    {
        expression_set_error(expr);
        return;
    }

    char symbol_is_invalid = 0;

    type_t* array_type = NULL;
    type_t* synthesized_type = NULL;

    int rank_of_type = -1;

    scope_entry_t* symbol = expression_get_symbol(ASTSon0(expr));
    if (symbol == NULL
            || (!is_array_type(symbol->type_information)
                && !is_pointer_to_array_type(symbol->type_information)))
    {
        symbol_is_invalid = 1;
    }
    else
    {
        array_type = symbol->type_information;
        if (is_pointer_to_array_type(symbol->type_information))
            array_type = pointer_type_get_pointee_type(symbol->type_information);

        synthesized_type = get_rank0_type(array_type);
        rank_of_type = get_rank_of_type(array_type);

        // get_rank_of_type normally does not take into account the array of chars
        if (is_fortran_character_type(array_type))
            rank_of_type++;
    }

    AST subscript_list = ASTSon1(expr);

    int num_subscripts = 0;
    AST it;
    for_each_element(subscript_list, it)
    {
        AST subscript = ASTSon1(it);

        if (ASTType(subscript) == AST_SUBSCRIPT_TRIPLET)
        {
            AST lower = ASTSon0(subscript);
            AST upper = ASTSon1(subscript);
            AST stride = ASTSon2(subscript);
            if (lower != NULL)
                fortran_check_expression_impl_(lower, decl_context);
            if (upper != NULL)
                fortran_check_expression_impl_(upper, decl_context);
            if (stride != NULL)
                fortran_check_expression_impl_(stride, decl_context);

            // Do not attempt to compute at the moment the sizes of the bounds
            // maybe we will in the future
            if (!symbol_is_invalid)
            {
                synthesized_type = get_array_type_bounds(synthesized_type, NULL, NULL, decl_context);
            }

            // FIXME - Mark subscript triplets
        }
        else
        {
            fortran_check_expression_impl_(subscript, decl_context);
        }
        num_subscripts++;
    }

    if (symbol_is_invalid)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: data reference '%s' does not designate an array\n",
                    ast_location(expr), fortran_prettyprint_in_buffer(ASTSon0(expr)));
        }
        expression_set_error(expr);
        return;
    }

    if (num_subscripts != rank_of_type)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: mismatch in subscripts of array reference, expecting %d got %d\n",
                    ast_location(expr),
                    get_rank_of_type(symbol->type_information),
                    num_subscripts);
        }
        expression_set_error(expr);
        return;
    }

    expression_set_type(expr, synthesized_type);

    ASTAttrSetValueType(expr, LANG_IS_ARRAY_SUBSCRIPT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_SUBSCRIPTED_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expr)));
    ASTAttrSetValueType(expr, LANG_SUBSCRIPT_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expr)));
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

static void compute_boz_literal(AST expr, const char *valid_prefix, int base)
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

    expression_set_type(expr, get_signed_int_type());
    expression_set_constant(expr, const_value_get(value, 4, 1));

    ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_INTEGER_LITERAL, tl_type_t, tl_bool(1));
}


static void check_binary_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    compute_boz_literal(expr, "b", 2);
}

static void check_boolean_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    if (strcasecmp(ASTText(expr), ".true.") == 0)
    {
        expression_set_type(expr, get_bool_type());
        expression_set_constant(expr, const_value_get_one(1, 1));
    }
    else if (strcasecmp(ASTText(expr), ".false.") == 0)
    {
        expression_set_type(expr, get_bool_type());
        expression_set_constant(expr, const_value_get_zero(1, 1));
    }
    else
    {
        internal_error("Invalid boolean literal", 0);
    }
    ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_BOOLEAN_LITERAL, tl_type_t, tl_bool(1));
}

static void check_complex_literal(AST expr, decl_context_t decl_context)
{
    // Const value does not support yet complex numbers, simply compute its
    // type
    AST real_part = ASTSon0(expr);
    AST imag_part = ASTSon1(expr);

    fortran_check_expression_impl_(real_part, decl_context);
    if (is_error_type(expression_get_type(real_part)))
    {
        expression_set_error(imag_part);
        expression_set_error(expr);
        return;
    }

    fortran_check_expression_impl_(imag_part, decl_context);
    if (is_error_type(expression_get_type(imag_part)))
    {
        expression_set_error(expr);
        return;
    }

    type_t* real_part_type = expression_get_type(real_part);
    type_t* imag_part_type = expression_get_type(imag_part);

    if (is_integer_type(real_part_type)
            && is_integer_type(imag_part_type))
    {
        expression_set_type(expr, get_complex_type(get_float_type()));
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

        expression_set_type(expr, get_complex_type(element_type));
    }
    else
    {
        running_error("%s: error: invalid complex constant '%s'\n", 
                ast_location(expr),
                fortran_prettyprint_in_buffer(expr));
    }

    ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_COMPLEX_LITERAL, tl_type_t, tl_bool(1));
}

static void check_component_ref(AST expr, decl_context_t decl_context)
{
    fortran_check_expression_impl_(ASTSon0(expr), decl_context);

    type_t* t = expression_get_type(ASTSon0(expr));

    if (is_error_type(t))
    {
        expression_set_error(expr);
        return;
    }

    if (!is_pointer_to_class_type(t)
            && !is_class_type(t))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: '%s' does not denote a derived type\n",
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(ASTSon0(expr)));
        }
        expression_set_error(expr);
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
            fprintf(stderr, "%s: warning: '%s' is not a component of '%s'\n",
                    ast_location(expr),
                    field,
                    fortran_print_type_str(class_type));
        }
        expression_set_error(expr);
        return;
    }

    expression_set_type(expr, entry->type_information);
    expression_set_symbol(expr, entry);

    ASTAttrSetValueType(ASTSon1(expr), LANG_IS_ACCESSED_MEMBER, tl_type_t, tl_bool(1));

    ASTAttrSetValueType(expr, LANG_ACCESSED_ENTITY, tl_type_t, tl_ast(ASTSon0(expr)));
    ASTAttrSetValueType(expr, LANG_ACCESSED_MEMBER, tl_type_t, tl_ast(ASTSon1(expr)));

    if (is_pointer_to_class_type(t))
    {
        ASTAttrSetValueType(expr, LANG_IS_POINTER_MEMBER_ACCESS, tl_type_t, tl_bool(1));
    }
    else
    {
        ASTAttrSetValueType(expr, LANG_IS_MEMBER_ACCESS, tl_type_t, tl_bool(1));
    }
}

static void check_concat_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
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
        scope_entry_t* sym = query_name(decl_context, p);
        if (sym == NULL
                || sym->kind != SK_VARIABLE
                || !is_const_qualified_type(sym->type_information))
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: invalid kind '%s'\n", 
                        ast_location(expr), 
                        p);
            }
            return 0;
        }

        ERROR_CONDITION(sym->expression_value == NULL,
                "Invalid constant for kind '%s'", sym->symbol_name);

        ERROR_CONDITION(!expression_is_constant(sym->expression_value),
                "Invalid nonconstant expression for kind '%s'", 
                fortran_prettyprint_in_buffer(sym->expression_value));

        return const_value_cast_to_4(expression_get_constant(sym->expression_value));
    }
}

static void check_decimal_literal(AST expr, decl_context_t decl_context)
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
            expression_set_error(expr);
            return;
        }
    }

    long long int value = strtoll(decimal_text, NULL, 10);

    expression_set_type(expr, choose_int_type_from_kind(expr, kind));
    expression_set_constant(expr, const_value_get(value, kind, 1));

    ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_INTEGER_LITERAL, tl_type_t, tl_bool(1));

}

static void check_derived_type_constructor(AST expr, decl_context_t decl_context)
{
    AST derived_type_spec = ASTSon0(expr);
    AST component_spec_list = ASTSon1(expr);

    AST type_param_spec_list = ASTSon1(derived_type_spec);
    if (type_param_spec_list != NULL)
    {
        running_error("%s: sorry: derived types with type parameters not supported\n", ast_location(expr));
    }

    AST derived_name = ASTSon0(derived_type_spec);
    scope_entry_t* entry = query_name(decl_context, ASTText(derived_name));

    if (entry == NULL
            || entry->kind != SK_CLASS)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: '%s' is not a derived-type-name\n",
                    ast_location(expr),
                    ASTText(derived_name));
        }
        expression_set_error(expr);
        return;
    }

    // decl_context_t class_context = class_type_get_inner_context(entry->type_information);

    // FIXME - We should do some more things here
    if (component_spec_list != NULL)
    {
        AST it;
        for_each_element(component_spec_list, it)
        {
            AST component_spec = ASTSon1(it);
            AST component_data_source = ASTSon1(component_spec);

            fortran_check_expression_impl_(component_data_source, decl_context);
        }
    }

    expression_set_type(expr, entry->type_information);

    ASTAttrSetValueType(expr, LANG_IS_EXPLICIT_TYPE_CONVERSION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_EXPLICIT_TYPE_CONVERSION_ARGS, tl_type_t, tl_ast(component_spec_list));
}

static void check_different_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_div_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_equal_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_floating_literal(AST expr, decl_context_t decl_context)
{
   const char* floating_text = ASTText(expr);

   // Our constant evaluation system does not support floats yet so simply
   // compute the type

   int kind = 4;
   const char *q = strchr(floating_text, '_');
   if (q != NULL)
   {
       q++;
       kind = compute_kind_from_literal(q, expr, decl_context);
       if (kind == 0)
       {
           expression_set_error(expr);
           return;
       }
   }

   expression_set_type(expr, choose_float_type_from_kind(expr, kind));

   ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
   ASTAttrSetValueType(expr, LANG_IS_FLOATING_LITERAL, tl_type_t, tl_bool(1));
}


typedef
struct actual_argument_info_tag
{
    const char* keyword;
    type_t* type;
} actual_argument_info_t;

#define MAX_ARGUMENTS 128

static scope_entry_t* get_specific_interface(scope_entry_t* symbol, int num_arguments, actual_argument_info_t* temp_argument_types)
{
    scope_entry_t* result = NULL;
    int i;
    for (i = 0; i < symbol->entity_specs.num_related_symbols; i++)
    {
        scope_entry_t* specific_symbol = symbol->entity_specs.related_symbols[i];

        char ok = 1;

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_types[MAX_ARGUMENTS];
        memset(argument_types, 0, sizeof(argument_types));

        for (i = 0; (i < num_arguments) && ok; i++)
        {
            int position = -1;
            if (temp_argument_types[i].keyword == NULL)
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

                    if (strcasecmp(related_sym->symbol_name, temp_argument_types[i].keyword) == 0)
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
            argument_types[position].type = temp_argument_types[i].type;
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

static void check_function_call(AST expr, decl_context_t decl_context)
{
    char is_call_stmt = (ASTText(expr) != NULL
            && (strcmp(ASTText(expr), "call") == 0));

    AST procedure_designator = ASTSon0(expr);
    AST actual_arg_spec_list = ASTSon1(expr);

    if (ASTType(procedure_designator) == AST_SYMBOL
            && is_call_stmt)
    {
        scope_entry_t* call_sym = query_name(decl_context, ASTText(procedure_designator));
        if (call_sym == NULL)
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
            if (call_sym->entity_specs.is_implicit_basic_type)
            {
                call_sym->kind = SK_FUNCTION;
                call_sym->type_information = get_nonproto_function_type(NULL, 0);
                call_sym->entity_specs.is_implicit_basic_type = 0;
                call_sym->entity_specs.is_extern = 1;
            }
        }
        expression_set_symbol(procedure_designator, call_sym);
        expression_set_type(procedure_designator, call_sym->type_information);
    }
    else
    {
        fortran_check_expression_impl_(procedure_designator, decl_context);
    }

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
            }
            else if (with_keyword) // keyword == NULL
            {
                running_error("%s: error: in function call, '%s' argument requires a keyword\n",
                        ast_location(actual_arg_spec),
                        fortran_prettyprint_in_buffer(actual_arg_spec));
            }
            AST actual_arg = ASTSon1(actual_arg_spec);

            if (ASTType(actual_arg) != AST_ALTERNATE_RESULT_SPEC)
            {
                fortran_check_expression_impl_(actual_arg, decl_context);

                if (is_error_type(expression_get_type(actual_arg)))
                {
                    wrong_arg_spec_list = 1;
                }
            }
            else
            {
                if (!is_call_stmt)
                {
                    running_error("%s: error: only CALL statement allows an alternate return\n",
                            ast_location(actual_arg_spec));
                }
                if (!seen_alternate_return)
                {
                    seen_alternate_return = 1;
                }
                else
                {
                    running_error("%s: error: in a procedure reference an alternate return must be at the last position\n", 
                            ast_location(actual_arg_spec));
                }
            }
        }

        if (wrong_arg_spec_list)
        {
            expression_set_error(expr);
            return;
        }
    }

    if (is_error_type(expression_get_type(procedure_designator)))
    {
        expression_set_error(expr);
        return;
    }

    scope_entry_t* symbol = expression_get_symbol(procedure_designator);

    if (symbol != NULL
            && symbol->kind == SK_VARIABLE
            && is_void_type(symbol->type_information))
    {
        // Upgrade the symbol to a function with unknown arguments
        symbol->kind = SK_FUNCTION;

        type_t* return_type = symbol->type_information;
        if (is_call_stmt)
            return_type = NULL;

        symbol->type_information = get_nonproto_function_type(return_type, 0);
    }

    if (symbol == NULL
            || symbol->kind != SK_FUNCTION)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: in %s, '%s' does not designate a procedure\n",
                    ast_location(expr),
                    !is_call_stmt ? "function reference" : "CALL statement",
                    fortran_prettyprint_in_buffer(procedure_designator));
        }
        expression_set_error(expr);
        return;
    }

    if (inside_context_of_symbol(decl_context, symbol)
            && !symbol->entity_specs.is_recursive)
    {
        running_error("%s: error: cannot call recursively '%s'\n",
                ast_location(expr),
                fortran_prettyprint_in_buffer(procedure_designator));
    }

    actual_argument_info_t temp_argument_types[MAX_ARGUMENTS];
    memset(temp_argument_types, 0, sizeof(temp_argument_types));
    int num_arguments = 0;

    // Gather arguments (syntactic checking has happened before)
    int i;
    if (actual_arg_spec_list != NULL)
    {
        AST it;
        for_each_element(actual_arg_spec_list, it)
        {
            AST actual_arg_spec = ASTSon1(it);
            AST keyword = ASTSon0(actual_arg_spec);
            AST actual_arg = ASTSon1(actual_arg_spec);

            if (ASTType(actual_arg) == AST_ALTERNATE_RESULT_SPEC)
                continue;

            if (keyword != NULL)
            {
                temp_argument_types[num_arguments].keyword = ASTText(keyword);
            }
            temp_argument_types[num_arguments].type = expression_get_type(actual_arg);

            num_arguments++;
        }
    }

    type_t* return_type = NULL; 
    // This is a generic procedure reference
    if (symbol->entity_specs.is_builtin)
    {
        // OK, this is a builtin, aka a dreadful Fortran intrinsic, its type
        // will be a computed function type

        computed_function_type_t fun = computed_function_type_get_computing_function(symbol->type_information);

        int num_args = 0;
        AST* arg_list = NULL;
        type_t** type_list = NULL;
        if (actual_arg_spec_list != NULL)
        {
            AST it;
            for_each_element(actual_arg_spec_list, it)
            {
                AST actual_arg_spec = ASTSon1(it);
                AST actual_arg = ASTSon1(actual_arg_spec);

                if (ASTType(actual_arg) == AST_ALTERNATE_RESULT_SPEC)
                    continue;

                P_LIST_ADD(arg_list, num_args, actual_arg_spec);
                num_args--;
                P_LIST_ADD(type_list, num_args, expression_get_type(actual_arg));
            }
        }

        scope_entry_t* entry = fun(symbol, type_list, arg_list, num_arguments);
        if (entry == NULL)
        {
            fprintf(stderr, "%s: cannot call intrinsic '%s'\n", 
                    ast_location(expr),
                    symbol->symbol_name);
            expression_set_error(expr);
            return;
        }

        return_type = function_type_get_return_type(entry->type_information);
    }
    else
    {
        if (symbol->entity_specs.is_generic_spec)
        {
            scope_entry_t* specific_symbol = get_specific_interface(symbol, num_arguments, temp_argument_types);
            if (specific_symbol == NULL)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: no specific interface matches generic interface '%s' in function reference\n",
                            ast_location(expr),
                            symbol->symbol_name);
                }
                expression_set_error(expr);
                return;
            }
            symbol = specific_symbol;
        }

        // This is now a specfic procedure reference
        ERROR_CONDITION (!is_function_type(symbol->type_information), "Invalid type for function symbol!\n", 0);

        // Complete with those arguments that are not present
        // Reorder the arguments
        actual_argument_info_t argument_types[MAX_ARGUMENTS];
        memset(argument_types, 0, sizeof(argument_types));

        for (i = 0; i < num_arguments; i++)
        {
            int position = -1;
            if (temp_argument_types[i].keyword == NULL)
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

                    if (strcasecmp(related_sym->symbol_name, temp_argument_types[i].keyword) == 0)
                    {
                        position = related_sym->entity_specs.parameter_position;
                    }
                }
                if (position < 0)
                {
                    running_error("%s: error: keyword '%s' is not a dummy argument of function '%s'\n",
                            ast_location(expr), 
                            temp_argument_types[i].keyword,
                            symbol->symbol_name);
                }
            }

            if (argument_types[position].type != NULL)
            {
                running_error("%s: error: argument keyword '%s' specified more than once\n",
                        ast_location(expr), temp_argument_types[i].keyword);
            }
            argument_types[position].type = temp_argument_types[i].type;
        }


        // Now complete with the optional ones
        for (i = 0; i < symbol->entity_specs.num_related_symbols; i++)
        {
            scope_entry_t* related_sym = symbol->entity_specs.related_symbols[i];

            if (related_sym->entity_specs.is_parameter)
            {
                if (argument_types[related_sym->entity_specs.parameter_position].type == NULL)
                {
                    if (related_sym->entity_specs.is_optional)
                    {
                        argument_types[related_sym->entity_specs.parameter_position].type = related_sym->type_information;
                        num_arguments++;
                    }
                    else 
                    {
                        running_error("%s: error: dummy argument '%s' of function '%s' has not been specified in function reference\n",
                                ast_location(expr),
                                related_sym->symbol_name,
                                symbol->symbol_name);
                    }
                }
            }
        }

        if (!function_type_get_lacking_prototype(symbol->type_information) 
                && num_arguments > function_type_get_num_parameters(symbol->type_information))
        {
            fprintf(stderr, "%s: warning: too many actual arguments in function reference to '%s'\n",
                    ast_location(expr),
                    symbol->symbol_name);
            expression_set_error(expr);
            return;
        }

        ERROR_CONDITION(!function_type_get_lacking_prototype(symbol->type_information) 
                && (num_arguments != function_type_get_num_parameters(symbol->type_information)), 
                "Mismatch between arguments and the type of the function %d != %d", 
                num_arguments,
                function_type_get_num_parameters(symbol->type_information));

        char argument_type_mismatch = 0;
        int common_rank = -1;
        if (!function_type_get_lacking_prototype(symbol->type_information))
        {
            actual_argument_info_t fixed_argument_types[MAX_ARGUMENTS];
            memcpy(fixed_argument_types, argument_types, sizeof(fixed_argument_types));

            if (symbol->entity_specs.is_elemental)
            {
                // We may have to adjust the ranks, first check that all the
                // ranks match
                char ok = 1;
                for (i = 0; i < num_arguments && ok; i++)
                {
                    int current_rank = get_rank_of_type(fixed_argument_types[i].type); 
                    if (common_rank < 0)
                    {
                        common_rank = current_rank;
                    }
                    else if (common_rank != current_rank)
                    {
                        ok = 0;
                    }
                }

                if (ok)
                {
                    // Remove rank if they match, otherwise let it fail later
                    for (i = 0; i < num_arguments && ok; i++)
                    {
                        fixed_argument_types[i].type = get_rank0_type(fixed_argument_types[i].type);
                    }
                }
            }

            for (i = 0; i < num_arguments; i++)
            {
                type_t* formal_type = function_type_get_parameter_type_num(symbol->type_information, i);
                type_t* real_type = fixed_argument_types[i].type;

                if (!equivalent_tkr_types(formal_type, real_type))
                {
                    if (!checking_ambiguity())
                    {
                        fprintf(stderr, "%s: warning: type mismatch in argument %d between the "
                                "real argument %s and the dummy argument %s\n",
                                ast_location(expr),
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
            expression_set_error(expr);
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
            fprintf(stderr, "%s: warning: invalid function reference to a SUBROUTINE\n",
                    ast_location(expr));
            expression_set_error(expr);
            return;
        }
        return_type = get_void_type();
    }
    else
    {
        if (is_call_stmt)
        {
            fprintf(stderr, "%s: warning: invalid CALL statement to a FUNCTION\n",
                    ast_location(expr));
            expression_set_error(expr);
            return;
        }
    }

    expression_set_type(expr, return_type);

    ASTAttrSetValueType(expr, LANG_IS_FUNCTION_CALL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_CALLED_EXPRESSION, tl_type_t, tl_ast(procedure_designator));
    ASTAttrSetValueType(expr, LANG_FUNCTION_ARGUMENTS, tl_type_t, tl_ast(actual_arg_spec_list));
}

static void check_greater_or_equal_than(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_greater_than(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_hexadecimal_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    // We allow X and Z
    compute_boz_literal(expr, "xz", 16);
}

static void check_image_ref(AST expr UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
    running_error("%s: sorry: image references not supported\n", 
            ast_location(expr));
}

static void check_logical_and(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_logical_equal(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_logical_or(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_lower_or_equal_than(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_lower_than(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_minus_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_mult_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static void check_neg_op(AST expr, decl_context_t decl_context)
{
    common_unary_check(expr, decl_context);
}

static void check_not_op(AST expr, decl_context_t decl_context)
{
    common_unary_check(expr, decl_context);
}

static void check_octal_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    compute_boz_literal(expr, "o", 8);
}

static void check_parenthesized_expression(AST expr, decl_context_t decl_context)
{
    fortran_check_expression_impl_(ASTSon0(expr), decl_context);
    expression_set_type(expr, expression_get_type(ASTSon0(expr)));

    if (expression_is_constant(ASTSon0(expr)))
    {
        expression_set_constant(expr, expression_get_constant(ASTSon0(expr)));
    }

    ASTAttrSetValueType(expr, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon0(expr)));
}

static void check_plus_op(AST expr, decl_context_t decl_context)
{
    common_unary_check(expr, decl_context);
}

static void check_power_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

static char* binary_expression_attr[] =
{
    [AST_MULT_OP] = LANG_IS_MULT_OP,
    [AST_DIV_OP] = LANG_IS_DIVISION_OP,
    [AST_MOD_OP] = LANG_IS_MODULUS_OP,
    [AST_ADD_OP] = LANG_IS_ADDITION_OP,
    [AST_MINUS_OP] = LANG_IS_SUBSTRACTION_OP,
    [AST_SHL_OP] = LANG_IS_SHIFT_LEFT_OP,
    [AST_SHR_OP] = LANG_IS_SHIFT_RIGHT_OP,
    [AST_LOWER_THAN] = LANG_IS_LOWER_THAN_OP,
    [AST_GREATER_THAN] = LANG_IS_GREATER_THAN_OP,
    [AST_GREATER_OR_EQUAL_THAN] = LANG_IS_GREATER_OR_EQUAL_THAN_OP,
    [AST_LOWER_OR_EQUAL_THAN] = LANG_IS_LOWER_OR_EQUAL_THAN_OP,
    [AST_EQUAL_OP] = LANG_IS_EQUAL_OP,
    [AST_DIFFERENT_OP] = LANG_IS_DIFFERENT_OP,
    [AST_BITWISE_AND] = LANG_IS_BITWISE_AND_OP,
    [AST_BITWISE_XOR] = LANG_IS_BITWISE_XOR_OP,
    [AST_BITWISE_OR] = LANG_IS_BITWISE_OR_OP,
    [AST_LOGICAL_AND] = LANG_IS_LOGICAL_AND_OP,
    [AST_LOGICAL_OR] = LANG_IS_LOGICAL_OR_OP,
    [AST_POWER_OP] = LANG_IS_POWER_OP,
    [AST_CONCAT_OP] = LANG_IS_CONCAT_OP,
};

static void common_binary_intrinsic_check(AST expr, type_t* lhs_type, type_t* rhs_type);
static void common_binary_check(AST expr, decl_context_t decl_context)
{
    AST lhs = ASTSon0(expr);
    AST rhs = ASTSon1(expr);
    fortran_check_expression_impl_(lhs, decl_context);
    fortran_check_expression_impl_(rhs, decl_context);

    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_2(lhs_type, rhs_type, expr);

    common_binary_intrinsic_check(expr, lhs_type, rhs_type);

    ASTAttrSetValueType(expr, LANG_IS_BINARY_OPERATION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, binary_expression_attr[ASTType(expr)], tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expr)));
    ASTAttrSetValueType(expr, LANG_RHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expr)));
}

static void common_binary_intrinsic_check(AST expr, type_t* lhs_type, type_t* rhs_type)
{
    expression_set_type(expr, compute_result_of_intrinsic_operator(expr, lhs_type, rhs_type));
}

static void common_unary_intrinsic_check(AST expr, type_t* rhs_type);
static void common_unary_check(AST expr, decl_context_t decl_context) 
{
    AST rhs = ASTSon0(expr);
    fortran_check_expression_impl_(rhs, decl_context);

    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_1(rhs_type, expr);

    common_unary_intrinsic_check(expr, rhs_type);
}

static void common_unary_intrinsic_check(AST expr, type_t* rhs_type)
{
    expression_set_type(expr, compute_result_of_intrinsic_operator(expr, NULL, rhs_type));
}

static void check_string_literal(AST expr, decl_context_t decl_context)
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
            running_error("%s: error: KIND specificier is too long\n",
                    ast_location(expr));
            expression_set_error(expr);
            return;
        }
        literal++;

        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: ignoring KIND=%s of character-literal\n",
                    kind,
                    ast_location(expr));
        }
    }

    int length = strlen(literal);

    AST one = const_value_to_tree(const_value_get_one(4, 1));
    AST length_tree = const_value_to_tree(const_value_get(length, 4, 1));

    expression_set_type(expr, get_array_type_bounds(get_signed_char_type(), one, length_tree, decl_context));

    ASTAttrSetValueType(expr, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_STRING_LITERAL, tl_type_t, tl_bool(1));
}

static void check_user_defined_unary_op(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    running_error("%s: sorry: not yet implemented\n", ast_location(expr));
}

static char function_has_result(scope_entry_t* entry)
{
    int i;
    for (i = 0; i < entry->entity_specs.num_related_symbols; i++)
    {
        if (entry->entity_specs.related_symbols[i]->entity_specs.is_result)
            return 1;
    }
    return 0;
}

static void check_symbol(AST expr, decl_context_t decl_context)
{
    scope_entry_t* entry = query_name(decl_context, ASTText(expr));

    if (entry == NULL)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: unknown entity '%s'\n",
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(expr));
        }
        expression_set_error(expr);
        return;
    }

    if (entry->kind == SK_VARIABLE)
    {
        // It might happen that dummy arguments/result do not have any implicit
        // type here (because the input code is wrong)
        if (is_error_type(entry->type_information))
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: entity '%s' does not have any IMPLICIT type\n",
                        ast_location(expr),
                        fortran_prettyprint_in_buffer(expr));
            }
            expression_set_error(expr);
            return;
        }

        expression_set_symbol(expr, entry);
        expression_set_type(expr, entry->type_information);

        if (is_const_qualified_type(entry->type_information)
                && entry->expression_value != NULL
                && expression_is_constant(entry->expression_value))
        {
            // PARAMETER are const qualified
            expression_set_constant(expr, expression_get_constant(entry->expression_value));
        }
    }
    else if (entry->kind == SK_FUNCTION)
    {
        // This function has a RESULT(X) so it cannot be used as a variable
        expression_set_symbol(expr, entry);
        expression_set_type(expr, entry->type_information);
    }
    else
    {
        expression_set_error(expr);
    }

    ASTAttrSetValueType(expr, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(expr));
}

static void check_assignment(AST expr, decl_context_t decl_context)
{
    AST lvalue = ASTSon0(expr);
    AST rvalue = ASTSon1(expr);

    fortran_check_expression_impl_(lvalue, decl_context);

    type_t* lvalue_type = expression_get_type(lvalue);
    if (is_error_type(lvalue_type))
    {
        expression_set_error(expr);
        return;
    }

    fortran_check_expression_impl_(rvalue, decl_context);

    type_t* rvalue_type = expression_get_type(rvalue);
    if (is_error_type(rvalue_type))
    {
        expression_set_error(expr);
        return;
    }

    if (expression_has_symbol(lvalue))
    {
        scope_entry_t* sym = expression_get_symbol(lvalue);
        if (sym->kind == SK_FUNCTION)
        {
            if(function_type_get_return_type(sym->type_information) != NULL
                    && !function_has_result(sym))
            {
                lvalue_type = function_type_get_return_type(sym->type_information);
            }
            else
            {
                running_error("%s: '%s' is not a variable\n",
                        ast_location(expr), fortran_prettyprint_in_buffer(lvalue));

            }
        }
        else if (sym->kind == SK_VARIABLE)
        {
            // Do nothing
        }
        else
        {
            internal_error("Invalid symbol kind in left part of assignment", 0);
        }
    }

    expression_set_type(expr, lvalue_type);

    if (expression_is_constant(rvalue))
    {
        expression_set_constant(expr, expression_get_constant(rvalue));
    }

    ASTAttrSetValueType(expr, LANG_IS_BINARY_OPERATION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_IS_ASSIGNMENT, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expr)));
    ASTAttrSetValueType(expr, LANG_RHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expr)));
}

static void disambiguate_expression(AST expr, decl_context_t decl_context)
{
    int num_ambig = ast_get_num_ambiguities(expr);

    int i;
    int correct_option = -1;
    int function_call = -1;
    for (i = 0; i < num_ambig; i++)
    {
        AST current_expr = ast_get_ambiguity(expr, i);
        switch (ASTType(current_expr))
        {
            case AST_FUNCTION_CALL:
            case AST_ARRAY_REF:
            case AST_DERIVED_TYPE_CONSTRUCTOR:
                {
                    enter_test_expression();
                    fortran_check_expression_impl_(current_expr, decl_context);
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

        if (!is_error_type(expression_get_type(current_expr)))
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
    expression_clear_computed_info(expr);
    fortran_check_expression_impl_(expr, decl_context);
}

static type_t* common_kind(type_t* t1, type_t* t2)
{
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
    if (is_pointer_to_fortran_character_type(t1))
        t1 = pointer_type_get_pointee_type(t1);
    if (is_pointer_to_fortran_character_type(t2))
        t1 = pointer_type_get_pointee_type(t2);

    AST length1 = array_type_get_array_size_expr(t1);
    AST length2 = array_type_get_array_size_expr(t2);

    type_t* char1 = array_type_get_element_type(t1);
    type_t* char2 = array_type_get_element_type(t2);

    if (!equivalent_types(char1, char2))
        return NULL;

    type_t* result = NULL;
    if (length1 != NULL
            && length2 != NULL)
    {
        AST new_lower_bound = const_value_to_tree(const_value_get_one(4, 1));
        AST new_upper_bound = 
            ASTMake2(AST_ADD_OP,
                    ast_copy_for_instantiation(array_type_get_array_size_expr(t1)),
                    ast_copy_for_instantiation(array_type_get_array_size_expr(t2)),
                    NULL, 0, NULL);

        result = get_array_type_bounds(char1, 
                new_lower_bound, 
                new_upper_bound, 
                array_type_get_array_size_expr_context(t1));
    }
    else
    {
        result = get_array_type(char1, NULL, array_type_get_array_size_expr_context(t1));
    }

    return result;
}

static type_t* logical_type(type_t* t1 UNUSED_PARAMETER, type_t* t2 UNUSED_PARAMETER)
{
    return get_bool_type();
}

static char is_logical_type(type_t* t1)
{
    return is_bool_type(t1);
}

typedef
struct operand_types_tag
{
    char (*lhs_type)(type_t*);
    char (*rhs_type)(type_t*);
    type_t* (*common_type)(type_t*, type_t*);
} operand_types_t;

static operand_types_t arithmetic_unary[] = 
{
    { NULL, is_integer_type, second_type },
    { NULL, is_floating_type, second_type },
    { NULL, is_complex_type, second_type },
};

static operand_types_t arithmetic_binary[] =
{
    { is_integer_type, is_integer_type, common_kind },
    { is_integer_type, is_floating_type,  second_type },
    { is_integer_type, is_complex_type, second_type },
    { is_floating_type, is_integer_type, first_type },
    { is_floating_type, is_floating_type, common_kind },
    { is_floating_type, is_complex_type, second_type },
    { is_complex_type, is_integer_type, first_type },
    { is_complex_type, is_floating_type, first_type },
    { is_complex_type, is_complex_type, common_kind },
};

static char is_fortran_character_type_or_pointer_to(type_t* t)
{
    return is_pointer_to_fortran_character_type(t)
        || is_fortran_character_type(t);
}

static operand_types_t concat_op[] = 
{
    { is_fortran_character_type_or_pointer_to, is_fortran_character_type_or_pointer_to, combine_character_array },
};

static operand_types_t relational_equality[] =
{
    { is_integer_type, is_integer_type, logical_type },
    { is_integer_type, is_floating_type,  logical_type },
    { is_integer_type, is_complex_type, logical_type },
    { is_floating_type, is_integer_type, logical_type },
    { is_floating_type, is_floating_type, logical_type },
    { is_floating_type, is_complex_type, logical_type },
    { is_complex_type, is_integer_type, logical_type },
    { is_complex_type, is_floating_type, logical_type },
    { is_complex_type, is_complex_type, logical_type },
    { is_fortran_character_type, is_fortran_character_type, logical_type },
};

static operand_types_t relational_weak[] =
{
    { is_integer_type, is_integer_type, logical_type },
    { is_integer_type, is_floating_type,  logical_type },
    { is_floating_type, is_integer_type, logical_type },
    { is_floating_type, is_floating_type, logical_type },
    { is_fortran_character_type, is_fortran_character_type, logical_type },
};

static operand_types_t logical_unary[] =
{
    { NULL, is_logical_type, second_type },
};

static operand_types_t logical_binary[] =
{
    { is_logical_type, is_logical_type, common_kind }
};

typedef struct operand_map_tag
{
    node_t node_type;
    operand_types_t* operand_types;
    int num_operands;

    void (*compute_const)(AST expr, AST lhs, AST rhs);
} operand_map_t;

#define HANDLER_MAP(_node_op, _operands, _compute_const) \
{ _node_op, _operands, sizeof(_operands) / sizeof(_operands[0]), _compute_const }

static void const_unary_plus(AST expr, AST lhs, AST rhs);
static void const_unary_neg(AST expr, AST lhs, AST rhs);
static void const_bin_add(AST expr, AST lhs, AST rhs);
static void const_bin_sub(AST expr, AST lhs, AST rhs);
static void const_bin_mult(AST expr, AST lhs, AST rhs);
static void const_bin_div(AST expr, AST lhs, AST rhs);
static void const_bin_power(AST expr, AST lhs, AST rhs);
static void const_bin_equal(AST expr, AST lhs, AST rhs);
static void const_bin_not_equal(AST expr, AST lhs, AST rhs);
static void const_bin_lt(AST expr, AST lhs, AST rhs);
static void const_bin_lte(AST expr, AST lhs, AST rhs);
static void const_bin_gt(AST expr, AST lhs, AST rhs);
static void const_bin_gte(AST expr, AST lhs, AST rhs);
static void const_unary_not(AST expr, AST lhs, AST rhs);
static void const_bin_and(AST expr, AST lhs, AST rhs);
static void const_bin_or(AST expr, AST lhs, AST rhs);

static operand_map_t operand_map[] =
{
    // Arithmetic unary
    HANDLER_MAP(AST_PLUS_OP, arithmetic_unary, const_unary_plus),
    HANDLER_MAP(AST_NEG_OP, arithmetic_unary, const_unary_neg),
    // Arithmetic binary
    HANDLER_MAP(AST_ADD_OP, arithmetic_binary, const_bin_add),
    HANDLER_MAP(AST_MINUS_OP, arithmetic_binary, const_bin_sub),
    HANDLER_MAP(AST_MULT_OP, arithmetic_binary, const_bin_mult),
    HANDLER_MAP(AST_DIV_OP, arithmetic_binary, const_bin_div),
    HANDLER_MAP(AST_POWER_OP, arithmetic_binary, const_bin_power),
    // String concat
    HANDLER_MAP(AST_CONCAT_OP, concat_op, NULL),
    // Relational strong
    HANDLER_MAP(AST_EQUAL_OP, relational_equality, const_bin_equal),
    HANDLER_MAP(AST_DIFFERENT_OP, relational_equality, const_bin_not_equal),
    // Relational weak
    HANDLER_MAP(AST_LOWER_THAN, relational_weak, const_bin_lt),
    HANDLER_MAP(AST_LOWER_OR_EQUAL_THAN, relational_weak, const_bin_lte),
    HANDLER_MAP(AST_GREATER_THAN, relational_weak, const_bin_gt),
    HANDLER_MAP(AST_GREATER_OR_EQUAL_THAN, relational_weak, const_bin_gte),
    // Unary logical
    HANDLER_MAP(AST_NOT_OP, logical_unary, const_unary_not),
    // Binary logical
    HANDLER_MAP(AST_LOGICAL_EQUAL, logical_binary, const_bin_equal),
    HANDLER_MAP(AST_LOGICAL_DIFFERENT, logical_binary, const_bin_not_equal),
    HANDLER_MAP(AST_LOGICAL_AND, logical_binary, const_bin_and),
    HANDLER_MAP(AST_LOGICAL_OR, logical_binary, const_bin_or),
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

static void conform_types(type_t* lhs_type, type_t* rhs_type, type_t** conf_lhs_type, type_t** conf_rhs_type);

static type_t* rerank_type(type_t* rank0_common, type_t* lhs_type, type_t* rhs_type);

static type_t* compute_result_of_intrinsic_operator(AST expr, type_t* lhs_type, type_t* rhs_type)
{
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
            break;
        }
    }

    if (result == NULL)
    {
        if (lhs_type != NULL)
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: invalid operand types %s and %s for intrinsic binary operator '%s'\n",
                        ast_location(expr),
                        fortran_print_type_str(lhs_type),
                        fortran_print_type_str(rhs_type),
                        get_operator_for_expr(expr));
            }
            return get_error_type();
        }
    }

    if (value->compute_const != NULL)
    {
        if (lhs_type != NULL) 
        {
            // Binary
            value->compute_const(expr, ASTSon0(expr), ASTSon1(expr));
        }
        else
        {
            value->compute_const(expr, NULL, ASTSon0(expr));
        }
    }

    // Restore the rank of the common type
    result = rerank_type(result, lhs_type, rhs_type);

    return result;
}

const char* operator_names[] =
{
    [AST_PLUS_OP] = "+",
    [AST_NEG_OP] = "-",
    [AST_ADD_OP] = "+",
    [AST_MINUS_OP] = "-",
    [AST_MULT_OP] = "*",
    [AST_DIV_OP] = "/",
    [AST_POWER_OP] = "**",
    [AST_CONCAT_OP] = "//",
    [AST_EQUAL_OP] = "==",
    [AST_DIFFERENT_OP] = "/=",
    [AST_LOWER_THAN] = "<",
    [AST_LOWER_OR_EQUAL_THAN] = "<=",
    [AST_GREATER_THAN] = ">",
    [AST_GREATER_OR_EQUAL_THAN] = ">=",
    [AST_NOT_OP] = ".NOT.",
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

static void const_bin_(AST expr, AST lhs, AST rhs,
        const_value_t* (*compute)(const_value_t*, const_value_t*))
{
    if (expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        const_value_t* t = compute(expression_get_constant(lhs),
                expression_get_constant(rhs));
        expression_set_constant(expr, t);
    }
}

static void const_unary_(AST expr, AST lhs, const_value_t* (*compute)(const_value_t*))
{
    if (expression_is_constant(lhs))
    {
        const_value_t* t = compute(expression_get_constant(lhs));
        expression_set_constant(expr, t);
    }
}

static void const_unary_plus(AST expr, AST lhs UNUSED_PARAMETER, AST rhs)
{
    const_unary_(expr, rhs, const_value_plus);
}

static void const_unary_neg(AST expr, AST lhs UNUSED_PARAMETER, AST rhs)
{
    const_unary_(expr, rhs, const_value_neg);
}

static void const_bin_add(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_add);
}

static void const_bin_sub(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_sub);
}

static void const_bin_mult(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_mul);
}

static void const_bin_div(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_div);
}

static void const_bin_power(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_pow);
}

static void const_bin_equal(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_eq);
}

static void const_bin_not_equal(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_neq);
}

static void const_bin_lt(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_lt);
}

static void const_bin_lte(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_lte);
}

static void const_bin_gt(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_gt);
}

static void const_bin_gte(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_gte);
}

static void const_unary_not(AST expr, AST lhs UNUSED_PARAMETER, AST rhs)
{
    const_unary_(expr, rhs, const_value_not);
}

static void const_bin_and(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_and);
}

static void const_bin_or(AST expr, AST lhs, AST rhs)
{
    const_bin_(expr, lhs, rhs, const_value_or);
}

type_t* common_type_of_binary_operation(type_t* t1, type_t* t2)
{
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
