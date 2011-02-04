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
#include "cxx-ast.h"
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

static void fortran_check_expression_impl_(AST expression, decl_context_t decl_context);

char fortran_check_expression(AST a, decl_context_t decl_context)
{
    fortran_check_expression_impl_(a, decl_context);
    return (is_error_type(expression_get_type(a)));
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
        fprintf(stderr, "%s: sorry: unhandled expression %s\n", 
                ast_location(expression), 
                ast_print_node_type(ASTType(expression)));
        expression_set_error(expression);
        return;
    }
    (handler->handler)(expression, decl_context);

    DEBUG_CODE()
    {
        fprintf(stderr, "%s: '%s' has type '%s'\n",
                ast_location(expression),
                fortran_prettyprint_in_buffer(expression),
                print_declarator(expression_get_type(expression)));
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

    scope_entry_t* symbol = expression_get_symbol(ASTSon0(expr));
    if (symbol == NULL
            || (!is_array_type(symbol->type_information)
                && !is_pointer_to_array_type(symbol->type_information)))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: data reference '%s' does not designate an array\n",
                    ast_location(expr), fortran_prettyprint_in_buffer(ASTSon0(expr)));
        }
        expression_set_error(expr);
        return;
    }

    type_t* array_type = symbol->type_information;
    if (is_pointer_to_array_type(symbol->type_information))
            array_type = pointer_type_get_pointee_type(symbol->type_information);

    type_t* synthesized_type = get_rank0_type(array_type);
    int rank_of_type = get_rank_of_type(array_type);

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
            synthesized_type = get_array_type_bounds(synthesized_type, NULL, NULL, decl_context);
        }
        else
        {
            fortran_check_expression_impl_(subscript, decl_context);
        }
        num_subscripts++;
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
}

static void compute_boz_literal(AST expr, char prefix, int base)
{
    const char* literal_token = ASTText(expr);

    char literal_text[strlen(literal_token)];
    memset(literal_text, 0, sizeof(literal_text));

    char *q = literal_text;

    char had_prefix = 0;
    if (tolower(*literal_token) == prefix)
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
        if (tolower(*literal_token) != prefix)
        {
            ERROR_CONDITION(*literal_token != '\''
                    && *literal_token != '\"', "Invalid expr token!", 0);
        }
    }

    long long int value = strtoll(literal_text, NULL, base);

    expression_set_type(expr, get_signed_int_type());
    expression_set_constant(expr, const_value_get(value, 4, 1));
}


static void check_binary_literal(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    compute_boz_literal(expr, 'b', 2);
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

    if (!is_class_type(t))
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

    decl_context_t class_context = class_type_get_inner_context(t);

    const char * field = ASTText(ASTSon1(expr));

    scope_entry_t* entry = query_name(class_context, field);
    if (entry == NULL)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: '%s' is not a component of '%s'\n",
                    ast_location(expr),
                    field,
                    fortran_print_type_str(t));
        }
        expression_set_error(expr);
        return;
    }

    expression_set_type(expr, entry->type_information);
    expression_set_symbol(expr, entry);
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

    char decimal_text[strlen(c)];
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
}

static void check_function_call(AST expr, decl_context_t decl_context)
{
    AST procedure_designator = ASTSon0(expr);
    AST actual_arg_spec_list = ASTSon1(expr);

    fortran_check_expression_impl_(procedure_designator, decl_context);

    if (is_error_type(expression_get_type(procedure_designator)))
    {
        expression_set_error(expr);
        return;
    }

    scope_entry_t* symbol = expression_get_symbol(procedure_designator);

    if (symbol != NULL
            && symbol->kind == SK_VARIABLE
            && symbol->entity_specs.is_implicit)
    {
        // Upgrade the symbol to a function with unknown arguments
        symbol->kind = SK_FUNCTION;
        symbol->type_information = get_nonproto_function_type(symbol->type_information, 0);
    }

    if (symbol == NULL
            || symbol->kind != SK_FUNCTION)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: in function call, '%s' does not designate a procedure\n",
                    ast_location(expr),
                    fortran_prettyprint_in_buffer(procedure_designator));
        }
        expression_set_error(expr);
        return;
    }

    if (actual_arg_spec_list != NULL)
    {
        AST it;
        for_each_element(actual_arg_spec_list, it)
        {
            AST actual_arg_spec = ASTSon0(it);
            AST actual_arg = ASTSon1(actual_arg_spec);

            if (ASTType(actual_arg) != AST_ALTERNATE_RESULT_SPEC)
            {
                fortran_check_expression_impl_(actual_arg, decl_context);
            }
        }
    }

    expression_set_type(expr, function_type_get_return_type(symbol->type_information));
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
    compute_boz_literal(expr, 'z', 16);
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
    compute_boz_literal(expr, 'o', 8);
}

static void check_parenthesized_expression(AST expr, decl_context_t decl_context)
{
    fortran_check_expression_impl_(ASTSon0(expr), decl_context);
    expression_set_type(expr, expression_get_type(ASTSon0(expr)));
}

static void check_plus_op(AST expr, decl_context_t decl_context)
{
    common_unary_check(expr, decl_context);
}

static void check_power_op(AST expr, decl_context_t decl_context)
{
    common_binary_check(expr, decl_context);
}

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

    expression_set_type(expr, get_array_type_bounds(get_char_type(), one, length_tree, decl_context));
}

static void check_user_defined_unary_op(AST expr, decl_context_t decl_context UNUSED_PARAMETER)
{
    running_error("%s: sorry: not yet implemented\n", ast_location(expr));
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

    if (entry->kind == SK_VARIABLE
            || entry->kind == SK_FUNCTION)
    {
        expression_set_symbol(expr, entry);
        expression_set_type(expr, entry->type_information);
    }
    else
    {
        expression_set_error(expr);
        return;
    }
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

    expression_set_type(expr, lvalue_type);
}

static void disambiguate_expression(AST expr, decl_context_t decl_context)
{
    int num_ambig = ast_get_num_ambiguities(expr);

    int i;
    int correct_option = -1;
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
        // Do this for diagnostics in this case
        fortran_check_expression_impl_(ast_get_ambiguity(expr, 0), decl_context);
        expression_set_error(expr);
    }
    else
    {
        ast_replace_with_ambiguity(expr, correct_option);
    }
}

static type_t* rebuild_array_type(type_t* rank0_type, type_t* array_type);

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
} operand_map_t;

#define HANDLER_MAP(_node_op, _operands) \
{ _node_op, _operands, sizeof(_operands) / sizeof(_operands[0]) }

static operand_map_t operand_map[] =
{
    // Arithmetic unary
    HANDLER_MAP(AST_PLUS_OP, arithmetic_unary),
    HANDLER_MAP(AST_NEG_OP, arithmetic_unary),
    // Arithmetic binary
    HANDLER_MAP(AST_ADD_OP, arithmetic_binary),
    HANDLER_MAP(AST_MINUS_OP, arithmetic_binary),
    HANDLER_MAP(AST_MULT_OP, arithmetic_binary),
    HANDLER_MAP(AST_DIV_OP, arithmetic_binary),
    HANDLER_MAP(AST_POWER_OP, arithmetic_binary),
    // String concat
    HANDLER_MAP(AST_CONCAT_OP, concat_op),
    // Relational strong
    HANDLER_MAP(AST_EQUAL_OP, relational_equality),
    HANDLER_MAP(AST_DIFFERENT_OP, relational_equality),
    // Relational weak
    HANDLER_MAP(AST_LOWER_THAN, relational_weak),
    HANDLER_MAP(AST_LOWER_OR_EQUAL_THAN, relational_weak),
    HANDLER_MAP(AST_GREATER_THAN, relational_weak),
    HANDLER_MAP(AST_GREATER_OR_EQUAL_THAN, relational_weak),
    // Unary logical
    HANDLER_MAP(AST_NOT_OP, logical_unary),
    // Binary logical
    HANDLER_MAP(AST_LOGICAL_EQUAL, logical_binary),
    HANDLER_MAP(AST_LOGICAL_DIFFERENT, logical_binary),
    HANDLER_MAP(AST_LOGICAL_AND, logical_binary),
    HANDLER_MAP(AST_LOGICAL_OR, logical_binary),
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
    for (i = 0; i < value->num_operands; i++)
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
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: invalid operand types %s and %s for intrinsic binary operator '%s'\n",
                            ast_location(expr),
                            fortran_print_type_str(lhs_type),
                            fortran_print_type_str(rhs_type),
                            get_operator_for_expr(expr));
                }
            }
            return get_error_type();
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
    *conf_lhs_type = get_rank0_type(lhs_type);
    *conf_rhs_type = get_rank0_type(rhs_type);
}

static type_t* rerank_type(type_t* rank0_common, type_t* lhs_type, type_t* rhs_type)
{
    if (is_array_type(lhs_type))
    {
        // They should have the same shape so it does not matter very much which one we use, right?
        return rebuild_array_type(rank0_common, lhs_type);
    }
    else if (is_array_type(rhs_type))
    {
        return rebuild_array_type(rank0_common, rhs_type);
    }
    else
    {
        return rank0_common;
    }
}

static type_t* rebuild_array_type(type_t* rank0_type, type_t* array_type)
{
    ERROR_CONDITION(!is_scalar_type(rank0_type), "Invalid rank0 type", 0);

    if (!is_array_type(array_type))
    {
        return rank0_type;
    }
    else
    {
        type_t* t = rebuild_array_type(rank0_type, array_type_get_element_type(array_type));
        return get_array_type_bounds(t, 
                array_type_get_array_lower_bound(array_type),
                array_type_get_array_upper_bound(array_type),
                array_type_get_array_size_expr_context(array_type));
    }
}
