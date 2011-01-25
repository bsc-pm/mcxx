#include "fortran03-exprtype.h"
#include "cxx-exprtype.h"
#include "cxx-ast.h"
#include "cxx-utils.h"

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
 STATEMENT_HANDLER(AST_USER_DEFINED_UNARY_OP, check_user_defined_unary_op)

// Prototypes
#define STATEMENT_HANDLER(_kind, _handler) \
    static void _handler(AST, decl_context_t);
STATEMENT_HANDLER_TABLE
#undef STATEMENT_HANDLER

// Table
#define STATEMENT_HANDLER(_kind, _handler) \
   { .ast_kind = _kind, .handler = _handler },
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
        fprintf(stderr, "%s: sorry: unhandled expression\n", ast_location(expression));
        return;
    }
    (handler->handler)(expression, decl_context);

}

static void check_add_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_array_constructor(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_array_ref(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_binary_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_boolean_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_complex_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_component_ref(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_concat_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_decimal_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_derived_type_constructor(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_different_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_div_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_equal_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_floating_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_function_call(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_greater_or_equal_than(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_greater_than(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_hexadecimal_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_image_ref(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_logical_and(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_logical_equal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_logical_or(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_lower_or_equal_than(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_lower_than(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_minus_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_mult_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_neg_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_not_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_octal_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_parenthesized_expression(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_plus_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_power_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_string_literal(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

static void check_user_defined_unary_op(AST a UNUSED_PARAMETER, decl_context_t decl_context UNUSED_PARAMETER)
{
}

