#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include <ctype.h>
#include <string.h>

static
type_t *compute_expression_type_rec(AST expr, scope_t *sc, decl_context_t decl_context);

type_t *compute_expression_type(AST expr, scope_t *sc, decl_context_t decl_context)
{
    if (expr == NULL)
    {
        internal_error("Invalid null node\n", 0);
    }

    type_t* result = compute_expression_type_rec(expr, sc, decl_context);

    return result;
}

static type_t *decimal_literal_type(AST expr);
static type_t *character_literal(AST expr);
static type_t *floating_literal(AST expr);
static type_t *string_literal(AST expr);

static type_t *pointer_to_type(type_t* t);
static type_t *integer_type(void);
static type_t *unsigned_integer_type(void);

static
type_t *compute_expression_type_rec(AST expr, scope_t *sc, decl_context_t decl_context)
{
    type_t* result = NULL;

    switch (ASTType(expr))
    {
        case AST_EXPRESSION : 
        case AST_CONSTANT_EXPRESSION : 
        case AST_PARENTHESIZED_EXPRESSION :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);
                break;
            }
        case AST_SYMBOL :
            {
                scope_entry_list_t* list = query_id_expression(sc, expr, FULL_UNQUALIFIED_LOOKUP, decl_context);
                if (list == NULL)
                    break;

                scope_entry_t* entry = list->entry;

                result = entry->type_information;
                break;
            }
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {
                result = decimal_literal_type(expr);
                break;
            }
        case AST_CHARACTER_LITERAL :
            {
                result = character_literal(expr);
                break;
            }
        case AST_FLOATING_LITERAL :
        case AST_HEXADECIMAL_FLOAT :
            {
                result = floating_literal(expr);
                break;
            }
        case AST_STRING_LITERAL :
            {
                result = string_literal(expr);
                break;
            }
        case AST_ARRAY_SUBSCRIPT :
            {
                type_t* subscripted_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);

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

                break;
            }
        case AST_FUNCTION_CALL :
            {
                type_t* function_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);

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

                break;
            }
        case AST_CLASS_MEMBER_ACCESS :
        case AST_POINTER_CLASS_MEMBER_ACCESS :
            {
                // We have to lookup in the class scope
                type_t* class_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);

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
                else break;

                // Now get the member of the class
                scope_entry_list_t* list = query_id_expression(class_scope, ASTSon1(expr), NOFULL_UNQUALIFIED_LOOKUP, decl_context);
                if (list == NULL)
                    break;

                scope_entry_t* entry = list->entry;

                result = entry->type_information;
                break;
            }
        case AST_POSTINCREMENT :
        case AST_POSTDECREMENT :
        case AST_PREINCREMENT :
        case AST_PREDECREMENT :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);
                break;
            }
        case AST_SIZEOF :
            {
                // Technically this is size_t
                result = unsigned_integer_type();
                break;
            }
        case AST_SIZEOF_TYPEID :
            {
                // Technically this is size_t
                result = unsigned_integer_type();
                break;
            }
        case AST_DERREFERENCE :
            {
                type_t* referenced_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);

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

                break;
            }
        case AST_REFERENCE :
            {
                type_t* referenced_type = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);

                if (referenced_type == NULL)
                    break;

                result = pointer_to_type(referenced_type);
                break;
            }
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                result = compute_expression_type_rec(ASTSon0(expr), sc, decl_context);
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

                break;
            }
        case AST_MULT_OP :
        case AST_DIV_OP :
        case AST_MOD_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
            {
                break;
            }
        case AST_SHL_OP :
        case AST_SHR_OP :
            {
                break;
            }
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
            {
                break;
            }
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
            {
                break;
            }
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
            {
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
                break;
            }
        default:
            internal_error("Unknown node type %s at %s\n", ast_print_node_type(ASTType(expr)), node_information(expr));
    }

    return result;
}


// Signed

static type_t *integer_type(void)
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

static type_t *unsigned_integer_type(void)
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
