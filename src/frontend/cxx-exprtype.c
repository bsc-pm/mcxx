/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-typeunif.h"
#include "cxx-typededuc.h"
#include "cxx-koenig.h"
#include "cxx-tltype.h"
#include "cxx-ambiguity.h"
#include "cxx-attrnames.h"
#include "cxx-overload.h"
#include "cxx-prettyprint.h"
#include "cxx-instantiation.h"
#include "cxx-buildscope.h"
#include "cxx-cexpr.h"
#include "cxx-typeenviron.h"
#include "cxx-gccsupport.h"
#include "cxx-entrylist.h"
#include <ctype.h>
#include <string.h>

#define MAX_ARGUMENTS (256)

typedef
struct expression_info_tag
{
    char is_lvalue:1;
    char is_value_dependent:1;
    int _reserved0;

    type_t* type_info;

    const_value_t* const_val;
    scope_entry_t* symbol;
} expression_info_t;

static const char builtin_prefix[] = "__builtin_";

static unsigned long long int _bytes_used_expr_check = 0;

unsigned long long exprtype_used_memory(void)
{
    return _bytes_used_expr_check;
}

unsigned long long expression_info_sizeof(void)
{
    return sizeof(expression_info_t);
}

static expression_info_t* expression_get_expression_info(AST expr)
{
    return ASTAttrValueType(expr, LANG_EXPRESSION_INFO, expression_info_t);
}

type_t* expression_get_type(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->type_info;
}

void expression_set_symbol(AST expr, scope_entry_t* entry)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->symbol = entry;
}

scope_entry_t* expression_get_symbol(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->symbol;
}

char expression_has_symbol(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->symbol != NULL;
}

void expression_set_type(AST expr, type_t* t)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->type_info = t;
}

char expression_is_lvalue(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->is_lvalue;
}

char expression_is_constant(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return (expr_info->const_val != NULL);
}

void expression_set_non_constant(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->const_val = NULL;
}

void expression_set_constant(AST expr, const_value_t* const_val)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->const_val = const_val;
}

const_value_t* expression_get_constant(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->const_val;
}

void expression_set_is_lvalue(AST expr, char is_lvalue)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->is_lvalue = is_lvalue;
}

char expression_is_value_dependent(AST expr)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    return expr_info->is_value_dependent;
}

void expression_set_is_value_dependent(AST expr, char value_dependent)
{
    expression_info_t* expr_info = expression_get_expression_info(expr);
    expr_info->is_value_dependent = value_dependent;
}

// This sets both the dependent_expr_type and the value_dependent attribute
static void expression_set_dependent(AST expr)
{
    expression_set_type(expr, get_dependent_expr_type());
    expression_set_is_value_dependent(expr, 1);
}

#define MAX_BUILTINS (256)
typedef
struct builtin_operators_set_tag
{
    scope_entry_list_t *entry_list;
    scope_entry_t entry[MAX_BUILTINS];
    int num_builtins;
} builtin_operators_set_t;

static
void build_unary_builtin_operators(type_t* t1,
        builtin_operators_set_t *result,
        decl_context_t decl_context, AST operator, 
        char (*property)(type_t*),
        type_t* (*result_type)(type_t**));

static
void build_binary_builtin_operators(type_t* t1, 
        type_t* t2, 
        builtin_operators_set_t *result,
        decl_context_t decl_context, AST operator, 
        char (*property)(type_t*, type_t*),
        type_t* (*result_type)(type_t**, type_t**));

static
void build_ternary_builtin_operators(type_t* t1, 
        type_t* t2, 
        type_t* t3, 
        builtin_operators_set_t *result,
        decl_context_t decl_context, char* operator_name, 
        char (*property)(type_t*, type_t*, type_t*),
        type_t* (*result_type)(type_t**, type_t**, type_t**));

static type_t* check_template_function(scope_entry_list_t* entry_list,
        AST template_id, decl_context_t decl_context, char *dependent_template_arguments);

scope_entry_list_t* get_entry_list_from_builtin_operator_set(builtin_operators_set_t* builtin_operators)
{
    if (builtin_operators->num_builtins == 0)
        return NULL;
    else 
        return (builtin_operators->entry_list);
}

static type_t* lvalue_ref(type_t* t)
{
    CXX_LANGUAGE()
    {
        if (!is_lvalue_reference_type(t)
                && !is_rvalue_reference_type(t))
            return get_lvalue_reference_type(t);
    }
    return t;
}


scope_entry_t* lookup_template_parameter_name(decl_context_t decl_context, AST a)
{
    ERROR_CONDITION(!is_template_parameter_name(a),
            "'%s' should be a template parameter name!", prettyprint_in_buffer(a));

    tl_type_t* symbol = ASTAttrValue(a, LANG_TEMPLATE_PARAMETER_NAME_SYMBOL);

    ERROR_CONDITION(symbol == NULL,
            "Template parameter related symbol is wrong", 0);

    scope_entry_t* result = lookup_of_template_parameter(decl_context, 
            symbol->data._entry->entity_specs.template_parameter_nesting,
            symbol->data._entry->entity_specs.template_parameter_position);

    if (result == NULL)
    {
        // Return the stored symbol
        result = symbol->data._entry;
    }

    return result;
}

static
scope_entry_t* expand_template_given_arguments(scope_entry_t* entry,
        type_t** argument_types, int num_arguments, decl_context_t decl_context,
        const char* filename, int line,
        template_argument_list_t* explicit_template_arguments)
{
    // We have to expand the template
    type_t* specialization_type = template_type_get_primary_type(entry->type_information);
    scope_entry_t* specialization_symbol = named_type_get_symbol(specialization_type);
    type_t* specialized_function_type = specialization_symbol->type_information;

    template_parameter_list_t* template_parameters = 
        template_specialized_type_get_template_parameters(specialized_function_type);

    deduction_set_t* deduction_result = NULL;

    if (deduce_arguments_from_call_to_specific_template_function(argument_types,
                num_arguments, specialization_type, template_parameters,
                decl_context, &deduction_result, filename, line, 
                explicit_template_arguments))
    {
        template_argument_list_t* argument_list = build_template_argument_list_from_deduction_set(
                deduction_result);

        // Now get a specialized template type for this
        // function (this will sign it in if it does not exist)
        type_t* named_specialization_type = template_type_get_specialized_type(entry->type_information,
                argument_list, template_parameters,
                decl_context, line, filename);

        if (named_specialization_type == NULL)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Got failure when coming up with a specialization\n");
            }
            return NULL;
        }

        scope_entry_t* specialized_symbol = named_type_get_symbol(named_specialization_type);

        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Got specialization '%s' at '%s:%d' with type '%s'\n", 
                    specialized_symbol->symbol_name,
                    specialized_symbol->file,
                    specialized_symbol->line,
                    print_declarator(specialized_symbol->type_information));
        }

        return specialized_symbol;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Discarding symbol '%s' declared at '%s:%d' as its arguments could not be deduced.\n",
                    specialization_symbol->symbol_name,
                    specialization_symbol->file,
                    specialization_symbol->line);
        }
    }
    return NULL;
}

type_t* compute_type_for_type_id_tree(AST type_id, decl_context_t decl_context)
{
    AST type_specifier = ASTSon0(type_id);
    AST abstract_declarator = ASTSon1(type_id);

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    type_t* simple_type_info = NULL;
    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
            decl_context);

    type_t* declarator_type = simple_type_info;
    compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
            &declarator_type, decl_context);

    return declarator_type;
}

scope_entry_list_t* unfold_and_mix_candidate_functions(
        scope_entry_list_t* result_from_lookup,
        scope_entry_list_t* builtin_list,
        type_t** argument_types,
        int num_arguments,
        decl_context_t decl_context,
        const char *filename,
        int line,
        template_argument_list_t *explicit_template_arguments
        )
{
    scope_entry_list_t* overload_set = NULL;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(result_from_lookup);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (entry->kind == SK_TEMPLATE)
        {
            scope_entry_t* specialized_symbol = expand_template_given_arguments(entry,
                    argument_types, num_arguments, decl_context, filename, line,
                    explicit_template_arguments);

            if (specialized_symbol != NULL)
            {
                overload_set = entry_list_add(overload_set, specialized_symbol);
            }
        }
        else if (entry->kind == SK_FUNCTION)
        {
            overload_set = entry_list_add(overload_set, entry);
        }
    }
    entry_list_iterator_free(it);
    
    // Add builtins but only if their signature is not already in the overload
    // set
    for (it = entry_list_iterator_begin(builtin_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* builtin = entry_list_iterator_current(it);
        scope_entry_list_iterator_t* it2 = NULL;
        char found = 0;
        for (it2 = entry_list_iterator_begin(overload_set);
                !entry_list_iterator_end(it2) && !found;
                entry_list_iterator_next(it2))
        {
            scope_entry_t* ovl = entry_list_iterator_current(it2);

            found = equivalent_types(ovl->type_information, builtin->type_information);
        }
        entry_list_iterator_free(it2);

        if (!found)
        {
            overload_set = entry_list_add(overload_set, builtin);
        }
    }
    entry_list_iterator_free(it);

    return overload_set;
}

static
scope_entry_list_t* get_member_of_class_type(type_t* class_type,
        AST id_expression, decl_context_t decl_context)
{
    if (is_named_class_type(class_type)
            && class_type_is_incomplete_independent(get_actual_class_type(class_type)))
    {
        scope_entry_t* symbol = named_type_get_symbol(class_type);

        instantiate_template_class(symbol, decl_context, ASTFileName(id_expression), ASTLine(id_expression));
    }

    class_type = get_actual_class_type(class_type);

    const char *name = NULL;
    switch (ASTType(id_expression))
    {
        case AST_SYMBOL :
            {
                name = ASTText(id_expression);
            }
            break;
        default:
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(id_expression)));
    }

    ERROR_CONDITION(name == NULL, "Name not properly computed", 0);

    decl_context_t class_context = class_type_get_inner_context(class_type);
    if (class_context.class_scope != NULL)
    {
        return class_context_lookup(class_context, name);
    }
    else
    {
        return NULL;
    }
}

static
scope_entry_list_t* get_member_function_of_class_type(type_t* class_type,
        AST id_expression, decl_context_t decl_context)
{
    if (is_named_class_type(class_type)
            && class_type_is_incomplete_independent(get_actual_class_type(class_type)))
    {
        scope_entry_t* symbol = named_type_get_symbol(class_type);

        instantiate_template_class(symbol, decl_context, ASTFileName(id_expression), ASTLine(id_expression));
    }

    class_type = get_actual_class_type(class_type);

    const char *name = NULL;
    switch (ASTType(id_expression))
    {
        case AST_SYMBOL :
            {
                name = ASTText(id_expression);
            }
            break;
        case AST_TEMPLATE_ID :
            {
                name = ASTText(ASTSon0(id_expression));
            }
            break;
        case AST_DESTRUCTOR_ID :
        case AST_DESTRUCTOR_TEMPLATE_ID :
            {
                AST symbol = ASTSon0(id_expression);
                name = ASTText(symbol);
            }
            break;
        case AST_OPERATOR_FUNCTION_ID :
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                name = get_operator_function_name(id_expression);
            }
            break;
        case AST_QUALIFIED_ID:
        case AST_QUALIFIED_TEMPLATE:
            {
                decl_context_t class_context = class_type_get_inner_context(class_type);

                if (class_context.class_scope != NULL)
                {
                    return query_id_expression(decl_context, id_expression);
                }
                else
                {
                    return NULL;
                }
            }
            break;
        default:
            internal_error("Invalid node type '%s'\n", ast_print_node_type(ASTType(id_expression)));
    }

    ERROR_CONDITION(name == NULL, "Name not properly computed", 0);

    decl_context_t class_context = class_type_get_inner_context(class_type);
    if (class_context.class_scope != NULL)
    {
        return class_context_lookup(class_context, name);
    }
    else
    {
        return NULL;
    }
}

static type_t* decimal_literal_type(AST expr, const_value_t** val);
static type_t* character_literal_type(AST expr, const_value_t** val);
static type_t* floating_literal_type(AST expr);
static type_t* string_literal_type(AST expr);

// Typechecking functions
static char check_for_qualified_id(AST expr, decl_context_t decl_context, const_value_t** val);
static char check_for_symbol(AST expr, decl_context_t decl_context, const_value_t** val);
static char check_for_array_subscript_expr(AST expr, decl_context_t decl_context);
static char check_for_function_call(AST expr, decl_context_t decl_context);
static char check_for_explicit_type_conversion(AST expr, decl_context_t decl_context);
static char check_for_explicit_typename_type_conversion(AST expr, decl_context_t decl_context);
static char check_for_member_access(AST member_access, decl_context_t decl_context, char is_arrow);
static char check_for_typeid_expr(AST expr, decl_context_t decl_context);
static char check_for_typeid_type(AST expr, decl_context_t decl_context);
static char check_for_sizeof_expr(AST expr, decl_context_t decl_context);
static char check_for_sizeof_typeid(AST expr, decl_context_t decl_context);
static char check_for_cast_expr(AST expression, AST type_id, AST casted_expression, decl_context_t decl_context);
static char check_for_new_expression(AST new_expr, decl_context_t decl_context);
static char check_for_new_type_id_expr(AST new_expr, decl_context_t decl_context);
static char check_for_delete_expression(AST expression, decl_context_t decl_context);
static char check_for_initializer_list(AST initializer_list, decl_context_t decl_context, type_t* declared_type);
static char check_for_binary_expression(AST expression, decl_context_t decl_context, const_value_t** val);
static char check_for_unary_expression(AST expression, decl_context_t decl_context, const_value_t** val);
static char check_for_template_id_expr(AST expr, decl_context_t decl_context);
static char check_for_templated_member_access(AST templated_member_access, decl_context_t decl_context, char is_arrow);
static char check_for_postincrement(AST expr, decl_context_t decl_context);
static char check_for_postdecrement(AST expr, decl_context_t decl_context);
static char check_for_preincrement(AST expr, decl_context_t decl_context);
static char check_for_predecrement(AST expr, decl_context_t decl_context);
static char check_for_conditional_expression(AST expression, decl_context_t decl_context);
static char check_for_comma_operand(AST expression, decl_context_t decl_context);
static char check_for_pointer_to_member(AST expression, decl_context_t decl_context);
static char check_for_pointer_to_pointer_to_member(AST expression, decl_context_t decl_context);
static char check_for_conversion_function_id_expression(AST expression, decl_context_t decl_context);
static char check_for_pseudo_destructor_call(AST expression, decl_context_t decl_context);

static char check_for_array_section_expression(AST expression, decl_context_t decl_context);
static char check_for_shaping_expression(AST expression, decl_context_t decl_context);

static char check_for_gcc_builtin_offsetof(AST expression, decl_context_t decl_context);
static char check_for_gcc_builtin_choose_expr(AST expression, decl_context_t decl_context);
static char check_for_gcc_builtin_types_compatible_p(AST expression, decl_context_t decl_context);

static char* assig_op_attr[] =
{
    [AST_ASSIGNMENT]     = LANG_IS_ASSIGNMENT,
    [AST_MUL_ASSIGNMENT] = LANG_IS_MUL_ASSIGNMENT,
    [AST_DIV_ASSIGNMENT] = LANG_IS_DIV_ASSIGNMENT,
    [AST_ADD_ASSIGNMENT] = LANG_IS_ADD_ASSIGNMENT,
    [AST_SUB_ASSIGNMENT] = LANG_IS_SUB_ASSIGNMENT,
    [AST_SHL_ASSIGNMENT] = LANG_IS_SHL_ASSIGNMENT,
    [AST_SHR_ASSIGNMENT] = LANG_IS_SHR_ASSIGNMENT,
    [AST_AND_ASSIGNMENT] = LANG_IS_AND_ASSIGNMENT,
    [AST_OR_ASSIGNMENT ] = LANG_IS_OR_ASSIGNMENT, 
    [AST_XOR_ASSIGNMENT] = LANG_IS_XOR_ASSIGNMENT,
    [AST_MOD_ASSIGNMENT] = LANG_IS_MOD_ASSIGNMENT,
};

static char* unary_expression_attr[] =
{
    [AST_DERREFERENCE]  = LANG_IS_DERREFERENCE_OP,
    [AST_REFERENCE]     = LANG_IS_REFERENCE_OP,
    [AST_PLUS_OP]       = LANG_IS_PLUS_OP,
    [AST_NEG_OP]        = LANG_IS_NEGATE_OP,
    [AST_NOT_OP]        = LANG_IS_NOT_OP,
    [AST_COMPLEMENT_OP] = LANG_IS_COMPLEMENT_OP
};

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
    [AST_LOGICAL_OR] = LANG_IS_LOGICAL_OR_OP
};

// Returns if the function is ok
//
// Do not return within this function, set result to 0 or 1 and let it
// reach the end, by default result == 0
char check_for_expression(AST expression, decl_context_t decl_context)
{
    char result = 0;

    // Shortcut
    if (expression_get_type(expression) != NULL)
    {
        // This is the only point we allow returning
        return 1;
    }

    switch (ASTType(expression))
    {
        case AST_EXPRESSION :
        case AST_CONSTANT_EXPRESSION :
        case AST_PARENTHESIZED_EXPRESSION :
            // GCC extensions
        case AST_GCC_EXTENSION_EXPR : 
            {
                result = check_for_expression(ASTSon0(expression), decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(ASTSon0(expression)));

                    expression_set_type(expression, expression_get_type(ASTSon0(expression)));
                    expression_set_is_lvalue(expression, expression_is_lvalue(ASTSon0(expression)));
                    expression_set_constant(expression, expression_get_constant(ASTSon0(expression)));
                    expression_set_is_value_dependent(expression, expression_is_value_dependent(ASTSon0(expression)));
                }

                break;
            }
            // Primaries
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_INTEGER_LITERAL, tl_type_t, tl_bool(1));

                const_value_t* val = NULL;
                expression_set_type(expression, decimal_literal_type(expression, &val));
                expression_set_constant(expression, val);
                expression_set_is_lvalue(expression, 0);

                result = 1;
                break;
            }
        case AST_FLOATING_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_FLOATING_LITERAL, tl_type_t, tl_bool(1));

                expression_set_type(expression, floating_literal_type(expression));
                expression_set_is_lvalue(expression, 0);

                result = 1;
                break;
            }
        case AST_BOOLEAN_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_BOOLEAN_LITERAL, tl_type_t, tl_bool(1));

                expression_set_type(expression, get_bool_type());
                expression_set_is_lvalue(expression, 0);

                const char* literal = ASTText(expression);

                const_value_t* val = NULL;
                if (strcmp(literal, "true") == 0)
                {
                    val = const_value_get_one(type_get_size(get_bool_type()), 0);
                }
                else
                {
                    val = const_value_get_zero(type_get_size(get_bool_type()), 0);
                }

                expression_set_constant(expression, val);

                result = 1;
                break;
            }
        case AST_CHARACTER_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_CHARACTER_LITERAL, tl_type_t, tl_bool(1));

                const_value_t *val = NULL;
                expression_set_type(expression, character_literal_type(expression, &val));
                expression_set_is_lvalue(expression, 0);
                expression_set_constant(expression, val);

                result = 1;
                break;
            }
        case AST_STRING_LITERAL :
            {
                ASTAttrSetValueType(expression, LANG_IS_LITERAL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(expression, LANG_IS_STRING_LITERAL, tl_type_t, tl_bool(1));

                expression_set_type(expression, 
                        lvalue_ref(string_literal_type(expression)));
                expression_set_is_lvalue(expression, 1);

                result = 1;
                break;
            }
        case AST_THIS_VARIABLE :
            {
                scope_entry_list_t* entry_list = query_unqualified_name_str(decl_context, "this");

                if (entry_list != NULL)
                {
                    scope_entry_t *entry = entry_list_head(entry_list);

                    if (is_pointer_to_class_type(entry->type_information))
                    {
                        if (!is_dependent_type(entry->type_information))
                        {
                            expression_set_type(expression, get_lvalue_reference_type(entry->type_information));
                            expression_set_is_lvalue(expression, 1);
                        }
                        else
                        {
                            expression_set_dependent(expression);
                        }
                        result = 1;

                        ASTAttrSetValueType(expression, LANG_IS_THIS_VARIABLE, tl_type_t, tl_bool(1));
                        expression_set_symbol(expression, entry);
                    }
                }
                entry_list_free(entry_list);

                break;
            }
        case AST_QUALIFIED_ID :
            {
                const_value_t* val = NULL;
                result = check_for_qualified_id(expression, decl_context, &val);

                if (result)
                {
                    AST global_qualif = ASTSon0(expression);
                    AST nested_name_spec = ASTSon1(expression);
                    AST unqualified_id = ASTSon2(expression);

                    ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_QUALIFIED_ID, tl_type_t, tl_bool(1));

                    if (global_qualif != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_IS_GLOBAL_QUALIFIED, tl_type_t, tl_bool(1));
                    }

                    if (nested_name_spec != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_NESTED_NAME_SPECIFIER, tl_type_t, tl_ast(nested_name_spec));
                    }

                    ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(unqualified_id));

                    expression_set_constant(expression, val);
                }

                break;
            }
        case AST_QUALIFIED_TEMPLATE :
            {
                // This always yields a value because it must be used in a
                // template context where everything is regarded as an
                // expression unless the user "typenames" things
                //
                // so, in following code
                //
                // int a;
                //
                // template <typename _T>
                // void f(_T t)
                // {
                //    _T::template g<_T>(a);
                // }
                //
                //  If this was intended to be a declaration of a variable 'a'
                //  of type '_T::template h<_T>', it should have been preceded
                //  by 'typename' keyword which syntactically disambiguates the
                //  whole declaration
                //
                const_value_t* val = NULL;
                result = check_for_qualified_id(expression, decl_context, &val);
                if (result)
                {
                    AST global_qualif = ASTSon0(expression);
                    AST nested_name_spec = ASTSon1(expression);
                    AST unqualified_id = ASTSon2(expression);

                    ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_QUALIFIED_ID, tl_type_t, tl_bool(1));

                    if (global_qualif != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_IS_GLOBAL_QUALIFIED, tl_type_t, tl_bool(1));
                    }

                    if (nested_name_spec != NULL)
                    {
                        ASTAttrSetValueType(expression, LANG_NESTED_NAME_SPECIFIER, tl_type_t, tl_ast(nested_name_spec));
                    }

                    ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(unqualified_id));

                    expression_set_constant(expression, val);
                }

                break;
            }
        case AST_SYMBOL :
        case AST_OPERATOR_FUNCTION_ID :
            {
                const_value_t* val = NULL;
                result = check_for_symbol(expression, decl_context, &val);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(expression));

                    expression_set_constant(expression, val);
                }

                break;
            }
        case AST_DESTRUCTOR_ID :
        case AST_DESTRUCTOR_TEMPLATE_ID :
            {
                AST symbol = ASTSon0(expression);
                result = check_for_symbol(symbol, decl_context, NULL);
                break;
            }
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                result = check_for_template_id_expr(expression, decl_context);
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_TEMPLATE_NAME, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_TEMPLATE_ARGS, tl_type_t, tl_ast(ASTSon1(expression)));
                }
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                result = check_for_conversion_function_id_expression(expression, decl_context);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                result = check_for_template_id_expr(expression, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(expression));

                    ASTAttrSetValueType(expression, LANG_IS_TEMPLATE_ID, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_TEMPLATE_NAME, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_TEMPLATE_ARGS, tl_type_t, tl_ast(ASTSon1(expression)));
                }
                break;
            }
            // Postfix expressions
        case AST_ARRAY_SUBSCRIPT :
            {
                result = check_for_array_subscript_expr(expression, decl_context);
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_ARRAY_SUBSCRIPT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_SUBSCRIPTED_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_SUBSCRIPT_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));

                    expression_set_is_value_dependent(expression,
                            expression_is_value_dependent(ASTSon0(expression)));
                }
                break;
            }
        case AST_FUNCTION_CALL :
            {
                result = check_for_function_call(expression, decl_context );
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_FUNCTION_CALL, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_CALLED_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_FUNCTION_ARGUMENTS, tl_type_t, tl_ast(ASTSon1(expression)));
                }
                break;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                result = check_for_explicit_type_conversion(expression, decl_context );
                break;
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONV :
            {
                result = check_for_explicit_typename_type_conversion(expression, decl_context);
                break;
            }
        case AST_POINTER_CLASS_MEMBER_ACCESS :
        case AST_CLASS_MEMBER_ACCESS :
            {
                char is_arrow = (ASTType(expression) == AST_POINTER_CLASS_MEMBER_ACCESS);
                result = check_for_member_access(expression, decl_context, is_arrow);

                if (result)
                {
                    ASTAttrSetValueType(ASTSon1(expression), LANG_IS_ACCESSED_MEMBER, tl_type_t, tl_bool(1));

                    ASTAttrSetValueType(expression, LANG_ACCESSED_ENTITY, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_ACCESSED_MEMBER, tl_type_t, tl_ast(ASTSon1(expression)));

                    if (ASTType(expression) == AST_POINTER_CLASS_MEMBER_ACCESS)
                    {
                        ASTAttrSetValueType(expression, LANG_IS_POINTER_MEMBER_ACCESS, tl_type_t, tl_bool(1));
                    }
                    else
                    {
                        ASTAttrSetValueType(expression, LANG_IS_MEMBER_ACCESS, tl_type_t, tl_bool(1));
                    }
                }

                break;
            }
        case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
        case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
            {
                char is_arrow = (ASTType(expression) == AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS);
                result = check_for_templated_member_access(expression, decl_context, is_arrow);
                break;
            }
        case AST_POSTINCREMENT :
            {
                result = check_for_postincrement(expression, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_POSTINCREMENT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_INCREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                }
                break;
            }
        case AST_POSTDECREMENT :
            {
                result = check_for_postdecrement(expression, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_POSTDECREMENT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_DECREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                }
                break;
            }
        case AST_DYNAMIC_CAST :
        case AST_STATIC_CAST :
        case AST_REINTERPRET_CAST :
        case AST_CONST_CAST :
            {
                AST type_id = ASTSon0(expression);
                AST casted_expr = ASTSon1(expression);

                result = check_for_cast_expr(expression, type_id, casted_expr, decl_context);
                break;
            }
        case AST_TYPEID_TYPE :
            {
                result = check_for_typeid_type(expression, decl_context);
                break;
            }
        case AST_TYPEID_EXPR :
            {
                result = check_for_typeid_expr(expression, decl_context);
                break;
            }
        // Unary expressions
        case AST_PREINCREMENT :
            {
                result = check_for_preincrement(expression, decl_context);
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_PREINCREMENT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_INCREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                }
                break;
            }
        case AST_PREDECREMENT :
            {
                result = check_for_predecrement(expression, decl_context);
                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_IS_PREDECREMENT, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_EXPRESSION_DECREMENTED, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                }
                break;
            }
        case AST_SIZEOF :
            /* UPC has upc_{local,block,elem}sizeof that are identical to the normal one */
        case AST_UPC_BLOCKSIZEOF :
        case AST_UPC_ELEMSIZEOF :
        case AST_UPC_LOCALSIZEOF :
            {
                result = check_for_sizeof_expr(expression, decl_context);
                break;
            }
        case AST_SIZEOF_TYPEID :
            /* UPC has upc_{local,block,elem}sizeof that are identical to the normal one */
        case AST_UPC_BLOCKSIZEOF_TYPEID :
        case AST_UPC_ELEMSIZEOF_TYPEID :
        case AST_UPC_LOCALSIZEOF_TYPEID :
            {
                result = check_for_sizeof_typeid(expression, decl_context);
                break;
            }
        case AST_DERREFERENCE :
        case AST_REFERENCE :
        case AST_PLUS_OP :
        case AST_NEG_OP :
        case AST_NOT_OP :
        case AST_COMPLEMENT_OP :
            {
                const_value_t* val = NULL;
                result = check_for_unary_expression(expression, decl_context, &val);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_UNARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, 
                            unary_expression_attr[ASTType(expression)], tl_type_t, tl_bool(1));

                    ASTAttrSetValueType(expression, LANG_UNARY_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));

                    expression_set_constant(expression, val);

                    expression_set_is_value_dependent(expression, 
                            expression_is_value_dependent(ASTSon0(expression)));
                }

                break;
            }
            // Cast expression
        case AST_CAST_EXPRESSION :
            {
                AST type_id = ASTSon0(expression);
                AST casted_expr = ASTSon1(expression);

                result = check_for_cast_expr(expression, type_id, casted_expr, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_CAST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_CAST_TYPE, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_CASTED_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                }

                break;
            }
        // Pointer to method expressions 
        case AST_POINTER_TO_POINTER_MEMBER :
            {
                result = check_for_pointer_to_pointer_to_member(expression, decl_context);
                break;
            }
        case AST_POINTER_TO_MEMBER :
            {
                // This should always yield a value
                result = check_for_pointer_to_member(expression, decl_context);
                break;
            }
        case AST_MULT_OP :
        case AST_DIV_OP :
        case AST_MOD_OP :
        case AST_ADD_OP :
        case AST_MINUS_OP :
        case AST_SHL_OP :
        case AST_SHR_OP :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_EQUAL_OP :
        case AST_DIFFERENT_OP :
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
            {
                const_value_t* val = NULL;
                result = check_for_binary_expression(expression, decl_context, &val);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_BINARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, binary_expression_attr[ASTType(expression)], tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_RHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expression)));

                    expression_set_is_value_dependent(expression,
                            expression_is_value_dependent(ASTSon0(expression))
                            || expression_is_value_dependent(ASTSon1(expression)));

                    expression_set_constant(expression, val);
                }

                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
        case AST_GCC_CONDITIONAL_EXPRESSION :
            {
                result = check_for_conditional_expression(expression, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_CONDITIONAL_EXPRESSION,
                            tl_type_t, tl_bool(1));

                    if (ASTType(expression) == AST_CONDITIONAL_EXPRESSION)
                    {
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_TRUE_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_FALSE_EXPRESSION, tl_type_t, tl_ast(ASTSon2(expression)));

                        expression_set_is_value_dependent(expression,
                                expression_is_value_dependent(ASTSon0(expression))
                                || expression_is_value_dependent(ASTSon1(expression))
                                || expression_is_value_dependent(ASTSon2(expression)));
                    }
                    else
                    {
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_TRUE_EXPRESSION, tl_type_t, tl_ast(ASTSon0(expression)));
                        ASTAttrSetValueType(expression, LANG_CONDITIONAL_FALSE_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));

                        expression_set_is_value_dependent(expression,
                                expression_is_value_dependent(ASTSon0(expression))
                                || expression_is_value_dependent(ASTSon1(expression)));
                    }
                }

                break;
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
                result = check_for_binary_expression(expression, decl_context, NULL);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_BINARY_OPERATION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, assig_op_attr[ASTType(expression)], tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_LHS_ASSIGNMENT, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_RHS_ASSIGNMENT, tl_type_t, tl_ast(ASTSon1(expression)));

                    expression_set_is_value_dependent(expression,
                            expression_is_value_dependent(ASTSon0(expression))
                            || expression_is_value_dependent(ASTSon1(expression)));
                }
                break;
            }
        case AST_THROW_EXPRESSION :
            {
                result = 1;
                if (ASTSon0(expression) != NULL)
                {
                    result = check_for_expression(ASTSon0(expression), decl_context);
                }
                if (result)
                {
                    expression_set_type(expression, get_throw_expr_type());
                }
                break;
            }
        case AST_COMMA_OP :
            {
                result = check_for_comma_operand(expression, decl_context);

                if (result)
                {
                    ASTAttrSetValueType(expression, LANG_IS_COMMA_OP, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_LHS_OPERAND, tl_type_t, tl_ast(ASTSon1(expression)));

                    expression_set_is_value_dependent(expression,
                            expression_is_value_dependent(ASTSon0(expression)));
                }
                break;
            }
            // GCC Extension
        case AST_GCC_LABEL_ADDR :
            {
                // Let's assume this is correct and has type 'void*'
                expression_set_type(expression, get_pointer_type(get_void_type()));
                result = 1;
                break;
            }
        case AST_GCC_REAL_PART :
        case AST_GCC_IMAG_PART :
            {
                result = check_for_expression(ASTSon0(expression), decl_context);
                if (result)
                {
                    // Be careful because gcc accepts lvalues and transfers the lvalueness
                    type_t* complex_type = expression_get_type(ASTSon0(expression));

                    // Minimal support for C99 complex types
                    if (complex_type != NULL
                            && is_complex_type(no_ref(complex_type)))
                    {
                        type_t* real_part_type = complex_type_get_base_type(no_ref(complex_type));

                        if (is_lvalue_reference_type(complex_type))
                        {
                            real_part_type = lvalue_ref(real_part_type);
                            expression_set_type(expression, real_part_type);
                            expression_set_is_lvalue(expression, 1);
                        }
                        else
                        {
                            expression_set_type(expression, real_part_type);
                        }
                        result = 1;
                    }
                    else
                    {
                        result = 0;
                    }
                }
                break;
            }
        case AST_GCC_ALIGNOF :
            {
                // Reuse the sizeof code
                result = check_for_sizeof_expr(expression, decl_context );
                break;
            }
        case AST_GCC_ALIGNOF_TYPE :
            {
                // Reuse the sizeof code
                result = check_for_sizeof_typeid(expression, decl_context );
                break;
            }
        case AST_NEW_EXPRESSION :
            {
                // This is always a value, never a type
                result = check_for_new_expression(expression, decl_context );
                break;
            }
        case AST_NEW_TYPE_ID_EXPR :
            {
                result = check_for_new_type_id_expr(expression, decl_context );
                break;
            }
        case AST_DELETE_EXPR :
        case AST_DELETE_ARRAY_EXPR :
            {
                result = check_for_delete_expression(expression, decl_context);
                break;
            }
        case AST_PSEUDO_DESTRUCTOR_CALL :
        case AST_POINTER_PSEUDO_DESTRUCTOR_CALL :
            {
                result = check_for_pseudo_destructor_call(expression, decl_context);
                break;
            }
        case AST_VLA_EXPRESSION :
            {
                // This is not actually an expression per se, but it appears in
                // a place where 99% of the time an expression is used instead,
                // so instead of filling the code with checks for this node
                // type, accept it in the club of expressions with a type of
                // signed int and a non constant expression
                result = 1;
                expression_set_type(expression, get_signed_int_type());
                break;
            }
        case AST_GCC_POSTFIX_EXPRESSION :
            {
                result = check_for_type_id_tree(ASTSon0(expression), decl_context);

                if (result)
                {
                    // Compute this postfix type
                    AST type_id = ASTSon0(expression);
                    AST type_specifier_seq = ASTSon0(type_id);
                    AST abstract_decl = ASTSon1(type_id);

                    type_t *type_info = NULL;

                    gather_decl_spec_t gather_info;
                    memset(&gather_info, 0, sizeof(gather_info));

                    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, decl_context);

                    if (is_named_type(type_info)
                            && named_type_get_symbol(type_info)->kind == SK_TEMPLATE)
                    {
                        // This is not valid here
                        if (!checking_ambiguity())
                        {
                            fprintf(stderr, "%s: warning: invalid '%s' type, it names a template-name\n",
                                    ast_location(type_specifier_seq),
                                    prettyprint_in_buffer(type_specifier_seq));
                        }
                        return 0;
                    }

                    type_t* declarator_type = type_info;
                    compute_declarator_type(abstract_decl, 
                            &gather_info, type_info, &declarator_type,
                            decl_context);

                    type_t* type_in_context = declarator_type;
                    if (is_array_type(type_in_context))
                    {
                        type_in_context = array_type_get_element_type(type_in_context);
                    }
                    else if (is_vector_type(type_in_context))
                    {
                        type_in_context = vector_type_get_element_type(type_in_context);
                    }

                    if (!check_for_initializer_list(ASTSon1(expression), decl_context, type_in_context))
                    {
                        result = 0;
                    }
                    else
                    {
                        expression_set_type(expression, declarator_type);
                        expression_set_is_lvalue(expression, 0);
                    }
                }
                break;
            }
        case AST_GCC_BUILTIN_VA_ARG :
            {
                // This is an historic builtin we do not handle by means of 
                // the future generic builtin mechanism since it has special syntax
                result = check_for_expression(ASTSon0(expression), decl_context)
                    && check_for_type_id_tree(ASTSon1(expression), decl_context);

                // Compute the whole result
                if (result)
                {
                    // Compute type
                    AST type_id = ASTSon1(expression);
                    AST type_specifier_seq = ASTSon0(type_id);
                    AST abstract_decl = ASTSon1(type_id);

                    type_t *type_info = NULL;

                    gather_decl_spec_t gather_info;
                    memset(&gather_info, 0, sizeof(gather_info));

                    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, decl_context);

                    if (is_named_type(type_info)
                            && named_type_get_symbol(type_info)->kind == SK_TEMPLATE)
                    {
                        // This is not valid here
                        if (!checking_ambiguity())
                        {
                            fprintf(stderr, "%s: warning: invalid '%s' type-name, it names a template-name\n",
                                    ast_location(type_specifier_seq),
                                    prettyprint_in_buffer(type_specifier_seq));
                        }
                        return 0;
                    }

                    type_t* declarator_type = type_info;
                    compute_declarator_type(abstract_decl, 
                            &gather_info, type_info, &declarator_type,
                            decl_context);

                    expression_set_type(expression, declarator_type);
                }
                break;
            }
        case AST_GCC_BUILTIN_OFFSETOF :
            {
                result = check_for_gcc_builtin_offsetof(expression, decl_context);
                break;
            }
        case AST_GCC_BUILTIN_CHOOSE_EXPR :
            {
                result = check_for_gcc_builtin_choose_expr(expression, decl_context);
                break;
            }
        case AST_GCC_BUILTIN_TYPES_COMPATIBLE_P :
            {
                result = check_for_gcc_builtin_types_compatible_p(expression, decl_context);
                break;
            }
        case AST_GCC_PARENTHESIZED_EXPRESSION :
            {
                AST compound_statement = ASTSon0(expression);
                build_scope_statement(compound_statement, decl_context);

                AST statement_seq = ASTSon0(compound_statement);
                if (statement_seq == NULL)
                {
                    expression_set_type(expression, get_void_type());
                    result = 1;
                }
                else
                {
                    AST last_statement = ASTSon1(statement_seq);

                    if (expression_get_type(last_statement) != NULL)
                    {
                        expression_set_type(expression, expression_get_type(last_statement));
                        expression_set_is_lvalue(expression, expression_is_lvalue(last_statement));
                        result = 1;
                    }
                }

                break;
            }
        case AST_GXX_TYPE_TRAITS :
            {
                result = check_for_gxx_type_traits(expression, decl_context);
                break;
            }
            // This is a mcxx extension
            // that brings the power of Fortran 90 array-sections into C/C++ :-)
        case AST_ARRAY_SECTION :
            {
                if (check_for_array_section_expression(expression, decl_context))
                {
                    result = 1;

                    ASTAttrSetValueType(expression, LANG_IS_ARRAY_SECTION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_ARRAY_SECTION_ITEM, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_ARRAY_SECTION_LOWER, tl_type_t, tl_ast(ASTSon1(expression)));
                    ASTAttrSetValueType(expression, LANG_ARRAY_SECTION_UPPER, tl_type_t, tl_ast(ASTSon2(expression)));
                }
                break;
            }
            // This is a mcxx extension
            // that gives an array shape to pointer expressions
        case AST_SHAPING_EXPRESSION:
            {
                if (check_for_shaping_expression(expression, decl_context))
                {
                    result = 1;

                    ASTAttrSetValueType(expression, LANG_IS_SHAPING_EXPRESSION, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(expression, LANG_SHAPE_LIST, tl_type_t, tl_ast(ASTSon0(expression)));
                    ASTAttrSetValueType(expression, LANG_SHAPED_EXPRESSION, tl_type_t, tl_ast(ASTSon1(expression)));
                }
                break;
            }
            // Special nodes
        case AST_DIMENSION_STR:
            {
                result = 1;
                // We do this to avoid later failures
                expression_set_type(expression, get_signed_int_type());
                expression_set_is_lvalue(expression, 0);
                break;
            }
        case AST_AMBIGUITY :
            {
                if (!solve_ambiguous_expression(expression, decl_context))
                {
                    result = 0;
                }
                else
                {
                    result = check_for_expression(expression, decl_context);
                }
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
                ast_location(expression));
                break;
            }
    }

    // This is used when moving within an expression tree
    ASTAttrSetValueType(expression, LANG_IS_EXPRESSION_COMPONENT, tl_type_t, tl_bool(1));

    if (!result 
            || (expression_get_type(expression) == NULL))
    {
        if (!checking_ambiguity() 
                && CURRENT_CONFIGURATION->strict_typecheck)
        {
            internal_error("Expression '%s' at '%s' does not have a valid computed type\n",
                    prettyprint_in_buffer(expression),
                    ast_location(expression));
        }
    }

    DEBUG_CODE()
    {
        if (expression_get_type(expression) != NULL)
        {
            fprintf(stderr, "EXPRTYPE: Expression '%s' at '%s' has as computed type '%s' and it is a '%s'",
                    prettyprint_in_buffer(expression),
                    ast_location(expression),
                    print_declarator(expression_get_type(expression)),
                    expression_is_lvalue(expression) ? "lvalue" : "rvalue");

            if (expression_is_constant(expression))
            {
                const_value_t* v = expression_get_constant(expression);
                fprintf(stderr, " with a constant value of ");
                if (const_value_is_signed(v))
                {
                    fprintf(stderr, " '%lld'", (long long int)const_value_cast_to_8(v));
                }
                else
                {
                    fprintf(stderr, " '%llu'", (unsigned long long int)const_value_cast_to_8(v));
                }
            }

            if (expression_is_value_dependent(expression))
            {
                fprintf(stderr, " [VALUE DEPENDENT]");
            }

            fprintf(stderr, "\n");
        }
    }

    return result;
}


#define RETURN_IF_ERROR_OR_DEPENDENT_2(t1, t2, e) \
{ \
    if (((t1) == NULL) \
        || ((t2) == NULL)) \
    { \
       expression_set_type(e, NULL); \
       return NULL; \
    }\
    if (is_dependent_expr_type(t1) \
            || is_dependent_expr_type(t2)) \
    { \
      expression_set_dependent(e); \
      DEBUG_CODE() \
        { \
            fprintf(stderr, "EXPRTYPE: Found expression '%s' to be dependent\n", \
                    prettyprint_in_buffer(e)); \
        } \
      return expression_get_type(e); \
    } \
}

#define RETURN_IF_ERROR_OR_DEPENDENT_1(t1, e) \
{ \
    if ((t1) == NULL) \
    { \
       expression_set_type(e, NULL); \
       return NULL; \
    } \
    if (is_dependent_expr_type(t1)) \
    { \
      expression_set_dependent(e); \
      DEBUG_CODE() \
        { \
            fprintf(stderr, "EXPRTYPE: Found expression '%s' to be dependent\n", \
                    prettyprint_in_buffer(e)); \
        } \
      return expression_get_type(e); \
    } \
}

// Given a decimal literal computes the type due to its lexic form
static type_t* decimal_literal_type(AST expr, const_value_t** val)
{
    const char *literal = ASTText(expr);
    const char *last = literal + strlen(literal) - 1;

    char is_unsigned = 0;
    char is_long = 0;
    char is_complex = 0;

    // This loop goes backwards until the literal figures are found again
    while (toupper(*last) == 'L' 
            || toupper(*last) == 'U'
            // This is a GNU extension for complex
            || toupper(*last) == 'I'
            || toupper(*last) == 'J')
    {
        switch (toupper(*last))
        {
            case 'L' :
                is_long++;
                break;
            case 'U' :
                is_unsigned = 1;
                break;
            case 'J':
            case 'I':
                is_complex = 1;
                break;
            default:
                internal_error("Code unreachable", 0);
                break;
        }
        last--;
    }

    type_t* result = NULL;
    switch (is_long)
    {
        case 0 :
            {
                result = ((is_unsigned == 0 ) ? get_signed_int_type() : get_unsigned_int_type());

                if (is_unsigned)
                {
                    result = get_unsigned_int_type();
                    *val = const_value_get(strtoul(literal, NULL, 0), type_get_size(result), /*sign*/ 0);
                }
                else
                {
                    result = get_signed_int_type();
                    *val = const_value_get(strtol(literal, NULL, 0), type_get_size(result), /* sign*/ 1);
                }
                break;
            }
        case 1 : 
            {
                if (is_unsigned)
                {
                    result = get_unsigned_long_int_type();
                    *val = const_value_get(strtoul(literal, NULL, 0), type_get_size(result), 0);
                }
                else
                {
                    result = get_signed_long_int_type();
                    *val = const_value_get(strtol(literal, NULL, 0), type_get_size(result), 1);
                }
                break;
            }
        default :
            {
                if (is_unsigned)
                {
                    result = get_unsigned_long_long_int_type();
                    *val = const_value_get(strtoull(literal, NULL, 0), type_get_size(result), 0);
                }
                else
                {
                    result = get_signed_long_long_int_type();
                    *val = const_value_get(strtoll(literal, NULL, 0), type_get_size(result), 1);
                }
                break;
            }
    }

    if (is_complex)
    {
        result = get_complex_type(result);
        // Not a constant, at the moment
        *val = NULL;
    }

    // Special case for zero in C++
    CXX_LANGUAGE()
    {
        if (ASTType(expr) == AST_OCTAL_LITERAL
                && (strcmp(ASTText(expr), "0") == 0))
        {
            result = get_zero_type();
        }
    }

    return result;
}

// Given a character literal computes the type due to its lexic form
static type_t *character_literal_type(AST expr, const_value_t** val)
{
    const char *literal = ASTText(expr);

    type_t* result = NULL;
    if (*literal != 'L')
    {
        result = get_char_type();

        // literal[0] is the quote
        uint64_t value = 0;
        if (literal[1] != '\\')
        {
            value = literal[1];
        }
        else
        {
            switch (literal[2])
            {
                case '\'': { value = literal[2]; break; }
                case 'a' : { value = '\a'; break; }
                case 'b' : { value = '\b'; break; }
                case 'e' : { value = '\e'; break; } // This is a gcc extension
                case 'f' : { value = '\f'; break; }
                case 'n' : { value = '\n'; break; }
                case 'r' : { value = '\r'; break; }
                case 't' : { value = '\t'; break; }
                case 'v' : { value = '\v'; break; }
                case '\\': { value = '\\'; break; }
                case '\"': { value = '\"'; break; }
                case '0': 
                case '1': 
                case '2': 
                case '3': 
                case '4': 
                case '5': 
                case '6': 
                case '7': 
                       {
                           int i;
                           char c[32] = { 0 };
                           // Copy until the quote
                           for (i = 2; literal[i] != '\'' && literal[i] != '\0' ; i++)
                           {
                               c[i - 2] = literal[i];
                           }

                           char *err = NULL;
                           value = strtol(c, &err, 8);

                           if (!(*c != '\0'
                                       && *err == '\0'))
                           {
                               running_error("%s: error: %s does not seem a valid character literal\n", 
                                       ast_location(expr),
                                       prettyprint_in_buffer(expr));
                           }
                           break;
                       }
                case 'x':
                        {
                            int i;
                            char c[32] = { 0 };
                            // Copy until the quote
                            // Note literal_value is '\x000' so the number
                            // starts at literal_value[3]
                            for (i = 3; literal[i] != '\'' && literal[i] != '\0' ; i++)
                            {
                                c[i - 3] = literal[i];
                            }

                            char * err = NULL;
                            value = strtol(c, &err, 16);

                            if (!(*c != '\0'
                                        && *err == '\0'))
                            {
                                running_error("%s: error: %s does not seem a valid character literal\n", 
                                        ast_location(expr),
                                        prettyprint_in_buffer(expr));
                            }
                            break;
                        }
                default:
                    internal_error("Unhandled char literal '%s'", literal);
            }
        }

        // TODO - We need to know if char is signed or not
        *val = const_value_get(value, type_get_size(get_char_type()), /* sign */ 0);
    }
    else
    {
        result = get_wchar_t_type();
        // We do not evaluate wide chars as we need something more powerful
        // here
    }

    return result;
}

// Given a floating literal computes the type due to its lexic form
static type_t *floating_literal_type(AST expr)
{
    const char *literal = ASTText(expr);
    const char *last = literal + strlen(literal) - 1;

    char is_float = 0;
    char is_long_double = 0;
    char is_complex = 0;

    while (toupper(*last) == 'F' 
            || toupper(*last) == 'L'
            // This is a GNU extension for complex
            || toupper(*last) == 'I'
            || toupper(*last) == 'J')
    {
        switch (toupper(*last))
        {
            case 'L' :
                is_long_double++;
                break;
            case 'F' :
                is_float = 1;
                break;
            case 'I':
            case 'J':
                is_complex = 1;
                break;
            default:
                break;
        }
        last--;
    }

    type_t* result = NULL ;
    if (is_long_double)
    {
        result = get_long_double_type();
    }
    else if (is_float)
    {
        result = get_float_type();
    }
    else
    {
        result = get_double_type();
    }

    if (is_complex)
    {
        result = get_complex_type(result);
    }

    return result;
}

#define IS_OCTA_CHAR(_c) \
(((_c) >= '0') \
  && ((_c) <= '7'))

#define IS_HEXA_CHAR(_c) \
((((_c) >= '0') \
  && ((_c) <= '9')) \
 || (((_c) >= 'a') \
     && ((_c) <= 'f')) \
 || (((_c) >= 'A') \
     && ((_c) <= 'F')))

static void compute_length_of_literal_string(AST expr, int* length, char *is_wchar)
{
    // We allow the parser not to mix the two strings
    const char *literal = ASTText(expr);

    int num_of_strings_seen = 0;

    *is_wchar = 0;
    // At least the NULL is there
    *length = 1;

    // Beginning of a string
    while (*literal != '\0')
    {
        // C99 says that "a" L"b" is L"ab"
        // C++ says it is undefined
        // 
        // Do it like C99
        if ((*literal) == 'L')
        {
            (*is_wchar) = 1;
            // Advance L
            literal++;
        }

        ERROR_CONDITION(*literal != '"',
                "Lexic problem in the literal '%s'\n", ASTText(expr));

        // Advance the "
        literal++;

        // Advance till we find a '"'
        while (*literal != '"')
        {
            switch (*literal)
            {
                case '\\' :
                    {
                        // This is used for diagnostics
                        const char *beginning_of_escape = literal;

                        // A scape sequence
                        literal++;
                        switch (*literal)
                        {
                            case '\'' :
                            case '"' :
                            case '?' :
                            case '\\' :
                            case 'a' :
                            case 'b' :
                            case 'f' :
                            case 'n' :
                            case 'r' :
                            case 't' :
                            case 'v' :
                            case 'e' : // GNU Extension: A synonim for \033
                                {
                                    break;
                                }
                            case '0' :
                            case '1' :
                            case '2' :
                            case '3' :
                            case '4' :
                            case '5' :
                            case '6' :
                            case '7' :
                                // This is an octal
                                // Advance up to three octals
                                {
                                    // Advance this octal, so the remaining figures are 2
                                    literal++;
                                    int remaining_figures = 2;

                                    while (IS_OCTA_CHAR(*literal)
                                            && (remaining_figures > 0))
                                    {
                                        remaining_figures--;
                                        literal++;
                                    }
                                    // Go backwards because we have already
                                    // advanced the last element of this
                                    // escaped entity
                                    literal--;
                                    break;
                                }
                            case 'x' :
                                // This is an hexadecimal
                                {
                                    // Jump 'x' itself
                                    literal++;

                                    while (IS_HEXA_CHAR(*literal))
                                    {
                                            literal++;
                                    }

                                    // Go backwards because we have already
                                    // advanced the last element of this
                                    // escaped entity
                                    literal--;
                                    break;
                                }
                            case 'u' :
                            case 'U' :
                                {
                                    // Universal names are followed by 4 hexa digits
                                    // or 8 depending on 'u' or 'U' respectively
                                    char remaining_hexa_digits = 8;
                                    if (*literal == 'u')
                                    {
                                        remaining_hexa_digits = 4;
                                    }

                                    // Advance 'u'/'U'
                                    literal++;

                                    while (remaining_hexa_digits > 0)
                                    {
                                        if (!IS_HEXA_CHAR(*literal))
                                        {
                                            char ill_literal[11];
                                            strncpy(ill_literal, beginning_of_escape, /* hexa */ 8 + /* escape */ 1 + /* null*/ 1 );
                                            running_error("%s: error: invalid universal literal name '%s'", 
                                                    ast_location(expr),
                                                    ill_literal);
                                        }

                                        literal++;
                                        remaining_hexa_digits--;
                                    }

                                    // Go backwards one
                                    literal--;
                                }
                            default:
                                {
                                    char c[3];

                                    strncpy(c, beginning_of_escape, 3);
                                    running_error("%s: error: invalid escape sequence '%s'\n",
                                            ast_location(expr),
                                            c);
                                }
                        }
                        break;
                    }
                default:
                    {
                        // Do nothing with this one
                        break;
                    }
            }

            // Make 'literal' always point to the last thing that represents one char/wchar_t
            //
            // For instance, for "\n", "\002", "\uabcd" and "\U98abcdef" (*literal) should
            // be 'n', '2', 'd' and 'f' respectively.
            literal++;

            (*length)++;
        }

        // Advance the "
        literal++;

        num_of_strings_seen++;
    }

    ERROR_CONDITION(num_of_strings_seen == 0, "Empty string literal '%s'\n", ASTText(expr));
}

static type_t *string_literal_type(AST expr)
{
    char is_wchar = 0;
    int length = 0;

    compute_length_of_literal_string(expr, &length, &is_wchar);

    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: String literal %s type is '%s[%d]'\n",
                ASTText(expr),
                !is_wchar ? "char" : "wchar_t",
                length);
    }

    type_t* result = get_literal_string_type(length, is_wchar);

    return result;
}

struct bin_operator_funct_type_t
{
    type_t* (*func)(AST expr, AST lhs, AST rhs, decl_context_t, const_value_t**);
};

struct unary_operator_funct_type_t
{
    type_t* (*func)(AST expr, AST operand, decl_context_t, const_value_t**);
};

#define OPERATOR_FUNCT_INIT(_x) { .func = _x }

static 
char operand_is_class_or_enum(type_t* op_type)
{
    return (is_enum_type(op_type)
            || is_class_type(op_type));
}

static 
char any_operand_is_class_or_enum(type_t* lhs_type, type_t* rhs_type)
{
    return operand_is_class_or_enum(lhs_type)
        || operand_is_class_or_enum(rhs_type);
}

static char is_promoteable_integral_type(type_t* t)
{
    return (is_signed_char_type(t)
            || is_unsigned_char_type(t)
            || is_signed_short_int_type(t)
            || is_unsigned_short_int_type(t)
            || is_bool_type(t)
            || is_enum_type(t)
            || is_wchar_t_type(t));
}

static type_t* promote_integral_type(type_t* t)
{
    ERROR_CONDITION(!is_promoteable_integral_type(t), 
            "This type cannot be promoted!", 0);

    if (is_enum_type(t))
    {
        return enum_type_get_underlying_type(t);
    }
    else if (is_wchar_t_type(t))
    {
        // FIXME - We sould get the underlying wchar type
        return get_signed_int_type();
    }
    else // char, bool or short
    {
        return get_signed_int_type();
    }
}

static 
char both_operands_are_integral(type_t* lhs_type, type_t* rhs_type)
{
    return (is_integral_type(lhs_type) || is_enum_type(lhs_type))
        && (is_integral_type(rhs_type) || is_enum_type(lhs_type));
};

static 
char both_operands_are_arithmetic(type_t* lhs_type, type_t* rhs_type)
{
    return is_arithmetic_type(lhs_type)
        && is_arithmetic_type(rhs_type);
}

static char both_operands_are_vector_types(type_t* lhs_type, type_t* rhs_type)
{
    return is_vector_type(lhs_type)
        && is_vector_type(rhs_type)
        && equivalent_types(get_unqualified_type(lhs_type), get_unqualified_type(rhs_type));
}

static char is_pointer_and_integral_type(type_t* lhs_type, type_t* rhs_type)
{
    if (is_array_type(lhs_type))
    {
        // Convert to a pointer
        lhs_type = get_pointer_type(array_type_get_element_type(lhs_type));
    }

    return (is_pointer_type(lhs_type)
            && is_integral_type(rhs_type));
}

static 
char is_pointer_arithmetic(type_t* lhs_type, type_t* rhs_type)
{
    return is_pointer_and_integral_type(lhs_type, rhs_type)
        || is_pointer_and_integral_type(rhs_type, lhs_type);
}

static char long_int_can_represent_unsigned_int(void)
{
    static char result = 2;

    if (result == 2)
    {
        const_value_t* min_signed_long_int = integer_type_get_minimum(get_signed_long_int_type());
        const_value_t* max_signed_long_int = integer_type_get_maximum(get_signed_long_int_type());

        const_value_t* min_unsigned_int = integer_type_get_minimum(get_unsigned_int_type());
        const_value_t* max_unsigned_int = integer_type_get_maximum(get_unsigned_int_type());

#define B_(x) const_value_is_nonzero(x)
        if (B_(const_value_lte(min_signed_long_int, min_unsigned_int))
                && B_(const_value_lte(max_unsigned_int, max_signed_long_int)))
        {
            result = 1;
        }
        else
        {
            result = 0;
        }
#undef B_
    }

    return result;
}

static char long_long_int_can_represent_unsigned_long_int(void)
{
    static char result = 2;

    if (result == 2)
    {
        const_value_t* min_signed_long_long_int = integer_type_get_minimum(get_signed_long_long_int_type());
        const_value_t* max_signed_long_long_int = integer_type_get_maximum(get_signed_long_long_int_type());

        const_value_t* min_unsigned_long_int = integer_type_get_minimum(get_unsigned_long_int_type());
        const_value_t* max_unsigned_long_int = integer_type_get_maximum(get_unsigned_long_int_type());

#define B_(x) const_value_is_nonzero(x)
        if (B_(const_value_lte(min_signed_long_long_int, min_unsigned_long_int))
                && B_(const_value_lte(max_unsigned_long_int, max_signed_long_long_int)))
        {
            result = 1;
        }
        else
        {
            result = 0;
        }
#undef B_
    }

    return result;
}

static type_t* usual_arithmetic_conversions(type_t* lhs_type, type_t* rhs_type)
{
    ERROR_CONDITION(!is_arithmetic_type(lhs_type)
            || !is_arithmetic_type(rhs_type),
            "Both should be arithmetic types", 0);

    char is_complex = is_complex_type(lhs_type)
       || is_complex_type(rhs_type); 

    if (is_complex_type(lhs_type))
    {
        lhs_type = complex_type_get_base_type(lhs_type);
    }
    if (is_complex_type(rhs_type))
    {
        rhs_type = complex_type_get_base_type(rhs_type);
    }

    // Floating point case is easy
    if (is_floating_type(lhs_type)
            || is_floating_type(rhs_type))
    {
        type_t* result = NULL;
        if (is_long_double_type(lhs_type)
                || is_long_double_type(rhs_type))
        {
            result = get_long_double_type();
        }
        else if (is_double_type(lhs_type)
                || is_double_type(rhs_type))
        {
            result = get_double_type();
        }
        else
        {
            result = get_float_type();
        }

        if (is_complex)
        {
            result = get_complex_type(result);
        }

        return result;
    }

    // Perform integral promotions
    {
        type_t** types[2] = {&lhs_type, &rhs_type};
        int i;
        for (i = 0; i < 2; i++)
        {
            type_t** curr_type = types[i];

            if (is_promoteable_integral_type(*curr_type))
                (*curr_type) = promote_integral_type((*curr_type));
        }
    }

    ERROR_CONDITION(!is_any_int_type(lhs_type) || !is_any_int_type(rhs_type),
            "Error, the types are wrong, they should be either of integer nature at this point", 0);

    type_t* result = NULL;
    // If either is unsigned long long, convert to unsigned long long
    if (is_unsigned_long_long_int_type(lhs_type)
            || is_unsigned_long_long_int_type(rhs_type))
    {
        result = get_unsigned_long_long_int_type();
    }
    // If one operand is a long long int and the other unsigned long int, then
    // if a long long int can represent all the values of an unsigned long int,
    // the unsigned long int shall be converted to a long long int; otherwise
    // both operands shall be converted to unsigned long long int.
    else if ((is_signed_long_long_int_type(lhs_type)
                && is_unsigned_long_int_type(rhs_type))
            || (is_signed_long_long_int_type(rhs_type)
                && is_unsigned_long_int_type(lhs_type)))
    {
        if (long_long_int_can_represent_unsigned_long_int())
        {
            result = get_signed_long_long_int_type();
        }
        else
        {
            result = get_unsigned_long_long_int_type();
        }
    }
    // If either is signed long long, convert to signed long long
    else if (is_signed_long_long_int_type(lhs_type)
            || is_signed_long_long_int_type(rhs_type))
    {
        result = get_signed_long_long_int_type();
    }
    // If either is unsigned long convert to unsigned long
    else if (is_unsigned_long_int_type(lhs_type)
            || is_unsigned_long_int_type(rhs_type))
    {
        result = get_unsigned_long_int_type();
    }
    // If one operand is a long int and the other unsigned int, then if a long
    // int can represent all the values of an unsigned int, the unsigned int
    // shall be converted to a long int; otherwise both operands shall be
    // converted to unsigned long int.
    else if ((is_signed_long_int_type(lhs_type)
                && is_unsigned_int_type(rhs_type))
            || (is_signed_long_int_type(rhs_type)
                && is_unsigned_int_type(lhs_type)))
    {
        if (long_int_can_represent_unsigned_int())
        {
            result = get_signed_long_int_type();
        }
        else
        {
            result = get_unsigned_long_int_type();
        }
    }
    // If either is signed long, convert to signed long
    else if (is_signed_long_int_type(lhs_type)
            || is_signed_long_int_type(rhs_type))
    {
        result = get_signed_long_int_type();
    }
    // If either is unsigned int the the other should be
    else if (is_unsigned_int_type(lhs_type)
            || is_unsigned_int_type(rhs_type))
    {
        result = get_unsigned_int_type();
    }
    // both should be int here
    else if (!is_signed_int_type(lhs_type)
            || !is_signed_int_type(rhs_type))
    {
        internal_error("Unreachable code", 0);
    }
    else 
    {
        result = get_signed_int_type();
    }

    if (is_complex)
    {
        result = get_complex_type(result);
    }

    return result;
}

static
type_t* compute_arithmetic_builtin_bin_op(type_t* lhs_type, type_t* rhs_type)
{
    return usual_arithmetic_conversions(lhs_type, rhs_type);
}

static type_t* compute_pointer_arithmetic_type(type_t* lhs_type, type_t* rhs_type)
{
    type_t* return_type = NULL;

    type_t* types[2] = {lhs_type, rhs_type};

    int i;
    for (i = 0; i < 2; i++)
    {
        type_t* current_type = types[i];

        if (is_pointer_type(current_type) || is_array_type(current_type))
        {
            if (is_pointer_type(current_type))
            {
                return_type = current_type;
            }
            else if (is_array_type(current_type))
            {
                return_type = get_pointer_type(array_type_get_element_type(current_type));
            }
        }
    }

    return return_type;
}

static char filter_only_nonmembers(scope_entry_t* e)
{
    if (e->kind != SK_FUNCTION 
            && e->kind != SK_TEMPLATE)
        return 0;

    if (e->kind == SK_TEMPLATE)
    {
        e = named_type_get_symbol(template_type_get_primary_type(e->type_information));
        if (e->kind != SK_FUNCTION)
            return 0;
    }

    if (!e->entity_specs.is_member)
        return 1;

    return 0;
}

static void error_message_delete_call(decl_context_t decl_context, AST expr, scope_entry_t* entry)
{
    running_error("%s: error: call to deleted function '%s'\n",
            ast_location(expr),
            print_decl_type_str(entry->type_information, decl_context,
                get_qualified_symbol_name(entry, decl_context)));
}

static void ensure_not_deleted(decl_context_t decl_context, AST expr, scope_entry_t* entry)
{
    if (entry->entity_specs.is_deleted)
    {
        error_message_delete_call(decl_context, expr, entry);
    }
}

static void error_message_overload_failed(decl_context_t decl_context, AST expr, candidate_t* candidates)
{
    fprintf(stderr, "%s: warning: overload call to '%s' failed\n",
            ast_location(expr),
            prettyprint_in_buffer(expr));

    if (candidates != NULL)
    {
        fprintf(stderr, "%s: note: candidates are\n", ast_location(expr));

        candidate_t* it = candidates;
        while (it != NULL)
        {
            scope_entry_t* entry = it->entry;

            const char* file = entry->file != NULL ? entry->file : ASTFileName(expr);
            int line = entry->file != NULL ? (unsigned)entry->line : (unsigned)ASTLine(expr);

            fprintf(stderr, "%s:%d: note:    %s",
                    file, line,
                    print_decl_type_str(entry->type_information, decl_context, 
                        get_qualified_symbol_name(entry, decl_context)));

            if (entry->entity_specs.is_builtin)
            {
                fprintf(stderr, " [built-in]");
            }
            fprintf(stderr, "\n");
            it = it->next;
        }
    }
    else
    {
        fprintf(stderr, "%s: note: no candidate functions\n", ast_location(expr));
    }
}

static type_t* compute_user_defined_bin_operator_type(AST operator_name, 
        AST expr,
        AST lhs, AST rhs, 
        scope_entry_list_t* builtins,
        decl_context_t decl_context,
        scope_entry_t** selected_operator)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    type_t* argument_types[2] = { lhs_type, rhs_type };
    int num_arguments = 2;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(lhs_type)))
    {
        scope_entry_list_t* operator_entry_list = get_member_function_of_class_type(no_ref(lhs_type), 
                operator_name, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                ASTFileName(expr), ASTLine(expr),
                /* explicit template arguments */ NULL);
        entry_list_free(operator_entry_list);
    }

    // This uses Koenig, otherwise some operators might not be found
    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, operator_name);

    // Normal lookup might find member functions at this point, filter them
    scope_entry_list_t* nonmember_entry_list = filter_symbol_using_predicate(entry_list, filter_only_nonmembers);
    entry_list_free(entry_list);
    
    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(nonmember_entry_list,
            builtins, argument_types, num_arguments,
            decl_context,
            ASTFileName(expr), ASTLine(expr),
            /* explicit template arguments */ NULL);
    entry_list_free(nonmember_entry_list);

    scope_entry_list_t* old_overload_set = overload_set;
    overload_set = entry_list_merge(old_overload_set, operator_overload_set);
    entry_list_free(old_overload_set);
    entry_list_free(operator_overload_set);

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = add_to_candidate_set(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);
    entry_list_free(overload_set);

    scope_entry_t* conversors[2] = { NULL, NULL };

    scope_entry_t *overloaded_call = solve_overload(candidate_set,
            decl_context,
            ASTFileName(expr), ASTLine(expr), conversors);

    type_t* overloaded_type = NULL;
    if (overloaded_call != NULL)
    {
        ensure_not_deleted(decl_context, expr, overloaded_call);

        if (conversors[0] != NULL)
        {
            ensure_not_deleted(decl_context, expr, conversors[0]);

            ASTAttrSetValueType(lhs, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(lhs, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[0]));
        }
        if (conversors[1] != NULL)
        {
            ensure_not_deleted(decl_context, expr, conversors[1]);

            ASTAttrSetValueType(rhs, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(rhs, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[1]));
        }

        if (!overloaded_call->entity_specs.is_builtin)
        {
            ASTAttrSetValueType(expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(overloaded_call));
        }

        *selected_operator = overloaded_call;

        overloaded_type = overloaded_call->type_information;

        expression_set_type(expr, function_type_get_return_type(overloaded_type));
        expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));
    }
    else if (!checking_ambiguity())
    {
        error_message_overload_failed(decl_context, expr, candidate_set);
    }
    return overloaded_type;
}


static type_t* compute_user_defined_unary_operator_type(AST operator_name, 
        AST expr,
        AST op,
        scope_entry_list_t* builtins,
        decl_context_t decl_context,
        scope_entry_t** selected_operator)
{
    type_t* op_type = expression_get_type(op);

    type_t* argument_types[1] = { op_type };
    int num_arguments = 1;

    candidate_t* candidate_set = NULL;

    if (is_class_type(no_ref(op_type)))
    {
        scope_entry_list_t* operator_entry_list = get_member_function_of_class_type(no_ref(op_type), 
                operator_name, decl_context);

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            // It is impossible to deduce anything since a unary overloaded
            // operator has zero parameters, so discard templates at this point
            if (entry->kind != SK_TEMPLATE)
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        entry,
                        num_arguments,
                        argument_types);
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(operator_entry_list);
    }

    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, operator_name);

    // Remove any member that might have slip in because of plain lookup
    scope_entry_list_t* nonmember_entry_list = filter_symbol_using_predicate(entry_list, filter_only_nonmembers);
    entry_list_free(entry_list);
    
    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            nonmember_entry_list, builtins, argument_types, num_arguments,
            decl_context,
            ASTFileName(expr), ASTLine(expr),
            /* explicit_template_arguments */ NULL);
    entry_list_free(nonmember_entry_list);

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = add_to_candidate_set(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);
    entry_list_free(overload_set);

    scope_entry_t* conversors[1] = { NULL };
    
    scope_entry_t *overloaded_call = solve_overload(candidate_set,
            decl_context, 
            ASTFileName(expr), ASTLine(expr), 
            conversors);

    type_t* overloaded_type = NULL;
    if (overloaded_call != NULL)
    {
        ensure_not_deleted(decl_context, expr, overloaded_call);

        if (!overloaded_call->entity_specs.is_builtin)
        {
            *selected_operator = overloaded_call;

            ASTAttrSetValueType(expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(overloaded_call));
        }

        if (conversors[0] != NULL)
        {
            ensure_not_deleted(decl_context, expr, conversors[0]);

            ASTAttrSetValueType(op, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(op, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[0]));
        }

        overloaded_type = overloaded_call->type_information;

        expression_set_type(expr, function_type_get_return_type(overloaded_type));
        expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));
    }
    else if (!checking_ambiguity())
    {
        error_message_overload_failed(decl_context, expr, candidate_set);
    }
    return overloaded_type;
}

static char operator_bin_plus_builtin_pred(type_t* lhs, type_t* rhs)
{
    // <arithmetic> + <arithmetic>
    return ((is_arithmetic_type(no_ref(lhs))
                && is_arithmetic_type(no_ref(rhs)))
            // T* + <arithmetic>
            || ((is_pointer_type(no_ref(lhs)) || is_array_type(no_ref(lhs)))
                && is_arithmetic_type(no_ref(rhs)))
            // <arithmetic> + T*
            || (is_arithmetic_type(no_ref(lhs))
                && (is_pointer_type(no_ref(rhs)) || is_array_type(no_ref(rhs)))));
}

static type_t* operator_bin_plus_builtin_result(type_t** lhs, type_t** rhs)
{
    if (is_arithmetic_type(no_ref(*lhs))
        && is_arithmetic_type(no_ref(*rhs)))
    {
        if (is_promoteable_integral_type(no_ref(*lhs)))
            *lhs = promote_integral_type(no_ref(*lhs));

        if (is_promoteable_integral_type(no_ref(*rhs)))
            *rhs = promote_integral_type(no_ref(*rhs));

        return usual_arithmetic_conversions(no_ref(*lhs), no_ref(*rhs));
    }
    else if (is_pointer_arithmetic(no_ref(*lhs), no_ref(*rhs)))
    {
        type_t** pointer_type = NULL;
        if (is_pointer_type(no_ref(*lhs))
                || is_array_type(no_ref(*lhs)))
        {
            pointer_type = lhs;
        }
        else
        {
            pointer_type = rhs;
        }

        if (is_array_type(no_ref(*pointer_type)))
        {
            *pointer_type = get_pointer_type(array_type_get_element_type(no_ref(*pointer_type)));
        }

        return *pointer_type;
    }

    return NULL;
}

static
type_t* compute_bin_operator_add_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

            if (val != NULL
                    && computed_type != NULL
                    && both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type))
                    && expression_is_constant(lhs)
                    && expression_is_constant(rhs))
            {
                *val = const_value_add(expression_get_constant(lhs),
                        expression_get_constant(rhs));
            }
        }
        else if (is_pointer_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_pointer_arithmetic_type(no_ref(lhs_type), no_ref(rhs_type));
        }
        // Vector case
        else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = lhs_type;
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    // Now in C++ we have to rely on overloading for operators
    static AST operation_add_tree = NULL;
    if (operation_add_tree == NULL)
    {
        operation_add_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operation_add_tree, 
            operator_bin_plus_builtin_pred,
            operator_bin_plus_builtin_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_bin_operator_type(operation_add_tree, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && result != NULL
            && selected_operator != NULL
            && selected_operator->entity_specs.is_builtin
            && both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_add(expression_get_constant(lhs),
                expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_only_arithmetic_pred(type_t* lhs, type_t* rhs)
{
    return (is_arithmetic_type(no_ref(lhs))
            && is_arithmetic_type(no_ref(rhs)));
}

static type_t* operator_bin_only_arithmetic_result(type_t** lhs, type_t** rhs)
{
    if (is_promoteable_integral_type(no_ref(*lhs)))
        *lhs = promote_integral_type(no_ref(*lhs));

    if (is_promoteable_integral_type(no_ref(*rhs)))
        *rhs = promote_integral_type(no_ref(*rhs));

    return usual_arithmetic_conversions(no_ref(*lhs), no_ref(*rhs));
}

static
type_t* compute_bin_operator_only_arithmetic_types(AST expr, AST lhs, AST rhs, AST operator, decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));
        }
        else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = lhs_type;
        }
        else
        {
            return NULL;
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_only_arithmetic_pred,
            operator_bin_only_arithmetic_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // Now in C++ we have to rely on overloading for operators
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static
type_t* compute_bin_operator_mul_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MULT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_arithmetic_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_mul(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static
type_t* compute_bin_operator_div_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIV_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_arithmetic_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_div(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_only_integer_pred(type_t* lhs, type_t* rhs)
{
    return (is_integer_type(lhs) 
            && is_integer_type(rhs));
}

static type_t* operator_bin_only_integer_result(type_t** lhs, type_t** rhs)
{
    if (is_promoteable_integral_type(no_ref(*lhs)))
        *lhs = promote_integral_type(no_ref(*lhs));

    if (is_promoteable_integral_type(no_ref(*rhs)))
        *rhs = promote_integral_type(no_ref(*rhs));

    return usual_arithmetic_conversions(no_ref(*lhs), no_ref(*rhs));
}

static 
type_t* compute_bin_operator_only_integer_types(AST expr, AST lhs, AST rhs, AST operator, decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));
        }
        else
            return NULL;

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_only_integer_pred,
            operator_bin_only_integer_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // Now in C++ we have to rely on overloading for operators
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static
type_t* compute_bin_operator_mod_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MOD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integer_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(expression_get_type(lhs), expression_get_type(rhs))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_mod(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_sub_builtin_pred(type_t* lhs, type_t* rhs)
{
    // <arithmetic> - <arithmetic>
    return ((is_arithmetic_type(lhs)
                && is_arithmetic_type(rhs))
            // T* - <arithmetic>
            || ((is_pointer_type(lhs) || is_array_type(lhs))
                && is_arithmetic_type(rhs)));
}

static type_t* operator_bin_sub_builtin_result(type_t** lhs, type_t** rhs)
{
    if (is_arithmetic_type(*lhs)
        && is_arithmetic_type(*rhs))
    {
        if (is_promoteable_integral_type(*lhs))
            *lhs = promote_integral_type(*lhs);

        if (is_promoteable_integral_type(*rhs))
            *rhs = promote_integral_type(*rhs);

        return usual_arithmetic_conversions(*lhs, *rhs);
    }
    else if(((is_pointer_type(*lhs) || is_array_type(*rhs))
                && is_arithmetic_type(*rhs)))
    {
        type_t** pointer_type = lhs;

        if (is_array_type(*pointer_type))
        {
            *pointer_type = get_pointer_type(array_type_get_element_type(*pointer_type));
        }

        return *pointer_type;
    }

    return NULL;
}

static type_t* compute_bin_operator_sub_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));
            if (computed_type != NULL
                    && val != NULL
                    && both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type))
                    && expression_is_constant(lhs)
                    && expression_is_constant(rhs))
            {
                *val = const_value_sub(expression_get_constant(lhs), expression_get_constant(rhs));
            }
        }
        else if (is_pointer_and_integral_type(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = compute_pointer_arithmetic_type(no_ref(lhs_type), no_ref(rhs_type));
        }
        else if (pointer_types_are_similar(no_ref(lhs_type), no_ref(rhs_type)))
        {
            // FIXME, this should the type related to ptrdiff_t (usually int)
            computed_type = get_signed_int_type();
        }
        else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
        {
            computed_type = lhs_type;
        }
        else
        {
            return NULL;
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    static AST operator = NULL;
    if (operator == NULL)
    {
        operator = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MINUS_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_sub_builtin_pred,
            operator_bin_sub_builtin_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && result != NULL
            && selected_operator != NULL
            && selected_operator->entity_specs.is_builtin
            && both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_sub(expression_get_constant(lhs),
                expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_left_integral_right_integral_pred(type_t* lhs, type_t* rhs)
{
    return (is_integral_type(lhs)
            && is_integral_type(rhs));
}

static type_t* operator_bin_left_integral_result(type_t** lhs, type_t** rhs)
{
    if (is_promoteable_integral_type(*lhs))
    {
        *lhs = promote_integral_type(*lhs);
    }

    if (is_promoteable_integral_type(*rhs))
    {
        *rhs = promote_integral_type(*rhs);
    }

    return (*lhs);
}


static type_t* compute_bin_operator_only_integral_lhs_type(AST expr, AST lhs, AST rhs, AST operator,
        decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
        {
            // Always the left one in this case
            computed_type = no_ref(lhs_type);
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_left_integral_right_integral_pred,
            operator_bin_left_integral_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static type_t* compute_bin_operator_shl_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LEFT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integral_lhs_type(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_shl(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_shr_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_RIGHT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integral_lhs_type(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_shr(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_arithmetic_pointer_or_enum_pred(type_t* lhs, type_t* rhs)
{
    // Two arithmetics or enum or pointer
    return ((is_arithmetic_type(lhs)
                && is_arithmetic_type(rhs))
            // T* < T*
            || ((is_pointer_type(lhs) || is_array_type(lhs))
                && (is_pointer_type(rhs) || is_array_type(rhs))
                && equivalent_types(get_unqualified_type(lhs), get_unqualified_type(rhs)))
            // Silly case for zero_type
            || (is_pointer_type(lhs) && is_zero_type(rhs))
            || (is_zero_type(lhs) && is_pointer_type(rhs))
            // enum E < enum E
            || (is_enum_type(lhs)
                && is_enum_type(rhs)
                && equivalent_types(get_unqualified_type(lhs), get_unqualified_type(rhs))));
}

static type_t* operator_bin_arithmetic_pointer_or_enum_result(type_t** lhs, type_t** rhs)
{
    if (is_arithmetic_type(*lhs)
            && is_arithmetic_type(*rhs))
    {
        if (is_promoteable_integral_type(*lhs))
            *lhs = promote_integral_type(*lhs);
        if (is_promoteable_integral_type(*rhs))
            *rhs = promote_integral_type(*rhs);

        return get_bool_type();
    }
    else if ((is_pointer_type(*lhs) || is_array_type(*lhs))
            && (is_pointer_type(*rhs) || is_array_type(*rhs))
            && equivalent_types(get_unqualified_type(*lhs), get_unqualified_type(*rhs)))
    {
        if (is_array_type(*lhs))
        {
            *lhs = get_pointer_type(array_type_get_element_type(*lhs));
        }

        if (is_array_type(*rhs))
        {
            *rhs = get_pointer_type(array_type_get_element_type(*rhs));
        }

        return get_bool_type();
    }
    else if ((is_zero_type(*lhs) && is_pointer_type(*rhs))
            || (is_pointer_type(*lhs) && is_zero_type(*rhs)))
    {
        // Convert the zero type to the other pointer type
        if (is_zero_type(*lhs))
        {
            *lhs = *rhs;
        }
        if (is_zero_type(*rhs))
        {
            *rhs = *lhs;
        }

        return get_bool_type();
    }
    else if (is_enum_type(no_ref(*lhs))
            && is_enum_type(no_ref(*rhs))
            && equivalent_types(get_unqualified_type(no_ref(*lhs)), get_unqualified_type(no_ref(*rhs))))
    {
        return get_bool_type();
    }
    return NULL;
}

static type_t* compute_bin_operator_relational(AST expr, AST lhs, AST rhs, AST operator, decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    type_t* no_ref_lhs_type = no_ref(lhs_type);
    type_t* no_ref_rhs_type = no_ref(rhs_type);

    char requires_overload = 0;

    CXX_LANGUAGE()
    {
        // Try to simplify unresolved overloads
        type_t** op_types[] = { &no_ref_lhs_type, &no_ref_rhs_type, NULL };
        int i;
        for (i = 0; op_types[i] != NULL; i++)
        {
            if (is_unresolved_overloaded_type(*(op_types[i])))
            {
                scope_entry_t* function = unresolved_overloaded_type_simplify(*(op_types[i]),
                        decl_context, ASTLine(lhs), ASTFileName(lhs));
                if (function != NULL)
                {
                    *(op_types[i]) = get_pointer_type(function->type_information);
                }
            }
        }

        requires_overload = any_operand_is_class_or_enum(no_ref_lhs_type, no_ref_rhs_type);
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;

        if (both_operands_are_arithmetic(no_ref_lhs_type, no_ref_rhs_type)
                || pointer_types_are_similar(no_ref_lhs_type, no_ref_rhs_type))
        {
            C_LANGUAGE()
            {
                computed_type = get_signed_int_type();
            }
            CXX_LANGUAGE()
            {
                computed_type = get_bool_type();
            }
        }
        else if (both_operands_are_vector_types(no_ref_lhs_type, no_ref_rhs_type))
        {
            computed_type = lhs_type;
        }
        else
        {
            return NULL;
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            no_ref_lhs_type, no_ref_rhs_type, 
            &builtin_set,
            decl_context, operator, 
            operator_bin_arithmetic_pointer_or_enum_pred,
            operator_bin_arithmetic_pointer_or_enum_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static type_t* compute_bin_operator_lower_equal_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, 
        const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LESS_OR_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_lte(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_lower_than_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOWER_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_lt(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_greater_equal_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_GREATER_OR_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_gte(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_greater_than_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_GREATER_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_gte(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_different_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIFFERENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_neq(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_equal_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_relational(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_eq(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_logical_types_pred(type_t* lhs, type_t* rhs)
{
    standard_conversion_t dummy;
    return (standard_conversion_between_types(&dummy, lhs, get_bool_type())
            && standard_conversion_between_types(&dummy, rhs, get_bool_type()));
}

static type_t* operator_bin_logical_types_result(type_t** lhs, type_t** rhs)
{
    // We want the prototype of the builtin operation as 'bool operator#(bool, bool)' not
    // 'bool operator#(L, R)' with L and R convertible to bool
    *lhs = get_bool_type();
    *rhs = get_bool_type();

    return get_bool_type();
}

static type_t* compute_bin_logical_op_type(AST expr, AST lhs, AST rhs, AST operator, decl_context_t decl_context)
{
    standard_conversion_t lhs_to_bool;
    standard_conversion_t rhs_to_bool;

    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    type_t* conversion_type = NULL;

    char requires_overload = 0;

    C_LANGUAGE()
    {
        conversion_type = get_signed_int_type();
    }
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
        conversion_type = get_bool_type();
    }

    if (!requires_overload
            && standard_conversion_between_types(&lhs_to_bool, lhs_type, conversion_type)
            && standard_conversion_between_types(&rhs_to_bool, rhs_type, conversion_type))
    {
        type_t* computed_type = NULL;
        C_LANGUAGE()
        {
            computed_type = get_signed_int_type();
        }
        CXX_LANGUAGE()
        {
            computed_type = get_bool_type();
        }

        expression_set_type(expr, computed_type);
        expression_set_is_lvalue(expr, 0);

        return computed_type;
    }

    C_LANGUAGE()
    {
        return NULL;
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_logical_types_pred,
            operator_bin_logical_types_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static type_t* compute_bin_operator_logical_or_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_OR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_logical_op_type(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_or(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_logical_and_type(AST expr, AST lhs, AST rhs, decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_AND_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_logical_op_type(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_and(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_bitwise_and_type(AST expr, AST lhs, AST rhs, 
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_AND_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integer_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_bitand(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_bitwise_or_type(AST expr, AST lhs, AST rhs, 
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_OR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integer_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_bitor(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static type_t* compute_bin_operator_bitwise_xor_type(AST expr, AST lhs, AST rhs, 
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_XOR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    type_t* result = compute_bin_operator_only_integer_types(expr, lhs, rhs, operation_tree, decl_context);

    if (result != NULL
            && val != NULL
            && both_operands_are_integral(no_ref(expression_get_type(lhs)), 
                no_ref(expression_get_type(rhs)))
            && expression_is_constant(lhs)
            && expression_is_constant(rhs))
    {
        *val = const_value_bitxor(expression_get_constant(lhs), expression_get_constant(rhs));
    }

    return result;
}

static char operator_bin_assign_only_integer_pred(type_t* lhs, type_t* rhs)
{
    return (is_lvalue_reference_type(lhs)
            && is_integral_type(reference_type_get_referenced_type(lhs))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs))
            && is_integral_type(no_ref(rhs)));
}

static type_t* operator_bin_assign_only_integer_result(type_t** lhs, type_t** rhs)
{
    type_t* ref_type = reference_type_get_referenced_type(*lhs);

    cv_qualifier_t cv_qualif = CV_NONE;
    advance_over_typedefs_with_cv_qualif(ref_type, &cv_qualif);

    if (is_promoteable_integral_type(*rhs))
        *rhs = promote_integral_type(*rhs);

    type_t* result = get_lvalue_reference_type(
            get_cv_qualified_type(ref_type, cv_qualif));

    return result;
}

static type_t* compute_bin_operator_assig_only_integral_type(AST expr, AST lhs, AST rhs, AST operator,
        decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        C_LANGUAGE()
        {
            if (!expression_is_lvalue(lhs))
                return NULL;
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(lhs_type)
                    || is_const_qualified_type(reference_type_get_referenced_type(lhs_type)))
                return NULL;
        }

        if (!both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
        {
            return NULL;
        }

        expression_set_type(expr, lhs_type);
        expression_set_is_lvalue(expr, 1);
        return lhs_type;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            // Note that the left is not removed its reference type
            lhs_type, no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_assign_only_integer_pred,
            operator_bin_assign_only_integer_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // We need to do overload
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static char operator_bin_assign_arithmetic_or_pointer_pred(type_t* lhs, type_t* rhs)
{
    return (is_lvalue_reference_type(lhs)
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs))
            && (both_operands_are_arithmetic(reference_type_get_referenced_type(lhs), no_ref(rhs))
                || is_pointer_arithmetic(reference_type_get_referenced_type(lhs), no_ref(rhs))));
}

static type_t* operator_bin_assign_arithmetic_or_pointer_result(type_t** lhs, type_t** rhs)
{
    type_t* ref_type = reference_type_get_referenced_type(*lhs);
    cv_qualifier_t cv_qualif = CV_NONE;

    advance_over_typedefs_with_cv_qualif(ref_type, &cv_qualif);

    if (both_operands_are_arithmetic(ref_type, no_ref(*rhs)))
    {
        if (is_promoteable_integral_type(*rhs))
            *rhs = promote_integral_type(*rhs);

        return *lhs;
    }
    else if (is_pointer_arithmetic(*lhs, *rhs))
    {
        return *lhs;
    }

    return NULL;
}

static type_t* compute_bin_operator_assig_arithmetic_or_pointer_type(AST expr, AST lhs, AST rhs, AST operator,
        decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        C_LANGUAGE()
        {
            if (!expression_is_lvalue(lhs))
                return NULL;
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(lhs_type)
                    || is_const_qualified_type(reference_type_get_referenced_type(lhs_type)))
                return NULL;
        }

        if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type))
                || is_pointer_arithmetic(no_ref(lhs_type), no_ref(rhs_type))
                || both_operands_are_vector_types(no_ref(lhs_type), 
                    no_ref(rhs_type)))
        {
            expression_set_type(expr, lhs_type);
            expression_set_is_lvalue(expr, 1);
            return lhs_type;
        }

        return NULL;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            // Note that the left is not removed its reference type
            lhs_type, no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_assign_arithmetic_or_pointer_pred,
            operator_bin_assign_arithmetic_or_pointer_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // We need to do overload
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static char operator_bin_assign_only_arithmetic_pred(type_t* lhs, type_t* rhs)
{
    return (is_lvalue_reference_type(lhs)
            && is_arithmetic_type(reference_type_get_referenced_type(lhs))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs))
            && is_arithmetic_type(rhs));
}

static type_t* operator_bin_assign_only_arithmetic_result(type_t** lhs, type_t** rhs)
{
    if (is_promoteable_integral_type(*rhs))
        *rhs = promote_integral_type(*rhs);

    return *lhs;
}

static
void build_binary_nonop_assign_builtin(type_t* lhs_type, 
        builtin_operators_set_t *result,
        AST operator, decl_context_t decl_context)
{
    memset(result, 0, sizeof(*result));

    if (!is_lvalue_reference_type(lhs_type)
            || is_const_qualified_type(reference_type_get_referenced_type(lhs_type)))
        return;

    // Classes have their own operators
    if (is_class_type(no_ref(lhs_type)))
        return;

    parameter_info_t parameters[2] =
    {
        { 
            .is_ellipsis = 0,
            .type_info = lhs_type,
            .nonadjusted_type_info = NULL
        },
        {
            .is_ellipsis = 0,
            .type_info = get_lvalue_reference_type(
                    get_const_qualified_type(
                        reference_type_get_referenced_type(lhs_type))),
            .nonadjusted_type_info = NULL
        }
    };

    type_t* function_type = get_new_function_type(lhs_type, parameters, 2);

    // Fill the minimum needed for this 'faked' function symbol
    (*result).entry[(*result).num_builtins].kind = SK_FUNCTION;
    (*result).entry[(*result).num_builtins].symbol_name = get_operator_function_name(operator);
    (*result).entry[(*result).num_builtins].entity_specs.is_builtin = 1;
    (*result).entry[(*result).num_builtins].type_information = function_type;
    (*result).entry[(*result).num_builtins].decl_context = decl_context;

    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Generated builtin '%s' for '%s'\n",
                print_declarator((*result).entry[(*result).num_builtins].type_information),
                (*result).entry[(*result).num_builtins].symbol_name);
    }

    // Add to the results and properly chain things
    (*result).entry_list = entry_list_add((*result).entry_list, &((*result).entry[(*result).num_builtins]));
    (*result).num_builtins++;
}

static type_t* compute_bin_nonoperator_assig_only_arithmetic_type(AST expr, AST lhs, AST rhs, AST operator,
        decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        // Enums are not considered for overloading in operator= because any overload
        // of operator= must be member, so classes are only eligible here for overload.
        requires_overload = is_class_type(no_ref(lhs_type)) || is_class_type(no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        C_LANGUAGE()
        {
            if (!expression_is_lvalue(lhs))
                return NULL;
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(lhs_type)
                    || is_const_qualified_type(no_ref(lhs_type)))
                return NULL;

            // If the rhs is an unresolved overloaded type we have
            // to solve it here using lhs_type
            if (is_unresolved_overloaded_type(no_ref(rhs_type)))
            {
                scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(rhs_type);
                scope_entry_t* solved_function = address_of_overloaded_function(
                        unresolved_set,
                        unresolved_overloaded_type_get_explicit_template_arguments(rhs_type),
                        no_ref(lhs_type), 
                        decl_context,
                        ASTFileName(lhs),
                        ASTLine(lhs));
                entry_list_free(unresolved_set);

                if (solved_function == NULL)
                {
                    return NULL;
                }

#if 0
                if (function_type_is_incomplete_independent(solved_function->type_information))
                {
                    instantiate_template_function(solved_function, decl_context,
                            ASTFileName(rhs), ASTLine(rhs));
                }
#endif

                // Update the types everywhere
                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_type(solved_function->type_information));
                }
                else
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_to_member_type(
                                solved_function->type_information,
                                named_type_get_symbol(solved_function->entity_specs.class_type)));
                }
                expression_set_type(rhs, rhs_type);
            }
        }

        standard_conversion_t sc;
        if (!standard_conversion_between_types(&sc, no_ref(rhs_type), no_ref(lhs_type)))
        {
            return NULL;
        }

        expression_set_type(expr, lvalue_ref(lhs_type));
        expression_set_is_lvalue(expr, 1);
        return lhs_type;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_nonop_assign_builtin(
            lhs_type, &builtin_set, operator, decl_context);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // We need to do overload
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static type_t* compute_bin_operator_assig_only_arithmetic_type(AST expr, AST lhs, AST rhs, AST operator,
        decl_context_t decl_context)
{
    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    RETURN_IF_ERROR_OR_DEPENDENT_2(lhs_type, rhs_type, expr);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        C_LANGUAGE()
        {
            if (!expression_is_lvalue(lhs))
                return NULL;
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(lhs_type)
                    || is_const_qualified_type(no_ref(lhs_type)))
                return NULL;

            // If the rhs is an unresolved overloaded type we have
            // to solve it here using lhs_type
            if (is_unresolved_overloaded_type(no_ref(rhs_type)))
            {
                scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(rhs_type);
                scope_entry_t* solved_function = address_of_overloaded_function(
                        unresolved_set,
                        unresolved_overloaded_type_get_explicit_template_arguments(rhs_type),
                        no_ref(lhs_type), 
                        decl_context,
                        ASTFileName(lhs),
                        ASTLine(lhs));
                entry_list_free(unresolved_set);

                if (solved_function == NULL)
                {
                    return NULL;
                }
                
#if 0
                if (function_type_is_incomplete_independent(solved_function->type_information))
                {
                    instantiate_template_function(solved_function, decl_context,
                            ASTFileName(rhs), ASTLine(rhs));
                }
#endif

                // Update the types everywhere
                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_type(solved_function->type_information));
                }
                else
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_to_member_type(
                                solved_function->type_information,
                                named_type_get_symbol(solved_function->entity_specs.class_type)));
                }
            }
        }

        standard_conversion_t sc;
        if (!standard_conversion_between_types(&sc, no_ref(rhs_type), no_ref(lhs_type)))
        {
            return NULL;
        }

        expression_set_type(expr, lvalue_ref(lhs_type));
        expression_set_is_lvalue(expr, 1);
        return lhs_type;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            // Note that the left is not removed its reference type
            lhs_type, no_ref(rhs_type), 
            &builtin_set,
            decl_context, operator, 
            operator_bin_assign_only_arithmetic_pred,
            operator_bin_assign_only_arithmetic_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // We need to do overload
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            expr, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

static type_t* compute_bin_operator_mod_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MOD_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_shl_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LEFT_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_shr_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_RIGHT_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_bitwise_and_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_AND_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_bitwise_or_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_OR_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_bitwise_xor_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_XOR_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_integral_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_mul_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MUL_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_arithmetic_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ASSIGNMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_nonoperator_assig_only_arithmetic_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_div_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIV_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_only_arithmetic_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_add_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_arithmetic_or_pointer_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

static type_t* compute_bin_operator_sub_assig_type(AST expr, AST lhs, AST rhs,
        decl_context_t decl_context, const_value_t** val)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_SUB_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (val != NULL)
        *val = NULL;

    return compute_bin_operator_assig_arithmetic_or_pointer_type(expr, lhs, rhs, 
            operation_tree, decl_context);
}

char operator_unary_derref_pred(type_t* op_type)
{
    if (is_pointer_type(op_type))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_derref_result(type_t** op_type)
{
    if (is_pointer_type(*op_type))
    {
        return get_lvalue_reference_type(pointer_type_get_pointee_type(*op_type));
    }
    return NULL;
}

static type_t* compute_operator_derreference_type(AST expression,
        AST op, decl_context_t decl_context, const_value_t** val)
{
    char requires_overload = 0;

    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    if (val != NULL)
        *val = NULL;

    CXX_LANGUAGE()
    {
        if (operand_is_class_or_enum(no_ref(op_type)))
            requires_overload = 1;
    }

    if (!requires_overload)
    {
        if (is_pointer_type(no_ref(op_type))
                && !is_function_type(pointer_type_get_pointee_type(no_ref(op_type))))
        {
            expression_set_type(expression, lvalue_ref(pointer_type_get_pointee_type(no_ref(op_type))));
            expression_set_is_lvalue(expression, 1);
        }
        else if (is_pointer_type(no_ref(op_type))
                && is_function_type(pointer_type_get_pointee_type(no_ref(op_type))))
        {
            // Bypass derreference of pointer to function type
            expression_set_type(expression, lvalue_ref(op_type));
            expression_set_is_lvalue(expression, 1);
        }
        else if (is_array_type(no_ref(op_type)))
        {
            expression_set_type(expression, lvalue_ref(array_type_get_element_type(no_ref(op_type))));
            expression_set_is_lvalue(expression, 1);
        }
        else if (is_function_type(no_ref(op_type)))
        {
            // Create a pointer type
            expression_set_type(expression, lvalue_ref(get_pointer_type(no_ref(op_type))));
            expression_set_is_lvalue(expression, 1);
        }
        else
            return NULL;

        return expression_get_type(expression);
    }

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MULT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            no_ref(op_type),
            &builtin_set,
            decl_context, operation_tree,
            operator_unary_derref_pred,
            operator_unary_derref_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_unary_operator_type(operation_tree,
            expression, op, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    return result;
}

char operator_unary_plus_pred(type_t* op_type)
{
    if (is_pointer_type(op_type))
    {
        return 1;
    }
    else if (is_arithmetic_type(op_type))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_plus_result(type_t** op_type)
{
    if (is_pointer_type(*op_type))
    {
        return *op_type;
    }
    else if (is_arithmetic_type(*op_type))
    {
        if (is_promoteable_integral_type(*op_type))
        {
            *op_type = promote_integral_type(*op_type);
        }

        return *op_type;
    }
    return NULL;
}

static type_t* compute_operator_plus_type(AST expression,
        AST op, decl_context_t decl_context, const_value_t** val)
{
    char requires_overload = 0;

    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    CXX_LANGUAGE()
    {
        if (operand_is_class_or_enum(no_ref(op_type)))
            requires_overload = 1;
    }

    if (!requires_overload)
    {
        if (is_pointer_type(no_ref(op_type)))
        {
            // Bypass
            expression_set_type(expression, no_ref(op_type));
            expression_set_is_lvalue(expression, 0);
        }
        else if (is_arithmetic_type(no_ref(op_type)))
        {
            if (is_promoteable_integral_type(no_ref(op_type)))
            {
                op_type = promote_integral_type(no_ref(op_type));
            }

            expression_set_type(expression, no_ref(op_type));
            expression_set_is_lvalue(expression, 0);

            if (val != NULL
                    && is_integral_type(no_ref(op_type))
                    && expression_is_constant(op))
            {
                *val = const_value_plus(expression_get_constant(op));
            }
        }
        else
            return NULL;

        return expression_get_type(expression);
    }

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            no_ref(op_type),
            &builtin_set,
            decl_context, operation_tree,
            operator_unary_plus_pred,
            operator_unary_plus_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator;

    type_t* result = compute_user_defined_unary_operator_type(operation_tree,
            expression, op, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && is_integral_type(no_ref(result))
            && is_integral_type(no_ref(op_type))
            && expression_is_constant(op))
    {
        *val = const_value_plus(expression_get_constant(op));
    }

    return result;
}

char operator_unary_minus_pred(type_t* op_type)
{
    if (is_arithmetic_type(op_type))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_minus_result(type_t** op_type)
{
    if (is_arithmetic_type(*op_type))
    {
        if (is_promoteable_integral_type(*op_type))
        {
            *op_type = promote_integral_type(*op_type);
        }

        return *op_type;
    }
    return NULL;
}

static type_t* compute_operator_minus_type(AST expression,
        AST op, decl_context_t decl_context, const_value_t** val)
{
    char requires_overload = 0;

    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    CXX_LANGUAGE()
    {
        if (operand_is_class_or_enum(no_ref(op_type)))
            requires_overload = 1;
    }

    if (!requires_overload)
    {
        if (is_arithmetic_type(no_ref(op_type)))
        {
            if (is_promoteable_integral_type(no_ref(op_type)))
            {
                op_type = promote_integral_type(no_ref(op_type));
            }

            expression_set_type(expression, no_ref(op_type));
            expression_set_is_lvalue(expression, 0);

            if (val != NULL
                    && is_integral_type(no_ref(op_type))
                    && expression_is_constant(op))
            {
                *val = const_value_neg(expression_get_constant(op));
            }
        }
        else
            return NULL;

        return expression_get_type(expression);
    }

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MINUS_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            no_ref(op_type),
            &builtin_set,
            decl_context, operation_tree,
            operator_unary_minus_pred,
            operator_unary_minus_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_unary_operator_type(operation_tree,
            expression, op, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && result != NULL
            && selected_operator != NULL
            && selected_operator->entity_specs.is_builtin
            && (is_integral_type(no_ref(op_type)) || is_enum_type(no_ref(op_type)))
            && expression_is_constant(op))
    {
        *val = const_value_neg(expression_get_constant(op));
    }

    return result;
}

char operator_unary_complement_pred(type_t* op_type)
{
    if (is_integral_type(op_type))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_complement_result(type_t** op_type)
{
    if (is_integral_type(*op_type))
    {
        if (is_promoteable_integral_type(*op_type))
        {
            *op_type = promote_integral_type(*op_type);
        }

        return *op_type;
    }
    return NULL;
}

static type_t* compute_operator_complement_type(AST expression,
        AST op, decl_context_t decl_context, const_value_t** val)
{
    char requires_overload = 0;

    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    CXX_LANGUAGE()
    {
        if (operand_is_class_or_enum(no_ref(op_type)))
            requires_overload = 1;
    }

    if (!requires_overload)
    {
        if (is_integral_type(no_ref(op_type)))
        {
            if (is_promoteable_integral_type(no_ref(op_type)))
            {
                op_type = promote_integral_type(no_ref(op_type));
            }

            if (val != NULL
                    && expression_is_constant(op))
            {
                *val = const_value_bitnot(expression_get_constant(op));
            }

            expression_set_type(expression, no_ref(op_type));
            expression_set_is_lvalue(expression, 0);
        }
        else
            return NULL;

        return expression_get_type(expression);
    }

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_NEG_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
        build_unary_builtin_operators(
            no_ref(op_type),
            &builtin_set,
            decl_context, operation_tree,
            operator_unary_complement_pred,
            operator_unary_complement_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_unary_operator_type(operation_tree,
            expression, op, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && result != NULL
            && selected_operator != NULL
            && selected_operator->entity_specs.is_builtin
            && (is_integral_type(no_ref(op_type)) || is_enum_type(no_ref(op_type)))
            && expression_is_constant(op))
    {
        *val = const_value_bitnot(expression_get_constant(op));
    }

    return result;
}

char operator_unary_not_pred(type_t* op_type)
{
    standard_conversion_t to_bool;

    if (standard_conversion_between_types(&to_bool,
                op_type, get_bool_type()))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_not_result(type_t** op_type)
{
    // We want the function to be 'bool operator!(bool)' not 'bool
    // operator!(L)' with L something that can be converted to bool
    *op_type = get_bool_type();

    return *op_type;
}

static type_t* compute_operator_not_type(AST expression,
        AST op, decl_context_t decl_context, const_value_t** val)
{
    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        // Try to simplify unresolved overloads
        if (is_unresolved_overloaded_type(no_ref(op_type)))
        {
            scope_entry_t* function = unresolved_overloaded_type_simplify(no_ref(op_type),
                    decl_context, ASTLine(op), ASTFileName(op));
            if (function != NULL)
            {
                op_type = get_pointer_type(function->type_information);
            }
        }

        if (operand_is_class_or_enum(no_ref(op_type)))
            requires_overload = 1;
    }

    if (!requires_overload)
    {
        standard_conversion_t to_bool;

        if (standard_conversion_between_types(&to_bool,
                    op_type, get_bool_type()))
        {
            C_LANGUAGE()
            {
                expression_set_type(expression, get_signed_int_type());
            }
            CXX_LANGUAGE()
            {
                expression_set_type(expression, get_bool_type());
            }

            if (val != NULL
                    && expression_is_constant(op))
            {
                *val = const_value_not(expression_get_constant(op));
            }

            expression_set_is_lvalue(expression, 0);
        }
        else 
            return NULL;

        return expression_get_type(expression);
    }

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_NOT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            no_ref(op_type),
            &builtin_set,
            decl_context, operation_tree,
            operator_unary_not_pred,
            operator_unary_not_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_unary_operator_type(operation_tree,
            expression, op, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (val != NULL
            && result != NULL
            && selected_operator != NULL
            && selected_operator->entity_specs.is_builtin
            && (is_integral_type(no_ref(op_type)) || is_enum_type(no_ref(op_type)))
            && expression_is_constant(op))
    {
        *val = const_value_not(expression_get_constant(op));
    }

    return result;
}

static type_t* compute_operator_reference_type(AST expression, 
        AST op, decl_context_t decl_context, const_value_t** val)
{
    // This is an easy operator since it can't be overloaded
    // but requires checking the syntax in C++
    type_t* op_type = expression_get_type(op);

    RETURN_IF_ERROR_OR_DEPENDENT_1(op_type, expression);

    *val = NULL;

    CXX_LANGUAGE()
    {
        // If it is a nested name it might be a pointer to member type
        if (ASTType(ASTSon0(expression)) == AST_QUALIFIED_ID)
        {
            if (is_unresolved_overloaded_type(op_type))
            {
                // This is an easy case
                expression_set_type(expression, op_type);
                return expression_get_type(expression);
            }
            else
            {
                // This must be a data member
                scope_entry_list_t* entry_list = query_id_expression_flags(decl_context, ASTSon0(expression), 
                        DF_DEPENDENT_TYPENAME);

                if (entry_list == NULL)
                {
                    return NULL;
                }

                if (entry_list_size(entry_list) > 1)
                {
                    // This can't happen with data members
                    entry_list_free(entry_list);
                    return NULL;
                }

                scope_entry_t* entry = entry_list_head(entry_list);
                entry_list_free(entry_list);

                if (entry->kind == SK_DEPENDENT_ENTITY)
                {
                    expression_set_dependent(expression);
                    return expression_get_type(expression);
                }

                if (!entry->entity_specs.is_member
                        || entry->entity_specs.is_static)
                {
                    expression_set_type(expression, get_pointer_type(entry->type_information));
                }
                else
                {
                    expression_set_type(expression, get_pointer_to_member_type(entry->type_information,
                                named_type_get_symbol(entry->entity_specs.class_type)));
                }

                return expression_get_type(expression);
            }
        }

        if (is_dependent_expr_type(op_type))
        {
            expression_set_dependent(expression);
            return expression_get_type(expression);
        }

        if (is_unresolved_overloaded_type(op_type))
        {
            // Bypass the type for overload addresses
            expression_set_type(expression, op_type);
            return expression_get_type(expression);
        }
    }
    
    // We only can get the address of lvalues
    if (!expression_is_lvalue(op))
    {
        return NULL;
    }

    type_t* ptr_type = get_pointer_type(no_ref(op_type));

    expression_set_type(expression, ptr_type);
    expression_set_is_lvalue(expression, 0);

    return ptr_type;
}

static struct bin_operator_funct_type_t binary_expression_fun[] =
{
    [AST_ADD_OP]                = OPERATOR_FUNCT_INIT(compute_bin_operator_add_type),
    [AST_MULT_OP]               = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_type),
    [AST_DIV_OP]                = OPERATOR_FUNCT_INIT(compute_bin_operator_div_type),
    [AST_MOD_OP]                = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_type),
    [AST_MINUS_OP]              = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_type),
    [AST_SHL_OP]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_type),
    [AST_SHR_OP]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_type),
    [AST_LOWER_THAN]            = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_than_type),
    [AST_GREATER_THAN]          = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_than_type),
    [AST_GREATER_OR_EQUAL_THAN] = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_equal_type),
    [AST_LOWER_OR_EQUAL_THAN]   = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_equal_type),
    [AST_EQUAL_OP]              = OPERATOR_FUNCT_INIT(compute_bin_operator_equal_type),
    [AST_DIFFERENT_OP]          = OPERATOR_FUNCT_INIT(compute_bin_operator_different_type),
    [AST_BITWISE_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_type),
    [AST_BITWISE_XOR]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_type),
    [AST_BITWISE_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_type),
    [AST_LOGICAL_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_and_type),
    [AST_LOGICAL_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_or_type),

    [AST_ASSIGNMENT]            = OPERATOR_FUNCT_INIT(compute_bin_operator_assig_type),
    [AST_MUL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_assig_type),
    [AST_DIV_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_div_assig_type),
    [AST_ADD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_add_assig_type),
    [AST_SUB_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_assig_type),
    [AST_SHL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_assig_type),
    [AST_SHR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_assig_type),
    [AST_AND_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_assig_type),
    [AST_OR_ASSIGNMENT ]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_assig_type),
    [AST_XOR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_assig_type),
    [AST_MOD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_assig_type),
};

static struct unary_operator_funct_type_t unary_expression_fun[] =
{
    [AST_DERREFERENCE]          = OPERATOR_FUNCT_INIT(compute_operator_derreference_type),
    [AST_REFERENCE]             = OPERATOR_FUNCT_INIT(compute_operator_reference_type),
    [AST_PLUS_OP]               = OPERATOR_FUNCT_INIT(compute_operator_plus_type),
    [AST_NEG_OP]                = OPERATOR_FUNCT_INIT(compute_operator_minus_type),
    [AST_NOT_OP]                = OPERATOR_FUNCT_INIT(compute_operator_not_type),
    [AST_COMPLEMENT_OP]         = OPERATOR_FUNCT_INIT(compute_operator_complement_type),
};

static type_t* get_binary_op_type(AST expr, decl_context_t decl_context, const_value_t** val)
{
    return (binary_expression_fun[ASTType(expr)].func)(
            expr, ASTSon0(expr), ASTSon1(expr), 
            decl_context, val);
}

static type_t* get_unary_op_type(AST expr, decl_context_t decl_context, const_value_t** val)
{
    return (unary_expression_fun[ASTType(expr)].func)(
            expr, ASTSon0(expr), decl_context, val);
}

static char check_for_binary_expression(AST expression, decl_context_t decl_context, const_value_t** val)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    char lhs_check = check_for_expression(lhs, decl_context);
    char rhs_check = check_for_expression(rhs, decl_context);

    if (lhs_check
            && rhs_check)
    {
        type_t* bin_type = get_binary_op_type(expression, decl_context, val);

        if (bin_type == NULL
                && !checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: binary %s cannot be applied to operands '%s' (of type '%s') and '%s' (of type '%s')\n",
                    ast_location(expression),
                    get_operation_function_name(expression), 
                    prettyprint_in_buffer(lhs), print_type_str(expression_get_type(lhs), decl_context),
                    prettyprint_in_buffer(rhs), print_type_str(expression_get_type(rhs), decl_context));
        }

        return (bin_type != NULL);
    }

    return 0;
}

static char check_for_unary_expression(AST expression, decl_context_t decl_context, const_value_t** val)
{
    AST op = ASTSon0(expression);

    if (check_for_expression(op, decl_context))
    {
        type_t* unary_type = get_unary_op_type(expression, decl_context, val);

        if (unary_type == NULL
                && !checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: unary %s cannot be applied to operand '%s' (of type '%s')\n",
                    ast_location(expression),
                    get_operation_function_name(expression), 
                    prettyprint_in_buffer(op), print_type_str(expression_get_type(op), decl_context));
        }

        return (unary_type != NULL);
    }

    return 0;
}


static char compute_symbol_type(AST expr, decl_context_t decl_context, const_value_t** val)
{
    scope_entry_list_t* result = NULL;

    if (is_template_parameter_name(expr))
    {
        result = entry_list_new(lookup_template_parameter_name(decl_context, expr));
    }
    else
    {
        result = query_nested_name(decl_context, NULL, NULL, expr); 
    }

    char names_a_builtin = 0;
    const char *name = ASTText(expr);

    if (name != NULL
            && strncmp(name, builtin_prefix, strlen(builtin_prefix)) == 0)
    {
        names_a_builtin = 1;
    }

    if (result == NULL)
    {
        if (!names_a_builtin)
        {
            return 0;
        }
        else
        {
            // Do not know anything about this type so set to something that is
            // never considered an error (even in C)
            expression_set_type(expr, get_dependent_expr_type());
            return 1;
        }
    }

    scope_entry_t* entry = entry_list_head(result);

    if (entry->kind == SK_VARIABLE
            || entry->kind == SK_DEPENDENT_ENTITY
            || entry->kind == SK_ENUMERATOR
            || entry->kind == SK_FUNCTION
            // template function names
            || entry->kind == SK_TEMPLATE
            || entry->kind == SK_TEMPLATE_PARAMETER)
    {
        expression_set_symbol(expr, entry);

        if (entry->kind == SK_TEMPLATE)
        {
            type_t* primary_type = template_type_get_primary_type(entry->type_information);
            type_t* function_specialization = named_type_get_symbol(primary_type)->type_information;

            // Only template-function-names are allowed here
            if (!is_function_type(function_specialization))
            {
                entry_list_free(result);
                return 0;
            }
        }

        C_LANGUAGE()
        {
            if (entry->kind == SK_ENUMERATOR)
            {
                expression_set_type(expr, entry->type_information);
                expression_set_is_lvalue(expr, 0);

                if (val != NULL
                        && expression_is_constant(entry->expression_value))
                {
                    *val = expression_get_constant(entry->expression_value);
                }
            }
            else if (entry->kind == SK_VARIABLE
                    || entry->kind == SK_FUNCTION)
            {
                expression_set_type(expr, entry->type_information);
                expression_set_is_lvalue(expr, 1);

                if (val != NULL
                        && entry->kind == SK_VARIABLE
                        && !entry->entity_specs.is_parameter
                        && is_const_qualified_type(entry->type_information)
                        && entry->expression_value != NULL
                        && expression_is_constant(entry->expression_value))
                {
                    *val = expression_get_constant(entry->expression_value);
                }
            }
            else
            {
                internal_error("Invalid entry kind in an expression", 0);
            }
        }
        CXX_LANGUAGE()
        {
            if (entry->kind == SK_ENUMERATOR)
            {
                if (is_dependent_type(entry->type_information))
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "EXPRTYPE: Found '%s' at '%s' to be dependent\n",
                                prettyprint_in_buffer(expr), ast_location(expr));
                    }
                    expression_set_dependent(expr);
                }
                else
                {
                    expression_set_type(expr, entry->type_information);
                    expression_set_is_lvalue(expr, 0);

                    if (val != NULL
                            && expression_is_constant(entry->expression_value))
                    {
                        *val = expression_get_constant(entry->expression_value);
                    }
                }
                if (expression_is_value_dependent(entry->expression_value))
                {
                    expression_set_is_value_dependent(expr, 1);
                }
            }
            else if (entry->kind == SK_VARIABLE)
            {
                if (!is_dependent_type(entry->type_information))
                {
                    if (!entry->entity_specs.is_template_argument)
                    {
                        expression_set_type(expr, lvalue_ref(entry->type_information));
                        expression_set_is_lvalue(expr, 1);
                    }
                    else
                    {
                        expression_set_type(expr, entry->type_information);
                        expression_set_is_lvalue(expr, 0);
                    }

                    if (!entry->entity_specs.is_parameter
                            && (is_const_qualified_type(entry->type_information)
                                || entry->entity_specs.is_template_argument)
                            && entry->expression_value != NULL)
                    {
                        if (val != NULL
                                && expression_is_constant(entry->expression_value))
                            *val = expression_get_constant(entry->expression_value);

                        if (expression_is_value_dependent(entry->expression_value))
                            expression_set_is_value_dependent(expr, 1);
                    }
                }
                else
                {
                    DEBUG_CODE()
                    {
                        fprintf(stderr, "EXPRTYPE: Found '%s' at '%s' to be dependent\n",
                                prettyprint_in_buffer(expr), ast_location(expr));
                    }
                    expression_set_dependent(expr);
                }
            }
            else if (entry->kind == SK_FUNCTION)
            {
                expression_set_type(expr, get_unresolved_overloaded_type(result, /* template args */ NULL));
            }
            else if (entry->kind == SK_DEPENDENT_ENTITY)
            {
                expression_set_is_value_dependent(expr, 1);
                expression_set_dependent(expr);
            }
            else if (entry->kind == SK_TEMPLATE_PARAMETER)
            {
                // Mark this in the tree for further queries
                set_as_template_parameter_name(expr, entry);

                // Nontype template parameter
                // Mark it as value dependent
                expression_set_is_value_dependent(expr, 1);

                // Template parameters may have a dependent type

                if (!is_dependent_type(entry->type_information))
                {
                    expression_set_type(expr, entry->type_information);
                    // Although this expression has a non-dependent type
                    // we need to know it is value dependent
                    expression_set_is_value_dependent(expr, 1);
                }
                else
                {
                    expression_set_dependent(expr);
                }
            }
            else if (entry->kind == SK_TEMPLATE)
            {
                expression_set_type(expr, get_unresolved_overloaded_type(result, /* template_args*/ NULL));
            }
            else
            {
                internal_error("Invalid entry kind in an expression", 0);
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Symbol '%s' at '%s' has type '%s'\n",
                    prettyprint_in_buffer(expr), ast_location(expr),
                    print_declarator(expression_get_type(expr)));
        }

        entry_list_free(result);
        return 1;
    }
    else
    {
        entry_list_free(result);
        return 0;
    }
}

static char check_for_symbol(AST expr, decl_context_t decl_context, const_value_t** val)
{
    char result = compute_symbol_type(expr, decl_context, val);

    if (!result 
            && !checking_ambiguity())
    {
        fprintf(stderr, "%s: warning: symbol '%s' not found in current scope\n",
                ast_location(expr), ASTText(expr));
    }

    return result;
}


static char compute_qualified_id_type(AST expr, decl_context_t decl_context, const_value_t** val)
{
    // This function only can be called in C++ world
    AST global_scope = ASTSon0(expr);
    AST nested_name_spec = ASTSon1(expr);
    AST unqualified_object = ASTSon2(expr);

    scope_entry_list_t* result_list = query_nested_name_flags(decl_context, global_scope, nested_name_spec, 
            unqualified_object, DF_DEPENDENT_TYPENAME);

    if (result_list != NULL
            && entry_list_size(result_list) == 1)
    {
        scope_entry_t* entry = entry_list_head(result_list);
        if (entry->kind == SK_DEPENDENT_ENTITY)
        {
            expression_set_dependent(expr);
            entry_list_free(result_list);
            return 1;
        }
    }

    char dependent_template_arguments = 0;

    if (ASTType(unqualified_object) == AST_TEMPLATE_ID)
    {
        if (result_list != NULL)
        {
            type_t* solved = check_template_function(result_list, 
                    unqualified_object, decl_context, 
                    &dependent_template_arguments);
            entry_list_free(result_list);

            if (solved != NULL)
            {
                expression_set_type(expr, solved);
                // This is arguable since this is unlikely to go at the left of any
                // expression, but ok, lvalueness is such a mystic thing throughout the
                // standard.
                expression_set_is_lvalue(expr, 1);
            }
            else if (dependent_template_arguments)
            {
                expression_set_dependent(expr);
            }
            else
            {
                return 0;
            }
            return 1;
        }
        return 0;
    }
    else
    {
        if (result_list == NULL)
            return 0;

        scope_entry_t* entry = entry_list_head(result_list);
        if (entry->kind != SK_VARIABLE
                && entry->kind != SK_ENUMERATOR
                && entry->kind != SK_FUNCTION
                && entry->kind != SK_TEMPLATE)
        {
            entry_list_free(result_list);
            return 0;
        }

        expression_set_symbol(expr, entry);

        if (entry->kind == SK_VARIABLE)
        {
            if (!is_dependent_type(entry->type_information))
            {
                expression_set_type(expr, lvalue_ref(entry->type_information));
                expression_set_is_lvalue(expr, 1);
            }
            else
            {
                expression_set_dependent(expr);
            }

            if (!entry->entity_specs.is_parameter
                    && (is_const_qualified_type(entry->type_information)
                        || entry->entity_specs.is_template_argument)
                    && entry->expression_value != NULL)
            {
                if (val != NULL
                        && expression_is_constant(entry->expression_value))
                    *val = expression_get_constant(entry->expression_value);

                if (expression_is_value_dependent(entry->expression_value))
                    expression_set_is_value_dependent(expr, 1);
            }
        }
        else if (entry->kind == SK_ENUMERATOR)
        {
            expression_set_type(expr, entry->type_information);
            expression_set_is_lvalue(expr, 0);

            if (val != NULL
                    && expression_is_constant(entry->expression_value))
            {
                *val = expression_get_constant(entry->expression_value);
            }

            if (expression_is_value_dependent(entry->expression_value))
            {
                expression_set_is_value_dependent(expr, 1);
            }
        }
        else if (entry->kind == SK_FUNCTION)
        {
            expression_set_type(expr, get_unresolved_overloaded_type(result_list, /* template_args */ NULL));
        }
        else if (entry->kind == SK_TEMPLATE)
        {
            type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
            scope_entry_t* named_type = named_type_get_symbol(primary_named_type);

            if (named_type->kind != SK_FUNCTION)
            {
                entry_list_free(result_list);
                return 0;
            }

            expression_set_type(expr, get_unresolved_overloaded_type(result_list, /* template_args */ NULL));
        }

        entry_list_free(result_list);
        return 1;
    }
}

static char check_for_array_subscript_expr(AST expr, decl_context_t decl_context)
{
    if (!check_for_expression(ASTSon0(expr), decl_context))
    {
        return 0;
    }

    if (!check_for_expression(ASTSon1(expr), decl_context))
    {
        return 0;
    }

    AST subscripted_expr = ASTSon0(expr);
    AST subscript_expr = ASTSon1(expr);

    type_t* subscripted_type = expression_get_type(subscripted_expr);
    type_t* subscript_type = expression_get_type(subscript_expr);

    if (is_dependent_expr_type(subscripted_type))
    {
        expression_set_type(expr, get_dependent_expr_type());
        CXX_LANGUAGE()
        {
            expression_set_is_value_dependent(expr, 1);
        }
        return 1;
    }

    // Builtin cases
    if (is_array_type(no_ref(subscripted_type)))
    {
        expression_set_type(expr, lvalue_ref(array_type_get_element_type(no_ref(subscripted_type))));
        expression_set_is_lvalue(expr, 1);
        return 1;
    }
    else if (is_pointer_type(no_ref(subscripted_type)))
    {
        expression_set_type(expr, lvalue_ref(pointer_type_get_pointee_type(no_ref(subscripted_type))));
        expression_set_is_lvalue(expr, 1);
        return 1;
    }
    else
    {
        C_LANGUAGE()
        {
            fprintf(stderr, "%s: warning: expression '%s' is invalid since '%s' has type '%s'\n",
                    ast_location(expr),
                    prettyprint_in_buffer(expr),
                    prettyprint_in_buffer(subscripted_expr),
                    print_type_str(subscripted_type, decl_context));
            return 0;
        }
    }

    // Now we are in C++
    if (is_class_type(no_ref(subscripted_type)))
    {
        static AST operator_subscript_tree = NULL;
        if (operator_subscript_tree == NULL)
        {
            operator_subscript_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_SUBSCRIPT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }


        if (is_dependent_expr_type(subscript_type))
        {
            expression_set_type(expr, subscript_type);
            return 1;
        }
        else
        {
            scope_entry_list_t* operator_subscript_list = get_member_function_of_class_type(
                    no_ref(subscripted_type),
                    operator_subscript_tree,
                    decl_context);

            // Solve operator[]. It is always a member operator
            int num_arguments = 2;
            type_t* argument_types[2] = { subscripted_type, subscript_type };

            scope_entry_t* conversors[2] = {NULL, NULL};

            scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(operator_subscript_list,
                    /* builtins */ NULL, argument_types + 1, num_arguments - 1,
                    decl_context,
                    ASTFileName(expr), ASTLine(expr),
                    /* explicit_template_arguments */ NULL);
            entry_list_free(operator_subscript_list);

            candidate_t* candidate_set = NULL;
            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(overload_set);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        entry_list_iterator_current(it),
                        num_arguments,
                        argument_types);
            }
            entry_list_iterator_free(it);
            entry_list_free(overload_set);

            scope_entry_t *overloaded_call = solve_overload(candidate_set,
                    decl_context, ASTFileName(expr), ASTLine(expr), conversors);

            if (overloaded_call == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_message_overload_failed(decl_context, expr, candidate_set);
                }
                return 0;
            }

            ensure_not_deleted(decl_context, expr, overloaded_call);

            if (conversors[1] != NULL)
            {
                ensure_not_deleted(decl_context, expr, conversors[1]);

                ASTAttrSetValueType(subscript_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(subscript_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[1]));
            }

            ASTAttrSetValueType(expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(overloaded_call));

            expression_set_type(expr, function_type_get_return_type(overloaded_call->type_information));
            expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));
            return 1;
        }
    }

    fprintf(stderr, "%s: warning: in '%s' operator[] cannot be applied to '%s' of type '%s'\n",
            ast_location(expr),
            prettyprint_in_buffer(expr),
            prettyprint_in_buffer(subscripted_expr),
            print_type_str(subscripted_type, decl_context));

    return 0;
}

static char check_for_conversion_function_id_expression(AST expression, decl_context_t decl_context)
{
    scope_entry_list_t* entry_list = query_id_expression(decl_context, expression);

    if (entry_list == NULL)
    {
        return 0;
    }

    type_t* conversion_type = NULL;
    /* char* conversion_function_name = */ 
    get_conversion_function_name(decl_context, expression, &conversion_type);

    ERROR_CONDITION(conversion_type == NULL,
            "Conversion type was not computed", 0);

    if (is_dependent_type(conversion_type))
    {
        expression_set_dependent(expression);
        return 1;
    }

    char found = 0;
    scope_entry_t* found_entry = NULL;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it) && !found;
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        type_t* current_conversion_type 
            = function_type_get_return_type(entry->type_information);

        if (equivalent_types(current_conversion_type, conversion_type))
        {
            found = 1;
            found_entry = entry;
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(entry_list);

    if (!found)
    {
        return 0;
    }

    expression_set_type(expression, found_entry->type_information);
    expression_set_is_lvalue(expression, is_lvalue_reference_type(found_entry->type_information));
    return 1;
}

static char convert_in_conditional_expr(type_t* from_t1, type_t* to_t2, 
        char *is_ambiguous_conversion,
        decl_context_t decl_context)
{
    *is_ambiguous_conversion = 0;

    if (is_lvalue_reference_type(to_t2))
    {
    /*
     * If E2 is a lvalue, E1 can be converted to match E2 if E1 can be implicitly
     * converted to the type 'reference of T2'
     */
        standard_conversion_t result;
        if (standard_conversion_between_types(&result, from_t1, to_t2))
        {
            return 1;
        }
        return 0;
    }
    else
    {
        /* If E2 is an rvalue or if the conversion above cannot be done
         *
         * - If E1 and E2 have class type, and the underlying class types are the same
         *   or one is base class of the other E1 can be converted to match E2
         */
        if (is_class_type(no_ref(from_t1))
                && is_class_type(no_ref(to_t2))
                && class_type_is_base(no_ref(to_t2), no_ref(from_t1))
                && is_more_or_equal_cv_qualified_type(to_t2, from_t1))
        {
            return 1;
        }
        else if (is_class_type(no_ref(from_t1))
                || is_class_type(no_ref(to_t2)))
        {
            return type_can_be_implicitly_converted_to(from_t1, 
                    to_t2, 
                    decl_context,
                    is_ambiguous_conversion, /* conversor */ NULL);
        }
        else
        {
            return 0;
        }
    }
}

static char ternary_operator_property(type_t* t1, type_t* t2, type_t* t3)
{
    if (is_bool_type(no_ref(t1)))
    {
        if (is_arithmetic_type(no_ref(t2))
                    && is_arithmetic_type(no_ref(t3)))
        {
            return 1;
        }
        else if (is_pointer_type(no_ref(t2))
                && is_pointer_type(no_ref(t3))
                && equivalent_types(
                    get_unqualified_type(no_ref(t2)), 
                    get_unqualified_type(no_ref(t3))))
        {
            return 1;
        }
        else if (is_pointer_to_member_type(no_ref(t2))
                && is_pointer_to_member_type(no_ref(t3))
                && equivalent_types(
                    get_unqualified_type(no_ref(t2)), 
                    get_unqualified_type(no_ref(t3))))
        {
            return 1;
        }
    }

    return 0;
}

static type_t* ternary_operator_result(type_t** t1 UNUSED_PARAMETER, 
        type_t** t2, type_t** t3)
{
    if (is_arithmetic_type(no_ref(*t2))
            && is_arithmetic_type(no_ref(*t3)))
    {
        if (is_promoteable_integral_type(no_ref(*t2)))
            *t2 = promote_integral_type(no_ref(*t2));

        if (is_promoteable_integral_type(no_ref(*t3)))
            *t3 = promote_integral_type(no_ref(*t3));

        return usual_arithmetic_conversions(no_ref(*t2), no_ref(*t3));
    }
    else
    {
        return no_ref(*t2);
    }
}

static type_t* composite_pointer_to_member(type_t* p1, type_t* p2)
{
    if (equivalent_types(p1, p2))
        return p1;

    if (is_zero_type(p1))
        return p2;

    if (is_zero_type(p2))
        return p1;

    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(p1, &cv_qualif_1);
    advance_over_typedefs_with_cv_qualif(p2, &cv_qualif_2);

    cv_qualifier_t result_cv = cv_qualif_1 | cv_qualif_2;

    type_t* result = NULL;

    standard_conversion_t sc;
    // This is not exact
    if (standard_conversion_between_types(&sc, p1, p2))
    {
        result = p2;
    }
    else if (standard_conversion_between_types(&sc, p2, p1))
    {
        result = p1;
    }
    else
    {
        internal_error("Unreachable code", 0);
    }

    result = get_cv_qualified_type(result, result_cv);

    return result;
}

static type_t* composite_pointer(type_t* p1, type_t* p2)
{
    if (equivalent_types(p1, p2))
        return p1;

    if (is_zero_type(p1))
        return p2;

    if (is_zero_type(p2))
        return p1;

    cv_qualifier_t cv_qualif_1 = CV_NONE;
    cv_qualifier_t cv_qualif_2 = CV_NONE;

    advance_over_typedefs_with_cv_qualif(p1, &cv_qualif_1);
    advance_over_typedefs_with_cv_qualif(p2, &cv_qualif_2);

    cv_qualifier_t result_cv = cv_qualif_1 | cv_qualif_2;

    type_t* result = NULL;

    standard_conversion_t sc;
    if (is_void_pointer_type(p1) 
            || is_void_pointer_type(p2))
    {
        result = get_pointer_type(get_void_type());
    }
    // This is not exact
    else if (standard_conversion_between_types(&sc, p1, p2))
    {
        result = p2;
    }
    else if (standard_conversion_between_types(&sc, p2, p1))
    {
        result = p1;
    }
    else
    {
        internal_error("Unreachable code", 0);
    }

    result = get_cv_qualified_type(result, result_cv);

    return result;
}

static char check_for_conditional_expression_impl(AST expression, 
        AST first_op, AST second_op, AST third_op, decl_context_t decl_context)
{
    /*
     * This is more complex that it might seem at first ...
     */
    char result = check_for_expression(first_op, decl_context )
        && check_for_expression(second_op, decl_context )
        && check_for_expression(third_op, decl_context );

    if (!result)
        return 0;

    type_t* first_type = expression_get_type(first_op);

    type_t* second_type = expression_get_type(second_op);
    char second_is_lvalue = expression_is_lvalue(second_op);

    type_t* third_type = expression_get_type(third_op);
    char third_is_lvalue = expression_is_lvalue(third_op);

    if (first_type == NULL
            || second_type == NULL
            || third_type == NULL)
    {
        return 0;
    }

    if (is_dependent_expr_type(first_type)
            || is_dependent_expr_type(second_type)
            || is_dependent_expr_type(third_type))
    {
        expression_set_dependent(expression);
        return 1;
    }

    type_t* converted_type = NULL;
    C_LANGUAGE()
    {
        converted_type = get_signed_int_type();

        standard_conversion_t sc;
        if (!standard_conversion_between_types(&sc, first_type, converted_type))
        {
            return 0;
        }
    }

    // In C++ this might be an lvalue, but it is not in C
    expression_set_is_lvalue(expression, 0);

    CXX_LANGUAGE()
    {
        converted_type = get_bool_type();

        /*
         * C++ standard is a mess here but we will try to make it clear
         */
        if (is_void_type(no_ref(second_type)) 
                || is_void_type(no_ref(third_type)))
        {
            /*
             * If either the the second or the third operand is a void type
             */
            /*
             * All lvalue-conversions are applied here
             */
            type_t* operand_types[] = { second_type, third_type };

            int i;
            for (i = 0; i < 2; i++)
            {
                operand_types[i] = no_ref(operand_types[i]);

                if (is_array_type(operand_types[i]))
                {
                    operand_types[i] = get_pointer_type(array_type_get_element_type(operand_types[i]));
                }
                if (is_function_type(operand_types[i]))
                {
                    operand_types[i] = get_pointer_type(operand_types[i]);
                }
            }

            if ((is_throw_expr_type(operand_types[0]) 
                        || is_throw_expr_type(operand_types[1])) 
                    && !(is_throw_expr_type(operand_types[0]) 
                        && is_throw_expr_type(operand_types[1])))
            {
                /*
                 *  a) If any (but not both) is a throw expression (throw expressions
                 *     yield a type of void, a special one) the result is the type of the
                 *     other and is a rvalue
                 */
                if (is_throw_expr_type(operand_types[0]))
                {
                    expression_set_type(expression, operand_types[1]);
                }
                else
                {
                    expression_set_type(expression, operand_types[0]);
                }
            }
            else
            {
                /*
                 * b) Both the second and third operands have type void the result is of type void
                 * and is a rvalue
                 */
                expression_set_type(expression, get_void_type());
            }
            
            // Nothing else has to be done for 'void' types
            return 1;
        }

        if (!equivalent_types(no_ref(second_type), no_ref(third_type))
                && (is_class_type(no_ref(second_type)) 
                    || is_class_type(no_ref(third_type))))
        {
            /*
             * otherwise, if the second or the third are different types and either is a class type
             * an attempt to convert one to the other is performed.
             */
            char second_to_third_is_ambig = 0;
            char second_to_third = 
                convert_in_conditional_expr(second_type, 
                        third_type, 
                        &second_to_third_is_ambig,
                        decl_context);

            char third_to_second_is_ambig = 0;
            char third_to_second = 
                convert_in_conditional_expr(third_type, 
                        second_type, 
                        &third_to_second_is_ambig,
                        decl_context);

            if (second_to_third 
                    && third_to_second)
            {
                return 0;
            }

            if (second_to_third)
            {
                if (second_to_third_is_ambig)
                {
                    return 0;
                }

                third_type = second_type;
            }

            if (third_to_second)
            {
                if (third_to_second_is_ambig)
                {
                    return 0;
                }

                second_type = third_type;
            }
        }


        /*
         * If the second and third operand do not have the same type 
         * we rely in overload mechanism
         */
        if (!equivalent_types(no_ref(second_type), no_ref(third_type))
                && ((is_class_type(no_ref(second_type))
                        || is_class_type(no_ref(third_type)))
                    || (is_enum_type(no_ref(second_type))
                        || is_enum_type(no_ref(third_type)))
                    )
                )
        {
            builtin_operators_set_t builtin_set;

            build_ternary_builtin_operators(get_bool_type(),
                    no_ref(second_type),
                    no_ref(third_type),
                    &builtin_set,
                    decl_context,
                    "operator ?",
                    ternary_operator_property,
                    ternary_operator_result
                    );

            scope_entry_list_t* builtins = 
                get_entry_list_from_builtin_operator_set(&builtin_set);

            int num_arguments = 3;

            type_t* argument_types[3] = {
                get_bool_type(),
                second_type,
                third_type,
            };

            candidate_t* candidate_set = NULL;
            scope_entry_list_iterator_t *it = NULL;
            for (it = entry_list_iterator_begin(builtins);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        entry_list_iterator_current(it),
                        num_arguments,
                        argument_types);
            }
            entry_list_iterator_free(it);
            entry_list_free(builtins);

            scope_entry_t* conversors[3] = { NULL, NULL, NULL };
            scope_entry_t *overloaded_call = solve_overload(candidate_set,
                    decl_context, ASTFileName(expression), ASTLine(expression), 
                    conversors);

            if (overloaded_call == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_message_overload_failed(decl_context, expression, candidate_set);
                }
                return 0;
            }

            ensure_not_deleted(decl_context, expression, overloaded_call);

            // Note that for AST_GCC_CONDITIONAL_EXPRESSION first_op == second_op
            // but writing it twice in the loop should be harmless
            AST ops[] = { first_op, second_op, third_op };

            int k;
            for (k = 0; k < 3; k++)
            {
                if (conversors[k] != NULL)
                {
                    ensure_not_deleted(decl_context, expression, conversors[k]);

                    ASTAttrSetValueType(ops[k], 
                            LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(ops[k],
                            LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[k]));
                }
            }

            // Get the converted types and use them instead of the originals
            second_type = function_type_get_parameter_type_num(overloaded_call->type_information, 1);
            third_type = function_type_get_parameter_type_num(overloaded_call->type_information, 2);
        }

        /*
         * If both types are the same and lvalue the resulting expression is a lvalue
         */
        if (second_is_lvalue 
                && third_is_lvalue
                && equivalent_types(no_ref(second_type), no_ref(third_type)))
        {
            expression_set_is_lvalue(expression, 1);
        }
    }

    /*
     * Now apply lvalue conversions to both types
     */
    type_t* operand_types[] = { second_type, third_type };

    int i;
    for (i = 0; i < 2; i++)
    {
        operand_types[i] = no_ref(operand_types[i]);

        if (is_array_type(operand_types[i]))
        {
            operand_types[i] = get_pointer_type(array_type_get_element_type(operand_types[i]));
        }
        else if (is_function_type(operand_types[i]))
        {
            operand_types[i] = get_pointer_type(operand_types[i]);
        }

        // Drop top level qualifiers since they do not play any role now
        operand_types[i] = get_unqualified_type(operand_types[i]);
    }

    char is_pointer_and_zero = 
        (is_pointer_type(operand_types[0]) && is_zero_type(operand_types[1]))
        || (is_pointer_type(operand_types[1]) && is_zero_type(operand_types[0]));

    char is_pointer_to_member_and_zero = 
        (is_pointer_to_member_type(operand_types[0]) && is_zero_type(operand_types[1]))
        || (is_pointer_to_member_type(operand_types[1]) && is_zero_type(operand_types[0]));

    type_t* final_type = NULL;

    if (equivalent_types(operand_types[0], operand_types[1]))
    {
        final_type = operand_types[1];
    }
    else if (both_operands_are_arithmetic(operand_types[0], operand_types[1]))
    {
        final_type = usual_arithmetic_conversions(operand_types[0], operand_types[1]);
    }
    else if (both_operands_are_vector_types(operand_types[0], operand_types[1]))
    {
        final_type = operand_types[0];
    }
    else if ((is_pointer_type(operand_types[0]) && is_pointer_type(operand_types[1]))
            || is_pointer_and_zero)
    {
        final_type = composite_pointer(operand_types[0], operand_types[1]);
    }
    else if ((is_pointer_to_member_type(operand_types[0])
            && is_pointer_to_member_type(operand_types[1]))
            || is_pointer_to_member_and_zero)
    {
        final_type = composite_pointer_to_member(operand_types[0], operand_types[1]);
    }
    else
    {
        return 0;
    }

    CXX_LANGUAGE()
    {
        // It could be that the final type is a reference
        // or the whole expression was already a lvalue. 
        // Keep in synch lvalueness with reference types
        if (expression_is_lvalue(expression) ||
                is_lvalue_reference_type(final_type))
        {
            final_type = lvalue_ref(final_type);
            expression_set_is_lvalue(expression, 1);
        }
    }

    expression_set_type(expression, final_type);

    return 1;
}


static char check_for_conditional_expression(AST expression, decl_context_t decl_context)
{
    AST first_op = ASTSon0(expression);
    AST second_op = NULL, third_op = NULL;

    if (ASTType(expression) == AST_CONDITIONAL_EXPRESSION)
    {
        second_op = ASTSon1(expression);
        third_op = ASTSon2(expression);
    }
    else if (ASTType(expression) == AST_GCC_CONDITIONAL_EXPRESSION)
    {
        second_op = first_op;
        third_op = ASTSon1(expression);
    }
    else
    {
        internal_error("Invalid node '%s'\n", ast_print_node_type(ASTType(expression)));
    }

    char result = check_for_conditional_expression_impl(expression, 
            first_op, second_op, third_op, decl_context);

    if (!result
            && !checking_ambiguity())
    {
        fprintf(stderr, "%s: warning: ternary operand '?' cannot be applied to first operand '%s' (of type '%s'), "
                 "second operand '%s' (of type '%s') and third operand '%s' (of type '%s')\n",
                 ast_location(expression),
                 prettyprint_in_buffer(first_op), print_type_str(expression_get_type(first_op), decl_context),
                 prettyprint_in_buffer(second_op), print_type_str(expression_get_type(second_op), decl_context),
                 prettyprint_in_buffer(third_op), print_type_str(expression_get_type(third_op), decl_context));
    }
    else
    {
        if (expression_is_constant(first_op)
                && expression_is_constant(second_op)
                && expression_is_constant(third_op))
        {
            if (const_value_is_nonzero(expression_get_constant(first_op)))
            {
                expression_set_constant(expression, expression_get_constant(second_op));
            }
            else
            {
                expression_set_constant(expression, expression_get_constant(third_op));
            }
        }
    }

    return result;
}

static char check_for_new_expression(AST new_expr, decl_context_t decl_context)
{
    AST global_op = ASTSon0(new_expr);
    AST new_placement = ASTSon1(new_expr);
    AST new_type_id = ASTSon2(new_expr);
    AST new_initializer = ASTSon3(new_expr);

    ASTAttrSetValueType(new_expr, LANG_IS_NEW_EXPRESSION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(new_expr, LANG_NEW_TYPEID, tl_type_t, tl_ast(new_type_id));
    ASTAttrSetValueType(new_expr, LANG_NEW_INITIALIZER, tl_type_t,
            new_initializer != NULL ? tl_ast(ASTSon0(new_initializer)) : tl_ast(NULL));

    if (!check_for_type_id_tree(new_type_id, decl_context))
        return 0;

    if (new_placement != NULL)
    {
        AST expression_list = ASTSon0(new_placement);

        if (!check_for_expression_list(expression_list, decl_context))
            return 0;
    }

    AST type_specifier_seq = ASTSon0(new_type_id);
    AST new_declarator = ASTSon1(new_type_id);

    type_t* dummy_type = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &dummy_type, decl_context);

    // template-names are not allowed here since they do not name a type
    if (is_named_type(dummy_type)
            && named_type_get_symbol(dummy_type)->kind == SK_TEMPLATE)
    {
        // This is not valid here
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: invalid '%s' type-name, it names a template-name\n",
                    ast_location(type_specifier_seq),
                    prettyprint_in_buffer(type_specifier_seq));
        }
        return 0;
    }

    type_t* declarator_type = NULL;
    compute_declarator_type(new_declarator, &gather_info, dummy_type, &declarator_type, decl_context);

    if (new_initializer != NULL)
    {
        AST expression_list = ASTSon0(new_initializer);

        if (expression_list != NULL)
        {
            if (!check_for_expression_list(expression_list, decl_context))
            {
                return 0;
            }
        }
    }

    char new_array = 0;

    if (is_array_type(declarator_type))
    {
        new_array = 1;
        declarator_type = get_pointer_type(array_type_get_element_type(declarator_type));
    }
    else
    {
        declarator_type = get_pointer_type(declarator_type);
    }

    // Solve 'operator new' being invoked
    if (!is_dependent_type(declarator_type))
    {
        scope_entry_t* conversors[MAX_ARGUMENTS];
        memset(conversors, 0, sizeof(conversors));

        type_t* arguments[MAX_ARGUMENTS];
        memset(arguments, 0, sizeof(arguments));

        // At least the size_t parameter (+1 because we may need an implicit)
        int num_arguments = 2;

        // Note: arguments[0] will be left as NULL since 'operator new' is
        // always static if it is a member function
        arguments[1] = get_size_t_type();

        char has_dependent_placement_args = 0;

        int i = 2; 
        if (new_placement != NULL)
        {
            AST expression_list = ASTSon0(new_placement);
            AST iter;

            for_each_element(expression_list, iter)
            {
                AST expr = ASTSon1(iter);

                arguments[i] = expression_get_type(expr);

                if (is_dependent_expr_type(arguments[i]))
                {
                    has_dependent_placement_args = 1;
                }

                i++;
                num_arguments++;
            }
        }

        if (!has_dependent_placement_args)
        {
            decl_context_t op_new_context = decl_context;

            if (is_pointer_to_class_type(declarator_type)
                    && global_op == NULL)
            {
                type_t* class_type = pointer_type_get_pointee_type(declarator_type);

                // Instantiate the class if needed
                if (is_named_class_type(class_type)
                        && class_type_is_incomplete_independent(get_actual_class_type(class_type)))
                {
                    scope_entry_t* symbol = named_type_get_symbol(class_type);
                    instantiate_template_class(symbol, decl_context, ASTFileName(new_expr), ASTLine(new_expr));
                }

                op_new_context = class_type_get_inner_context(
                        get_actual_class_type(pointer_type_get_pointee_type(declarator_type)));
            }
            else 
            {
                // Use the global scope
                op_new_context.current_scope = op_new_context.global_scope;
            }

            static AST operation_new_tree = NULL;
            if (operation_new_tree == NULL)
            {
                operation_new_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                        ASTLeaf(AST_NEW_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
            }

            static AST operation_new_array_tree = NULL;
            if (operation_new_array_tree == NULL)
            {
                operation_new_array_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                        ASTLeaf(AST_NEW_ARRAY_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
            }

            AST called_operation_new_tree = operation_new_tree;

            if (new_array)
            {
                called_operation_new_tree = operation_new_array_tree;
            }

            scope_entry_list_t *operator_new_list = query_id_expression(op_new_context, called_operation_new_tree);

            if (operator_new_list == NULL)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: no suitable '%s' has been found in the scope\n",
                            ast_location(new_expr),
                            prettyprint_in_buffer(called_operation_new_tree));
                }
                return 0;
            }

            candidate_t* candidate_set = NULL;
            scope_entry_list_iterator_t *it = NULL;
            for (it = entry_list_iterator_begin(operator_new_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);
                if (entry->entity_specs.is_member)
                {
                    candidate_set = add_to_candidate_set(candidate_set,
                            entry,
                            num_arguments,
                            arguments);
                }
                else
                {
                    candidate_set = add_to_candidate_set(candidate_set,
                            entry,
                            num_arguments - 1,
                            arguments + 1);
                }
            }
            entry_list_iterator_free(it);

            scope_entry_t* chosen_operator_new = solve_overload(candidate_set, 
                    decl_context, ASTFileName(new_expr), ASTLine(new_expr), 
                    conversors);

            if (chosen_operator_new == NULL)
            {
                if (!checking_ambiguity())
                {
                    const char* argument_call = uniquestr("");

                    argument_call = strappend(argument_call, "operator new");
                    if (new_array)
                    {
                        argument_call = strappend(argument_call, "[]");
                    }
                    argument_call = strappend(argument_call, "(");

                    for (i = 1; i < num_arguments; i++)
                    {
                        argument_call = strappend(argument_call, print_type_str(arguments[i], decl_context));
                        if ((i + 1) < num_arguments)
                        {
                            argument_call = strappend(argument_call, ", ");
                        }
                    }
                    argument_call = strappend(argument_call, ")");

                    fprintf(stderr, "%s: warning: no suitable '%s' found for new-expression '%s'\n",
                            ast_location(new_expr),
                            argument_call,
                            prettyprint_in_buffer(new_expr));
                    fprintf(stderr, "%s: note: candidates are:\n", ast_location(new_expr));
                    for (it = entry_list_iterator_begin(operator_new_list);
                            !entry_list_iterator_end(it);
                            entry_list_iterator_next(it))
                    {
                        scope_entry_t* candidate_op = entry_list_iterator_current(it);
                        fprintf(stderr, "%s: note:    %s\n", ast_location(new_expr),
                                print_decl_type_str(candidate_op->type_information, decl_context, 
                                    get_qualified_symbol_name(candidate_op, decl_context)));
                    }
                    entry_list_iterator_free(it);
                    entry_list_free(operator_new_list);
                }
                return 0;
            }

            ensure_not_deleted(decl_context, new_expr, chosen_operator_new);

            ASTAttrSetValueType(new_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(new_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(chosen_operator_new));

            // Store conversions
            if (new_placement != NULL)
            {
                AST expression_list = ASTSon0(new_placement);
                AST iter;

                // Ignore the size_t (and the implicit too)
                i = 2;
                if (expression_list != NULL)
                {
                    for_each_element(expression_list, iter)
                    {
                        AST current_expr = ASTSon1(iter);

                        if (conversors[i] != NULL)
                        {
                            ensure_not_deleted(decl_context, new_expr, conversors[i]);

                            ASTAttrSetValueType(current_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                            ASTAttrSetValueType(current_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[i]));
                        }

                        i++;
                    }
                }
            }


            if (is_pointer_to_class_type(declarator_type))
            {
                type_t* class_type = pointer_type_get_pointee_type(declarator_type);

                memset(conversors, 0, sizeof(conversors));
                memset(arguments, 0, sizeof(arguments));

                num_arguments = 0;
                char has_constructor_dep_args = 0;

                if (new_initializer != NULL)
                {
                    AST expression_list = ASTSon0(new_initializer);

                    if (expression_list != NULL)
                    {
                        AST iter;
                        for_each_element(expression_list, iter)
                        {
                            AST expr = ASTSon1(iter);

                            arguments[num_arguments] = expression_get_type(expr);

                            if (is_dependent_expr_type(arguments[num_arguments]))
                            {
                                has_constructor_dep_args = 1;
                            }

                            num_arguments++;
                        }
                    }
                }

                if (!has_constructor_dep_args)
                {
                    scope_entry_list_t* candidates = NULL;
                    scope_entry_t* chosen_constructor = solve_constructor(class_type,
                            arguments, num_arguments,
                            /* is_explicit */ 1,
                            decl_context,
                            ASTFileName(new_expr), ASTLine(new_expr),
                            conversors,
                            &candidates);
                    entry_list_free(candidates);

                    if (chosen_constructor == NULL)
                    {
                        if (!checking_ambiguity())
                        {
                            fprintf(stderr, "%s: warning: no suitable constructor of type '%s' "
                                    "found for new-initializer '%s'\n",
                                    ast_location(new_expr),
                                    print_decl_type_str(class_type, decl_context, ""),
                                    prettyprint_in_buffer(new_initializer));
                        }

                        return 0;
                    }

                    ensure_not_deleted(decl_context, new_expr, chosen_constructor);

                    // FIXME - This does not belong to the expression!
                    ASTAttrSetValueType(new_type_id, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(new_type_id, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(chosen_constructor));

                    // Store conversions
                    if (new_initializer != NULL)
                    {
                        AST expression_list = ASTSon0(new_initializer);
                        AST iter;

                        i = 0;
                        if (expression_list != NULL)
                        {
                            for_each_element(expression_list, iter)
                            {
                                AST current_expr = ASTSon1(iter);

                                if (conversors[i] != NULL)
                                {
                                    ensure_not_deleted(decl_context, new_expr, conversors[i]);

                                    ASTAttrSetValueType(current_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                                    ASTAttrSetValueType(current_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[i]));
                                }

                                i++;
                            }
                        }
                    }
                }
                else 
                {
                    // Constructor args are dependent
                    expression_set_dependent(new_expr);
                    expression_set_is_lvalue(new_expr, 0);
                    return 1;
                }
            }

            expression_set_type(new_expr, declarator_type);
            expression_set_is_lvalue(new_expr, 0);
            return 1;
        }
        else 
        {
            // Placement expression is dependent
            expression_set_dependent(new_expr);
            expression_set_is_lvalue(new_expr, 0);
            return 1;
        }

    }
    else
    {
        // The new type is dependent
        expression_set_dependent(new_expr);
        expression_set_is_lvalue(new_expr, 0);
        return 1;
    }

    // Invalid
    return 0;
}

static char check_for_new_type_id_expr(AST new_expr, decl_context_t decl_context)
{
    return check_for_new_expression(new_expr, decl_context );
}

static char is_deallocation_function(scope_entry_t* entry)
{
    if (entry->kind != SK_FUNCTION)
        return 0;

    type_t* function_type = entry->type_information;

    if (function_type_get_num_parameters(function_type) == 0
             || function_type_get_num_parameters(function_type) > 2)
        return 0;

    // Only deallocation for classes may have 2 parameters
    if (function_type_get_num_parameters(function_type) == 2
            && !entry->entity_specs.is_member)
        return 0;

    type_t* void_pointer = function_type_get_parameter_type_num(function_type, 0);

    if (!equivalent_types(void_pointer, get_pointer_type(get_void_type())))
        return 0;

    if (function_type_get_num_parameters(function_type) == 2)
    {
        type_t* size_t_type = function_type_get_parameter_type_num(function_type, 1);
        if (!equivalent_types(size_t_type, get_size_t_type()))
            return 0;
    }

    if (is_template_specialized_type(function_type))
        return 0;

    return 1;
}

static char check_for_delete_expression(AST expression, decl_context_t decl_context)
{
    char is_array_delete = 0;
    if (ASTType(expression) == AST_DELETE_ARRAY_EXPR)
    {
        is_array_delete = 1;
    }

    ASTAttrSetValueType(expression, LANG_IS_DELETE_EXPRESSION, tl_type_t, tl_bool(1));

    // This is always a value, never a type
    AST global_op = ASTSon0(expression);
    AST deleted_expression = ASTSon1(expression);

    char result = check_for_expression(deleted_expression, decl_context );

    if (!result)
        return 0;

    type_t* deleted_expr_type = no_ref(expression_get_type(deleted_expression));

    if (!is_dependent_expr_type(deleted_expr_type)
            && (!is_pointer_type(deleted_expr_type)
                || is_pointer_to_function_type(deleted_expr_type)))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: expression '%s' of type '%s' cannot be deleted\n",
                    ast_location(deleted_expression),
                    prettyprint_in_buffer(deleted_expression),
                    print_decl_type_str(deleted_expr_type, decl_context, ""));
        }
        return 0;
    }

    if (!is_dependent_expr_type(deleted_expr_type))
    {
        // Lookup the destructor if this is a class
        if (is_pointer_to_class_type(deleted_expr_type))
        {
            type_t* class_type 
                = get_actual_class_type(pointer_type_get_pointee_type(deleted_expr_type));

            scope_entry_t* destructor = class_type_get_destructor(class_type);

            // PODs may not have any destructor or have it trivial
            if (destructor != NULL
                    && !destructor->entity_specs.is_trivial)
            {
                ASTAttrSetValueType(deleted_expression, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(deleted_expression, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(destructor));
            }
        }
        
        // Now lookup the deallocation function
        static AST operation_delete_tree = NULL;
        if (operation_delete_tree == NULL)
        {
            operation_delete_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_DELETE_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }

        static AST operation_delete_array_tree = NULL;
        if (operation_delete_array_tree == NULL)
        {
            operation_delete_array_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_DELETE_ARRAY_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }

        AST operation_delete_name = operation_delete_tree;
        if (is_array_delete)
        {
            operation_delete_name = operation_delete_array_tree;
        }

        decl_context_t op_delete_context = decl_context;

        if (global_op == NULL
                && is_pointer_to_class_type(deleted_expr_type))
        {
            type_t* class_type = pointer_type_get_pointee_type(deleted_expr_type);

            if (class_type_is_incomplete_independent(get_actual_class_type(class_type)))
            {
                scope_entry_t* entry = named_type_get_symbol(class_type);
                instantiate_template_class(entry, decl_context,
                        ASTFileName(expression), ASTLine(expression));
            }

            if (is_complete_type(class_type))
            {
                op_delete_context = class_type_get_inner_context(
                        get_actual_class_type(pointer_type_get_pointee_type(deleted_expr_type)));
            }
            else
            {
                running_error("%s: error: cannot delete an incomplete class type\n",
                        ast_location(expression));
            }
        }
        else
        {
            // Use the global scope
            op_delete_context.current_scope 
                = op_delete_context.global_scope;
        }

        scope_entry_list_t *operator_delete_list = query_id_expression(op_delete_context, operation_delete_name);

        if (operator_delete_list == NULL)
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: no suitable '%s' has been found in the scope\n",
                        ast_location(expression),
                        prettyprint_in_buffer(operation_delete_name));
            }
            return 0;
        }

        // If more than one, select the one with only one parameter
        scope_entry_t* chosen = NULL;

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_delete_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* operator_delete = entry_list_iterator_current(it);

            if (is_deallocation_function(operator_delete))
            {
                if (chosen == NULL
                        || (function_type_get_num_parameters(chosen->type_information) > 
                            function_type_get_num_parameters(operator_delete->type_information)))
                {
                    chosen = operator_delete;
                }
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(operator_delete_list);

        if (chosen == NULL)
        {
            fprintf(stderr, "%s: warning: no suitable '%s' valid for deallocation of '%s' has been found in the scope\n",
                    ast_location(expression),
                    prettyprint_in_buffer(operation_delete_name),
                    print_decl_type_str(pointer_type_get_pointee_type(deleted_expr_type), 
                        decl_context, ""));
            return 0;
        }
        else
        {
            ASTAttrSetValueType(expression, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(expression, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(chosen));
        }
    }

    expression_set_type(expression, get_void_type());
    expression_set_is_lvalue(expression, 0);
    return 1;
}

static char check_for_explicit_type_conversion_common(type_t* type_info, 
        AST expr, AST expression_list, decl_context_t decl_context)
{
    type_t* argument_types[MAX_ARGUMENTS];
    memset(argument_types, 0, sizeof(argument_types));
    int num_arguments = 0;

    scope_entry_t* conversors[MAX_ARGUMENTS];
    memset(conversors, 0, sizeof(conversors));

    char has_dependent_arguments = 0;
    char has_value_dependent_arguments = 0;

    ASTAttrSetValueType(expr, LANG_IS_EXPLICIT_TYPE_CONVERSION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(expr, LANG_EXPLICIT_TYPE_CONVERSION_ARGS, tl_type_t, tl_ast(expression_list));

    if (expression_list != NULL)
    {
        if (!check_for_expression_list(expression_list, decl_context))
            return 0;

        AST iter;
        for_each_element(expression_list, iter)
        {
            AST current_expression = ASTSon1(iter);

            if (check_for_expression(current_expression, decl_context))
            {
                argument_types[num_arguments] = expression_get_type(current_expression);

                if (is_dependent_expr_type(argument_types[num_arguments]))
                {
                    has_dependent_arguments = 1;
                }

                if (expression_is_value_dependent(current_expression))
                {
                    has_value_dependent_arguments = 1;
                }
            }
            else
            {
                return 0;
            }

            num_arguments++;
        }
    }

    if (is_dependent_type(type_info)
            || has_dependent_arguments)
    {
        expression_set_dependent(expr);
    }
    else
    {
        if (is_class_type(type_info))
        {
            scope_entry_list_t* candidates = NULL;
            scope_entry_t* constructor = 
                solve_constructor(type_info,
                        argument_types,
                        num_arguments,
                        /* is_explicit */ 1,
                        decl_context,
                        ASTFileName(expr), ASTLine(expr),
                        conversors,
                        &candidates);
            entry_list_free(candidates);

            if (constructor == NULL)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: cannot cast to type '%s' in '%s'\n",
                            ast_location(expr),
                            print_decl_type_str(type_info, decl_context, ""),
                            prettyprint_in_buffer(expr));
                }
                return 0;
            }

            ensure_not_deleted(decl_context, expr, constructor);

            int k = 0;
            if (expression_list != NULL)
            {
                AST iter;
                for_each_element(expression_list, iter)
                {
                    AST expression = ASTSon1(expression_list);

                    if (conversors[k] != NULL)
                    {
                        ensure_not_deleted(decl_context, expr, conversors[k]);

                        ASTAttrSetValueType(expression, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(expression, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[k]));
                    }

                    k++;
                }
            }

            ASTAttrSetValueType(expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(constructor));
        }

        expression_set_type(expr, type_info);
        expression_set_is_lvalue(expr, 0);

        if (num_arguments == 1
                && expression_is_constant(ASTSon1(expression_list))
                && is_integral_type(type_info))
        {
            expression_set_constant(
                    expr,
                    const_value_cast_to_bytes(
                        expression_get_constant(ASTSon1(expression_list)),
                            type_get_size(type_info),
                            /* sign */ is_signed_integral_type(type_info)));
        }

        // Everything might have a non dependent type but it might carry a
        // dependent value
        expression_set_is_value_dependent(expr, has_value_dependent_arguments);
    }

    return 1;
}

static char check_for_explicit_typename_type_conversion(AST expr, decl_context_t decl_context)
{
    AST id_expression = ASTSon0(expr);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression);

    if (entry_list == NULL)
        return 0;

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (entry->kind != SK_TYPEDEF
            && entry->kind != SK_ENUM
            && entry->kind != SK_CLASS
            && entry->kind != SK_DEPENDENT_ENTITY
            && entry->kind != SK_TEMPLATE_TYPE_PARAMETER)
    {
        return 0;
    }

    AST expr_list = ASTSon1(expr);

    return check_for_explicit_type_conversion_common(get_user_defined_type(entry), expr, expr_list, decl_context);
}

static char check_for_explicit_type_conversion(AST expr, decl_context_t decl_context)
{
    // An explicit type conversion is of the form
    //
    //   T ( e );
    //
    // T has to be a valid typename
    char result = 0;
    AST simple_type_spec = ASTSon0(expr);

    type_t* type_info = NULL;
    result = check_for_simple_type_spec(simple_type_spec, decl_context, &type_info);
    if (!result)
    {
        return 0;
    }
    else
    {
        AST expression_list = ASTSon1(expr);
        return check_for_explicit_type_conversion_common(type_info, expr, expression_list, decl_context);
    }
}

static char check_for_function_arguments(AST arguments, decl_context_t decl_context, int *num_arguments)
{
    *num_arguments = 0;
    if (arguments != NULL)
    {
        if (ASTType(arguments) == AST_AMBIGUITY)
        {
            solve_ambiguous_expression_list(arguments, decl_context);
        }

        AST list = arguments;
        AST iter;

        for_each_element(list, iter)
        {
            AST parameter_expr = ASTSon1(iter);

            ASTAttrSetValueType(parameter_expr, LANG_IS_EXPRESSION_COMPONENT, tl_type_t, tl_bool(1));

            if (!check_for_expression(parameter_expr, decl_context))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: When checking function call, argument %d '%s' could not be checked\n",
                            (*num_arguments), prettyprint_in_buffer(parameter_expr));
                }
                return 0;
            }
            (*num_arguments)++;
        }

        ASTAttrSetValueType(arguments, LANG_IS_EXPRESSION_COMPONENT, tl_type_t, tl_bool(1));
    }

    return 1;
}

static char check_for_koenig_expression(AST called_expression, AST arguments, decl_context_t decl_context)
{
    // First try to do a normal lookup
    scope_entry_list_t* entry_list = query_id_expression(decl_context, called_expression);

    enum cxx_symbol_kind filter_function_names[] = 
    {
        SK_VARIABLE,
        SK_FUNCTION,
        SK_TEMPLATE, 
        SK_DEPENDENT_ENTITY
    };

    scope_entry_list_t* old_entry_list = entry_list;
    entry_list = filter_symbol_kind_set(old_entry_list,
            STATIC_ARRAY_LENGTH(filter_function_names), filter_function_names);
    entry_list_free(old_entry_list);

    char still_requires_koenig = 1;

    if (entry_list != NULL)
    {
        // If no member is found we still have to perform member

        char invalid = 0;

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it) && !invalid;
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            type_t* type = no_ref(advance_over_typedefs(entry->type_information));
            if (entry->kind != SK_FUNCTION
                    && (entry->kind != SK_VARIABLE
                        || (!is_class_type(type)
                            && !is_pointer_to_function_type(type)
                            && !is_dependent_type(type)))
                    && (entry->kind != SK_TEMPLATE
                        || !is_function_type(
                            named_type_get_symbol(template_type_get_primary_type(type))
                            ->type_information))
                    && (entry->kind != SK_DEPENDENT_ENTITY))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: Symbol '%s' with type '%s' cannot be called\n",
                            entry->symbol_name,
                            entry->type_information != NULL ? print_declarator(entry->type_information)
                            : " <no type> ");
                }
                invalid = 1;
            }
            else
            {
                // It can be a dependent entity because of a using of an undefined base
                if (entry->kind == SK_DEPENDENT_ENTITY
                        || entry->entity_specs.is_member
                        || (entry->kind == SK_VARIABLE
                            && (is_class_type(type)
                                || is_pointer_to_function_type(type)
                                || is_dependent_type(type))))
                {
                    still_requires_koenig = 0;
                }
            }
        }
        entry_list_iterator_free(it);

        // This cannot be called at all
        if (invalid)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Trying to call something not callable (it is not a function, template function or object)\n");
            }
            return 0;
        }

        if (!still_requires_koenig)
        {
            // No koenig needed
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Not Koenig will be performed since we found something that "
                        "is member or pointer to function type or dependent entity\n");
            }
            return check_for_expression(called_expression, decl_context);
        }
    }

    // Build types of arguments
    type_t* argument_types[MAX_ARGUMENTS];
    int num_arguments = 0;

    memset(argument_types, 0, sizeof(argument_types));

    if (arguments != NULL)
    {
        AST iter;
        for_each_element(arguments, iter)
        {
            AST argument_tree = ASTSon1(iter);

            type_t* argument_type = expression_get_type(argument_tree);

            if (is_dependent_expr_type(argument_type))
            {
                // Everything turned out to be dependent
                expression_set_dependent(called_expression);
                return 1;
            }
            else if (is_unresolved_overloaded_type(argument_type))
            {
                // If possible, simplify it
                scope_entry_t* entry =
                    unresolved_overloaded_type_simplify(argument_type, decl_context, ASTLine(argument_tree), ASTFileName(argument_tree));
                if (entry != NULL)
                {
                    if (!entry->entity_specs.is_member
                            || entry->entity_specs.is_static)
                    {
                        argument_type = get_lvalue_reference_type(entry->type_information);
                    }
                    else
                    {
                        argument_type = get_pointer_to_member_type(entry->type_information,
                                named_type_get_symbol(entry->entity_specs.class_type));
                    }
                }
            }

            ERROR_CONDITION(num_arguments >= MAX_ARGUMENTS, "Too many arguments", 0);

            argument_types[num_arguments] = argument_type;
            num_arguments++;
        }
    }

    entry_list_free(entry_list);
    entry_list = koenig_lookup(
            num_arguments,
            argument_types,
            decl_context,
            called_expression);

    old_entry_list = entry_list;
    entry_list = filter_symbol_kind_set(old_entry_list,
            STATIC_ARRAY_LENGTH(filter_function_names), filter_function_names);
    entry_list_free(old_entry_list);

    // Filter the list again
    if (entry_list != NULL)
    {
        // If no member is found we still have to perform member
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            type_t* type = no_ref(advance_over_typedefs(entry->type_information));
            if (entry->kind != SK_FUNCTION
                    && (entry->kind != SK_VARIABLE
                        || (!is_class_type(type)
                            && !is_pointer_to_function_type(type)
                            && !is_dependent_type(type)))
                    && (entry->kind != SK_TEMPLATE
                        || !is_function_type(
                            named_type_get_symbol(template_type_get_primary_type(type))
                            ->type_information)))
            {
                // This can't be called!
                DEBUG_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: Trying to call something not callable (it is not a function, template function or object)\n");
                }
                return 0;
            }
        }
        entry_list_iterator_free(it);
    }

    // Set these attributes
    ASTAttrSetValueType(called_expression, LANG_IS_ID_EXPRESSION, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(called_expression, LANG_IS_UNQUALIFIED_ID, tl_type_t, tl_bool(1));
    ASTAttrSetValueType(called_expression, LANG_UNQUALIFIED_ID, tl_type_t, tl_ast(called_expression));

    if (entry_list != NULL)
    {
        expression_set_type(called_expression, get_unresolved_overloaded_type(entry_list, 
                /* explicit template arguments */ NULL));
        return 1;
    }

    return 0;
}

static char any_is_member_function(scope_entry_list_t* candidates)
{
    char is_member = 0;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(candidates);
            !entry_list_iterator_end(it) && !is_member;
            entry_list_iterator_next(it))
    {
        is_member |= entry_list_iterator_current(it)->entity_specs.is_member;
    }
    entry_list_iterator_free(it);

    return is_member;
}

/*
 * This function performs the dirty job when computing the type of a functional call.
 * Note that in this code we already know that this sentence is a feasible type
 */
static char check_for_functional_expression(AST whole_function_call, AST called_expression, 
        AST arguments, decl_context_t decl_context, char might_require_koenig)
{
    // 1. If function arguments did not yield a valid expression ignore them
    int num_explicit_arguments = 0;
    if (!check_for_function_arguments(arguments, decl_context, &num_explicit_arguments))
    {
        return 0;
    }

    // 2. Ignore all redundant parentheses, this will simplify the left part of
    //    the call
    AST advanced_called_expression = advance_expression_nest(called_expression);

    C_LANGUAGE()
    {
        // 2.1. C is easy. Just check the symbol exists, enclosing routine will check
        //      it is a function type or pointer type
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Checking simple function call '%s' at '%s'\n", 
                    prettyprint_in_buffer(whole_function_call),
                    ast_location(whole_function_call));
        }
        if (!check_for_expression(called_expression, decl_context))
        {
            if (ASTType(advanced_called_expression) == AST_SYMBOL)
            {
                scope_entry_list_t *entry_list = query_id_expression(decl_context, 
                        advanced_called_expression);

                const char *name = ASTText(advanced_called_expression);
                // Maybe it named a type
                if (entry_list == NULL)
                {
                    expression_set_type(called_expression, get_nonproto_function_type(get_signed_int_type(),
                                num_explicit_arguments));
                    if (!checking_ambiguity())
                    {
                        fprintf(stderr, "%s: warning: function '%s' has not been declared, assuming it to be like '%s'\n", 
                                ast_location(called_expression),
                                prettyprint_in_buffer(called_expression),
                                print_decl_type_str(expression_get_type(called_expression), decl_context, name));
                    }
                }
                entry_list_free(entry_list);
            }
            else
                return 0;
        }

        if (is_computed_function_type(expression_get_type(called_expression)))
        {
            computed_function_type_t compute_type_function = 
                computed_function_type_get_computing_function(expression_get_type(called_expression));

            scope_entry_list_t *entry_list = query_id_expression(decl_context, 
                    advanced_called_expression);

            scope_entry_t *entry = entry_list_head(entry_list);
            entry_list_free(entry_list);

            type_t** argument_list = NULL;
            int num_arguments_tmp = 0;

            // Create the argument array
            AST iter;
            for_each_element(arguments, iter)
            {
                AST current_arg = ASTSon1(iter);
                P_LIST_ADD(argument_list, num_arguments_tmp, expression_get_type(current_arg));
            }

            scope_entry_t* solved_function = compute_type_function(entry, argument_list, num_arguments_tmp);

            if (solved_function != NULL)
            {
                expression_set_type(called_expression, solved_function->type_information);
            }
            else
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: no match found for computed function type '%s'\n",
                            ast_location(called_expression), 
                            prettyprint_in_buffer(advanced_called_expression));
                }
                return 0;
            }
        }
        else if (is_dependent_expr_type(expression_get_type(called_expression)))
        {
            // A word on this: C does not have dependent expressions. But when
            // calling a __builtin_XXX which is unknown it turns that we need
            // to shut up the compiler typecheck. Dependent expressions are not
            // silently ignored so we use them, even in C.  We overwrite the
            // dependent expression type with a nonprotoized function type
            // which will not be checked anyway

            // Why we are not doing this in called_expression? Well, we do not
            // actually know there whether it will be a call or not (it will be
            // a plain symbol check). We actually know that the callee is an
            // unknown builtin when checking a function call

            // Side effect: If the user writes (__builtin_foo + 1); this will
            // be silently ignored as well
            expression_set_type(called_expression, get_nonproto_function_type(get_signed_int_type(),
                        num_explicit_arguments));
        }
        else if (!is_function_type(expression_get_type(called_expression))
                && !is_pointer_to_function_type(expression_get_type(called_expression)))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Expression is not callable\n");
            }
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: cannot call '%s' since its type '%s' is neither a function or a pointer to function\n",
                        ast_location(called_expression),
                        prettyprint_in_buffer(called_expression),
                        print_type_str(expression_get_type(called_expression), decl_context));
            }
            return 0;
        }

        // Do not allow invalid types from now
        ERROR_CONDITION(!is_function_type(expression_get_type(called_expression))
                && !is_pointer_to_function_type(expression_get_type(called_expression)),
                "Invalid type for function call %s\n", print_declarator(expression_get_type(called_expression)));

        // Check arguments, at the moment just check the number of arguments. 
        // Maybe we will be checking the types themselves one day
        type_t* proper_function_type = expression_get_type(called_expression);
        if (is_pointer_to_function_type(proper_function_type))
        {
            proper_function_type = pointer_type_get_pointee_type(proper_function_type);
        }

        int max_args_to_check = num_explicit_arguments;
        if (!function_type_get_has_ellipsis(proper_function_type))
        {
            if (num_explicit_arguments != function_type_get_num_parameters(proper_function_type))
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: call with %d arguments for a function with %d parameters\n",
                            ast_location(whole_function_call),
                            num_explicit_arguments,
                            function_type_get_num_parameters(proper_function_type));
                }
                if (CURRENT_CONFIGURATION->strict_typecheck)
                    return 0;

                if (function_type_get_num_parameters(proper_function_type) < max_args_to_check)
                    max_args_to_check = function_type_get_num_parameters(proper_function_type);
            }
        }
        else
        {
            int min_arguments = function_type_get_num_parameters(proper_function_type) - 1;
            max_args_to_check = min_arguments;

            if (num_explicit_arguments < min_arguments)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: call with %d arguments for a function with at least %d parameters\n",
                            ast_location(whole_function_call),
                            num_explicit_arguments,
                            min_arguments);
                }
                if (CURRENT_CONFIGURATION->strict_typecheck)
                    return 0;
            }
        }

        int i = 0;

        if (arguments != NULL)
        {
            AST iter;
            for_each_element(arguments, iter)
            {
                // Ellipsis is not to be checked
                if (i >= max_args_to_check)
                    break;

                AST arg = ASTSon1(iter);

                type_t* arg_type = expression_get_type(arg);
                type_t* param_type = function_type_get_parameter_type_num(proper_function_type, i);

                standard_conversion_t result;
                if (!standard_conversion_between_types(&result, arg_type, param_type))
                {
                    if (!checking_ambiguity())
                    {
                        fprintf(stderr, "%s: warning: argument %d of type '%s' cannot be "
                                "converted to type '%s' of parameter\n",
                                ast_location(arg),
                                i,
                                print_type_str(arg_type, decl_context),
                                print_type_str(param_type, decl_context));
                    }
                    if (CURRENT_CONFIGURATION->strict_typecheck)
                        return 0;
                }

                i++;
            }
        }

        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Function call seems fine in C semantics\n");
        }
        
        return 1;
    }

    // C++ only from now
    char valid_expr = 0;
    if (might_require_koenig)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Performing expression check of '%s' at '%s' using argument dependent lookup (Koenig lookup)\n",
                    prettyprint_in_buffer(called_expression),
                    ast_location(called_expression));
        }
        // 3. a) If this call was eligible for Koenig lookup, perform the lookup of the called entity here
        valid_expr = check_for_koenig_expression(called_expression, arguments, decl_context);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Performing expression check of '%s' at '%s' by regular means\n",
                    prettyprint_in_buffer(called_expression),
                    ast_location(called_expression));
        }
        // 3. b) If the call was not eligible for Koenig, just check normally
        valid_expr = check_for_expression(called_expression, decl_context);
    }

    if (!valid_expr)
    {
        // 4. If the called expression could not be checked return
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Called expression '%s' at '%s' could not be checked (koenig = %s)\n",
                    prettyprint_in_buffer(called_expression),
                    ast_location(called_expression),
                    might_require_koenig ? "yes" : "no");
        }
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: called entity '%s' is not valid\n", 
                    ast_location(called_expression), 
                    prettyprint_in_buffer(called_expression));
        }
        return 0;
    }
    else
    {
        // 5. We are given a type possibly function, pointer to function, template (!), object, reference to object
        //    other types can't be 'called'
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Called expression '%s' has type '%s'\n",
                    prettyprint_in_buffer(called_expression), 
                    print_declarator(expression_get_type(called_expression)));
        }
    }

    if (!is_unresolved_overloaded_type(expression_get_type(called_expression))
            && !is_class_type(no_ref(expression_get_type(called_expression))))
    {
        // Nothing else must be done
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Fortunately we do not have to resolve any overload in the call '%s' at '%s'\n",
                    prettyprint_in_buffer(called_expression),
                    ast_location(called_expression));
        }
        return 1;
    }

    // Build types of arguments
    type_t* argument_types[MAX_ARGUMENTS];
    int num_arguments = 0;

    memset(argument_types, 0, sizeof(argument_types));

    // Leave room for the implicit one. We will fill it later
    num_arguments++;

    if (arguments != NULL)
    {
        AST iter;
        for_each_element(arguments, iter)
        {
            AST argument_tree = ASTSon1(iter);

            type_t* argument_type = expression_get_type(argument_tree);

            if (is_dependent_expr_type(argument_type))
            {
                expression_set_dependent(called_expression);
                return 1;
            }

            if (is_unresolved_overloaded_type(argument_type))
            {
                // If possible, simplify it
                scope_entry_t* entry =
                    unresolved_overloaded_type_simplify(argument_type, decl_context, ASTLine(argument_tree), ASTFileName(argument_tree));
                if (entry != NULL)
                {
                    if (!entry->entity_specs.is_member
                            || entry->entity_specs.is_static)
                    {
                        argument_type = get_lvalue_reference_type(entry->type_information);
                    }
                    else
                    {
                        argument_type = get_pointer_to_member_type(entry->type_information,
                                named_type_get_symbol(entry->entity_specs.class_type));
                    }
                }
            }

            ERROR_CONDITION(num_arguments >= MAX_ARGUMENTS, "Too many arguments", 0);

            argument_types[num_arguments] = argument_type;
            num_arguments++;
        }
    }

    scope_entry_list_t *candidates = NULL;
    if (is_unresolved_overloaded_type(expression_get_type(called_expression)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Call '%s' at '%s' yields an unresolved function type so we have to solve it\n",
                    prettyprint_in_buffer(called_expression),
                    ast_location(called_expression));
        }

        template_argument_list_t* explicit_template_arguments = 
            unresolved_overloaded_type_get_explicit_template_arguments(expression_get_type(called_expression));
        
        // These might include template_types that we have to "unfold" properly
        scope_entry_list_t* first_candidates = unresolved_overloaded_type_get_overload_set(expression_get_type(called_expression));
        candidates = unfold_and_mix_candidate_functions(first_candidates,
                /* builtins */ NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                ASTFileName(whole_function_call), ASTLine(whole_function_call),
                explicit_template_arguments);
        entry_list_free(first_candidates);

        // Now check the form of the call in order to fill the implicit argument type
        if (ASTType(called_expression) == AST_CLASS_MEMBER_ACCESS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: This is a call of the form 'a.b' (in fact it is '%s') adding '%s' "
                        "as the implicit argument type\n", 
                        prettyprint_in_buffer(called_expression),
                        prettyprint_in_buffer(ASTSon0(called_expression)));
            }
            type_t* class_type = expression_get_type(ASTSon0(called_expression));

            if (is_dependent_type(class_type))
            {
                // Nothing else to do this is a dependent call
                entry_list_free(candidates);
                expression_set_dependent(called_expression);
                return 1;
            }
            else
            {
                argument_types[0] = class_type;
            }
        }
        else if (ASTType(called_expression) == AST_POINTER_CLASS_MEMBER_ACCESS)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: This is a call of the form 'a->b' (in fact it is '%s') adding '*(%s)' "
                        "as the implicit argument type\n", 
                        prettyprint_in_buffer(called_expression),
                        prettyprint_in_buffer(ASTSon0(called_expression)));
            }
            type_t* class_type = expression_get_type(ASTSon0(called_expression));

            if (!is_pointer_to_class_type(no_ref(class_type)))
            {
                entry_list_free(candidates);
                return 0;
            }

            class_type = pointer_type_get_pointee_type(no_ref(class_type));

            if (is_dependent_type(class_type))
            {
                // Nothing else to do this is a dependent call
                entry_list_free(candidates);
                expression_set_dependent(called_expression);
                return 1;
            }
            else
            {
                argument_types[0] = class_type;
            }
        }
        else if (any_is_member_function(candidates))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: This is a call of the form 'f' (in fact it is '%s') adding 'this' "
                        "as the implicit argument type because we are in class-scope\n", 
                        prettyprint_in_buffer(called_expression));
            }

            scope_entry_list_t* this_symbol_list 
                = query_unqualified_name_str(decl_context, "this");

            if (this_symbol_list != NULL)
            {
                scope_entry_t* this_symbol = entry_list_head(this_symbol_list);

                if (is_dependent_type(this_symbol->type_information))
                {
                    entry_list_free(candidates);
                    expression_set_dependent(called_expression);
                    return 1;
                }
                else
                {
                    type_t* class_type = this_symbol->type_information;
                    class_type = pointer_type_get_pointee_type(class_type);
                    argument_types[0] = class_type;
                }
            }
            else
            {
                // FIXME - See ticket #337
                argument_types[0] = entry_list_head(candidates)->entity_specs.class_type;
            }
        }
    }
    else if (is_class_type(no_ref(expression_get_type(called_expression))))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: This is a call to an object, using operator() as the "
                    "candidate set\n");
        }
        type_t* class_type = no_ref(expression_get_type(called_expression));
        // This is a call to a nonstatic member function
        argument_types[0] = expression_get_type(called_expression);

        static AST operator = NULL;
        if (operator == NULL)
        {
            operator = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_FUNCTION_CALL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }

        scope_entry_list_t* first_set_candidates = get_member_function_of_class_type(class_type, operator, decl_context);
        candidates = unfold_and_mix_candidate_functions(first_set_candidates,
                /* builtins */ NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                ASTFileName(whole_function_call), ASTLine(whole_function_call),
                /* explicit_template_arguments */ NULL);
        entry_list_free(first_set_candidates);

#define MAX_SURROGATE_FUNCTIONS (64)
        int num_surrogate_functions = 0;

        type_t* actual_class_type = get_actual_class_type(class_type);
        if (is_named_class_type(class_type)
                && class_type_is_incomplete_independent(actual_class_type))
        {
            scope_entry_t* symbol = named_type_get_symbol(class_type);
            instantiate_template_class(symbol, decl_context, 
                    ASTFileName(called_expression), ASTLine(called_expression));
        }

        scope_entry_list_t* conversion_list = class_type_get_all_conversions(actual_class_type, decl_context);

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(conversion_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* conversion = entry_list_iterator_current(it);

            type_t* destination_type = function_type_get_return_type(conversion->type_information);

            if (is_pointer_to_function_type(no_ref(destination_type)))
            {
                ERROR_CONDITION(num_surrogate_functions == MAX_SURROGATE_FUNCTIONS,
                        "Too many surrogate functions to be considered in '%s'\n", 
                        prettyprint_in_buffer(whole_function_call));

                // Create a faked surrogate function with the type described below
                scope_entry_t* surrogate_symbol =
                    counted_calloc(1, sizeof(*surrogate_symbol), &_bytes_used_expr_check);

                // Add to candidates
                candidates = entry_list_add(candidates, surrogate_symbol);

                surrogate_symbol->kind = SK_FUNCTION;
                {
                    char c[256];
                    snprintf(c, 255, "<surrogate-function-%d>", num_surrogate_functions);
                    c[255] = '\0';

                    surrogate_symbol->symbol_name = uniquestr(c);
                }

                // Check this to be the proper context required
                surrogate_symbol->decl_context = decl_context;

                surrogate_symbol->line = ast_get_line(whole_function_call);
                surrogate_symbol->file = ast_get_filename(whole_function_call);

                // This is a surrogate function created here
                surrogate_symbol->entity_specs.is_surrogate_function = 1;
                surrogate_symbol->entity_specs.is_builtin = 1;

                // Given
                //
                // struct A
                // {
                //   operator R (*)(P1, .., Pn)();
                // };
                //
                // Create a type
                //
                //  R () (R (*) (P1, .., Pn), P1, .., Pn)
                type_t* conversion_functional_type = pointer_type_get_pointee_type(no_ref(destination_type));
                type_t* conversion_functional_return_type = function_type_get_return_type(conversion_functional_type);
                int conversion_functional_num_parameters = function_type_get_num_parameters(conversion_functional_type);

                // We add one for the first parameter type
                int surrogate_num_parameters = conversion_functional_num_parameters + 1;
                if (function_type_get_has_ellipsis(conversion_functional_type))
                {
                    // Add another for the ellipsis if needed
                    surrogate_num_parameters++;
                }

#define SURROGATE_MAX_PARAMETERS (32)
                parameter_info_t parameter_info[SURROGATE_MAX_PARAMETERS];
                memset(parameter_info, 0, sizeof(parameter_info));

                ERROR_CONDITION(SURROGATE_MAX_PARAMETERS <= surrogate_num_parameters, 
                        "Too many surrogate parameters %d", surrogate_num_parameters);

                // First parameter is the type itself
                parameter_info[0].type_info = destination_type;

                int k;
                for (k = 0; k < conversion_functional_num_parameters; k++)
                {
                    parameter_info[k + 1].type_info = function_type_get_parameter_type_num(conversion_functional_type, k);
                }

                if (function_type_get_has_ellipsis(conversion_functional_type))
                {
                    parameter_info[k + 1].is_ellipsis = 1;
                }

                // Get the type
                type_t* surrogate_function_type = get_new_function_type(conversion_functional_return_type, 
                        parameter_info, surrogate_num_parameters);

                // Set it as the type of function
                surrogate_symbol->type_information = surrogate_function_type;

                num_surrogate_functions++;
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(conversion_list);
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Calling overload resolution machinery\n");
    }

    scope_entry_t* conversors[MAX_ARGUMENTS];
    memset(conversors, 0, sizeof(conversors));

    candidate_t* candidate_set = NULL;
    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(candidates);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);

        if (entry->entity_specs.is_member
                || entry->entity_specs.is_surrogate_function)
        {
            candidate_set = add_to_candidate_set(candidate_set,
                    entry,
                    num_arguments,
                    argument_types);
        }
        else
        {
            candidate_set = add_to_candidate_set(candidate_set,
                    entry,
                    num_arguments - 1,
                    argument_types + 1);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(candidates);

    scope_entry_t* overloaded_call = solve_overload(candidate_set,
            decl_context,
            ASTFileName(whole_function_call),
            ASTLine(whole_function_call),
            conversors);

    if (overloaded_call != NULL)
    {
        ensure_not_deleted(decl_context, whole_function_call, overloaded_call);

        // Update the unresolved call
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Overload resolution succeeded\n");
        }
        expression_set_type(called_expression, overloaded_call->type_information);
        expression_set_type(advanced_called_expression, overloaded_call->type_information);
        // Tag the node with symbol information (this is useful to know who is being called)
        expression_set_symbol(advanced_called_expression, overloaded_call);

        if (arguments != NULL)
        {
            // Set conversors of arguments if needed
            AST iter;
            int arg_i = 0;
            for_each_element(arguments, iter)
            {
                AST argument = ASTSon1(iter);

                if (conversors[arg_i] != NULL)
                {
                    ensure_not_deleted(decl_context, argument, conversors[arg_i]);

                    ASTAttrSetValueType(argument, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(argument, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[arg_i]));
                }

                arg_i++;
            }
        }

#if 0
        // Instantiate if needed the overloaded call
        if (function_type_is_incomplete_independent(overloaded_call->type_information))
        {
            instantiate_template_function(overloaded_call, decl_context,
                    ASTFileName(whole_function_call),
                    ASTLine(whole_function_call));
        }
#endif
        return 1;
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Overload resolution failed\n");
        }

        if (!checking_ambiguity())
        {
            error_message_overload_failed(decl_context, whole_function_call, candidate_set);
        }

        return 0;
    }

    return 0;
}

char can_be_called_with_number_of_arguments(scope_entry_t *entry, int num_arguments)
{
    type_t* function_type = entry->type_information;

    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    int num_parameters = function_type_get_num_parameters(function_type);
    // Number of real parameters, ellipsis are counted as parameters
    // but only in the type system
    if (function_type_get_has_ellipsis(function_type))
        num_parameters--;

    // Simple case everybody considers
    if (num_parameters == num_arguments)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Function '%s' at '%s:%d' can be called with %d arguments since it matches the number of parameters\n",
                    entry->symbol_name,
                    entry->file,
                    entry->line,
                    num_arguments);
        }
        return 1;
    }
    else if (num_arguments > num_parameters)
    {
        // This can only be done if we have an ellipsis
        if (function_type_get_has_ellipsis(function_type))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Function '%s' at '%s:%d' can be called with %d (although the "
                        "function just has %d parameters) because of ellipsis\n",
                        entry->symbol_name,
                        entry->file,
                        entry->line,
                        num_arguments,
                        num_parameters);
            }
            return 1;
        }
        else
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Function '%s' at '%s:%d' cannot be called with %d arguments "
                        "since it expects %d parameters\n",
                        entry->symbol_name,
                        entry->file,
                        entry->line,
                        num_arguments,
                        num_parameters);
            }
            return 0;
        }
    }
    else if (num_arguments < num_parameters)
    {
        // We have to check that parameter num_arguments has default argument
        if (entry->entity_specs.default_argument_info != NULL
                && entry->entity_specs.default_argument_info[num_arguments] != NULL)
        {
            // Sanity check
            int i;
            for (i = num_arguments; i < num_parameters; i++)
            {
                ERROR_CONDITION(entry->entity_specs.default_argument_info[i] == NULL,
                        "Bad function parameter declaration info", 0);
            }
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Function '%s' at '%s:%d' can be called with %d arguments "
                        "(although it has %d parameters) because of default arguments\n",
                        entry->symbol_name,
                        entry->file,
                        entry->line,
                        num_arguments,
                        num_parameters);
            }
            return 1;
        }
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Function '%s' at '%s:%d' cannot be called with %d arguments "
                    "since it expects %d parameters\n",
                    entry->symbol_name,
                    entry->file,
                    entry->line,
                    num_arguments,
                    num_parameters);
        }
        return 0;
    }

    return 0;
}

// A function call is of the form
//   e1 ( e2 )
static char check_for_function_call(AST expr, decl_context_t decl_context)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Checking for function call '%s' at '%s'\n", 
                prettyprint_in_buffer(expr),
                ast_location(expr));
    }
    // e1 (in the comment above)
    AST called_expression = ASTSon0(expr);
    // e2 (in the comment above)
    AST arguments = ASTSon1(expr);

    // 1. Check if we might require Koenig lookup
    char might_require_koenig = 0;
    CXX_LANGUAGE()
    {
        // Note that koenig lookup is simply disabled by means of parentheses,
        // so the check has to be done here.
        if (ASTType(called_expression) == AST_SYMBOL
                // Note: template-ids do not qualify for Koenig (although they are unqualified id's)
                // || ASTType(called_expression) == AST_TEMPLATE_ID 
                // || ASTType(called_expression) == AST_OPERATOR_FUNCTION_ID_TEMPLATE
                // || ASTType(called_expression) == AST_DESTRUCTOR_ID
                // || ASTType(called_expression) == AST_DESTRUCTOR_TEMPLATE_ID 
            || ASTType(called_expression) == AST_CONVERSION_FUNCTION_ID
            || ASTType(called_expression) == AST_OPERATOR_FUNCTION_ID
            )
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Call to '%s' might require argument dependent lookup\n",
                        prettyprint_in_buffer(called_expression));
            }
            might_require_koenig = 1;
        }
    }

    // 2. Now perform the actual check. If this function succeeds
    //    'called_expression' node will have a functional type
    //    a pointer to function or a dependent type
    if (!check_for_functional_expression(expr, called_expression, arguments, 
            decl_context, might_require_koenig))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Functional expression '%s' at '%s' could not be checked\n",
                    prettyprint_in_buffer(expr), ast_location(expr));
        }
        return 0;
    }

    type_t* function_type = expression_get_type(called_expression);
    // CXX_LANGUAGE()
    {
        // 3. If is a dependent expression nothing else has to be computed
        //    tag all the expression as dependent
        if (is_dependent_expr_type(function_type))
        {
            expression_set_type(expr, function_type);
            return 1;
        }
    }

    // 4. If no dependent, computed type should be a functional or pointer
    //    to function type
    if (!is_function_type(no_ref(function_type))
            && !is_pointer_to_function_type(no_ref(function_type)))
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Solved expression at '%s' is neither a function nor a pointer to function\n", 
                    ast_location(expr));
        }
        return 0;
    }

    // 5. Jump over pointer if computed type was pointer to function type
    if (is_pointer_to_function_type(no_ref(function_type)))
        function_type = pointer_type_get_pointee_type(no_ref(function_type));

    // 6. Get the return type and tag all the expression with it

    type_t* return_type = function_type_get_return_type(no_ref(function_type));

    expression_set_type(expr, return_type);
    expression_set_is_lvalue(expr, 0);

    CXX_LANGUAGE()
    {
        if (is_dependent_type(return_type))
        {
            expression_set_dependent(expr);
        }
        expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));
    }

    return 1;
}

static char check_for_cast_expr(AST expr, AST type_id, AST casted_expression, decl_context_t decl_context)
{
    if (check_for_type_id_tree(type_id, decl_context)
            && check_for_expression(casted_expression, decl_context))
    {
        AST type_specifier = ASTSon0(type_id);
        AST abstract_declarator = ASTSon1(type_id);

        gather_decl_spec_t gather_info;
        memset(&gather_info, 0, sizeof(gather_info));

        type_t* simple_type_info = NULL;
        build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                decl_context);

        if (is_named_type(simple_type_info)
                && named_type_get_symbol(simple_type_info)->kind == SK_TEMPLATE)
        {
            // This is not valid here
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: invalid '%s' type-name, it names a template-name\n",
                        ast_location(type_specifier),
                        prettyprint_in_buffer(type_specifier));
            }
            return 0;
        }

        type_t* declarator_type = simple_type_info;
        compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                &declarator_type, decl_context);

        if (!check_for_expression(casted_expression, decl_context))
        {
            return 0;
        }

        if (is_dependent_type(declarator_type))
        {
            expression_set_dependent(expr);
        }
        else
        {
            expression_set_type(expr, declarator_type);

            char is_lvalue = 0;
            if (is_lvalue_reference_type(declarator_type))
            {
                is_lvalue = 1;
            }

            if (expression_is_constant(casted_expression)
                    && is_integral_type(declarator_type))
            {
                expression_set_constant(expr, 
                        const_value_cast_to_bytes(
                            expression_get_constant(casted_expression),
                            type_get_size(declarator_type), 
                            /* sign */ is_signed_integral_type(declarator_type)));
            }

            expression_set_is_value_dependent(expr,
                    expression_is_value_dependent(casted_expression));

            expression_set_is_lvalue(expr, is_lvalue);
        }
        return 1;
    }
    else
    {
        return 0;
    }
}


static char check_for_comma_operand(AST expression, decl_context_t decl_context)
{
    static AST operation_comma_tree = NULL;
    if (operation_comma_tree == NULL)
    {
        operation_comma_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_COMMA_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }


    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    char result = check_for_expression(lhs, decl_context)
        && check_for_expression(rhs, decl_context);

    if (!result)
        return 0;

    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    if (is_dependent_expr_type(lhs_type)
            || is_dependent_expr_type(rhs_type))
    {
        expression_set_dependent(expression);
        return 1;
    }

    char requires_overload = 0;

    CXX_LANGUAGE()
    {
        requires_overload = any_operand_is_class_or_enum(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (requires_overload)
    {
        // For comma it is empty
        scope_entry_list_t* builtins = NULL;

        scope_entry_t* selected_operator = NULL;

        // We do not want a warning if no overloads are available
        enter_test_expression();
        type_t* computed_type = compute_user_defined_bin_operator_type(operation_comma_tree,
                expression,
                lhs,
                rhs,
                builtins,
                decl_context,
                &selected_operator);
        leave_test_expression();

        // We will fall-through if no overload exists
        if (computed_type != NULL)
            return 1;
    }

    // Fall-through if no overload exists for comma
    expression_set_type(expression, expression_get_type(rhs));
    expression_set_is_lvalue(expression, expression_is_lvalue(rhs));

    if (expression_is_constant(rhs))
    {
        expression_set_constant(expression, expression_get_constant(rhs));
    }

    return 1;
}


static char check_for_templated_member_access(AST templated_member_access, decl_context_t decl_context, char is_arrow)
{
    return check_for_member_access(templated_member_access, decl_context, is_arrow);
}


static char check_for_member_access(AST member_access, decl_context_t decl_context, char is_arrow)
{
    char operator_arrow = 0;

    AST class_expr = ASTSon0(member_access);
    AST id_expression = ASTSon1(member_access);

    if (ASTType(id_expression) == AST_TEMPLATE_ID
            || ASTType(id_expression) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        solve_possibly_ambiguous_template_id(id_expression, decl_context);
    }

    if (!check_for_expression(class_expr, decl_context))
    {
        return 0;
    }

    type_t* accessed_type = expression_get_type(class_expr);

    CXX_LANGUAGE()
    {
        if (is_dependent_expr_type(accessed_type))
        {
            expression_set_type(member_access, accessed_type);
            // Best effort to remove ambiguities
            query_id_expression(decl_context, id_expression);
            return 1;
        }
    }

    // First we adjust the actually accessed type
    // if we are in '->' syntax
    if (is_arrow)
    {
        if (is_pointer_type(no_ref(accessed_type)))
        {
            accessed_type = pointer_type_get_pointee_type(no_ref(accessed_type));
        }
        else if (is_array_type(no_ref(accessed_type)))
        {
            accessed_type = array_type_get_element_type(no_ref(accessed_type));
        }
        else if (IS_CXX_LANGUAGE
                && is_class_type(no_ref(accessed_type)))
        {
            operator_arrow = 1;
        }
        else
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: '->' cannot be applied to '%s' (of type '%s')\n",
                        ast_location(class_expr),
                        prettyprint_in_buffer(class_expr),
                        print_type_str(no_ref(accessed_type), decl_context));
            }
            return 0;
        }
    }

    if (operator_arrow)
    {
        // In this case we have to lookup for an arrow operator
        // and then update the accessed type. We will rely on
        // overload mechanism to do it
        static AST arrow_operator_tree = NULL;
        if (arrow_operator_tree == NULL)
        {
            arrow_operator_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_POINTER_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }

        // First normalize the type keeping the cv-qualifiers
        cv_qualifier_t cv_qualif = CV_NONE;
        accessed_type = advance_over_typedefs_with_cv_qualif(no_ref(accessed_type), &cv_qualif);
        accessed_type = get_cv_qualified_type(accessed_type, cv_qualif);

        scope_entry_list_t* operator_arrow_list = get_member_function_of_class_type(accessed_type,
                arrow_operator_tree, decl_context);

        if (operator_arrow_list == NULL)
        {
            return 0;
        }

        type_t* argument_types[1] = { 
            /* Note that we want the real original type since it might be a referenced type (¿?¿??) */
            expression_get_type(class_expr) 
        };

        candidate_t* candidate_set = NULL;
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_arrow_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* entry = entry_list_iterator_current(it);
            candidate_set = add_to_candidate_set(candidate_set,
                    entry,
                    /* num_arguments */ 1,
                    argument_types);
        }
        entry_list_iterator_free(it);
        entry_list_free(operator_arrow_list);

        scope_entry_t* conversors[1] = { NULL };

        scope_entry_t* selected_operator_arrow = solve_overload(candidate_set,
                decl_context, ASTFileName(member_access), ASTLine(member_access),
                conversors);

        if (selected_operator_arrow == NULL)
        {
            if (!checking_ambiguity())
            {
                error_message_overload_failed(decl_context, member_access, candidate_set);
            }
            return 0;
        }

        ensure_not_deleted(decl_context, member_access, selected_operator_arrow);

        if (!is_pointer_to_class_type(function_type_get_return_type(selected_operator_arrow->type_information)))
        {
            return 0;
        }

        // Now we update the class_expr with the resulting type, this is used later when solving
        // overload in calls made using this syntax.
        expression_set_type(class_expr, function_type_get_return_type(selected_operator_arrow->type_information));

        // The accessed type is the pointed type
        accessed_type = pointer_type_get_pointee_type(no_ref(expression_get_type(class_expr)));
    }

    if (!is_class_type(no_ref(accessed_type)))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: '.' cannot be applied to '%s' (of type '%s')\n",
                    ast_location(class_expr),
                    prettyprint_in_buffer(class_expr),
                    print_type_str(no_ref(accessed_type), decl_context));
        }
        return 0;
    }
    
    // Advance over all typedefs keeping the underlying cv-qualification
    cv_qualifier_t cv_qualif = CV_NONE;
    accessed_type = advance_over_typedefs_with_cv_qualif(no_ref(accessed_type), &cv_qualif);
    accessed_type = get_cv_qualified_type(accessed_type, cv_qualif);

    // If syntax is 'a.operator T' or 'a->operator T' check if it yields a
    // dependent type
    type_t* conversion_type = NULL;
    if (ASTType(id_expression) == AST_CONVERSION_FUNCTION_ID)
    {
        /* char* conversion_function_name = */ get_conversion_function_name(decl_context, id_expression, 
                &conversion_type);

        // If the computed type is dependent then all the expression is dependent
        if (is_dependent_type(conversion_type))
        {
            expression_set_dependent(member_access);
            return 1;
        }
    }

    // This need not to be a member function but 'get_member_function_of_class_type' works
    // also for data members
    scope_entry_list_t* entry_list = get_member_function_of_class_type(accessed_type,
            id_expression, decl_context);

    if (entry_list == NULL)
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: '%s' is not a member/field of type '%s'\n",
                    ast_location(id_expression),
                    prettyprint_in_buffer(id_expression),
                    print_type_str(no_ref(accessed_type), decl_context));
        }
        return 0;
    }

    CXX_LANGUAGE()
    {
        // Again, if syntax is 'a.operator T' or 'a->operator T' check manually
        // the resulting type (this is T)
        if (ASTType(id_expression) == AST_CONVERSION_FUNCTION_ID)
        {
            ERROR_CONDITION(conversion_type == NULL,
                    "No type was computed for this conversion!", 0);
            // Get one that matches the type (we previously already checked 
            // it was not dependent)
            char found = 0;

            // FIXME - This might be template!
            scope_entry_list_iterator_t *it = NULL;
            for (it = entry_list_iterator_begin(entry_list);
                    !entry_list_iterator_end(it) && !found;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);

                type_t* current_conversion_type 
                    = function_type_get_return_type(entry->type_information);

                if (equivalent_types(conversion_type, current_conversion_type))
                {
                    entry_list_free(entry_list);
                    entry_list = entry_list_new(entry);
                    found = 1;
                }
            }
            entry_list_iterator_free(it);

            if (!found)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: '%s' is not a member of type '%s'\n",
                            ast_location(id_expression),
                            prettyprint_in_buffer(id_expression),
                            print_type_str(no_ref(accessed_type), decl_context));
                }
                return 0;
            }
        }
        // We have to solve this template-id
        else if (ASTType(id_expression) == AST_OPERATOR_FUNCTION_ID_TEMPLATE
                || ASTType(id_expression) == AST_TEMPLATE_ID)
        {
            char dependent_template_arguments = 0;
            type_t* solved = check_template_function(entry_list, id_expression, decl_context,
                    &dependent_template_arguments);

            if (solved != NULL)
            {
                expression_set_type(member_access, solved);
                return 1;
            }
            else if (dependent_template_arguments)
            {
                expression_set_dependent(member_access);
                return 1;
            }
            else
            {
                // solved == NULL && !dependent_template_arguments 
                return 0;
            }
        }
    }

    char ok = 0;
    scope_entry_t* entry = entry_list_head(entry_list);
    C_LANGUAGE()
    {
        // Store the symbol found
        expression_set_symbol(id_expression, entry);

        // C only will have fields
        expression_set_type(member_access, entry->type_information);
        expression_set_is_lvalue(member_access, 1);
        ok = 1;
    }

    CXX_LANGUAGE()
    {
        if (entry->kind == SK_VARIABLE)
        {
            // Store the member found
            expression_set_symbol(id_expression, entry);

            // This is a reference to the type
            if (!is_dependent_expr_type(entry->type_information))
            {
                expression_set_type(member_access, lvalue_ref(entry->type_information));
                expression_set_is_lvalue(member_access, 1);
                ok = 1;
            }
            else
            {
                expression_set_dependent(member_access);
                ok = 1;
            }
        }
        // In C++ if we have overload remember it
        else if (entry->kind == SK_FUNCTION
                || entry->kind == SK_TEMPLATE)
        {
            expression_set_type(member_access, get_unresolved_overloaded_type(entry_list, 
                        /* explicit_template_arguments */ NULL));
            ok = 1;
        }
    }

    entry_list_free(entry_list);

    return ok;
}

static char check_for_qualified_id(AST expr, decl_context_t decl_context, const_value_t** val)
{
    char result = compute_qualified_id_type(expr, decl_context, val);

    if (!result
            && !checking_ambiguity())
    {
        fprintf(stderr, "%s: warning: symbol '%s' not found in current scope\n",
                ast_location(expr), prettyprint_in_buffer(expr));
    }


    return result;
}

static type_t* check_template_function(scope_entry_list_t* entry_list,
        AST template_id, decl_context_t decl_context, 
        char *dependent_template_arguments)
{
    *dependent_template_arguments = 0;
    ERROR_CONDITION(entry_list == NULL,
            "This list cannot be NULL", 0);

    ERROR_CONDITION(ASTType(template_id) != AST_TEMPLATE_ID
            && ASTType(template_id) != AST_OPERATOR_FUNCTION_ID_TEMPLATE,
            "Invalid template-id", 0);

    // entry_list is a list of SK_TEMPLATE (that we expect them to be
    // function templates, actually)

    scope_entry_t* entry = entry_list_head(entry_list);

    // Basic check to disambiguate against class templates
    if (!is_template_type(entry->type_information)
            || named_type_get_symbol(
                template_type_get_primary_type(entry->type_information)
                )->kind != SK_FUNCTION)
    {
        return NULL;
    }

    if (!solve_possibly_ambiguous_template_id(template_id, decl_context))
    {
        return NULL;
    }

    int nesting_level = template_type_get_nesting_level(
            entry->type_information);

    // Sanity check
    {
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* current_entry = entry_list_iterator_current(it);
            ERROR_CONDITION(
                    template_type_get_nesting_level(current_entry->type_information) != nesting_level, 
                    "Nesting level of all specializations do not match!!\n", 0);
        }
        entry_list_iterator_free(it);
    }

    scope_entry_t* primary_symbol = 
        named_type_get_symbol(template_type_get_primary_type(entry->type_information));

    if (primary_symbol->entity_specs.is_member
            && is_dependent_type(primary_symbol->entity_specs.class_type))
    {
        *dependent_template_arguments = 1;
        return NULL;
    }

    // Now we have to solve the template function
    template_argument_list_t* template_arguments = get_template_arguments_from_syntax(
            ASTSon1(template_id), decl_context, nesting_level);

    if (has_dependent_template_arguments(template_arguments))
    {
        *dependent_template_arguments = 1;
        return NULL;
    }

    return get_unresolved_overloaded_type(entry_list, template_arguments);
}

static char check_for_template_id_expr(AST expr, decl_context_t decl_context)
{
    ERROR_CONDITION(ASTType(expr) != AST_TEMPLATE_ID
            && ASTType(expr) != AST_OPERATOR_FUNCTION_ID, "Invalid template-id", 0);

    scope_entry_list_t *entry_list = query_id_expression(decl_context, expr);

    if (entry_list == NULL)
        return 0;

    char dependent_template_arguments = 0;

    type_t* solved = check_template_function(entry_list, expr, decl_context,
            &dependent_template_arguments);

    char ok = 0;
    if (solved != NULL)
    {
        expression_set_type(expr, solved);
        // This is arguable since this is unlikely to go at the left of any
        // expression, but ok, lvalueness is such a mystic thing throughout the
        // standard.
        expression_set_is_lvalue(expr, 1);
        ok = 1;
    } 
    else if (dependent_template_arguments)
    {
        // This might well be a function template name but with wrong template-parameters
        // because of type dependency. So give it a second chance
        expression_set_dependent(expr);
        ok = 1;
    }

    entry_list_free(entry_list);

    return ok;
}

static char check_for_postoperator_user_defined(AST expr, AST operator, 
        AST postoperated_expr, 
        decl_context_t decl_context,
        scope_entry_list_t* builtins)
{
    type_t* incremented_type = expression_get_type(postoperated_expr);

    type_t* argument_types[2] = {
        incremented_type, // Member argument
        get_zero_type() // Postoperation
    };
    int num_arguments = 2;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(incremented_type)))
    {
        scope_entry_list_t *operator_entry_list = get_member_function_of_class_type(no_ref(incremented_type),
                operator, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                ASTFileName(expr), ASTLine(expr),
                /* explicit_template_arguments */ NULL);
        entry_list_free(operator_entry_list);
    }

    // We need to do koenig lookup for non-members
    // otherwise some overloads might not be found
    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, operator);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(entry_list,
            builtins, argument_types, num_arguments,
            decl_context,
            ASTFileName(expr), ASTLine(expr), /* explicit_template_arguments */ NULL);
    entry_list_free(entry_list);

    scope_entry_list_t* old_overload_set = overload_set;
    overload_set = entry_list_merge(old_overload_set, operator_overload_set);
    entry_list_free(old_overload_set);
    entry_list_free(operator_overload_set);

    scope_entry_t* conversors[1] = { NULL };

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = add_to_candidate_set(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);

    scope_entry_t* overloaded_call = solve_overload(candidate_set,
            decl_context, ASTFileName(expr), ASTLine(expr), 
            conversors);

    if (overloaded_call != NULL)
    {
        ensure_not_deleted(decl_context, expr, overloaded_call);

        expression_set_type(expr, function_type_get_return_type(overloaded_call->type_information));
        expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));

        if (conversors[0] != NULL)
        {
            ensure_not_deleted(decl_context, expr, conversors[0]);
            ASTAttrSetValueType(postoperated_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(postoperated_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[0]));
        }

        return 1;
    }
    else
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(decl_context, expr, candidate_set);
        }
    }

    return 0;
}

static char check_for_preoperator_user_defined(AST expr, AST operator, 
        AST preoperated_expr,
        decl_context_t decl_context,
        scope_entry_list_t* builtins)
{
    type_t* incremented_type = expression_get_type(preoperated_expr);

    type_t* argument_types[1] = {
        incremented_type, 
    };
    int num_arguments = 1;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(incremented_type)))
    {
        scope_entry_list_t *operator_entry_list = get_member_function_of_class_type(no_ref(incremented_type),
                operator, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                ASTFileName(expr), ASTLine(expr),
                /* explicit_template_arguments */ NULL);
        entry_list_free(operator_entry_list);
    }

    // Otherwise lookup in non-member operator
    decl_context_t outer_namespace_scope = decl_context;
    outer_namespace_scope.current_scope = outer_namespace_scope.namespace_scope;

    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, operator);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            entry_list, builtins, argument_types, num_arguments,
            decl_context,
            ASTFileName(expr), ASTLine(expr), /* explicit_template_arguments */ NULL);
    entry_list_free(entry_list);

    scope_entry_list_t* old_overload_set = overload_set;
    overload_set = entry_list_merge(old_overload_set, operator_overload_set);
    entry_list_free(old_overload_set);
    entry_list_free(operator_overload_set);

    scope_entry_t* conversors[1] = { NULL };

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(overload_set);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        candidate_set = add_to_candidate_set(candidate_set,
                entry_list_iterator_current(it),
                num_arguments,
                argument_types);
    }
    entry_list_iterator_free(it);
    entry_list_free(overload_set);

    scope_entry_t* overloaded_call = solve_overload(candidate_set,
            decl_context, ASTFileName(expr), ASTLine(expr), conversors);

    if (overloaded_call != NULL)
    {
        ensure_not_deleted(decl_context, expr, overloaded_call);

        expression_set_type(expr, function_type_get_return_type(overloaded_call->type_information));
        expression_set_is_lvalue(expr, is_lvalue_reference_type(expression_get_type(expr)));

        if (conversors[0] != NULL)
        {
            ensure_not_deleted(decl_context, expr, conversors[0]);
            ASTAttrSetValueType(preoperated_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(preoperated_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[0]));
        }

        return 1;
    }
    else
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(decl_context, expr, candidate_set);
        }
    }

    return 0;
}

static char postoperator_incr_pred(type_t* lhs, type_t* rhs)
{
    return (is_lvalue_reference_type(lhs)
            && (is_arithmetic_type(reference_type_get_referenced_type(lhs))
                || is_pointer_type(reference_type_get_referenced_type(lhs)))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs))
            && is_zero_type(rhs));
}

static char postoperator_decr_pred(type_t* lhs, type_t* rhs)
{
    return (is_lvalue_reference_type(lhs)
            && (is_arithmetic_type(reference_type_get_referenced_type(lhs))
                || is_pointer_type(reference_type_get_referenced_type(lhs)))
            && !is_bool_type(reference_type_get_referenced_type(lhs))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs))
            && is_zero_type(rhs));
}

static type_t* postoperator_result(type_t** lhs, 
        type_t** rhs UNUSED_PARAMETER // This one holds a zero type
        )
{
    // a++ returns a value, not a reference
    type_t* result = get_unqualified_type(no_ref(*lhs));
    return result;
}

static char check_for_postoperator(AST expr, AST operator, AST postoperated_expr, 
        decl_context_t decl_context, char is_decrement)
{
    if (!check_for_expression(postoperated_expr, decl_context))
        return 0;

    type_t* operated_type = expression_get_type(postoperated_expr);
    char is_lvalue = expression_is_lvalue(postoperated_expr);

    if (is_dependent_expr_type(operated_type))
    {
        expression_set_dependent(expr);
        return 1;
    }

    char requires_overload = 0;
    
    CXX_LANGUAGE()
    {
        requires_overload = is_class_type(no_ref(operated_type))
            || is_enum_type(no_ref(operated_type));
    }

    if (!requires_overload)
    {
        if (is_pointer_type(no_ref(operated_type))
                || is_arithmetic_type(no_ref(operated_type)))
        {
            // Should be a lvalue
            C_LANGUAGE()
            {
                if (!is_lvalue)
                    return 0;
            }
            CXX_LANGUAGE()
            {
                if (!is_lvalue_reference_type(operated_type))
                    return 0;

                operated_type = reference_type_get_referenced_type(operated_type);
            }

            expression_set_type(expr, lvalue_ref(get_unqualified_type(operated_type)));
            expression_set_is_lvalue(expr, 0);
            return 1;
        }
        else
        {
            return 0;
        }
    }

    char (*postoperator_pred)(type_t*, type_t*);
    if (is_decrement)
    {
        postoperator_pred = postoperator_decr_pred;
    }
    else
    {
        postoperator_pred = postoperator_incr_pred;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            // Note that we do not remove the left reference
            operated_type, 
            // This is the 0 argument of operator++(T&, 0)
            get_zero_type(),
            &builtin_set,
            decl_context, operator,
            postoperator_pred,
            postoperator_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);
    
    // Only C++ after this point
    char result = check_for_postoperator_user_defined(expr, operator, 
            postoperated_expr, decl_context, builtins);

    entry_list_free(builtins);

    return result;
}

static char preoperator_incr_pred(type_t* lhs)
{
    return (is_lvalue_reference_type(lhs)
            && (is_arithmetic_type(reference_type_get_referenced_type(lhs))
                || is_pointer_type(reference_type_get_referenced_type(lhs)))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs)));
}

static char preoperator_decr_pred(type_t* lhs)
{
    return (is_lvalue_reference_type(lhs)
            && (is_arithmetic_type(reference_type_get_referenced_type(lhs))
                || is_pointer_type(reference_type_get_referenced_type(lhs)))
            && !is_bool_type(reference_type_get_referenced_type(lhs))
            && !is_const_qualified_type(reference_type_get_referenced_type(lhs)));
}

static type_t* preoperator_result(type_t** lhs)
{
    return *lhs;
}

static char check_for_preoperator(AST expr, AST operator, 
        AST preoperated_expr, decl_context_t decl_context,
        char is_decrement)
{
    if (!check_for_expression(preoperated_expr, decl_context))
        return 0;

    type_t* operated_type = expression_get_type(preoperated_expr);
    char is_lvalue = expression_is_lvalue(preoperated_expr);

    if (is_dependent_expr_type(operated_type))
    {
        expression_set_dependent(expr);
        return 1;
    }

    if (is_pointer_type(no_ref(operated_type))
            || is_arithmetic_type(no_ref(operated_type)))
    {
        C_LANGUAGE()
        {
            // Must be an lvalue
            if (!is_lvalue)
                return 0;
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(operated_type))
                return 0;
        }

        expression_set_type(expr, operated_type);
        expression_set_is_lvalue(expr, 1);
        return 1;
    }
    else
    {
        C_LANGUAGE()
        {
            return 0;
        }
    }

    char (*preoperator_pred)(type_t*);
    if (is_decrement)
    {
        preoperator_pred = preoperator_decr_pred;
    }
    else
    {
        preoperator_pred = preoperator_incr_pred;
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            // Note that we do not remove the left reference
            operated_type, 
            &builtin_set,
            decl_context, operator, 
            preoperator_pred,
            preoperator_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    // Only C++ after this point
    char result = check_for_preoperator_user_defined(expr, operator, 
            preoperated_expr, decl_context, builtins);

    entry_list_free(builtins);

    return result;
}

static char check_for_postincrement(AST expr, decl_context_t decl_context)
{
    // In C++
    //
    // 'e++' is either 'operator++(e, 0)' or 'e.operator++(0)'
    //
    AST postincremented_expr = ASTSon0(expr);

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_INCREMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    return check_for_postoperator(expr, operation_tree, 
            postincremented_expr, decl_context, /* is_decr */ 0);
}

static char check_for_postdecrement(AST expr, decl_context_t decl_context)
{
    // In C++
    //
    // 'e--' is either 'operator--(e, 0)' or 'e.operator--(0)'
    //
    AST postdecremented_expr = ASTSon0(expr);

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DECREMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    return check_for_postoperator(expr, operation_tree, 
            postdecremented_expr, decl_context, /* is_decr */ 1);
}

static char check_for_preincrement(AST expr, decl_context_t decl_context)
{
    // In C++
    //
    // '++e' is either 'operator++(e)' or 'e.operator++()'
    //
    AST preincremented_expr = ASTSon0(expr);

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_INCREMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    return check_for_preoperator(expr, operation_tree, preincremented_expr, 
            decl_context, /* is_decr */ 0);
}

static char check_for_predecrement(AST expr, decl_context_t decl_context)
{
    // In C++
    //
    // '--e' is either 'operator--(e)' or 'e.operator--()'
    //
    AST predecremented_expr = ASTSon0(expr);

    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DECREMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    return check_for_preoperator(expr, operation_tree, predecremented_expr, 
            decl_context, /* is_decr */ 1);
}

static scope_entry_t* get_typeid_symbol(decl_context_t decl_context, AST expr)
{
    // Lookup for 'std::type_info'
    static scope_entry_t* typeid_sym = NULL;

    // FIXME: This will last accross files
    if (typeid_sym == NULL)
    {
        decl_context_t global_context = decl_context;
        global_context.current_scope = global_context.global_scope;

        scope_entry_list_t* entry_list = query_in_scope_str(global_context, "std");

        if (entry_list == NULL 
                || entry_list_head(entry_list)->kind != SK_NAMESPACE)
        {
            if (entry_list != NULL)
                entry_list_free(entry_list);

            running_error("%s: error: namespace 'std' not found when looking up 'std::type_info' (because of '%s'). \n"
                    "Maybe you need '#include <typeinfo>'",
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
        }

        decl_context_t std_context = entry_list_head(entry_list)->namespace_decl_context;
        entry_list_free(entry_list);
        entry_list = query_in_scope_str(std_context, "type_info");

        if (entry_list == NULL
                || (entry_list_head(entry_list)->kind != SK_CLASS
                    && entry_list_head(entry_list)->kind != SK_TYPEDEF))
        {
            if (entry_list != NULL)
                entry_list_free(entry_list);

            running_error("%s: error: typename 'type_info' not found when looking up 'std::type_info' (because of '%s')\n"
                    "Maybe you need '#include <typeinfo>'",
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
        }

        typeid_sym = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return typeid_sym;
}

scope_entry_t* get_std_initializer_list_template(decl_context_t decl_context, AST expr, char mandatory)
{
    // Lookup for 'std::initializer_list'
    static scope_entry_t* initializer_list_sym = NULL;

    // FIXME: This will last accross files
    if (initializer_list_sym == NULL)
    {
        decl_context_t global_context = decl_context;
        global_context.current_scope = global_context.global_scope;

        scope_entry_list_t* entry_list = query_in_scope_str(global_context, "std");

        if (entry_list == NULL 
                || entry_list_head(entry_list)->kind != SK_NAMESPACE)
        {
            if (entry_list != NULL)
                entry_list_free(entry_list);
            if (!mandatory)
                return NULL;

            running_error("%s: error: namespace 'std' not found when looking up 'std::initializer_list' (because of '%s'). \n"
                    "Maybe you need '#include <initializer_list>'",
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
        }

        decl_context_t std_context = entry_list_head(entry_list)->namespace_decl_context;
        entry_list_free(entry_list);

        entry_list = query_in_scope_str(std_context, "initializer_list");

        if (entry_list == NULL
                || entry_list_head(entry_list)->kind != SK_TEMPLATE)
        {
            if (entry_list != NULL)
                entry_list_free(entry_list);
            if (!mandatory)
                return NULL;

            running_error("%s: error: template-name 'initializer_list' not found when looking up 'std::initializer_list' (because of '%s')\n"
                    "Maybe you need '#include <initializer_list>'",
                    ast_location(expr),
                    prettyprint_in_buffer(expr));
        }

        initializer_list_sym = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return initializer_list_sym;
}

static char check_for_typeid_type(AST expr, decl_context_t decl_context)
{
    char result = check_for_type_id_tree(ASTSon0(expr), decl_context);

    if (!result)
        return 0;

    type_t* type = compute_type_for_type_id_tree(ASTSon0(expr), decl_context);
    if (is_dependent_type(type))
    {
        expression_set_is_value_dependent(expr, 1);
    }

    expression_set_type(expr, 
            get_user_defined_type(get_typeid_symbol(decl_context, expr)));
    expression_set_is_lvalue(expr, 0);

    return 1;
}

static char check_for_typeid_expr(AST expr, decl_context_t decl_context)
{
    char result = check_for_expression(ASTSon0(expr), decl_context);

    if (!result)
        return 0;

    if (is_dependent_expr_type(expression_get_type(ASTSon0(expr)))
            || expression_is_value_dependent(ASTSon0(expr)))
    {
        expression_set_is_value_dependent(expr, 1);
    }

    expression_set_type(expr, 
            get_user_defined_type(get_typeid_symbol(decl_context, expr)));
    expression_set_is_lvalue(expr, 0);
    return 1;
}

static char check_for_designation(AST designation, decl_context_t decl_context)
{
    AST designator_list = ASTSon0(designation);
    AST iter;

    for_each_element(designator_list, iter)
    {
        AST designator = ASTSon1(iter);

        if (ASTType(designator) == AST_INDEX_DESIGNATOR)
        {
            AST index_designator = designator;
            AST constant_expression = ASTSon0(index_designator);
            if (!check_for_expression(constant_expression, decl_context))
                return 0;
        }
    }

    return 1;
}

type_t* get_designated_type(AST designation, decl_context_t decl_context, type_t* designated_type)
{
    // This is currently for C99 only, so no need to check too many things on the C++ part
    
    AST designator_list = ASTSon0(designation);
    AST iterator;

    for_each_element(designator_list, iterator)
    {
        AST current_designator = ASTSon1(iterator);

        switch (ASTType(current_designator))
        {
            case AST_FIELD_DESIGNATOR:
                {
                    AST symbol = ASTSon0(current_designator);
                    if (is_class_type(designated_type))
                    {
                        scope_entry_list_t* member_list = get_member_of_class_type(designated_type, symbol, decl_context);

                        char ok = 1;
                        if (member_list != NULL)
                        {
                            ok = 0;
                        }
                        else
                        {
                            scope_entry_t* member = entry_list_head(member_list);
                            entry_list_free(member_list);

                            if (member->kind == SK_VARIABLE)
                            {
                                designated_type = member->type_information;
                            }
                            else
                            {
                                ok = 0;
                            }
                        }

                        if (!ok)
                        {
                            fprintf(stderr, "%s: warning: designator '%s' of type '%s' is not valid\n",
                                ast_location(current_designator),
                                prettyprint_in_buffer(current_designator),
                                print_decl_type_str(designated_type, decl_context, ""));
                            designated_type = get_signed_int_type();
                        }
                    }
                    break;
                }
            case AST_INDEX_DESIGNATOR:
                {
                    if (is_array_type(designated_type))
                    {
                        designated_type = array_type_get_element_type(designated_type);
                    }
                    else
                    {
                        fprintf(stderr, "%s: warning: designator '%s' of type '%s' is not valid\n",
                                ast_location(current_designator),
                                prettyprint_in_buffer(current_designator),
                                print_decl_type_str(designated_type, decl_context, ""));
                        return get_signed_int_type();
                    }
                    break;
                }
             default:
                internal_error("Invalid AST '%s'\n", ast_print_node_type(ASTType(current_designator)));
        }
    }

    return designated_type;
}

static char check_for_braced_initializer_list(AST initializer, decl_context_t decl_context, type_t* declared_type)
{
    ERROR_CONDITION (ASTType(initializer) != AST_INITIALIZER_BRACES, "Invalid node", 0);

    AST expression_list = ASTSon0(initializer);
    if ((is_class_type(declared_type)
                || is_array_type(declared_type)
                || is_vector_type(declared_type))
            && is_aggregate_type(declared_type))
    {
        if (expression_list != NULL)
        {
            if (!is_array_type(declared_type)
                    && !is_class_type(declared_type)
                    && !is_vector_type(declared_type)
                    && !is_dependent_type(declared_type))
            {
                fprintf(stderr, "%s: warning: brace initialization can only be used with\n",
                        ast_location(initializer));
                fprintf(stderr, "%s: warning: array types, struct, class, union or vector types\n",
                        ast_location(initializer));
            }

            // Special case for this sort of initializations
            /*
               char a[] = { "hello" };
             */

            // The expression list has only one element of kind expression
            if (ASTSon0(expression_list) == NULL
                    && ASTType(ASTSon1(expression_list)) != AST_INITIALIZER_BRACES
                    && ASTType(ASTSon1(expression_list)) != AST_DESIGNATED_INITIALIZER
                    && ASTType(ASTSon1(expression_list)) != AST_GCC_INITIALIZER_CLAUSE)
            {
                // Attempt an interpretation like char a[] = "hello";
                // This will shut up the compiler if it is a failed attempt
                enter_test_expression();
                char result = check_for_initializer_clause(ASTSon1(expression_list), decl_context, declared_type);
                leave_test_expression();

                if (result)
                {
                    type_t* t = expression_get_type(ASTSon1(expression_list));
                    if (!is_dependent_expr_type(t))
                    {
                        expression_set_type(initializer, expression_get_type(ASTSon1(expression_list)));
                        // Do nothing else
                        return 1;
                    }
                }
            }

            char faulty = 0;
            int initializer_num = 0;
            AST iter;
            for_each_element(expression_list, iter)
            {
                AST initializer_clause = ASTSon1(iter);

                type_t* type_in_context = declared_type;

                // If the initializer is not a designated one, the type in context
                // is the subtype of the aggregated one
                if (ASTType(initializer_clause) != AST_DESIGNATED_INITIALIZER
                        // This is the gcc-esque way of a designated
                        // initializer, used when C99 is not available
                        && ASTType(initializer_clause) != AST_GCC_INITIALIZER_CLAUSE)
                {
                    if (is_class_type(declared_type))
                    {
                        type_t* actual_class_type = get_actual_class_type(declared_type);

                        if (initializer_num >= class_type_get_num_nonstatic_data_members(actual_class_type))
                        {
                            fprintf(stderr, "%s: warning: too many initializers for aggregated data type\n",
                                    ast_location(initializer_clause));

                            type_in_context = get_signed_int_type();
                        }
                        else
                        {
                            scope_entry_t* data_member = class_type_get_nonstatic_data_member_num(actual_class_type, initializer_num);
                            type_in_context = data_member->type_information;
                        }
                    }
                    else if (is_array_type(declared_type))
                    {
                        type_in_context = array_type_get_element_type(declared_type);
                    }
                    else if (is_vector_type(declared_type))
                    {
                        type_in_context = vector_type_get_element_type(declared_type);
                    }
                    else
                    {
                        internal_error("Invalid aggregated type '%s'",
                                print_decl_type_str(declared_type, decl_context, ""));
                    }
                }

                if (!check_for_initializer_clause(initializer_clause, decl_context, type_in_context))
                    faulty = 1;

                initializer_num++;
            }

            if (faulty)
                return 0;

            if (is_class_type(declared_type))
            {
                expression_set_type(initializer, declared_type);
            }
            else if (is_array_type(declared_type))
            {
                char c[64];
                snprintf(c, 63, "%d", initializer_num);
                c[63] = '\0';
                AST length = 
                    ASTMake1(AST_EXPRESSION,
                            ASTLeaf(AST_DECIMAL_LITERAL, 
                                ast_get_filename(initializer),
                                ast_get_line(initializer),
                                c),
                            ast_get_filename(initializer),
                            ast_get_line(initializer),
                            NULL);

                type_t *list_array = get_array_type(
                        array_type_get_element_type(declared_type),
                        length, decl_context);

                expression_set_type(initializer, list_array);
            }
        }
        return 1;
    }
    // Not an aggregate class
    else if (is_class_type(declared_type)
            && !is_aggregate_type(declared_type))
    {
        // This one is the toughest
        type_t* arg_list[MAX_ARGUMENTS];
        memset(arg_list, 0, sizeof(arg_list));

        char any_is_dependent = 0;

        int num_args = 0;
        AST iter;
        for_each_element(expression_list, iter)
        {
            AST expr = ASTSon1(iter);

            if (num_args == MAX_ARGUMENTS)
            {
                running_error("%s: error: too many elements in initializer-list\n",
                        ast_location(expr));
            }

            if (!check_for_expression(expr, decl_context))
            {
                return 0;
            }

            arg_list[num_args] = expression_get_type(expr);

            if (is_dependent_expr_type(arg_list[num_args]))
                any_is_dependent = 1;
            num_args++;
        }

        any_is_dependent = any_is_dependent || is_dependent_type(declared_type);
        if (any_is_dependent)
            return 1;

        // Now construct the candidates for overloading among the constructors
        int num_ctors = class_type_get_num_constructors(get_actual_class_type(declared_type));

        scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, initializer, /* mandatory */ 0);

        char has_initializer_list_ctor = 0;

        // If std::initializer_list is not available there is no need to check
        // this because it would have failed before
        if (std_initializer_list_template != NULL)
        {
            int i;
            for (i = 0; i < num_ctors && !has_initializer_list_ctor; i++)
            {
                scope_entry_t* entry = class_type_get_constructors_num(get_actual_class_type(declared_type), i);

                int num_parameters = function_type_get_num_parameters(entry->type_information);
                // Number of real parameters, ellipsis are counted as parameters
                // but only in the type system
                if (function_type_get_has_ellipsis(entry->type_information))
                    num_parameters--;

                if (num_parameters > 0
                        && can_be_called_with_number_of_arguments(entry, 1))
                {
                    type_t* first_param = function_type_get_parameter_type_num(entry->type_information, 0);

                    if (is_class_type(first_param))
                        first_param = get_actual_class_type(first_param);

                    if (is_template_specialized_type(first_param)
                            && equivalent_types(template_specialized_type_get_related_template_type(first_param), 
                                std_initializer_list_template->type_information))
                    {
                        has_initializer_list_ctor = 1;
                    }
                }
            }
        }

        if (!has_initializer_list_ctor)
        {
            // Plain constructor resolution should be enough here
            scope_entry_t* conversors[MAX_ARGUMENTS] = { 0 };
            scope_entry_list_t* candidates = NULL;
            scope_entry_t* constructor = solve_constructor(declared_type,
                    arg_list,
                    num_args,
                    /* is_explicit */ 0,
                    decl_context,
                    ASTFileName(initializer), ASTLine(initializer),
                    conversors,
                    &candidates);
            entry_list_free(candidates);

            if (constructor == NULL)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: invalid initializer for type '%s'\n", 
                            ast_location(initializer),
                            print_type_str(declared_type, decl_context));
                }
                return 0;
            }
            else
            {
                ensure_not_deleted(decl_context, initializer, constructor);
                int i;
                for (i = 0; i < num_args; i++)
                {
                    if (conversors[i] != NULL)
                    {
                        ensure_not_deleted(decl_context, initializer, conversors[i]);
                    }
                }
                return 1;
            }
        }
        else
        {
            type_t* initializer_list_type = NULL;

            initializer_list_type = get_braced_list_type(num_args, arg_list);

            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: For initializer list '%s', common type is '%s'\n", prettyprint_in_buffer(initializer), 
                        print_type_str(initializer_list_type, decl_context));
            }

            scope_entry_t* conversors[MAX_ARGUMENTS] = { 0 };

            template_parameter_list_t* template_parameters = 
                template_type_get_template_parameters(std_initializer_list_template->type_information);

            template_argument_list_t *argument_list = counted_calloc(1, sizeof(*argument_list), &_bytes_used_expr_check);
            template_argument_t *argument = counted_calloc(1, sizeof(*argument), &_bytes_used_expr_check);
            argument->kind = TAK_TYPE;
            argument->type = initializer_list_type;

            P_LIST_ADD(argument_list->argument_list, argument_list->num_arguments, argument);

            type_t* specialized_std_initializer = 
                template_type_get_specialized_type(std_initializer_list_template->type_information,
                        argument_list, template_parameters,
                        decl_context, ASTLine(initializer), ASTFileName(initializer));

            // Should it be a const T&  ?
            arg_list[0] = specialized_std_initializer;
            num_args = 1;

            scope_entry_list_t* candidates = NULL;
            scope_entry_t* constructor = solve_constructor(declared_type,
                    arg_list,
                    num_args,
                    /* is_explicit */ 0,
                    decl_context,
                    ASTFileName(initializer), ASTLine(initializer),
                    conversors,
                    &candidates);
            entry_list_free(candidates);

            if (constructor == NULL)
            {
                if (!checking_ambiguity())
                {
                    fprintf(stderr, "%s: warning: invalid initializer for type '%s'\n", 
                            ast_location(initializer),
                            print_type_str(declared_type, decl_context));
                }
                return 0;
            }
            else
            {
                ensure_not_deleted(decl_context, initializer, constructor);
                int i = 0;
                for (i = 0; i < num_args; i++)
                {
                    if (conversors[i] != NULL)
                    {
                        ensure_not_deleted(decl_context, initializer, conversors[i]);
                    }
                }
                return 1;
            }
        }
    }
    // Not an aggregate of any kind
    else 
    {
        if (expression_list != NULL)
        {
            AST prev = ASTSon0(expression_list);

            if (prev != NULL)
            {
                fprintf(stderr, "%s: warning: brace initialization with more than one element is not valid here\n",
                        ast_location(initializer));
                return 0;
            }

            AST initializer_clause = ASTSon1(expression_list);
            if (check_for_initializer_clause(initializer_clause, decl_context, declared_type))
            {
                expression_set_type(expression_list, expression_get_type(initializer_clause));
                expression_set_constant(expression_list, expression_get_constant(initializer_clause));
                return 1;
            }
        }
        else
        {
            // Empty is OK
            // FIXME: We should compute the constant value
            expression_set_type(expression_list, declared_type);
            return 1;
        }
    }

    return 0;
}

char check_for_initializer_clause(AST initializer, decl_context_t decl_context, type_t* declared_type)
{
    switch (ASTType(initializer))
    {
        // Default is an expression
        default:
            {
                char result = check_for_expression(initializer, decl_context);

                if (result)
                {
                    type_t* declared_type_no_cv = get_unqualified_type(declared_type);
                    type_t* initializer_expr_type = expression_get_type(initializer);
                    // Now we have to check whether this can be converted to the declared entity
                    char ambiguous_conversion = 0;
                    scope_entry_t* conversor = NULL;
                    standard_conversion_t standard_conversion_sequence;
                    if (!is_dependent_type(declared_type_no_cv)
                            && !is_dependent_expr_type(initializer_expr_type)
                            && !(IS_C_LANGUAGE
                                && standard_conversion_between_types(
                                    &standard_conversion_sequence,
                                    initializer_expr_type,
                                    declared_type_no_cv))
                            && !(IS_CXX_LANGUAGE 
                                && type_can_be_implicitly_converted_to(initializer_expr_type, 
                                    declared_type_no_cv, 
                                    decl_context, 
                                    &ambiguous_conversion, 
                                    &conversor))
                            // A cv char[x] can be initialized with a string literal, we do not check the size
                            && !(is_array_type(declared_type_no_cv)
                                && is_character_type(array_type_get_element_type(declared_type_no_cv))
                                && is_array_type(no_ref(initializer_expr_type))
                                && is_char_type(array_type_get_element_type(no_ref(initializer_expr_type)))
                                && is_literal_string_type(no_ref(initializer_expr_type))
                                )
                            // A wchar_t[x] can be initialized with a wide string literal, we do not check the size
                            && !(is_array_type(declared_type_no_cv)
                                    && is_wchar_t_type(array_type_get_element_type(declared_type_no_cv))
                                    && is_array_type(no_ref(initializer_expr_type))
                                    && is_wchar_t_type(array_type_get_element_type(no_ref(initializer_expr_type)))
                                    && is_literal_string_type(no_ref(initializer_expr_type))
                                )
                            )
                            {
                                if (!checking_ambiguity())
                                {
                                    fprintf(stderr, "%s: warning: initializer initializer '%s' has type '%s' not convertible to '%s'\n",
                                            ast_location(initializer),
                                            prettyprint_in_buffer(initializer),
                                            print_decl_type_str(initializer_expr_type, decl_context, ""),
                                            print_decl_type_str(declared_type, decl_context, ""));
                                }
                                return 0;
                            }

                    if (conversor != NULL)
                    {
                        ASTAttrSetValueType(initializer, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                        ASTAttrSetValueType(initializer, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversor));
                    }
                }

                return result;
                break;
            }
        case AST_INITIALIZER_BRACES:
            {
                return check_for_braced_initializer_list(initializer, decl_context, declared_type);
            }
        case AST_DESIGNATED_INITIALIZER :
            {
                AST designation = ASTSon0(initializer);

                check_for_designation(designation, decl_context);

                AST initializer_clause = ASTSon1(initializer);

                type_t* designated_type = get_designated_type(designation, decl_context, declared_type);

                char result = check_for_initializer_clause(initializer_clause, decl_context, designated_type);
                if (result)
                {
                    expression_set_type(initializer, expression_get_type(initializer_clause));
                }
                return result;
                break;
            }
        case AST_GCC_INITIALIZER_CLAUSE :
            {
                AST symbol = ASTSon0(initializer);
                AST initializer_clause = ASTSon1(initializer);

                char result = 0;
                if (is_class_type(declared_type))
                {
                    scope_entry_list_t* member = get_member_of_class_type(declared_type, symbol, decl_context);

                    result = check_for_initializer_clause(initializer_clause, decl_context, 
                            entry_list_head(member)->type_information);
                    entry_list_free(member);

                    if (result)
                    {
                        expression_set_type(initializer, expression_get_type(initializer_clause));
                    }
                }
                else
                {
                    if (!checking_ambiguity())
                    {
                        fprintf(stderr, "%s: warning: gcc-style initializer clause but type is not a struct/union/class\n",
                                ast_location(initializer));
                    }
                }

                return result;
                break;
            }
    }

    internal_error("Code unreachable", 0);
    return 0;
}

static char check_for_initializer_list(AST initializer_list, decl_context_t decl_context, type_t* declared_type)
{
    char faulty = 0;
    if (initializer_list != NULL)
    {
        AST iter;

        for_each_element(initializer_list, iter)
        {
            AST initializer_clause = ASTSon1(iter);

            if (!check_for_initializer_clause(initializer_clause, decl_context, declared_type))
                faulty = 1;
        }
    }

    return !faulty;
}

static char operator_bin_pointer_to_pm_pred(type_t* lhs, type_t* rhs)
{
    if (is_pointer_to_class_type(no_ref(lhs))
            && is_pointer_to_member_type(no_ref(rhs))
            && class_type_is_base(pointer_type_get_pointee_type(no_ref(lhs)), 
                    pointer_to_member_type_get_class_type(no_ref(rhs))))
    {
        return 1;
    }

    return 0;
}

static type_t* operator_bin_pointer_to_pm_result(type_t** lhs, type_t** rhs)
{
    type_t* c1 = pointer_type_get_pointee_type(no_ref(*lhs));
    // type_t* c2 = pointer_to_member_type_get_class_type(no_ref(*rhs));
    type_t* t = pointer_type_get_pointee_type(no_ref(*rhs));

    // Union of both CV qualifiers
    cv_qualifier_t result_cv = (get_cv_qualifier(c1) | get_cv_qualifier(t));

    return get_lvalue_reference_type(get_cv_qualified_type(t, result_cv));
}

static char check_for_pointer_to_pointer_to_member(AST expression, decl_context_t decl_context)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    if (!check_for_expression(lhs, decl_context)
            || !check_for_expression(rhs, decl_context))
    {
        return 0;
    }

    type_t* lhs_type = expression_get_type(lhs);
    type_t* rhs_type = expression_get_type(rhs);

    if (lhs_type == NULL
            || rhs_type == NULL)
    {
        return 0;
    }

    if (is_dependent_expr_type(lhs_type)
            || is_dependent_expr_type(rhs_type))
    {
        expression_set_dependent(expression);
        return 1;
    }

    // This is an awkward operator, it requires overload if lhs is not a
    // pointer to a class or if rhs is a class type (and not a pointer to
    // member type)

    char requires_overload = 0;

    requires_overload = (!is_pointer_to_class_type(no_ref(lhs_type))
            || (!is_pointer_to_member_type(no_ref(rhs_type))
                && is_class_type(no_ref(rhs_type))));

    if (!requires_overload)
    {
        // This is redundant, but it won't hurt
        if (!is_pointer_to_member_type(no_ref(rhs_type))
                || !is_pointer_to_class_type(no_ref(lhs_type)))
        {
            return 0;
        }

        type_t* pm_class_type = 
            pointer_to_member_type_get_class_type(no_ref(rhs_type));

        type_t* pointed_lhs_type =
            pointer_type_get_pointee_type(no_ref(lhs_type));

        if (!equivalent_types(
                    get_actual_class_type(pm_class_type),
                    get_actual_class_type(pointed_lhs_type))
                 && !class_type_is_base(
                     get_actual_class_type(pointed_lhs_type),
                     get_actual_class_type(pm_class_type)))
        {
            return 0;
        }

        type_t* pm_pointed_type = 
            pointer_type_get_pointee_type(no_ref(rhs_type));

        cv_qualifier_t cv_qualif_object = CV_NONE;
        advance_over_typedefs_with_cv_qualif(pointed_lhs_type, &cv_qualif_object);

        cv_qualifier_t cv_qualif_pointer = CV_NONE;
        advance_over_typedefs_with_cv_qualif(no_ref(rhs_type), &cv_qualif_pointer);

        expression_set_type(expression, lvalue_ref(
                get_cv_qualified_type(
                    pm_pointed_type, 
                    cv_qualif_object | cv_qualif_pointer)));
        expression_set_is_lvalue(expression, 1);
        return 1;
    }

    // Solve the binary overload of 'operator->*'
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_POINTER_DERREF_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    builtin_operators_set_t builtin_set; 
    build_binary_builtin_operators(
            no_ref(lhs_type), no_ref(rhs_type), 
            &builtin_set,
            decl_context, operation_tree, 
            operator_bin_pointer_to_pm_pred,
            operator_bin_pointer_to_pm_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* computed_type = compute_user_defined_bin_operator_type(operation_tree, 
            expression, lhs, rhs, builtins, decl_context, &selected_operator);

    entry_list_free(builtins);

    if (computed_type != NULL)
        return 1;

    return 0;
}

static char check_for_pointer_to_member(AST expression, decl_context_t decl_context)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    if (!check_for_expression(lhs, decl_context)
            || !check_for_expression(rhs, decl_context))
    {
        return 0;
    }

    type_t* lhs_type = no_ref(expression_get_type(lhs));
    type_t* rhs_type = no_ref(expression_get_type(rhs));

    if (lhs_type == NULL
            || rhs_type == NULL)
    {
        return 0;
    }

    if (is_dependent_expr_type(lhs_type)
            || is_dependent_expr_type(rhs_type))
    {
        expression_set_dependent(expression);
        return 1;
    }

    if (!is_pointer_to_member_type(no_ref(rhs_type))
            || !is_class_type(no_ref(lhs_type)))
    {
        return 0;
    }

    type_t* pm_class_type = 
        pointer_to_member_type_get_class_type(no_ref(rhs_type));

    if (!equivalent_types(get_actual_class_type(no_ref(pm_class_type)), 
                get_actual_class_type(no_ref(lhs_type))) 
            && !class_type_is_base(get_actual_class_type(no_ref(lhs_type)), 
                get_actual_class_type(no_ref(pm_class_type))))
    {
        return 0;
    }

    cv_qualifier_t cv_qualif_object = CV_NONE;
    advance_over_typedefs_with_cv_qualif(no_ref(lhs_type), &cv_qualif_object);

    cv_qualifier_t cv_qualif_pointer = CV_NONE;
    advance_over_typedefs_with_cv_qualif(no_ref(rhs_type), &cv_qualif_pointer);

    expression_set_type(expression, lvalue_ref(
            get_cv_qualified_type(pointer_type_get_pointee_type(no_ref(rhs_type)), 
                cv_qualif_object | cv_qualif_pointer)));
    expression_set_is_lvalue(expression, 1);
    return 1;
}

static char check_for_parenthesized_initializer(AST initializer_list, decl_context_t decl_context, type_t* declared_type)
{
    if (ASTType(initializer_list) == AST_AMBIGUITY)
    {
        solve_ambiguous_expression_list(initializer_list, decl_context);
    }

    ERROR_CONDITION(ASTType(initializer_list) != AST_NODE_LIST,
            "Unknown node '%s' at '%s'\n",
            ast_print_node_type(ASTType(initializer_list)), ast_location(initializer_list));

    char is_class = is_class_type(declared_type);
    int initializer_num = 0;

    // Single initializer, used later for non-class cases
    AST single_initializer_expr = ASTSon1(initializer_list);

    type_t* argument_types[MAX_ARGUMENTS];

    char has_dependent_arguments = 0;

    AST iterator;
    for_each_element(initializer_list, iterator)
    {
        ERROR_CONDITION(initializer_num >= 256, "Too many arguments\n", 0);

        if (!checking_ambiguity()
                && !is_class 
                && !is_dependent_type(declared_type) 
                && (initializer_num > 0))
        {
            fprintf(stderr, "%s: warning: too many initializers in parenthesized initializer of non class type\n",
                    ast_location(initializer_list));
        }

        AST initializer = ASTSon1(iterator);

        if (!check_for_expression(initializer, decl_context))
        {
            return 0;
        }

        argument_types[initializer_num] = expression_get_type(initializer);

        if (is_dependent_expr_type(argument_types[initializer_num]))
        {
            has_dependent_arguments = 1;
        }

        initializer_num++;
    }

    if (has_dependent_arguments)
    {
        return 1;
    }

    if (!is_class)
    {
        // This is 'int a(10);' to be considered like 'int a = 10;'
        char ambiguous_conversion = 0;
        scope_entry_t* conversor = NULL;
        if (!is_dependent_type(declared_type)
                && !is_dependent_expr_type(argument_types[0])
                && !type_can_be_implicitly_converted_to(argument_types[0], declared_type, decl_context, 
                    &ambiguous_conversion, &conversor))
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: parenthesized initializer '%s' has type '%s' not convertible to '%s'\n",
                        ast_location(single_initializer_expr),
                        prettyprint_in_buffer(single_initializer_expr),
                        print_decl_type_str(argument_types[0], decl_context, ""),
                        print_decl_type_str(declared_type, decl_context, ""));
            }
            return 0;
        }

        if (conversor != NULL)
        {
            ASTAttrSetValueType(single_initializer_expr, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
            ASTAttrSetValueType(single_initializer_expr, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversor));
        }
    }
    else if (is_class && !is_dependent_type(declared_type))
    {
        scope_entry_t* conversors[MAX_ARGUMENTS];
        memset(conversors, 0, sizeof(conversors));

        scope_entry_list_t* candidates = NULL;
        scope_entry_t* constructor = solve_constructor(declared_type,
                argument_types,
                initializer_num,
                /* is_explicit */ 1,
                decl_context,
                ASTFileName(initializer_list), ASTLine(initializer_list),
                conversors,
                &candidates);
        entry_list_free(candidates);

        if (constructor == NULL)
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: no suitable constructor for type '%s'\n",
                        ast_location(initializer_list),
                        print_decl_type_str(declared_type, decl_context, ""));
            }
            return 0;
        }

        ensure_not_deleted(decl_context, initializer_list, constructor);

        int k = 0;
        for_each_element(initializer_list, iterator)
        {
            AST initializer = ASTSon1(iterator);

            if (conversors[k] != NULL)
            {
                ensure_not_deleted(decl_context, initializer, conversors[k]);

                ASTAttrSetValueType(initializer, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
                ASTAttrSetValueType(initializer, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(conversors[k]));
            }
            k++;
        }
    }

    return 1;
}

char check_for_initialization(AST initializer, decl_context_t decl_context, type_t* declared_type)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Checking initializer '%s'\n",
                prettyprint_in_buffer(initializer));
    }

    char result = 0;

    switch (ASTType(initializer))
    {
        case AST_EQUAL_INITIALIZER:
            {
                AST initializer_clause = ASTSon0(initializer);
                result = check_for_initializer_clause(initializer_clause, decl_context, declared_type);

                if (result)
                {
                    ASTAttrSetValueType(initializer, LANG_IS_EXPRESSION_NEST, tl_type_t, tl_bool(1));
                    ASTAttrSetValueType(initializer, LANG_EXPRESSION_NESTED, tl_type_t, tl_ast(initializer_clause));

                    expression_set_type(initializer, expression_get_type(initializer_clause));

                    expression_set_constant(initializer, expression_get_constant(initializer_clause));

                    expression_set_is_value_dependent(initializer, expression_is_value_dependent(initializer_clause));
                }
                break;
            }
        case AST_INITIALIZER_BRACES:
            {
                return check_for_braced_initializer_list(initializer, decl_context, declared_type);
            }
        case AST_PARENTHESIZED_INITIALIZER :
            {
                AST parenthesized_initializer = ASTSon0(initializer);
                result = check_for_parenthesized_initializer(parenthesized_initializer, decl_context, declared_type);

                if (result)
                {
                    expression_set_type(initializer, expression_get_type(parenthesized_initializer));
                    expression_set_is_lvalue(initializer, 0);

                    expression_set_constant(initializer,
                            expression_get_constant(parenthesized_initializer));

                    expression_set_is_value_dependent(initializer,
                            expression_is_value_dependent(parenthesized_initializer));
                }
                break;
            }
        default:
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(initializer)));
            }
    }

    DEBUG_CODE()
    {
        if (result)
        {
            fprintf(stderr, "EXPRTYPE: Initializer '%s' has type '%s'",
                    prettyprint_in_buffer(initializer),
                    expression_get_type(initializer) == NULL 
                    ? "<< no type >>" 
                    : print_declarator(expression_get_type(initializer)));

            if (expression_is_constant(initializer))
            {
                const_value_t* v = expression_get_constant(initializer);
                fprintf(stderr, " with a constant value of ");
                if (const_value_is_signed(v))
                {
                    fprintf(stderr, " '%lld'", (long long int)const_value_cast_to_8(v));
                }
                else
                {
                    fprintf(stderr, " '%llu'", (unsigned long long int)const_value_cast_to_8(v));
                }
            }
            fprintf(stderr, "\n");
        }
        else
        {
            fprintf(stderr, "EXPRTYPE: Initializer '%s' does not have any computed type\n",
                    prettyprint_in_buffer(initializer));
        }
    }

    if (CURRENT_CONFIGURATION->strict_typecheck
            && !result)
    {
        internal_error("Initializer '%s' at '%s' does not have a valid computed type\n",
                prettyprint_in_buffer(initializer),
                ast_location(initializer));
    }

    return result;
}

AST advance_expression_nest(AST expr)
{
    return advance_expression_nest_flags(expr, 1);
}

AST advance_expression_nest_flags(AST expr, char advance_parentheses)
{
    AST result = expr;

    for ( ; ; )
    {
        switch (ASTType(result))
        {
            case AST_EXPRESSION : 
            case AST_CONSTANT_EXPRESSION : 
                {
                    result = ASTSon0(result);
                    break;
                }
            case AST_PARENTHESIZED_EXPRESSION :
                {
                    if (advance_parentheses)
                    {
                        result = ASTSon0(result);
                    }
                    else
                    {
                        return result;
                    }
                    break;
                }
            default:
                return result;
        }
    }
}

static void accessible_types_through_conversion(type_t* t, type_t ***result, int *num_types, decl_context_t decl_context)
{
    ERROR_CONDITION(is_unresolved_overloaded_type(t), 
            "Do not invoke this function on unresolved overloaded types", 0);
    ERROR_CONDITION(is_dependent_type(t), "Do not invoke this function on dependent types", 0);

    (*num_types) = 0;

    ERROR_CONDITION(is_lvalue_reference_type(t), "Reference types should have been removed here", 0);

    if (is_enum_type(t))
    {
        P_LIST_ADD(*result, *num_types, enum_type_get_underlying_type(t));
        return;
    }
    else if (is_class_type(t))
    {
        type_t* class_type = get_actual_class_type(t);
        if (is_named_class_type(t)
                && class_type_is_incomplete_independent(class_type))
        {
            scope_entry_t* symbol = named_type_get_symbol(t);
            instantiate_template_class(symbol, decl_context, 
                    // FIXME - Locus was lost here!
                    0, 0);
        }

        scope_entry_list_t* conversion_list = class_type_get_all_conversions(class_type, decl_context);

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(conversion_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t *conversion = entry_list_iterator_current(it);

            if (!is_template_specialized_type(conversion->type_information))
            {
                type_t* destination_type = function_type_get_return_type(conversion->type_information);

                // The implicit parameter of this operator function is a reference
                // to the class type, this will filter not eligible conversion functions
                // (e.g. given a 'const T' we cannot call a non-const method)
                type_t* implicit_parameter = conversion->entity_specs.class_type;
                if (is_const_qualified_type(conversion->type_information))
                {
                    implicit_parameter = get_cv_qualified_type(implicit_parameter, CV_CONST);
                }
                implicit_parameter = get_lvalue_reference_type(implicit_parameter);

                standard_conversion_t first_sc;
                if (standard_conversion_between_types(&first_sc, get_lvalue_reference_type(t), 
                            implicit_parameter))
                {
                    P_LIST_ADD_ONCE(*result, *num_types, destination_type);
                }
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(conversion_list);
    }
}

static
void build_unary_builtin_operators(type_t* t1,
        builtin_operators_set_t *result,
        decl_context_t decl_context, AST operator, 
        char (*property)(type_t*),
        type_t* (*result_type)(type_t**))
{
    type_t** accessibles_1 = NULL;
    int num_accessibles_1 = 0;

    if (!is_unresolved_overloaded_type(no_ref(t1)))
    {
        accessible_types_through_conversion(no_ref(t1), &accessibles_1, &num_accessibles_1, decl_context);
        // Add ourselves because we might be things like 'int&'
        // or 'T*&'
        P_LIST_ADD(accessibles_1, num_accessibles_1, t1);
    }

    memset(result, 0, sizeof(*result));

    int i;
    for (i = 0; i < num_accessibles_1; i++)
    {
        type_t* accessible_from_t1 = accessibles_1[i];
        if (property(accessible_from_t1))
        {
            int num_parameters = 1;

            type_t* adjusted_t1 = accessible_from_t1;

            type_t* function_result_type = result_type(&adjusted_t1);

            ERROR_CONDITION(function_result_type == NULL, "This type cannot be NULL!", 0);

            parameter_info_t parameters[1] =
            {
                { 
                    .is_ellipsis = 0,
                    .type_info = adjusted_t1,
                    .nonadjusted_type_info = NULL,
                },
            };

            type_t* function_type = get_new_function_type(function_result_type, parameters, num_parameters);

            // If this type is already in the type set, do not add it
            char found = 0;

            {
                int k;
                for (k = 0; (k < (*result).num_builtins) && !found; k++)
                {
                    scope_entry_t* sym = &((*result).entry[k]);
                    type_t* builtin_function_type = sym->type_information;

                    found = (equivalent_types(function_type, builtin_function_type));
                }
            }

            if (!found)
            {
                // Fill the minimum needed for this 'faked' function symbol
                (*result).entry[(*result).num_builtins].kind = SK_FUNCTION;
                (*result).entry[(*result).num_builtins].symbol_name = get_operator_function_name(operator);
                (*result).entry[(*result).num_builtins].entity_specs.is_builtin = 1;
                (*result).entry[(*result).num_builtins].type_information = function_type;
                (*result).entry[(*result).num_builtins].decl_context = decl_context;

                // Add to the results and properly chain things
                (*result).entry_list = entry_list_add((*result).entry_list, &((*result).entry[(*result).num_builtins]));
                (*result).num_builtins++;
            }
        }
    }
}

static
void build_binary_builtin_operators(type_t* t1, 
        type_t* t2, 
        builtin_operators_set_t *result,
        decl_context_t decl_context, AST operator, 
        char (*property)(type_t*, type_t*),
        type_t* (*result_type)(type_t**, type_t**))
{
    type_t** accessibles_1 = NULL;
    int num_accessibles_1 = 0;

    type_t** accessibles_2 = NULL;
    int num_accessibles_2 = 0;

    if (!is_unresolved_overloaded_type(no_ref(t1)))
    {
        accessible_types_through_conversion(no_ref(t1), &accessibles_1, &num_accessibles_1, decl_context);
        P_LIST_ADD(accessibles_1, num_accessibles_1, t1);
    }

    if (!is_unresolved_overloaded_type(no_ref(t2)))
    {
        accessible_types_through_conversion(no_ref(t2), &accessibles_2, &num_accessibles_2, decl_context);
        P_LIST_ADD(accessibles_2, num_accessibles_2, t2);
    }

    memset(result, 0, sizeof(*result));

    int i;
    for (i = 0; i < num_accessibles_1; i++)
    {
        type_t* accessible_from_t1 = accessibles_1[i];
        int j;
        for (j = 0; j < num_accessibles_2; j++)
        {
            type_t* accessible_from_t2 = accessibles_2[j];

            if (property(accessible_from_t1, accessible_from_t2))
            {
                int num_parameters = 2;

                type_t* adjusted_t1 = accessible_from_t1;
                type_t* adjusted_t2 = accessible_from_t2;

                type_t* function_result_type = result_type(&adjusted_t1, &adjusted_t2);

                ERROR_CONDITION(function_result_type == NULL, "This type cannot be NULL!", 0);
                
                parameter_info_t parameters[2] =
                {
                    { 
                        .is_ellipsis = 0,
                        .type_info = adjusted_t1,
                        .nonadjusted_type_info = NULL
                    },
                    {
                        .is_ellipsis = 0,
                        .type_info = adjusted_t2,
                        .nonadjusted_type_info = NULL
                    }
                };

                type_t* function_type = get_new_function_type(function_result_type, parameters, num_parameters);

                // If this type is already in the type set, do not add it
                char found = 0;

                {
                    int k;
                    for (k = 0; (k < (*result).num_builtins) && !found; k++)
                    {
                        scope_entry_t* sym = &((*result).entry[k]);
                        type_t* builtin_function_type = sym->type_information;

                        found = (equivalent_types(function_type, builtin_function_type));
                    }
                }

                if (!found)
                {
                    // Fill the minimum needed for this 'faked' function symbol
                    (*result).entry[(*result).num_builtins].kind = SK_FUNCTION;
                    (*result).entry[(*result).num_builtins].symbol_name = get_operator_function_name(operator);
                    (*result).entry[(*result).num_builtins].entity_specs.is_builtin = 1;
                    (*result).entry[(*result).num_builtins].type_information = function_type;
                    (*result).entry[(*result).num_builtins].decl_context = decl_context;

                    DEBUG_CODE()
                    {
                        fprintf(stderr, "EXPRTYPE: Generated builtin '%s' for '%s'\n",
                                print_declarator((*result).entry[(*result).num_builtins].type_information),
                                (*result).entry[(*result).num_builtins].symbol_name);
                    }

                    // Add to the results and properly chain things
                    (*result).entry_list = entry_list_add((*result).entry_list, &((*result).entry[(*result).num_builtins]) );
                    (*result).num_builtins++;
                }
            }
        }
    }
}

// All this is just for conditional expressions (ternary 'operator ?')
static
void build_ternary_builtin_operators(type_t* t1, 
        type_t* t2, 
        type_t* t3, 
        builtin_operators_set_t *result,
        // Note that since no ternary operator actually exists we use a faked name
        decl_context_t decl_context, char* operator_name, 
        char (*property)(type_t*, type_t*, type_t*),
        type_t* (*result_type)(type_t**, type_t**, type_t**))
{
    type_t** accessibles_1 = NULL;
    int num_accessibles_1 = 0;

    type_t** accessibles_2 = NULL;
    int num_accessibles_2 = 0;

    type_t** accessibles_3 = NULL;
    int num_accessibles_3 = 0;

    if (!is_unresolved_overloaded_type(no_ref(t1)))
    {
        accessible_types_through_conversion(no_ref(t1), &accessibles_1, &num_accessibles_1, decl_context);
        P_LIST_ADD(accessibles_1, num_accessibles_1, t1);
    }

    if (!is_unresolved_overloaded_type(no_ref(t2)))
    {
        accessible_types_through_conversion(no_ref(t2), &accessibles_2, &num_accessibles_2, decl_context);
        P_LIST_ADD(accessibles_2, num_accessibles_2, t2);
    }

    if (!is_unresolved_overloaded_type(no_ref(t3)))
    {
        accessible_types_through_conversion(no_ref(t3), &accessibles_3, &num_accessibles_3, decl_context);
        P_LIST_ADD(accessibles_3, num_accessibles_3, t3);
    }

    memset(result, 0, sizeof(*result));

    int i;
    for (i = 0; i < num_accessibles_1; i++)
    {
        type_t* accessible_from_t1 = accessibles_1[i];
        int j;
        for (j = 0; j < num_accessibles_2; j++)
        {
            type_t* accessible_from_t2 = accessibles_2[j];

            int k;
            for (k = 0; k < num_accessibles_3; k++)
            {
                type_t* accessible_from_t3 = accessibles_3[k];

                if (property(accessible_from_t1, 
                            accessible_from_t2, 
                            accessible_from_t3))
                {
                    int num_parameters = 3;

                    type_t* adjusted_t1 = accessible_from_t1;
                    type_t* adjusted_t2 = accessible_from_t2;
                    type_t* adjusted_t3 = accessible_from_t3;

                    type_t* function_result_type = result_type(&adjusted_t1, &adjusted_t2, &adjusted_t3);

                    ERROR_CONDITION(function_result_type == NULL, "This type cannot be NULL!", 0);

                    parameter_info_t parameters[3] =
                    {
                        { 
                            .is_ellipsis = 0,
                            .type_info = adjusted_t1,
                            .nonadjusted_type_info = NULL,
                        },
                        {
                            .is_ellipsis = 0,
                            .type_info = adjusted_t2,
                            .nonadjusted_type_info = NULL,
                        },
                        {
                            .is_ellipsis = 0,
                            .type_info = adjusted_t3,
                            .nonadjusted_type_info = NULL,
                        }
                    };

                    type_t* function_type = get_new_function_type(function_result_type, parameters, num_parameters);

                    // If this type is already in the type set, do not add it
                    char found = 0;

                    {
                        int m;
                        for (m = 0; (m < (*result).num_builtins) && !found; m++)
                        {
                            scope_entry_t* sym = &((*result).entry[m]);
                            type_t* builtin_function_type = sym->type_information;

                            found = (equivalent_types(function_type, builtin_function_type));
                        }
                    }

                    if (!found)
                    {
                        // Fill the minimum needed for this 'faked' function symbol
                        (*result).entry[(*result).num_builtins].kind = SK_FUNCTION;
                        (*result).entry[(*result).num_builtins].symbol_name = operator_name;
                        (*result).entry[(*result).num_builtins].entity_specs.is_builtin = 1;
                        (*result).entry[(*result).num_builtins].type_information = function_type;

                        DEBUG_CODE()
                        {
                            fprintf(stderr, "EXPRTYPE: Generated builtin '%s' for '%s'\n",
                                    print_declarator((*result).entry[(*result).num_builtins].type_information),
                                    (*result).entry[(*result).num_builtins].symbol_name);
                        }

                        // Add to the results and properly chain things
                        (*result).entry_list = entry_list_add((*result).entry_list, &((*result).entry[(*result).num_builtins]));
                        (*result).num_builtins++;
                    }
                }
            }
        }
    }
}

static char check_for_sizeof_expr(AST expr, decl_context_t decl_context)
{
    AST sizeof_expression = ASTSon0(expr);
    if (check_for_expression(sizeof_expression, decl_context))
    {
        type_t* t = expression_get_type(sizeof_expression);

        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            if (!is_dependent_expr_type(t)
                    && !type_is_runtime_sized(t))
            {
                CXX_LANGUAGE()
                {
                    if (is_named_class_type(t)
                            && class_type_is_incomplete_independent(get_actual_class_type(t)))
                    {
                        scope_entry_t* symbol = named_type_get_symbol(t);
                        instantiate_template_class(symbol, decl_context, 
                                ASTFileName(sizeof_expression), ASTLine(sizeof_expression));
                    }
                }

                if (is_incomplete_type(t))
                {
                    running_error("%s: error: sizeof of incomplete type '%s'\n", 
                            ast_location(sizeof_expression),
                            print_type_str(t, decl_context));
                }

                char is_const_expr = 0;
                _size_t type_size = 0;

                switch (ASTType(expr))
                {
                    case AST_SIZEOF:
                        {
                            is_const_expr = 1;
                            type_size = type_get_size(t);
                            break;
                        }
                    case AST_GCC_ALIGNOF:
                        {
                            is_const_expr = 1;
                            type_size = type_get_alignment(t);
                            break;
                        }
                    default: 
                        ;
                }


                if (is_const_expr)
                {
                    expression_set_constant(expr, 
                            const_value_get(type_size, /*bytes*/ 8, /* sign*/ 0));
                }

                DEBUG_SIZEOF_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: %s: '%s' yields a value of %zu\n",
                            ast_location(expr),
                            prettyprint_in_buffer(expr),
                            type_size);
                }
            }
            else if (is_dependent_expr_type(t))
            {
                expression_set_is_value_dependent(expr, 1);
            }
        }

        expression_set_type(expr, get_size_t_type());
        expression_set_is_lvalue(expr, 0);
        return 1;
    }

    return 0;
}

static char check_for_sizeof_typeid(AST expr, decl_context_t decl_context)
{
    AST type_id = ASTSon0(expr);
    if (check_for_type_id_tree(type_id, decl_context))
    {
        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            AST type_specifier = ASTSon0(type_id);
            AST abstract_declarator = ASTSon1(type_id);

            gather_decl_spec_t gather_info;
            memset(&gather_info, 0, sizeof(gather_info));

            type_t* simple_type_info = NULL;
            build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
                    decl_context);

            type_t* declarator_type = simple_type_info;
            compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
                    &declarator_type, decl_context);

            if (!is_dependent_type(declarator_type)
                    && !type_is_runtime_sized(declarator_type))
            {
                CXX_LANGUAGE()
                {
                    if (is_named_class_type(declarator_type)
                            && class_type_is_incomplete_independent(get_actual_class_type(declarator_type)))
                    {
                        scope_entry_t* symbol = named_type_get_symbol(declarator_type);
                        instantiate_template_class(symbol, decl_context, 
                                ASTFileName(type_id), ASTLine(type_id));
                    }
                }

                if (is_incomplete_type(declarator_type))
                {
                    running_error("%s: error: sizeof of incomplete type '%s'\n", 
                            ast_location(type_id),
                            print_type_str(declarator_type, decl_context));
                }

                char is_const_expr = 0;
                _size_t type_size = 0;

                switch (ASTType(expr))
                {
                    case AST_SIZEOF_TYPEID:
                        {
                            is_const_expr = 1;
                            type_size = type_get_size(declarator_type);
                            break;
                        }
                    case AST_GCC_ALIGNOF_TYPE:
                        {
                            is_const_expr = 1;
                            type_size = type_get_alignment(declarator_type);
                            break;
                        }
                     default:
                        ;
                }

                if (is_const_expr)
                {
                    expression_set_constant(
                            expr, const_value_get(type_size, /* bytes */ 8, 0));
                }

                DEBUG_SIZEOF_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: %s: '%s' yields a value of %zu\n",
                            ast_location(expr),
                            prettyprint_in_buffer(expr),
                            type_size);
                }
            }
            else if (is_dependent_type(declarator_type))
            {
                expression_set_is_value_dependent(expr, 1);
            }

            // Set the type of the type_id
            expression_set_type(type_id, declarator_type);
            expression_set_is_lvalue(type_id, 0);
        }

        expression_set_type(expr, get_size_t_type());
        expression_set_is_lvalue(expr, 0);
        return 1;
    }
    return 0;
}

static char check_for_pseudo_destructor_call(AST expression, decl_context_t decl_context)
{
    char is_arrow = 0;
    if (ASTType(expression) == AST_POINTER_PSEUDO_DESTRUCTOR_CALL)
    {
        is_arrow = 1;
    }

    AST postfix_expression = ASTSon0(expression);
    
    char postfix_check = check_for_expression(postfix_expression, decl_context);
    if (!postfix_check
            || expression_get_type(postfix_expression) == NULL)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: postfix expression '%s' of pseudo-destructor call does not seem valid\n",
                    prettyprint_in_buffer(postfix_expression));
        }
        return 0;
    }

    type_t* considered_type = expression_get_type(postfix_expression);

    if (is_dependent_expr_type(no_ref(considered_type)))
    {
        expression_set_dependent(expression);
        return 1;
    }

    if (is_arrow)
    {
        if (!is_pointer_type(no_ref(expression_get_type(postfix_expression))))
        {
            return 0;
        }

        considered_type = pointer_type_get_pointee_type(no_ref(expression_get_type(postfix_expression)));
    }

    if (!is_scalar_type(no_ref(considered_type)))
    {
        return 0;
    }

    AST pseudo_destructor_name = ASTSon1(expression);

    AST pseudo_destructor_id_expression = ASTSon0(pseudo_destructor_name);
    AST destructor_id = ASTSon1(pseudo_destructor_name);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, pseudo_destructor_id_expression);

    if (entry_list == NULL)
    {
        running_error("%s: error: pseudo-destructor '%s' not found", 
                ast_location(pseudo_destructor_name),
                prettyprint_in_buffer(pseudo_destructor_name));
    }

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        if (entry->kind != SK_ENUM 
                && entry->kind != SK_CLASS 
                && entry->kind != SK_TYPEDEF 
                && entry->kind != SK_TEMPLATE
                && entry->kind != SK_TEMPLATE_TYPE_PARAMETER
                && entry->kind != SK_TEMPLATE_TEMPLATE_PARAMETER
                && entry->kind != SK_GCC_BUILTIN_TYPE)
        {
            running_error("%s: error: pseudo-destructor '%s' does not name a type", 
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }
    }
    entry_list_iterator_free(it);

    scope_entry_t * entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    // Checking that both T's are the same entity in '{[nested-name-specifier] T}::~T'

    decl_context_t destructor_lookup = decl_context;
    // If the name is qualified the lookup is performed within the scope of the
    // qualified type-name
    if (ASTSon0(pseudo_destructor_id_expression) != NULL
            || ASTSon1(pseudo_destructor_id_expression) != NULL)
    {
        destructor_lookup = entry->decl_context;
    }

    if (entry->kind == SK_TYPEDEF)
    {
        type_t* advanced_type = advance_over_typedefs(entry->type_information);

        if (is_named_type(advanced_type))
        {
            entry = named_type_get_symbol(advanced_type);
        }
    }

    // We'll want to find the injected class name for class names since it
    // simplifies lots of things related with templates
    if (entry->kind == SK_CLASS)
    {
        // But this requires an instantiated class
        if (class_type_is_incomplete_independent(entry->type_information))
        {
            instantiate_template_class(entry, decl_context, 
                    ASTFileName(pseudo_destructor_id_expression), 
                    ASTLine(pseudo_destructor_id_expression));
        }

        destructor_lookup = class_type_get_inner_context(entry->type_information);
    }

    if (ASTType(destructor_id) == AST_DESTRUCTOR_ID)
    {
        const char * type_name = ASTText(ASTSon0(destructor_id));
        type_name++; // Ignore '~'

        scope_entry_list_t *new_name = query_unqualified_name_str(destructor_lookup,
                type_name);

        if (new_name == NULL)
        {
            running_error("%s: error: pseudo-destructor '%s' does not refer to any type-name",
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }

        scope_entry_t* new_name_entry = entry_list_head(new_name);
        entry_list_free(new_name);

        if (!equivalent_types(get_user_defined_type(new_name_entry), 
                    get_user_defined_type(entry)))
        {
            running_error("%s: error: pseudo-destructor '%s' does not match the type-name",
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }
    }
    else if (ASTType(destructor_id) == AST_DESTRUCTOR_TEMPLATE_ID)
    {
        // This must be a class
        if (entry->kind != SK_CLASS)
        {
            running_error("%s: error: pseudo-destructor '%s' does not refer to a class",
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }

        AST template_id = ASTSon0(destructor_id);

        scope_entry_list_t *new_name = query_id_expression(destructor_lookup,
                template_id);

        if (new_name == NULL)
        {
            running_error("%s: error: pseudo-destructor template-id '%s' does not refer to any type-name",
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }

        scope_entry_t* new_name_entry = entry_list_head(new_name);
        entry_list_free(new_name);

        if (!equivalent_types(get_user_defined_type(entry), 
                    get_user_defined_type(new_name_entry)))
        {
            running_error("%s: error: pseudo-destructor template-id '%s' does not match the type-name",
                    ast_location(pseudo_destructor_name),
                    prettyprint_in_buffer(pseudo_destructor_name));
        }
    }

    // Everything seems right
    expression_set_type(expression, get_pseudo_destructor_call_type());
    return 1;
}

static char check_for_gcc_builtin_offsetof(AST expression, decl_context_t decl_context)
{
    // We are not checking that the designated member is correct
    AST type_id = ASTSon0(expression);
    AST member_designator = ASTSon1(expression);

    char result = check_for_type_id_tree(type_id, decl_context);

    if (!result)
        return 0;

    // Remove ambiguities
    AST designator_list = ASTSon1(member_designator);
    if (designator_list != NULL)
    {
        AST iter;
        for_each_element(designator_list, iter)
        {
            AST designator = ASTSon1(iter);

            if (ASTType(designator) == AST_INDEX_DESIGNATOR)
            {
                AST constant_expr = ASTSon0(designator);
                if (!check_for_expression(constant_expr, decl_context))
                {
                    return 0;
                }
            }
        }
    }

    // We need to compute the size of the type, get the member and then retrieve its offset
    type_t* type = compute_type_for_type_id_tree(type_id, decl_context);
    ERROR_CONDITION(type == NULL, "Invalid type computed for '%s'", 
            prettyprint_in_buffer(type_id));

    if (!is_dependent_type(type))
    {
        // Compute its size (this will compute the layout)
        type_get_size(type);

        // The tree of offsetof is a bit awkward, let's make our life easier
#define MAX_DESIGNATORS 64
        int num_designators = 0;
        AST designators[MAX_DESIGNATORS];

        // Initial identifier
        designators[num_designators] = ASTSon0(member_designator);
        num_designators++;
        if (designator_list != NULL)
        {
            AST iter;
            for_each_element(designator_list, iter)
            {
                if (num_designators == MAX_DESIGNATORS)
                {
                    internal_error("Too many designators", 0);
                }
                designators[num_designators] = ASTSon1(iter);
                num_designators++;
            }
        }
#undef MAX_DESIGNATORS

        _size_t computed_offset = 0;

        type_t* current_type = type;
        int i;
        for (i = 0; i < num_designators; i++)
        {
            if (is_dependent_type(type))
            {
                // Give up
                expression_set_is_value_dependent(expression, 1);
                break;
            }

            AST name = designators[i];
            if (ASTType(name) == AST_SYMBOL)
            {
                // Do nothing
            }
            else if (ASTType(name) == AST_FIELD_DESIGNATOR)
            {
                // This is .name
                name = ASTSon0(name);
            }
            else if (ASTType(name) == AST_INDEX_DESIGNATOR)
            {
                // This is [constant-expression]
                // No lookup for this type, just check that this is an array
                if (!is_array_type(current_type))
                {
                    running_error("%s: error: invalid designator '%s' for non-array type '%s'\n",
                            ast_location(expression),
                            prettyprint_in_buffer(name),
                            print_type_str(current_type, decl_context));
                }

                AST const_expr = ASTSon0(name);
                if (expression_is_value_dependent(const_expr))
                {
                    // Give up
                    expression_set_is_value_dependent(expression, 1);
                    break;
                }

                if (!expression_is_constant(const_expr))
                {
                    running_error("%s: error: expression '%s' should be constant\n",
                            ast_location(const_expr),
                            prettyprint_in_buffer(const_expr));
                }

                current_type = array_type_get_element_type(current_type);
                computed_offset += const_value_cast_to_8(expression_get_constant(const_expr)) * type_get_size(current_type);
                continue;
            }
            else
            {
                internal_error("Code unreachable", 0);
            }

            if (!is_class_type(current_type))
            {
                running_error("%s: error: __builtin_offsetof applied to '%s' which is not struct/class/union\n",
                        ast_location(expression),
                        print_type_str(current_type, decl_context));
            }

            scope_entry_list_t* member_list = get_member_of_class_type(current_type, name, decl_context);
            if (member_list == NULL)
            {
                running_error("%s: error: '%s' is not a member of type '%s'\n",
                        ast_location(expression),
                        prettyprint_in_buffer(name),
                        print_type_str(current_type, decl_context));
            }

            scope_entry_t* member = entry_list_head(member_list);
            if (entry_list_size(member_list) > 1
                    || member->kind != SK_VARIABLE)
            {
                running_error("%s: error: '%s' is not a valid member for __builtin_offsetof\n", 
                        ast_location(expression),
                        prettyprint_in_buffer(name));
            }

            entry_list_free(member_list);

            computed_offset += member->entity_specs.field_offset;
            current_type = member->type_information;
        }

        expression_set_constant(expression, 
                const_value_get(computed_offset, type_get_size(get_size_t_type()), /* signed */ 0));
    }
    else
    {
        // If the type is dependent, any offset should be considered as value
        // dependent even if the type of the whole expression is size_t
        expression_set_is_value_dependent(expression, 1);
    }

    expression_set_type(expression, get_size_t_type());
    return 1;
}

static char check_for_gcc_builtin_choose_expr(AST expression, decl_context_t decl_context)
{
    AST selector_expr = ASTSon0(expression);
    AST first_expr = ASTSon1(expression);
    AST second_expr = ASTSon2(expression);

    // Since the exact type of this expression depends on the value yield by selector_expr
    // we will check the selector_expr and then evaluate it. 
    //
    // Note that this is only valid for C so we do not need to check
    // whether the expression is dependent

    if (!check_for_expression(selector_expr, decl_context))
        return 0;

    if (!expression_is_constant(selector_expr))
        return 0;

    AST selected_expr = NULL;
    if (const_value_is_nonzero(expression_get_constant(selector_expr)))
    {
        if (!check_for_expression(first_expr, decl_context))
            return 0;

        selected_expr = first_expr;
    }
    else
    {
        if (!check_for_expression(second_expr, decl_context))
            return 0;

        selected_expr = second_expr;
    }

    expression_set_type(expression, expression_get_type(selected_expr));
    expression_set_is_lvalue(expression, expression_is_lvalue(selected_expr));
    expression_set_constant(expression, expression_get_constant(selected_expr));
    return 1;
}


static char check_for_gcc_builtin_types_compatible_p(AST expression, decl_context_t decl_context)
{
    // This builtin always returns an integer type
    AST first_type_tree = ASTSon0(expression);
    AST second_type_tree = ASTSon1(expression);

    if (!check_for_type_id_tree(first_type_tree, decl_context)
            || !check_for_type_id_tree(second_type_tree, decl_context))
        return 0;

    type_t* first_type = compute_type_for_type_id_tree(first_type_tree, decl_context);
    type_t* second_type = compute_type_for_type_id_tree(second_type_tree, decl_context);

    if (!is_dependent_type(first_type)
            && !is_dependent_type(second_type))
    {
        expression_set_constant(
                expression,
                equivalent_types(first_type, second_type) ?  
                const_value_get_one(/*bytes*/ 1, /*signed*/ 0) 
                : const_value_get_zero(/*bytes*/ 1,  /*signed*/ 0));
    }
    else
    {
        expression_set_is_value_dependent(expression, 1);
    }

    expression_set_type(expression, get_signed_int_type());
    expression_set_is_lvalue(expression, 0);
    return 1;
}

static char check_for_array_section_expression(AST expression, decl_context_t decl_context)
{
    // At the moment there is not a specific type backing sections
    // up. So what we do is just check that the indexed entity is
    // an array check the two bound expressions and then bypass the
    // computed entity type as if it was a normal array
    //
    // int *b, a[10];
    //
    // a[1:10] = 3;
    // b[1:10] = 4;
    //
    // a[1:10] will have int type
    // b[1:10] will have int type too
    // 
    // (so, they do not have a special 'array section type of int' or something)
    //
    // For C++, no overloading is considered here.
    //
    // Note: 
    // A proper check of 
    //
    //    a[1:2] = b[2:3]
    //
    // would require having a section type
    
    AST postfix_expression = ASTSon0(expression);
    AST lower_bound = ASTSon1(expression);
    AST upper_bound = ASTSon2(expression);

    char postfix_check = check_for_expression(postfix_expression, decl_context);
    char lower_bound_check = check_for_expression(lower_bound, decl_context);
    char upper_bound_check = check_for_expression(upper_bound, decl_context);

    if (!postfix_check
            || !lower_bound_check
            || !upper_bound_check)
        return 0;
    
    type_t* indexed_type = no_ref(expression_get_type(postfix_expression));

    type_t* result_type = NULL;

    if (is_array_type(indexed_type))
    {
        result_type = lvalue_ref(array_type_get_element_type(indexed_type));
    }
    else if (is_pointer_type(indexed_type))
    {
        result_type = lvalue_ref(pointer_type_get_pointee_type(indexed_type));
    }
    else
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: array section '%s' is invalid since '%s' has type '%s'\n",
                    ast_location(expression),
                    prettyprint_in_buffer(expression),
                    prettyprint_in_buffer(postfix_expression),
                    print_type_str(indexed_type, decl_context));
        }
        return 0;
    }

    // This should be deemed always as a lvalue
    expression_set_is_lvalue(expression, 1);
    expression_set_type(expression, result_type);

    return 1;
}

static char check_for_shaping_expression(AST expression, decl_context_t decl_context)
{
    char result = 1;
    AST shaped_expr = ASTSon1(expression);
    AST shape_list = ASTSon0(expression);

    if (!check_for_expression(shaped_expr, decl_context))
    {
        result = 0;
    }

    if (!check_for_expression_list(shape_list, decl_context))
    {
        result = 0;
    }

    AST it;
    for_each_element(shape_list, it)
    {
        AST current_expr = ASTSon1(it);
        type_t *current_expr_type = expression_get_type(current_expr);

        standard_conversion_t scs;
        if (!standard_conversion_between_types(&scs, no_ref(current_expr_type), get_signed_int_type()))
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: shaping expression '%s' cannot be converted to 'int'\n",
                        ast_location(current_expr),
                        prettyprint_in_buffer(current_expr));
            }
            result = 0;
        }
    }

    // Now check the shape makes sense
    type_t* shaped_expr_type = expression_get_type(shaped_expr);

    if (!is_pointer_type(no_ref(shaped_expr_type)))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: shaped expression '%s' does not have pointer type\n",
                    ast_location(shaped_expr),
                    prettyprint_in_buffer(shaped_expr));
        }
        result = 0;
    }

    if (is_void_pointer_type(no_ref(shaped_expr_type)))
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: shaped expression '%s' has type 'void*' which is invalid\n",
                    ast_location(shaped_expr),
                    prettyprint_in_buffer(shaped_expr));
        }
        result = 0;
    }

    if (result)
    {
        // Synthesize a new type based on what we got
        type_t* result_type = pointer_type_get_pointee_type(no_ref(shaped_expr_type));

        it = shape_list;
        // Traverse the list backwards
        while (it != NULL)
        {
            AST current_expr = ASTSon1(it);
            result_type = get_array_type(result_type, current_expr, decl_context);
            it = ASTSon0(it);
        }

        expression_set_type(expression, result_type);
        expression_set_is_lvalue(expression, 1);
    }

    return result;
}


char check_for_expression_list(AST expression_list, decl_context_t decl_context)
{
    if (expression_list == NULL)
    {
        // An empty list is right
        return 1;
    }
    else if (ASTType(expression_list) == AST_AMBIGUITY)
    {
        int correct_choice = -1;
        int i;
        char result = 1;
        for (i = 0; i < ast_get_num_ambiguities(expression_list); i++)
        {
            AST current_expression_list = ast_get_ambiguity(expression_list, i);

            enter_test_expression();
            result &= check_for_expression_list(current_expression_list, decl_context);
            leave_test_expression();

            if (result)
            {
                if (correct_choice < 0)
                {
                    correct_choice = i;
                }
                else
                {
                    AST previous_choice = ast_get_ambiguity(expression_list, correct_choice);
                    AST current_choice = ast_get_ambiguity(expression_list, i);
                    internal_error("More than one valid alternative '%s' vs '%s'", 
                            ast_print_node_type(ASTType(previous_choice)),
                            ast_print_node_type(ASTType(current_choice)));
                }
            }
        }

        if (!(correct_choice < 0))
        {
            ast_replace_with_ambiguity(expression_list, correct_choice);
        }

        return !(correct_choice < 0);
    }
    else
    {
        return check_for_expression_list(ASTSon0(expression_list), decl_context)
            && check_for_expression(ASTSon1(expression_list), decl_context);
    }
}

char check_zero_args_constructor(type_t* class_type, decl_context_t decl_context, AST declarator)
{
    int num_arguments = 0;
    type_t** arguments = NULL;

    scope_entry_list_t* candidates = NULL;
    scope_entry_t* chosen_constructor = solve_constructor(class_type,
            arguments, num_arguments,
            /* is_explicit */ 1,
            decl_context,
            ASTFileName(declarator), ASTLine(declarator),
            /* conversors */ NULL,
            &candidates);
    entry_list_free(candidates);

    if (chosen_constructor == NULL)
    {
        if (class_type_get_num_constructors(get_actual_class_type(class_type)) != 0)
        {
            fprintf(stderr, "%s: warning: no default constructor for '%s' type\n",
                    ast_location(declarator),
                    print_decl_type_str(class_type, decl_context, ""));
        }
        return 0;
    }
    else
    {
        ensure_not_deleted(decl_context, declarator, chosen_constructor);

        ASTAttrSetValueType(declarator, LANG_IS_IMPLICIT_CALL, tl_type_t, tl_bool(1));
        ASTAttrSetValueType(declarator, LANG_IMPLICIT_CALL, tl_type_t, tl_symbol(chosen_constructor));
    }

    return 1;
}
