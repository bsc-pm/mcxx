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



#include "cxx-exprtype.h"
#include "cxx-ambiguity.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-typeunif.h"
#include "cxx-typededuc.h"
#include "cxx-koenig.h"
#include "cxx-tltype.h"
#include "cxx-ambiguity.h"
#include "cxx-overload.h"
#include "cxx-prettyprint.h"
#include "cxx-instantiation.h"
#include "cxx-buildscope.h"
#include "cxx-cexpr.h"
#include "cxx-typeenviron.h"
#include "cxx-gccsupport.h"
#include "cxx-cuda.h"
#include "cxx-entrylist.h"
#include "cxx-limits.h"
#include "cxx-diagnostic.h"
#include "cxx-codegen.h"
#include <ctype.h>
#include <string.h>

#ifdef FORTRAN_SUPPORT
#include "fortran/fortran03-exprtype.h"
#endif


static const char builtin_prefix[] = "__builtin_";

static unsigned long long int _bytes_used_expr_check = 0;

unsigned long long exprtype_used_memory(void)
{
    return _bytes_used_expr_check;
}

typedef
struct builtin_operators_set_tag
{
    scope_entry_list_t *entry_list;
    scope_entry_t entry[MCXX_MAX_BUILTINS_IN_OVERLOAD];
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

scope_entry_list_t* get_entry_list_from_builtin_operator_set(builtin_operators_set_t* builtin_operators)
{
    if (builtin_operators->num_builtins == 0)
        return NULL;
    else 
        return (builtin_operators->entry_list);
}


type_t* actual_type_of_conversor(scope_entry_t* conv)
{
    if (conv->entity_specs.is_constructor)
    {
        return conv->entity_specs.class_type;
    }
    else if (conv->entity_specs.is_conversion)
    {
        return function_type_get_return_type(conv->type_information);
    }
    else
    {
        internal_error("Invalid conversion function!", 0);
    }
}

static
scope_entry_t* expand_template_given_arguments(scope_entry_t* entry,
        type_t** argument_types, int num_arguments, decl_context_t decl_context,
        const char* filename, int line,
        template_parameter_list_t* explicit_template_parameters)
{
    // We have to expand the template
    type_t* specialization_type = template_type_get_primary_type(entry->type_information);
    scope_entry_t* specialization_symbol = named_type_get_symbol(specialization_type);
    type_t* specialized_function_type = specialization_symbol->type_information;

    template_parameter_list_t* template_parameters = 
        template_specialized_type_get_template_arguments(specialized_function_type);

    deduction_set_t* deduction_result = NULL;

    if (deduce_arguments_from_call_to_specific_template_function(argument_types,
                num_arguments, specialization_type, template_parameters,
                decl_context, &deduction_result, filename, line, 
                explicit_template_parameters))
    {
        template_parameter_list_t* argument_list = build_template_parameter_list_from_deduction_set(
                template_parameters,
                deduction_result);

        // Now get a specialized template type for this
        // function (this will sign it in if it does not exist)
        type_t* named_specialization_type = template_type_get_specialized_type(entry->type_information,
                argument_list, decl_context, filename, line);

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

    nodecl_t dummy_nodecl_output = nodecl_null();

    type_t* simple_type_info = NULL;
    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, decl_context, &dummy_nodecl_output);

    type_t* declarator_type = simple_type_info;

    if (!is_error_type(declarator_type))
    {
        compute_declarator_type(abstract_declarator, 
                &gather_info, simple_type_info, 
                &declarator_type, decl_context, 
                &dummy_nodecl_output);
    }

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
        template_parameter_list_t *explicit_template_parameters
        )
{
    scope_entry_list_t* overload_set = NULL;

    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(result_from_lookup);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        scope_entry_t *orig_entry = entry;

        entry = entry_advance_aliases(entry);

        if (entry->kind == SK_TEMPLATE)
        {
            scope_entry_t* specialized_symbol = expand_template_given_arguments(entry,
                    argument_types, num_arguments, decl_context, filename, line,
                    explicit_template_parameters);

            if (specialized_symbol != NULL)
            {
                overload_set = entry_list_add(overload_set, specialized_symbol);
            }
        }
        else if (entry->kind == SK_FUNCTION)
        {
            overload_set = entry_list_add(overload_set, orig_entry);
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
            scope_entry_t* ovl = entry_advance_aliases(entry_list_iterator_current(it2));

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
scope_entry_list_t* get_member_of_class_type_nodecl(
        decl_context_t decl_context,
        type_t* class_type,
        nodecl_t nodecl_name)
{
    return query_nodecl_name_in_class(
            decl_context,
            named_type_get_symbol(advance_over_typedefs(class_type)), 
            nodecl_name);
}

// Remove this function in a future
static scope_entry_list_t* get_member_of_class_type(type_t* class_type,
        AST id_expression, decl_context_t decl_context)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    return get_member_of_class_type_nodecl(decl_context,
            class_type,
            nodecl_name);
}

static void decimal_literal_type(AST expr, nodecl_t* nodecl_output);
static void character_literal_type(AST expr, nodecl_t* nodecl_output);
static void floating_literal_type(AST expr, nodecl_t* nodecl_output);
static void string_literal_type(AST expr, nodecl_t* nodecl_output);

// Typechecking functions
static void check_qualified_id(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_symbol(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_array_subscript_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_function_call(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_explicit_type_conversion(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_explicit_typename_type_conversion(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_member_access(AST member_access, decl_context_t decl_context, char is_arrow, nodecl_t* nodecl_output);
static void check_typeid_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_typeid_type(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_sizeof_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_sizeof_typeid(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_cast_expr(AST expression, 
        AST type_id, AST casted_expression_list, 
        decl_context_t decl_context, 
        const char* cast_kind,
        nodecl_t* nodecl_output);
static void check_new_expression(AST new_expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_new_type_id_expr(AST new_expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_delete_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
// static void check_initializer_list(AST initializer_list, decl_context_t decl_context, type_t* declared_type);
static void check_binary_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_unary_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_throw_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_template_id_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_templated_member_access(AST templated_member_access, decl_context_t decl_context, 
        char is_arrow, nodecl_t* nodecl_output);
static void check_postincrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_postdecrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_preincrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_predecrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_conditional_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_comma_operand(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_pointer_to_member(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_pointer_to_pointer_to_member(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_conversion_function_id_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

static void check_vla_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

static void check_array_section_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_shaping_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

static void check_gcc_builtin_offsetof(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_builtin_choose_expr(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_builtin_types_compatible_p(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_label_addr(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_real_or_imag_part(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_alignof_expr(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_alignof_typeid(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_postfix_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_builtin_va_arg(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);
static void check_gcc_parenthesized_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

// Returns if the function is ok
//
// Do not return within this function, set result to 0 or 1 and let it
// reach the end, by default result == 0

static void check_expression_impl_(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

static char c_check_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output);

char check_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
#ifdef FORTRAN_SUPPORT
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
#endif
    return c_check_expression(expression, decl_context, nodecl_output);
#ifdef FORTRAN_SUPPORT
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        return fortran_check_expression(expression, decl_context, nodecl_output);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
#endif
}

void ensure_function_is_emitted(scope_entry_t* entry,
        const char* filename,
        int line)
{
    if (entry != NULL
            && entry->kind == SK_FUNCTION)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "EXPRTYPE: Ensuring function '%s' will be emitted\n", get_qualified_symbol_name(entry, entry->decl_context));
        }
        if (is_template_specialized_type(entry->type_information)
                || entry->entity_specs.is_non_emitted)
        {
            instantiation_add_symbol_to_instantiate(entry, filename, line);
        }
    }
}

static char c_check_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    check_expression_impl_(expression, decl_context, nodecl_output);
    char is_ok = !nodecl_is_err_expr(*nodecl_output);
    return is_ok;
}

static void check_expression_impl_(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    ERROR_CONDITION(nodecl_output == NULL, "This cannot be NULL\n", 0);
    switch (ASTType(expression))
    {
        case AST_EXPRESSION :
        case AST_CONSTANT_EXPRESSION :
        case AST_PARENTHESIZED_EXPRESSION :
            // GCC extensions
        case AST_GCC_EXTENSION_EXPR : 
            {
                check_expression_impl_(ASTSon0(expression), decl_context, nodecl_output);
                break;
            }
            // Primaries
        case AST_DECIMAL_LITERAL :
        case AST_OCTAL_LITERAL :
        case AST_HEXADECIMAL_LITERAL :
            {

                decimal_literal_type(expression, nodecl_output);
                break;
            }
        case AST_FLOATING_LITERAL :
            {

                floating_literal_type(expression, nodecl_output);
                break;
            }
        case AST_BOOLEAN_LITERAL :
            {

                type_t* t = get_bool_type();

                const char* literal = ASTText(expression);

                const_value_t* val = NULL;
                if (strcmp(literal, "true") == 0)
                {
                    val = const_value_get_one(type_get_size(t), 0);
                }
                else if (strcmp(literal, "false") == 0)
                {
                    val = const_value_get_zero(type_get_size(t), 0);
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }

                *nodecl_output = nodecl_make_boolean_literal(t, val, ASTFileName(expression), ASTLine(expression));
                break;
            }
        case AST_CHARACTER_LITERAL :
            {

                character_literal_type(expression, nodecl_output);
                break;
            }
        case AST_STRING_LITERAL :
            {

                string_literal_type(expression, nodecl_output);
                break;
            }
        case AST_THIS_VARIABLE :
            {
                scope_entry_list_t* entry_list = query_name_str(decl_context, "this");

                if (entry_list != NULL)
                {
                    scope_entry_t *entry = entry_list_head(entry_list);

                    if (is_pointer_to_class_type(entry->type_information))
                    {
                        *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expression), ASTLine(expression));
                        nodecl_set_type(*nodecl_output, lvalue_ref(entry->type_information));
                        if (is_dependent_type(entry->type_information))
                        {
                            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
                        }
                        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
                    }
                }
                else
                {
                    *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
                }
                entry_list_free(entry_list);

                break;
            }
        case AST_SYMBOL :
        case AST_OPERATOR_FUNCTION_ID :
            {
                check_symbol(expression, decl_context, nodecl_output);
                break;
            }
        case AST_QUALIFIED_ID :
        case AST_QUALIFIED_TEMPLATE :
            {
                check_qualified_id(expression, decl_context, nodecl_output);

                break;
            }
        case AST_DESTRUCTOR_ID :
        case AST_DESTRUCTOR_TEMPLATE_ID :
            {
                AST symbol = ASTSon0(expression);
                check_symbol(symbol, decl_context, nodecl_output);
                break;
            }
        case AST_OPERATOR_FUNCTION_ID_TEMPLATE :
            {
                check_template_id_expr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_CONVERSION_FUNCTION_ID :
            {
                check_conversion_function_id_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_TEMPLATE_ID :
            {
                check_template_id_expr(expression, decl_context, nodecl_output);
                break;
            }
            // Postfix expressions
        case AST_ARRAY_SUBSCRIPT :
            {
                check_array_subscript_expr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_FUNCTION_CALL :
            {
                check_function_call(expression, decl_context, nodecl_output);
                break;
            }
        case AST_EXPLICIT_TYPE_CONVERSION :
            {
                check_explicit_type_conversion(expression, decl_context, nodecl_output);
                break;
            }
        case AST_TYPENAME_EXPLICIT_TYPE_CONV :
            {
                check_explicit_typename_type_conversion(expression, decl_context, nodecl_output);
                break;
            }
        case AST_POINTER_CLASS_MEMBER_ACCESS :
        case AST_CLASS_MEMBER_ACCESS :
            {
                char is_arrow = (ASTType(expression) == AST_POINTER_CLASS_MEMBER_ACCESS);
                check_member_access(expression, decl_context, is_arrow, nodecl_output);
                break;
            }
        case AST_CLASS_TEMPLATE_MEMBER_ACCESS :
        case AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS :
            {
                char is_arrow = (ASTType(expression) == AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS);
                check_templated_member_access(expression, decl_context, is_arrow, nodecl_output);
                break;
            }
        case AST_POINTER_TO_MEMBER :
            {
                check_pointer_to_member(expression, decl_context, nodecl_output);
                break;
            }
        case AST_POINTER_TO_POINTER_MEMBER :
            {
                check_pointer_to_pointer_to_member(expression, decl_context, nodecl_output);
                break;
            }
        case AST_POSTINCREMENT :
            {
                check_postincrement(expression, decl_context, nodecl_output);
                break;
            }
        case AST_POSTDECREMENT :
            {
                check_postdecrement(expression, decl_context, nodecl_output);
                break;
            }
        case AST_DYNAMIC_CAST :
        case AST_STATIC_CAST :
        case AST_REINTERPRET_CAST :
        case AST_CONST_CAST :
            {
                AST type_id = ASTSon0(expression);
                AST casted_expr = ASTSon1(expression);

                const char* cast_kind = NULL;
                switch (ASTType(expression))
                {
                    case AST_DYNAMIC_CAST : cast_kind = "dynamic_cast"; break;
                    case AST_STATIC_CAST : cast_kind = "static_cast"; break;
                    case AST_REINTERPRET_CAST : cast_kind = "reinterpret_cast"; break;
                    case AST_CONST_CAST : cast_kind = "const_cast"; break;
                    default:
                          internal_error("Code unreachable", 0);
                }

                check_cast_expr(expression, type_id, casted_expr, decl_context, cast_kind, nodecl_output);
                break;
            }
        case AST_TYPEID_TYPE :
            {
                check_typeid_type(expression, decl_context, nodecl_output);
                break;
            }
        case AST_TYPEID_EXPR :
            {
                check_typeid_expr(expression, decl_context, nodecl_output);
                break;
            }
            // Unary expressions
        case AST_PREINCREMENT :
            {
                check_preincrement(expression, decl_context, nodecl_output);
                break;
            }
        case AST_PREDECREMENT :
            {
                check_predecrement(expression, decl_context, nodecl_output);
                break;
            }
        case AST_SIZEOF :
            {
                check_sizeof_expr(expression, decl_context, nodecl_output);
                break;
            }
            /* UPC has upc_{local,block,elem}sizeof that are identical to the normal one */
        case AST_UPC_BLOCKSIZEOF :
        case AST_UPC_ELEMSIZEOF :
        case AST_UPC_LOCALSIZEOF :
            {
                error_printf("%s: sorry: UPC constructs not supported yet\n",
                        ast_location(expression));
                break;
            }
            /* UPC has upc_{local,block,elem}sizeof that are identical to the normal one */
        case AST_UPC_BLOCKSIZEOF_TYPEID :
        case AST_UPC_ELEMSIZEOF_TYPEID :
        case AST_UPC_LOCALSIZEOF_TYPEID :
            {
                error_printf("%s: sorry: UPC constructs not supported yet\n",
                        ast_location(expression));
                break;
            }
        case AST_SIZEOF_TYPEID :
            {
                check_sizeof_typeid(expression, decl_context, nodecl_output);
                break;
            }
        case AST_DERREFERENCE :
        case AST_REFERENCE :
        case AST_PLUS :
        case AST_NEG :
        case AST_LOGICAL_NOT :
        case AST_BITWISE_NOT :
            {
                check_unary_expression(expression, decl_context, nodecl_output);
                break;
            }
            // Cast expression
        case AST_CAST :
            {
                AST type_id = ASTSon0(expression);
                AST casted_expr = ASTSon1(expression);

                check_cast_expr(expression, type_id, casted_expr, decl_context, "C", nodecl_output);
                break;
            }
        case AST_MUL :
        case AST_DIV :
        case AST_MOD :
        case AST_ADD :
        case AST_MINUS :
        case AST_SHL :
        case AST_SHR :
        case AST_LOWER_THAN :
        case AST_GREATER_THAN :
        case AST_GREATER_OR_EQUAL_THAN :
        case AST_LOWER_OR_EQUAL_THAN :
        case AST_EQUAL :
        case AST_DIFFERENT :
        case AST_BITWISE_AND :
        case AST_BITWISE_XOR :
        case AST_BITWISE_OR :
        case AST_LOGICAL_AND :
        case AST_LOGICAL_OR :
#ifdef FORTRAN_SUPPORT
        case AST_POWER:
#endif
            {
                check_binary_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_CONDITIONAL_EXPRESSION :
        case AST_GCC_CONDITIONAL_EXPRESSION :
            {
                check_conditional_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_ASSIGNMENT :
        case AST_MUL_ASSIGNMENT :
        case AST_DIV_ASSIGNMENT :
        case AST_ADD_ASSIGNMENT :
        case AST_SUB_ASSIGNMENT :
        case AST_SHL_ASSIGNMENT :
        case AST_SHR_ASSIGNMENT :
        case AST_BITWISE_AND_ASSIGNMENT :
        case AST_BITWISE_OR_ASSIGNMENT :
        case AST_BITWISE_XOR_ASSIGNMENT :
        case AST_MOD_ASSIGNMENT :
            {
                check_binary_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_THROW_EXPRESSION :
            {
                check_throw_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_COMMA :
            {
                check_comma_operand(expression, decl_context, nodecl_output);
                break;
            }
            // GCC Extension
        case AST_GCC_LABEL_ADDR :
            {
                check_gcc_label_addr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_REAL_PART :
        case AST_GCC_IMAG_PART :
            {
                check_gcc_real_or_imag_part(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_ALIGNOF :
            {
                check_gcc_alignof_expr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_ALIGNOF_TYPE :
            {
                check_gcc_alignof_typeid(expression, decl_context, nodecl_output);
                break;
            }
        case AST_NEW_EXPRESSION :
            {
                check_new_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_NEW_TYPE_ID_EXPR :
            {
                check_new_type_id_expr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_DELETE_EXPR :
        case AST_DELETE_ARRAY_EXPR :
            {
                check_delete_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_VLA_EXPRESSION :
            {
                check_vla_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_POSTFIX_EXPRESSION :
            {
                check_gcc_postfix_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_BUILTIN_VA_ARG :
            {
                check_gcc_builtin_va_arg(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_BUILTIN_OFFSETOF :
            {
                check_gcc_builtin_offsetof(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_BUILTIN_CHOOSE_EXPR :
            {
                check_gcc_builtin_choose_expr(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_BUILTIN_TYPES_COMPATIBLE_P :
            {
                check_gcc_builtin_types_compatible_p(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_PARENTHESIZED_EXPRESSION :
            {
                check_gcc_parenthesized_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_GXX_TYPE_TRAITS :
            {
                check_gxx_type_traits(expression, decl_context, nodecl_output);
                break;
            }
            // This is a mcxx extension
            // that brings the power of Fortran 90 array-sections into C/C++ :-)
        case AST_ARRAY_SECTION :
            {
                check_array_section_expression(expression, decl_context, nodecl_output);
                break;
            }
        case AST_ARRAY_SECTION_SIZE :
            {
                check_array_section_expression(expression, decl_context, nodecl_output);
                break;
            }
            // This is a mcxx extension
            // that gives an array shape to pointer expressions
        case AST_SHAPING_EXPRESSION:
            {
                check_shaping_expression(expression, decl_context, nodecl_output);
                break;
            }
            // Special nodes
        case AST_DIMENSION_STR:
            {
                internal_error("Not supported", 0);
                break;
            }
            // CUDA
        case AST_CUDA_KERNEL_CALL:
            {
                check_cuda_kernel_call(expression, decl_context, nodecl_output);
                break;
            }
        case AST_AMBIGUITY :
            {
                solve_ambiguous_expression(expression, decl_context, nodecl_output);
                break;
            }
        default :
            {
                internal_error("Unexpected node '%s' %s", ast_print_node_type(ASTType(expression)), 
                        ast_location(expression));
                break;
            }
    }

    if (nodecl_is_null(*nodecl_output))
    {
        internal_error("Expression '%s' at '%s' lacks a nodecl\n",
                prettyprint_in_buffer(expression),
                ast_location(expression));
    }

    DEBUG_CODE()
    {
        if (!nodecl_is_err_expr(*nodecl_output))
        {
            type_t* t = nodecl_get_type(*nodecl_output);

            fprintf(stderr, "EXPRTYPE: Expression '%s' at '%s' has as computed type '%s' and it is a '%s'",
                    prettyprint_in_buffer(expression),
                    ast_location(expression),
                    t != NULL ? print_declarator(t) : "<<NO TYPE>>",
                    nodecl_expr_is_lvalue(*nodecl_output) ? "lvalue" : "rvalue");

            if (nodecl_is_constant(*nodecl_output))
            {
                const_value_t* v = nodecl_get_constant(*nodecl_output);
                fprintf(stderr, " with a constant value of ");
                if (const_value_is_integer(v))
                {
                    if (const_value_is_signed(v))
                    {
                        fprintf(stderr, " '%lld'", (long long int)const_value_cast_to_8(v));
                    }
                    else
                    {
                        fprintf(stderr, " '%llu'", (unsigned long long int)const_value_cast_to_8(v));
                    }
                }
                else if (const_value_is_float(v))
                {
                    fprintf(stderr, " '%f'", const_value_cast_to_float(v));
                }
                else if (const_value_is_double(v))
                {
                    fprintf(stderr, " '%f'", const_value_cast_to_double(v));
                }
                else if (const_value_is_long_double(v))
                {
                    fprintf(stderr, " '%Lf'", const_value_cast_to_long_double(v));
                }
            }

            if (nodecl_expr_is_value_dependent(*nodecl_output))
            {
                fprintf(stderr, " [VALUE DEPENDENT]");
            }

            fprintf(stderr, "\n");
        }
    }

    if (!checking_ambiguity()
            && CURRENT_CONFIGURATION->strict_typecheck
            && nodecl_is_err_expr(*nodecl_output))
    {
        internal_error("Invalid expression '%s' at '%s'\n", prettyprint_in_buffer(expression), ast_location(expression));
    }
}


// Given a decimal literal computes the type due to its lexic form
static void decimal_literal_type(AST expr, nodecl_t* nodecl_output)
{
    const char *literal = ASTText(expr);
    const char *last = literal + strlen(literal) - 1;

    char is_unsigned = 0;
    char is_long = 0;
    char is_complex = 0;

    const_value_t* val = NULL;

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
                    val = const_value_get_integer(strtoul(literal, NULL, 0), type_get_size(result), /*sign*/ 0);
                }
                else
                {
                    result = get_signed_int_type();
                    val = const_value_get_integer(strtol(literal, NULL, 0), type_get_size(result), /* sign*/ 1);
                }
                break;
            }
        case 1 : 
            {
                if (is_unsigned)
                {
                    result = get_unsigned_long_int_type();
                    val = const_value_get_integer(strtoul(literal, NULL, 0), type_get_size(result), 0);
                }
                else
                {
                    result = get_signed_long_int_type();
                    val = const_value_get_integer(strtol(literal, NULL, 0), type_get_size(result), 1);
                }
                break;
            }
        default :
            {
                if (is_unsigned)
                {
                    result = get_unsigned_long_long_int_type();
                    val = const_value_get_integer(strtoull(literal, NULL, 0), type_get_size(result), 0);
                }
                else
                {
                    result = get_signed_long_long_int_type();
                    val = const_value_get_integer(strtoll(literal, NULL, 0), type_get_size(result), 1);
                }
                break;
            }
    }

    // Zero is a null pointer constant requiring a distinguishable 'int' type
    if (ASTType(expr) == AST_OCTAL_LITERAL
            && (strcmp(ASTText(expr), "0") == 0))
    {
        result = get_zero_type();
    }

    if (is_complex)
    {
        type_t* element_type = result;
        result = get_complex_type(result);
        val = const_value_make_complex(const_value_get_zero(type_get_size(result), !is_unsigned), val);

        *nodecl_output = nodecl_make_complex_literal(
                nodecl_make_integer_literal(element_type, const_value_get_zero(type_get_size(result), !is_unsigned), 
                        ASTFileName(expr), ASTLine(expr)),
                nodecl_make_integer_literal(element_type, val, ASTFileName(expr), ASTLine(expr)),
                result,
                ASTFileName(expr), ASTLine(expr));
    }
    else
    {
        *nodecl_output = nodecl_make_integer_literal(result, val, ASTFileName(expr), ASTLine(expr));
    }
}

// Given a character literal computes the type due to its lexic form
static void character_literal_type(AST expr, nodecl_t* nodecl_output)
{
    const char *literal = ASTText(expr);

    type_t* result = NULL;
    if (*literal == 'L')
    {
        result = get_wchar_t_type();
        literal++;
    }
    else
    {
        result = get_char_type();
    }

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
                               error_printf("%s: error: %s does not seem a valid character literal\n", 
                                       ast_location(expr),
                                       prettyprint_in_buffer(expr));
                               *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                               return;
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
                               error_printf("%s: error: %s does not seem a valid character literal\n", 
                                       ast_location(expr),
                                       prettyprint_in_buffer(expr));
                               *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                               return;
                           }
                           break;
                       }
            default: {
                         error_printf("%s: error: %s does not seem a valid escape character\n", 
                                 ast_location(expr),
                                 prettyprint_in_buffer(expr));
                         *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
                         return;
                     }
        }
    }

    *nodecl_output = nodecl_make_integer_literal(result, 
            const_value_get_integer(value, type_get_size(result), is_signed_integral_type(result)), 
            ASTFileName(expr), ASTLine(expr));
}

static void floating_literal_type(AST expr, nodecl_t* nodecl_output)
{
    const_value_t* value = NULL;
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

    type_t* result = NULL;
    const_value_t* zero = NULL;
    if (is_long_double)
    {
        result = get_long_double_type();

        long double ld = strtold(literal, NULL);
        value = const_value_get_long_double(ld);

        if (is_complex)
            zero = const_value_get_long_double(0.0L);
    }
    else if (is_float)
    {
        result = get_float_type();

        float f = strtof(literal, NULL);
        value = const_value_get_float(f);

        if (is_complex)
            zero = const_value_get_float(0.0f);
    }
    else
    {
        result = get_double_type();

        double d = strtod(literal, NULL);
        value = const_value_get_double(d);

        if (is_complex)
            zero = const_value_get_double(0.0);
    }

    if (is_complex)
    {
        type_t* element_type = result;
        result = get_complex_type(result);
        *nodecl_output = 
            nodecl_make_complex_literal(
                    nodecl_make_floating_literal(element_type, zero, ASTFileName(expr), ASTLine(expr)),
                    nodecl_make_floating_literal(element_type, value, ASTFileName(expr), ASTLine(expr)),
                    result,
                    ASTFileName(expr), ASTLine(expr));
    }
    else
    {
        *nodecl_output = nodecl_make_floating_literal(result, value, ASTFileName(expr), ASTLine(expr));
    }
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

static void compute_length_of_literal_string(AST expr, int* length, char *is_wchar, int **real_literal)
{
    // We allow the parser not to mix the two strings
    const char *literal = ASTText(expr);

    int max_real_size = strlen(literal);
    (*real_literal) = counted_calloc(max_real_size, sizeof(**real_literal), &_bytes_used_expr_check);

    int num_of_strings_seen = 0;

    *length = 0;
    *is_wchar = 0;

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
                            case '\'' : { (*real_literal)[(*length)]  = '\''; break; }
                            case '"' : { (*real_literal)[(*length)]  = '"'; break; }
                            case '?' : { (*real_literal)[(*length)]  = '\?'; break; }
                            case '\\' : { (*real_literal)[(*length)]  = '\\'; break; }
                            case 'a' : { (*real_literal)[(*length)]  = '\a'; break; }
                            case 'b' : { (*real_literal)[(*length)]  = '\b'; break; }
                            case 'f' : { (*real_literal)[(*length)]  = '\f'; break; }
                            case 'n' : { (*real_literal)[(*length)]  = '\n'; break; }
                            case 'r' : { (*real_literal)[(*length)]  = '\r'; break; }
                            case 't' : { (*real_literal)[(*length)]  = '\t'; break; }
                            case 'v' : { (*real_literal)[(*length)]  = '\v'; break; }
                            case 'e' : { (*real_literal)[(*length)]  = '\033'; break; } // GNU Extension: A synonim for \033
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
                                    unsigned int current_value = (*literal) - '0';

                                    while (IS_OCTA_CHAR(*literal)
                                            && (remaining_figures > 0))
                                    {
                                        current_value *= 8;
                                        current_value += ((*literal) - '0');
                                        remaining_figures--;
                                        literal++;
                                    }
                                    // Go backwards because we have already
                                    // advanced the last element of this
                                    // escaped entity
                                    literal--;

                                    (*real_literal)[(*length)] = current_value;
                                    break;
                                }
                            case 'x' :
                                // This is an hexadecimal
                                {
                                    // Jump 'x' itself
                                    literal++;

                                    unsigned int current_value = 0;

                                    while (IS_HEXA_CHAR(*literal))
                                    {
                                        current_value *= 16;
                                        char current_literal = tolower(*literal);
                                        if (('0' <= tolower(current_literal))
                                                && (tolower(current_literal) <= '9'))
                                        {
                                            current_value += current_literal - '0';
                                        }
                                        else if (('a' <= tolower(current_literal))
                                                && (tolower(current_literal) <= 'f'))
                                        {
                                            current_value += 10 + (tolower(current_literal) - 'a');
                                        }
                                        else
                                        {
                                            internal_error("Code unreachable", 0);
                                        }
                                        literal++;
                                    }

                                    // Go backwards because we have already
                                    // advanced the last element of this
                                    // escaped entity
                                    literal--;

                                    (*real_literal)[(*length)] = current_value;
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

                                    unsigned int current_value = 0;

                                    while (remaining_hexa_digits > 0)
                                    {
                                        if (!IS_HEXA_CHAR(*literal))
                                        {
                                            char ill_literal[11];
                                            strncpy(ill_literal, beginning_of_escape, /* hexa */ 8 + /* escape */ 1 + /* null*/ 1 );
                                            error_printf("%s: error: invalid universal literal name '%s'", 
                                                    ast_location(expr),
                                                    ill_literal);
                                            *length = -1;
                                            return;
                                        }

                                        current_value *= 16;
                                        char current_literal = tolower(*literal);
                                        if (('0' <= tolower(current_literal))
                                                && (tolower(current_literal) <= '9'))
                                        {
                                            current_value += current_literal - '0';
                                        }
                                        else if (('a' <= tolower(current_literal))
                                                && (tolower(current_literal) <= 'f'))
                                        {
                                            current_value += 10 + (tolower(current_literal) - 'a');
                                        }
                                        else
                                        {
                                            internal_error("Code unreachable", 0);
                                        }

                                        literal++;
                                        remaining_hexa_digits--;
                                    }

                                    // Go backwards one
                                    literal--;

                                    (*real_literal)[(*length)] = current_value;
                                    break;
                                }
                            default:
                                {
                                    char c[3];

                                    strncpy(c, beginning_of_escape, 3);
                                    error_printf("%s: error: invalid escape sequence '%s'\n",
                                            ast_location(expr),
                                            c);
                                    *length = -1;
                                    return;
                                }
                        }
                        break;
                    }
                default:
                    {
                        // A plain literal
                        (*real_literal)[(*length)] = (*literal);
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
    
    // NULL
    (*real_literal)[(*length)] = 0;
    (*length)++;

    ERROR_CONDITION(num_of_strings_seen == 0, "Empty string literal '%s'\n", ASTText(expr));
}

static void string_literal_type(AST expr, nodecl_t* nodecl_output)
{
    char is_wchar = 0;
    int length = 0;

    const_value_t *value = NULL;

    int* real_literal = NULL;
    compute_length_of_literal_string(expr, &length, &is_wchar, &real_literal);
    if (length < 0)
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: String literal %s type is '%s[%d]'\n",
                ASTText(expr),
                !is_wchar ? "char" : "wchar_t",
                length);
    }

    // Now we have an unsigned int[] with the values of the elements of the string, create one or other according to the type
    if (!is_wchar)
    {
        char c[length];
        int i;
        for (i = 0; i < length; i++)
            c[i] = real_literal[i];

        // -1 due to '\0' being added by compute_length_of_literal_string
        value = const_value_make_string(c, length - 1);
    }
    else
    {
        // -1 due to '\0' being added by compute_length_of_literal_string
        value = const_value_make_wstring(real_literal, length - 1);
    }

    type_t* result = get_literal_string_type(length, is_wchar);

    *nodecl_output = nodecl_make_string_literal(result, value, ASTFileName(expr), ASTLine(expr));
}


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
        C_LANGUAGE()
        {
            return CURRENT_CONFIGURATION->type_environment->int_type_of_wchar_t();
        }
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
        && (is_integral_type(rhs_type) || is_enum_type(rhs_type));
};

static 
char both_operands_are_integral_noref(type_t* lhs_type, type_t* rhs_type)
{
    return (is_integral_type(no_ref(lhs_type)) || is_enum_type(no_ref(lhs_type)))
        && (is_integral_type(no_ref(rhs_type)) || is_enum_type(no_ref(rhs_type)));
};

static 
char both_operands_are_arithmetic(type_t* lhs_type, type_t* rhs_type)
{
    return (is_arithmetic_type(lhs_type) || is_enum_type(lhs_type))
        && (is_arithmetic_type(rhs_type) || is_enum_type(rhs_type));
}

static 
char both_operands_are_arithmetic_noref(type_t* lhs_type, type_t* rhs_type)
{
    return (is_arithmetic_type(no_ref(lhs_type)) || is_enum_type(no_ref(lhs_type)))
        && (is_arithmetic_type(no_ref(rhs_type)) || is_enum_type(no_ref(rhs_type)));
}

static char both_operands_are_vector_types(type_t* lhs_type, type_t* rhs_type)
{
    return is_vector_type(lhs_type)
        && is_vector_type(rhs_type)
        && equivalent_types(get_unqualified_type(lhs_type), get_unqualified_type(rhs_type));
}

static char one_scalar_operand_and_one_vector_operand(type_t* lhs_type, type_t* rhs_type)
{
    return (is_vector_type(lhs_type) && is_scalar_type(rhs_type))
           || (is_scalar_type(lhs_type) && is_vector_type(rhs_type));
}

static char left_operand_is_vector_and_right_operand_is_scalar(type_t* lhs_type, type_t* rhs_type)
{
    return (is_vector_type(lhs_type) && is_scalar_type(rhs_type));
}    

static char is_pointer_and_integral_type(type_t* lhs_type, type_t* rhs_type)
{
    if (is_array_type(lhs_type))
    {
        // Convert to a pointer
        lhs_type = get_pointer_type(array_type_get_element_type(lhs_type));
    }

    return (is_pointer_type(lhs_type)
            && (is_integral_type(rhs_type) || is_enum_type(rhs_type)));
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
    ERROR_CONDITION (!both_operands_are_arithmetic_noref(lhs_type, rhs_type),
            "Both should be arithmetic types", 0);

    if (is_enum_type(lhs_type))
    {
        lhs_type = enum_type_get_underlying_type(lhs_type);
    }
    if (is_enum_type(rhs_type))
    {
        rhs_type = enum_type_get_underlying_type(rhs_type);
    }

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

static type_t * compute_scalar_vector_type(type_t* lhs_type, type_t* rhs_type)
{
    if (is_vector_type(lhs_type))
        return lhs_type;
    else
        return rhs_type;
}

static char filter_only_nonmembers(scope_entry_t* e, void* p UNUSED_PARAMETER)
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

static void error_message_delete_call(decl_context_t decl_context, scope_entry_t* entry, const char* filename, int line)
{
    error_printf("%s:%d: error: call to deleted function '%s'\n",
            filename, line,
            print_decl_type_str(entry->type_information, decl_context,
                get_qualified_symbol_name(entry, decl_context)));
}

char function_has_been_deleted(decl_context_t decl_context, scope_entry_t* entry, const char* filename, int line)
{
    char c = entry->entity_specs.is_deleted;
    if (c)
    {
        error_message_delete_call(decl_context, entry, filename, line);
    }
    return c;
}

static void error_message_overload_failed(candidate_t* candidates, const char* filename, int line);

static type_t* compute_user_defined_bin_operator_type(AST operator_name, 
        nodecl_t *lhs, nodecl_t *rhs, 
        scope_entry_list_t* builtins,
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** selected_operator)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    type_t* argument_types[2] = { lhs_type, rhs_type };
    int num_arguments = 2;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(lhs_type)))
    {
        scope_entry_list_t* operator_entry_list = get_member_of_class_type(no_ref(lhs_type), 
                operator_name, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                filename, line,
                /* explicit template arguments */ NULL);
        entry_list_free(operator_entry_list);
    }

    // This uses Koenig, otherwise some operators might not be found
    nodecl_t nodecl_op_name = 
        nodecl_make_cxx_dep_name_simple(get_operator_function_name(operator_name),
                filename, line);
    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, nodecl_op_name);

    nodecl_free(nodecl_op_name); // Not used anymore

    // Normal lookup might find member functions at this point, filter them
    scope_entry_list_t* nonmember_entry_list = filter_symbol_using_predicate(entry_list, filter_only_nonmembers, NULL);
    entry_list_free(entry_list);
    
    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(nonmember_entry_list,
            builtins, argument_types, num_arguments,
            decl_context,
            filename, line,
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
            filename, line,
            conversors);

    type_t* overloaded_type = NULL;
    if (overloaded_call != NULL)
    {
        if (function_has_been_deleted(decl_context, overloaded_call, filename, line))
        {
            return get_error_type();
        }

        if (!overloaded_call->entity_specs.is_member)
        {
            type_t* param_type_0 = function_type_get_parameter_type_num(overloaded_call->type_information, 0);

            if (is_class_type(param_type_0))
            {
                check_nodecl_expr_initializer(*lhs, decl_context, param_type_0, lhs);
            }
            else
            {
                if (conversors[0] != NULL)
                {
                    if (function_has_been_deleted(decl_context, conversors[0], filename, line))
                    {
                        return get_error_type();
                    }

                    *lhs = cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversors[0], nodecl_get_filename(*lhs), nodecl_get_line(*lhs)),
                            nodecl_make_list_1(*lhs),
                            actual_type_of_conversor(conversors[0]), nodecl_get_filename(*lhs), nodecl_get_line(*lhs));
                }
                else if (is_unresolved_overloaded_type(lhs_type))
                {
                    scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(lhs_type);
                    scope_entry_t* solved_function = address_of_overloaded_function(
                            unresolved_set,
                            unresolved_overloaded_type_get_explicit_template_arguments(lhs_type),
                            no_ref(param_type_0), 
                            decl_context,
                            nodecl_get_filename(*lhs),
                            nodecl_get_line(*lhs));

                    ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                    if (!solved_function->entity_specs.is_member
                            || solved_function->entity_specs.is_static)
                    {
                        *lhs = nodecl_make_symbol(solved_function, 
                                nodecl_get_filename(*lhs), nodecl_get_line(*lhs));
                        nodecl_set_type(*lhs, lvalue_ref(solved_function->type_information));
                    }
                    else
                    {
                        *lhs = nodecl_make_pointer_to_member(solved_function, 
                                get_lvalue_reference_type(
                                    get_pointer_to_member_type(solved_function->type_information,
                                        named_type_get_symbol(solved_function->entity_specs.class_type))),
                                nodecl_get_filename(*lhs), nodecl_get_line(*lhs));
                    }
                }
            }
        }

        type_t* param_type_1 = NULL;
        
        if (!overloaded_call->entity_specs.is_member)
        {
            param_type_1 = function_type_get_parameter_type_num(overloaded_call->type_information, 1);
        }
        else
        {
            param_type_1 = function_type_get_parameter_type_num(overloaded_call->type_information, 0);
        }

        if (is_class_type(param_type_1))
        {
            check_nodecl_expr_initializer(*rhs, decl_context, param_type_1, rhs);
        }
        else
        {
            if (conversors[1] != NULL)
            {
                if (function_has_been_deleted(decl_context, conversors[1], filename, line))
                {
                    return get_error_type();
                }

                *rhs = cxx_nodecl_make_function_call(
                        nodecl_make_symbol(conversors[1], nodecl_get_filename(*rhs), nodecl_get_line(*rhs)),
                        nodecl_make_list_1(*rhs),
                        actual_type_of_conversor(conversors[1]), nodecl_get_filename(*rhs), nodecl_get_line(*rhs));
            }
            else if (is_unresolved_overloaded_type(rhs_type))
            {

                scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(rhs_type);
                scope_entry_t* solved_function = address_of_overloaded_function(
                        unresolved_set,
                        unresolved_overloaded_type_get_explicit_template_arguments(rhs_type),
                        no_ref(param_type_1), 
                        decl_context,
                        nodecl_get_filename(*rhs),
                        nodecl_get_line(*rhs));

                ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    *rhs = nodecl_make_symbol(solved_function, 
                            nodecl_get_filename(*rhs), nodecl_get_line(*rhs));
                    nodecl_set_type(*rhs, lvalue_ref(solved_function->type_information));
                }
                else
                {
                    *rhs = nodecl_make_pointer_to_member(solved_function, 
                            get_lvalue_reference_type(
                                get_pointer_to_member_type(solved_function->type_information,
                                    named_type_get_symbol(solved_function->entity_specs.class_type))),
                            nodecl_get_filename(*rhs), nodecl_get_line(*rhs));
                }
            }
        }

        *selected_operator = overloaded_call;

        overloaded_type = function_type_get_return_type(overloaded_call->type_information);
    }
    else 
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(candidate_set, filename, line);
        }
        overloaded_type = get_error_type();
    }
    return overloaded_type;
}


static type_t* compute_user_defined_unary_operator_type(AST operator_name, 
        nodecl_t* op,
        scope_entry_list_t* builtins,
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** selected_operator)

{
    type_t* op_type = nodecl_get_type(*op);

    type_t* argument_types[1] = { op_type };
    int num_arguments = 1;

    candidate_t* candidate_set = NULL;

    if (is_class_type(no_ref(op_type)))
    {
        scope_entry_list_t* operator_entry_list = get_member_of_class_type(no_ref(op_type), 
                operator_name, decl_context);

        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_entry_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* orig_entry = entry_list_iterator_current(it);
            scope_entry_t* entry = entry_advance_aliases(orig_entry);
            // It is impossible to deduce anything since a unary overloaded
            // operator has zero parameters, so discard templates at this point
            if (entry->kind != SK_TEMPLATE)
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        orig_entry,
                        num_arguments,
                        argument_types);
            }
        }
        entry_list_iterator_free(it);
        entry_list_free(operator_entry_list);
    }

    nodecl_t nodecl_op_name = 
        nodecl_make_cxx_dep_name_simple(get_operator_function_name(operator_name),
                filename, line);

    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, nodecl_op_name);

    nodecl_free(nodecl_op_name); // Not used anymore

    // Remove any member that might have slip in because of plain lookup
    scope_entry_list_t* nonmember_entry_list = filter_symbol_using_predicate(entry_list, filter_only_nonmembers, NULL);
    entry_list_free(entry_list);
    
    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            nonmember_entry_list, builtins, argument_types, num_arguments,
            decl_context,
            filename, line,
            /* explicit_template_parameters */ NULL);
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
            filename, line,
            conversors);

    type_t* overloaded_type = NULL;
    if (overloaded_call != NULL)
    {
        if (function_has_been_deleted(decl_context, overloaded_call, filename, line))
        {
            return get_error_type();
        }

        if (!overloaded_call->entity_specs.is_member)
        {
            type_t* param_type = function_type_get_parameter_type_num(overloaded_call->type_information, 0);

            if (is_class_type(param_type))
            {
                check_nodecl_expr_initializer(*op, decl_context, param_type, op);
            }
            else
            {
                if (conversors[0] != NULL)
                {
                    if (function_has_been_deleted(decl_context, conversors[0], filename, line))
                    {
                        return get_error_type();
                    }

                    *op = cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversors[0], nodecl_get_filename(*op), nodecl_get_line(*op)),
                            nodecl_make_list_1(*op),
                            actual_type_of_conversor(conversors[0]), nodecl_get_filename(*op), nodecl_get_line(*op));
                }
                else if (is_unresolved_overloaded_type(op_type))
                {

                    scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(op_type);
                    scope_entry_t* solved_function = address_of_overloaded_function(
                            unresolved_set,
                            unresolved_overloaded_type_get_explicit_template_arguments(op_type),
                            no_ref(param_type), 
                            decl_context,
                            nodecl_get_filename(*op),
                            nodecl_get_line(*op));

                    ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                    if (!solved_function->entity_specs.is_member
                            || solved_function->entity_specs.is_static)
                    {
                        *op = nodecl_make_symbol(solved_function, 
                                nodecl_get_filename(*op), nodecl_get_line(*op));
                        nodecl_set_type(*op, lvalue_ref(solved_function->type_information));
                    }
                    else
                    {
                        *op = nodecl_make_pointer_to_member(solved_function, 
                                get_lvalue_reference_type(
                                    get_pointer_to_member_type(solved_function->type_information,
                                        named_type_get_symbol(solved_function->entity_specs.class_type))),
                                nodecl_get_filename(*op), nodecl_get_line(*op));
                    }
                }
            }
        }

        *selected_operator = overloaded_call;

        overloaded_type = function_type_get_return_type(overloaded_call->type_information);
    }
    else 
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(candidate_set, filename, line);
        }
        overloaded_type = get_error_type();
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

        *pointer_type = no_ref(*pointer_type);

        if (is_array_type(no_ref(*pointer_type)))
        {
            *pointer_type = get_pointer_type(array_type_get_element_type(no_ref(*pointer_type)));
        }

        return *pointer_type;
    }

    return get_error_type();
}

static void unary_record_conversion_to_result(type_t* result, nodecl_t* op)
{
    type_t* op_type = nodecl_get_type(*op);

    if (!equivalent_types(
                get_unqualified_type(no_ref(result)),
                get_unqualified_type(no_ref(op_type))))
    {
        *op = cxx_nodecl_make_conversion(*op, result,
                nodecl_get_filename(*op),
                nodecl_get_line(*op));
    }
}

static void binary_record_conversion_to_result(type_t* result, nodecl_t* lhs, nodecl_t* rhs)
{
    unary_record_conversion_to_result(result, lhs);
    unary_record_conversion_to_result(result, rhs);
}

static
type_t* compute_type_no_overload_add_operation(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else if (is_pointer_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return compute_pointer_arithmetic_type(no_ref(lhs_type), no_ref(rhs_type));
    }
    // Vector case
    else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (one_scalar_operand_and_one_vector_operand(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
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

// Generic function for binary typechecking in C and C++
static
void compute_bin_operator_generic(
        nodecl_t* lhs, nodecl_t* rhs, 
        // Operator name and context
        AST operator, 
        decl_context_t decl_context,
        // Functions
        char (*will_require_overload)(type_t*, type_t*),
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char *filename, int line),
        const_value_t* (*const_value_bin_fun)(const_value_t*, const_value_t*),
        type_t* (*compute_type_no_overload)(nodecl_t*, nodecl_t*),
        char types_allow_constant_evaluation(type_t*, type_t*),
        char (*overload_operator_predicate)(type_t*, type_t*),
        type_t* (*overload_operator_result_types)(type_t**, type_t**),
        // Locus
        const char* filename, int line,

        nodecl_t* nodecl_output)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);
    
    if (nodecl_expr_is_type_dependent(*lhs)
            || nodecl_expr_is_type_dependent(*rhs))
    {
        *nodecl_output = nodecl_bin_fun(*lhs, *rhs,  
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);

        if (// If the expression can be constant and any of the operands is value dependent, so it is
                const_value_bin_fun != NULL 
                && (nodecl_expr_is_value_dependent(*lhs)
                    || nodecl_expr_is_value_dependent(*rhs)))
        {
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }
        return;
    }

    const_value_t* val = NULL;

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        // Try to simplify unresolved overloads
        struct 
        {
            nodecl_t* op;
            type_t* op_type;
        } info[] = 
        { 
            { lhs, lhs_type},
            { rhs, rhs_type},
            //Sentinel
            { NULL, NULL}
        };
        int i;
        for (i = 0; info[i].op != NULL; i++)
        {
            type_t* current_type = no_ref(info[i].op_type);

            if (is_unresolved_overloaded_type(current_type))
            {
                scope_entry_t* function = unresolved_overloaded_type_simplify(current_type,
                        decl_context, filename, line);
                if (function != NULL)
                {
                    //Change the type of the operand
                    info[i].op_type = get_pointer_type(function->type_information);
                    nodecl_set_type(*(info[i].op), info[i].op_type);
                }
            }
        }
        requires_overload = will_require_overload(no_ref(lhs_type), no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = compute_type_no_overload(lhs, rhs);

        if (is_error_type(computed_type))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        *nodecl_output = nodecl_bin_fun(*lhs, *rhs,
                computed_type, filename, line);

        if (const_value_bin_fun != NULL
                && types_allow_constant_evaluation(lhs_type, rhs_type)
                && nodecl_is_constant(*lhs)
                && nodecl_is_constant(*rhs))
        {
            val = const_value_bin_fun(nodecl_get_constant(*lhs), 
                    nodecl_get_constant(*rhs));
        }

        nodecl_set_constant(*nodecl_output, val);

        if (nodecl_expr_is_value_dependent(*lhs)
                || nodecl_expr_is_value_dependent(*rhs))
        {
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }
        return;
    }

    builtin_operators_set_t builtin_set;
    build_binary_builtin_operators(
            lhs_type, rhs_type, 
            &builtin_set,
            decl_context, operator, 
            overload_operator_predicate,
            overload_operator_result_types);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // Now in C++ we have to rely on overloading for operators
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            lhs, rhs, builtins, decl_context, filename, line, &selected_operator);

    ERROR_CONDITION(result == NULL, "Invalid type", 0);

    entry_list_free(builtins);

    if (selected_operator != NULL)
    {
        if (selected_operator->entity_specs.is_builtin)
        {
            if (const_value_bin_fun != NULL
                    && types_allow_constant_evaluation(lhs_type, rhs_type)
                    && nodecl_is_constant(*lhs)
                    && nodecl_is_constant(*rhs))
            {
                val = const_value_bin_fun(nodecl_get_constant(*lhs), 
                        nodecl_get_constant(*rhs));
            }

            type_t* computed_type = compute_type_no_overload(lhs, rhs);

            ERROR_CONDITION(is_error_type(computed_type), 
                    "Compute type no overload cannot deduce a type for a builtin solved overload lhs=%s rhs=%s\n",
                    print_declarator(nodecl_get_type(*lhs)),
                    print_declarator(nodecl_get_type(*rhs)));

            ERROR_CONDITION(!equivalent_types(result, computed_type), 
                "Mismatch between the types of builtin functions (%s) and result of no overload type (%s)\n",
                print_declarator(result),
                print_declarator(computed_type));

            *nodecl_output = 
                nodecl_bin_fun(
                        *lhs,
                        *rhs,
                        computed_type, 
                        filename, line);

            nodecl_set_constant(*nodecl_output, val);

            if (nodecl_expr_is_value_dependent(*lhs)
                    || nodecl_expr_is_value_dependent(*rhs))
            {
                nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
            }
        }
        else
        {
            *nodecl_output = 
                    cxx_nodecl_make_function_call(
                        nodecl_make_symbol(selected_operator, filename, line),
                        nodecl_make_list_2(*lhs, *rhs),
                        result, filename, line);
        }
    }
    else
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
    }
}

static
type_t* compute_type_no_overload_bin_arithmetic(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    // Vector case
    else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (one_scalar_operand_and_one_vector_operand(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
}

static
void compute_bin_operator_add_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    // Now in C++ we have to rely on overloading for operators
    static AST operation_add_tree = NULL;
    if (operation_add_tree == NULL)
    {
        operation_add_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_generic(lhs, rhs, 
            operation_add_tree, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_make_add,
            const_value_add,
            compute_type_no_overload_add_operation,
            both_operands_are_arithmetic_noref,
            operator_bin_plus_builtin_pred,
            operator_bin_plus_builtin_result,
            filename, line,
            nodecl_output);
}

static
void compute_bin_operator_only_arithmetic_types(nodecl_t* lhs, nodecl_t* rhs, 
        AST operator, 
        decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char *filename, int line),
        const_value_t* (*const_value_bin_fun)(const_value_t*, const_value_t*),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            const_value_bin_fun,
            compute_type_no_overload_bin_arithmetic,
            both_operands_are_arithmetic_noref,
            operator_bin_only_arithmetic_pred,
            operator_bin_only_arithmetic_result,
            filename, line,
            nodecl_output);
}

static
void compute_bin_operator_mul_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MUL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_arithmetic_types(lhs, rhs, operation_tree, 
            decl_context, 
            nodecl_make_mul,
            const_value_mul,
            filename, line,
            nodecl_output);
}

#ifdef FORTRAN_SUPPORT
static
void compute_bin_operator_pow_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    // No operation_tree for Fortran's **
    AST operation_tree = NULL;

    compute_bin_operator_only_arithmetic_types(lhs, rhs, operation_tree, 
            decl_context, 
            nodecl_make_power,
            const_value_pow,
            filename, line,
            nodecl_output);
}
#endif

static
void compute_bin_operator_div_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIV_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_arithmetic_types(lhs, rhs, operation_tree, 
            decl_context, 
            nodecl_make_div,
            const_value_div,
            filename, line,
            nodecl_output);
}

static char operator_bin_only_integer_pred(type_t* lhs, type_t* rhs)
{
    return (is_integer_type(no_ref(lhs)) 
            && is_integer_type(no_ref(rhs)));
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
type_t* compute_type_no_overload_bin_only_integer(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);
        
        return result;
    }
    // Vector case
    else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (one_scalar_operand_and_one_vector_operand(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
}

static 
void compute_bin_operator_only_integer_types(nodecl_t* lhs, nodecl_t* rhs, 
        AST operator, 
        decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char *filename, int line),
        const_value_t* (*const_value_bin_fun)(const_value_t*, const_value_t*),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            const_value_bin_fun,
            compute_type_no_overload_bin_only_integer,
            both_operands_are_arithmetic_noref,
            operator_bin_only_integer_pred,
            operator_bin_only_integer_result,
            filename, line,
            nodecl_output);
}

static
void compute_bin_operator_mod_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MOD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integer_types(lhs, rhs, operation_tree, decl_context, 
            nodecl_make_mod, const_value_mod, 
            filename, line, nodecl_output);
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

    return get_error_type();
}

static type_t* compute_type_no_overload_sub(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else if (is_pointer_and_integral_type(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return compute_pointer_arithmetic_type(no_ref(lhs_type), no_ref(rhs_type));
    }
    else if (pointer_types_are_similar(no_ref(lhs_type), no_ref(rhs_type)))
    {
        // FIXME, this should be the type related to ptrdiff_t (usually int)
        return get_signed_int_type();
    }
    // Vector case
    else if (both_operands_are_vector_types(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (one_scalar_operand_and_one_vector_operand(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));

        binary_record_conversion_to_result(result, lhs, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
}

static 
void compute_bin_operator_sub_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operator = NULL;
    if (operator == NULL)
    {
        operator = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MINUS_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_make_minus,
            const_value_sub,
            compute_type_no_overload_sub,
            both_operands_are_arithmetic_noref,
            operator_bin_sub_builtin_pred,
            operator_bin_sub_builtin_result,
            filename, line,
            nodecl_output);
}

static char operator_bin_left_integral_right_integral_pred(type_t* lhs, type_t* rhs)
{
    return (is_integral_type(no_ref(lhs))
            && is_integral_type(no_ref(rhs)));
}

static type_t* operator_bin_left_integral_result(type_t** lhs, type_t** rhs)
{
    if (is_promoteable_integral_type(no_ref(*lhs)))
    {
        *lhs = promote_integral_type(no_ref(*lhs));
    }

    if (is_promoteable_integral_type(no_ref(*rhs)))
    {
        *rhs = promote_integral_type(no_ref(*rhs));
    }

    return (*lhs);
}

static type_t* compute_type_no_overload_only_integral_lhs_type(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
    {
        // Always the left one in this case
        return no_ref(lhs_type);
    }
    else if(left_operand_is_vector_and_right_operand_is_scalar(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* result = vector_type_get_element_type(lhs_type);

        unary_record_conversion_to_result(result, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
}

static
void compute_bin_operator_only_integral_lhs_type(nodecl_t* lhs, nodecl_t* rhs, 
        AST operator, 
        decl_context_t decl_context, 
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const_value_t* (const_value_bin_fun)(const_value_t*, const_value_t*),
        const char* filename, int line, nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            const_value_bin_fun,
            compute_type_no_overload_only_integral_lhs_type,
            both_operands_are_integral_noref,
            operator_bin_left_integral_right_integral_pred,
            operator_bin_left_integral_result,
            filename, line,
            nodecl_output);
}

void compute_bin_operator_shl_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LEFT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integral_lhs_type(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_shl,
            const_value_shl,
            filename, line, nodecl_output);
}

void compute_bin_operator_shr_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_RIGHT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integral_lhs_type(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_shr,
            const_value_shr,
            filename, line, nodecl_output);
}

static char operator_bin_arithmetic_pointer_or_enum_pred(type_t* lhs, type_t* rhs)
{
    // Two arithmetics or enum or pointer
    return ((is_arithmetic_type(no_ref(lhs))
                && is_arithmetic_type(no_ref(rhs)))
            // T* < T*
            || ((is_pointer_type(no_ref(lhs)) || is_array_type(no_ref(lhs)))
                && (is_pointer_type(no_ref(rhs)) || is_array_type(no_ref(rhs)))
                && equivalent_types(get_unqualified_type(no_ref(lhs)), get_unqualified_type(no_ref(rhs))))
            // Silly case for zero_type
            || (is_pointer_type(no_ref(lhs)) && is_zero_type(no_ref(rhs)))
            || (is_zero_type(no_ref(lhs)) && is_pointer_type(no_ref(rhs)))
            // enum E < enum E
            || (is_enum_type(no_ref(lhs))
                && is_enum_type(no_ref(rhs))
                && equivalent_types(get_unqualified_type(no_ref(lhs)), get_unqualified_type(no_ref(rhs)))));
}

static type_t* operator_bin_arithmetic_pointer_or_enum_result(type_t** lhs, type_t** rhs)
{
    if (is_arithmetic_type(no_ref(*lhs))
            && is_arithmetic_type(no_ref(*rhs)))
    {
        if (is_promoteable_integral_type(no_ref(*lhs)))
            *lhs = promote_integral_type(no_ref(*lhs));
        if (is_promoteable_integral_type(no_ref(*rhs)))
            *rhs = promote_integral_type(no_ref(*rhs));

        return get_bool_type();
    }
    else if ((is_pointer_type(no_ref(*lhs)) || is_array_type(no_ref(*lhs)))
            && (is_pointer_type(no_ref(*rhs)) || is_array_type(no_ref(*rhs)))
            && equivalent_types(get_unqualified_type(no_ref(*lhs)), get_unqualified_type(no_ref(*rhs))))
    {
        if (is_array_type(no_ref(*lhs)))
        {
            *lhs = get_pointer_type(array_type_get_element_type(no_ref(*lhs)));
        }

        if (is_array_type(no_ref(*rhs)))
        {
            *rhs = get_pointer_type(array_type_get_element_type(no_ref(*rhs)));
        }

        return get_bool_type();
    }
    else if ((is_zero_type(no_ref(*lhs)) && is_pointer_type(no_ref(*rhs)))
            || (is_pointer_type(no_ref(*lhs)) && is_zero_type(no_ref(*rhs))))
    {
        // Convert the zero type to the other pointer type
        if (is_zero_type(no_ref(*lhs)))
        {
            *lhs = no_ref(*rhs);
        }
        if (is_zero_type(no_ref(*rhs)))
        {
            *rhs = no_ref(*lhs);
        }

        return get_bool_type();
    }
    else if (is_enum_type(no_ref(no_ref(*lhs)))
            && is_enum_type(no_ref(no_ref(*rhs)))
            && equivalent_types(get_unqualified_type(no_ref(no_ref(*lhs))), get_unqualified_type(no_ref(no_ref(*rhs)))))
    {
        return get_bool_type();
    }
    return get_error_type();
}

static
type_t* compute_type_no_overload_relational_operator(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    type_t* no_ref_lhs_type = no_ref(lhs_type);
    type_t* no_ref_rhs_type = no_ref(rhs_type);

    if (both_operands_are_arithmetic(no_ref_lhs_type, no_ref_rhs_type)
            || pointer_types_are_similar(no_ref_lhs_type, no_ref_rhs_type))
    {
        C_LANGUAGE()
        {
            return get_signed_int_type();
        }
        CXX_LANGUAGE()
        {
            return get_bool_type();
        }
    }
    else if (both_operands_are_vector_types(no_ref_lhs_type, no_ref_rhs_type)
            || one_scalar_operand_and_one_vector_operand(no_ref_lhs_type, no_ref_rhs_type))
    {
        // return compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));
        type_t* common_vec_type = NULL;
        if (one_scalar_operand_and_one_vector_operand(no_ref_lhs_type, no_ref_rhs_type))
        {
            common_vec_type = compute_scalar_vector_type(no_ref(lhs_type), no_ref(rhs_type));
        }
        else
        {
            if (vector_type_get_vector_size(no_ref_lhs_type) == 0)
            {
                common_vec_type = no_ref_rhs_type;
            }
            else if (vector_type_get_vector_size(no_ref_rhs_type) == 0)
            {
                common_vec_type = no_ref_lhs_type;
            }
            else if (vector_type_get_vector_size(no_ref_rhs_type) 
                    != vector_type_get_vector_size(no_ref_rhs_type))
            {
                // Vectors do not match their size
                return get_error_type();
            }
            else
            {
                common_vec_type = no_ref_lhs_type;
            }

            type_t* elem_lhs_type = vector_type_get_element_type(no_ref_lhs_type);
            type_t* elem_rhs_type = vector_type_get_element_type(no_ref_rhs_type);
            if (!both_operands_are_arithmetic(elem_lhs_type, elem_rhs_type))
            {
                // We cannot do a binary relational op on them
                return get_error_type();
            }
        }

        type_t* ret_bool_type = NULL;
        C_LANGUAGE()
        {
            ret_bool_type = get_signed_int_type();
        }
        CXX_LANGUAGE()
        {
            ret_bool_type = get_bool_type();
        }
        return get_vector_type(ret_bool_type, vector_type_get_vector_size(common_vec_type));
    }
    else
    {
        return get_error_type();
    }

    return get_error_type();
}

static void compute_bin_operator_relational(nodecl_t* lhs, nodecl_t* rhs, AST operator, decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const_value_t* (const_value_bin_fun)(const_value_t*, const_value_t*),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            const_value_bin_fun,
            compute_type_no_overload_relational_operator,
            both_operands_are_arithmetic_noref,
            operator_bin_arithmetic_pointer_or_enum_pred,
            operator_bin_arithmetic_pointer_or_enum_result,
            filename, line,
            nodecl_output);
}

static
void compute_bin_operator_lower_equal_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LESS_OR_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_lower_or_equal_than,
            const_value_lte,
            filename, line,
            nodecl_output);
}

void compute_bin_operator_lower_than_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOWER_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_lower_than,
            const_value_lt,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_greater_equal_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_GREATER_OR_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_greater_or_equal_than,
            const_value_gte,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_greater_than_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_GREATER_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_greater_than,
            const_value_gt,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_different_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIFFERENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_different,
            const_value_neq,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_equal_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_EQUAL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_relational(lhs, rhs, 
            operation_tree, 
            decl_context, 
            nodecl_make_equal,
            const_value_eq,
            filename, line,
            nodecl_output);
}

static char operator_bin_logical_types_pred(type_t* lhs, type_t* rhs)
{
    standard_conversion_t dummy;
    return (standard_conversion_between_types(&dummy, no_ref(lhs), get_bool_type())
            && standard_conversion_between_types(&dummy, no_ref(rhs), get_bool_type()));
}

static type_t* operator_bin_logical_types_result(type_t** lhs, type_t** rhs)
{
    // We want the prototype of the builtin operation as 'bool operator#(bool, bool)' not
    // 'bool operator#(L, R)' with L and R convertible to bool
    *lhs = get_bool_type();
    *rhs = get_bool_type();

    return get_bool_type();
}

static type_t* compute_type_no_overload_logical_op(nodecl_t* lhs, nodecl_t* rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    type_t* conversion_type = NULL;
    C_LANGUAGE()
    {
        conversion_type = get_signed_int_type();
    }
    CXX_LANGUAGE()
    {
        conversion_type = get_bool_type();
    }

    char is_vector_op = 0;
    int vector_size = 0;
    if (both_operands_are_vector_types(no_ref(lhs_type),
                no_ref(rhs_type)))
    {
        is_vector_op = 1;
        vector_size = vector_type_get_vector_size(lhs_type);

        if (vector_size != vector_type_get_vector_size(rhs_type))
        {
            return get_error_type();
        }

        lhs_type = vector_type_get_element_type(lhs_type);
        rhs_type = vector_type_get_element_type(rhs_type);
    }

    standard_conversion_t lhs_to_bool;
    standard_conversion_t rhs_to_bool;
    if (standard_conversion_between_types(&lhs_to_bool, no_ref(lhs_type), conversion_type)
            && standard_conversion_between_types(&rhs_to_bool, no_ref(rhs_type), conversion_type))
    {
        if (is_vector_op)
        {
            return get_vector_type(conversion_type, vector_size);
        }
        else
        {
            return conversion_type;
        }
    }

    return get_error_type();
}

static void compute_bin_logical_op_type(nodecl_t* lhs, nodecl_t* rhs, AST operator, 
        decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const_value_t* (const_value_bin_fun)(const_value_t*, const_value_t*),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            const_value_bin_fun,
            compute_type_no_overload_logical_op,
            both_operands_are_arithmetic_noref,
            operator_bin_logical_types_pred,
            operator_bin_logical_types_result,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_logical_or_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_OR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_logical_op_type(lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_logical_or,
            const_value_or,
            filename, line, 
            nodecl_output);
}

static void compute_bin_operator_logical_and_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_AND_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_logical_op_type(lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_logical_and,
            const_value_and,
            filename, line, 
            nodecl_output);
}

static void compute_bin_operator_bitwise_and_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)

{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_AND_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integer_types(
            lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_bitwise_and,
            const_value_bitand,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_bitwise_or_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_OR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integer_types(
            lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_bitwise_or,
            const_value_bitor,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_bitwise_xor_type(nodecl_t* lhs, nodecl_t* rhs, decl_context_t decl_context, 
        const char* filename, int line, nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_XOR_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_only_integer_types(
            lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_bitwise_xor,
            const_value_bitxor,
            filename, line,
            nodecl_output);
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

static type_t* compute_type_no_overload_assig_only_integral_type(nodecl_t* lhs, nodecl_t* rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    C_LANGUAGE()
    {
        if (!nodecl_expr_is_lvalue(*lhs))
            return get_error_type();
    }

    CXX_LANGUAGE()
    {
        if (!is_lvalue_reference_type(lhs_type)
                || is_const_qualified_type(reference_type_get_referenced_type(lhs_type)))
            return get_error_type();
    }

    if (both_operands_are_integral(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* common_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        unary_record_conversion_to_result(common_type, rhs);

        return lhs_type;
    }
    else if (left_operand_is_vector_and_right_operand_is_scalar(no_ref(lhs_type), no_ref(rhs_type)))
    { 
        type_t* result = vector_type_get_element_type(no_ref(lhs_type));

        unary_record_conversion_to_result(result, rhs);

        return result;
    }
    else
    {
        return get_error_type();
    }
}


static void compute_bin_operator_assig_only_integral_type(nodecl_t* lhs, nodecl_t* rhs, AST operator,
        decl_context_t decl_context, nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            NULL, // No constants
            compute_type_no_overload_assig_only_integral_type,
            NULL,
            operator_bin_assign_only_integer_pred,
            operator_bin_assign_only_integer_result,
            filename, line,
            nodecl_output);
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

    return get_error_type();
}

static type_t* compute_type_no_overload_assig_arithmetic_or_pointer_type(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    C_LANGUAGE()
    {
        if (!nodecl_expr_is_lvalue(*lhs))
            return get_error_type();
    }

    CXX_LANGUAGE()
    {
        if (!is_lvalue_reference_type(lhs_type)
                || is_const_qualified_type(reference_type_get_referenced_type(lhs_type)))
            return get_error_type();
    }

    if (both_operands_are_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        type_t* common_type = compute_arithmetic_builtin_bin_op(no_ref(lhs_type), no_ref(rhs_type));

        unary_record_conversion_to_result(common_type, rhs);

        return lhs_type;
    }
    else if (is_pointer_arithmetic(no_ref(lhs_type), no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (both_operands_are_vector_types(no_ref(lhs_type), 
                no_ref(rhs_type)))
    {
        return lhs_type;
    }
    else if (left_operand_is_vector_and_right_operand_is_scalar(no_ref(lhs_type), no_ref(rhs_type)))
    {
        unary_record_conversion_to_result(lhs_type, rhs);

        return lhs_type;
    }


    return get_error_type();
}

static void compute_bin_operator_assig_arithmetic_or_pointer_type(nodecl_t* lhs, nodecl_t* rhs, 
        AST operator,
        decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            NULL, // No constants
            compute_type_no_overload_assig_arithmetic_or_pointer_type,
            NULL,
            operator_bin_assign_arithmetic_or_pointer_pred,
            operator_bin_assign_arithmetic_or_pointer_result,
            filename, line,
            nodecl_output);
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

static void compute_bin_nonoperator_assig_only_arithmetic_type(nodecl_t *lhs, nodecl_t *rhs, 
        AST operator, decl_context_t decl_context,
        const char* filename, int line,
        nodecl_t* nodecl_output)

{
    // (Non-operator) Assignment is so special that it cannot use the generic procedure for
    // solving binary operations
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    if (nodecl_expr_is_type_dependent(*lhs)
            || nodecl_expr_is_type_dependent(*rhs))
    {
        *nodecl_output = nodecl_make_assignment(*lhs, *rhs, 
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        // Enums are not considered for overloading in operator= because any overload
        // of operator= must be member, so classes are only eligible here for overload.
        requires_overload = is_class_type(no_ref(lhs_type)) || is_class_type(no_ref(rhs_type));
    }

    if (!requires_overload)
    {
        type_t* computed_type = NULL;
        C_LANGUAGE()
        {
            if (!nodecl_expr_is_lvalue(*lhs))
                computed_type = get_error_type();
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(lhs_type)
                    || is_const_qualified_type(no_ref(lhs_type)))
            {
                computed_type = get_error_type();
            }

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
                        nodecl_get_filename(*lhs),
                        nodecl_get_line(*lhs));
                entry_list_free(unresolved_set);

                if (solved_function == NULL)
                {
                    computed_type = get_error_type();
                }

                // Update the types everywhere
                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_type(solved_function->type_information));

                    *rhs = nodecl_make_symbol(solved_function, 
                            nodecl_get_filename(*rhs), nodecl_get_line(*rhs));
                    nodecl_set_type(*rhs, lvalue_ref(solved_function->type_information));
                }
                else
                {
                    rhs_type = get_lvalue_reference_type(get_pointer_to_member_type(
                                solved_function->type_information,
                                named_type_get_symbol(solved_function->entity_specs.class_type)));

                    *rhs = nodecl_make_pointer_to_member(solved_function, 
                            rhs_type,
                            nodecl_get_filename(*rhs), nodecl_get_line(*rhs));
                }
            }
        }

        computed_type = rhs_type;

        standard_conversion_t sc;
        if (computed_type != NULL
                && !is_error_type(computed_type)
                && !standard_conversion_between_types(&sc, no_ref(rhs_type), no_ref(lhs_type)))
        {
            computed_type = get_error_type();
        }

        if (is_error_type(computed_type))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* t = lvalue_ref(lhs_type);

        // Keep conversions
        if (!equivalent_types(
                    get_unqualified_type(no_ref(nodecl_get_type(*lhs))), 
                    get_unqualified_type(no_ref(t))))
        {
            *lhs = cxx_nodecl_make_conversion(*lhs, t,
                    nodecl_get_filename(*lhs),
                    nodecl_get_line(*lhs));
        }
        if (!equivalent_types(
                    get_unqualified_type(no_ref(nodecl_get_type(*rhs))), 
                    get_unqualified_type(no_ref(t))))
        {
            *rhs = cxx_nodecl_make_conversion(*rhs, t,
                    nodecl_get_filename(*rhs),
                    nodecl_get_line(*rhs));
        }

        *nodecl_output = nodecl_make_assignment(
                *lhs,
                *rhs,
                t, 
                filename, line);
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        return;
    }

    builtin_operators_set_t builtin_set; 
    build_binary_nonop_assign_builtin(
            lhs_type, &builtin_set, operator, decl_context);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    // We need to do overload
    type_t* result = compute_user_defined_bin_operator_type(operator, 
            lhs, rhs, builtins, decl_context, filename, line, &selected_operator);

    entry_list_free(builtins);

    if (result != NULL
            && selected_operator != NULL)
    {
        if (selected_operator->entity_specs.is_builtin)
        {
            // Keep conversions
            if (!equivalent_types(
                        get_unqualified_type(no_ref(nodecl_get_type(*lhs))), 
                        get_unqualified_type(no_ref(result))))
            {
                *lhs = cxx_nodecl_make_conversion(*lhs, result,
                        nodecl_get_filename(*lhs),
                        nodecl_get_line(*lhs));
            }
            if (!equivalent_types(
                        get_unqualified_type(no_ref(nodecl_get_type(*rhs))), 
                        get_unqualified_type(no_ref(result))))
            {
                *rhs = cxx_nodecl_make_conversion(*rhs, result,
                        nodecl_get_filename(*rhs),
                        nodecl_get_line(*rhs));
            }

            *nodecl_output = nodecl_make_assignment(
                    *lhs,
                    *rhs,
                    result, 
                    filename, line);
        }
        else
        {
            *nodecl_output = 
                    cxx_nodecl_make_function_call(
                        nodecl_make_symbol(selected_operator, filename, line),
                        nodecl_make_list_2(*lhs, *rhs),
                        result, filename, line);
        }
    }
}

static type_t* compute_type_no_overload_assig_only_arithmetic_type(nodecl_t *lhs, nodecl_t *rhs)
{
    type_t* lhs_type = nodecl_get_type(*lhs);
    type_t* rhs_type = nodecl_get_type(*rhs);

    C_LANGUAGE()
    {
        if (!nodecl_expr_is_lvalue(*lhs))
            return get_error_type();
    }

    CXX_LANGUAGE()
    {
        if (!is_lvalue_reference_type(lhs_type)
                || is_const_qualified_type(no_ref(lhs_type)))
            return get_error_type();
    }

    if (both_operands_are_arithmetic(no_ref(rhs_type), no_ref(lhs_type)))
    {
        unary_record_conversion_to_result(lhs_type, rhs);

        return lhs_type;
    }

    return get_error_type();
}

static void compute_bin_operator_assig_only_arithmetic_type(nodecl_t* lhs, nodecl_t* rhs, AST operator,
        decl_context_t decl_context,
        nodecl_t (*nodecl_bin_fun)(nodecl_t, nodecl_t, type_t*, const char*, int),
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    compute_bin_operator_generic(lhs, rhs, 
            operator, 
            decl_context,
            any_operand_is_class_or_enum,
            nodecl_bin_fun,
            NULL, // No constants
            compute_type_no_overload_assig_only_arithmetic_type,
            NULL,
            operator_bin_assign_only_arithmetic_pred,
            operator_bin_assign_only_arithmetic_result,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_mod_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MOD_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, 
            nodecl_make_mod_assignment,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_shl_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LEFT_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_shl_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_shr_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_RIGHT_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    return compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_shr_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_bitwise_and_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)

{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_AND_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_bitwise_and_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_bitwise_or_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_OR_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_bitwise_or_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_bitwise_xor_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_XOR_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_only_integral_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_bitwise_xor_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_mul_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MUL_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }
    
    compute_bin_operator_assig_arithmetic_or_pointer_type(lhs, rhs,
            operation_tree, decl_context, nodecl_make_mul_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ASSIGNMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_nonoperator_assig_only_arithmetic_type(lhs, rhs, 
            operation_tree, decl_context,
            filename, line,
            nodecl_output);
}

static void compute_bin_operator_div_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_DIV_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_arithmetic_or_pointer_type(lhs, rhs,
            operation_tree, decl_context, nodecl_make_div_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_add_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_arithmetic_or_pointer_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_add_assignment,
            filename, line, nodecl_output);
}

static void compute_bin_operator_sub_assig_type(nodecl_t* lhs, nodecl_t* rhs,
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_SUB_ASSIGN_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_bin_operator_assig_arithmetic_or_pointer_type(lhs, rhs, 
            operation_tree, decl_context, nodecl_make_sub_assignment,
            filename, line, nodecl_output);
}

static void compute_unary_operator_generic(
        nodecl_t* op, 
        // Operator name and context
        AST operator,
        decl_context_t decl_context, 
        // Functions
        char (*will_require_overload)(type_t*),
        nodecl_t (*nodecl_unary_fun)(nodecl_t, type_t*, const char *filename, int line),
        const_value_t* (*const_value_unary_fun)(const_value_t*),
        type_t* (*compute_type_no_overload)(nodecl_t*, char* is_lvalue),
        char types_allow_constant_evaluation(type_t*),
        char (*overload_operator_predicate)(type_t*),
        type_t* (*overload_operator_result_types)(type_t**),
        // Whether we have to record conversions
        char save_conversions,
        // Locus
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    type_t* op_type = nodecl_get_type(*op);

    if (nodecl_expr_is_type_dependent(*op))
    {
        *nodecl_output = nodecl_unary_fun(*op, get_unknown_dependent_type(), filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);

        nodecl_expr_set_is_value_dependent(*nodecl_output, nodecl_expr_is_value_dependent(*op));
        return;
    }

    char requires_overload = 0;
    CXX_LANGUAGE()
    {
        type_t* no_ref_op_type = no_ref(op_type);

        // Try to simplify unresolved overloads
        if (is_unresolved_overloaded_type(no_ref_op_type))
        {
            scope_entry_t* function = unresolved_overloaded_type_simplify(no_ref(op_type),
                    decl_context, filename, line);
            if (function != NULL)
            {
                op_type = get_pointer_type(function->type_information);
                nodecl_set_type(*op, op_type);
            }
        }

        requires_overload = will_require_overload(no_ref_op_type);
    }

    if (!requires_overload)
    {
        char is_lvalue = 0;
        type_t* computed_type = compute_type_no_overload(op, &is_lvalue);

        if (is_error_type(computed_type))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        const_value_t* val = NULL;

        if (const_value_unary_fun != NULL
                && types_allow_constant_evaluation(op_type)
                && nodecl_is_constant(*op))
        {
            val = const_value_unary_fun(nodecl_get_constant(*op));
        }

        if (save_conversions
                && !equivalent_types(
                    get_unqualified_type(no_ref(nodecl_get_type(*op))), 
                    get_unqualified_type(no_ref(computed_type))))
        {
            *op = cxx_nodecl_make_conversion(*op, computed_type,
                    nodecl_get_filename(*op),
                    nodecl_get_line(*op));
        }

        *nodecl_output = nodecl_unary_fun(
                *op,
                computed_type, filename, line);

        nodecl_expr_set_is_lvalue(*nodecl_output, is_lvalue);

        if (nodecl_expr_is_value_dependent(*op))
        {
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }

        nodecl_set_constant(*nodecl_output, val);

        return;
    }

    builtin_operators_set_t builtin_set;
    build_unary_builtin_operators(
            op_type,
            &builtin_set,
            decl_context, operator,
            overload_operator_predicate,
            overload_operator_result_types);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* result = compute_user_defined_unary_operator_type(operator,
            op, builtins, decl_context, 
            filename, line, &selected_operator);

    ERROR_CONDITION(result == NULL, "Invalid type", 0);

    entry_list_free(builtins);

    if (selected_operator != NULL)
    {
        if (selected_operator->entity_specs.is_builtin)
        {
            const_value_t* val = NULL;

            if (const_value_unary_fun != NULL
                    && types_allow_constant_evaluation(op_type)
                    && nodecl_is_constant(*op))
            {
                val = const_value_unary_fun(nodecl_get_constant(*op));
            }

            if (save_conversions
                    && !equivalent_types(
                        get_unqualified_type(no_ref(nodecl_get_type(*op))), 
                        get_unqualified_type(no_ref(result))))
            {
                *op = cxx_nodecl_make_conversion(*op, result,
                        nodecl_get_filename(*op),
                        nodecl_get_line(*op));
            }

            *nodecl_output = nodecl_unary_fun(*op, result, filename, line);

            if (nodecl_expr_is_value_dependent(*op))
            {
                nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
            }

            nodecl_set_constant(*nodecl_output, val);
        }
        else
        {
            *nodecl_output = 
                cxx_nodecl_make_function_call(
                        nodecl_make_symbol(selected_operator, filename, line),
                        nodecl_make_list_1(*op),
                        result, filename, line);
        }
    }
    else
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
    }
}

char operator_unary_derref_pred(type_t* op_type)
{
    if (is_pointer_type(no_ref(op_type)))
    {
        return 1;
    }
    return 0;
}

type_t* operator_unary_derref_result(type_t** op_type)
{
    if (is_pointer_type(no_ref(*op_type)))
    {
        return get_lvalue_reference_type(pointer_type_get_pointee_type(no_ref(*op_type)));
    }
    return get_error_type();
}

type_t* compute_type_no_overload_derref(nodecl_t *nodecl_op, char *is_lvalue)
{
    type_t* op_type = nodecl_get_type(*nodecl_op);

    type_t* computed_type = get_error_type();
    if (is_pointer_type(no_ref(op_type))
            && !is_function_type(pointer_type_get_pointee_type(no_ref(op_type))))
    {
        computed_type = lvalue_ref(pointer_type_get_pointee_type(no_ref(op_type)));
        *is_lvalue = 1;
    }
    else if (is_pointer_type(no_ref(op_type))
            && is_function_type(pointer_type_get_pointee_type(no_ref(op_type))))
    {
        // Bypass derreference of pointer to function type
        computed_type = lvalue_ref(op_type);
        *is_lvalue = 1;
    }
    else if (is_array_type(no_ref(op_type)))
    {
        computed_type = lvalue_ref(array_type_get_element_type(no_ref(op_type)));
        *is_lvalue = 1;
    }
    else if (is_function_type(no_ref(op_type)))
    {
        // Create a pointer type
        computed_type = lvalue_ref(get_pointer_type(no_ref(op_type)));
        *is_lvalue = 1;
    }

    return computed_type;
}

static void compute_operator_derreference_type(
        nodecl_t *op, decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t *nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MUL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_unary_operator_generic(op, 
            operation_tree, decl_context,
            operand_is_class_or_enum,
            nodecl_make_derreference,
            NULL, // No constants
            compute_type_no_overload_derref,
            NULL,
            operator_unary_derref_pred,
            operator_unary_derref_result,
            /* save_conversions */ 0,
            filename, line,
            nodecl_output);
}

static char operator_unary_plus_pred(type_t* op_type)
{
    if (is_pointer_type(no_ref(op_type)))
    {
        return 1;
    }
    else if (is_arithmetic_type(no_ref(op_type)))
    {
        return 1;
    }
    return 0;
}

static type_t* operator_unary_plus_result(type_t** op_type)
{
    if (is_pointer_type(no_ref(*op_type)))
    {
        return no_ref(*op_type);
    }
    else if (is_arithmetic_type(no_ref(*op_type)))
    {
        if (is_promoteable_integral_type(no_ref(*op_type)))
        {
            *op_type = promote_integral_type(no_ref(*op_type));
        }

        return no_ref(*op_type);
    }
    return get_error_type();
}

static type_t* compute_type_no_overload_plus(nodecl_t *op, char *is_lvalue)
{
    *is_lvalue = 0;

    type_t* op_type = nodecl_get_type(*op);

    if (is_pointer_type(no_ref(op_type)))
    {
        // Bypass
        return no_ref(op_type);
    }
    else if (is_arithmetic_type(no_ref(op_type)))
    {
        if (is_promoteable_integral_type(no_ref(op_type)))
        {
            return promote_integral_type(no_ref(op_type));
        }
        else
        {
            return no_ref(op_type);
        }
    }
    else if (is_vector_type(no_ref(op_type)))
    {
        return no_ref(op_type);
    }
    else
    {
        return get_error_type();
    }
}

static void compute_operator_plus_type(nodecl_t* op, 
        decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_ADD_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_unary_operator_generic(op, 
            operation_tree, decl_context,
            operand_is_class_or_enum,
            nodecl_make_plus,
            const_value_plus, 
            compute_type_no_overload_plus,
            is_integral_type,
            operator_unary_plus_pred,
            operator_unary_plus_result,
            /* save_conversions */ 1,
            filename, line,
            nodecl_output);
}

static char operator_unary_minus_pred(type_t* op_type)
{
    if (is_arithmetic_type(no_ref(op_type)))
    {
        return 1;
    }
    return 0;
}

static type_t* operator_unary_minus_result(type_t** op_type)
{
    if (is_arithmetic_type(no_ref(*op_type)))
    {
        if (is_promoteable_integral_type(no_ref(*op_type)))
        {
            *op_type = promote_integral_type(no_ref(*op_type));
        }

        return no_ref(*op_type);
    }
    return get_error_type();
}

static type_t* compute_type_no_overload_neg(nodecl_t *op, char *is_lvalue)
{
    *is_lvalue = 0;

    type_t* op_type = nodecl_get_type(*op);

    if (is_arithmetic_type(no_ref(op_type)))
    {
        if (is_promoteable_integral_type(no_ref(op_type)))
        {
            return promote_integral_type(no_ref(op_type));
        }
        else
        {
            return no_ref(op_type);
        }
    }
    else if (is_vector_type(no_ref(op_type)))
    {
        return no_ref(op_type);
    }
    else
    {
        return get_error_type(); 
    }
}

static void compute_operator_minus_type(nodecl_t* op, decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_MINUS_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_unary_operator_generic(op, 
            operation_tree, decl_context,
            operand_is_class_or_enum,
            nodecl_make_neg,
            const_value_neg, 
            compute_type_no_overload_neg,
            is_integral_type,
            operator_unary_minus_pred,
            operator_unary_minus_result,
            /* save_conversions */ 1,
            filename, line,
            nodecl_output);
}

static char operator_unary_complement_pred(type_t* op_type)
{
    if (is_integral_type(no_ref(op_type)))
    {
        return 1;
    }
    return 0;
}

static type_t* operator_unary_complement_result(type_t** op_type)
{
    if (is_integral_type(no_ref(*op_type)))
    {
        if (is_promoteable_integral_type(no_ref(*op_type)))
        {
            *op_type = promote_integral_type(no_ref(*op_type));
        }

        return no_ref(*op_type);
    }
    return get_error_type();
}

static type_t* compute_type_no_overload_complement(nodecl_t *op, char *is_lvalue)
{
    return compute_type_no_overload_neg(op, is_lvalue);
}

static void compute_operator_complement_type(nodecl_t* op, 
        decl_context_t decl_context, 
        const char *filename, int line,
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_NEG_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_unary_operator_generic(op, 
            operation_tree, decl_context,
            operand_is_class_or_enum,
            nodecl_make_bitwise_not,
            const_value_bitnot, 
            compute_type_no_overload_complement,
            is_integral_type,
            operator_unary_complement_pred,
            operator_unary_complement_result,
            /* save_conversions */ 1,
            filename, line,
            nodecl_output);
}

static char operator_unary_not_pred(type_t* op_type)
{
    standard_conversion_t to_bool;

    if (standard_conversion_between_types(&to_bool,
                no_ref(op_type), get_bool_type()))
    {
        return 1;
    }
    return 0;
}

static type_t* operator_unary_not_result(type_t** op_type)
{
    // We want the function to be 'bool operator!(bool)' not 'bool
    // operator!(L)' with L something that can be converted to bool
    *op_type = get_bool_type();

    return *op_type;
}

static type_t* compute_type_no_overload_logical_not(nodecl_t *op, char *is_lvalue)
{
    *is_lvalue = 0;

    type_t* op_type = nodecl_get_type(*op);

    standard_conversion_t to_bool;

    if (standard_conversion_between_types(&to_bool,
                op_type, get_bool_type()))
    {
        C_LANGUAGE()
        {
            return get_signed_int_type();
        }
        CXX_LANGUAGE()
        {
            return get_bool_type();
        }
        internal_error("Code unreachable", 0);
    }
    else
    {
        return get_error_type();
    }
}


static void compute_operator_not_type(nodecl_t* op, 
        decl_context_t decl_context, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_LOGICAL_NOT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    compute_unary_operator_generic(op, 
            operation_tree, decl_context,
            operand_is_class_or_enum,
            nodecl_make_logical_not,
            const_value_not, 
            compute_type_no_overload_logical_not,
            is_integral_type,
            operator_unary_not_pred,
            operator_unary_not_result,
            /* save_conversions */ 1,
            filename, line,
            nodecl_output);
}

static char operator_unary_reference_pred(type_t* t)
{
    return is_lvalue_reference_type(t);
}

static type_t* operator_unary_reference_result(type_t** op_type)
{
    // T* operator&(T&)
    return get_pointer_type(no_ref(*op_type));
}

static type_t* compute_type_no_overload_reference(nodecl_t *op, char *is_lvalue)
{
    *is_lvalue = 0;

    if (nodecl_expr_is_lvalue(*op))
    {
        type_t* t = no_ref(nodecl_get_type(*op));

        return get_pointer_type(t);
    }

    return get_error_type();
}

static void parse_reference(AST op,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        // C++ from now
        // We need this function because this operator is so silly in C++
        if (ASTType(op) == AST_QUALIFIED_ID
                || ASTType(op) == AST_QUALIFIED_TEMPLATE)
        {
            nodecl_t op_name = nodecl_null();
            compute_nodecl_name_from_id_expression(op, decl_context, &op_name);

            if (nodecl_is_err_expr(op_name))
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid qualified name '%s'\n",
                            ast_location(op), prettyprint_in_buffer(op));
                }
                *nodecl_output = nodecl_make_err_expr(ASTFileName(op), ASTLine(op));
                return;
            }

            scope_entry_list_t* entry_list = query_nodecl_name_flags(decl_context, op_name, DF_DEPENDENT_TYPENAME);

            if (entry_list == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid qualified name '%s'\n",
                            ast_location(op), prettyprint_in_buffer(op));
                }
                *nodecl_output = nodecl_make_err_expr(ASTFileName(op), ASTLine(op));
                return;
            }

            scope_entry_t* entry = entry_list_head(entry_list);
            entry_list_free(entry_list);

            if (entry->kind == SK_TEMPLATE)
            {
                entry = named_type_get_symbol(template_type_get_primary_type(entry->type_information));
            }

            if ((entry->kind == SK_VARIABLE
                        || entry->kind == SK_FUNCTION)
                    && entry->entity_specs.is_member
                    && !entry->entity_specs.is_static)
            {
                // This is a pointer to a member
                *nodecl_output = op_name;
                return;
            }
        }
    }

    // Usual check
    check_expression_impl_(op, decl_context, nodecl_output);
}

static void compute_operator_reference_type(nodecl_t* op, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        const char* filename, int line, 
        nodecl_t* nodecl_output)
{
    static AST operation_tree = NULL;
    if (operation_tree == NULL)
    {
        operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_BITWISE_AND_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    // If parse_reference passes us a qualified name we now this is a pointer
    // to member reference
    if (nodecl_get_kind(*op) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED
            || nodecl_get_kind(*op) == NODECL_CXX_DEP_NAME_NESTED)
    {
        scope_entry_list_t* entry_list = query_nodecl_name(decl_context, *op);

        ERROR_CONDITION(entry_list == NULL, "Invalid list", 0);

        scope_entry_t* entry = entry_list_head(entry_list);

        if (entry->kind == SK_TEMPLATE)
        {
            entry = named_type_get_symbol(template_type_get_primary_type(entry->type_information));
        }

        if (entry->kind == SK_VARIABLE)
        {
            *nodecl_output = nodecl_make_pointer_to_member(entry, 
                    get_lvalue_reference_type(
                        get_pointer_to_member_type(entry->type_information,
                            named_type_get_symbol(entry->entity_specs.class_type))),
                    filename, line);
        }
        else if (entry->kind == SK_FUNCTION)
        {
            *nodecl_output = nodecl_make_symbol(entry, filename, line);

            template_parameter_list_t* last_template_args = NULL;
            if (nodecl_name_ends_in_template_id(*op))
            {
                last_template_args = nodecl_name_name_last_template_arguments(*op);
            }

            type_t* t = get_unresolved_overloaded_type(entry_list, last_template_args);

            nodecl_set_type(*nodecl_output, t);
        }
        else
        {
            internal_error("Code unreachable", 0);
        }
        entry_list_free(entry_list);
    }
    else
    {
        // This one is special
        if (is_unresolved_overloaded_type(nodecl_get_type(*op)))
        {
            scope_entry_t* entry = unresolved_overloaded_type_simplify(
                    nodecl_get_type(*op),
                    decl_context,
                    filename, line);
            if (entry != NULL)
            {
                *op = nodecl_make_symbol(entry, filename, line);
                nodecl_set_type(*op, entry->type_information);
                nodecl_expr_set_is_lvalue(*op, 1);
            }
        }

        compute_unary_operator_generic(op, 
                operation_tree, decl_context,
                operand_is_class_or_enum,
                nodecl_make_reference,
                // No constants
                NULL,
                compute_type_no_overload_reference,
                NULL,
                operator_unary_reference_pred,
                operator_unary_reference_result,
                /* save_conversions */ 0,
                filename, line,
                nodecl_output);
    }
}

struct bin_operator_funct_type_t
{
    void (*pre)(AST op, decl_context_t, nodecl_t*);
    void (*func)(nodecl_t* lhs, nodecl_t* rhs, decl_context_t, const char*, int, nodecl_t*);
};

struct unary_operator_funct_type_t
{
    void (*pre)(AST op, decl_context_t, nodecl_t*);
    void (*func)(nodecl_t* operand, decl_context_t, const char*, int, nodecl_t*);
};

#define OPERATOR_FUNCT_INIT(_x) { .pre = check_expression_impl_, .func = _x }
#define OPERATOR_FUNCT_INIT_PRE(_pre, _x) { .pre = _pre, .func = _x }

static struct bin_operator_funct_type_t binary_expression_fun[] =
{
    [AST_ADD]                = OPERATOR_FUNCT_INIT(compute_bin_operator_add_type),
    [AST_MUL]                = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_type),
    [AST_DIV]                = OPERATOR_FUNCT_INIT(compute_bin_operator_div_type),
    [AST_MOD]                = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_type),
    [AST_MINUS]              = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_type),
    [AST_SHL]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_type),
    [AST_SHR]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_type),
    [AST_LOWER_THAN]            = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_than_type),
    [AST_GREATER_THAN]          = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_than_type),
    [AST_GREATER_OR_EQUAL_THAN] = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_equal_type),
    [AST_LOWER_OR_EQUAL_THAN]   = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_equal_type),
    [AST_EQUAL]              = OPERATOR_FUNCT_INIT(compute_bin_operator_equal_type),
    [AST_DIFFERENT]          = OPERATOR_FUNCT_INIT(compute_bin_operator_different_type),
    [AST_BITWISE_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_type),
    [AST_BITWISE_XOR]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_type),
    [AST_BITWISE_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_type),
    [AST_LOGICAL_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_and_type),
    [AST_LOGICAL_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_or_type),
#ifdef FORTRAN_SUPPORT
    [AST_POWER]              = OPERATOR_FUNCT_INIT(compute_bin_operator_pow_type),
#endif
    [AST_ASSIGNMENT]            = OPERATOR_FUNCT_INIT(compute_bin_operator_assig_type),
    [AST_MUL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_assig_type),
    [AST_DIV_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_div_assig_type),
    [AST_ADD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_add_assig_type),
    [AST_SUB_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_assig_type),
    [AST_SHL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_assig_type),
    [AST_SHR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_assig_type),
    [AST_BITWISE_AND_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_assig_type),
    [AST_BITWISE_OR_ASSIGNMENT ]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_assig_type),
    [AST_BITWISE_XOR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_assig_type),
    [AST_MOD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_assig_type),

    // Same as above for nodecl
    [NODECL_ADD]                = OPERATOR_FUNCT_INIT(compute_bin_operator_add_type),
    [NODECL_MUL]                = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_type),
    [NODECL_DIV]                = OPERATOR_FUNCT_INIT(compute_bin_operator_div_type),
    [NODECL_MOD]                = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_type),
    [NODECL_MINUS]              = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_type),
    [NODECL_SHL]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_type),
    [NODECL_SHR]                = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_type),
    [NODECL_LOWER_THAN]            = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_than_type),
    [NODECL_GREATER_THAN]          = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_than_type),
    [NODECL_GREATER_OR_EQUAL_THAN] = OPERATOR_FUNCT_INIT(compute_bin_operator_greater_equal_type),
    [NODECL_LOWER_OR_EQUAL_THAN]   = OPERATOR_FUNCT_INIT(compute_bin_operator_lower_equal_type),
    [NODECL_EQUAL]              = OPERATOR_FUNCT_INIT(compute_bin_operator_equal_type),
    [NODECL_DIFFERENT]          = OPERATOR_FUNCT_INIT(compute_bin_operator_different_type),
    [NODECL_BITWISE_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_type),
    [NODECL_BITWISE_XOR]           = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_type),
    [NODECL_BITWISE_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_type),
    [NODECL_LOGICAL_AND]           = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_and_type),
    [NODECL_LOGICAL_OR]            = OPERATOR_FUNCT_INIT(compute_bin_operator_logical_or_type),
#ifdef FORTRAN_SUPPORT
    [NODECL_POWER]              = OPERATOR_FUNCT_INIT(compute_bin_operator_pow_type),
#endif
    [NODECL_ASSIGNMENT]            = OPERATOR_FUNCT_INIT(compute_bin_operator_assig_type),
    [NODECL_MUL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mul_assig_type),
    [NODECL_DIV_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_div_assig_type),
    [NODECL_ADD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_add_assig_type),
    [NODECL_SUB_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_sub_assig_type),
    [NODECL_SHL_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shl_assig_type),
    [NODECL_SHR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_shr_assig_type),
    [NODECL_BITWISE_AND_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_and_assig_type),
    [NODECL_BITWISE_OR_ASSIGNMENT ]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_or_assig_type),
    [NODECL_BITWISE_XOR_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_bitwise_xor_assig_type),
    [NODECL_MOD_ASSIGNMENT]        = OPERATOR_FUNCT_INIT(compute_bin_operator_mod_assig_type),
};

static struct unary_operator_funct_type_t unary_expression_fun[] =
{
    [AST_DERREFERENCE]          = OPERATOR_FUNCT_INIT(compute_operator_derreference_type),
    [AST_REFERENCE]             = OPERATOR_FUNCT_INIT_PRE(parse_reference, compute_operator_reference_type),
    [AST_PLUS]               = OPERATOR_FUNCT_INIT(compute_operator_plus_type),
    [AST_NEG]                = OPERATOR_FUNCT_INIT(compute_operator_minus_type),
    [AST_LOGICAL_NOT]                = OPERATOR_FUNCT_INIT(compute_operator_not_type),
    [AST_BITWISE_NOT]         = OPERATOR_FUNCT_INIT(compute_operator_complement_type),

    // Same as above for nodecl
    [NODECL_DERREFERENCE]          = OPERATOR_FUNCT_INIT(compute_operator_derreference_type),
    [NODECL_REFERENCE]             = OPERATOR_FUNCT_INIT(compute_operator_reference_type),
    [NODECL_PLUS]               = OPERATOR_FUNCT_INIT(compute_operator_plus_type),
    [NODECL_NEG]                = OPERATOR_FUNCT_INIT(compute_operator_minus_type),
    [NODECL_LOGICAL_NOT]                = OPERATOR_FUNCT_INIT(compute_operator_not_type),
    [NODECL_BITWISE_NOT]         = OPERATOR_FUNCT_INIT(compute_operator_complement_type),
};

static void check_unary_expression_(node_t node_kind,
        nodecl_t* op,
        decl_context_t decl_context,
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    (unary_expression_fun[node_kind].func)(
            op,
            decl_context, 
            filename, line,
            nodecl_output);
}

static void check_binary_expression_(node_t node_kind,
        nodecl_t* nodecl_lhs,
        nodecl_t* nodecl_rhs,
        decl_context_t decl_context,
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    (binary_expression_fun[node_kind].func)(
            nodecl_lhs, nodecl_rhs,
            decl_context, 
            filename, line,
            nodecl_output);
}

static void check_binary_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    nodecl_t nodecl_lhs = nodecl_null();
    nodecl_t nodecl_rhs = nodecl_null();

    node_t node_kind = ASTType(expression);
    (binary_expression_fun[node_kind].pre)(lhs, decl_context, &nodecl_lhs);
    (binary_expression_fun[node_kind].pre)(rhs, decl_context, &nodecl_rhs);

    if (nodecl_is_err_expr(nodecl_lhs)
            || nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
        return;
    }

    check_binary_expression_(ASTType(expression), 
            &nodecl_lhs,
            &nodecl_rhs,
            decl_context,
            ASTFileName(expression), ASTLine(expression),
            nodecl_output);

    if (nodecl_is_err_expr(*nodecl_output))
    {
        if(!checking_ambiguity())
        {
            error_printf("%s: error: binary %s cannot be applied to operands '%s' (of type '%s') and '%s' (of type '%s')\n",
                    ast_location(expression),
                    get_operation_function_name(expression), 
                    prettyprint_in_buffer(lhs), print_type_str(nodecl_get_type(nodecl_lhs), decl_context),
                    prettyprint_in_buffer(rhs), print_type_str(nodecl_get_type(nodecl_rhs), decl_context));
        }
    }
}

static void check_unary_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST op = ASTSon0(expression);

    nodecl_t nodecl_op = nodecl_null();

    node_t node_kind = ASTType(expression);
    (unary_expression_fun[node_kind].pre)(op, decl_context, &nodecl_op);

    if (nodecl_is_err_expr(nodecl_op))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
        return;
    }

    check_unary_expression_(ASTType(expression), 
            &nodecl_op, 
            decl_context, 
            ASTFileName(expression), ASTLine(expression),
            nodecl_output);

    if (nodecl_is_err_expr(*nodecl_output))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: unary %s cannot be applied to operand '%s' (of type '%s')\n",
                    ast_location(expression),
                    get_operation_function_name(expression), 
                    prettyprint_in_buffer(op), print_type_str(nodecl_get_type(nodecl_op), decl_context));
        }
    }
}

static void check_throw_expression_nodecl(nodecl_t nodecl_thrown, const char* filename, int line,
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_throw(nodecl_thrown, get_throw_expr_type(), filename, line);
}

static void check_throw_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_thrown = nodecl_null();
    if (ASTSon0(expression) != NULL)
    {
        check_expression_impl_(ASTSon0(expression), decl_context, &nodecl_thrown);
    }

    check_throw_expression_nodecl(nodecl_thrown, ASTFileName(expression), ASTLine(expression), nodecl_output);
}

static void cxx_common_name_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output);

static void compute_symbol_type(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    CXX_LANGUAGE()
    {
        // C++ names are handled in another routine
        cxx_common_name_check(expr, decl_context, nodecl_output);
        return;
    }

    scope_entry_list_t* result = NULL;
    result = query_nested_name(decl_context, NULL, NULL, expr); 

    // char names_a_builtin = 0;
    // const char *name = ASTText(expr);

    // if (name != NULL
    //         && strncmp(name, builtin_prefix, strlen(builtin_prefix)) == 0)
    // {
    //     names_a_builtin = 1;
    // }

    if (result == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: symbol '%s' not found in current scope\n",
                    ast_location(expr), ASTText(expr));
        }

        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    scope_entry_t* entry = entry_advance_aliases(entry_list_head(result));

    if (entry->kind == SK_ENUMERATOR)
    {
        *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expr), ASTLine(expr));

        if (nodecl_is_constant(entry->value))
        {
            nodecl_set_constant(*nodecl_output, nodecl_get_constant(entry->value));
        }

        nodecl_set_type(*nodecl_output, entry->type_information);
    }
    else if (entry->kind == SK_VARIABLE
            || entry->kind == SK_FUNCTION)
    {
        *nodecl_output = nodecl_make_symbol(entry, ASTFileName(expr), ASTLine(expr));
        if (entry->entity_specs.is_member_of_anonymous)
        {
            nodecl_t accessor = entry->entity_specs.anonymous_accessor;
            *nodecl_output = nodecl_make_class_member_access(
                    accessor,
                    *nodecl_output,
                    entry->type_information,
                    ASTFileName(expr),
                    ASTLine(expr));
        }

        if (entry->kind == SK_VARIABLE
                && !entry->entity_specs.is_parameter
                && is_const_qualified_type(entry->type_information)
                && !nodecl_is_null(entry->value)
                && nodecl_is_constant(entry->value))
        {
            nodecl_set_constant(*nodecl_output, nodecl_get_constant(entry->value));
        }

        nodecl_set_type(*nodecl_output, entry->type_information);
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
    }
    else
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: name '%s' not valid in expression\n",
                    ast_location(expr), ASTText(expr));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
    }

    entry_list_free(result);
}

static void check_symbol(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    compute_symbol_type(expr, decl_context, nodecl_output);
}

static void cxx_compute_name_from_entry_list(nodecl_t nodecl_name, 
        scope_entry_list_t* entry_list, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    if (entry_list != NULL
            && entry_list_size(entry_list) == 1)
    {
        scope_entry_t* entry = entry_list_head(entry_list);
        if (entry->kind == SK_DEPENDENT_ENTITY)
        {
            *nodecl_output = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
            nodecl_set_type(*nodecl_output, entry->type_information);
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
            return;
        }
    }

    entry_list = filter_friend_declared(entry_list);

    if (entry_list == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: symbol '%s' not found in current scope\n",
                    nodecl_get_locus(nodecl_name), c_cxx_codegen_to_str(nodecl_name));
        }
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        return;
    }

    scope_entry_t* entry = entry_advance_aliases(entry_list_head(entry_list));

    if (entry->kind != SK_VARIABLE
            && entry->kind != SK_ENUMERATOR
            && entry->kind != SK_FUNCTION
            && entry->kind != SK_TEMPLATE
            && entry->kind != SK_TEMPLATE_PARAMETER)
    {
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        return;
    }

    template_parameter_list_t* last_template_args = NULL;
    if (nodecl_name_ends_in_template_id(nodecl_name))
    {
        last_template_args = nodecl_name_name_last_template_arguments(nodecl_name);
    }

    if (entry->kind == SK_VARIABLE)
    {
        nodecl_t nodecl_access_to_symbol = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));

        nodecl_set_type(nodecl_access_to_symbol, lvalue_ref(entry->type_information));
        nodecl_expr_set_is_lvalue(nodecl_access_to_symbol, 1);

        scope_entry_t* accessing_symbol = entry;
        if (entry->entity_specs.is_member_of_anonymous)
        {
            nodecl_t accessor = entry->entity_specs.anonymous_accessor;
            nodecl_access_to_symbol = nodecl_make_class_member_access(
                    accessor,
                    nodecl_access_to_symbol,
                    lvalue_ref(entry->type_information),
                    nodecl_get_filename(nodecl_name),
                    nodecl_get_line(nodecl_name));
            accessing_symbol = nodecl_get_symbol(accessor);
        }

        if (!accessing_symbol->entity_specs.is_member
                || accessing_symbol->entity_specs.is_static)
        {
            *nodecl_output = nodecl_access_to_symbol;
        }
        else
        {
            scope_entry_list_t* this_symbol_list 
                = query_name_str(decl_context, "this");
            if (this_symbol_list != NULL)
            {
                scope_entry_t* this_symbol = entry_list_head(this_symbol_list);

                // Construct (*this).x
                type_t* this_type = pointer_type_get_pointee_type(this_symbol->type_information);
                cv_qualifier_t this_qualifier = get_cv_qualifier(this_type);

                nodecl_t nodecl_this_derref = 
                    nodecl_make_derreference(
                            nodecl_make_symbol(this_symbol, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name)),
                            get_lvalue_reference_type(this_type),
                            nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));

                type_t* qualified_data_member_type = entry->type_information;
                if (!entry->entity_specs.is_mutable)
                {
                    qualified_data_member_type = get_cv_qualified_type(qualified_data_member_type, this_qualifier);
                }
                qualified_data_member_type = lvalue_ref(qualified_data_member_type);

                *nodecl_output =
                    nodecl_make_class_member_access(nodecl_this_derref,
                            nodecl_access_to_symbol,
                            qualified_data_member_type,
                            nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
            }
            else
            {
                // Invalid access to a nonstatic member from a "this" lacking context
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: cannot access to nonstatic data member '%s'\n",
                            nodecl_get_locus(nodecl_name),
                            get_qualified_symbol_name(entry, entry->decl_context));
                }
                *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
                return;
            }
            entry_list_free(this_symbol_list);
        }

        nodecl_expr_set_is_lvalue(*nodecl_output, 1);

        if (!entry->entity_specs.is_parameter
                && is_const_qualified_type(no_ref(entry->type_information))
                && !nodecl_is_null(entry->value)
                && nodecl_is_constant(entry->value))
        {
            nodecl_set_constant(*nodecl_output, nodecl_get_constant(entry->value));
        }

        if (!nodecl_is_null(entry->value)
                && nodecl_expr_is_value_dependent(entry->value))
        {
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }

        if (is_dependent_type(entry->type_information))
        {
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }
    }
    else if (entry->kind == SK_ENUMERATOR)
    {
        *nodecl_output = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        nodecl_set_type(*nodecl_output, entry->type_information);

        if (is_dependent_type(entry->type_information)
                || nodecl_expr_is_value_dependent(entry->value))
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Found '%s' at '%s' to be dependent\n",
                        nodecl_get_locus(nodecl_name), c_cxx_codegen_to_str(nodecl_name));
            }
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }
        else
        {
            ERROR_CONDITION(!nodecl_is_constant(entry->value), "This should be constant", 0);
            nodecl_set_constant(*nodecl_output, nodecl_get_constant(entry->value));
        }
    }
    else if (entry->kind == SK_FUNCTION)
    {
        type_t* t = get_unresolved_overloaded_type(entry_list, last_template_args);
        *nodecl_output = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        nodecl_set_type(*nodecl_output, t);

        if (last_template_args != NULL
                && has_dependent_template_parameters(last_template_args))
        {
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        }
    }
    else if (entry->kind == SK_TEMPLATE)
    {
        type_t* primary_named_type = template_type_get_primary_type(entry->type_information);
        scope_entry_t* named_type = named_type_get_symbol(primary_named_type);

        if (named_type->kind != SK_FUNCTION)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: invalid template class-name '%s' in expression\n", 
                        nodecl_get_locus(nodecl_name),
                        c_cxx_codegen_to_str(nodecl_name));
            }
            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
            return;
        }

        type_t* t =  get_unresolved_overloaded_type(entry_list, last_template_args);
        *nodecl_output = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        nodecl_set_type(*nodecl_output, t);

        if (last_template_args != NULL
                && has_dependent_template_parameters(last_template_args))
        {
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        }
    }
    else if (entry->kind == SK_TEMPLATE_PARAMETER)
    {
        *nodecl_output = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_name), nodecl_get_line(nodecl_name));
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);

        // Template parameters may have a dependent type
        if (!is_dependent_type(entry->type_information))
        {
            nodecl_set_type(*nodecl_output, entry->type_information);
        }
        else
        {
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        }
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
    }
}

static void cxx_common_name_check(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(expr, decl_context, &nodecl_name);

    if (nodecl_is_err_expr(nodecl_name))
    {
        *nodecl_output = nodecl_name;
        return;
    }

    scope_entry_list_t* result_list = query_nodecl_name_flags(decl_context, nodecl_name, DF_DEPENDENT_TYPENAME);

    cxx_compute_name_from_entry_list(nodecl_name, result_list, decl_context, nodecl_output);
}

static void check_array_subscript_expr_nodecl(
        nodecl_t nodecl_subscripted, 
        nodecl_t nodecl_subscript, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    const char* filename = nodecl_get_filename(nodecl_subscripted);
    int line = nodecl_get_line(nodecl_subscripted);

    if (nodecl_is_err_expr(nodecl_subscripted)
            || nodecl_is_err_expr(nodecl_subscript))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_subscripted)
            || nodecl_expr_is_type_dependent(nodecl_subscript))
    {
        *nodecl_output = nodecl_make_array_subscript(nodecl_subscripted, 
                nodecl_make_list_1(nodecl_subscript),
                get_unknown_dependent_type(), 
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* subscripted_type = nodecl_get_type(nodecl_subscripted);
    type_t* subscript_type = nodecl_get_type(nodecl_subscript);

    if (is_pointer_and_integral_type(no_ref(subscript_type), no_ref(subscripted_type)))
    {
        // C oddity: since E1[E2] is equivalent to *(E1 + E2) and it is also
        // valid *(E2 + E1), then E2[E1] is valid too
        // Swap everything E1[E2] can be E2[E1] if one is a pointer and the other an integral type
        {
            nodecl_t t = nodecl_subscripted;
            nodecl_subscripted = nodecl_subscript;
            nodecl_subscript = t;
        }

        subscripted_type = nodecl_get_type(nodecl_subscripted);
        subscript_type = nodecl_get_type(nodecl_subscript);
    }

    // Builtin cases
    if (is_array_type(no_ref(subscripted_type)))
    {
        type_t* t = lvalue_ref(array_type_get_element_type(no_ref(subscripted_type)));

        if (nodecl_get_kind(nodecl_subscripted) != NODECL_ARRAY_SUBSCRIPT)
        {
            *nodecl_output = nodecl_make_array_subscript(
                    nodecl_subscripted,
                    nodecl_make_list_1(nodecl_subscript),
                    t, filename, line);
        }
        else
        {
            nodecl_t nodecl_indexed = nodecl_get_child(nodecl_subscripted, 0);
            nodecl_t nodecl_subscript_list = nodecl_get_child(nodecl_subscripted, 1);

            nodecl_subscript_list = nodecl_append_to_list(nodecl_subscript_list, 
                    nodecl_subscript);

            *nodecl_output = nodecl_make_array_subscript(
                    nodecl_indexed,
                    nodecl_subscript_list,
                    t, filename, line);
        }
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        return;
    }
    else if (is_pointer_type(no_ref(subscripted_type)))
    {
        type_t* t = lvalue_ref(pointer_type_get_pointee_type(no_ref(subscripted_type)));

        *nodecl_output = nodecl_make_array_subscript(
                nodecl_subscripted,
                nodecl_make_list_1(nodecl_subscript),
                t, filename, line);

        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        return;
    }
    else
    {
        C_LANGUAGE()
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: expression '%s[%s]' is invalid since '%s' has type '%s' which is neither an array-type or pointer-type\n",
                        nodecl_get_locus(nodecl_subscripted),
                        c_cxx_codegen_to_str(nodecl_subscripted),
                        c_cxx_codegen_to_str(nodecl_subscript),
                        c_cxx_codegen_to_str(nodecl_subscripted),
                        print_type_str(subscripted_type, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
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

        scope_entry_list_t* operator_subscript_list = get_member_of_class_type(
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
                filename, line,
                /* explicit_template_parameters */ NULL);
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
                decl_context, 
                filename, line,
                conversors);

        if (overloaded_call == NULL)
        {
            if (!checking_ambiguity())
            {
                error_message_overload_failed(candidate_set, filename, line);
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (function_has_been_deleted(decl_context, overloaded_call, filename, line))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* param_type = function_type_get_parameter_type_num(overloaded_call->type_information, 0);
        
        if (is_class_type(param_type))
        {
            check_nodecl_expr_initializer(nodecl_subscript, decl_context, param_type, &nodecl_subscript);
        }
        else
        {
            if (conversors[1] != NULL)
            {
                if (function_has_been_deleted(decl_context, conversors[1], filename, line))
                {
                    *nodecl_output = nodecl_make_err_expr(filename, line);
                    return;
                }

                nodecl_subscript = cxx_nodecl_make_function_call(
                        nodecl_make_symbol(conversors[1], filename, line),
                        nodecl_make_list_1(nodecl_subscript),
                        actual_type_of_conversor(conversors[1]), filename, line);

            }
            else if (is_unresolved_overloaded_type(subscript_type))
            {
                scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(subscript_type);
                scope_entry_t* solved_function = address_of_overloaded_function(unresolved_set,
                        unresolved_overloaded_type_get_explicit_template_arguments(subscript_type),
                        param_type,
                        decl_context,
                        nodecl_get_filename(nodecl_subscript), 
                        nodecl_get_line(nodecl_subscript));

                ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    nodecl_subscript =
                        nodecl_make_symbol(solved_function, nodecl_get_filename(nodecl_subscript), nodecl_get_line(nodecl_subscript));
                    nodecl_set_type(nodecl_subscript, lvalue_ref(solved_function->type_information));
                }
                else
                {
                    nodecl_subscript =
                        nodecl_make_pointer_to_member(solved_function, 
                                get_lvalue_reference_type(
                                    get_pointer_to_member_type(solved_function->type_information,
                                        named_type_get_symbol(solved_function->entity_specs.class_type))),
                                nodecl_get_filename(nodecl_subscript), nodecl_get_line(nodecl_subscript));
                }
            }
        }

        type_t* t = function_type_get_return_type(overloaded_call->type_information);

        *nodecl_output = cxx_nodecl_make_function_call(
                nodecl_make_symbol(overloaded_call, filename, line),
                nodecl_make_list_2(nodecl_subscripted, nodecl_subscript),
                t, filename, line);

        nodecl_expr_set_is_lvalue(*nodecl_output, is_lvalue_reference_type(t));
        return;
    }

    if (!checking_ambiguity())
    {
        error_printf("%s: error: in '%s[%s]' no matching operator[] for types '%s'\n",
                nodecl_get_locus(nodecl_subscripted),
                c_cxx_codegen_to_str(nodecl_subscripted),
                c_cxx_codegen_to_str(nodecl_subscript),
                print_type_str(subscripted_type, decl_context));
    }
    *nodecl_output = nodecl_make_err_expr(filename, line);
}

static void check_array_subscript_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_subscripted = nodecl_null();
    check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_subscripted);

    nodecl_t nodecl_subscript = nodecl_null();
    check_expression_impl_(ASTSon1(expr), decl_context, &nodecl_subscript);

    check_array_subscript_expr_nodecl(nodecl_subscripted, nodecl_subscript, decl_context, nodecl_output);
}

static void check_conversion_function_id_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    scope_entry_list_t* entry_list = query_id_expression(decl_context, expression);

    if (entry_list == NULL)
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
        return;
    }

    type_t* conversion_type = NULL;
    /* char* conversion_function_name = */ 
    nodecl_t dummy_nodecl_output = nodecl_null();
    get_conversion_function_name(decl_context, expression, &conversion_type, &dummy_nodecl_output);

    ERROR_CONDITION(conversion_type == NULL,
            "Conversion type was not computed", 0);

    if (is_dependent_type(conversion_type))
    {
        compute_nodecl_name_from_id_expression(expression, decl_context, nodecl_output);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
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
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
        return;
    }

    *nodecl_output = nodecl_make_symbol(found_entry, ASTFileName(expression), ASTLine(expression));
}

static char convert_in_conditional_expr(type_t* from_t1, type_t* to_t2, 
        char *is_ambiguous_conversion,
        decl_context_t decl_context,
        const char* filename,
        int line)
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
                    is_ambiguous_conversion, /* conversor */ NULL,
                    filename, line);
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

static void check_conditional_expression_impl_nodecl_aux(nodecl_t first_op, 
        nodecl_t second_op, 
        nodecl_t third_op, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    const char* filename = nodecl_get_filename(first_op);
    int line = nodecl_get_line(first_op);

    if (nodecl_is_err_expr(first_op)
            || nodecl_is_err_expr(second_op)
            || nodecl_is_err_expr(third_op))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(first_op)
            || nodecl_expr_is_type_dependent(second_op)
            || nodecl_expr_is_type_dependent(third_op))
    {
        *nodecl_output = nodecl_make_conditional_expression(
                first_op,
                second_op,
                third_op,
                get_unknown_dependent_type(), 
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* first_type = nodecl_get_type(first_op);

    type_t* second_type = nodecl_get_type(second_op);
    char second_is_lvalue = nodecl_expr_is_lvalue(second_op);

    type_t* third_type = nodecl_get_type(third_op);
    char third_is_lvalue = nodecl_expr_is_lvalue(third_op);


    nodecl_t nodecl_conditional[3] = { 
        first_op, 
        second_op, 
        third_op 
    };

    type_t* converted_type = NULL;
    C_LANGUAGE()
    {
        if (!is_vector_type(first_type))
        {
            converted_type = get_signed_int_type();
        }
        else
        {
            converted_type = get_vector_type(get_signed_int_type(), vector_type_get_vector_size(first_type));
        }

        standard_conversion_t sc;
        if (!standard_conversion_between_types(&sc, first_type, converted_type))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
    }

    char is_lvalue = 0;

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

            type_t* final_type = NULL;
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
                    final_type = operand_types[1];
                }
                else
                {
                    final_type = operand_types[0];
                }
            }
            else
            {
                /*
                 * b) Both the second and third operands have type void the result is of type void
                 * and is a rvalue
                 */
                final_type = get_void_type();
            }

            *nodecl_output = nodecl_make_conditional_expression(
                    nodecl_conditional[0],
                    nodecl_conditional[1],
                    nodecl_conditional[2],
                    final_type, filename, line);
            
            // Nothing else has to be done for 'void' types
            return;
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
                        decl_context,
                        filename,
                        line);

            char third_to_second_is_ambig = 0;
            char third_to_second = 
                convert_in_conditional_expr(third_type, 
                        second_type, 
                        &third_to_second_is_ambig,
                        decl_context,
                        filename,
                        line);

            if (second_to_third 
                    && third_to_second)
            {
                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }

            if (second_to_third)
            {
                if (second_to_third_is_ambig)
                {
                    *nodecl_output = nodecl_make_err_expr(filename, line);
                    return;
                }

                third_type = second_type;
            }

            if (third_to_second)
            {
                if (third_to_second_is_ambig)
                {
                    *nodecl_output = nodecl_make_err_expr(filename, line);
                    return;
                }

                second_type = third_type;
            }
        }


        /*
         * If the second and third operand do not have the same type 
         * we rely in overload mechanism
         *
         * Note that 'operator?' cannot be overloaded, overloading mechanism
         * is used there to force a conversion

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
                    second_type,
                    third_type,
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
                    decl_context, filename, line, conversors);

            if (overloaded_call == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_message_overload_failed(candidate_set, filename, line);
                }
                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }

            if (function_has_been_deleted(decl_context, overloaded_call, filename, line))
            {
                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }

            int k;
            for (k = 0; k < 3; k++)
            {
                if (conversors[k] != NULL)
                {
                    if (function_has_been_deleted(decl_context, conversors[k], filename, line))
                    {
                        *nodecl_output = nodecl_make_err_expr(filename, line);
                        return;
                    }

                    nodecl_conditional[k] = cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversors[k], filename, line),
                            nodecl_make_list_1(nodecl_conditional[k]),
                            actual_type_of_conversor(conversors[k]),
                            filename, line);
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
            is_lvalue = 1;
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
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    CXX_LANGUAGE()
    {
        // It could be that the final type is a reference
        // or the whole expression was already a lvalue. 
        // Keep in synch lvalueness with reference types
        if (is_lvalue ||
                is_lvalue_reference_type(final_type))
        {
            final_type = lvalue_ref(final_type);
            is_lvalue = 1;
        }
    }

    *nodecl_output = nodecl_make_conditional_expression(
            nodecl_conditional[0],
            nodecl_conditional[1],
            nodecl_conditional[2],
            final_type, filename, line);

    nodecl_expr_set_is_lvalue(*nodecl_output, is_lvalue);
}

static void check_conditional_expression_impl_nodecl(nodecl_t first_op, 
        nodecl_t second_op, 
        nodecl_t third_op, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    check_conditional_expression_impl_nodecl_aux(first_op, 
            second_op,
            third_op,
            decl_context,
            nodecl_output);

    if (nodecl_is_err_expr(*nodecl_output))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: ternary operand '?' cannot be applied to first operand '%s' (of type '%s'), "
                    "second operand '%s' (of type '%s') and third operand '%s' (of type '%s')\n",
                    nodecl_get_locus(first_op),
                    c_cxx_codegen_to_str(first_op), print_type_str(nodecl_get_type(first_op), decl_context),
                    c_cxx_codegen_to_str(second_op), print_type_str(nodecl_get_type(second_op), decl_context),
                    c_cxx_codegen_to_str(third_op), print_type_str(nodecl_get_type(third_op), decl_context));
        }
    }
    else
    {
        if (nodecl_is_constant(first_op)
                && nodecl_is_constant(second_op)
                && nodecl_is_constant(third_op))
        {
            if (const_value_is_nonzero(nodecl_get_constant(first_op)))
            {
                nodecl_set_constant(*nodecl_output, nodecl_get_constant(second_op));
            }
            else
            {
                nodecl_set_constant(*nodecl_output, nodecl_get_constant(third_op));
            }
        }

        if (nodecl_expr_is_value_dependent(first_op)
                || nodecl_expr_is_value_dependent(second_op)
                || nodecl_expr_is_value_dependent(third_op))
        {
            nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        }
    }
}

static void check_conditional_expression_impl(AST expression UNUSED_PARAMETER, 
        AST first_op, AST second_op, AST third_op, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    /*
     * This is more complex that it might seem at first ...
     */

    nodecl_t nodecl_first_op = nodecl_null();
    check_expression_impl_(first_op, decl_context, &nodecl_first_op);
    nodecl_t nodecl_second_op = nodecl_null();
    check_expression_impl_(second_op, decl_context, &nodecl_second_op);
    nodecl_t nodecl_third_op = nodecl_null();
    check_expression_impl_(third_op, decl_context, &nodecl_third_op);

    check_conditional_expression_impl_nodecl(nodecl_first_op, nodecl_second_op, nodecl_third_op, 
            decl_context, nodecl_output);
}


static void check_conditional_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    check_conditional_expression_impl(expression, 
            first_op, second_op, third_op, decl_context, nodecl_output);
}

UNUSED_PARAMETER static void check_default_constructor(type_t* t, 
        decl_context_t decl_context,
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(!is_class_type(t), "Invalid type", 0);

    int num_arguments = 0;
    type_t** arguments = NULL;

    scope_entry_list_t* candidates = NULL;
    scope_entry_t* chosen_constructor = solve_constructor(t,
            arguments, num_arguments,
            /* is_explicit */ 1,
            decl_context,
            filename, line,
            /* conversors */ NULL,
            &candidates);

    if (chosen_constructor == NULL)
    {
        if (entry_list_size(candidates) != 0)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: no default constructor for class type '%s'\n",
                        filename, line,
                        print_type_str(t, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
        }
        entry_list_free(candidates);
        return;
    }
    else
    {
        entry_list_free(candidates);
        if (function_has_been_deleted(decl_context, chosen_constructor, filename, line))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        *nodecl_output = cxx_nodecl_make_function_call(
                nodecl_make_symbol(chosen_constructor, filename, line),
                nodecl_null(),
                actual_type_of_conversor(chosen_constructor),
                filename, line);
    }
}

static void check_nodecl_initializer_clause(nodecl_t initializer_clause, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);
static void check_nodecl_equal_initializer(nodecl_t equal_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);
void check_nodecl_expr_initializer(nodecl_t expr, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);
static void check_nodecl_braced_initializer(nodecl_t braced_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);
static void check_nodecl_designated_initializer(nodecl_t braced_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);
static void check_nodecl_parenthesized_initializer(nodecl_t direct_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output);

static void check_new_expression_impl(
        nodecl_t nodecl_placement_list, 
        nodecl_t nodecl_init_list, 
        type_t* new_type, 
        char is_global,
        decl_context_t decl_context,
        const char *filename,
        int line,
        nodecl_t* nodecl_output)
{
    if (is_dependent_type(new_type))
    {
        // The new type is dependent
        *nodecl_output = nodecl_make_new(
                nodecl_init_list, 
                nodecl_placement_list,
                /* allocation function */ nodecl_null(),
                get_unknown_dependent_type(), 
                is_global ? "global" : "",
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    char is_new_array = is_array_type(new_type);

    scope_entry_t* conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(conversors, 0, sizeof(conversors));

    type_t* arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(arguments, 0, sizeof(arguments));

    char has_dependent_placement_args = 0;

    int num_placement_items = 0;
    if (!nodecl_is_null(nodecl_placement_list))
    {
        nodecl_t* nodecl_list = nodecl_unpack_list(nodecl_placement_list, &num_placement_items);

        int i;
        for (i = 0; i < num_placement_items; i++)
        {
            if (nodecl_expr_is_type_dependent(nodecl_list[i]))
            {
                has_dependent_placement_args = 1;
            }
            // 0 -> this
            // 1 -> size_t
            arguments[i + 2] = nodecl_get_type(nodecl_list[i]);
        }

        free(nodecl_list);
    }

    nodecl_t nodecl_allocation_function = nodecl_null();
    nodecl_t nodecl_placement_list_out = nodecl_null();

    if (has_dependent_placement_args)
    {
        // Well, not all the whole new is type dependent but the placement arguments are
        nodecl_expr_set_is_type_dependent(nodecl_placement_list, 1);
    }
    else 
    {
        // At least the size_t parameter (+1 because we may need an implicit)
        int num_arguments = 2;
        // Note: arguments[0] will be left as NULL since 'operator new' is
        // always static if it is a member function
        arguments[1] = get_size_t_type();
        num_arguments += num_placement_items;

        decl_context_t op_new_context = decl_context;

        if (is_class_type(new_type)
                && !is_global)
        {
            // Instantiate the class if needed
            if (is_named_class_type(new_type)
                    && class_type_is_incomplete_independent(new_type))
            {
                scope_entry_t* symbol = named_type_get_symbol(new_type);
                instantiate_template_class(symbol, decl_context, filename, line);
            }

            op_new_context = class_type_get_inner_context(new_type);
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

        if (is_new_array)
        {
            called_operation_new_tree = operation_new_array_tree;
        }

        scope_entry_list_t *operator_new_list = query_id_expression(op_new_context, called_operation_new_tree);

        if (operator_new_list == NULL)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: no suitable '%s' has been found in the scope\n",
                        filename, line,
                        prettyprint_in_buffer(called_operation_new_tree));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        candidate_t* candidate_set = NULL;
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_new_list);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            scope_entry_t* orig_entry = entry_list_iterator_current(it);
            scope_entry_t* entry = entry_advance_aliases(orig_entry);
            if (entry->entity_specs.is_member)
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        orig_entry,
                        num_arguments,
                        arguments);
            }
            else
            {
                candidate_set = add_to_candidate_set(candidate_set,
                        orig_entry,
                        num_arguments - 1,
                        arguments + 1);
            }
        }
        entry_list_iterator_free(it);

        scope_entry_t* chosen_operator_new = solve_overload(candidate_set, 
                decl_context, filename, line,
                conversors);

        if (chosen_operator_new == NULL)
        {
            if (!checking_ambiguity())
            {
                // Format a nice message
                const char* argument_call = uniquestr("");

                argument_call = strappend(argument_call, "operator new");
                if (is_new_array)
                {
                    argument_call = strappend(argument_call, "[]");
                }
                argument_call = strappend(argument_call, "(");

                int i;
                for (i = 1; i < num_arguments; i++)
                {
                    argument_call = strappend(argument_call, print_type_str(arguments[i], decl_context));
                    if ((i + 1) < num_arguments)
                    {
                        argument_call = strappend(argument_call, ", ");
                    }
                }
                argument_call = strappend(argument_call, ")");

                error_printf("%s:%d: error: no suitable '%s' found for new-expression\n",
                        filename, line,
                        argument_call);

                diagnostic_candidates(operator_new_list, filename, line);
                entry_list_free(operator_new_list);
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (function_has_been_deleted(decl_context, chosen_operator_new, filename, line))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        // Store conversions
        if (!nodecl_is_null(nodecl_placement_list))
        {
            int num_items = 0, i;
            nodecl_t* list = nodecl_unpack_list(nodecl_placement_list, &num_items);

            for (i = 0; i < num_items; i++)
            {
                int j = i + 2;

                nodecl_t nodecl_expr = list[i];

                type_t* param_type = function_type_get_parameter_type_num(chosen_operator_new->type_information, i);

                if (is_class_type(param_type))
                {
                    check_nodecl_expr_initializer(nodecl_expr, decl_context, param_type, &nodecl_expr);
                }
                else
                {
                    if (conversors[j] != NULL)
                    {
                        if (function_has_been_deleted(decl_context, conversors[j], filename, line))
                        {
                            *nodecl_output = nodecl_make_err_expr(filename, line);
                            return;
                        }

                        nodecl_expr = cxx_nodecl_make_function_call(
                                nodecl_make_symbol(conversors[j], nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr)),
                                nodecl_make_list_1(nodecl_expr),
                                actual_type_of_conversor(conversors[j]),
                                nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                    }
                    else if (is_unresolved_overloaded_type(nodecl_get_type(nodecl_expr)))
                    {
                        type_t* arg_type = nodecl_get_type(nodecl_expr);

                        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(arg_type);
                        scope_entry_t* solved_function = address_of_overloaded_function(unresolved_set,
                                unresolved_overloaded_type_get_explicit_template_arguments(arg_type),
                                no_ref(arg_type),
                                decl_context,
                                nodecl_get_filename(nodecl_expr), 
                                nodecl_get_line(nodecl_expr));

                        ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                        if (!solved_function->entity_specs.is_member
                                || solved_function->entity_specs.is_static)
                        {
                            nodecl_expr =
                                nodecl_make_symbol(solved_function, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                            nodecl_set_type(nodecl_expr, lvalue_ref(solved_function->type_information));
                        }
                        else
                        {
                            nodecl_expr =
                                nodecl_make_pointer_to_member(solved_function, 
                                        get_lvalue_reference_type(
                                            get_pointer_to_member_type(solved_function->type_information,
                                                named_type_get_symbol(solved_function->entity_specs.class_type))),
                                        nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                        }
                    }
                }

                nodecl_placement_list_out = nodecl_append_to_list(nodecl_placement_list_out,
                        nodecl_expr);
            }

            free(list);
        }

        nodecl_allocation_function = nodecl_make_symbol(chosen_operator_new, filename, line);
    }

    nodecl_t nodecl_init_out = nodecl_null();

    // Verify the initializer
    check_nodecl_parenthesized_initializer(nodecl_init_list,
            decl_context,
            new_type,
            &nodecl_init_out);

    type_t* synthesized_type = new_type;

    if (is_array_type(new_type))
    {
        synthesized_type = get_pointer_type(array_type_get_element_type(new_type));
    }
    else
    {
        synthesized_type = get_pointer_type(new_type);
    }

    nodecl_t nodecl_new = nodecl_make_new(
            nodecl_init_out, 
            nodecl_placement_list_out,
            nodecl_allocation_function,
            synthesized_type,
            is_global ? "global" : "",
            filename, line);

    *nodecl_output = nodecl_new;
}

static void check_new_expression(AST new_expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(new_expr);
    int line = ASTLine(new_expr);

    AST global_op = ASTSon0(new_expr);
    AST new_placement = ASTSon1(new_expr);
    AST new_type_id = ASTSon2(new_expr);
    AST new_initializer = ASTSon3(new_expr);

    nodecl_t nodecl_placement = nodecl_null();

    if (new_placement != NULL)
    {
        AST expression_list = ASTSon0(new_placement);

        if (!check_expression_list(expression_list, decl_context, &nodecl_placement))
        {
            nodecl_free(nodecl_placement);

            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
    }

    AST type_specifier_seq = ASTSon0(new_type_id);
    AST new_declarator = ASTSon1(new_type_id);

    type_t* dummy_type = NULL;
    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    nodecl_t dummy_nodecl_output = nodecl_null();
    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &dummy_type, decl_context, &dummy_nodecl_output);

    if (is_error_type(dummy_type))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* declarator_type = NULL;
    compute_declarator_type(new_declarator, &gather_info, dummy_type, &declarator_type, decl_context, &dummy_nodecl_output);

    nodecl_t nodecl_init_list = nodecl_null();
    if (new_initializer != NULL)
    {
        AST expression_list = ASTSon0(new_initializer);

        if (expression_list != NULL)
        {
            if (!check_expression_list(expression_list, decl_context, &nodecl_init_list))
            {
                nodecl_free(nodecl_init_list);
                nodecl_free(nodecl_placement);

                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }
        }
    }
    nodecl_init_list = nodecl_make_cxx_parenthesized_initializer(nodecl_init_list, filename, line);

    check_new_expression_impl(nodecl_placement, 
            nodecl_init_list, 
            declarator_type, 
            /* is_global */ global_op != NULL,
            decl_context, 
            filename, 
            line, 
            nodecl_output);
}

static void check_new_type_id_expr(AST new_expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    check_new_expression(new_expr, decl_context, nodecl_output);
}

UNUSED_PARAMETER static char is_deallocation_function(scope_entry_t* entry)
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

static void check_delete_expression_nodecl(nodecl_t nodecl_deleted_expr,
        decl_context_t decl_context UNUSED_PARAMETER,
        const char* filename, int line,
        char is_array_delete,
        nodecl_t* nodecl_output)
{
    // FIXME - We are not calling the deallocation function
    type_t* deleted_type = no_ref(nodecl_get_type(nodecl_deleted_expr));

    if (!is_dependent_type(deleted_type))
    {
        if (!is_pointer_type(deleted_type)
                || is_pointer_to_function_type(deleted_type)
                || is_pointer_to_member_type(deleted_type))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: invalid type '%s' for delete%s expression\n",
                        filename, line,
                        is_array_delete ? "[]" : "",
                        print_type_str(deleted_type, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* full_type = pointer_type_get_pointee_type(deleted_type);
        if (!is_complete_type(full_type))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: invalid incomplete type '%s' in delete%s expression\n",
                        filename, line,
                        is_array_delete ? "[]" : "",
                        print_type_str(full_type, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        nodecl_set_type(nodecl_deleted_expr, full_type);
    }

    if (is_array_delete)
    {
        *nodecl_output = nodecl_make_delete_array(nodecl_deleted_expr, get_void_type(),
                filename, line);
    }
    else
    {
        *nodecl_output = nodecl_make_delete(nodecl_deleted_expr, get_void_type(),
                filename, line);
    }
}


static void check_delete_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    char is_array_delete = 0;
    if (ASTType(expression) == AST_DELETE_ARRAY_EXPR)
    {
        is_array_delete = 1;
    }

    AST deleted_expression = ASTSon1(expression);

    nodecl_t nodecl_deleted_expr = nodecl_null();
    check_expression_impl_(deleted_expression, decl_context, &nodecl_deleted_expr);

    if (nodecl_is_err_expr(nodecl_deleted_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
        return;
    }

    check_delete_expression_nodecl(nodecl_deleted_expr, decl_context, 
            ASTFileName(expression), 
            ASTLine(expression),
            is_array_delete,
            nodecl_output);
}

static void check_nodecl_explicit_type_conversion(type_t* type_info,
        nodecl_t parenthesized_init, decl_context_t decl_context,
        nodecl_t* nodecl_output,
        const char* filename,
        int line)
{
    check_nodecl_parenthesized_initializer(parenthesized_init, decl_context, type_info, nodecl_output);

    if (nodecl_is_err_expr(*nodecl_output))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (is_dependent_type(type_info))
    {
        *nodecl_output = 
            nodecl_make_cxx_explicit_type_cast(*nodecl_output, type_info, filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
    }
    if (is_dependent_type(type_info)
            || nodecl_expr_is_value_dependent(*nodecl_output))
    {
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
    }
}

static void check_explicit_type_conversion_common(type_t* type_info, 
        AST expr, AST expression_list, decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr_list = nodecl_null();

    check_expression_list(expression_list, decl_context, &nodecl_expr_list);

    if (!nodecl_is_null(nodecl_expr_list) 
            && nodecl_is_err_expr(nodecl_expr_list))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression_list), ASTLine(expression_list));
        return;
    }

    nodecl_expr_list = nodecl_make_cxx_parenthesized_initializer(nodecl_expr_list, ASTFileName(expr), ASTLine(expr));

    check_nodecl_explicit_type_conversion(type_info, nodecl_expr_list, decl_context,
            nodecl_output, ASTFileName(expr), ASTLine(expr));
}

static void check_explicit_typename_type_conversion(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST id_expression = ASTSon0(expr);

    scope_entry_list_t* entry_list = query_id_expression(decl_context, id_expression);

    if (entry_list == NULL)
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (entry->kind != SK_TYPEDEF
            && entry->kind != SK_ENUM
            && entry->kind != SK_CLASS
            && entry->kind != SK_DEPENDENT_ENTITY
            && entry->kind != SK_TEMPLATE_TYPE_PARAMETER)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' does not name a type\n",
                    ast_location(expr),
                    prettyprint_in_buffer(id_expression));
        }
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST expr_list = ASTSon1(expr);

    check_explicit_type_conversion_common(get_user_defined_type(entry), expr, expr_list, decl_context, nodecl_output);
}

static void check_explicit_type_conversion(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // An explicit type conversion is of the form
    //
    //   T ( e );
    //
    // T has to be a valid typename
    AST type_specifier_seq = ASTSon0(expr);

    type_t *type_info = NULL;

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    nodecl_t dummy_nodecl_output = nodecl_null();
    build_scope_decl_specifier_seq(type_specifier_seq, &gather_info, &type_info, decl_context, &dummy_nodecl_output);

    if (is_error_type(type_info))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST expression_list = ASTSon1(expr);
    check_explicit_type_conversion_common(type_info, expr, expression_list, decl_context, nodecl_output);
}

void check_function_arguments(AST arguments, decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_null();

    int i;
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
            if (ASTType(iter) == AST_AMBIGUITY)
            {
                solve_ambiguous_expression_list(iter, decl_context);
            }

            AST parameter_expr = ASTSon1(iter);

            nodecl_t nodecl_expr = nodecl_null();
            check_expression_impl_(parameter_expr, decl_context, &nodecl_expr);

            if (nodecl_is_err_expr(nodecl_expr))
            {
                DEBUG_CODE()
                {
                    fprintf(stderr, "EXPRTYPE: When checking function call, argument %d '%s' could not be checked\n",
                            i, prettyprint_in_buffer(parameter_expr));
                }
                *nodecl_output = nodecl_make_err_expr(ASTFileName(parameter_expr), ASTLine(parameter_expr));
                return;
            }
            i++;

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_expr);
        }
    }
}

static scope_entry_list_t* do_koenig_lookup(nodecl_t nodecl_simple_name, 
        nodecl_t nodecl_argument_list, 
        decl_context_t decl_context)
{
    // First try to do a normal lookup
    scope_entry_list_t* entry_list = query_name_str(decl_context, 
            nodecl_get_text(nodecl_simple_name));

    enum cxx_symbol_kind filter_function_names[] = 
    {
        SK_VARIABLE,
        SK_FUNCTION,
        SK_TEMPLATE, 
        SK_DEPENDENT_ENTITY,
        SK_USING,
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
            scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));

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
            return NULL;
        }

        if (!still_requires_koenig)
        {
            // No koenig needed
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Not Koenig will be performed since we found something that "
                        "is member or pointer to function type or dependent entity\n");
            }
            return NULL;
        }
    }

    // Build types of arguments
    type_t* argument_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    int num_arguments = 0;

    memset(argument_types, 0, sizeof(argument_types));

    if (!nodecl_is_null(nodecl_argument_list))
    {
        int num_items = 0, i;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);

        for (i = 0; i < num_items; i++)
        {
            nodecl_t nodecl_arg = list[i];

            type_t* argument_type = nodecl_get_type(nodecl_arg);

            if (is_unresolved_overloaded_type(argument_type))
            {
                // If possible, simplify it
                scope_entry_t* entry =
                    unresolved_overloaded_type_simplify(argument_type, decl_context, 
                            nodecl_get_filename(nodecl_arg), 
                            nodecl_get_line(nodecl_arg));
                if (entry != NULL)
                {
                    nodecl_t nodecl_argument = nodecl_null();
                    if (!entry->entity_specs.is_member
                            || entry->entity_specs.is_static)
                    {
                        argument_type = get_lvalue_reference_type(entry->type_information);
                        nodecl_argument = nodecl_make_symbol(entry, nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                    }
                    else
                    {
                        argument_type = get_lvalue_reference_type(
                                get_pointer_to_member_type(entry->type_information,
                                    named_type_get_symbol(entry->entity_specs.class_type)));
                        nodecl_argument = nodecl_make_pointer_to_member(entry, 
                                argument_type,
                                nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                    }

                    nodecl_arg = nodecl_argument;
                }

                argument_type = nodecl_get_type(nodecl_arg);
            }

            ERROR_CONDITION(num_arguments >= MCXX_MAX_FUNCTION_CALL_ARGUMENTS, "Too many arguments", 0);

            argument_types[num_arguments] = argument_type;
            num_arguments++;
        }
        free(list);
    }

    entry_list_free(entry_list);
    entry_list = koenig_lookup(
            num_arguments,
            argument_types,
            decl_context,
            nodecl_simple_name);

    old_entry_list = entry_list;
    entry_list = filter_symbol_kind_set(old_entry_list,
            STATIC_ARRAY_LENGTH(filter_function_names), filter_function_names);
    entry_list_free(old_entry_list);

    // Remove friend declared
    old_entry_list = entry_list;
    entry_list = filter_friend_declared(entry_list);
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
            scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));
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

    return entry_list;
}

typedef
struct check_arg_data_tag
{
    decl_context_t decl_context;
} check_arg_data_t;


static char arg_type_is_ok_for_param_type_c(type_t* arg_type, type_t* param_type, 
        int num_parameter, nodecl_t *arg UNUSED_PARAMETER, void *data UNUSED_PARAMETER)
{
    check_arg_data_t* p = (check_arg_data_t*)data;

    standard_conversion_t result;
    if (!standard_conversion_between_types(&result, arg_type, param_type))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: argument %d of type '%s' cannot be "
                    "converted to type '%s' of parameter\n",
                    nodecl_get_locus(*arg),
                    num_parameter,
                    print_type_str(arg_type, p->decl_context),
                    print_type_str(param_type, p->decl_context));
        }
        return 0;
    }
    return 1;
}

static char arg_type_is_ok_for_param_type_cxx(type_t* arg_type, type_t* param_type, 
        int num_parameter, nodecl_t *arg, void* data)
{
    check_arg_data_t* p = (check_arg_data_t*)data;

    nodecl_t nodecl_result = nodecl_null();
    check_nodecl_expr_initializer(*arg, p->decl_context, param_type, &nodecl_result);

    if (nodecl_is_err_expr(nodecl_result))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: argument %d of type '%s' cannot be "
                    "converted to type '%s' of parameter\n",
                    nodecl_get_locus(*arg),
                    num_parameter,
                    print_type_str(arg_type, p->decl_context),
                    print_type_str(param_type, p->decl_context));
        }
        return 0;
    }
    else
    {
        *arg = nodecl_result;
    }
    return 1;
}

static char check_argument_types_of_call(
        type_t* function_type,
        nodecl_t nodecl_argument_list,
        char (*arg_type_is_ok_for_param_type)(type_t* argument_type, type_t* parameter_type, int num_parameter, nodecl_t *arg, void*),
        const char* filename, int line,
        void *data,
        nodecl_t* nodecl_output_argument_list)
{
    ERROR_CONDITION(!is_function_type(function_type), "This is not a function type", 0);

    int num_explicit_arguments = nodecl_list_length(nodecl_argument_list);

    int num_args_to_check = num_explicit_arguments;

    if (!function_type_get_lacking_prototype(function_type))
    {
        if (!function_type_get_has_ellipsis(function_type))
        {
            if (num_explicit_arguments != function_type_get_num_parameters(function_type))
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: call using %d arguments to a function with %d parameters\n",
                            filename, line,
                            num_explicit_arguments,
                            function_type_get_num_parameters(function_type));
                }
                return 0;
            }
        }
        else
        {
            int min_arguments = function_type_get_num_parameters(function_type) - 1;
            num_args_to_check = min_arguments;

            if (num_explicit_arguments < min_arguments)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: call with %d arguments for a function with at least %d parameters\n",
                            filename, line,
                            num_explicit_arguments,
                            min_arguments);
                }
                return 0;
            }
        }
    }

    *nodecl_output_argument_list = nodecl_null();

    char no_arg_is_faulty = 1;
    if (!nodecl_is_null(nodecl_argument_list))
    {
        int i, num_elements = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_elements);

        for (i = 0; i < num_elements; i++)
        {
            nodecl_t arg = list[i];
            
            // Ellipsis is not to be checked
            if (i < num_args_to_check) 
            {
                type_t* arg_type = nodecl_get_type(arg);
                type_t* param_type = NULL;
                if (!function_type_get_lacking_prototype(function_type))
                {
                    param_type = function_type_get_parameter_type_num(function_type, i);
                }
                else
                {
                    param_type = get_signed_int_type();
                }

                if (!arg_type_is_ok_for_param_type(arg_type, param_type, i, &arg, data))
                {
                    no_arg_is_faulty = 0;
                }
            }
            *nodecl_output_argument_list = nodecl_append_to_list(*nodecl_output_argument_list,
                    arg);
        }

        free(list);
    }

    return no_arg_is_faulty;
}

UNUSED_PARAMETER static char any_is_member_function(scope_entry_list_t* candidates)
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

void check_nodecl_function_call(nodecl_t nodecl_called, 
        nodecl_t nodecl_argument_list, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    const char* filename = nodecl_get_filename(nodecl_called);
    int line = nodecl_get_line(nodecl_called);

    C_LANGUAGE()
    {
        type_t* called_type = nodecl_get_type(nodecl_called);
        if (!is_function_type(called_type)
                && !is_pointer_to_function_type(called_type))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: expression '%s' cannot be called\n", 
                        nodecl_get_locus(nodecl_called),
                        c_cxx_codegen_to_str(nodecl_called));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (is_pointer_to_function_type(called_type))
            called_type = pointer_type_get_pointee_type(called_type);

        check_arg_data_t data;
        data.decl_context = decl_context;

        nodecl_t nodecl_argument_list_output = nodecl_null();
        if (!check_argument_types_of_call(called_type,
                nodecl_argument_list,
                arg_type_is_ok_for_param_type_c,
                filename, line,
                &data,
                &nodecl_argument_list_output))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* return_type = function_type_get_return_type(called_type);

        // Everything else seems fine
        *nodecl_output = nodecl_make_function_call(
                nodecl_called,
                nodecl_argument_list_output,
                return_type,
                filename, line);
        return;
    }

    // From here only C++
    // Let's check the called entity
    //  - If it is a NODECL_CXX_DEP_NAME_SIMPLE it will require Koenig lookup
    scope_entry_list_t* candidates = NULL;
    template_parameter_list_t* explicit_template_arguments = NULL;
    type_t* called_type = NULL;
    if (nodecl_get_kind(nodecl_called) == NODECL_CXX_DEP_NAME_SIMPLE)
    {
        candidates = do_koenig_lookup(nodecl_called, nodecl_argument_list, decl_context);

        if (candidates == NULL)
        {
            // Try a plain lookup
            candidates = query_nodecl_name_flags(decl_context, nodecl_called, DF_DEPENDENT_TYPENAME);
        }

        if (candidates == NULL)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: called name '%s' not found in the current scope\n",
                        filename, line,
                        c_cxx_codegen_to_str(nodecl_called));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
        else
        {
            cxx_compute_name_from_entry_list(nodecl_copy(nodecl_called), candidates, decl_context, &nodecl_called);
        }
    }

    if (nodecl_expr_is_type_dependent(nodecl_called)
            // It may be value dependent if we are calling a nontype template
            // parameter with type pointer to function
            || nodecl_expr_is_value_dependent(nodecl_called))
    {
        *nodecl_output = nodecl_make_function_call(nodecl_called,
                nodecl_argument_list,
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }
    else if (is_unresolved_overloaded_type(nodecl_get_type(nodecl_called)))
    {
        type_t* unresolved_type = nodecl_get_type(nodecl_called);
        candidates = unresolved_overloaded_type_get_overload_set(unresolved_type);
        explicit_template_arguments = unresolved_overloaded_type_get_explicit_template_arguments(unresolved_type);
    }
    else
    {
        called_type = nodecl_get_type(nodecl_called);
    }

    // This 1+ is room for the implicit argument
    int num_arguments = 1 + nodecl_list_length(nodecl_argument_list);
    type_t* argument_types[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { NULL };

    // Fill the argument_types here
    {
        int num_items = 0, i;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);
        for (i = 0; i < num_items; i++)
        {
            nodecl_t nodecl_arg = list[i];
            // This +1 is because 0 is reserved for the implicit argument type
            argument_types[i + 1] = nodecl_get_type(nodecl_arg);
        }
        free(list);
    }

    // This will be filled later
    nodecl_t nodecl_implicit_argument = nodecl_null();

    // We already know the called type
    // When calling a function using a function name
    // called_type is null
    if (called_type != NULL)
    {
        if (!is_class_type(no_ref(called_type))
                && !is_function_type(no_ref(called_type))
                && !is_pointer_to_function_type(no_ref(called_type)))
        {
            // This cannot be called at all
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: expression '%s' cannot be called\n",
                        filename, line,
                        c_cxx_codegen_to_str(nodecl_called));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
        else if (is_function_type(no_ref(called_type))
                    || is_pointer_to_function_type(no_ref(called_type)))
        {
            // This is a C style check, no overload is involved here
            type_t* function_type = no_ref(called_type);
            if (is_pointer_to_function_type(no_ref(function_type)))
                function_type = pointer_type_get_pointee_type(no_ref(function_type));

            check_arg_data_t data;
            data.decl_context = decl_context;

            nodecl_t nodecl_argument_list_output = nodecl_null();

            if (!check_argument_types_of_call(function_type,
                        nodecl_argument_list,
                        arg_type_is_ok_for_param_type_cxx,
                        filename, line,
                        &data,
                        &nodecl_argument_list_output))
            {
                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }

            type_t* return_type = function_type_get_return_type(function_type);

            // Everything seems fine here
            *nodecl_output = nodecl_make_function_call(
                    nodecl_called,
                    nodecl_argument_list_output,
                    return_type,
                    filename, line);

            if (is_lvalue_reference_type(return_type))
            {
                nodecl_expr_set_is_lvalue(*nodecl_output, 1);
            }
            return;
        }
        else if (is_class_type(no_ref(called_type)))
        {
            type_t* class_type = no_ref(called_type);

            static AST operator = NULL;
            if (operator == NULL)
            {
                operator = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                        ASTLeaf(AST_FUNCTION_CALL_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
            }

            scope_entry_list_t* first_set_candidates = get_member_of_class_type(class_type, operator, decl_context);
            candidates = unfold_and_mix_candidate_functions(first_set_candidates,
                    /* builtins */ NULL, argument_types + 1, num_arguments - 1,
                    decl_context,
                    filename, line,
                    /* explicit_template_parameters */ NULL);
            entry_list_free(first_set_candidates);

            int num_surrogate_functions = 0;
            if (is_named_class_type(class_type)
                    && class_type_is_incomplete_independent(class_type))
            {
                scope_entry_t* symbol = named_type_get_symbol(class_type);
                instantiate_template_class(symbol, decl_context, filename, line);
            }

            scope_entry_list_t* conversion_list = class_type_get_all_conversions(class_type, decl_context);

            scope_entry_list_iterator_t *it = NULL;
            for (it = entry_list_iterator_begin(conversion_list);
                    !entry_list_iterator_end(it);
                    entry_list_iterator_next(it))
            {
                scope_entry_t* conversion = entry_advance_aliases(entry_list_iterator_current(it));

                type_t* destination_type = function_type_get_return_type(conversion->type_information);

                if (is_pointer_to_function_type(no_ref(destination_type)))
                {
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

                    surrogate_symbol->file = filename;
                    surrogate_symbol->line = line;

                    // This is a surrogate function created here
                    surrogate_symbol->entity_specs.is_surrogate_function = 1;
                    surrogate_symbol->entity_specs.is_builtin = 1;

                    surrogate_symbol->entity_specs.alias_to = conversion;

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

                    parameter_info_t parameter_info[MCXX_MAX_FUNCTION_PARAMETERS];
                    memset(parameter_info, 0, sizeof(parameter_info));

                    ERROR_CONDITION(MCXX_MAX_FUNCTION_PARAMETERS <= surrogate_num_parameters, 
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

            // This is the implicit argument
            nodecl_implicit_argument = nodecl_copy(nodecl_called);
            argument_types[0] = called_type;
        }
    }
    else
    {
        // Expand the candidate set
        scope_entry_list_t* first_set_candidates = candidates;
        candidates = unfold_and_mix_candidate_functions(first_set_candidates,
                /* builtins */ NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                filename, line,
                explicit_template_arguments);
        entry_list_free(first_set_candidates);
    }

    // Now fill the implicit argument type if not done yet
    if (nodecl_is_null(nodecl_implicit_argument))
    {
        if (nodecl_get_kind(nodecl_called) == NODECL_CLASS_MEMBER_ACCESS)
        {
            nodecl_implicit_argument = nodecl_get_child(nodecl_called, 0);
            argument_types[0] = nodecl_get_type(nodecl_implicit_argument);
        }
        else 
        {
            scope_entry_list_t* this_query = query_name_str(decl_context, "this");

            if (this_query != NULL)
            {
                scope_entry_t* this_ = entry_list_head(this_query);

                type_t* ptr_class_type = this_->type_information;
                type_t* class_type = pointer_type_get_pointee_type(ptr_class_type);
                argument_types[0] = class_type;

                entry_list_free(this_query);

                nodecl_t nodecl_sym = nodecl_make_symbol(this_, 
                        nodecl_get_filename(nodecl_called), 
                        nodecl_get_line(nodecl_called));
                nodecl_set_type(nodecl_sym, ptr_class_type);

                nodecl_implicit_argument = 
                    nodecl_make_derreference(
                            nodecl_sym,
                            class_type,
                            nodecl_get_filename(nodecl_called), 
                            nodecl_get_line(nodecl_called));
            }
        }
    }

    // Add the set of candidates and call the overload machinery
    scope_entry_t* conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
    memset(conversors, 0, sizeof(conversors));

    candidate_t* candidate_set = NULL;
    scope_entry_list_iterator_t *it = NULL;
    for (it = entry_list_iterator_begin(candidates);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* orig_entry = entry_list_iterator_current(it);
        scope_entry_t* entry = entry_advance_aliases(orig_entry);

        if (entry->entity_specs.is_member 
                || entry->entity_specs.is_surrogate_function)
        {
            candidate_set = add_to_candidate_set(candidate_set,
                    orig_entry,
                    num_arguments,
                    argument_types);
        }
        else
        {
            candidate_set = add_to_candidate_set(candidate_set,
                    orig_entry,
                    num_arguments - 1,
                    argument_types + 1);
        }
    }
    entry_list_iterator_free(it);
    entry_list_free(candidates);

    scope_entry_t* overloaded_call = solve_overload(candidate_set,
            decl_context,
            filename,
            line,
            conversors);

    if (overloaded_call == NULL)
    {
        // Overload failed
        if (!checking_ambiguity())
        {
            error_message_overload_failed(candidate_set, filename, line);
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* function_type_of_called = NULL;

    nodecl_t nodecl_argument_list_output = nodecl_null();

    // We are calling a surrogate, this implies calling first the conversion function
    if (overloaded_call->entity_specs.is_surrogate_function)
    {
        ERROR_CONDITION(nodecl_is_null(nodecl_implicit_argument), "There must be an implicit argument when calling a surrogate!", 0);

        nodecl_called = cxx_nodecl_make_function_call(
                nodecl_make_symbol(overloaded_call->entity_specs.alias_to, 
                    nodecl_get_filename(nodecl_implicit_argument), nodecl_get_line(nodecl_implicit_argument)),
                nodecl_make_list_1(nodecl_implicit_argument),
                function_type_get_return_type(overloaded_call->entity_specs.alias_to->type_information),
                nodecl_get_filename(nodecl_implicit_argument), nodecl_get_line(nodecl_implicit_argument)
                );


        overloaded_call = overloaded_call->entity_specs.alias_to;

        function_type_of_called = no_ref(function_type_get_return_type(overloaded_call->type_information));

        if (is_pointer_to_function_type(function_type_of_called))
        {
            function_type_of_called = pointer_type_get_pointee_type(function_type_of_called);
        }
        ERROR_CONDITION(!is_function_type(function_type_of_called), "Invalid function type!\n", 0);
    }
    else
    {
        nodecl_called = nodecl_make_symbol(overloaded_call, filename, line);

        function_type_of_called = overloaded_call->type_information;

        // Add this
        if (!nodecl_is_null(nodecl_implicit_argument)
                && overloaded_call->entity_specs.is_member 
                && !overloaded_call->entity_specs.is_static)
        {
            nodecl_argument_list_output = nodecl_append_to_list(nodecl_argument_list_output,
                    nodecl_implicit_argument);
        }
    }

    // Note that we check this here because of a surrogate being an alias
    if (function_has_been_deleted(decl_context, overloaded_call, filename, line))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Overload resolution succeeded\n");
    }

    // Update the unresolved call with all the conversions
    {
        // Starting from 0 would be a conversion of 'this' which does not apply here
        int arg_i = 1;
        // Set conversors of arguments if needed
        int i, num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);
        
        int num_parameters = function_type_get_num_parameters(function_type_of_called);
        if (function_type_get_has_ellipsis(function_type_of_called))
        {
            num_parameters--;
        }

        for (i = 0; i < num_items; i++, arg_i++)
        {
            nodecl_t nodecl_arg = list[i];

            if(i < num_parameters)
            {
                type_t* arg_type = nodecl_get_type(nodecl_arg);
                type_t* param_type = function_type_get_parameter_type_num(function_type_of_called, i);

                if (is_class_type(param_type))
                {
                    check_nodecl_expr_initializer(nodecl_arg, decl_context, param_type, &nodecl_arg);
                }
                else 
                {
                    if (is_unresolved_overloaded_type(arg_type))
                    {
                        scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(arg_type);
                        scope_entry_t* solved_function = address_of_overloaded_function(unresolved_set,
                                unresolved_overloaded_type_get_explicit_template_arguments(arg_type),
                                no_ref(param_type),
                                decl_context,
                                filename, line);

                        ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                        if (!solved_function->entity_specs.is_member
                                || solved_function->entity_specs.is_static)
                        {
                            nodecl_arg =
                                nodecl_make_symbol(solved_function, nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                            nodecl_set_type(nodecl_arg, lvalue_ref(solved_function->type_information));
                        }
                        else
                        {
                            nodecl_arg =
                                nodecl_make_pointer_to_member(solved_function, 
                                        get_lvalue_reference_type(
                                            get_pointer_to_member_type(solved_function->type_information,
                                                named_type_get_symbol(solved_function->entity_specs.class_type))),
                                        nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                        }
                    }

                    if (conversors[arg_i] != NULL)
                    {
                        if (function_has_been_deleted(decl_context, conversors[arg_i], filename, line))
                        {
                            *nodecl_output = nodecl_make_err_expr(filename, line);
                            return;
                        }

                        nodecl_arg = cxx_nodecl_make_function_call(
                                nodecl_make_symbol(conversors[arg_i], nodecl_get_filename(nodecl_arg),  nodecl_get_line(nodecl_arg)),
                                nodecl_make_list_1(nodecl_arg),
                                actual_type_of_conversor(conversors[arg_i]), nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                    }
                }
            }

            nodecl_argument_list_output = nodecl_append_to_list(nodecl_argument_list_output, nodecl_arg);
        }

        free(list);
    }

    type_t* return_type = function_type_get_return_type(function_type_of_called);

    // Everything seems fine here
    *nodecl_output = cxx_nodecl_make_function_call(nodecl_called, 
            nodecl_argument_list_output, 
            return_type,
            filename, line);

    nodecl_expr_set_is_lvalue(*nodecl_output, is_lvalue_reference_type(return_type));
}

// A function call is of the form
//   e1 ( e2 )
static void check_function_call(AST expr, decl_context_t decl_context, nodecl_t *nodecl_output)
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

    // This one will be filled later
    nodecl_t nodecl_called = nodecl_null();

    nodecl_t nodecl_argument_list = nodecl_null();
    check_function_arguments(arguments, decl_context, &nodecl_argument_list);

    if (!nodecl_is_null(nodecl_argument_list) 
            && nodecl_is_err_expr(nodecl_argument_list))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    CXX_LANGUAGE()
    {
        // If any in the expression list is type dependent this call is all dependent
        char any_arg_is_dependent = 0;
        int i, num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);
        for (i = 0; i < num_items && !any_arg_is_dependent; i++)
        {
            nodecl_t argument = list[i];
            if (nodecl_expr_is_type_dependent(argument))
            {
                any_arg_is_dependent = 1;
            }
        }
        free(list);
        
        // Note that koenig lookup is simply disabled by means of parentheses,
        // so the check has to be done here.
        if (ASTType(called_expression) == AST_SYMBOL
            || ASTType(called_expression) == AST_CONVERSION_FUNCTION_ID
            || ASTType(called_expression) == AST_OPERATOR_FUNCTION_ID)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: Call to '%s' will require argument dependent lookup\n",
                        prettyprint_in_buffer(called_expression));
            }
            compute_nodecl_name_from_id_expression(called_expression, decl_context, &nodecl_called);
        }
        else
        {
            check_expression_impl_(called_expression, decl_context, &nodecl_called);
        }

        if (any_arg_is_dependent 
                || nodecl_expr_is_type_dependent(nodecl_called))
        {
            *nodecl_output = nodecl_make_function_call(
                    nodecl_called,
                    nodecl_argument_list,
                    get_unknown_dependent_type(),
                    ASTFileName(expr), ASTLine(expr));
            nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
            return;
        }
    }

    C_LANGUAGE()
    {
        AST advanced_called_expression = advance_expression_nest(called_expression);

        if (ASTType(advanced_called_expression) == AST_SYMBOL)
        {
            scope_entry_list_t* result = query_nested_name(decl_context, NULL, NULL, advanced_called_expression); 

            scope_entry_t* entry = NULL;
            if (result == NULL)
            {
                // Create a symbol here
                entry = new_symbol(decl_context, decl_context.global_scope, ASTText(advanced_called_expression));
                entry->kind = SK_FUNCTION;
                entry->file = ASTFileName(advanced_called_expression);
                entry->line = ASTLine(advanced_called_expression);

                type_t* nonproto_type = get_nonproto_function_type(get_signed_int_type(),
                        nodecl_list_length(nodecl_argument_list));

                entry->type_information = nonproto_type;

                if (!checking_ambiguity())
                {
                    warn_printf("%s: warning: unknown function '%s' in call. Assuming '%s'\n",
                            ast_location(advanced_called_expression),
                            entry->symbol_name,
                            print_decl_type_str(entry->type_information, decl_context, entry->symbol_name));
                }
            }
            else
            {
                entry = entry_list_head(result);
            }

            nodecl_called = nodecl_make_symbol(entry, ASTFileName(called_expression), ASTLine(called_expression));
            nodecl_set_type(nodecl_called, entry->type_information);
            entry_list_free(result);
        }
        else
        {
            check_expression_impl_(called_expression, decl_context, &nodecl_called);
        }
    }

    if (nodecl_is_err_expr(nodecl_called))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    check_nodecl_function_call(nodecl_called, nodecl_argument_list, decl_context, nodecl_output);
}

static void check_nodecl_cast_expr(nodecl_t nodecl_casted_expr, 
        decl_context_t decl_context, 
        type_t* declarator_type,
        const char* cast_kind,
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    if (is_dependent_type(declarator_type))
    {
        *nodecl_output = nodecl_make_cast(
                nodecl_casted_expr,
                declarator_type,
                cast_kind,
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    if (IS_CXX_LANGUAGE
            && (strcmp(cast_kind, "C") == 0
                || strcmp(cast_kind, "static_cast") == 0))
    {
        // FIXME - For AST_CAST we need to check the following cases (in this order!)
        //
        //   const_cast
        //   static_cast
        //   static_cast + const_cast
        //   reinterpret_cast
        //   reinterpret_cast + const_cast
        // 
        // We may be missing conversions in a AST_CAST if a const_cast is required after a static_cast

        // Shut up the compiler if things go wrong
        enter_test_expression();
        // Check if an initialization is possible
        // If possible this will set a proper conversion call
        nodecl_t nodecl_cast_output = nodecl_null();
        nodecl_t nodecl_parenthesized_init = nodecl_make_cxx_parenthesized_initializer(
                nodecl_make_list_1(nodecl_copy(nodecl_casted_expr)),
                nodecl_get_filename(nodecl_casted_expr),
                nodecl_get_line(nodecl_casted_expr));
        // This actually checks T(e)
        check_nodecl_parenthesized_initializer(nodecl_parenthesized_init, 
                decl_context, 
                declarator_type, 
                &nodecl_cast_output);
        leave_test_expression();

        if (!nodecl_is_err_expr(nodecl_cast_output))
        {
            nodecl_casted_expr = nodecl_cast_output;

            // T(e) becomes (T){e}, so we get 'e' so the result is (T)e and not (T)(T){e}
            if (nodecl_get_kind(nodecl_casted_expr) == NODECL_STRUCTURED_VALUE)
            {
                nodecl_casted_expr = nodecl_list_head(nodecl_get_child(nodecl_casted_expr, 0));
            }
        }
    }

    char is_lvalue = 0;
    if (is_lvalue_reference_type(declarator_type))
    {
        is_lvalue = 1;
    }

    *nodecl_output = nodecl_make_cast(
            nodecl_casted_expr,
            declarator_type,
            cast_kind,
            filename, line);

    if (nodecl_is_constant(nodecl_casted_expr)
            && is_integral_type(declarator_type))
    {
        nodecl_set_constant(*nodecl_output,
                const_value_cast_to_bytes(
                    nodecl_get_constant(nodecl_casted_expr),
                    type_get_size(declarator_type), 
                    /* sign */ is_signed_integral_type(declarator_type)));
    }

    if (is_dependent_type(declarator_type))
    {
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
    }
    if (is_dependent_type(declarator_type)
            || nodecl_expr_is_value_dependent(nodecl_casted_expr))
    {
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
    }

    nodecl_expr_set_is_lvalue(*nodecl_output, is_lvalue);
}

static void check_cast_expr(AST expr, AST type_id, AST casted_expression_list, decl_context_t decl_context,
        const char* cast_kind,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_casted_expr = nodecl_null();
    AST casted_expression = ASTSon1(casted_expression_list);
    check_expression_impl_(casted_expression, decl_context, &nodecl_casted_expr);

    if (nodecl_is_err_expr(nodecl_casted_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    AST type_specifier = ASTSon0(type_id);
    AST abstract_declarator = ASTSon1(type_id);

    gather_decl_spec_t gather_info;
    memset(&gather_info, 0, sizeof(gather_info));

    nodecl_t dummy_nodecl_output = nodecl_null();

    type_t* simple_type_info = NULL;
    build_scope_decl_specifier_seq(type_specifier, &gather_info, &simple_type_info, 
            decl_context, &dummy_nodecl_output);

    if (is_error_type(simple_type_info))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    type_t* declarator_type = simple_type_info;
    compute_declarator_type(abstract_declarator, &gather_info, simple_type_info, 
            &declarator_type, decl_context, &dummy_nodecl_output);

    check_nodecl_cast_expr(nodecl_casted_expr, decl_context, declarator_type, cast_kind,
            ASTFileName(expr), ASTLine(expr),
            nodecl_output);
}

static void check_nodecl_comma_operand(nodecl_t nodecl_lhs, 
        nodecl_t nodecl_rhs, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output,
        const char* filename,
        int line)
{
    static AST operation_comma_tree = NULL;
    if (operation_comma_tree == NULL)
    {
        operation_comma_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                ASTLeaf(AST_COMMA_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
    }

    if (nodecl_is_err_expr(nodecl_lhs)
            || nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_lhs)
            || nodecl_expr_is_type_dependent(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_comma(nodecl_lhs, 
                nodecl_rhs, 
                get_unknown_dependent_type(),
                nodecl_get_filename(nodecl_lhs),
                nodecl_get_line(nodecl_lhs));
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* lhs_type = nodecl_get_type(nodecl_lhs);
    type_t* rhs_type = nodecl_get_type(nodecl_rhs);


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
                &nodecl_lhs,
                &nodecl_rhs,
                builtins,
                decl_context,
                filename, line,
                &selected_operator);
        leave_test_expression();

        if (!is_error_type(computed_type))
        {
            ERROR_CONDITION(selected_operator == NULL, "Invalid operator", 0);
            *nodecl_output = 
                cxx_nodecl_make_function_call(
                        nodecl_make_symbol(selected_operator, filename, line),
                        nodecl_make_list_2(nodecl_lhs, nodecl_rhs),
                        function_type_get_return_type(selected_operator->type_information),
                        filename, line);
            return;
        }
        // We will fall-through if no overload exists
    }

    *nodecl_output = nodecl_make_comma(
                nodecl_lhs,
                nodecl_rhs,
                nodecl_get_type(nodecl_rhs),
                filename, line);

    if (nodecl_is_constant(nodecl_rhs))
    {
        nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_rhs));
    }

    if (nodecl_expr_is_value_dependent(nodecl_rhs))
    {
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
    }
}

static void check_comma_operand(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    nodecl_t nodecl_lhs = nodecl_null();
    check_expression_impl_(lhs, decl_context, &nodecl_lhs);

    nodecl_t nodecl_rhs = nodecl_null();
    check_expression_impl_(rhs, decl_context, &nodecl_rhs);

    check_nodecl_comma_operand(nodecl_lhs, nodecl_rhs, decl_context,
            nodecl_output,
            ASTFileName(expression),
            ASTLine(expression));
}


static void check_templated_member_access(AST templated_member_access, decl_context_t decl_context, 
        char is_arrow, nodecl_t* nodecl_output)
{
    check_member_access(templated_member_access, decl_context, is_arrow, nodecl_output);
}

static nodecl_t integrate_field_accesses(nodecl_t base, nodecl_t accessor)
{
    if (nodecl_get_kind(accessor) == NODECL_CLASS_MEMBER_ACCESS)
    {
        nodecl_t accessor_base = nodecl_get_child(accessor, 0);
        nodecl_t accessor_symbol = nodecl_get_child(accessor, 1);

        nodecl_t integrated_nodecl = integrate_field_accesses(base, accessor_base);

        return nodecl_make_class_member_access(integrated_nodecl,
                accessor_symbol,
                lvalue_ref(nodecl_get_symbol(accessor_symbol)->type_information),
                nodecl_get_filename(integrated_nodecl),
                nodecl_get_line(integrated_nodecl));
    }
    else if (nodecl_get_kind(accessor) == NODECL_SYMBOL)
    {
        return nodecl_make_class_member_access(base, accessor, 
                lvalue_ref(nodecl_get_symbol(accessor)->type_information),
                nodecl_get_filename(base),
                nodecl_get_line(base));
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static char is_pseudo_destructor_id(decl_context_t decl_context,
        type_t* accessed_type,
        nodecl_t nodecl_member)
{
    // A pseudo destructor id has the following structure
    //
    // ::[opt] nested-name-specifier-seq[opt] type-name1 :: ~ type-name2
    //
    // But we have created a nodecl_name which looks like as a qualified name
    //
    // We first lookup this part
    //
    // ::[opt] nested-name-specifier-seq[opt] type-name1
    //
    // it should give a type equivalent to accessed_type
    //
    // then we have to check that type-name1 and type-name2 mean the same name. Note that
    // both can be typedefs and such, but they must mean the same. type-name2 is looked
    // up in the context of type-name1, lest type-name1 was a qualified name

    if (nodecl_get_kind(nodecl_member) != NODECL_CXX_DEP_GLOBAL_NAME_NESTED
            && nodecl_get_kind(nodecl_member) != NODECL_CXX_DEP_NAME_NESTED)
    {
        return 0;
    }

    nodecl_t nodecl_last_part = nodecl_name_get_last_part(nodecl_member);
    if (nodecl_get_kind(nodecl_last_part) != NODECL_CXX_DEP_NAME_SIMPLE)
    {
        return 0;
    }

    const char* last_name = nodecl_get_text(nodecl_last_part);
    // This is not a destructor-id
    if (last_name[0] != '~')
        return 0;

    // Ignore '~'
    last_name++;

    // Now build ::[opt] nested-name-specifier-seq[opt] type-name1
    nodecl_t new_list = nodecl_null();
    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(nodecl_member, 0), &num_items);
    if (num_items < 2)
    {
        free(list);
        return 0;
    }

    // Build the same list without the last name
    nodecl_t nodecl_new_nested_name = nodecl_null();
    if ((num_items - 1) > 1)
    {
        int i;
        for (i = 0; i < num_items - 1; i++)
        {
            new_list = nodecl_append_to_list(new_list, nodecl_copy(list[i]));
        }

        if (nodecl_get_kind(nodecl_member) == NODECL_CXX_DEP_GLOBAL_NAME_NESTED)
        {
            nodecl_new_nested_name = nodecl_make_cxx_dep_global_name_nested(new_list, 
                    nodecl_get_filename(nodecl_member),
                    nodecl_get_line(nodecl_member));
        }
        else
        {
            nodecl_new_nested_name = nodecl_make_cxx_dep_name_nested(new_list, 
                    nodecl_get_filename(nodecl_member),
                    nodecl_get_line(nodecl_member));
        }
    }
    else
    {
        // For the case T::~T, we cannot build a nested name with a single
        // element, so use the element itself
        nodecl_new_nested_name = nodecl_copy(list[0]);
    }

    scope_entry_list_t* entry_list = query_nodecl_name_flags(decl_context, 
            nodecl_new_nested_name, DF_DEPENDENT_TYPENAME);

    if (entry_list == NULL)
    {
        return 0;
    }

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_advance_aliases(entry_list_iterator_current(it));

        if (entry->kind == SK_TYPEDEF)
        {
            // Advance if typedef to the ultimate type
            if (is_named_class_type(advance_over_typedefs(entry->type_information)))
            {
                entry = named_type_get_symbol(advance_over_typedefs(entry->type_information));
            }
        }
        // Note that dependent stuff is ignored here as we want a generic node
        // for member access not a special one for pseudo destructors
        if (entry->kind != SK_ENUM 
                && entry->kind != SK_TYPEDEF)
        {
            entry_list_free(entry_list);
            return 0;
        }
    }

    scope_entry_t* entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (!is_scalar_type(entry->type_information))
    {
        return 0;
    }

    // Now check that type-name2 names the same type we have found so far

    entry_list = query_name_str(entry->decl_context, last_name);

    if (entry_list == NULL)
    {
        return 0;
    }

    for (it = entry_list_iterator_begin(entry_list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current_entry = entry_advance_aliases(entry_list_iterator_current(it));
        // Note that dependent stuff is ignored here as we want a generic node
        // for member access not a special one for pseudo destructors
        if (current_entry->kind != SK_ENUM 
                && current_entry->kind != SK_TYPEDEF)
        {
            entry_list_free(entry_list);
            return 0;
        }
    }

    scope_entry_t* second_entry = entry_list_head(entry_list);
    entry_list_free(entry_list);

    if (!equivalent_types(
                get_unqualified_type(no_ref(entry->type_information)),
                get_unqualified_type(no_ref(second_entry->type_information))))
    {
        return 0;
    }

    if (!equivalent_types(get_unqualified_type(no_ref(entry->type_information)),
                get_unqualified_type(accessed_type)))
    {
        return 0;
    }

    return 1;
}

static void check_nodecl_member_access(
        nodecl_t nodecl_accessed, 
        nodecl_t nodecl_member,
        decl_context_t decl_context, 
        char is_arrow,
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_accessed))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* conversion_type = NULL;
    if (nodecl_get_kind(nodecl_member) == NODECL_CXX_DEP_NAME_CONVERSION)
    {
        conversion_type = nodecl_get_type(nodecl_member);
    }

    if (nodecl_expr_is_type_dependent(nodecl_accessed)
            // If syntax is 'a.operator T' or 'a->operator T' check if it is a
            // dependent type
            || (conversion_type != NULL
                && is_dependent_type(conversion_type)))
    {
        if (!is_arrow)
        {
            *nodecl_output = nodecl_make_class_member_access(
                    nodecl_accessed,
                    nodecl_member,
                    get_unknown_dependent_type(),
                    filename, line);
        }
        else
        {
            *nodecl_output = nodecl_make_cxx_arrow(
                    nodecl_accessed,
                    nodecl_member,
                    get_unknown_dependent_type(),
                    filename, line);
        }
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }


    type_t* accessed_type = nodecl_get_type(nodecl_accessed);
    nodecl_t nodecl_accessed_out = nodecl_accessed;
    char operator_arrow = 0;
    
    // First we adjust the actually accessed type
    // if we are in '->' syntax
    if (is_arrow)
    {
        if (is_pointer_type(no_ref(accessed_type)))
        {
            accessed_type = pointer_type_get_pointee_type(no_ref(accessed_type));

            nodecl_accessed_out = 
                nodecl_make_derreference(
                        nodecl_accessed,
                        accessed_type,
                        nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
        }
        else if (is_array_type(no_ref(accessed_type)))
        {
            accessed_type = array_type_get_element_type(no_ref(accessed_type));

            nodecl_accessed_out = 
                nodecl_make_derreference(
                        nodecl_accessed,
                        accessed_type,
                        nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
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
                error_printf("%s: error: '->%s' cannot be applied to '%s' (of type '%s')\n",
                        nodecl_get_locus(nodecl_accessed),
                        c_cxx_codegen_to_str(nodecl_member),
                        c_cxx_codegen_to_str(nodecl_accessed),
                        print_type_str(no_ref(accessed_type), decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
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

        scope_entry_list_t* operator_arrow_list = get_member_of_class_type(accessed_type,
                arrow_operator_tree, decl_context);

        if (operator_arrow_list == NULL)
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* argument_types[1] = { 
            /* Note that we want the real original type since it might be a referenced type */
            nodecl_get_type(nodecl_accessed) 
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
                decl_context, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed),
                conversors);

        if (selected_operator_arrow == NULL)
        {
            if (!checking_ambiguity())
            {
                error_message_overload_failed(candidate_set, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (function_has_been_deleted(decl_context, selected_operator_arrow, 
                    nodecl_get_filename(nodecl_accessed), 
                    nodecl_get_line(nodecl_accessed)))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (!is_pointer_to_class_type(function_type_get_return_type(selected_operator_arrow->type_information)))
        {
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        // Now we update the nodecl_accessed with the resulting type, this is used later when solving
        // overload in calls made using this syntax.
        type_t* t = function_type_get_return_type(selected_operator_arrow->type_information);

        // The accessed type is the pointed type
        accessed_type = pointer_type_get_pointee_type(no_ref(t));

        nodecl_accessed_out = 
            nodecl_make_derreference(
                    cxx_nodecl_make_function_call(
                        nodecl_make_symbol(selected_operator_arrow, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed)),
                        nodecl_make_list_1(nodecl_accessed),
                        t, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed)),
                    pointer_type_get_pointee_type(t), nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
    }

    if (IS_CXX_LANGUAGE
            && is_scalar_type(no_ref(accessed_type))
            && is_pseudo_destructor_id(decl_context, no_ref(accessed_type), nodecl_member))
    {
        *nodecl_output = nodecl_make_pseudo_destructor_name(nodecl_accessed_out, 
                nodecl_member,
                get_pseudo_destructor_call_type(),
                filename, line);
        return;
    }
    else if (!is_class_type(no_ref(accessed_type)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s%s' cannot be applied to '%s' (of type '%s')\n",
                    nodecl_get_locus(nodecl_accessed),
                    operator_arrow ? "->" : ".",
                    c_cxx_codegen_to_str(nodecl_member),
                    c_cxx_codegen_to_str(nodecl_accessed),
                    print_type_str(no_ref(accessed_type), decl_context));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    // Advance over all typedefs keeping the underlying cv-qualification
    cv_qualifier_t cv_qualif = CV_NONE;
    accessed_type = advance_over_typedefs_with_cv_qualif(no_ref(accessed_type), &cv_qualif);
    accessed_type = get_cv_qualified_type(accessed_type, cv_qualif);

    // This need not to be a member function but 'get_member_of_class_type' works
    // also for data members
    scope_entry_list_t* entry_list = get_member_of_class_type_nodecl(
            decl_context,
            accessed_type,
            nodecl_member);

    if (entry_list == NULL)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' is not a member/field of type '%s'\n",
                    nodecl_get_locus(nodecl_member),
                    c_cxx_codegen_to_str(nodecl_member),
                    print_type_str(no_ref(accessed_type), decl_context));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    char ok = 0;
    scope_entry_t* entry = entry_advance_aliases(entry_list_head(entry_list));
    C_LANGUAGE()
    {
        nodecl_t nodecl_field = nodecl_accessed_out;
        if (entry->entity_specs.is_member_of_anonymous)
        {
            nodecl_t accessor = entry->entity_specs.anonymous_accessor;
            nodecl_field = integrate_field_accesses(nodecl_field, 
                    accessor);
        }

        ok = 1;

        *nodecl_output = nodecl_make_class_member_access(
                nodecl_field,
                nodecl_make_symbol(entry, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed)),
                entry->type_information,
                nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
    }

    CXX_LANGUAGE()
    {
        if (entry->kind == SK_VARIABLE)
        {
            type_t* t = lvalue_ref(entry->type_information);
            ok = 1;

            nodecl_t nodecl_field = nodecl_accessed_out;
            if (entry->entity_specs.is_member_of_anonymous)
            {
                nodecl_t accessor = entry->entity_specs.anonymous_accessor;
                nodecl_field = integrate_field_accesses(nodecl_field, 
                        accessor);
            }

            *nodecl_output = nodecl_make_class_member_access(
                    nodecl_field,
                    nodecl_make_symbol(entry, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed)),
                    t, 
                    nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
            nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        }
        // In C++ if we have overload remember it
        else if (entry->kind == SK_FUNCTION
                || entry->kind == SK_TEMPLATE)
        {
            template_parameter_list_t* last_template_args = NULL;
            if (nodecl_name_ends_in_template_id(nodecl_member))
            {
                last_template_args = nodecl_name_name_last_template_arguments(nodecl_member);
            }

            type_t* t = get_unresolved_overloaded_type(entry_list, last_template_args);

            ok = 1;

            *nodecl_output = nodecl_make_class_member_access(
                    nodecl_accessed_out,
                    nodecl_make_symbol(entry, nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed)),
                    t, 
                    nodecl_get_filename(nodecl_accessed), nodecl_get_line(nodecl_accessed));
        }
    }

    entry_list_free(entry_list);

    if (!ok)
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
    }
}

static void check_member_access(AST member_access, decl_context_t decl_context, char is_arrow, nodecl_t* nodecl_output)
{
    AST class_expr = ASTSon0(member_access);
    AST id_expression = ASTSon1(member_access);

    nodecl_t nodecl_accessed = nodecl_null();
    check_expression_impl_(class_expr, decl_context, &nodecl_accessed);

    if (nodecl_is_err_expr(nodecl_accessed))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(member_access), ASTLine(member_access));
        return;
    }

    nodecl_t nodecl_name = nodecl_null();
    compute_nodecl_name_from_id_expression(id_expression, decl_context, &nodecl_name);

    check_nodecl_member_access(nodecl_accessed, nodecl_name, decl_context, is_arrow, 
            ASTFileName(member_access), ASTLine(member_access),
            nodecl_output);
}

static void check_qualified_id(AST expr, decl_context_t decl_context, nodecl_t *nodecl_output)
{
    cxx_common_name_check(expr, decl_context, nodecl_output);
}


// This checks that a template-id-expr is feasible in an expression
static void check_template_id_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    cxx_common_name_check(expr, decl_context, nodecl_output);
}

static void check_postoperator_user_defined(
        AST operator, 
        nodecl_t postoperated_expr, 
        decl_context_t decl_context,
        scope_entry_list_t* builtins,
        nodecl_t (*nodecl_fun)(nodecl_t, type_t*, const char*, int),
        nodecl_t* nodecl_output)
{
    type_t* incremented_type = nodecl_get_type(postoperated_expr);

    type_t* argument_types[2] = {
        incremented_type, // Member argument
        get_zero_type() // Postoperation
    };
    int num_arguments = 2;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(incremented_type)))
    {
        scope_entry_list_t *operator_entry_list = get_member_of_class_type(no_ref(incremented_type),
                operator, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr),
                /* explicit_template_parameters */ NULL);
        entry_list_free(operator_entry_list);
    }

    // We need to do koenig lookup for non-members
    // otherwise some overloads might not be found
    nodecl_t nodecl_op_name = 
        nodecl_make_cxx_dep_name_simple(
                get_operator_function_name(operator),
                nodecl_get_filename(postoperated_expr), 
                nodecl_get_line(postoperated_expr));
    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, nodecl_op_name);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(entry_list,
            builtins, argument_types, num_arguments,
            decl_context,
            nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr), /* explicit_template_parameters */ NULL);
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
            decl_context, nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr), 
            conversors);

    if (overloaded_call == NULL)
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(candidate_set, nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
        }
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
        return;
    }

    if (function_has_been_deleted(decl_context, overloaded_call, nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr)))
    {
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
        return;
    }

    if (!overloaded_call->entity_specs.is_member)
    {
        type_t* param_type = function_type_get_parameter_type_num(overloaded_call->type_information, 0);

        if (is_class_type(param_type))
        {
            check_nodecl_expr_initializer(postoperated_expr, decl_context, param_type, &postoperated_expr);
        }
        else
        {
            if (conversors[0] != NULL)
            {
                if (function_has_been_deleted(decl_context, conversors[0], nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr)))
                {
                    *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
                    return;
                }

                postoperated_expr =
                    cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversors[0], nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr)),
                            nodecl_make_list_1(postoperated_expr),
                            actual_type_of_conversor(conversors[0]), nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
            }
        }
    }

    if (overloaded_call->entity_specs.is_builtin)
    {
        *nodecl_output = nodecl_fun(
                postoperated_expr,
                function_type_get_return_type(overloaded_call->type_information), 
                nodecl_get_filename(postoperated_expr), 
                nodecl_get_line(postoperated_expr));
    }
    else
    {
        *nodecl_output = cxx_nodecl_make_function_call(
                nodecl_make_symbol(overloaded_call, nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr)),
                nodecl_make_list_2(/* 0 */ 
                    postoperated_expr,
                    const_value_to_nodecl(const_value_get_zero(type_get_size(get_signed_int_type()), /* signed */ 1))),
                function_type_get_return_type(overloaded_call->type_information), 
                nodecl_get_filename(postoperated_expr), nodecl_get_line(postoperated_expr));
    }
}

static void check_preoperator_user_defined(AST operator, 
        nodecl_t preoperated_expr,
        decl_context_t decl_context,
        scope_entry_list_t* builtins,
        nodecl_t (*nodecl_fun)(nodecl_t, type_t*, const char*, int),
        nodecl_t* nodecl_output)
{
    type_t* incremented_type = nodecl_get_type(preoperated_expr);

    type_t* argument_types[1] = {
        incremented_type, 
    };
    int num_arguments = 1;

    candidate_t* candidate_set = NULL;

    scope_entry_list_t* operator_overload_set = NULL;
    if (is_class_type(no_ref(incremented_type)))
    {
        scope_entry_list_t *operator_entry_list = get_member_of_class_type(no_ref(incremented_type),
                operator, decl_context);

        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, argument_types + 1, num_arguments - 1,
                decl_context,
                nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr),
                /* explicit_template_parameters */ NULL);
        entry_list_free(operator_entry_list);
    }

    nodecl_t nodecl_op_name = 
        nodecl_make_cxx_dep_name_simple(
                get_operator_function_name(operator),
                nodecl_get_filename(preoperated_expr), 
                nodecl_get_line(preoperated_expr));
    scope_entry_list_t *entry_list = koenig_lookup(num_arguments,
            argument_types, decl_context, nodecl_op_name);

    scope_entry_list_t* overload_set = unfold_and_mix_candidate_functions(
            entry_list, builtins, argument_types, num_arguments,
            decl_context,
            nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr), /* explicit_template_parameters */ NULL);
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
            decl_context, nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr), conversors);

    if (overloaded_call == NULL)
    {
        if (!checking_ambiguity())
        {
            error_message_overload_failed(candidate_set, nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
        }
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
        return;
    }

    if (function_has_been_deleted(decl_context, overloaded_call, nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr)))
    {
        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
        return;
    
    }

    if (!overloaded_call->entity_specs.is_member)
    {
        type_t* param_type = function_type_get_parameter_type_num(overloaded_call->type_information, 0);

        if (is_class_type(param_type))
        {
            check_nodecl_expr_initializer(preoperated_expr, decl_context, param_type, &preoperated_expr);
        }
        else
        {
            if (conversors[0] != NULL)
            {
                if (function_has_been_deleted(decl_context, conversors[0], nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr)))
                {
                    *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
                    return;
                }

                preoperated_expr = 
                    cxx_nodecl_make_function_call(
                            nodecl_make_symbol(conversors[0], nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr)),
                            nodecl_make_list_1(preoperated_expr),
                            actual_type_of_conversor(conversors[0]), nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
            }
        }
    }

    if (overloaded_call->entity_specs.is_builtin)
    {
        *nodecl_output = 
                nodecl_fun(
                    preoperated_expr,
                    function_type_get_return_type(overloaded_call->type_information), 
                    nodecl_get_filename(preoperated_expr), 
                    nodecl_get_line(preoperated_expr));
    }
    else
    {
        *nodecl_output = 
                cxx_nodecl_make_function_call(
                    nodecl_make_symbol(overloaded_call, nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr)),
                    nodecl_make_list_1(preoperated_expr),
                    function_type_get_return_type(overloaded_call->type_information), 
                    nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
    }
    return;
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

static void check_postoperator(AST operator, 
        nodecl_t postoperated_expr, 
        decl_context_t decl_context, char is_decrement,
        nodecl_t (*nodecl_fun)(nodecl_t, type_t*, const char*, int),
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(postoperated_expr))
    {
        *nodecl_output = nodecl_make_err_expr(
                nodecl_get_filename(postoperated_expr), 
                nodecl_get_line(postoperated_expr));
        return;
    }

    if (nodecl_expr_is_type_dependent(postoperated_expr))
    {
        *nodecl_output = nodecl_fun(postoperated_expr,
                get_unknown_dependent_type(),
                nodecl_get_filename(postoperated_expr), 
                nodecl_get_line(postoperated_expr));
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1); 
        return;
    }

    type_t* operated_type = nodecl_get_type(postoperated_expr);
    char is_lvalue = nodecl_expr_is_lvalue(postoperated_expr);

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
                {
                    *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), 
                            nodecl_get_line(postoperated_expr));
                    return;
                }
            }
            CXX_LANGUAGE()
            {
                if (!is_lvalue_reference_type(operated_type))
                {
                    *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), 
                            nodecl_get_line(postoperated_expr));
                    return;
                }

                operated_type = reference_type_get_referenced_type(operated_type);
            }

            *nodecl_output = nodecl_fun(postoperated_expr,
                        operated_type,
                        nodecl_get_filename(postoperated_expr),
                        nodecl_get_line(postoperated_expr));
            return;
        }
        else
        {
            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(postoperated_expr), 
                    nodecl_get_line(postoperated_expr));
            return;
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
    check_postoperator_user_defined(operator, 
            postoperated_expr, decl_context, builtins, nodecl_fun, nodecl_output);

    entry_list_free(builtins);
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

static void check_preoperator(AST operator, 
        nodecl_t preoperated_expr, decl_context_t decl_context,
        char is_decrement, nodecl_t (*nodecl_fun)(nodecl_t, type_t*, const char*, int),
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(preoperated_expr))
    {
        *nodecl_output = nodecl_make_err_expr(
                nodecl_get_filename(preoperated_expr), 
                nodecl_get_line(preoperated_expr));
        return;
    }

    if (nodecl_expr_is_type_dependent(preoperated_expr))
    {
        *nodecl_output = nodecl_fun(preoperated_expr, 
                get_unknown_dependent_type(), 
                nodecl_get_filename(preoperated_expr), 
                nodecl_get_line(preoperated_expr));
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* operated_type = nodecl_get_type(preoperated_expr);
    char is_lvalue = nodecl_expr_is_lvalue(preoperated_expr);


    if (is_pointer_type(no_ref(operated_type))
            || is_arithmetic_type(no_ref(operated_type)))
    {
        C_LANGUAGE()
        {
            // Must be an lvalue
            if (!is_lvalue)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: %s operand is not a lvalue\n",
                            nodecl_get_locus(preoperated_expr),
                            is_decrement ? "predecrement" : "preincrement");
                }
                *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
                return;
            }
        }

        CXX_LANGUAGE()
        {
            if (!is_lvalue_reference_type(operated_type))
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: %s operand is not a lvalue\n",
                            nodecl_get_locus(preoperated_expr),
                            is_decrement ? "predecrement" : "preincrement");
                }
                *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
                return;
            }
        }

        *nodecl_output = nodecl_fun(preoperated_expr,
                    operated_type, 
                    nodecl_get_filename(preoperated_expr),
                    nodecl_get_line(preoperated_expr));
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        return;
    }
    else
    {
        C_LANGUAGE()
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: type %s is not valid for %s operator\n",
                        nodecl_get_locus(preoperated_expr),
                        print_type_str(operated_type, decl_context),
                        is_decrement ? "predecrement" : "preincrement");
            }
            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(preoperated_expr), nodecl_get_line(preoperated_expr));
            return;
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
    check_preoperator_user_defined(operator, 
            preoperated_expr, 
            decl_context, 
            builtins, 
            nodecl_fun,
            nodecl_output);

    entry_list_free(builtins);

    return;
}

static void check_postincrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    nodecl_t nodecl_postincremented = nodecl_null();
    check_expression_impl_(postincremented_expr, decl_context, &nodecl_postincremented);

    check_postoperator(operation_tree, 
            nodecl_postincremented, 
            decl_context, /* is_decr */ 0, 
            nodecl_make_postincrement, nodecl_output);
}

static void check_postdecrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    nodecl_t nodecl_postdecremented = nodecl_null();
    check_expression_impl_(postdecremented_expr, decl_context, &nodecl_postdecremented);

    check_postoperator(operation_tree, 
            nodecl_postdecremented, 
            decl_context, /* is_decr */ 1, 
            nodecl_make_postdecrement, nodecl_output);
}

static void check_preincrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    nodecl_t nodecl_preincremented = nodecl_null();
    check_expression_impl_(preincremented_expr, decl_context, &nodecl_preincremented);

    check_preoperator(operation_tree, nodecl_preincremented, 
            decl_context, /* is_decr */ 0, nodecl_make_preincrement, nodecl_output);
}

static void check_predecrement(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
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

    nodecl_t nodecl_predecremented = nodecl_null();
    check_expression_impl_(predecremented_expr, decl_context, &nodecl_predecremented);

    check_preoperator(operation_tree, nodecl_predecremented, 
            decl_context, /* is_decr */ 1, nodecl_make_predecrement, nodecl_output);
}

static scope_entry_t* get_typeid_symbol(decl_context_t decl_context, const char* filename, int line)
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

            error_printf("%s:%d: error: namespace 'std' not found when looking up 'std::type_info'. \n"
                    "Maybe you need '#include <typeinfo>'",
                    filename, line);
            return NULL;
        }

        decl_context_t std_context = entry_list_head(entry_list)->related_decl_context;
        entry_list_free(entry_list);
        entry_list = query_in_scope_str(std_context, "type_info");

        if (entry_list == NULL
                || (entry_list_head(entry_list)->kind != SK_CLASS
                    && entry_list_head(entry_list)->kind != SK_TYPEDEF))
        {
            if (entry_list != NULL)
                entry_list_free(entry_list);

            error_printf("%s:%d: error: namespace 'std' not found when looking up 'std::type_info'. \n"
                    "Maybe you need '#include <typeinfo>'",
                    filename, line);
            return NULL;
        }

        typeid_sym = entry_list_head(entry_list);
        entry_list_free(entry_list);
    }

    return typeid_sym;
}

scope_entry_t* get_std_initializer_list_template(decl_context_t decl_context, 
        const char* filename,
        int line, 
        char mandatory)
{
    // Lookup for 'std::initializer_list'
    scope_entry_t* result = NULL;

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

        if (!checking_ambiguity())
        {
            error_printf("%s:%d: error: namespace 'std' not found when looking up 'std::initializer_list'\n"
                    "Maybe you need '#include <initializer_list>'",
                    filename, line);
        }
        return NULL;
    }

    decl_context_t std_context = entry_list_head(entry_list)->related_decl_context;
    entry_list_free(entry_list);

    entry_list = query_in_scope_str(std_context, "initializer_list");

    if (entry_list == NULL
            || entry_list_head(entry_list)->kind != SK_TEMPLATE)
    {
        if (entry_list != NULL)
            entry_list_free(entry_list);
        if (!mandatory)
            return NULL;

        if (!checking_ambiguity())
        {
            error_printf("%s:%d: error: template-name 'initializer_list' not found when looking up 'std::initializer_list'\n"
                    "Maybe you need '#include <initializer_list>'",
                    filename, line);
        }
        return NULL;
    }

    result = entry_list_head(entry_list);
    entry_list_free(entry_list);

    return result;
}

static void check_nodecl_typeid_type(type_t* t, 
        decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    scope_entry_t* typeid_type_class = get_typeid_symbol(decl_context, filename, line);
    if (typeid_type_class == NULL)
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* typeid_type = lvalue_ref(get_const_qualified_type(get_user_defined_type(typeid_type_class)));

    nodecl_t nodecl_type = nodecl_make_type(t, filename, line);
    if (is_dependent_type(t))
    {
        nodecl_expr_set_is_type_dependent(nodecl_type, 1);
    }

    *nodecl_output = nodecl_make_typeid(
            nodecl_type,
            typeid_type, filename, line);

    nodecl_expr_set_is_lvalue(*nodecl_output, 1);
}

static void check_typeid_type(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    type_t* type = compute_type_for_type_id_tree(ASTSon0(expr), decl_context);

    if (is_error_type(type))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expr), ASTLine(expr));
        return;
    }

    check_nodecl_typeid_type(type, decl_context, ASTFileName(expr), ASTLine(expr), nodecl_output);
}

static void check_nodecl_typeid_expr(nodecl_t nodecl_typeid_expr, 
        decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_typeid_expr))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    scope_entry_t* typeid_type_class = get_typeid_symbol(decl_context, filename, line);
    if (typeid_type_class == NULL)
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* typeid_type = lvalue_ref(get_const_qualified_type(get_user_defined_type(typeid_type_class)));

    *nodecl_output = nodecl_make_typeid(
            nodecl_typeid_expr,
            typeid_type, filename, line);


    nodecl_expr_set_is_lvalue(*nodecl_output, 1);
}

static void check_typeid_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_typeid_expr = nodecl_null();
    check_expression_impl_(ASTSon0(expr), decl_context, &nodecl_typeid_expr);

    check_nodecl_typeid_expr(nodecl_typeid_expr, decl_context, 
            ASTFileName(expr), 
            ASTLine(expr), 
            nodecl_output);
}

static void check_nodecl_braced_initializer(nodecl_t braced_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(nodecl_get_kind(braced_initializer) != NODECL_CXX_BRACED_INITIALIZER, "Invalid nodecl", 0);

    if (is_dependent_type(declared_type))
    {
        *nodecl_output = braced_initializer;
        nodecl_set_type(*nodecl_output, declared_type);
        return;
    }

    nodecl_t initializer_clause_list = nodecl_get_child(braced_initializer, 0);

    if ((is_class_type(declared_type)
                || is_array_type(declared_type)
                || is_vector_type(declared_type))
            && is_aggregate_type(declared_type))
    {
        nodecl_t nodecl_initializer_list_output = nodecl_null();

        type_t* initializer_type = declared_type;
        if (!nodecl_is_null(initializer_clause_list))
        {
            if (!is_array_type(declared_type)
                    && !is_class_type(declared_type)
                    && !is_vector_type(declared_type)
                    && !is_dependent_type(declared_type))
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: brace initialization can only be used with\n",
                            nodecl_get_locus(braced_initializer));
                    error_printf("%s: error: array types, struct, class, union or vector types\n",
                            nodecl_get_locus(braced_initializer));
                }
                *nodecl_output = nodecl_make_err_expr(
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }

            // Special case for this sort of initializations
            //   char a[] = { "hello" };
            
            // The expression list has only one element of kind expression and this element is an array of chars o wchars
            type_t * type_element = nodecl_get_type(nodecl_list_head(initializer_clause_list));
            if (nodecl_list_length(initializer_clause_list) == 1
                    && nodecl_get_kind(nodecl_list_head(initializer_clause_list)) != NODECL_CXX_BRACED_INITIALIZER
                    && nodecl_get_kind(nodecl_list_head(initializer_clause_list)) != NODECL_C99_DESIGNATED_INITIALIZER
                    && (is_array_type(no_ref(type_element))
                        && (is_char_type(array_type_get_element_type(no_ref(type_element)))
                            || is_wchar_t_type(array_type_get_element_type(no_ref(type_element)))
                           )
                       )
               )
            {
                // Attempt an interpretation like char a[] = "hello";
                check_nodecl_expr_initializer(nodecl_list_head(initializer_clause_list), 
                        decl_context,
                        declared_type,
                        nodecl_output);
                return;
            }

            int i, num_items = 0;
            nodecl_t* list = nodecl_unpack_list(initializer_clause_list, &num_items);

            for (i = 0; i < num_items; i++)
            {
                nodecl_t nodecl_initializer_clause = list[i];

                type_t* type_in_context = declared_type;

                if (nodecl_get_kind(nodecl_initializer_clause) != NODECL_C99_DESIGNATED_INITIALIZER)
                {
                    if (is_class_type(declared_type))
                    {
                        type_t* actual_class_type = get_actual_class_type(declared_type);

                        scope_entry_list_t* nonstatic_data_members = 
                            class_type_get_nonstatic_data_members(actual_class_type);

                        if (i >= entry_list_size(nonstatic_data_members))
                        {
                            if (!checking_ambiguity())
                            {
                                error_printf("%s: error: too many initializers for aggregated data type\n",
                                        nodecl_get_locus(braced_initializer));
                            }
                            *nodecl_output = nodecl_make_err_expr(
                                    nodecl_get_filename(braced_initializer), 
                                    nodecl_get_line(braced_initializer));
                            return;
                        }
                        else
                        {
                            scope_entry_list_iterator_t* it = entry_list_iterator_begin(nonstatic_data_members);
                            int j;
                            for (j = 0; j < i; j++)
                            {
                                entry_list_iterator_next(it);
                            }

                            scope_entry_t* data_member = entry_list_iterator_current(it);
                            type_in_context = data_member->type_information;

                            entry_list_iterator_free(it);
                        }

                        entry_list_free(nonstatic_data_members);
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

                nodecl_t nodecl_initializer_clause_output = nodecl_null();
                check_nodecl_initializer_clause(nodecl_initializer_clause, decl_context, type_in_context,
                        &nodecl_initializer_clause_output);

                if (nodecl_is_err_expr(nodecl_initializer_clause_output))
                {
                    *nodecl_output = nodecl_make_err_expr(
                            nodecl_get_filename(braced_initializer), 
                            nodecl_get_line(braced_initializer));
                    return;
                }

                nodecl_initializer_list_output = nodecl_append_to_list(nodecl_initializer_list_output, nodecl_initializer_clause_output);
            }

            free(list);

            if (is_class_type(declared_type))
            {
                initializer_type = declared_type;
            }
            else if (is_array_type(declared_type))
            {
                char c[64];
                snprintf(c, 63, "%d", num_items);
                c[63] = '\0';

                nodecl_t length = nodecl_make_integer_literal(get_signed_int_type(),
                        const_value_get_unsigned_int(num_items), 
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));

                initializer_type = get_array_type(
                        array_type_get_element_type(declared_type),
                        length, decl_context);
            }
            else if (is_vector_type(declared_type))
            {
                initializer_type = declared_type;
            }
            else
            {
                internal_error("Code unreachable", 0);
            }
        }

        *nodecl_output = nodecl_make_structured_value(nodecl_initializer_list_output,
                initializer_type,
                nodecl_get_filename(braced_initializer), 
                nodecl_get_line(braced_initializer));
        return;
    }
    // Not an aggregate class
    else if (is_class_type(declared_type)
            && !is_aggregate_type(declared_type))
    {
        // This one is the toughest
        type_t* arg_list[MCXX_MAX_FUNCTION_CALL_ARGUMENTS];
        memset(arg_list, 0, sizeof(arg_list));

        int i, num_args = 0;
        nodecl_t* nodecl_list = nodecl_unpack_list(initializer_clause_list, &num_args);

        ERROR_CONDITION(num_args >= MCXX_MAX_FUNCTION_CALL_ARGUMENTS, "Too many elements in braced initializer", 0);

        for (i = 0; i < num_args; i++)
        {
            nodecl_t nodecl_initializer_clause = nodecl_list[i];
            arg_list[i] = nodecl_get_type(nodecl_initializer_clause);
        }

        // Now construct the candidates for overloading among the constructors
        scope_entry_list_t* constructors = class_type_get_constructors(get_actual_class_type(declared_type));

        scope_entry_t* std_initializer_list_template = get_std_initializer_list_template(decl_context, 
                nodecl_get_filename(braced_initializer), 
                nodecl_get_line(braced_initializer),
                /* mandatory */ 0);

        char has_initializer_list_ctor = 0;

        // If std::initializer_list is not available there is no need to check
        // this because it would have failed before
        if (std_initializer_list_template != NULL)
        {
            scope_entry_list_iterator_t* it = NULL;
            for (it = entry_list_iterator_begin(constructors);
                    !entry_list_iterator_end(it) 
                    && !has_initializer_list_ctor;
                    entry_list_iterator_next(it))
            {
                scope_entry_t* entry = entry_list_iterator_current(it);

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
            entry_list_iterator_free(it);
        }

        entry_list_free(constructors);

        if (!has_initializer_list_ctor)
        {
            // Plain constructor resolution should be enough here
            scope_entry_t* conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 };
            scope_entry_list_t* candidates = NULL;
            scope_entry_t* constructor = solve_constructor(declared_type,
                    arg_list,
                    num_args,
                    /* is_explicit */ 0,
                    decl_context,
                    nodecl_get_filename(braced_initializer), 
                    nodecl_get_line(braced_initializer),
                    conversors,
                    &candidates);
            entry_list_free(candidates);

            if (constructor == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid initializer for type '%s'\n", 
                            nodecl_get_locus(braced_initializer),
                            print_type_str(declared_type, decl_context));
                }
                *nodecl_output = nodecl_make_err_expr(
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }
            else
            {
                if (function_has_been_deleted(decl_context, constructor, 
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer)))
                {
                    *nodecl_output = nodecl_make_err_expr(
                            nodecl_get_filename(braced_initializer), 
                            nodecl_get_line(braced_initializer));
                    return;
                }
                for (i = 0; i < num_args; i++)
                {
                    if (conversors[i] != NULL)
                    {
                        if (function_has_been_deleted(decl_context, conversors[i], 
                                    nodecl_get_filename(braced_initializer), 
                                    nodecl_get_line(braced_initializer)))
                        {
                            *nodecl_output = nodecl_make_err_expr(
                                    nodecl_get_filename(braced_initializer), 
                                    nodecl_get_line(braced_initializer));
                            return;
                        }
                    }
                }

                nodecl_t nodecl_arguments_output = nodecl_null();
                for (i = 0; i < num_args; i++)
                {
                    nodecl_t nodecl_current = nodecl_list[i];

                    if (conversors[i] != NULL)
                    {
                        nodecl_current = 
                            cxx_nodecl_make_function_call(
                                    nodecl_make_symbol(conversors[i], 
                                        nodecl_get_filename(nodecl_current), nodecl_get_line(nodecl_current)),
                                    nodecl_make_list_1(nodecl_current),
                                    actual_type_of_conversor(conversors[i]),
                                    nodecl_get_filename(nodecl_current), nodecl_get_line(nodecl_current));
                    }

                    nodecl_arguments_output = nodecl_append_to_list(nodecl_arguments_output,
                            nodecl_current);
                }

                *nodecl_output = cxx_nodecl_make_function_call(
                        nodecl_make_symbol(constructor, 
                            nodecl_get_filename(braced_initializer), 
                            nodecl_get_line(braced_initializer)),
                        nodecl_arguments_output,
                        declared_type,
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }
        }
        else
        {
            type_t* initializer_list_type = NULL;

            initializer_list_type = get_braced_list_type(num_args, arg_list);

            DEBUG_CODE()
            {
                fprintf(stderr, "EXPRTYPE: For initializer list, common type is '%s'\n", 
                       //  prettyprint_in_buffer(initializer), 
                        print_type_str(initializer_list_type, decl_context));
            }

            scope_entry_t* conversors[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 };

            template_parameter_list_t* template_parameters = 
                template_type_get_template_parameters(std_initializer_list_template->type_information);

            // Get a specialization of std::initializer_list<T> [ T <- initializer_list_type ]
            template_parameter_value_t* argument = counted_calloc(1, sizeof(*argument), &_bytes_used_expr_check);
            argument->kind = TPK_TYPE;
            argument->type = initializer_list_type;

            template_parameter_list_t* updated_template_parameters = duplicate_template_argument_list(template_parameters);
            updated_template_parameters->arguments[0] = argument;

            type_t* specialized_std_initializer = 
                template_type_get_specialized_type(std_initializer_list_template->type_information,
                        updated_template_parameters, decl_context, 
                        nodecl_get_filename(braced_initializer), nodecl_get_line(braced_initializer));

            // Now solve the constructor using this specialization
            // Should it be a const T&  ?
            arg_list[0] = specialized_std_initializer;
            num_args = 1;

            scope_entry_list_t* candidates = NULL;
            scope_entry_t* constructor = solve_constructor(declared_type,
                    arg_list,
                    num_args,
                    /* is_explicit */ 0,
                    decl_context,
                    nodecl_get_filename(braced_initializer), nodecl_get_line(braced_initializer),
                    conversors,
                    &candidates);
            entry_list_free(candidates);

            if (constructor == NULL)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: invalid initializer for type '%s'\n", 
                            nodecl_get_locus(braced_initializer),
                            print_type_str(declared_type, decl_context));
                }
                *nodecl_output = nodecl_make_err_expr(
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }
            else
            {
                if (function_has_been_deleted(decl_context, constructor, 
                            nodecl_get_filename(braced_initializer), 
                            nodecl_get_line(braced_initializer)))
                {
                    *nodecl_output = nodecl_make_err_expr(
                            nodecl_get_filename(braced_initializer), 
                            nodecl_get_line(braced_initializer));
                    return;
                }

                int j;
                for (j = 0; i < num_args; i++)
                {
                    if (conversors[j] != NULL)
                    {
                        if (function_has_been_deleted(decl_context, conversors[j], 
                                    nodecl_get_filename(braced_initializer), 
                                    nodecl_get_line(braced_initializer)))
                        {
                            *nodecl_output = nodecl_make_err_expr(
                                    nodecl_get_filename(braced_initializer), 
                                    nodecl_get_line(braced_initializer));
                            return;
                        }
                    }
                }

                nodecl_t nodecl_arguments_output = nodecl_null();

                for (j = 0; j < num_args; j++)
                {
                    nodecl_t nodecl_current = nodecl_list[j];

                    if (conversors[i] != NULL)
                    {
                        nodecl_current = 
                            cxx_nodecl_make_function_call(
                                    nodecl_make_symbol(conversors[i], 
                                        nodecl_get_filename(nodecl_current), nodecl_get_line(nodecl_current)),
                                    nodecl_make_list_1(nodecl_current),
                                    actual_type_of_conversor(conversors[i]),
                                    nodecl_get_filename(nodecl_current), nodecl_get_line(nodecl_current));
                    }

                    nodecl_arguments_output = nodecl_append_to_list(nodecl_arguments_output,
                            nodecl_current);
                }

                *nodecl_output = cxx_nodecl_make_function_call(
                        nodecl_make_symbol(constructor, nodecl_get_filename(braced_initializer), nodecl_get_line(braced_initializer)),
                        nodecl_make_list_1(nodecl_make_structured_value(nodecl_arguments_output,
                                specialized_std_initializer,
                                nodecl_get_filename(braced_initializer), nodecl_get_line(braced_initializer))),
                        declared_type,
                        nodecl_get_filename(braced_initializer), nodecl_get_line(braced_initializer));

                return;
            }
        }
    }
    // Not an aggregate of any kind
    else 
    {
        if (!nodecl_is_null(initializer_clause_list))
        {
            int num_items = nodecl_list_length(initializer_clause_list);

            if (num_items != 1)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s: error: brace initialization with more than one element is not valid here\n",
                            nodecl_get_locus(braced_initializer));
                }
                *nodecl_output = nodecl_make_err_expr(
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }

            nodecl_t initializer_clause = nodecl_list_head(initializer_clause_list);
            nodecl_t nodecl_expr_out = nodecl_null();
            check_nodecl_initializer_clause(initializer_clause, decl_context, declared_type, &nodecl_expr_out);

            if (nodecl_is_err_expr(nodecl_expr_out))
            {
                *nodecl_output = nodecl_make_err_expr(
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));
                return;
            }
            else
            {
                *nodecl_output = nodecl_make_structured_value(
                        nodecl_make_list_1(nodecl_expr_out),
                        declared_type,
                        nodecl_get_filename(braced_initializer), 
                        nodecl_get_line(braced_initializer));

                nodecl_set_constant(*nodecl_output, nodecl_get_constant(nodecl_expr_out));
            }
            return;
        }
        else
        {
            // Empty is OK
            // FIXME: Should we compute the default value instead of an empty tree?
            *nodecl_output = nodecl_make_structured_value(nodecl_null(), 
                    declared_type,
                    nodecl_get_filename(braced_initializer), 
                    nodecl_get_line(braced_initializer));
            return;
        }
    }

    internal_error("Code unreachable", 0);
}

static void check_nodecl_designation_type(nodecl_t nodecl_designation,
        decl_context_t decl_context, 
        type_t* declared_type,
        type_t** designated_type,
        scope_entry_t** designated_field,
        nodecl_t* nodecl_output)
{ 
    (*designated_type) = declared_type;

    int num_designators = 0;
    nodecl_t* nodecl_designator_list = nodecl_unpack_list(nodecl_designation, &num_designators);

    int i;
    char ok = 1;
    for (i = 0; i < num_designators && ok; i++)
    {
        nodecl_t nodecl_current_designator = nodecl_designator_list[i];

        switch (nodecl_get_kind(nodecl_current_designator))
        {
            case NODECL_C99_FIELD_DESIGNATOR:
                {
                    if (!is_class_type(*designated_type))
                    {
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: in designated initializer '%s', field designator not valid for type '%s'\n",
                                    nodecl_get_locus(nodecl_current_designator),
                                    c_cxx_codegen_to_str(nodecl_current_designator),
                                    print_type_str(*designated_type, decl_context));
                        }
                        ok = 0;
                    }
                    else
                    {

                        nodecl_t nodecl_name = nodecl_get_child(nodecl_current_designator, 0);
                        scope_entry_list_t* entry_list = get_member_of_class_type_nodecl(
                                decl_context,
                                *designated_type,
                                nodecl_name);
                        if (entry_list == NULL)
                        {
                            ok = 0;
                        }
                        else
                        {
                            scope_entry_t* entry = entry_list_head(entry_list);

                            if (entry->kind == SK_VARIABLE)
                            {
                                (*designated_type)  = entry->type_information;
                                (*designated_field) = entry;
                            }
                            else
                            {
                                ok = 0;
                            }
                        }
                    }
                    break;
                }
            case NODECL_C99_INDEX_DESIGNATOR:
                {
                    if (!is_array_type(*designated_type))
                    {
                        if (!checking_ambiguity())
                        {
                            error_printf("%s: in designated initializer '%s', subscript designator not valid for type '%s'\n",
                                    nodecl_get_locus(nodecl_current_designator),
                                    c_cxx_codegen_to_str(nodecl_current_designator),
                                    print_type_str(*designated_type, decl_context));
                        }
                        ok = 0;
                    }
                    else
                    {
                        // We should check that the array is not unbounded
                        *designated_type = array_type_get_element_type(*designated_type);
                    }
                    break;
                }
            default:
                {
                    internal_error("Invalid nodecl '%s'\n", ast_print_node_type(nodecl_get_kind(nodecl_current_designator)));
                }
        }
    }

    free(nodecl_designator_list);

    if (!ok)
    {
        *nodecl_output = nodecl_make_err_expr(
                nodecl_get_filename(nodecl_designation),
                nodecl_get_line(nodecl_designation));
    }
}

static void check_nodecl_designated_initializer(nodecl_t designated_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    type_t* designated_type  = NULL;
    //the designated_field is not useful in this case
    scope_entry_t* designated_field = NULL;
    nodecl_t error_designation = nodecl_null();

    check_nodecl_designation_type(nodecl_get_child(designated_initializer, 0), 
            decl_context, declared_type, &designated_type, &designated_field, &error_designation);
    
    if(!nodecl_is_null(error_designation) && nodecl_is_err_expr(error_designation)) 
    {
        *nodecl_output = nodecl_make_err_expr(
                nodecl_get_filename(error_designation),
                nodecl_get_line(error_designation));
        return;
    }

    nodecl_t nodecl_initializer_clause = nodecl_get_child(designated_initializer, 1);
    check_nodecl_initializer_clause(nodecl_initializer_clause, decl_context, designated_type, nodecl_output);
}

static void check_nodecl_parenthesized_initializer(nodecl_t direct_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(nodecl_get_kind(direct_initializer) != NODECL_CXX_PARENTHESIZED_INITIALIZER, "Invalid nodecl", 0);

    if (is_dependent_type(declared_type))
    {
        *nodecl_output = direct_initializer;
        nodecl_set_type(*nodecl_output, declared_type);
        return;
    }

    nodecl_t nodecl_list = nodecl_get_child(direct_initializer, 0);

    const char* filename = nodecl_get_filename(direct_initializer);
    int line = nodecl_get_line(direct_initializer);

    if (is_class_type(declared_type))
    {
        int num_arguments = nodecl_list_length(nodecl_list);
        type_t* arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 };

        int i, num_items = 0;
        nodecl_t* list = nodecl_unpack_list(nodecl_list, &num_items);
        scope_entry_t* conversors[num_items];

        for (i = 0; i < num_items; i++)
        {
            nodecl_t nodecl_expr = list[i];

            arguments[i] = nodecl_get_type(nodecl_expr);
            conversors[i] = NULL;
        }

        scope_entry_list_t* candidates = NULL;
        scope_entry_t* chosen_constructor = solve_constructor(declared_type,
                arguments, num_arguments,
                /* is_explicit */ 1,
                decl_context,
                filename, line,
                conversors,
                &candidates);

        if (chosen_constructor == NULL)
        {
            if (entry_list_size(candidates) != 0)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: no suitable constructor for class type '%s'\n",
                            filename, line,
                            print_type_str(declared_type, decl_context));
                    diagnostic_candidates(candidates, filename, line);
                }
                *nodecl_output = nodecl_make_err_expr(filename, line);
            }
            entry_list_free(candidates);
            return;
        }
        else
        {
            entry_list_free(candidates);
            if (function_has_been_deleted(decl_context, chosen_constructor, filename, line))
            {
                *nodecl_output = nodecl_make_err_expr(filename, line);
                return;
            }

            nodecl_t argument_list = nodecl_null();

            for (i = 0; i < num_arguments; i++)
            {
                nodecl_t nodecl_arg = list[i];

                if (conversors[i] != NULL)
                {
                    nodecl_arg = nodecl_make_function_call(
                            nodecl_make_symbol(conversors[i], nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg)),
                            nodecl_make_list_1(nodecl_arg),
                            actual_type_of_conversor(conversors[i]), nodecl_get_filename(nodecl_arg), nodecl_get_line(nodecl_arg));
                }

                argument_list = nodecl_append_to_list(argument_list, nodecl_arg);
            }

            *nodecl_output = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(chosen_constructor, filename, line),
                    argument_list,
                    actual_type_of_conversor(chosen_constructor),
                    filename, line);
        }
    }
    else
    {
        if (nodecl_list_length(nodecl_list) > 1)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: too many initializers when initializing '%s' type\n", 
                        nodecl_get_locus(direct_initializer),
                        print_type_str(declared_type, decl_context));
            }

            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(direct_initializer), nodecl_get_line(direct_initializer));
        }
        else if (nodecl_list_length(nodecl_list) == 1)
        {
            nodecl_t expr = nodecl_list_head(nodecl_list);

            check_nodecl_expr_initializer(expr, decl_context, declared_type, nodecl_output);

            *nodecl_output = nodecl_make_structured_value(
                    nodecl_make_list_1(*nodecl_output), 
                    declared_type, 
                    nodecl_get_filename(direct_initializer), 
                    nodecl_get_line(direct_initializer));

            if (!nodecl_is_err_expr(*nodecl_output))
            {
                if (nodecl_is_constant(expr)
                        && is_integral_type(declared_type))
                {
                    nodecl_set_constant(*nodecl_output, nodecl_get_constant(expr));
                }

                if (nodecl_expr_is_value_dependent(expr))
                {
                    nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
                }
            }
        }
        else
        {
            *nodecl_output = nodecl_make_structured_value(nodecl_null(), declared_type, 
                    nodecl_get_filename(direct_initializer), nodecl_get_line(direct_initializer));
        }
    }
}

static void compute_nodecl_braced_initializer(AST braced_initializer, decl_context_t decl_context, nodecl_t* nodecl_output);
static void compute_nodecl_designated_initializer(AST braced_initializer, decl_context_t decl_context, nodecl_t* nodecl_output);
static void compute_nodecl_gcc_initializer(AST braced_initializer, decl_context_t decl_context, nodecl_t* nodecl_output);

static void compute_nodecl_initializer_clause(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    switch (ASTType(initializer))
    {
        // Default is an expression
        default:
            {
                check_expression_impl_(initializer, decl_context, nodecl_output);
                break;
            }
        case AST_INITIALIZER_BRACES:
            {
                compute_nodecl_braced_initializer(initializer, decl_context, nodecl_output);
                break;
            }
        case AST_DESIGNATED_INITIALIZER :
            {
                compute_nodecl_designated_initializer(initializer, decl_context, nodecl_output);
                break;
            }
        case AST_GCC_INITIALIZER_CLAUSE :
            {
                compute_nodecl_gcc_initializer(initializer, decl_context, nodecl_output);
                break;

            }
            // default: is at the beginning of this switch
    }
}

void check_initializer_clause(AST initializer, decl_context_t decl_context, type_t* declared_type, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_init = nodecl_null();
    compute_nodecl_initializer_clause(initializer, decl_context, &nodecl_init);
    check_nodecl_initializer_clause(nodecl_init, decl_context, declared_type, nodecl_output);
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

static void check_nodecl_pointer_to_pointer_member(
        nodecl_t nodecl_lhs, 
        nodecl_t nodecl_rhs, 
        decl_context_t decl_context, 
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_lhs) 
            || nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_lhs)
            || nodecl_expr_is_type_dependent(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_cxx_arrow_ptr_member(
                nodecl_lhs,
                nodecl_rhs,
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* lhs_type = nodecl_get_type(nodecl_lhs);
    type_t* rhs_type = nodecl_get_type(nodecl_rhs);

    // This is an awkward operator, it requires overload if nodecl_lhs is not a
    // pointer to a class or if nodecl_rhs is a class type (and not a pointer to
    // member type)

    char requires_overload = 0;

    requires_overload = (!is_pointer_to_class_type(no_ref(lhs_type))
            || (!is_pointer_to_member_type(no_ref(rhs_type))
                && is_class_type(no_ref(rhs_type))));

    if (!requires_overload)
    {
        if (!is_pointer_to_class_type(no_ref(lhs_type)))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: '%s' does not have pointer to class type\n",
                        nodecl_get_locus(nodecl_lhs), c_cxx_codegen_to_str(nodecl_lhs));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (!is_pointer_to_member_type(no_ref(rhs_type)))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: '%s' is does not have pointer to member type\n",
                        nodecl_get_locus(nodecl_rhs), c_cxx_codegen_to_str(nodecl_rhs));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* pm_class_type = 
            pointer_to_member_type_get_class_type(no_ref(rhs_type));

        type_t* pointed_lhs_type =
            pointer_type_get_pointee_type(no_ref(lhs_type));

        if (!equivalent_types(
                    get_actual_class_type(pm_class_type),
                    get_actual_class_type(pointed_lhs_type))
                && !class_type_is_base(
                    get_actual_class_type(pm_class_type),
                    get_actual_class_type(pointed_lhs_type)))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: pointer to member of type '%s' is not compatible with an object of type '%s'\n",
                        filename, line,
                        print_type_str(no_ref(rhs_type), decl_context), 
                        print_type_str(no_ref(pointed_lhs_type), decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        type_t* pm_pointed_type = 
            pointer_type_get_pointee_type(no_ref(rhs_type));

        cv_qualifier_t cv_qualif_object = CV_NONE;
        advance_over_typedefs_with_cv_qualif(pointed_lhs_type, &cv_qualif_object);

        cv_qualifier_t cv_qualif_pointer = CV_NONE;
        advance_over_typedefs_with_cv_qualif(no_ref(rhs_type), &cv_qualif_pointer);

        type_t* t = lvalue_ref(
                    get_cv_qualified_type(
                        pm_pointed_type, 
                        cv_qualif_object | cv_qualif_pointer));

        *nodecl_output = nodecl_make_offset(
                nodecl_make_derreference(
                    nodecl_lhs,
                    lvalue_ref(pointed_lhs_type), 
                    nodecl_get_filename(nodecl_lhs), nodecl_get_line(nodecl_lhs)),
                nodecl_rhs,
                t, 
                filename, line);

        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
        return;
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
            lhs_type, rhs_type, 
            &builtin_set,
            decl_context, operation_tree, 
            operator_bin_pointer_to_pm_pred,
            operator_bin_pointer_to_pm_result);

    scope_entry_list_t* builtins = get_entry_list_from_builtin_operator_set(&builtin_set);

    scope_entry_t* selected_operator = NULL;

    type_t* computed_type = compute_user_defined_bin_operator_type(operation_tree, 
            &nodecl_lhs, &nodecl_rhs, builtins, decl_context, filename, line, &selected_operator);

    entry_list_free(builtins);

    if (computed_type != NULL)
    {
        if (selected_operator->entity_specs.is_builtin)
        {
            *nodecl_output = nodecl_make_offset(
                    nodecl_make_derreference(
                        nodecl_lhs,
                        lvalue_ref(pointer_type_get_pointee_type(lhs_type)), 
                        nodecl_get_filename(nodecl_lhs), nodecl_get_line(nodecl_lhs)),
                    nodecl_rhs,
                    computed_type, filename, line);
        }
        else
        {
            *nodecl_output =
                    cxx_nodecl_make_function_call(
                        nodecl_rhs,
                        nodecl_make_list_1(nodecl_lhs),
                        function_type_get_return_type(selected_operator->type_information),
                        filename, line);
        }

        return;
    }

    *nodecl_output = nodecl_make_err_expr(filename, line);
}

static void check_pointer_to_pointer_to_member(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    nodecl_t nodecl_lhs = nodecl_null();
    nodecl_t nodecl_rhs = nodecl_null();

    check_expression_impl_(lhs, decl_context, &nodecl_lhs);
    check_expression_impl_(rhs, decl_context, &nodecl_rhs);

    check_nodecl_pointer_to_pointer_member(nodecl_lhs, 
            nodecl_rhs, 
            decl_context, 
            ASTFileName(expression),
            ASTLine(expression),
            nodecl_output);
}

static void check_nodecl_pointer_to_member(
        nodecl_t nodecl_lhs, 
        nodecl_t nodecl_rhs, 
        decl_context_t decl_context, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_lhs)
            || nodecl_is_err_expr(nodecl_rhs))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_lhs)
            || nodecl_expr_is_type_dependent(nodecl_lhs))
    {
        *nodecl_output = nodecl_make_cxx_dot_ptr_member(
                nodecl_lhs,
                nodecl_rhs,
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    type_t* lhs_type = nodecl_get_type(nodecl_lhs);
    type_t* rhs_type = nodecl_get_type(nodecl_rhs);

    if (!is_class_type(no_ref(lhs_type)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' does not have class type\n",
                    nodecl_get_locus(nodecl_lhs), c_cxx_codegen_to_str(nodecl_lhs));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }
    if (!is_pointer_to_member_type(no_ref(rhs_type)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: '%s' is not a pointer to member\n",
                    nodecl_get_locus(nodecl_rhs), c_cxx_codegen_to_str(nodecl_rhs));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    type_t* pm_class_type = 
        pointer_to_member_type_get_class_type(no_ref(rhs_type));

    if (!equivalent_types(get_actual_class_type(no_ref(pm_class_type)), 
                get_actual_class_type(no_ref(lhs_type))) 
            && !class_type_is_base(
                get_actual_class_type(no_ref(pm_class_type)),
                get_actual_class_type(no_ref(lhs_type))))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s:%d: error: pointer to member of type '%s' is not compatible with an object of type '%s'\n",
                    filename, line,
                    print_type_str(no_ref(rhs_type), decl_context), print_type_str(no_ref(lhs_type), decl_context));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    cv_qualifier_t cv_qualif_object = CV_NONE;
    advance_over_typedefs_with_cv_qualif(no_ref(lhs_type), &cv_qualif_object);

    cv_qualifier_t cv_qualif_pointer = CV_NONE;
    advance_over_typedefs_with_cv_qualif(no_ref(rhs_type), &cv_qualif_pointer);

    type_t* t = lvalue_ref(
            get_cv_qualified_type(pointer_type_get_pointee_type(no_ref(rhs_type)), 
                cv_qualif_object | cv_qualif_pointer));

    *nodecl_output = nodecl_make_offset(nodecl_lhs,
            nodecl_rhs,
            t,
            filename, line);

    nodecl_expr_set_is_lvalue(*nodecl_output, 1);
}

static void check_pointer_to_member(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST lhs = ASTSon0(expression);
    AST rhs = ASTSon1(expression);

    nodecl_t nodecl_lhs = nodecl_null();
    nodecl_t nodecl_rhs = nodecl_null();

    check_expression_impl_(lhs, decl_context, &nodecl_lhs);
    check_expression_impl_(rhs, decl_context, &nodecl_rhs);

    check_nodecl_pointer_to_member(nodecl_lhs, nodecl_rhs, decl_context, 
            ASTFileName(expression),
            ASTLine(expression),
            nodecl_output);
}


static void compute_nodecl_equal_initializer(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST expr = ASTSon0(initializer);

    compute_nodecl_initializer_clause(expr, decl_context, nodecl_output);

    if (!nodecl_is_err_expr(*nodecl_output))
    {
        char is_type_dependent = nodecl_expr_is_type_dependent(*nodecl_output);
        type_t* t = nodecl_get_type(*nodecl_output);

        *nodecl_output = nodecl_make_cxx_equal_initializer(*nodecl_output, 
                ASTFileName(initializer), ASTLine(initializer));
        nodecl_expr_set_is_type_dependent(*nodecl_output, is_type_dependent);

        nodecl_set_type(*nodecl_output, t);
    }
}

static void compute_nodecl_braced_initializer(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST initializer_list = ASTSon0(initializer);

    char any_is_type_dependent = 0;
    *nodecl_output = nodecl_null();
    if (initializer_list != NULL)
    {
        AST it;
        for_each_element(initializer_list, it)
        {
            AST initializer_clause = ASTSon1(it);

            nodecl_t nodecl_initializer_clause = nodecl_null();
            compute_nodecl_initializer_clause(initializer_clause, decl_context, &nodecl_initializer_clause);

            if (nodecl_is_err_expr(nodecl_initializer_clause))
            {
                *nodecl_output = nodecl_initializer_clause;
                return;
            }

            any_is_type_dependent = any_is_type_dependent || 
                nodecl_expr_is_type_dependent(nodecl_initializer_clause);

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_initializer_clause);
        }
    }

    if (nodecl_is_null(*nodecl_output)
            || !nodecl_is_err_expr(*nodecl_output))
    {
        *nodecl_output = nodecl_make_cxx_braced_initializer(*nodecl_output, 
                ASTFileName(initializer), ASTLine(initializer));
        nodecl_expr_set_is_type_dependent(*nodecl_output, any_is_type_dependent);
    }
}

static void compute_nodecl_designator_list(AST designator_list, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST it;
    for_each_element(designator_list, it)
    {
        nodecl_t nodecl_designator = nodecl_null();
        AST designator = ASTSon1(it);
        switch (ASTType(designator))
        {
            case AST_INDEX_DESIGNATOR:
                {
                    nodecl_t nodecl_cexpr = nodecl_null();
                    AST constant_expr = ASTSon0(designator);
                    check_expression_impl_(constant_expr, decl_context, &nodecl_cexpr);

                    if (nodecl_is_err_expr(nodecl_cexpr))
                    {
                        *nodecl_output = nodecl_make_err_expr(ASTFileName(designator), ASTLine(designator));
                        return;
                    }

                    nodecl_designator = nodecl_make_c99_index_designator(nodecl_cexpr, 
                            nodecl_get_filename(nodecl_cexpr), 
                            nodecl_get_line(nodecl_cexpr));
                    break;
                }
            case AST_FIELD_DESIGNATOR:
                {
                    AST symbol = ASTSon0(designator);

                    nodecl_designator = nodecl_make_c99_field_designator(
                            nodecl_make_cxx_dep_name_simple(ASTText(symbol), 
                                ASTFileName(symbol), ASTLine(symbol)), 
                            ASTFileName(symbol), ASTLine(symbol));
                    break;
                }
            default:
                {
                    internal_error("Unexpected node kind '%s'\n", ast_print_node_type(ASTType(designator)));
                }
        }

        *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_designator);
    }
}

static void compute_nodecl_designation(AST designation, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST designator_list = ASTSon0(designation);
    compute_nodecl_designator_list(designator_list, decl_context, nodecl_output);
}

static void compute_nodecl_designated_initializer(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST designation = ASTSon0(initializer);
    AST initializer_clause = ASTSon1(initializer);

    nodecl_t nodecl_designation = nodecl_null();
    compute_nodecl_designation(designation, decl_context, &nodecl_designation);
    nodecl_t nodecl_initializer_clause = nodecl_null();
    compute_nodecl_initializer_clause(initializer_clause, decl_context, &nodecl_initializer_clause);

    if (nodecl_is_err_expr(nodecl_designation)
            || nodecl_is_err_expr(nodecl_initializer_clause))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(initializer), ASTLine(initializer));
        return;
    }

    *nodecl_output = nodecl_make_c99_designated_initializer(
            nodecl_designation, 
            nodecl_initializer_clause, 
            ASTFileName(initializer), 
            ASTLine(initializer));
}

static void compute_nodecl_gcc_initializer(AST initializer, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST symbol = ASTSon0(initializer);
    AST initializer_clause = ASTSon1(initializer);

    // Simplify it as if it were a standard C99 designator
    nodecl_t nodecl_designation = 
        nodecl_make_list_1(
                nodecl_make_c99_field_designator(
                    nodecl_make_cxx_dep_name_simple(ASTText(symbol), 
                        ASTFileName(symbol), ASTLine(symbol)),
                    ASTFileName(symbol), ASTLine(symbol)));
    nodecl_t nodecl_initializer_clause = nodecl_null();
    compute_nodecl_initializer_clause(initializer_clause, decl_context, &nodecl_initializer_clause);

    if (nodecl_is_err_expr(nodecl_designation)
            || nodecl_is_err_expr(nodecl_initializer_clause))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(initializer), ASTLine(initializer));
        return;
    }

    *nodecl_output = nodecl_make_c99_designated_initializer(
            nodecl_designation, 
            nodecl_initializer_clause, 
            ASTFileName(initializer), 
            ASTLine(initializer));
}

static void compute_nodecl_direct_initializer(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST initializer_list = ASTSon0(initializer);

    char any_is_type_dependent = 0;
    *nodecl_output = nodecl_null();
    if (initializer_list != NULL)
    {
        AST it;
        for_each_element(initializer_list, it)
        {
            AST initializer_clause = ASTSon1(it);

            nodecl_t nodecl_initializer_clause = nodecl_null();
            compute_nodecl_initializer_clause(initializer_clause, decl_context, &nodecl_initializer_clause);

            if (nodecl_is_err_expr(nodecl_initializer_clause))
            {
                *nodecl_output = nodecl_initializer_clause;
                return;
            }

            any_is_type_dependent = any_is_type_dependent || 
                nodecl_expr_is_type_dependent(nodecl_initializer_clause);

            *nodecl_output = nodecl_append_to_list(*nodecl_output, nodecl_initializer_clause);
        }
    }

    *nodecl_output = nodecl_make_cxx_parenthesized_initializer(*nodecl_output, 
            ASTFileName(initializer), ASTLine(initializer));
    nodecl_expr_set_is_type_dependent(*nodecl_output, any_is_type_dependent);
}

void compute_nodecl_initialization(AST initializer, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    switch (ASTType(initializer))
    {
        case AST_EQUAL_INITIALIZER:
            {
                compute_nodecl_equal_initializer(initializer, decl_context, nodecl_output);
                break;
            }
        case AST_INITIALIZER_BRACES:
            {
                compute_nodecl_braced_initializer(initializer, decl_context, nodecl_output);
                break;
            }
        case AST_PARENTHESIZED_INITIALIZER :
            {
                compute_nodecl_direct_initializer(initializer, decl_context, nodecl_output);
                break;
            }
        default:
            {
                internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(initializer)));
            }
    }
}


void check_nodecl_expr_initializer(nodecl_t nodecl_expr,
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Conversion from expression '%s' with type '%s' to type '%s'\n",
                c_cxx_codegen_to_str(nodecl_expr),
                print_declarator(nodecl_get_type(nodecl_expr)),
                print_declarator(declared_type));
    }
    type_t* initializer_expr_type = nodecl_get_type(nodecl_expr);
    type_t* declared_type_no_cv = get_unqualified_type(declared_type);

    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        nodecl_set_type(*nodecl_output, declared_type_no_cv);
        return;
    }

    // Now we have to check whether this can be converted to the declared entity
    C_LANGUAGE()
    {
        standard_conversion_t standard_conversion_sequence;
        if (!standard_conversion_between_types(
                    &standard_conversion_sequence,
                    initializer_expr_type,
                    declared_type_no_cv)
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
                error_printf("%s: error: initializer '%s' has type '%s' not convertible to '%s'\n",
                        nodecl_get_locus(nodecl_expr),
                        c_cxx_codegen_to_str(nodecl_expr),
                        print_decl_type_str(initializer_expr_type, decl_context, ""),
                        print_decl_type_str(declared_type, decl_context, ""));
            }
            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_expr), 
                    nodecl_get_line(nodecl_expr));
            return;
        }


        *nodecl_output = nodecl_expr;
        return;
    }

    // C++ only from now

    if (is_dependent_type(declared_type_no_cv))
    {
        *nodecl_output = nodecl_expr;
        return;
    }

    char ambiguous_conversion = 0;
    scope_entry_t* conversor = NULL;

    if (!is_class_type(declared_type_no_cv))
    {
        if (!type_can_be_implicitly_converted_to(initializer_expr_type, declared_type_no_cv, 
                    decl_context,
                    &ambiguous_conversion, &conversor, 
                    nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr))
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
                error_printf("%s: error: initializer '%s' has type '%s' not convertible to '%s'\n",
                        nodecl_get_locus(nodecl_expr),
                        c_cxx_codegen_to_str(nodecl_expr),
                        print_decl_type_str(initializer_expr_type, decl_context, ""),
                        print_decl_type_str(declared_type, decl_context, ""));
            }
            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_expr), 
                    nodecl_get_line(nodecl_expr));
            return;
        }

        if (conversor != NULL)
        {
            *nodecl_output = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(conversor,
                        nodecl_get_filename(nodecl_expr), 
                        nodecl_get_line(nodecl_expr)),
                    nodecl_make_list_1(nodecl_expr),
                    actual_type_of_conversor(conversor),
                    nodecl_get_filename(nodecl_expr), 
                    nodecl_get_line(nodecl_expr));
        }
        else
        {
            if (is_unresolved_overloaded_type(initializer_expr_type))
            {
                // Note: type_can_be_implicitly_converted_to already did this:
                // figure a way to get this conversion without having to compute
                // this address overload twice
                scope_entry_list_t* unresolved_set = unresolved_overloaded_type_get_overload_set(initializer_expr_type);
                scope_entry_t* solved_function = address_of_overloaded_function(unresolved_set,
                        unresolved_overloaded_type_get_explicit_template_arguments(initializer_expr_type),
                        no_ref(declared_type_no_cv),
                        decl_context,
                        nodecl_get_filename(nodecl_expr), 
                        nodecl_get_line(nodecl_expr));

                ERROR_CONDITION(solved_function == NULL, "Code unreachable", 0);

                if (!solved_function->entity_specs.is_member
                        || solved_function->entity_specs.is_static)
                {
                    nodecl_expr =
                        nodecl_make_symbol(solved_function, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                    nodecl_set_type(nodecl_expr, lvalue_ref(solved_function->type_information));
                }
                else
                {
                    nodecl_expr =
                        nodecl_make_pointer_to_member(solved_function, 
                                get_lvalue_reference_type(
                                    get_pointer_to_member_type(solved_function->type_information,
                                        named_type_get_symbol(solved_function->entity_specs.class_type))),
                                nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                }
            }

            *nodecl_output = nodecl_expr;
        }
    }
    else
    {
        int num_arguments = 1;
        type_t* arguments[MCXX_MAX_FUNCTION_CALL_ARGUMENTS] = { 0 };
        scope_entry_t* conversors[1];

        arguments[0] = initializer_expr_type;
        conversors[0] = NULL;

        scope_entry_list_t* candidates = NULL;
        scope_entry_t* chosen_constructor = solve_constructor(declared_type_no_cv,
                arguments, num_arguments,
                /* is_explicit */ 0,
                decl_context,
                nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr),
                conversors,
                &candidates);
        
        if (chosen_constructor == NULL)
        {
            if (entry_list_size(candidates) != 0)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: no suitable constructor for direct initialization of type '%s' using an expression of type '%s'\n",
                            nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr),
                            print_type_str(initializer_expr_type, decl_context),
                            print_type_str(declared_type_no_cv, decl_context));
                    diagnostic_candidates(candidates, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
                }
            }
            entry_list_free(candidates);

            *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_expr), 
                    nodecl_get_line(nodecl_expr));
            return;
        }
        else
        {
            entry_list_free(candidates);
            if (function_has_been_deleted(decl_context, chosen_constructor, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr)))
            {
                *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_expr), 
                        nodecl_get_line(nodecl_expr));
                return;
            }

            conversor = conversors[0];
        }

        if (conversor != NULL)
        {
            nodecl_expr = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(conversor, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr)),
                    nodecl_make_list_1(nodecl_expr),
                    actual_type_of_conversor(conversor),
                    nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
        }

        // Remember a call to the constructor here
        *nodecl_output = cxx_nodecl_make_function_call(
                    nodecl_make_symbol(chosen_constructor, nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr)),
                    nodecl_make_list_1(nodecl_expr),
                    declared_type_no_cv,
                    nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
    }
}

void check_nodecl_equal_initializer(nodecl_t nodecl_initializer, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    ERROR_CONDITION(nodecl_get_kind(nodecl_initializer) != NODECL_CXX_EQUAL_INITIALIZER, "Invalid nodecl", 0);

    nodecl_t nodecl_expr = nodecl_get_child(nodecl_initializer, 0);
    check_nodecl_initializer_clause(nodecl_expr, decl_context, declared_type, nodecl_output);
}

void check_initialization_nodecl(nodecl_t nodecl_initializer, decl_context_t decl_context, type_t* declared_type, nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_initializer))
    {
        *nodecl_output = nodecl_initializer;
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_initializer))
    {
        *nodecl_output = nodecl_initializer;
        return;
    }

    switch (nodecl_get_kind(nodecl_initializer))
    {
        case NODECL_CXX_EQUAL_INITIALIZER:
            {
                check_nodecl_equal_initializer(nodecl_initializer, decl_context, declared_type, nodecl_output);
                break;
            }
        case NODECL_CXX_BRACED_INITIALIZER:
            {
                check_nodecl_braced_initializer(nodecl_initializer, decl_context, declared_type, nodecl_output);
                break;
            }
        case NODECL_CXX_PARENTHESIZED_INITIALIZER:
            {
                check_nodecl_parenthesized_initializer(nodecl_initializer, decl_context, declared_type, nodecl_output);
                break;
            }
        default:
            {
                internal_error("Unexpected initializer", 0);
            }
    }
}

static void check_nodecl_initializer_clause(nodecl_t initializer_clause, 
        decl_context_t decl_context, 
        type_t* declared_type, 
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(initializer_clause))
    {
        *nodecl_output = initializer_clause;
        return;
    }

    if (nodecl_expr_is_type_dependent(initializer_clause))
    {
        *nodecl_output = initializer_clause;
        return;
    }

    switch (nodecl_get_kind(initializer_clause))
    {
        // Default should be an expression
        default:
            {
                check_nodecl_expr_initializer(initializer_clause, decl_context, declared_type, nodecl_output);
                break;
            }
        case NODECL_CXX_BRACED_INITIALIZER:
            {
                check_nodecl_braced_initializer(initializer_clause, decl_context, declared_type, nodecl_output);
                break;
            }
        case NODECL_C99_DESIGNATED_INITIALIZER:
            {
                // Note: GCC-style designated initializers are subsumed in NODECL_C99_DESIGNATED_INITIALIZER
                check_nodecl_designated_initializer(initializer_clause, decl_context, declared_type, nodecl_output);
                break;
            }
    }
}

char check_initialization(AST initializer, decl_context_t decl_context, type_t* declared_type, nodecl_t* nodecl_output)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "EXPRTYPE: Checking initializer '%s'\n",
                prettyprint_in_buffer(initializer));
    }

    nodecl_t nodecl_init = nodecl_null();

    compute_nodecl_initialization(initializer, decl_context, &nodecl_init);

    check_initialization_nodecl(nodecl_init, decl_context, declared_type, nodecl_output);
    
    DEBUG_CODE()
    {
        if (!nodecl_is_err_expr(*nodecl_output))
        {
            fprintf(stderr, "EXPRTYPE: Initializer '%s' has type '%s'",
                    prettyprint_in_buffer(initializer),
                    nodecl_get_type(*nodecl_output) == NULL 
                    ? "<< no type >>" 
                    : print_declarator(nodecl_get_type(*nodecl_output)));

            if (nodecl_is_constant(*nodecl_output))
            {
                const_value_t* v = nodecl_get_constant(*nodecl_output);
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
            && nodecl_is_err_expr(*nodecl_output))
    {
        internal_error("Initializer '%s' at '%s' does not have a valid computed type\n",
                prettyprint_in_buffer(initializer),
                ast_location(initializer));
    }

    return !nodecl_is_err_expr(*nodecl_output);
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

            ERROR_CONDITION(function_result_type == NULL 
                    || is_error_type(function_result_type), "This type cannot be NULL!", 0);

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

                ERROR_CONDITION(function_result_type == NULL 
                        || is_error_type(function_result_type), "This type cannot be NULL!", 0);
                
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

                    ERROR_CONDITION(function_result_type == NULL 
                            || is_error_type(function_result_type), "This type cannot be NULL!", 0);

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

static void check_sizeof_type(type_t* t, decl_context_t decl_context, const char* filename, int line, nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_make_sizeof(
            nodecl_make_type(t, filename, line),
            get_size_t_type(),
            filename, line);
    
    if (!is_dependent_type(t)
            && !type_is_runtime_sized(t))
    {
        CXX_LANGUAGE()
        {
            if (is_named_class_type(t)
                    && class_type_is_incomplete_independent(get_actual_class_type(t)))
            {
                scope_entry_t* symbol = named_type_get_symbol(t);
                instantiate_template_class(symbol, decl_context, 
                       filename, line);
            }
        }

        if (is_incomplete_type(t))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: sizeof of incomplete type '%s'\n", 
                        filename, line,
                        print_type_str(t, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            _size_t type_size = 0;
            type_size = type_get_size(t);


            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "EXPRTYPE: %s:%d: sizeof yields a value of %zu\n",
                        filename, line, type_size);
            }

            nodecl_set_constant(*nodecl_output,
                    const_value_get_integer(type_size, type_get_size(get_size_t_type()), 0));
        }
    }
    else if (is_dependent_type(t))
    {
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
    }
}

static void check_sizeof_expr(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST sizeof_expression = ASTSon0(expr);

    nodecl_t nodecl_expr = nodecl_null();
    check_expression_impl_(sizeof_expression, decl_context, &nodecl_expr);

    const char* filename = ASTFileName(expr);
    int line = ASTLine(expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        *nodecl_output = nodecl_make_cxx_sizeof(nodecl_expr, get_size_t_type(), filename, line);
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        return;
    }

    type_t* t = nodecl_get_type(nodecl_expr);

    check_sizeof_type(t, decl_context, filename, line, nodecl_output);
}

static void check_sizeof_typeid(AST expr, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expr);
    int line = ASTLine(expr);

    AST type_id = ASTSon0(expr);

    type_t* declarator_type = compute_type_for_type_id_tree(type_id, decl_context);
    if (is_error_type(declarator_type))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    check_sizeof_type(declarator_type, decl_context, filename, line, nodecl_output);
}


static void check_vla_expression(AST expression UNUSED_PARAMETER, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output UNUSED_PARAMETER)
{
    // This is not actually an expression per se, but it appears in
    // a place where 99% of the time an expression is used instead,
    // so instead of filling the code with checks for this node
    // type, accept it in the club of expressions as a reference
    // to a fake symbol called '*'
    static scope_entry_t* star_symbol = NULL;
    if (star_symbol == NULL)
    {
        star_symbol = counted_calloc(1, sizeof(*star_symbol), &_bytes_used_expr_check);
        star_symbol->symbol_name = "*";
        star_symbol->decl_context = CURRENT_COMPILED_FILE->global_decl_context;
        star_symbol->do_not_print = 1;
        star_symbol->type_information = get_signed_int_type();
        star_symbol->entity_specs.is_builtin = 1;
    }
    *nodecl_output = nodecl_make_symbol(star_symbol, ASTFileName(expression), ASTLine(expression));
    nodecl_set_type(*nodecl_output, star_symbol->type_information);
}

static void compute_nodecl_gcc_offset_designation(AST gcc_offset_designator, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);

static void check_gcc_offset_designation(nodecl_t nodecl_designator, 
        decl_context_t decl_context,
        type_t* accessed_type, 
        nodecl_t* nodecl_output,
        const char* filename,
        int line)
{

    if (!is_complete_type(accessed_type))
    {
        error_printf("%s:%d: error: invalid use of incomplete type '%s'\n", 
                filename, line,
                print_type_str(accessed_type, decl_context));
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    //the designated_type is not useful in this case
    type_t* designated_type  = NULL;
    scope_entry_t* designated_field = NULL;
    nodecl_t error_designation = nodecl_null(); 
   
   check_nodecl_designation_type(nodecl_designator, decl_context, 
        accessed_type, &designated_type, &designated_field, &error_designation);
    
    if(!nodecl_is_null(error_designation) && nodecl_is_err_expr(error_designation)) 
    {
        *nodecl_output = nodecl_make_err_expr(
                nodecl_get_filename(error_designation),
                nodecl_get_line(error_designation));
        return;
    }

    type_get_size(accessed_type);
    size_t offset_field = designated_field->entity_specs.field_offset;
  
    *nodecl_output = nodecl_make_offsetof(nodecl_make_type(accessed_type, filename, line),
            nodecl_designator, get_signed_int_type(),filename, line);

    const_value_t* cval = const_value_get_integer(offset_field,sizeof(size_t),0);
    nodecl_set_constant(*nodecl_output, cval);
}

static void check_gcc_builtin_offsetof(AST expression,
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    AST type_id = ASTSon0(expression);
    AST member_designator = ASTSon1(expression);
    
    type_t* accessed_type = compute_type_for_type_id_tree(type_id, decl_context);

    if (is_error_type(accessed_type))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    nodecl_t nodecl_designator = nodecl_null();
    compute_nodecl_gcc_offset_designation(member_designator, decl_context, &nodecl_designator);
    
    if (is_dependent_type(accessed_type))
    {
        *nodecl_output = nodecl_make_offsetof(
                nodecl_make_type(accessed_type, filename, line),
                nodecl_designator, 
                get_signed_int_type(), 
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        nodecl_expr_set_is_value_dependent(*nodecl_output, 1);
        return;
    }

    // Check the designator and synthesize an offset value
    check_gcc_offset_designation(nodecl_designator, decl_context, accessed_type, nodecl_output, 
            filename, line);
}

static void compute_nodecl_gcc_offset_designation(AST gcc_offset_designator, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST name = ASTSon0(gcc_offset_designator);
    AST designator_list = ASTSon1(gcc_offset_designator);

    nodecl_t nodecl_designator = nodecl_make_list_1(
            nodecl_make_c99_field_designator(
                nodecl_make_cxx_dep_name_simple(ASTText(name), 
                    ASTFileName(name), ASTLine(name)),
                ASTFileName(name), ASTLine(name)));

    nodecl_t nodecl_designator_list = nodecl_null();
    if (designator_list != NULL)
    {
        compute_nodecl_designator_list(designator_list, decl_context, &nodecl_designator_list);
        *nodecl_output = nodecl_concat_lists(nodecl_designator, nodecl_designator_list);
    }
    else
    {
        *nodecl_output = nodecl_designator;
    }
}

static void check_gcc_builtin_choose_expr(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    AST selector_expr = ASTSon0(expression);
    AST first_expr = ASTSon1(expression);
    AST second_expr = ASTSon2(expression);

    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    // Since the exact type of this expression depends on the value yield by selector_expr
    // we will check the selector_expr and then evaluate it. 
    //
    // Note that this is only valid for C so we do not need to check
    // whether the expression is dependent

    nodecl_t nodecl_selector = nodecl_null();
    check_expression_impl_(selector_expr, decl_context, &nodecl_selector);
    if (nodecl_is_err_expr(nodecl_selector))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (!nodecl_is_constant(nodecl_selector))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (const_value_is_nonzero(nodecl_get_constant(nodecl_selector)))
    {
        check_expression_impl_(first_expr, decl_context, nodecl_output);
    }
    else
    {
        check_expression_impl_(second_expr, decl_context, nodecl_output);
    }
}

static void check_gcc_builtin_types_compatible_p(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    // This builtin always returns an integer type
    AST first_type_tree = ASTSon0(expression);
    AST second_type_tree = ASTSon1(expression);

    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    type_t* first_type = compute_type_for_type_id_tree(first_type_tree, decl_context);
    type_t* second_type = compute_type_for_type_id_tree(second_type_tree, decl_context);

    if (is_error_type(first_type)
            || is_error_type(second_type))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (!is_dependent_type(first_type)
            && !is_dependent_type(second_type))
    {
        const_value_t* value = 
                equivalent_types(first_type, second_type) ?  
                const_value_get_one(/*bytes*/ 1, /*signed*/ 0) 
                : const_value_get_zero(/*bytes*/ 1,  /*signed*/ 0);
        *nodecl_output = const_value_to_nodecl(value);
    }
    else
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: __builtin_types_compatible_p is not implemented for dependent expressions",
                    ast_location(expression));
        }
    }
}

static void check_gcc_label_addr(AST expression, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output)
{
    AST label = ASTSon0(expression);

    scope_entry_t* sym_label = add_label_if_not_found(label, decl_context);

    // Codegen will take care of this symbol and print &&label instead of &label
    *nodecl_output = nodecl_make_reference(
            nodecl_make_symbol(sym_label, ASTFileName(label), ASTLine(label)),
            get_pointer_type(get_void_type()),
            ASTFileName(expression),
            ASTLine(expression));
}

static void check_nodecl_gcc_real_or_imag_part(nodecl_t nodecl_expr,
        decl_context_t decl_context UNUSED_PARAMETER,
        char is_real,
        const char *filename,
        int line,
        nodecl_t* nodecl_output)
{
    type_t* result_type = NULL;
    char is_lvalue = 0;
    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        // OK
        result_type = get_unknown_dependent_type();
    }
    else
    {
        type_t* t = no_ref(nodecl_get_type(nodecl_expr));
        if (!is_complex_type(t))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: operand of '%s' is not a complex type\n",
                        nodecl_get_locus(nodecl_expr),
                        is_real ? "__real__" : "__imag__");
            }

            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        result_type = complex_type_get_base_type(t);

        if (is_lvalue_reference_type(t))
        {
            result_type = lvalue_ref(result_type);
            is_lvalue = 1;
        }
    }

    nodecl_t (*fun)(nodecl_t, type_t*, const char*, int line) = nodecl_make_imag_part;
    if (is_real)
    {
        fun = nodecl_make_real_part;
    }

    *nodecl_output = fun(nodecl_expr, result_type, filename, line);

    if (is_lvalue)
    {
        nodecl_expr_set_is_lvalue(*nodecl_output, 1);
    }
}

static void check_gcc_real_or_imag_part(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    char is_real = (ASTType(expression) == AST_GCC_REAL_PART);

    nodecl_t nodecl_expr = nodecl_null();
    check_expression_impl_(ASTSon0(expression), decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
    }

    check_nodecl_gcc_real_or_imag_part(nodecl_expr, 
            decl_context, is_real, 
            ASTFileName(expression), ASTLine(expression), 
            nodecl_output);
}

static void check_gcc_alignof_type(type_t* t,
        decl_context_t decl_context,
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{

    if (is_dependent_type(t))
    {
        *nodecl_output = nodecl_make_alignof(nodecl_make_type(t, filename, line), get_size_t_type(), filename, line);
    }
    else
    {
        CXX_LANGUAGE()
        {
            if (is_named_class_type(t)
                    && class_type_is_incomplete_independent(get_actual_class_type(t)))
            {
                scope_entry_t* symbol = named_type_get_symbol(t);
                instantiate_template_class(symbol, decl_context, filename, line);
            }
        }

        if (is_incomplete_type(t))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: alignof of incomplete type '%s'\n", 
                        filename, line,
                        print_type_str(t, decl_context));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        *nodecl_output = nodecl_make_alignof(nodecl_make_type(t, filename, line), get_size_t_type(), filename, line);

        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            _size_t type_alignment = type_get_alignment(t);

            DEBUG_SIZEOF_CODE()
            {
                fprintf(stderr, "EXPRTYPE: %s:%d: alignof yields a value of %zu\n",
                        filename, line, type_alignment);
            }

            nodecl_set_constant(*nodecl_output,
                    const_value_get_integer(type_alignment, type_get_size(get_size_t_type()), 0));
        }
    }
}

static void check_nodecl_gcc_alignof_expr(nodecl_t nodecl_expr,
        decl_context_t decl_context,
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        *nodecl_output = nodecl_make_cxx_alignof(nodecl_expr, get_size_t_type(), filename, line);
        return;
    }

    type_t* t = nodecl_get_type(nodecl_expr);

    check_gcc_alignof_type(t, decl_context, filename, line, nodecl_output);
}

static void check_gcc_alignof_expr(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    check_expression_impl_(expression, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(expression), ASTLine(expression));
    }

    check_nodecl_gcc_alignof_expr(nodecl_expr, decl_context, ASTFileName(expression), ASTLine(expression), nodecl_output);
}

static void check_gcc_alignof_typeid(AST type_id, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    type_t* t = compute_type_for_type_id_tree(type_id, decl_context);

    if (is_error_type(t))
    {
        *nodecl_output = nodecl_make_err_expr(ASTFileName(type_id), ASTLine(type_id));
        return;
    }

    check_gcc_alignof_type(t, decl_context, ASTFileName(type_id), ASTLine(type_id), nodecl_output);
}

static void check_gcc_postfix_expression(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    AST type_id = ASTSon0(expression);

    type_t* t = compute_type_for_type_id_tree(type_id, decl_context);

    if (is_error_type(t))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    AST braced_init_list = ASTSon1(expression);

    nodecl_t nodecl_braced_init = nodecl_null();
    compute_nodecl_braced_initializer(braced_init_list, decl_context, &nodecl_braced_init);

    if (nodecl_is_err_expr(nodecl_braced_init))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (is_dependent_type(t))
    {
        *nodecl_output = nodecl_make_cxx_postfix_initializer(nodecl_braced_init, t, filename, line);
        return;
    }

    check_nodecl_braced_initializer(nodecl_braced_init, decl_context, t, nodecl_output);
}

static void check_nodecl_gcc_parenthesized_expression(nodecl_t nodecl_context, 
        decl_context_t decl_context UNUSED_PARAMETER, 
        const char* filename, int line,
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_compound = nodecl_list_head(nodecl_get_child(nodecl_context, 0));

    ERROR_CONDITION(nodecl_get_kind(nodecl_compound) != NODECL_COMPOUND_STATEMENT, "Invalid node", 0);

    nodecl_t nodecl_list_of_stmts = nodecl_get_child(nodecl_compound, 0);

    type_t* computed_type = NULL;

    int num_items = 0;
    nodecl_t* nodecl_list = nodecl_unpack_list(nodecl_list_of_stmts, &num_items);

    if (num_items > 0)
    {
        nodecl_t nodecl_last_stmt = nodecl_list[num_items - 1];

        if (nodecl_get_kind(nodecl_last_stmt) != NODECL_EXPRESSION_STATEMENT)
        {
            if (!checking_ambiguity())
            {
                error_printf("%s:%d: error: last statement must be an expression statement\n",
                        filename, line);
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }

        nodecl_t nodecl_expr = nodecl_get_child(nodecl_last_stmt, 0);

        computed_type = nodecl_get_type(nodecl_expr);
    }
    else if (num_items == 0)
    {
        computed_type = get_void_type();
    }

    if (computed_type == NULL
            || is_error_type(computed_type))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    free(nodecl_list);

    *nodecl_output = nodecl_make_compound_expression(
            nodecl_context,
            computed_type,
            filename, line);
}

static void check_gcc_parenthesized_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    AST compound_statement = ASTSon0(expression);
    nodecl_t nodecl_stmt_seq = nodecl_null();

    // This returns a list, we only want the first item of that list
    build_scope_statement(compound_statement, decl_context, &nodecl_stmt_seq);

    ERROR_CONDITION(nodecl_list_length(nodecl_stmt_seq) != 1, "This should be 1", 0);

    nodecl_t nodecl_context = nodecl_list_head(nodecl_stmt_seq);
    ERROR_CONDITION(nodecl_get_kind(nodecl_context) != NODECL_CONTEXT, "Invalid tree", 0);

    check_nodecl_gcc_parenthesized_expression(nodecl_context, 
            decl_context, 
            ASTFileName(expression), 
            ASTLine(expression),
            nodecl_output);
}

static void check_gcc_builtin_va_arg(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    // This is an historic builtin we do not handle using the generic builtin
    // mechanism since it has very special syntax involving types
    nodecl_t nodecl_expr = nodecl_null();
    check_expression_impl_(ASTSon0(expression), decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    AST type_id = ASTSon1(expression);

    type_t* t = compute_type_for_type_id_tree(type_id, decl_context);

    if (is_error_type(t))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    *nodecl_output = nodecl_make_builtin_expr(
            nodecl_make_any_list(
                nodecl_make_list_2(
                    nodecl_expr,
                    nodecl_make_type(t, filename, line)),
                filename, line),
            t,
            "__builtin_va_arg",
            filename, line);
}

static void check_nodecl_array_section_expression(nodecl_t nodecl_postfix,
        nodecl_t nodecl_lower,
        nodecl_t nodecl_upper,
        decl_context_t decl_context, 
        char is_array_section_size,
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_is_err_expr(nodecl_postfix)
            || (!nodecl_is_null(nodecl_lower) && nodecl_is_err_expr(nodecl_lower))
            || (!nodecl_is_null(nodecl_upper) && nodecl_is_err_expr(nodecl_upper)))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (nodecl_expr_is_type_dependent(nodecl_postfix)
            || (!nodecl_is_null(nodecl_lower) && nodecl_expr_is_type_dependent(nodecl_lower))
            || (!nodecl_is_null(nodecl_upper) && nodecl_expr_is_type_dependent(nodecl_upper)))
    {
        if (is_array_section_size)
        {
            *nodecl_output = nodecl_make_cxx_array_section_size(nodecl_postfix, nodecl_lower, nodecl_upper, 
                    get_unknown_dependent_type(),
                    filename, line);
        }
        else
        {
            *nodecl_output = nodecl_make_cxx_array_section_range(nodecl_postfix, nodecl_lower, nodecl_upper, 
                    get_unknown_dependent_type(),
                    filename, line);
        }

        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }


    type_t* indexed_type = no_ref(nodecl_get_type(nodecl_postfix));

    if (nodecl_is_null(nodecl_lower) && (is_array_type(indexed_type) || is_pointer_type(indexed_type))) 
        nodecl_lower = array_type_get_array_lower_bound(indexed_type);

    if (nodecl_is_null(nodecl_upper) && (is_array_type(indexed_type))) 
        nodecl_upper = array_type_get_array_upper_bound(indexed_type);

#define MAX_NESTING_OF_ARRAY_REGIONS (16)

    type_t* advanced_types[MAX_NESTING_OF_ARRAY_REGIONS];
    int i = 0;
    while (is_array_type(indexed_type) && array_type_has_region(indexed_type))
    {
        ERROR_CONDITION(i == MAX_NESTING_OF_ARRAY_REGIONS, "Too many array regions nested %d\n", i);
        advanced_types[i] = indexed_type;
        indexed_type = array_type_get_element_type(indexed_type);
        i++;
    }

    if (is_array_section_size)
    {
        // L;U -> L: (U + L) - 1
        nodecl_upper = 
            nodecl_make_minus(
                    nodecl_make_add(
                        nodecl_copy(nodecl_upper), nodecl_copy(nodecl_lower),
                        get_signed_int_type(), 
                        nodecl_get_filename(nodecl_upper), 
                        nodecl_get_line(nodecl_upper)),
                    nodecl_make_integer_literal(
                        get_signed_int_type(),
                        const_value_get_one(type_get_size(get_signed_int_type()), 1),
                        nodecl_get_filename(nodecl_upper), 
                        nodecl_get_line(nodecl_upper)),
                    get_signed_int_type(),
                    nodecl_get_filename(nodecl_upper),
                    nodecl_get_line(nodecl_upper));
    }


    type_t* result_type = NULL;
    if (is_array_type(indexed_type))
    {
        nodecl_t array_lower_bound = array_type_get_array_lower_bound(indexed_type);
        nodecl_t array_upper_bound = array_type_get_array_upper_bound(indexed_type);
        decl_context_t array_decl_context = array_type_get_array_size_expr_context(indexed_type);

        result_type = get_array_type_bounds_with_regions(
                array_type_get_element_type(indexed_type),
                array_lower_bound,
                array_upper_bound,
                array_decl_context,
                nodecl_lower,
                nodecl_upper,
                decl_context);
    }
    else if (is_pointer_type(indexed_type))
    {
        if (i > 0)
        {
            if (!checking_ambiguity())
            {
                fprintf(stderr, "%s: warning: pointer types only allow one-level array sections\n",
                        nodecl_get_locus(nodecl_postfix));
            }
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
        result_type = get_array_type_bounds_with_regions(
                pointer_type_get_pointee_type(indexed_type),
                nodecl_lower,
                nodecl_upper,
                decl_context,
                nodecl_lower,
                nodecl_upper,
                decl_context);
    }
    else
    {
        if (!checking_ambiguity())
        {
            fprintf(stderr, "%s: warning: array section is invalid since '%s' has type '%s'\n",
                    nodecl_get_locus(nodecl_postfix),
                    c_cxx_codegen_to_str(nodecl_postfix),
                    print_type_str(indexed_type, decl_context));
        }
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    while (i > 0)
    {
        type_t* current_array_type = advanced_types[i - 1];
        nodecl_t array_lower_bound = array_type_get_array_lower_bound(current_array_type);
        nodecl_t array_upper_bound = array_type_get_array_upper_bound(current_array_type);
        nodecl_t array_region_lower_bound = array_type_get_region_lower_bound(current_array_type);
        nodecl_t array_region_upper_bound = array_type_get_region_upper_bound(current_array_type);
        decl_context_t array_decl_context = array_type_get_array_size_expr_context(current_array_type);

        result_type = get_array_type_bounds_with_regions(
                result_type,
                array_lower_bound,
                array_upper_bound,
                array_decl_context,
                array_region_lower_bound,
                array_region_upper_bound,
                decl_context);
        i--;
    }

    // Should the type be a reference in C++?

    *nodecl_output = nodecl_make_array_section(nodecl_postfix, nodecl_lower, nodecl_upper, 
            result_type,
            filename, line);
    nodecl_expr_set_is_lvalue(*nodecl_output, 1);
}

static void check_array_section_expression(AST expression, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    AST postfix_expression = ASTSon0(expression);
    AST lower_bound = ASTSon1(expression);
    AST upper_bound = ASTSon2(expression);

    nodecl_t nodecl_postfix = nodecl_null();
    check_expression_impl_(postfix_expression, decl_context, &nodecl_postfix);

    nodecl_t nodecl_lower = nodecl_null();
    if (lower_bound != NULL)
        check_expression_impl_(lower_bound, decl_context, &nodecl_lower);

    nodecl_t nodecl_upper = nodecl_null();
    if (upper_bound != NULL)
        check_expression_impl_(upper_bound, decl_context, &nodecl_upper);

    char is_array_section_size = (ASTType(expression) == AST_ARRAY_SECTION_SIZE);

    check_nodecl_array_section_expression(nodecl_postfix, nodecl_lower, nodecl_upper,
            decl_context, is_array_section_size, filename, line, nodecl_output);
}

static void check_nodecl_shaping_expression(nodecl_t nodecl_shaped_expr,
        nodecl_t nodecl_shape_list,
        decl_context_t decl_context UNUSED_PARAMETER, 
        const char* filename,
        int line,
        nodecl_t* nodecl_output)
{
    if (nodecl_expr_is_type_dependent(nodecl_shaped_expr)
            || nodecl_expr_is_type_dependent(nodecl_shape_list))
    {
        *nodecl_output = nodecl_make_shaping(nodecl_shaped_expr, 
                nodecl_shape_list, 
                get_unknown_dependent_type(),
                filename, line);
        nodecl_expr_set_is_type_dependent(*nodecl_output, 1);
        return;
    }

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_shape_list, &num_items);

    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t current_expr = list[i];

        type_t *current_expr_type = nodecl_get_type(current_expr);

        standard_conversion_t scs;
        if (!standard_conversion_between_types(&scs, no_ref(current_expr_type), get_signed_int_type()))
        {
            if (!checking_ambiguity())
            {
                error_printf("%s: error: shaping expression '%s' cannot be converted to 'int'\n",
                        nodecl_get_locus(current_expr),
                        c_cxx_codegen_to_str(current_expr));
            }
            free(list);
            *nodecl_output = nodecl_make_err_expr(filename, line);
            return;
        }
    }

    // Now check the shape makes sense
    type_t* shaped_expr_type = nodecl_get_type(nodecl_shaped_expr);

    // Array to pointer conversion
    if (is_array_type(no_ref(shaped_expr_type)))
    {
        shaped_expr_type = get_pointer_type(array_type_get_element_type(no_ref(shaped_expr_type)));
    }

    if (!is_pointer_type(no_ref(shaped_expr_type)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: shaped expression '%s' does not have pointer type\n",
                    nodecl_get_locus(nodecl_shaped_expr),
                    c_cxx_codegen_to_str(nodecl_shaped_expr));
        }
        free(list);
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    if (is_void_pointer_type(no_ref(shaped_expr_type)))
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: shaped expression '%s' has type 'void*' which is invalid\n",
                    nodecl_get_locus(nodecl_shaped_expr),
                    c_cxx_codegen_to_str(nodecl_shaped_expr));
        }
        free(list);
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    // Synthesize a new type based on what we got
    type_t* result_type = pointer_type_get_pointee_type(no_ref(shaped_expr_type));

    // Traverse the list backwards
    for (i = num_items - 1; i >= 0; i--)
    {
        nodecl_t current_expr = list[i];
        result_type = get_array_type(result_type, current_expr, decl_context);
    }
    free(list);

    *nodecl_output = nodecl_make_shaping(nodecl_shaped_expr, 
            nodecl_shape_list, 
            result_type,
            filename, line);
}

static void check_shaping_expression(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    const char* filename = ASTFileName(expression);
    int line = ASTLine(expression);

    AST shaped_expr = ASTSon1(expression);
    AST shape_list = ASTSon0(expression);

    nodecl_t nodecl_shaped_expr = nodecl_null();
    check_expression_impl_(shaped_expr, decl_context, &nodecl_shaped_expr);

    if (nodecl_is_err_expr(nodecl_shaped_expr))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    nodecl_t nodecl_shape_list = nodecl_null();
    if (!check_expression_list(shape_list, decl_context, &nodecl_shape_list))
    {
        *nodecl_output = nodecl_make_err_expr(filename, line);
        return;
    }

    check_nodecl_shaping_expression(nodecl_shaped_expr, 
            nodecl_shape_list,
            decl_context,
            filename, line,
            nodecl_output);
}


char check_expression_list(AST expression_list, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    *nodecl_output = nodecl_null();
    if (expression_list == NULL)
    {
        // An empty list is OK
        return 1;
    }

    if (ASTType(expression_list) == AST_AMBIGUITY)
    {
        int correct_choice = -1;
        int i;
        for (i = 0; i < ast_get_num_ambiguities(expression_list); i++)
        {
            AST current_expression_list = ast_get_ambiguity(expression_list, i);

            nodecl_t nodecl_expr = nodecl_null();
            enter_test_expression();
            check_expression_list(current_expression_list, decl_context, &nodecl_expr);
            leave_test_expression();

            if (nodecl_is_null(nodecl_expr)
                    || !nodecl_is_err_expr(nodecl_expr))
            {
                if (correct_choice < 0)
                {
                    correct_choice = i;
                    *nodecl_output = nodecl_expr;
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

        if (correct_choice < 0)
        {
            *nodecl_output = nodecl_make_err_expr(ASTFileName(expression_list), ASTLine(expression_list));
            return 0;
        }
        else
        {
            return 1;
        }
    }
    else
    {
        // Check the beginning of the list
        nodecl_t nodecl_prev_list = nodecl_null();
        check_expression_list(ASTSon0(expression_list), decl_context, &nodecl_prev_list);

        if (!nodecl_is_null(nodecl_prev_list)
                && nodecl_is_err_expr(nodecl_prev_list))
        {
            *nodecl_output = nodecl_prev_list;
            return 0;
        }

        nodecl_t nodecl_current = nodecl_null();
        check_expression_impl_(ASTSon1(expression_list), decl_context, &nodecl_current);

        if (nodecl_is_err_expr(nodecl_current))
        {
            *nodecl_output = nodecl_current;
            return 0;
        }

        *nodecl_output = nodecl_append_to_list(nodecl_prev_list, nodecl_current);

        return 1;
    }

    internal_error("Code unreachable", 0);
}


static char check_default_initialization_(scope_entry_t* entry,
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** constructor)
{
    if (constructor != NULL)
    {
        *constructor = NULL;
    }

    type_t* t = entry->type_information;
    if (entry->kind == SK_CLASS)
    {
        t = get_user_defined_type(entry);
    }

    if (is_lvalue_reference_type(t))
    {
        // References cannot be default initialized
        if (!checking_ambiguity())
        {
            error_printf("%s:%d: error: reference '%s' has not been initialized\n",
                    filename, line, get_qualified_symbol_name(entry, entry->decl_context));
        }
        return 0;
    }

    if (is_array_type(t))
    {
        t = array_type_get_element_type(t);
    }

    if (is_class_type(t))
    {
        int num_arguments = 0;
        type_t** arguments = NULL;

        scope_entry_list_t* candidates = NULL;
        scope_entry_t* chosen_constructor = solve_constructor(t,
                arguments, num_arguments,
                /* is_explicit */ 1,
                decl_context,
                filename, line,
                /* conversors */ NULL,
                &candidates);

        if (chosen_constructor == NULL)
        {
            if (entry_list_size(candidates) != 0)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: no default constructor for class type '%s' when initializing '%s'\n",
                            filename, line,
                            print_type_str(t, decl_context),
                            get_qualified_symbol_name(entry, entry->decl_context));
                }
            }
            entry_list_free(candidates);
            return 0;
        }
        else
        {
            entry_list_free(candidates);
            if (function_has_been_deleted(decl_context, chosen_constructor, filename, line))
            {
                return 0;
            }

            if (constructor != NULL)
            {
                *constructor = chosen_constructor;
            }
        }
    }
    return 1;
}

char check_copy_constructor(scope_entry_t* entry,
        decl_context_t decl_context,
        char has_const,
        const char* filename, int line,
        scope_entry_t** constructor)
{
    if (constructor != NULL)
    {
        *constructor = NULL;
    }

    type_t* t = entry->type_information;
    if (entry->kind == SK_CLASS)
    {
        t = get_user_defined_type(entry);
    }

    if (is_lvalue_reference_type(t))
    {
        return 1;
    }

    if (is_array_type(t))
    {
        t = array_type_get_element_type(t);
    }

    if (is_class_type(t))
    {
        int num_arguments = 1;

        type_t* parameter_type = t;
        if (has_const)
        {
            parameter_type = get_const_qualified_type(parameter_type);
        }
        parameter_type = get_lvalue_reference_type(parameter_type);

        type_t* arguments[1] = { parameter_type };

        scope_entry_t* conversors[1] = { NULL };

        scope_entry_list_t* candidates = NULL;
        scope_entry_t* chosen_constructor = solve_constructor(t,
                arguments, num_arguments,
                /* is_explicit */ 1,
                decl_context,
                filename, line,
                conversors,
                &candidates);

        if (chosen_constructor == NULL)
        {
            if (entry_list_size(candidates) != 0)
            {
                if (!checking_ambiguity())
                {
                    error_printf("%s:%d: error: no copy constructor for type '%s'\n",
                            filename, line,
                            print_type_str(t, decl_context));
                }
            }
            entry_list_free(candidates);
            return 0;
        }
        else
        {
            entry_list_free(candidates);
            if (function_has_been_deleted(decl_context, chosen_constructor, ASTFileName(NULL), ASTLine(NULL)))
            {
                return 0;
            }

            if (constructor != NULL)
            {
                *constructor = chosen_constructor;
            }
        }
    }
    return 1;
}

char check_copy_assignment_operator(scope_entry_t* entry,
        decl_context_t decl_context,
        char has_const,
        const char* filename, int line,
        scope_entry_t** constructor)
{
    if (constructor != NULL)
    {
        *constructor = NULL;
    }

    type_t* t = entry->type_information;
    if (entry->kind == SK_CLASS)
    {
        t = get_user_defined_type(entry);
    }

    if (is_lvalue_reference_type(t))
    {
        return 1;
    }

    if (is_array_type(t))
    {
        t = array_type_get_element_type(t);
    }

    if (is_class_type(t))
    {
        static AST operation_tree = NULL;
        if (operation_tree == NULL)
        {
            operation_tree = ASTMake1(AST_OPERATOR_FUNCTION_ID,
                    ASTLeaf(AST_ASSIGNMENT_OPERATOR, NULL, 0, NULL), NULL, 0, NULL);
        }

        type_t* argument_type = t;
        if (has_const)
        {
            argument_type = get_const_qualified_type(argument_type);
        }
        argument_type = get_lvalue_reference_type(argument_type);

        int num_arguments = 2;
        type_t* arguments[2] = { argument_type, argument_type };

        scope_entry_list_t* operator_overload_set = NULL;
        scope_entry_list_t* operator_entry_list = class_type_get_copy_assignment_operators(t);
        operator_overload_set = unfold_and_mix_candidate_functions(operator_entry_list,
                NULL, arguments + 1, num_arguments - 1,
                decl_context,
                filename, line,
                /* explicit template arguments */ NULL);
        entry_list_free(operator_entry_list);

        candidate_t* candidate_set = NULL;
        scope_entry_list_iterator_t *it = NULL;
        for (it = entry_list_iterator_begin(operator_overload_set);
                !entry_list_iterator_end(it);
                entry_list_iterator_next(it))
        {
            candidate_set = add_to_candidate_set(candidate_set,
                    entry_list_iterator_current(it),
                    num_arguments,
                    arguments);
        }
        entry_list_iterator_free(it);

        scope_entry_t* conversors[2] = { NULL, NULL };

        scope_entry_t *overloaded_call = solve_overload(candidate_set,
                decl_context,
                filename, line, conversors);

        if (overloaded_call == NULL)
        {
            if (!checking_ambiguity())
            {
                error_message_overload_failed(candidate_set, filename, line);
                entry_list_free(operator_overload_set);
                error_printf("%s:%d: error: no copy assignment operator for type '%s'\n",
                        filename, line,
                        print_type_str(t, decl_context));
            }
            return 0;
        }
        else
        {
            entry_list_free(operator_overload_set);
            if (function_has_been_deleted(decl_context, overloaded_call, ASTFileName(NULL), ASTLine(NULL)))
            {
                return 0;
            }

            if (constructor != NULL)
            {
                *constructor = overloaded_call;
            }
        }
    }
    return 1;
}

char check_default_initialization(scope_entry_t* entry, decl_context_t decl_context, 
        const char* filename, int line,
        scope_entry_t** constructor)
{
    return check_default_initialization_(entry, decl_context, filename, line, constructor);
}

char check_default_initialization_and_destruction_declarator(scope_entry_t* entry, decl_context_t decl_context,
        const char* filename,
        int line)
{
    scope_entry_t* constructor = NULL;
    check_default_initialization_(entry, decl_context, filename, line, &constructor);

    if (is_class_type(entry->type_information))
    {
        ensure_function_is_emitted(constructor, filename, line);

        scope_entry_t* destructor = class_type_get_destructor(entry->type_information);
        ERROR_CONDITION(destructor == NULL, "Invalid destructor", 0);
        ensure_function_is_emitted(destructor, filename, line);
    }

    return 1;
}

static void diagnostic_single_candidate(scope_entry_t* entry, 
        const char* filename UNUSED_PARAMETER, int line UNUSED_PARAMETER)
{
    entry = entry_advance_aliases(entry);
    info_printf("%s:%d: note:    %s",
            entry->file, entry->line,
            print_decl_type_str(entry->type_information, entry->decl_context, 
                get_qualified_symbol_name(entry, entry->decl_context)));

    if (entry->entity_specs.is_builtin)
    {
        info_printf(" [built-in]");
    }
    if (!entry->entity_specs.is_user_declared)
    {
        info_printf(" [implicit]");
    }
    info_printf("\n");
}

void diagnostic_candidates(scope_entry_list_t* candidates, const char* filename, int line)
{
    info_printf("%s:%d: info: candidates are:\n", filename, line);
    scope_entry_list_iterator_t* it;
    for (it = entry_list_iterator_begin(candidates);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* candidate_fun = entry_list_iterator_current(it);
        diagnostic_single_candidate(candidate_fun, filename, line);
    }
    entry_list_iterator_free(it);
}

static void error_message_overload_failed(candidate_t* candidates, const char* filename, int line)
{
    error_printf("%s:%d: error: overload call failed\n",
            filename, line);

    if (candidates != NULL)
    {
        info_printf("%s:%d: info: candidates are\n", filename, line);

        candidate_t* it = candidates;
        while (it != NULL)
        {
            scope_entry_t* entry = it->entry;

            diagnostic_single_candidate(entry, filename, line);

            it = it->next;
        }
    }
    else
    {
        info_printf("%s:%d: info: no candidate functions\n", filename, line);
    }
}

nodecl_t cxx_nodecl_make_conversion(nodecl_t expr, type_t* dest_type, const char* filename, int line)
{
    char is_value_dep = nodecl_expr_is_value_dependent(expr);
    char is_lvalue = nodecl_expr_is_lvalue(expr);
    const_value_t* val = nodecl_get_constant(expr);

    nodecl_t result = nodecl_make_conversion(expr, dest_type, filename, line);

    nodecl_set_constant(result, val);
    nodecl_expr_set_is_value_dependent(result, is_value_dep);
    nodecl_expr_set_is_lvalue(result, is_lvalue);

    return result;
}

nodecl_t cxx_nodecl_make_function_call(nodecl_t called, nodecl_t arg_list, type_t* t, const char* filename, int line)
{
    ERROR_CONDITION(!nodecl_is_null(arg_list) 
            && !nodecl_is_list(arg_list), "Argument nodecl is not a list", 0);

    scope_entry_t* called_symbol = nodecl_get_symbol(called);

    // This list will be the same as arg_list but with explicit conversions stored
    nodecl_t converted_arg_list = nodecl_null();

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(arg_list, &num_items);

    type_t* function_type = NULL;
    if (called_symbol != NULL)
    {
        function_type = called_symbol->type_information;
    }
    else
    {
        function_type = no_ref(nodecl_get_type(called));
    }
    if (is_pointer_to_function_type(function_type))
    {
        function_type = pointer_type_get_pointee_type(function_type);
    }
    ERROR_CONDITION(!is_function_type(function_type), "%s is not a function type!", 
            function_type == NULL ? "<<NULL>>" : print_declarator(function_type));

    int num_parameters = function_type_get_num_parameters(function_type);
    if (function_type_get_has_ellipsis(function_type))
        num_parameters--;

    // j is used to index the types of the function type
    // i is used to index the arguments of the call (possibly ignoring the
    // first one if it is known to be 'this')
    int j = 0;
    int i = 0;

    char ignore_this = 0;
    if (called_symbol != NULL
            && called_symbol->entity_specs.is_member
            && !called_symbol->entity_specs.is_static
            // Constructors and destructors are nonstatic but do not have
            // implicit argument
            && !called_symbol->entity_specs.is_constructor
            && !called_symbol->entity_specs.is_destructor)
    {
        // Ignore the first argument as we know it is 'this'
        i = 1;
        converted_arg_list = nodecl_append_to_list(converted_arg_list, list[0]);
        ignore_this = 1;
    }

    for (; i < num_items; i++, j++)
    {
        if (j < num_parameters)
        {
            type_t* param_type = function_type_get_parameter_type_num(function_type, j);
            type_t* arg_type = nodecl_get_type(list[i]);

            if (!equivalent_types(
                        get_unqualified_type(no_ref(param_type)),
                        get_unqualified_type(no_ref(arg_type))))
            {
                list[i] = cxx_nodecl_make_conversion(list[i], 
                        param_type, 
                        nodecl_get_filename(list[i]),
                        nodecl_get_line(list[i]));
            }
        }

        converted_arg_list = nodecl_append_to_list(converted_arg_list, list[i]);
    }

    free(list);

    if (called_symbol != NULL
            && called_symbol->kind == SK_FUNCTION)
    {
        ensure_function_is_emitted(called_symbol, nodecl_get_filename(called), nodecl_get_line(called));

        // Default arguments
        int arg_i = nodecl_list_length(converted_arg_list);
        if (ignore_this)
        {
            // Do not count the implicit member
            arg_i--;
        }
        ERROR_CONDITION(arg_i < 0, "Invalid argument count %d\n", arg_i);

        for(; arg_i < num_parameters; arg_i++)
        {
            ERROR_CONDITION(called_symbol->entity_specs.default_argument_info == NULL
                    || called_symbol->entity_specs.default_argument_info[arg_i] == NULL,
                    "Invalid default argument information %d", arg_i);

            converted_arg_list = nodecl_append_to_list(converted_arg_list,
                    called_symbol->entity_specs.default_argument_info[arg_i]->argument);
        }

        if (called_symbol->entity_specs.is_member 
                && called_symbol->entity_specs.is_virtual)
        {
            return nodecl_make_virtual_function_call(called, converted_arg_list, t, filename, line);
        }
        else
        {
            return nodecl_make_function_call(called, converted_arg_list, t, filename, line);
        }
    }
    else
    {
        return nodecl_make_function_call(called, converted_arg_list, t, filename, line);
    }
}

char check_nodecl_nontype_template_argument_expression(nodecl_t nodecl_expr,
        decl_context_t decl_context UNUSED_PARAMETER, 
        nodecl_t* nodecl_output)
{
    if (nodecl_expr_is_type_dependent(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        return 1;
    }

    type_t* expr_type = nodecl_get_type(nodecl_expr);

    scope_entry_t* related_symbol = NULL;

    char valid = 0;
    if (nodecl_get_kind(nodecl_expr) == NODECL_SYMBOL
            && nodecl_get_symbol(nodecl_expr)->kind == SK_TEMPLATE_PARAMETER)
    {
        valid = 1;
    }
    else if (is_integral_type(no_ref(expr_type))
            || is_enum_type(no_ref(expr_type)))
    {
        valid = 1;
    }
    else if (is_pointer_type(no_ref(expr_type))
            || is_function_type(no_ref(expr_type)))
    {
        // &a
        // a
        nodecl_t current_expr = nodecl_expr;
        char lacks_ref = 1;
        if (nodecl_get_kind(current_expr) == NODECL_REFERENCE)
        {
            current_expr = nodecl_get_child(current_expr, 0);
            lacks_ref = 0;
        }
        related_symbol = nodecl_get_symbol(current_expr);
        if (related_symbol != NULL
                && ((related_symbol->kind == SK_VARIABLE 
                        && (!related_symbol->entity_specs.is_member 
                            || related_symbol->entity_specs.is_static))
                    || (related_symbol->kind == SK_FUNCTION)
                    || (related_symbol->kind == SK_TEMPLATE 
                        && is_function_type(template_type_get_primary_type(related_symbol->type_information)))))
        {
            if (!lacks_ref)
            {
                valid = 1;
            }
            else if ((related_symbol->kind == SK_VARIABLE 
                        && (is_array_type(related_symbol->type_information) 
                            || is_pointer_to_function_type(related_symbol->type_information)))
                    || (related_symbol->kind == SK_FUNCTION)
                    || (related_symbol->kind == SK_TEMPLATE 
                        && is_function_type(template_type_get_primary_type(related_symbol->type_information))))
            {
                valid = 1;
            }
        }
    }
    else if (is_unresolved_overloaded_type(expr_type))
    {
        valid = 1;
    }
    else if (is_pointer_to_member_type(no_ref(expr_type)))
    {
        // &C::id
        nodecl_t current_expr = nodecl_expr;
        related_symbol = nodecl_get_symbol(current_expr);
        if (related_symbol != NULL)
        {
            valid = 1;
        }
    }

    if (!valid)
    {
        if (!checking_ambiguity())
        {
            error_printf("%s: error: invalid template argument '%s' for a nontype template parameter\n",
                    nodecl_get_locus(nodecl_expr),
                    c_cxx_codegen_to_str(nodecl_expr));
        }

        *nodecl_output = nodecl_make_err_expr(nodecl_get_filename(nodecl_expr), nodecl_get_line(nodecl_expr));
        return 0;
    }

    *nodecl_output = nodecl_expr;

    if (related_symbol != NULL
            && !related_symbol->entity_specs.is_template_parameter)
    {
        nodecl_set_symbol(*nodecl_output, related_symbol);
    }

    return 1;
}

char check_nontype_template_argument_expression(AST expression, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_expr = nodecl_null();
    check_expression_impl_(expression, decl_context, &nodecl_expr);

    if (nodecl_is_err_expr(nodecl_expr))
    {
        *nodecl_output = nodecl_expr;
        return 0;
    }

    return check_nodecl_nontype_template_argument_expression(nodecl_expr,
            decl_context,
            nodecl_output);
}

// Instantiation of expressions
#include "cxx-nodecl-visitor.h"

typedef
struct nodecl_instantiate_expr_visitor_tag
{
    nodecl_external_visitor_t _base_visitor;

    // Context info
    decl_context_t decl_context;

    // Keep the resulting expression here
    nodecl_t nodecl_result;
} nodecl_instantiate_expr_visitor_t;

typedef void (*nodecl_instantiate_expr_visitor_fun_t)(nodecl_instantiate_expr_visitor_t* visitor, nodecl_t node);
typedef void (*nodecl_visitor_fun_t)(nodecl_external_visitor_t* visitor, nodecl_t node);

static inline nodecl_visitor_fun_t instantiate_expr_visitor_fun(nodecl_instantiate_expr_visitor_fun_t p)
{
    return NODECL_VISITOR_FUN(p);
}

static nodecl_t instantiate_expr_walk(nodecl_instantiate_expr_visitor_t* visitor, nodecl_t node)
{
    visitor->nodecl_result = nodecl_null();
    NODECL_WALK(visitor, node);
    return visitor->nodecl_result;
}

static void instantiate_expr_init_visitor(nodecl_instantiate_expr_visitor_t*, decl_context_t);

nodecl_t instantiate_expression(nodecl_t nodecl_expr, decl_context_t decl_context)
{
    nodecl_instantiate_expr_visitor_t v;
    memset(&v, 0, sizeof(v));

    instantiate_expr_init_visitor(&v, decl_context);

    return instantiate_expr_walk(&v, nodecl_expr);
}

static void instantiate_expr_not_implemented_yet(nodecl_instantiate_expr_visitor_t* v UNUSED_PARAMETER,
        nodecl_t nodecl_expr)
{
    internal_error("Expression '%s' not yet implemented\n", ast_print_node_type(nodecl_get_kind(nodecl_expr)));
}

static void instantiate_expr_literal(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t result = nodecl_generic_make(nodecl_get_kind(node), nodecl_get_filename(node), nodecl_get_line(node));

    nodecl_set_type(result, nodecl_get_type(node));
    nodecl_set_constant(result, nodecl_get_constant(node));
    nodecl_set_text(result, nodecl_get_text(node));

    v->nodecl_result = result;
}

static void instantiate_symbol(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    scope_entry_t* sym = nodecl_get_symbol(node);

    nodecl_t result = nodecl_null();

    if (sym->kind == SK_TEMPLATE_PARAMETER)
    {
        scope_entry_t* argument = lookup_of_template_parameter(
                v->decl_context,
                sym->entity_specs.template_parameter_nesting,
                sym->entity_specs.template_parameter_position);

        if (argument->kind == SK_VARIABLE)
        {
            result = argument->value;
        }
        else
        {
            result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
        }
    }
    else if (sym->kind == SK_DEPENDENT_ENTITY)
    {
        scope_entry_list_t *entry_list = query_dependent_entity_in_context(v->decl_context, sym, nodecl_get_filename(node), nodecl_get_line(node));

        scope_entry_t* dependent_entry = NULL;
        nodecl_t dependent_parts = nodecl_null();
        dependent_typename_get_components(sym->type_information, &dependent_entry, &dependent_parts);

        cxx_compute_name_from_entry_list(dependent_parts, entry_list, v->decl_context, &result);
    }
    else
    {
        result = nodecl_make_symbol(nodecl_get_symbol(node), nodecl_get_filename(node), nodecl_get_line(node));
        nodecl_set_type(result, nodecl_get_type(node));
        nodecl_expr_set_is_value_dependent(result, nodecl_expr_is_value_dependent(node));
        nodecl_expr_set_is_lvalue(result, nodecl_expr_is_lvalue(node));
    }

    v->nodecl_result = result;
}

static void instantiate_binary_op(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_lhs = instantiate_expr_walk(v, nodecl_get_child(node, 0));
    nodecl_t nodecl_rhs = instantiate_expr_walk(v, nodecl_get_child(node, 1));

    nodecl_t result = nodecl_null();

    if (nodecl_is_err_expr(nodecl_lhs)
            || nodecl_is_err_expr(nodecl_rhs))
    {
        result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
    }
    else
    {
        check_binary_expression_(nodecl_get_kind(node), 
                &nodecl_lhs,
                &nodecl_rhs,
                v->decl_context,
                nodecl_get_filename(node), nodecl_get_line(node),
                &result);
    }

    v->nodecl_result = result;
}

static void instantiate_unary_op(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_op = instantiate_expr_walk(v, nodecl_get_child(node, 0));

    nodecl_t result = nodecl_null();

    if (nodecl_is_err_expr(nodecl_op))
    {
        result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
    }
    else
    {
        check_unary_expression_(nodecl_get_kind(node),
                &nodecl_op,
                v->decl_context,
                nodecl_get_filename(nodecl_op), nodecl_get_line(nodecl_op),
                &result);
    }

    v->nodecl_result = result;
}

static void instantiate_structured_value(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    type_t* t = nodecl_get_type(node);

    t = update_type_for_instantiation(t, 
            v->decl_context,
            nodecl_get_filename(node),
            nodecl_get_line(node));

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(node, 0), &num_items);

    nodecl_t nodecl_new_list = nodecl_null();

    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t nodecl_new_item = nodecl_new_item = instantiate_expr_walk(v, list[i]);

        if (nodecl_is_err_expr(nodecl_new_item))
        {
            v->nodecl_result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
            return;
        }

        nodecl_new_list = nodecl_append_to_list(nodecl_new_list, nodecl_new_item);
    }

    // We use braced because it is the most generic 
    nodecl_t nodecl_braced_init = 
        nodecl_make_cxx_braced_initializer(
                nodecl_new_list,
                nodecl_get_filename(node),
                nodecl_get_line(node));

    check_nodecl_braced_initializer(
            nodecl_braced_init,
            v->decl_context,
            t,
            &v->nodecl_result);
}

static void instantiate_reference(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_op = instantiate_expr_walk(v, nodecl_get_child(node, 0));

    if (nodecl_is_err_expr(nodecl_op))
    {
        v->nodecl_result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
    }
    else if (nodecl_get_kind(nodecl_op) == NODECL_SYMBOL)
    {
        scope_entry_t* sym = nodecl_get_symbol(nodecl_op);

        if (sym->entity_specs.is_member
                && !sym->entity_specs.is_static
                && (sym->kind == SK_VARIABLE
                    || sym->kind == SK_FUNCTION))
        {
            if (sym->kind == SK_VARIABLE)
            {
                v->nodecl_result = nodecl_make_pointer_to_member(sym, 
                        get_lvalue_reference_type(
                            get_pointer_to_member_type(sym->type_information,
                                named_type_get_symbol(sym->entity_specs.class_type))),
                        nodecl_get_filename(node), nodecl_get_line(node));
            }
            else // SK_FUNCTION
            {
                v->nodecl_result = nodecl_op;
            }
            return;
        }
    }

    check_unary_expression_(nodecl_get_kind(node),
            &nodecl_op,
            v->decl_context,
            nodecl_get_filename(nodecl_op), nodecl_get_line(nodecl_op),
            &v->nodecl_result);
}

static void instantiate_function_call(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_called = nodecl_null();
    nodecl_t orig_called = nodecl_get_child(node, 0);

    if (nodecl_get_kind(orig_called) == NODECL_CXX_DEP_NAME_SIMPLE)
    {
        nodecl_called = nodecl_copy(orig_called);
    }
    else
    {
        nodecl_called = instantiate_expr_walk(v, nodecl_get_child(node, 0));
    }

    if (nodecl_is_err_expr(nodecl_called))
    {
        v->nodecl_result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
        return;
    }

    nodecl_t nodecl_argument_list = nodecl_get_child(node, 1);

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_argument_list, &num_items);

    nodecl_t new_list = nodecl_null();
    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t current_arg = 
                instantiate_expr_walk(v, list[i]);

        if (nodecl_is_err_expr(current_arg))
        {
            v->nodecl_result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
            return;
        }

        new_list = nodecl_append_to_list(
                new_list,
                current_arg);
    }

    check_nodecl_function_call(nodecl_called, 
            new_list,
            v->decl_context,
            &v->nodecl_result);
}

static void instantiate_dep_sizeof_expr(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t dep_expr = nodecl_get_child(node, 0);

    nodecl_t expr = instantiate_expr_walk(v, dep_expr);

    nodecl_t result = nodecl_null();

    if (nodecl_is_err_expr(expr))
    {
        result = nodecl_make_err_expr(nodecl_get_filename(node), nodecl_get_line(node));
    }
    else
    {
        type_t* t = nodecl_get_type(expr);

        check_sizeof_type(t, v->decl_context, 
                nodecl_get_filename(node), 
                nodecl_get_line(node), 
                &result);
    }

    v->nodecl_result = result;
}

static void instantiate_nondep_sizeof(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_type = nodecl_get_child(node, 0);

    type_t* t = nodecl_get_type(nodecl_type);

    t = update_type_for_instantiation(t, 
            v->decl_context,
            nodecl_get_filename(node),
            nodecl_get_line(node));

    nodecl_t result = nodecl_null();

    check_sizeof_type(t, v->decl_context, 
            nodecl_get_filename(node),
            nodecl_get_line(node),
            &result);

    v->nodecl_result = result;
}

static void instantiate_explicit_type_cast(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    type_t * t = nodecl_get_type(node);
    t = update_type_for_instantiation(t, v->decl_context,
            nodecl_get_filename(node),
            nodecl_get_line(node));

    nodecl_t nodecl_new_list = nodecl_null();

    nodecl_t parenthesized_init = nodecl_get_child(node, 0);
    if (!nodecl_is_null(parenthesized_init))
    {
        nodecl_t old_list = nodecl_get_child(parenthesized_init, 0);
        int num_items = 0;
        nodecl_t* list = nodecl_unpack_list(old_list, &num_items);

        int i;
        for (i = 0; i < num_items; i++)
        {
            nodecl_t n = instantiate_expr_walk(v, list[i]);

            if (nodecl_is_err_expr(n))
            {
                v->nodecl_result = n;
                return;
            }

            nodecl_new_list = nodecl_append_to_list(nodecl_new_list, n);
        }

        free(list);
    }

    nodecl_t new_parenthesized_init = nodecl_make_cxx_parenthesized_initializer(nodecl_new_list, 
            nodecl_get_filename(node),
            nodecl_get_line(node));

    check_nodecl_explicit_type_conversion(t, 
            new_parenthesized_init, 
            v->decl_context, 
            &v->nodecl_result,
            nodecl_get_filename(node),
            nodecl_get_line(node));
}

static void instantiate_parenthesized_initializer(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_result_list = nodecl_null();

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(node, 0), &num_items);

    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t expr = instantiate_expr_walk(v, list[i]);

        if (nodecl_is_err_expr(expr))
        {
            v->nodecl_result = expr;
            return;
        }

        nodecl_result_list = nodecl_append_to_list(nodecl_result_list, expr);
    }

    free(list);

    v->nodecl_result = nodecl_make_cxx_parenthesized_initializer(nodecl_result_list, nodecl_get_filename(node), nodecl_get_line(node));
}

static void instantiate_equal_initializer(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t expr = instantiate_expr_walk(v, nodecl_get_child(node, 0));

    if (nodecl_is_err_expr(expr))
    {
        v->nodecl_result = expr;
    }
    else
    {
        v->nodecl_result = nodecl_make_cxx_equal_initializer(expr, nodecl_get_filename(node), nodecl_get_line(node));
    }
}

static void instantiate_braced_initializer(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_result_list = nodecl_null();

    int num_items = 0;
    nodecl_t* list = nodecl_unpack_list(nodecl_get_child(node, 0), &num_items);

    int i;
    for (i = 0; i < num_items; i++)
    {
        nodecl_t expr = instantiate_expr_walk(v, list[i]);

        if (nodecl_is_err_expr(expr))
        {
            v->nodecl_result = expr;
            return;
        }

        nodecl_result_list = nodecl_append_to_list(nodecl_result_list, expr);
    }

    free(list);

    v->nodecl_result = nodecl_make_cxx_braced_initializer(nodecl_result_list, nodecl_get_filename(node), nodecl_get_line(node));
}

static void instantiate_conversion(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_expr = instantiate_expr_walk(v, nodecl_get_child(node, 0));

    v->nodecl_result = cxx_nodecl_make_conversion(nodecl_expr, 
            nodecl_get_type(node), 
            nodecl_get_filename(node), nodecl_get_line(node));
}

static void instantiate_cast(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_casted_expr = instantiate_expr_walk(v, nodecl_get_child(node, 0));

    type_t* declarator_type = update_type_for_instantiation(nodecl_get_type(node),
            v->decl_context,
            nodecl_get_filename(node),
            nodecl_get_line(node));

    const char* cast_kind = nodecl_get_text(node);

    nodecl_t result = nodecl_null();

    check_nodecl_cast_expr(nodecl_casted_expr, 
            v->decl_context, 
            declarator_type, cast_kind,
            nodecl_get_filename(node),
            nodecl_get_line(node),
            &result);

    v->nodecl_result = result;
}

static void instantiate_conditional_expression(nodecl_instantiate_expr_visitor_t* v, nodecl_t node)
{
    nodecl_t nodecl_cond = instantiate_expr_walk(v, nodecl_get_child(node, 0));
    if (nodecl_is_err_expr(nodecl_cond))
    {
        v->nodecl_result = nodecl_cond;
        return;
    }

    nodecl_t nodecl_true = instantiate_expr_walk(v, nodecl_get_child(node, 1));
    if (nodecl_is_err_expr(nodecl_true))
    {
        v->nodecl_result = nodecl_true;
        return;
    }

    nodecl_t nodecl_false = instantiate_expr_walk(v, nodecl_get_child(node, 1));
    if (nodecl_is_err_expr(nodecl_false))
    {
        v->nodecl_result = nodecl_true;
        return;
    }

    check_conditional_expression_impl_nodecl(nodecl_cond, nodecl_true, nodecl_false, v->decl_context, &v->nodecl_result);
}

// Initialization
static void instantiate_expr_init_visitor(nodecl_instantiate_expr_visitor_t* v, decl_context_t decl_context)
{
    nodecl_init_walker((nodecl_external_visitor_t*)v, instantiate_expr_visitor_fun(instantiate_expr_not_implemented_yet));

    v->decl_context = decl_context;

    // Literals
    NODECL_VISITOR(v)->visit_integer_literal = instantiate_expr_visitor_fun(instantiate_expr_literal);
    NODECL_VISITOR(v)->visit_floating_literal = instantiate_expr_visitor_fun(instantiate_expr_literal);
    NODECL_VISITOR(v)->visit_string_literal = instantiate_expr_visitor_fun(instantiate_expr_literal);
    NODECL_VISITOR(v)->visit_boolean_literal = instantiate_expr_visitor_fun(instantiate_expr_literal);

    // Symbol
    NODECL_VISITOR(v)->visit_symbol = instantiate_expr_visitor_fun(instantiate_symbol);

    // Binary operations
    NODECL_VISITOR(v)->visit_add = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_mul = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_div = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_mod = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_minus = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_shl = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_shr = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_lower_than = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_greater_than = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_greater_or_equal_than = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_lower_or_equal_than = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_equal = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_different = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_and = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_xor = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_or = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_logical_and = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_logical_or = instantiate_expr_visitor_fun(instantiate_binary_op);
#ifdef FORTRAN_SUPPORT
    NODECL_VISITOR(v)->visit_power = instantiate_expr_visitor_fun(instantiate_binary_op);
#endif
    NODECL_VISITOR(v)->visit_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_mul_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_div_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_add_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_sub_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_shl_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_shr_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_and_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_or_assignment  = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_bitwise_xor_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    NODECL_VISITOR(v)->visit_mod_assignment = instantiate_expr_visitor_fun(instantiate_binary_op);
    
    // Unary
    NODECL_VISITOR(v)->visit_derreference = instantiate_expr_visitor_fun(instantiate_unary_op);
    NODECL_VISITOR(v)->visit_reference = instantiate_expr_visitor_fun(instantiate_reference);
    NODECL_VISITOR(v)->visit_plus = instantiate_expr_visitor_fun(instantiate_unary_op);
    NODECL_VISITOR(v)->visit_neg = instantiate_expr_visitor_fun(instantiate_unary_op);
    NODECL_VISITOR(v)->visit_logical_not = instantiate_expr_visitor_fun(instantiate_unary_op);
    NODECL_VISITOR(v)->visit_bitwise_not = instantiate_expr_visitor_fun(instantiate_unary_op);

    NODECL_VISITOR(v)->visit_structured_value = instantiate_expr_visitor_fun(instantiate_structured_value);

    // Function call
    NODECL_VISITOR(v)->visit_function_call = instantiate_expr_visitor_fun(instantiate_function_call);

    // Sizeof
    NODECL_VISITOR(v)->visit_sizeof = instantiate_expr_visitor_fun(instantiate_nondep_sizeof);
    NODECL_VISITOR(v)->visit_cxx_sizeof = instantiate_expr_visitor_fun(instantiate_dep_sizeof_expr);

    // Casts
    NODECL_VISITOR(v)->visit_cast = instantiate_expr_visitor_fun(instantiate_cast);

    // Conversion
    NODECL_VISITOR(v)->visit_conversion = instantiate_expr_visitor_fun(instantiate_conversion);

    // Initializers
    NODECL_VISITOR(v)->visit_cxx_equal_initializer = instantiate_expr_visitor_fun(instantiate_equal_initializer);
    NODECL_VISITOR(v)->visit_cxx_braced_initializer = instantiate_expr_visitor_fun(instantiate_braced_initializer);
    NODECL_VISITOR(v)->visit_cxx_parenthesized_initializer = instantiate_expr_visitor_fun(instantiate_parenthesized_initializer);
    NODECL_VISITOR(v)->visit_cxx_explicit_type_cast = instantiate_expr_visitor_fun(instantiate_explicit_type_cast);

    // Conditional
    NODECL_VISITOR(v)->visit_conditional_expression = instantiate_expr_visitor_fun(instantiate_conditional_expression);
}

