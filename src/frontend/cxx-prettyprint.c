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




#ifndef CXX_PRETTYPRINT_C
  #define CXX_PRETTYPRINT_C
#endif

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif
#ifdef HAVE_OPEN_MEMSTREAM
  // Needed, otherwise open_memstream is not declared
  #define _GNU_SOURCE
#endif
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-prettyprint.h"
#include "cxx-prettyprint-internal.h"
#include "cxx-diagnostic.h"
#include "cxx-codegen.h"

#include "fortran/fortran03-prettyprint.h"

HANDLER_PROTOTYPE(ambiguity_handler);
HANDLER_PROTOTYPE(sequence_handler);
HANDLER_PROTOTYPE(spaced_sequence_handler);
HANDLER_PROTOTYPE(list_handler);
HANDLER_PROTOTYPE(simple_declaration_handler);
HANDLER_PROTOTYPE(simple_parameter_handler);
HANDLER_PROTOTYPE(indented_simple_parameter_handler);
HANDLER_PROTOTYPE(unary_container_handler);
HANDLER_PROTOTYPE(parenthesized_son_handler);
HANDLER_PROTOTYPE(list_parenthesized_son_handler);
HANDLER_PROTOTYPE(simple_text_handler);
HANDLER_PROTOTYPE(template_id_handler);
HANDLER_PROTOTYPE(abstract_array_declarator_handler);
HANDLER_PROTOTYPE(abstract_declarator_function_handler);
HANDLER_PROTOTYPE(null_handler);
HANDLER_PROTOTYPE(parameter_decl_handler);
HANDLER_PROTOTYPE(parameters_and_qualifiers_handler);
HANDLER_PROTOTYPE(parameters_and_qualifiers_extra_handler);
HANDLER_PROTOTYPE(lambda_declarator_handler);
HANDLER_PROTOTYPE(lambda_capture_handler);
HANDLER_PROTOTYPE(init_declarator_handler);
HANDLER_PROTOTYPE(pointer_decl_handler);
HANDLER_PROTOTYPE(type_id_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_son_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_text);
HANDLER_PROTOTYPE(prefix_with_token_text_then_son_handler);
HANDLER_PROTOTYPE(braced_initializer_handler);
HANDLER_PROTOTYPE(pointer_spec_handler);
HANDLER_PROTOTYPE(type_specifier_sequence);
HANDLER_PROTOTYPE(binary_operator_handler);
HANDLER_PROTOTYPE(throw_expression_handler);
HANDLER_PROTOTYPE(conditional_expression_handler);
HANDLER_PROTOTYPE(cast_expression_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_parenthesized_son_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_optional_parenthesized_son_handler);
HANDLER_PROTOTYPE(new_expression_handler);
HANDLER_PROTOTYPE(new_type_id_handler);
HANDLER_PROTOTYPE(new_type_id_expr_handler);
HANDLER_PROTOTYPE(delete_expression_handler);
HANDLER_PROTOTYPE(array_subscript_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(explicit_type_conversion);
HANDLER_PROTOTYPE(typename_explicit_type_conversion);
HANDLER_PROTOTYPE(infix_parameter_handler);
HANDLER_PROTOTYPE(template_member_access);
HANDLER_PROTOTYPE(son_handler_then_suffix_parameter);
HANDLER_PROTOTYPE(templated_cast_handler);
HANDLER_PROTOTYPE(qualified_id_handler);
HANDLER_PROTOTYPE(member_declaration_qualif_handler);
HANDLER_PROTOTYPE(conversion_type_id_handler);
HANDLER_PROTOTYPE(constructor_initializer_handler);
HANDLER_PROTOTYPE(mem_initializer_handler);
HANDLER_PROTOTYPE(mem_initializer_id_handler);
HANDLER_PROTOTYPE(class_specifier_handler);
HANDLER_PROTOTYPE(class_head_handler);
HANDLER_PROTOTYPE(member_access_specifier_handler);
HANDLER_PROTOTYPE(using_declaration_handler);
HANDLER_PROTOTYPE(template_declaration_handler);
HANDLER_PROTOTYPE(type_parameter_class_or_typename_handler);
HANDLER_PROTOTYPE(type_parameter_template_handler);
HANDLER_PROTOTYPE(function_definition_handler);
HANDLER_PROTOTYPE(function_header_handler);
HANDLER_PROTOTYPE(compound_statement_handler);
HANDLER_PROTOTYPE(labeled_statement_handler);
HANDLER_PROTOTYPE(default_statement_handler);
HANDLER_PROTOTYPE(case_statement_handler);
HANDLER_PROTOTYPE(expression_statement_handler);
HANDLER_PROTOTYPE(condition_handler);
HANDLER_PROTOTYPE(while_statement_handler);
HANDLER_PROTOTYPE(do_while_statement_handler);
HANDLER_PROTOTYPE(for_statement_handler);
HANDLER_PROTOTYPE(return_statement_handler);
HANDLER_PROTOTYPE(goto_statement_handler);
HANDLER_PROTOTYPE(try_block_handler);
HANDLER_PROTOTYPE(exception_declaration_handler);
HANDLER_PROTOTYPE(member_declaration_handler);
HANDLER_PROTOTYPE(selection_statement_handler);
HANDLER_PROTOTYPE(catch_handler_handler);
HANDLER_PROTOTYPE(member_declarator_handler);
HANDLER_PROTOTYPE(base_clause_handler);
HANDLER_PROTOTYPE(base_specifier_handler);
HANDLER_PROTOTYPE(elaborated_type_class_handler);
HANDLER_PROTOTYPE(elaborated_type_enum_handler);
HANDLER_PROTOTYPE(elaborated_typename_handler);
HANDLER_PROTOTYPE(if_else_statement_handler);
HANDLER_PROTOTYPE(exception_specification_handler);
HANDLER_PROTOTYPE(operator_function_id_handler);
HANDLER_PROTOTYPE(asm_definition_handler);
HANDLER_PROTOTYPE(bitfield_declarator_handler);
HANDLER_PROTOTYPE(enum_specifier_handler);
HANDLER_PROTOTYPE(enumerator_def_handler);
HANDLER_PROTOTYPE(explicit_instantiation_handler);
HANDLER_PROTOTYPE(explicit_specialization_handler);
HANDLER_PROTOTYPE(linkage_specification_handler);
HANDLER_PROTOTYPE(linkage_specification_decl_handler);
HANDLER_PROTOTYPE(namespace_alias_definition_handler);
HANDLER_PROTOTYPE(using_directive_handler);
HANDLER_PROTOTYPE(namespace_definition_handler);
HANDLER_PROTOTYPE(parenthesized_initializer_handler);
HANDLER_PROTOTYPE(unknown_pragma_handler);
HANDLER_PROTOTYPE(kr_parameter_list_handler);
HANDLER_PROTOTYPE(designated_initializer_handler);
HANDLER_PROTOTYPE(designation_handler);
HANDLER_PROTOTYPE(index_designator_handler);
HANDLER_PROTOTYPE(field_designator_handler);

HANDLER_PROTOTYPE(trailing_return_handler);

HANDLER_PROTOTYPE(lambda_expression_handler);
HANDLER_PROTOTYPE(decltype_handler);
HANDLER_PROTOTYPE(static_assert_handler);

HANDLER_PROTOTYPE(pp_comment_handler);
HANDLER_PROTOTYPE(pp_prepro_token_handler);

HANDLER_PROTOTYPE(verbatim_construct_handler);

HANDLER_PROTOTYPE(double_colon_handler);

HANDLER_PROTOTYPE(defaulted_or_deleted_function_def);

// Pragma custom support
HANDLER_PROTOTYPE(pragma_custom_directive_handler);
HANDLER_PROTOTYPE(pragma_custom_construct_handler);
HANDLER_PROTOTYPE(pragma_custom_clause_handler);
HANDLER_PROTOTYPE(pragma_custom_line_handler);

// GCC Extensions
HANDLER_PROTOTYPE(gcc_label_declaration_handler);
HANDLER_PROTOTYPE(gcc_attribute_handler);
HANDLER_PROTOTYPE(gcc_attribute_value_handler);
HANDLER_PROTOTYPE(gcc_asm_definition_handler);
HANDLER_PROTOTYPE(gcc_asm_def_parameters);
HANDLER_PROTOTYPE(gcc_asm_operand_handler);
HANDLER_PROTOTYPE(gcc_typeof_handler);
HANDLER_PROTOTYPE(gcc_typeof_expr_handler);
HANDLER_PROTOTYPE(gcc_alignof_type_handler);
HANDLER_PROTOTYPE(gcc_asm_specification_handler);
HANDLER_PROTOTYPE(gcc_reference_spec_handler);
HANDLER_PROTOTYPE(gcc_initializer_clause_handler);
HANDLER_PROTOTYPE(gcc_case_statement_handler);
HANDLER_PROTOTYPE(gcc_goto_statement_handler);
HANDLER_PROTOTYPE(gcc_mem_initializer_handler);
HANDLER_PROTOTYPE(gcc_builtin_va_arg_handler);
HANDLER_PROTOTYPE(gcc_builtin_offsetof_handler);
HANDLER_PROTOTYPE(gcc_builtin_choose_expr_handler);
HANDLER_PROTOTYPE(gcc_builtin_types_compatible_p_handler);
HANDLER_PROTOTYPE(gcc_postfix_expression);
HANDLER_PROTOTYPE(gcc_conditional_expression);
HANDLER_PROTOTYPE(gcc_extension_prefix_handler);
HANDLER_PROTOTYPE(gcc_offsetof_member_designator_handler);
HANDLER_PROTOTYPE(gcc_parenthesized_expression_handler);

// g++ 4.3
HANDLER_PROTOTYPE(gxx_type_traits);

// Mercurium extensions
HANDLER_PROTOTYPE(array_section_handler);
HANDLER_PROTOTYPE(shaping_expression_handler);
HANDLER_PROTOTYPE(nodecl_literal_handler);

// UPC 1.2
HANDLER_PROTOTYPE(upc_forall_header);
HANDLER_PROTOTYPE(upc_iteration_statement);
HANDLER_PROTOTYPE(upc_layout_qualifier);
HANDLER_PROTOTYPE(upc_shared_qualifier);
HANDLER_PROTOTYPE(upc_sizeof_expr);
HANDLER_PROTOTYPE(upc_sizeof_type);
HANDLER_PROTOTYPE(upc_synch_statement);

// CUDA
HANDLER_PROTOTYPE(cuda_kernel_call_handler);
HANDLER_PROTOTYPE(cuda_kernel_arguments_handler);

// Intel
HANDLER_PROTOTYPE(intel_assume_aligned);

static prettyprint_entry_t handlers_list[] =
{
    NODE_HANDLER(AST_TRANSLATION_UNIT, unary_container_handler, NULL),
    NODE_HANDLER(AST_AMBIGUITY, ambiguity_handler, NULL),
    NODE_HANDLER(AST_NODE_LIST, sequence_handler, NULL),
    NODE_HANDLER(AST_SIMPLE_DECLARATION, simple_declaration_handler, NULL),
    NODE_HANDLER(AST_EMPTY_DECL, indented_simple_parameter_handler, ";\n"),
    NODE_HANDLER(AST_INIT_DECLARATOR, init_declarator_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR, unary_container_handler, NULL),
    NODE_HANDLER(AST_POINTER_DECLARATOR, pointer_decl_handler, NULL),
    NODE_HANDLER(AST_PARENTHESIZED_DECLARATOR, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_ID_EXPR, unary_container_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_ID_PACK, prefix_with_parameter_then_son_handler, "... "),
    NODE_HANDLER(AST_GLOBAL_SCOPE, double_colon_handler, NULL),
    NODE_HANDLER(AST_SYMBOL, simple_text_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_ID, template_id_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_EXPRESSION_ARGUMENT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_TYPE_ARGUMENT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_EXPRESSION_ARGUMENT_PACK, son_handler_then_suffix_parameter, " ..."),
    NODE_HANDLER(AST_TEMPLATE_TYPE_ARGUMENT_PACK, son_handler_then_suffix_parameter, " ..."),
    NODE_HANDLER(AST_INITIALIZER_CLAUSE_PACK_EXPANSION, son_handler_then_suffix_parameter, " ..."),
    NODE_HANDLER(AST_TYPE_ID, type_id_handler, NULL),
    NODE_HANDLER(AST_VARIADIC_ARG, simple_parameter_handler, "..."),
    NODE_HANDLER(AST_POINTER_LITERAL, simple_parameter_handler, "nullptr"),
    NODE_HANDLER(AST_CHAR16_T, simple_parameter_handler, "char16_t"),
    NODE_HANDLER(AST_CHAR32_T, simple_parameter_handler, "char32_t"),
    NODE_HANDLER(AST_EMPTY_PARAMETER_DECLARATION_CLAUSE, null_handler, NULL),
    NODE_HANDLER(AST_PARAMETER_DECL, parameter_decl_handler, NULL),
    NODE_HANDLER(AST_PARAMETERS_AND_QUALIFIERS, parameters_and_qualifiers_handler, NULL),
    NODE_HANDLER(AST_PARAMETERS_AND_QUALIFIERS_EXTRA, parameters_and_qualifiers_extra_handler, NULL),
    NODE_HANDLER(AST_LAMBDA_DECLARATOR, lambda_declarator_handler, NULL),
    NODE_HANDLER(AST_LAMBDA_CAPTURE, lambda_capture_handler, NULL),
    NODE_HANDLER(AST_LAMBDA_CAPTURE_DEFAULT_ADDR, simple_parameter_handler, "&"),
    NODE_HANDLER(AST_LAMBDA_CAPTURE_DEFAULT_VALUE, simple_parameter_handler, "="),
    NODE_HANDLER(AST_LAMBDA_CAPTURE_VALUE, simple_text_handler, NULL),
    NODE_HANDLER(AST_LAMBDA_CAPTURE_ADDRESS, prefix_with_parameter_then_text, "&"),
    NODE_HANDLER(AST_LAMBDA_CAPTURE_THIS, simple_parameter_handler, "this"),
    NODE_HANDLER(AST_PARENTHESIZED_INITIALIZER, parenthesized_initializer_handler, NULL),
    NODE_HANDLER(AST_EQUAL_INITIALIZER, prefix_with_parameter_then_son_handler, " = "),
    NODE_HANDLER(AST_INITIALIZER_BRACES, braced_initializer_handler, NULL),
    NODE_HANDLER(AST_POINTER_SPEC, pointer_spec_handler, NULL),
    NODE_HANDLER(AST_REFERENCE_SPEC, simple_parameter_handler, "&"),
    NODE_HANDLER(AST_RVALUE_REFERENCE_SPEC, simple_parameter_handler, "&&"),
    NODE_HANDLER(AST_CONST_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_VOLATILE_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_FUNC, abstract_declarator_function_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_ARRAY, abstract_array_declarator_handler, NULL),
    NODE_HANDLER(AST_TYPE_SPECIFIER_SEQ, type_specifier_sequence, NULL),
    NODE_HANDLER(AST_FRIEND_SPEC, simple_parameter_handler, "friend"),
    NODE_HANDLER(AST_TYPEDEF_SPEC, simple_parameter_handler, "typedef"),
    NODE_HANDLER(AST_SIGNED_TYPE, simple_parameter_handler, "signed"),
    NODE_HANDLER(AST_UNSIGNED_TYPE, simple_parameter_handler, "unsigned"),
    NODE_HANDLER(AST_LONG_TYPE, simple_parameter_handler, "long"),
    NODE_HANDLER(AST_SHORT_TYPE, simple_parameter_handler, "short"),
    NODE_HANDLER(AST_VOID_TYPE, simple_parameter_handler, "void"),
    NODE_HANDLER(AST_CHAR_TYPE, simple_parameter_handler, "char"),
    NODE_HANDLER(AST_WCHAR_TYPE, simple_parameter_handler, "wchar_t"),
    NODE_HANDLER(AST_BOOL_TYPE, simple_text_handler, NULL),
    NODE_HANDLER(AST_INT_TYPE, simple_parameter_handler, "int"),
    NODE_HANDLER(AST_FLOAT_TYPE, simple_parameter_handler, "float"),
    NODE_HANDLER(AST_DOUBLE_TYPE, simple_parameter_handler, "double"),
    NODE_HANDLER(AST_DECLTYPE, decltype_handler, NULL),
    NODE_HANDLER(AST_INLINE_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_VIRTUAL_SPEC, simple_parameter_handler, "virtual"),
    NODE_HANDLER(AST_EXPLICIT_SPEC, simple_parameter_handler, "explicit"),
    NODE_HANDLER(AST_DECIMAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_LAMBDA_EXPRESSION, lambda_expression_handler, NULL),
    NODE_HANDLER(AST_TRAILING_RETURN, trailing_return_handler, NULL),
    NODE_HANDLER(AST_VLA_EXPRESSION, simple_text_handler, NULL),
    NODE_HANDLER(AST_OCTAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_HEXADECIMAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_HEXADECIMAL_FLOAT, simple_text_handler, NULL),
    NODE_HANDLER(AST_FLOATING_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_BOOLEAN_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_CHARACTER_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_STRING_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_CONSTANT_EXPRESSION, unary_container_handler, NULL),
    NODE_HANDLER(AST_EXPRESSION, unary_container_handler, NULL),
    NODE_HANDLER(AST_COMMA, binary_operator_handler, ","),
    NODE_HANDLER(AST_CONDITIONAL_EXPRESSION, conditional_expression_handler, NULL),
    NODE_HANDLER(AST_ASSIGNMENT, binary_operator_handler, "="),
    NODE_HANDLER(AST_MUL_ASSIGNMENT, binary_operator_handler, "*="),
    NODE_HANDLER(AST_DIV_ASSIGNMENT, binary_operator_handler, "/="),
    NODE_HANDLER(AST_ADD_ASSIGNMENT, binary_operator_handler, "+="),
    NODE_HANDLER(AST_SUB_ASSIGNMENT, binary_operator_handler, "-="),
    NODE_HANDLER(AST_BITWISE_SHL_ASSIGNMENT, binary_operator_handler, "<<="),
    NODE_HANDLER(AST_SHR_ASSIGNMENT, binary_operator_handler, ">>="),
    NODE_HANDLER(AST_BITWISE_AND_ASSIGNMENT, binary_operator_handler, "&="),
    NODE_HANDLER(AST_BITWISE_OR_ASSIGNMENT, binary_operator_handler, "|="),
    NODE_HANDLER(AST_BITWISE_XOR_ASSIGNMENT, binary_operator_handler, "^="),
    NODE_HANDLER(AST_MOD_ASSIGNMENT, binary_operator_handler, "%="),
    NODE_HANDLER(AST_THROW_EXPRESSION, throw_expression_handler, NULL),
    NODE_HANDLER(AST_LOGICAL_OR, binary_operator_handler, "||"),
    NODE_HANDLER(AST_LOGICAL_AND, binary_operator_handler, "&&"),
    NODE_HANDLER(AST_BITWISE_OR, binary_operator_handler, "|"),
    NODE_HANDLER(AST_BITWISE_AND, binary_operator_handler, "&"),
    NODE_HANDLER(AST_BITWISE_XOR, binary_operator_handler, "^"),
    NODE_HANDLER(AST_EQUAL, binary_operator_handler, "=="),
    NODE_HANDLER(AST_DIFFERENT, binary_operator_handler, "!="),
    NODE_HANDLER(AST_LOWER_THAN, binary_operator_handler, "<"),
    NODE_HANDLER(AST_GREATER_THAN, binary_operator_handler, ">"),
    NODE_HANDLER(AST_GREATER_OR_EQUAL_THAN, binary_operator_handler, ">="),
    NODE_HANDLER(AST_LOWER_OR_EQUAL_THAN, binary_operator_handler, "<="),
    NODE_HANDLER(AST_BITWISE_SHL, binary_operator_handler, "<<"),
    NODE_HANDLER(AST_SHR, binary_operator_handler, ">>"),
    NODE_HANDLER(AST_MINUS, binary_operator_handler, "-"),
    NODE_HANDLER(AST_ADD, binary_operator_handler, "+"),
    NODE_HANDLER(AST_DIV, binary_operator_handler, "/"),
    NODE_HANDLER(AST_MOD, binary_operator_handler, "%"),
    NODE_HANDLER(AST_MUL, binary_operator_handler, "*"),
    NODE_HANDLER(AST_POWER, binary_operator_handler, "^^"),
    NODE_HANDLER(AST_POINTER_TO_MEMBER, infix_parameter_handler, ".*"),
    NODE_HANDLER(AST_POINTER_TO_POINTER_MEMBER, infix_parameter_handler, "->*"),
    NODE_HANDLER(AST_CAST, cast_expression_handler, NULL),
    NODE_HANDLER(AST_PREDECREMENT, prefix_with_parameter_then_son_handler, "--"),
    NODE_HANDLER(AST_PREINCREMENT, prefix_with_parameter_then_son_handler, "++"),
    NODE_HANDLER(AST_DERREFERENCE, prefix_with_parameter_then_son_handler, "*"),
    NODE_HANDLER(AST_REFERENCE, prefix_with_parameter_then_son_handler, "&"),
    NODE_HANDLER(AST_PLUS, prefix_with_parameter_then_son_handler, " +"),
    NODE_HANDLER(AST_NEG, prefix_with_parameter_then_son_handler, " -"),
    NODE_HANDLER(AST_LOGICAL_NOT, prefix_with_parameter_then_son_handler, "!"),
    NODE_HANDLER(AST_BITWISE_NOT, prefix_with_parameter_then_son_handler, "~"),
    NODE_HANDLER(AST_SIZEOF, prefix_with_parameter_then_son_handler, "sizeof "),
    NODE_HANDLER(AST_SIZEOF_TYPEID, prefix_with_parameter_then_parenthesized_son_handler, "sizeof"),
    NODE_HANDLER(AST_SIZEOF_PACK, prefix_with_parameter_then_son_handler, "sizeof... "),
    NODE_HANDLER(AST_NEW_EXPRESSION, new_expression_handler, NULL),
    NODE_HANDLER(AST_NEW_TYPE_ID, new_type_id_handler, NULL),
    NODE_HANDLER(AST_NEW_TYPE_ID_EXPR, new_type_id_expr_handler, NULL),
    NODE_HANDLER(AST_DELETE_EXPR, delete_expression_handler, NULL),
    NODE_HANDLER(AST_DELETE_ARRAY_EXPR, delete_expression_handler, NULL),
    NODE_HANDLER(AST_ARRAY_SUBSCRIPT, array_subscript_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_CALL, function_call_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_TYPE_CONVERSION, explicit_type_conversion, NULL),
    NODE_HANDLER(AST_TYPENAME_EXPLICIT_TYPE_CONV, typename_explicit_type_conversion, NULL),
    NODE_HANDLER(AST_CLASS_MEMBER_ACCESS, infix_parameter_handler, "."),
    NODE_HANDLER(AST_POINTER_CLASS_MEMBER_ACCESS, infix_parameter_handler, "->"),
    NODE_HANDLER(AST_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "."),
    NODE_HANDLER(AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "->"),
    NODE_HANDLER(AST_POSTINCREMENT, son_handler_then_suffix_parameter, "++"),
    NODE_HANDLER(AST_POSTDECREMENT, son_handler_then_suffix_parameter, "--"),
    NODE_HANDLER(AST_DYNAMIC_CAST, templated_cast_handler, "dynamic_cast"),
    NODE_HANDLER(AST_STATIC_CAST, templated_cast_handler, "static_cast"),
    NODE_HANDLER(AST_REINTERPRET_CAST, templated_cast_handler, "reinterpret_cast"),
    NODE_HANDLER(AST_CONST_CAST, templated_cast_handler, "const_cast"),
    NODE_HANDLER(AST_TYPEID_EXPR, prefix_with_parameter_then_parenthesized_son_handler, "typeid"),
    NODE_HANDLER(AST_TYPEID_TYPE, prefix_with_parameter_then_parenthesized_son_handler, "typeid"),
    NODE_HANDLER(AST_THIS_VARIABLE, simple_parameter_handler, "this"),
    NODE_HANDLER(AST_PARENTHESIZED_EXPRESSION, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_QUALIFIED_ID, qualified_id_handler, NULL),
    NODE_HANDLER(AST_DESTRUCTOR_ID, unary_container_handler, NULL),
    NODE_HANDLER(AST_DESTRUCTOR_TEMPLATE_ID, prefix_with_parameter_then_son_handler, "~"),
    NODE_HANDLER(AST_CONVERSION_FUNCTION_ID, prefix_with_parameter_then_son_handler, "operator "),
    NODE_HANDLER(AST_CONVERSION_TYPE_ID, conversion_type_id_handler, NULL),
    NODE_HANDLER(AST_CTOR_INITIALIZER, constructor_initializer_handler, NULL),
    NODE_HANDLER(AST_MEM_INITIALIZER, mem_initializer_handler, NULL),
    NODE_HANDLER(AST_MEM_INITIALIZER_ID, mem_initializer_id_handler, NULL),
    NODE_HANDLER(AST_CLASS_SPECIFIER, class_specifier_handler, NULL),
    NODE_HANDLER(AST_CLASS_KEY_CLASS, simple_parameter_handler, "class"),
    NODE_HANDLER(AST_CLASS_KEY_STRUCT, simple_parameter_handler, "struct"),
    NODE_HANDLER(AST_CLASS_KEY_UNION, simple_parameter_handler, "union"),
    NODE_HANDLER(AST_CLASS_HEAD_SPEC, class_head_handler, NULL),
    NODE_HANDLER(AST_MEMBER_ACCESS_SPEC, member_access_specifier_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATION_QUALIF, member_declaration_qualif_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATION, member_declaration_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATOR, member_declarator_handler, NULL),
    NODE_HANDLER(AST_USING_DECLARATION, using_declaration_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
    NODE_HANDLER(AST_EXPORT_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
    NODE_HANDLER(AST_TYPE_PARAMETER_CLASS, type_parameter_class_or_typename_handler, "class"),
    NODE_HANDLER(AST_TYPE_PARAMETER_TYPENAME, type_parameter_class_or_typename_handler, "typename"),
    NODE_HANDLER(AST_TYPE_PARAMETER_TEMPLATE, type_parameter_template_handler, NULL),
    NODE_HANDLER(AST_SIMPLE_TYPE_SPEC, unary_container_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_DEFINITION_HEADER, function_header_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_DEFINITION, function_definition_handler, NULL),
    NODE_HANDLER(AST_COMPOUND_STATEMENT, compound_statement_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_BODY, unary_container_handler, NULL),
    NODE_HANDLER(AST_EMPTY_STATEMENT, indented_simple_parameter_handler, ";\n"),
    NODE_HANDLER(AST_LABELED_STATEMENT, labeled_statement_handler, NULL),
    NODE_HANDLER(AST_DEFAULT_STATEMENT, default_statement_handler, NULL),
    NODE_HANDLER(AST_CASE_STATEMENT, case_statement_handler, NULL),
    NODE_HANDLER(AST_EXPRESSION_STATEMENT, expression_statement_handler, NULL),
    NODE_HANDLER(AST_CONDITION, condition_handler, NULL),
    NODE_HANDLER(AST_WHILE_STATEMENT, while_statement_handler, NULL),
    NODE_HANDLER(AST_DO_STATEMENT, do_while_statement_handler, NULL),
    NODE_HANDLER(AST_FOR_STATEMENT, for_statement_handler, NULL),
    NODE_HANDLER(AST_BREAK_STATEMENT, indented_simple_parameter_handler, "break;\n"),
    NODE_HANDLER(AST_CONTINUE_STATEMENT, indented_simple_parameter_handler, "continue;\n"),
    NODE_HANDLER(AST_RETURN_STATEMENT, return_statement_handler, NULL),
    NODE_HANDLER(AST_GOTO_STATEMENT, goto_statement_handler, NULL),
    NODE_HANDLER(AST_DECLARATION_STATEMENT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TRY_BLOCK, try_block_handler, NULL),
    NODE_HANDLER(AST_ANY_EXCEPTION, simple_parameter_handler, "..."),
    NODE_HANDLER(AST_EXCEPTION_DECLARATION, exception_declaration_handler, NULL),
    NODE_HANDLER(AST_SWITCH_STATEMENT, selection_statement_handler, NULL),
    NODE_HANDLER(AST_CATCH_HANDLER, catch_handler_handler, NULL),
    NODE_HANDLER(AST_PRIVATE_SPEC, simple_parameter_handler, "private"),
    NODE_HANDLER(AST_PROTECTED_SPEC, simple_parameter_handler, "protected"),
    NODE_HANDLER(AST_PUBLIC_SPEC, simple_parameter_handler, "public"),
    NODE_HANDLER(AST_BASE_CLAUSE, base_clause_handler, NULL),
    NODE_HANDLER(AST_BASE_SPEC, base_specifier_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_CLASS_SPEC, elaborated_type_class_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_ENUM_SPEC, elaborated_type_enum_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPENAME_SPEC, elaborated_typename_handler, NULL),
    NODE_HANDLER(AST_STATIC_SPEC, simple_parameter_handler, "static"),
    NODE_HANDLER(AST_IF_ELSE_STATEMENT, if_else_statement_handler, NULL),
    NODE_HANDLER(AST_EXCEPTION_SPECIFICATION, exception_specification_handler, NULL),
    NODE_HANDLER(AST_OPERATOR_FUNCTION_ID, operator_function_id_handler, NULL),
    NODE_HANDLER(AST_OPERATOR_FUNCTION_ID_TEMPLATE, operator_function_id_handler, NULL),
    NODE_HANDLER(AST_NEW_OPERATOR, simple_parameter_handler, "new"),
    NODE_HANDLER(AST_DELETE_OPERATOR, simple_parameter_handler, "delete"),
    NODE_HANDLER(AST_NEW_ARRAY_OPERATOR, simple_parameter_handler, "new[]"),
    NODE_HANDLER(AST_DELETE_ARRAY_OPERATOR, simple_parameter_handler, "delete[]"),
    NODE_HANDLER(AST_ADD_OPERATOR, simple_parameter_handler, "+"),
    NODE_HANDLER(AST_MINUS_OPERATOR, simple_parameter_handler, "-"),
    NODE_HANDLER(AST_MUL_OPERATOR, simple_parameter_handler, "*"),
    NODE_HANDLER(AST_DIV_OPERATOR, simple_parameter_handler, "/"),
    NODE_HANDLER(AST_MOD_OPERATOR, simple_parameter_handler, "%"),
    NODE_HANDLER(AST_BITWISE_XOR_OPERATOR, simple_parameter_handler, "^"),
    NODE_HANDLER(AST_BITWISE_AND_OPERATOR, simple_parameter_handler, "&"),
    NODE_HANDLER(AST_BITWISE_OR_OPERATOR, simple_parameter_handler, "|"),
    NODE_HANDLER(AST_BITWISE_NEG_OPERATOR, simple_parameter_handler, "~"),
    NODE_HANDLER(AST_ASSIGNMENT_OPERATOR, simple_parameter_handler, "="),
    NODE_HANDLER(AST_LOWER_OPERATOR, simple_parameter_handler, "<"),
    NODE_HANDLER(AST_GREATER_OPERATOR, simple_parameter_handler, ">"),
    NODE_HANDLER(AST_ADD_ASSIGN_OPERATOR, simple_parameter_handler, "+="),
    NODE_HANDLER(AST_SUB_ASSIGN_OPERATOR, simple_parameter_handler, "-="),
    NODE_HANDLER(AST_MUL_ASSIGN_OPERATOR, simple_parameter_handler, "*="),
    NODE_HANDLER(AST_DIV_ASSIGN_OPERATOR, simple_parameter_handler, "/="),
    NODE_HANDLER(AST_MOD_ASSIGN_OPERATOR, simple_parameter_handler, "%="),
    NODE_HANDLER(AST_BITWISE_XOR_ASSIGN_OPERATOR, simple_parameter_handler, "^="),
    NODE_HANDLER(AST_BITWISE_OR_ASSIGN_OPERATOR, simple_parameter_handler,  "|="),
    NODE_HANDLER(AST_BITWISE_AND_ASSIGN_OPERATOR, simple_parameter_handler,  "&="),
    NODE_HANDLER(AST_LEFT_OPERATOR, simple_parameter_handler, "<<"),
    NODE_HANDLER(AST_RIGHT_OPERATOR, simple_parameter_handler, ">>"),
    NODE_HANDLER(AST_LEFT_ASSIGN_OPERATOR, simple_parameter_handler, "<<="),
    NODE_HANDLER(AST_RIGHT_ASSIGN_OPERATOR, simple_parameter_handler, ">>="),
    NODE_HANDLER(AST_EQUAL_OPERATOR, simple_parameter_handler, "=="),
    NODE_HANDLER(AST_DIFFERENT_OPERATOR, simple_parameter_handler, "!="),
    NODE_HANDLER(AST_LESS_OR_EQUAL_OPERATOR, simple_parameter_handler, "<="),
    NODE_HANDLER(AST_GREATER_OR_EQUAL_OPERATOR, simple_parameter_handler, ">="),
    NODE_HANDLER(AST_LOGICAL_AND_OPERATOR, simple_parameter_handler, "&&"),
    NODE_HANDLER(AST_LOGICAL_OR_OPERATOR, simple_parameter_handler, "||"),
    NODE_HANDLER(AST_LOGICAL_NOT_OPERATOR, simple_parameter_handler, "!"),
    NODE_HANDLER(AST_INCREMENT_OPERATOR, simple_parameter_handler, "++"),
    NODE_HANDLER(AST_DECREMENT_OPERATOR, simple_parameter_handler, "--"),
    NODE_HANDLER(AST_COMMA_OPERATOR, simple_parameter_handler, ","),
    NODE_HANDLER(AST_POINTER_OPERATOR, simple_parameter_handler, "->"),
    NODE_HANDLER(AST_POINTER_DERREF_OPERATOR, simple_parameter_handler, "->*"),
    NODE_HANDLER(AST_FUNCTION_CALL_OPERATOR, simple_parameter_handler, "()"),
    NODE_HANDLER(AST_SUBSCRIPT_OPERATOR, simple_parameter_handler, "[]"),
    NODE_HANDLER(AST_ASM_DEFINITION, asm_definition_handler, NULL),
    NODE_HANDLER(AST_AUTO_STORAGE_SPEC, simple_parameter_handler, "auto"),
    NODE_HANDLER(AST_REGISTER_SPEC, simple_parameter_handler, "register"),
    NODE_HANDLER(AST_EXTERN_SPEC, simple_parameter_handler, "extern"),
    NODE_HANDLER(AST_MUTABLE_SPEC, simple_parameter_handler, "mutable"),
    NODE_HANDLER(AST_THREAD_SPEC, simple_parameter_handler, "__thread"),
    NODE_HANDLER(AST_BITFIELD_DECLARATOR, bitfield_declarator_handler, NULL),
    NODE_HANDLER(AST_ENUM_SPECIFIER, enum_specifier_handler, NULL),
    NODE_HANDLER(AST_ENUMERATOR_DEF, enumerator_def_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_INSTANTIATION, explicit_instantiation_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_SPECIALIZATION, explicit_specialization_handler, NULL),
    NODE_HANDLER(AST_LINKAGE_SPEC, linkage_specification_handler, NULL),
    NODE_HANDLER(AST_LINKAGE_SPEC_DECL, linkage_specification_decl_handler, NULL),
    NODE_HANDLER(AST_NAMESPACE_ALIAS, namespace_alias_definition_handler, NULL),
    NODE_HANDLER(AST_USING_NAMESPACE_DIRECTIVE, using_directive_handler, NULL),
    NODE_HANDLER(AST_NAMESPACE_DEFINITION, namespace_definition_handler, NULL),
    NODE_HANDLER(AST_NEW_PLACEMENT, list_parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_KR_PARAMETER_LIST, kr_parameter_list_handler, NULL),
    NODE_HANDLER(AST_DESIGNATED_INITIALIZER, designated_initializer_handler, NULL),
    NODE_HANDLER(AST_DESIGNATION, designation_handler, NULL),
    NODE_HANDLER(AST_INDEX_DESIGNATOR, index_designator_handler, NULL),
    NODE_HANDLER(AST_FIELD_DESIGNATOR, field_designator_handler, NULL),
    NODE_HANDLER(AST_UNKNOWN_PRAGMA, unknown_pragma_handler, NULL),
    NODE_HANDLER(AST_PP_COMMENT, pp_comment_handler, NULL),
    NODE_HANDLER(AST_PP_TOKEN, pp_prepro_token_handler, NULL),
    NODE_HANDLER(AST_VERBATIM, verbatim_construct_handler, NULL),
    NODE_HANDLER(AST_DIMENSION_STR, simple_text_handler, NULL),
    NODE_HANDLER(AST_STATIC_ASSERT, static_assert_handler, NULL),
    NODE_HANDLER(AST_DEFAULTED_FUNCTION_DEFINITION, defaulted_or_deleted_function_def, NULL),
    NODE_HANDLER(AST_DELETED_FUNCTION_DEFINITION, defaulted_or_deleted_function_def, NULL),
    NODE_HANDLER(AST_NOEXCEPT_EXPRESSION, prefix_with_parameter_then_parenthesized_son_handler, "noexcept"),
    NODE_HANDLER(AST_NOEXCEPT_SPECIFICATION, prefix_with_parameter_then_optional_parenthesized_son_handler, "noexcept"),
    // Pragma custom
    NODE_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, pragma_custom_directive_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, pragma_custom_construct_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CLAUSE, pragma_custom_clause_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_LINE, pragma_custom_line_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CLAUSE_ARG, simple_text_handler, NULL),
    // GCC Extensions
    NODE_HANDLER(AST_GCC_EXTENSION, gcc_extension_prefix_handler, "__extension__ "),
    NODE_HANDLER(AST_GCC_EXTENSION_EXPR, prefix_with_token_text_then_son_handler, NULL),
    NODE_HANDLER(AST_GCC_LABEL_DECL, gcc_label_declaration_handler, NULL),
    NODE_HANDLER(AST_GCC_ATTRIBUTE, gcc_attribute_handler, NULL),
    NODE_HANDLER(AST_GCC_ATTRIBUTE_EXPR, gcc_attribute_value_handler, NULL),
    NODE_HANDLER(AST_GCC_ASM_DEFINITION, gcc_asm_definition_handler, NULL),
    NODE_HANDLER(AST_GCC_ASM_DEF_PARMS, gcc_asm_def_parameters, NULL),
    NODE_HANDLER(AST_GCC_ASM_OPERAND, gcc_asm_operand_handler, NULL),
    NODE_HANDLER(AST_GCC_COMPLEX_TYPE, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_IMAGINARY_TYPE, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_TYPEOF, gcc_typeof_handler, NULL),
    NODE_HANDLER(AST_GCC_TYPEOF_EXPR, gcc_typeof_expr_handler, NULL),
    NODE_HANDLER(AST_GCC_RESTRICT_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_PARENTHESIZED_EXPRESSION, gcc_parenthesized_expression_handler, NULL),
    NODE_HANDLER(AST_GCC_REAL_PART, prefix_with_parameter_then_son_handler, "__real__ "),
    NODE_HANDLER(AST_GCC_IMAG_PART, prefix_with_parameter_then_son_handler, "__imag__ "),
    NODE_HANDLER(AST_GCC_ALIGNOF, prefix_with_token_text_then_son_handler, NULL),
    NODE_HANDLER(AST_GCC_ALIGNOF_TYPE, gcc_alignof_type_handler, NULL),
    NODE_HANDLER(AST_GCC_LABEL_ADDR, prefix_with_parameter_then_son_handler, "&&"),
    NODE_HANDLER(AST_GCC_ASM_SPEC, gcc_asm_specification_handler, NULL),
    NODE_HANDLER(AST_GCC_REFERENCE_SPEC, gcc_reference_spec_handler, NULL),
    NODE_HANDLER(AST_GCC_INITIALIZER_CLAUSE, gcc_initializer_clause_handler, NULL),
    NODE_HANDLER(AST_GCC_CASE_STATEMENT, gcc_case_statement_handler, NULL),
    NODE_HANDLER(AST_GCC_GOTO_STATEMENT, gcc_goto_statement_handler, NULL),
    NODE_HANDLER(AST_GCC_MEM_INITIALIZER, gcc_mem_initializer_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_VA_ARG, gcc_builtin_va_arg_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_OFFSETOF, gcc_builtin_offsetof_handler, NULL),
    NODE_HANDLER(AST_GCC_OFFSETOF_MEMBER_DESIGNATOR , gcc_offsetof_member_designator_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_CHOOSE_EXPR, gcc_builtin_choose_expr_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_TYPES_COMPATIBLE_P, gcc_builtin_types_compatible_p_handler, NULL),
    NODE_HANDLER(AST_GCC_POSTFIX_EXPRESSION, gcc_postfix_expression, NULL),
    NODE_HANDLER(AST_GCC_CONDITIONAL_EXPRESSION, gcc_conditional_expression, NULL),
    NODE_HANDLER(AST_GXX_TYPE_TRAITS, gxx_type_traits, NULL),
    // Mercurium extensions
    NODE_HANDLER(AST_ARRAY_SECTION, array_section_handler, ":"),
    NODE_HANDLER(AST_ARRAY_SECTION_SIZE, array_section_handler, ";"),
    NODE_HANDLER(AST_SHAPING_EXPRESSION, shaping_expression_handler, NULL),
    NODE_HANDLER(AST_NODECL_LITERAL, nodecl_literal_handler, NULL),
    // UPC 1.2
    NODE_HANDLER(AST_UPC_LOCALSIZEOF, upc_sizeof_expr, "local"),
    NODE_HANDLER(AST_UPC_BLOCKSIZEOF, upc_sizeof_expr, "block"),
    NODE_HANDLER(AST_UPC_ELEMSIZEOF, upc_sizeof_expr, "elem"),
    NODE_HANDLER(AST_UPC_LOCALSIZEOF_TYPEID, upc_sizeof_type, "local"),
    NODE_HANDLER(AST_UPC_BLOCKSIZEOF_TYPEID, upc_sizeof_type, "block"),
    NODE_HANDLER(AST_UPC_ELEMSIZEOF_TYPEID, upc_sizeof_type, "elem"),
    NODE_HANDLER(AST_UPC_SHARED, upc_shared_qualifier, NULL),
    NODE_HANDLER(AST_UPC_RELAXED, simple_parameter_handler, "relaxed"),
    NODE_HANDLER(AST_UPC_STRICT, simple_parameter_handler, "strict"),
    NODE_HANDLER(AST_UPC_LAYOUT_QUALIFIER, upc_layout_qualifier, NULL),
    NODE_HANDLER(AST_UPC_LAYOUT_UNDEF, simple_parameter_handler, "*"),
    NODE_HANDLER(AST_UPC_NOTIFY, upc_synch_statement, "upc_notify"),
    NODE_HANDLER(AST_UPC_WAIT, upc_synch_statement, "upc_wait"),
    NODE_HANDLER(AST_UPC_BARRIER, upc_synch_statement, "upc_barrier"),
    NODE_HANDLER(AST_UPC_FENCE, upc_synch_statement, "upc_fence"),
    NODE_HANDLER(AST_UPC_CONTINUE, simple_text_handler, "continue"),
    NODE_HANDLER(AST_UPC_FORALL, upc_iteration_statement, NULL),
    NODE_HANDLER(AST_UPC_FORALL_HEADER, upc_forall_header, NULL),
    // CUDA
    NODE_HANDLER(AST_CUDA_DEVICE, simple_text_handler, "__device__"),
    NODE_HANDLER(AST_CUDA_GLOBAL, simple_text_handler, "__global__"),
    NODE_HANDLER(AST_CUDA_HOST, simple_text_handler, "__host__"),
    NODE_HANDLER(AST_CUDA_CONSTANT, simple_text_handler, "__constant__"),
    NODE_HANDLER(AST_CUDA_SHARED, simple_text_handler, "__shared__"),
    NODE_HANDLER(AST_CUDA_KERNEL_CALL, cuda_kernel_call_handler, NULL),
    NODE_HANDLER(AST_CUDA_KERNEL_ARGUMENTS, cuda_kernel_arguments_handler, NULL),
    //OPENCL
    NODE_HANDLER(AST_OPENCL_KERNEL, simple_text_handler, "__kernel"),
    NODE_HANDLER(AST_OPENCL_CONSTANT, simple_text_handler, "__constant"),
    NODE_HANDLER(AST_OPENCL_GLOBAL, simple_text_handler, "__global"),
    NODE_HANDLER(AST_OPENCL_LOCAL, simple_text_handler, "__local"),
    // IBM XL
    NODE_HANDLER(AST_XL_BUILTIN_SPEC, simple_text_handler, "_Builtin"),
    // Nodecl nodes that may reach prettyprint due to diagnostics
    NODE_HANDLER(NODECL_CXX_DEP_NAME_SIMPLE, simple_text_handler, NULL),
    NODE_HANDLER(AST_INTEL_ASSUME, prefix_with_parameter_then_son_handler, "__assume"),
    NODE_HANDLER(AST_INTEL_ASSUME_ALIGNED, intel_assume_aligned, "__assume_aligned"),
};

static void prettyprint_level(FILE* f, AST a, prettyprint_context_t* pt_ctx);

void cxx_prettyprint(FILE* f, AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);
    prettyprint_level(f, a, &pt_ctx);
}

void prettyprint(FILE* f, AST a)
{
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        cxx_prettyprint(f, a);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        fortran_prettyprint(f, a);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

const char* cxx_prettyprint_in_buffer(AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    return prettyprint_in_buffer_common(a, prettyprint_level, &pt_ctx);
}

const char* prettyprint_in_buffer(AST a)
{
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        return cxx_prettyprint_in_buffer(a);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        return fortran_prettyprint_in_buffer(a);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

const char* cxx_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    pt_ctx.callback = callback;
    pt_ctx.callback_data = data;

    return prettyprint_in_buffer_common(a, prettyprint_level, &pt_ctx);
}

const char* prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data)
{
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        return cxx_prettyprint_in_buffer_callback(a, callback, data);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        return fortran_prettyprint_in_buffer_callback(a, callback, data);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

const char* cxx_list_handler_in_buffer(AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    return prettyprint_in_buffer_common(a, list_handler, &pt_ctx);
}

const char* list_handler_in_buffer(AST a)
{
    if (IS_C_LANGUAGE || IS_CXX_LANGUAGE)
    {
        return cxx_list_handler_in_buffer(a);
    }
    else if (IS_FORTRAN_LANGUAGE)
    {
        return fortran_list_handler_in_buffer(a);
    }
    else
    {
        internal_error("Code unreachable", 0);
    }
}

static void increase_level(prettyprint_context_t *pt_ctx)
{
    pt_ctx->level++;
}

static void zero_level(prettyprint_context_t* pt_ctx)
{
    pt_ctx->level = 0;
}

static void increase_level_num(prettyprint_context_t *pt_ctx, int num)
{
    pt_ctx->level += num;
}

static void increase_level_if_not_compound( prettyprint_context_t* pt_ctx, AST a)
{
    if (ASTKind(a) != AST_COMPOUND_STATEMENT)
        pt_ctx->level++;
}

static void double_colon_handler(FILE* f, AST node, prettyprint_context_t* pt_ctx)
{
    if (pt_ctx->last_is_left_angle)
    {
        token_fprintf(f, node, pt_ctx, " ");
    }

    token_fprintf(f, node, pt_ctx, "::");
}

static void indent_at_level(FILE* f, AST node, prettyprint_context_t* pt_ctx)
{
    int i;
    for (i = 0; i < pt_ctx->level; i++)
    {
        token_fprintf(f, node, pt_ctx, pt_ctx->indent_str);
    }
}

static void prettyprint_level(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (a == NULL)
        return;

    prettyprint_handler_t hnd = handlers_list[ASTKind(a)].handler;

    if (hnd == NULL)
    {
        fprintf(f, "<<<%s:%p>>>", ast_node_names[ASTKind(a)], a);
        return;
    }
    else
    {
        // fprintf(stderr, "Calling handler of '%s'\n", ast_node_names[ASTKind(a)]);
    }

    // If there is a callback, call it
    const char* cb_result = NULL;
    if (pt_ctx->callback != NULL)
    {
        cb_result = (pt_ctx->callback)(a, pt_ctx->callback_data);
    }
    // If the callback did not return anything use the normal handler
    if (cb_result == NULL)
    {
        (*hnd)(f, a, pt_ctx);
    }
    else
    {
        // Otherwise use for this node what the callback returned
        token_fprintf(f, a, pt_ctx, "%s", cb_result);
    }
}

static void ambiguity_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    // fprintf(f, "/* Ambiguous node with %d options */ ", a->num_ambig);
    // Print the first ambiguity because all "look like" the same, no matter
    // which one is actually printed
    prettyprint_level(f, ast_get_ambiguity(a, 0), pt_ctx);
}

static void character_separated_sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx, 
        const char* separator, prettyprint_handler_t specific_handler)
{
    if (a == NULL)
        return;
    if (ASTKind(a) == AST_AMBIGUITY)
    {
        character_separated_sequence_handler(f, ast_get_ambiguity(a, 0), pt_ctx, separator, specific_handler);
        return;
    }

    if (ASTSon0(a) != NULL)
    {
        character_separated_sequence_handler(f, ASTSon0(a), pt_ctx, separator, specific_handler);
        token_fprintf(f, a, pt_ctx, separator);
    }

    ((specific_handler == NULL) ? prettyprint_level : specific_handler)(f, ASTSon1(a), pt_ctx);
}

static void spaced_sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, " ", NULL);
}

static void list_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, ", ", NULL);
}

static void sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, "", NULL);
}

static void simple_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " ");
        }
    }

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void simple_parameter_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER);
}

static void indented_simple_parameter_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    simple_parameter_handler(f, a, pt_ctx);
}

static void init_declarator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void unary_container_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void parenthesized_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void list_parenthesized_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void simple_text_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "%s", ASTText(a));
}

static void template_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, "<");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    if (!pt_ctx->last_is_right_angle)
    {
        token_fprintf(f, a, pt_ctx, ">");
    }
    else
    {
        token_fprintf(f, a, pt_ctx, " >");
    }
}

static void type_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void abstract_declarator_function_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    if (ASTKind(ASTSon1(a)) != AST_KR_PARAMETER_LIST)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "(");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }

    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        spaced_sequence_handler(f, ASTSon2(a), pt_ctx);
    }

    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void abstract_array_declarator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, "[");

    // static of C99
    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), pt_ctx);
        fprintf(f, " ");
    }

    // cv-qualifier-seq of C99
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), pt_ctx);
        fprintf(f, " ");
    }

    // expression
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "]");
}

static void null_handler(FILE* f UNUSED_PARAMETER, AST a UNUSED_PARAMETER, prettyprint_context_t* pt_ctx UNUSED_PARAMETER) { }

static void parameter_decl_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " = ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void parameters_and_qualifiers_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void parameters_and_qualifiers_extra_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
        token_fprintf(f, a, pt_ctx, " ");
    spaced_sequence_handler(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
        token_fprintf(f, a, pt_ctx, " ");
    spaced_sequence_handler(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
        token_fprintf(f, a, pt_ctx, " ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    if (ASTSon3(a) != NULL)
        token_fprintf(f, a, pt_ctx, " ");
    prettyprint_level(f, ASTSon3(a), pt_ctx);
}

static void prefix_with_token_text_then_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void prefix_with_parameter_then_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void prefix_with_parameter_then_text(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s%s", HELPER_PARAMETER_STRING, ASTText(a));
}


static void braced_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "{");
    if (ASTSon0(a) != NULL)
    {
        AST init_list = ASTSon0(a), it;

        if (ASTSon0(init_list) != NULL)
        {
            token_fprintf(f, a, pt_ctx, "\n");

            NEW_PT_CONTEXT(new_ctx, increase_level);

            for_each_element(init_list, it)
            {
                indent_at_level(f, a, new_ctx);

                AST initializer = ASTSon1(it);
                prettyprint_level(f, initializer, new_ctx);

                if (it != init_list)
                {
                    token_fprintf(f, initializer, pt_ctx, ",");
                }
                token_fprintf(f, initializer, pt_ctx, "\n");
            }
            indent_at_level(f, a, pt_ctx);
        }
        // Aesthetic case for int a{3} and int a[] = {3};
        else
        {
            AST item = ASTSon1(init_list);
            prettyprint_level(f, item, pt_ctx);
        }
    }
    token_fprintf(f, a, pt_ctx, "}");
}

static void pointer_decl_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    // token_fprintf(f, a, pt_ctx, " ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void pointer_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "::");
    }

    token_fprintf(f, a, pt_ctx, "*");
    
    if (ASTSon1(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
}

static void type_specifier_sequence(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon0(a), pt_ctx);

        if (ASTSon1(a) != NULL || ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " ");
        }
    }


    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);

        if (ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " ");
        }
    }

    if (ASTSon2(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon2(a), pt_ctx);
    }
}

static void binary_operator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " %s ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void throw_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "throw");

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void conditional_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ? ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " : ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void cast_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ") ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void prefix_with_parameter_then_parenthesized_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s(", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void prefix_with_parameter_then_optional_parenthesized_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void new_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "new ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);

    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void new_type_id_expr_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, "new");

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");

    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void new_type_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void delete_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if(ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    
    token_fprintf(f, a, pt_ctx, "delete");
    if (ASTKind(a) == AST_DELETE_ARRAY_EXPR)
    {
        token_fprintf(f, a, pt_ctx, "[]");
    }
    token_fprintf(f, a, pt_ctx, " ");

    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void array_subscript_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "[");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "]");
}

static void function_call_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void explicit_type_conversion(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void typename_explicit_type_conversion(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "typename ");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void infix_parameter_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void template_member_access(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "%stemplate ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void son_handler_then_suffix_parameter(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
}

static void templated_cast_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s<", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " >");
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void member_declaration_qualif_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void qualified_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    if (ASTSon1(a) != NULL)
    {
        character_separated_sequence_handler(f, ASTSon1(a), pt_ctx, "::", NULL);
        token_fprintf(f, a, pt_ctx, "::");
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void conversion_type_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void constructor_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ": ");
    list_handler(f, ASTSon0(a), pt_ctx);
}

static void mem_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void mem_initializer_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void class_specifier_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "\n");
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "{\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx, increase_level_num, 2);

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), new_pt_ctx);
    }
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "}");
}

static void class_head_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void member_access_specifier_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    // We are decreasing, actually, this is what this -1 means here
    NEW_PT_CONTEXT_ARG(new_pt_ctx, increase_level_num, -1);

    indent_at_level(f, a, new_pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " :\n");
}

static void using_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "using ");

    if (ASTKind(a) == AST_USING_DECLARATION_TYPENAME)
    {
        token_fprintf(f, a, pt_ctx, "typename ");
    }
    
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ";\n");
}

static void template_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTKind(a) == AST_EXPORT_TEMPLATE_DECLARATION)
    {
        token_fprintf(f, a, pt_ctx, "export ");
    }
    token_fprintf(f, a, pt_ctx, "template<");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " >\n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void type_parameter_class_or_typename_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s ", HELPER_PARAMETER_STRING);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " = ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void type_parameter_template_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "template<");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " > class ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " = ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void member_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " ");
        }
    }

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void member_declarator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void function_header_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    // Decl specifier seq
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    // Functional declarator
    prettyprint_level(f, ASTSon1(a), pt_ctx);

    // C: This is always NULL
    // C++: GCC attributes
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void function_definition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);
    token_fprintf(f, a, pt_ctx, "\n");

    if (ASTSon1(a) != NULL)
    {
        // C++: Member initializers
        // C90: Old-style-function definition
        token_fprintf(f, a, pt_ctx, "\n");
        prettyprint_level(f, ASTSon1(a), new_pt_ctx);
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "\n");
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void defaulted_or_deleted_function_def(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ");

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    token_fprintf(f, a, pt_ctx, "= %s;\n", ASTText(a));
}

static void compound_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "{\n");

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), new_pt_ctx);
    }
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "}\n");
}

static void labeled_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " : \n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void case_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "case ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " : \n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void default_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "default : \n");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void expression_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void selection_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "switch (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")\n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void condition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " = ");
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void while_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "while (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx, increase_level_if_not_compound, ASTSon1(a));
    prettyprint_level(f, ASTSon1(a), new_pt_ctx);
}

static void do_while_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "do\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx, increase_level_if_not_compound, ASTSon0(a));
    prettyprint_level(f, ASTSon0(a), new_pt_ctx);

    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "while (");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ");\n");
}

static void for_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "for (");

    AST loop_control = ASTSon0(a);

    NEW_PT_CONTEXT(zero_ctx, zero_level);

    if (ASTSon0(loop_control) != NULL)
    {
        prettyprint_level(f, ASTSon0(loop_control), zero_ctx);
    }

    NEW_PT_CONTEXT(new_pt_ctx_0, increase_level);
    indent_at_level(f, loop_control, new_pt_ctx_0);
    if (ASTSon1(loop_control) != NULL)
    {
        prettyprint_level(f, ASTSon1(loop_control), zero_ctx);
    }
    
    token_fprintf(f, loop_control, pt_ctx, ";\n");

    indent_at_level(f, loop_control, new_pt_ctx_0);
    if (ASTSon2(loop_control) != NULL)
    {
        prettyprint_level(f, ASTSon2(loop_control), zero_ctx);
    }

    token_fprintf(f, loop_control, pt_ctx, ")\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx_1, increase_level_if_not_compound, ASTSon1(a));
    prettyprint_level(f, ASTSon1(a), new_pt_ctx_1);
}

static void return_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "return");

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, ";\n");
}

static void goto_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);

    token_fprintf(f, a, pt_ctx, "goto ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void try_block_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);

    token_fprintf(f, a, pt_ctx, "try\n");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void catch_handler_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "catch (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")\n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void exception_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void base_clause_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, " : ");
    list_handler(f, ASTSon0(a), pt_ctx);
}

static void base_specifier_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void elaborated_type_class_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ");

    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void elaborated_type_enum_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "enum ");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void elaborated_typename_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "typename ");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void if_else_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "if (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx_0, increase_level_if_not_compound, ASTSon1(a));
    prettyprint_level(f, ASTSon1(a), new_pt_ctx_0);

    if (ASTSon2(a) != NULL)
    {
        indent_at_level(f, a, pt_ctx);
        token_fprintf(f, a, pt_ctx, "else\n");

        NEW_PT_CONTEXT_ARG(new_pt_ctx_1, increase_level_if_not_compound, ASTSon2(a));
        prettyprint_level(f, ASTSon2(a), new_pt_ctx_1);
    }
}

static void exception_specification_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "throw (");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void operator_function_id_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "operator ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    
    if (ASTKind(a) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        token_fprintf(f, a, pt_ctx, "<");
        if (ASTSon1(a) != NULL)
        {
            list_handler(f, ASTSon1(a), pt_ctx);
        }
        token_fprintf(f, a, pt_ctx, " >");
    }
}

static void asm_definition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    // Use the exact token (asm or __asm__)
    token_fprintf(f, a, pt_ctx, "%s ", ASTText(a));

    // Optional 'volatile' for GCC
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ");\n");
}

static void bitfield_declarator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }

    token_fprintf(f, a, pt_ctx, ": ");

    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void enum_specifier_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "enum ");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "\n");
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "{");

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), new_pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "\n");
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "}");
}

static void enumerator_def_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "\n");
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " = ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void explicit_specialization_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "template<>\n");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void linkage_specification_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "extern ");
    
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    
    token_fprintf(f, a, pt_ctx, "\n");
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "{\n");

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), new_pt_ctx);
    }
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "}\n");
}

static void linkage_specification_decl_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "extern ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, "\n");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void namespace_alias_definition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "namespace ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " = ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void using_directive_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "using namespace ");

    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ";\n");
}

static void namespace_definition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "namespace ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " {\n");

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);

    prettyprint_level(f, ASTSon1(a), new_pt_ctx);
    
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "}\n");
}

static void parenthesized_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void static_assert_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "static_assert(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ");\n");
}

static void kr_parameter_list_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    AST list = ASTSon0(a);

    list_handler(f, list, pt_ctx);
}

static void designated_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void designation_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    sequence_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " = ");
}

static void index_designator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "[");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "]");
}

static void field_designator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ".");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void decltype_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    // If the language is c++0x It will be used '__decltype' function
    // Otherwise It will be used 'decltype' gnu function
    if (!IS_CXX11_LANGUAGE)
    {
        token_fprintf(f, a, pt_ctx, "__");
    }
    token_fprintf(f, a, pt_ctx, "decltype(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void unknown_pragma_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "#pragma ");
    token_fprintf(f, a, pt_ctx, "%s\n", ASTText(a));
}

static void pp_prepro_token_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    const char* text = ASTText(a);

    if (pt_ctx->internal_output)
    {
        // Do nothing special
        token_fprintf(f, a, pt_ctx, "@-P-@%s@-PP-@", text);
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "%s", text);
    }
}

static void pp_comment_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    const char* text = ASTText(a);

    if (pt_ctx->internal_output)
    {
        // Do nothing special
        token_fprintf(f, a, pt_ctx, "@-CC-@%s@-C-@", text);
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "/* %s */\n", text);
    }
}

static void verbatim_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (pt_ctx->internal_output)
    {
        token_fprintf(f, a, pt_ctx, "%s", "#pragma mcxx verbatim start");
        if (ASTSon0(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " type(");
            prettyprint_level(f, ASTSon0(a), pt_ctx);
            token_fprintf(f, a, pt_ctx, ")");
        }
        token_fprintf(f, a, pt_ctx, "\n", ASTText(a));
    }

    token_fprintf(f, a, pt_ctx, "%s", ASTText(a));
    if (pt_ctx->internal_output)
    {
        token_fprintf(f, a, pt_ctx, "%s", "#pragma mcxx verbatim end\n");
    }
}

static void pragma_custom_line_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTText(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "%s ", ASTText(a));
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");

        // This is a list inside another list, it cannot be 
        // handled normally
        list_handler(f, ASTSon1(a), pt_ctx);

        token_fprintf(f, a, pt_ctx, ") ");
    }
    if (ASTSon0(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "\n");
}

static void pragma_custom_directive_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "#pragma %s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void pragma_custom_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "#pragma %s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void pragma_custom_clause_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", ASTText(a));
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");

        // This is a list inside another list, it cannot be 
        // handled normally
        list_handler(f, ASTSon0(a), pt_ctx);

        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void gcc_label_declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "label ");
    list_handler(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ";\n");
}

static void gcc_attribute_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ASTText(a));
    token_fprintf(f, a, pt_ctx, "((");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "))");
}

static void gcc_attribute_value_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void gcc_asm_definition_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, ASTText(a));
    token_fprintf(f, a, pt_ctx, " ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, "(");

    prettyprint_level(f, ASTSon1(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ");\n");
}

static void gcc_asm_def_parameters(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ": ");

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ": ");

    if (ASTSon2(a) != NULL)
    {
        list_handler(f, ASTSon2(a), pt_ctx);
    }

    if (ASTSon3(a) != NULL)
    {
        // This can appear only if explicitly given
        token_fprintf(f, a, pt_ctx, ": ");
        list_handler(f, ASTSon3(a), pt_ctx);
    }
}

static void gcc_asm_operand_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "[");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "] ");
    }

    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " (");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_typeof_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ASTText(a));
    token_fprintf(f, a, pt_ctx, " (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_typeof_expr_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ASTText(a));
    CXX_LANGUAGE()
    {
        token_fprintf(f, a, pt_ctx, " ");
    }
    C_LANGUAGE()
    {
        token_fprintf(f, a, pt_ctx, "(");
    }
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    C_LANGUAGE()
    {
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void gcc_asm_specification_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s (", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_reference_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "& ");
    spaced_sequence_handler(f, ASTSon0(a), pt_ctx);
}

static void gcc_initializer_clause_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ":");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void gcc_case_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "case ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "...");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " : ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void gcc_goto_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "goto * ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void gcc_mem_initializer_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_builtin_va_arg_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "__builtin_va_arg(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_builtin_offsetof_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "__builtin_offsetof(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_builtin_choose_expr_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "__builtin_choose_expr(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_builtin_types_compatible_p_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "__builtin_types_compatible_p(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void gcc_postfix_expression(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void gcc_alignof_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, ASTText(a));
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void explicit_instantiation_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
    token_fprintf(f, a, pt_ctx, "template ");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void gcc_conditional_expression(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ? : ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void gcc_extension_prefix_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, ASTText(a));
    token_fprintf(f, a, pt_ctx, "\n");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void gcc_offsetof_member_designator_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void gcc_parenthesized_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    // This one is printed specially, otherwise it looks specially messed
    token_fprintf(f, a, pt_ctx, "({\n");
    AST compound_statement = ASTSon0(a);
    if (ASTSon0(compound_statement) != NULL)
    {
        NEW_PT_CONTEXT(new_pt_ctx, increase_level);
        prettyprint_level(f, ASTSon0(compound_statement), new_pt_ctx);
    }
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "})");
}

static void array_section_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "[");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "]");
}

static void shaping_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    AST it;
    AST shape_list = ASTSon0(a);
    for_each_element(shape_list, it)
    {
        AST expr = ASTSon1(it);

        token_fprintf(f, a, pt_ctx, "[");
        prettyprint_level(f, expr, pt_ctx);
        token_fprintf(f, a, pt_ctx, "]");
    }
    token_fprintf(f, a, pt_ctx, " ");

    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void nodecl_literal_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    nodecl_t nodecl = nodecl_make_from_ast_nodecl_literal(a);
    token_fprintf(f, a, pt_ctx, "%s", codegen_to_str(nodecl, CURRENT_COMPILED_FILE->global_decl_context));
}

static void gxx_type_traits(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s(", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void upc_sizeof_expr(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "upc_%s ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void upc_sizeof_type(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "upc_%s(", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void upc_shared_qualifier(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "shared");

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void upc_layout_qualifier(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "[");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "]");
}

static void upc_synch_statement(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ";\n");
}

static void upc_iteration_statement(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "\n");

    NEW_PT_CONTEXT_ARG(new_pt_ctx, increase_level_if_not_compound, ASTSon1(a));

    prettyprint_level(f, ASTSon1(a), new_pt_ctx);
}

static void upc_forall_header(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    NEW_PT_CONTEXT(zero_ctx, zero_level);

    token_fprintf(f, a, pt_ctx, "upc_forall(");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), zero_ctx);
    }

    NEW_PT_CONTEXT(new_pt_ctx, increase_level);

    indent_at_level(f, a, new_pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), zero_ctx);
    }
    token_fprintf(f, a, pt_ctx, ";\n");

    if (ASTSon2(a) != NULL)
    {
        indent_at_level(f, a, new_pt_ctx);
        prettyprint_level(f, ASTSon2(a), zero_ctx);
    }
    token_fprintf(f, a, pt_ctx, ";\n");

    if (ASTSon3(a) != NULL)
    {
        indent_at_level(f, a, new_pt_ctx);
        prettyprint_level(f, ASTSon3(a), zero_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void cuda_kernel_call_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    prettyprint_level(f, ASTSon1(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void cuda_kernel_arguments_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "<<<");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);

    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }

    token_fprintf(f, a, pt_ctx, ">>>");
}

static void intel_assume_aligned(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "__assume_aligned(");

    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);

    token_fprintf(f, a, pt_ctx, ")");
}

static void trailing_return_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, " -> ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void lambda_capture_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon0(a) != NULL
            && ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
    }
    list_handler(f, ASTSon1(a), pt_ctx);
}

static void lambda_declarator_handler(FILE *f, AST a, prettyprint_context_t *pt_ctx)
{
    AST parameter_decls = ASTSon0(a);
    AST mutable = ASTSon1(a);
    AST trailing_return = ASTSon2(a);

    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon0(parameter_decls), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");

    prettyprint_level(f, mutable, pt_ctx);
    if (mutable != NULL)
        token_fprintf(f, a, pt_ctx, " ");

    AST extra = ASTSon1(parameter_decls);

    if (extra != NULL)
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            AST parameter_decl = ast_get_child(extra, i);

            if (parameter_decl != NULL)
            {
                sequence_handler(f, parameter_decl, pt_ctx);
                token_fprintf(f, a, pt_ctx, " ");
            }
        }
    }

    prettyprint_level(f, trailing_return, pt_ctx);
}

static void lambda_expression_handler(FILE *f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "[");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "]");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

// Common

extern const char* prettyprint_in_buffer_common(AST a, 
        void (*pretty_func)(FILE*, AST, prettyprint_context_t* pt_ctx), 
        prettyprint_context_t *pt_ctx)
{
    char *result = NULL;
#ifdef HAVE_OPEN_MEMSTREAM
    size_t size = 0;

    FILE* temporal_stream = open_memstream(&result, &size);
    pretty_func(temporal_stream, a, pt_ctx);
    fclose(temporal_stream);
#else
    FILE* temporal_file = tmpfile();

    pretty_func(temporal_file, a, pt_ctx);

    int bytes_file = ftell(temporal_file) + 20;
    rewind(temporal_file);

    result = NEW_VEC(char, bytes_file);
    fread(result, bytes_file, sizeof(char), temporal_file);
    fclose(temporal_file);
#endif
    int c = strlen(result) - 1;

    while (result[c] == '\n')
    {
        result[c] = '\0';
        c--;
    }

    const char* unique_result = uniquestr(result);
    DELETE(result);

    return unique_result;
}

extern int character_level_vfprintf(FILE* stream, prettyprint_context_t *pt_ctx, const char* format, va_list args)
{
    int result;
    int size = 512;
    char* c = NEW_VEC(char, size);
    va_list va;

    va_copy(va, args);
    result = vsnprintf(c, size, format, va);
    va_end(va);

    while (result < 0 || result >= size)
    {
        va_copy(va, args);
        size *= 2;
        c = NEW_REALLOC(char, c, size);
        result = vsnprintf(c, size, format, va);
        va_end(va);
    }

    fprintf(stream, "%s", c);

    if (result > 0)
    {
        pt_ctx->last_is_left_angle = (c[result - 1] == '<');
        pt_ctx->last_is_right_angle = (c[result - 1] == '>');

        char *p = strrchr(c, '\n');
        if (p == NULL)
        {
            pt_ctx->column += strlen(c);
        }
        else
        {
            pt_ctx->column = (ptrdiff_t)((&c[result-1]) - p);
        }
    }

    DELETE(c);

    return result;
}


extern int token_fprintf(FILE *stream, AST node UNUSED_PARAMETER, prettyprint_context_t* pt_ctx, const char *format, ...)
{
    int result = 0;
    va_list args;

    va_start(args, format);
    result = character_level_vfprintf(stream, pt_ctx, format, args);
    va_end(args);

    return result;
}

