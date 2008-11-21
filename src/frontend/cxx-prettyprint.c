/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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


typedef void (*prettyprint_handler_t)(FILE* f, AST a, int level);

typedef struct {
    char* handler_name;
    prettyprint_handler_t handler;
    char* parameter;
} prettyprint_entry_t;

#define HANDLER_PROTOTYPE(name) \
 static void name(FILE* f, AST a, int level)

#define NODE_HANDLER(type, handler, parameter) \
  [type] = {#handler, handler, parameter}

HANDLER_PROTOTYPE(ambiguity_handler);
HANDLER_PROTOTYPE(sequence_handler);
HANDLER_PROTOTYPE(spaced_sequence_handler);
HANDLER_PROTOTYPE(list_handler);
HANDLER_PROTOTYPE(simple_declaration_handler);
HANDLER_PROTOTYPE(simple_parameter_handler);
HANDLER_PROTOTYPE(indented_simple_parameter_handler);
HANDLER_PROTOTYPE(unary_container_handler);
HANDLER_PROTOTYPE(parenthesized_son_handler);
HANDLER_PROTOTYPE(nested_name_handler);
HANDLER_PROTOTYPE(simple_text_handler);
HANDLER_PROTOTYPE(template_id_handler);
HANDLER_PROTOTYPE(abstract_array_declarator_handler);
HANDLER_PROTOTYPE(abstract_declarator_function_handler);
HANDLER_PROTOTYPE(null_handler);
HANDLER_PROTOTYPE(parameter_decl_handler);
HANDLER_PROTOTYPE(init_declarator_handler);
HANDLER_PROTOTYPE(pointer_decl_handler);
HANDLER_PROTOTYPE(abstract_declarator_handler);
HANDLER_PROTOTYPE(type_id_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_son_handler);
HANDLER_PROTOTYPE(prefix_with_token_text_then_son_handler);
HANDLER_PROTOTYPE(braced_initializer_handler);
HANDLER_PROTOTYPE(pointer_spec_handler);
HANDLER_PROTOTYPE(decl_specifier_sequence);
HANDLER_PROTOTYPE(binary_operator_handler);
HANDLER_PROTOTYPE(throw_expression_handler);
HANDLER_PROTOTYPE(conditional_expression_handler);
HANDLER_PROTOTYPE(cast_expression_handler);
HANDLER_PROTOTYPE(sizeof_typeid_handler);
HANDLER_PROTOTYPE(new_expression_handler);
HANDLER_PROTOTYPE(new_type_id_handler);
HANDLER_PROTOTYPE(new_type_id_expr_handler);
HANDLER_PROTOTYPE(new_initializer_handler);
HANDLER_PROTOTYPE(delete_expression_handler);
HANDLER_PROTOTYPE(array_subscript_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(typename_explicit_type_conversion);
HANDLER_PROTOTYPE(typename_template_explicit_type_conversion_handler);
HANDLER_PROTOTYPE(typename_template_template_explicit_type_conversion_handler);
HANDLER_PROTOTYPE(infix_parameter_handler);
HANDLER_PROTOTYPE(template_member_access);
HANDLER_PROTOTYPE(son_handler_then_suffix_parameter);
HANDLER_PROTOTYPE(templated_cast_handler);
HANDLER_PROTOTYPE(qualified_id_handler);
HANDLER_PROTOTYPE(qualified_template_handler);
HANDLER_PROTOTYPE(conversion_type_id_handler);
HANDLER_PROTOTYPE(conversion_declarator_handler);
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
HANDLER_PROTOTYPE(simple_type_specifier_handler);
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
HANDLER_PROTOTYPE(constant_initializer_handler);
HANDLER_PROTOTYPE(base_clause_handler);
HANDLER_PROTOTYPE(base_specifier_handler);
HANDLER_PROTOTYPE(base_specifier_access_handler);
HANDLER_PROTOTYPE(base_specifier_virtual_handler);
HANDLER_PROTOTYPE(base_specifier_access_virtual_handler);
HANDLER_PROTOTYPE(elaborated_type_class_handler);
HANDLER_PROTOTYPE(elaborated_type_template_class_handler);
HANDLER_PROTOTYPE(elaborated_type_template_template_class_handler);
HANDLER_PROTOTYPE(elaborated_type_enum_handler);
HANDLER_PROTOTYPE(elaborated_typename_handler);
HANDLER_PROTOTYPE(elaborated_typename_template_handler);
HANDLER_PROTOTYPE(if_else_statement_handler);
HANDLER_PROTOTYPE(exception_specification_handler);
HANDLER_PROTOTYPE(operator_function_id_handler);
HANDLER_PROTOTYPE(asm_definition_handler);
HANDLER_PROTOTYPE(bitfield_declarator_handler);
HANDLER_PROTOTYPE(enum_specifier_handler);
HANDLER_PROTOTYPE(enum_def_handler);
HANDLER_PROTOTYPE(explicit_instantiation_handler);
HANDLER_PROTOTYPE(explicit_specialization_handler);
HANDLER_PROTOTYPE(linkage_specification_handler);
HANDLER_PROTOTYPE(linkage_specification_decl_handler);
HANDLER_PROTOTYPE(namespace_alias_definition_handler);
HANDLER_PROTOTYPE(qualified_namespace_spec_handler);
HANDLER_PROTOTYPE(using_directive_handler);
HANDLER_PROTOTYPE(namespace_definition_handler);
HANDLER_PROTOTYPE(pseudo_destructor_name_handler);
HANDLER_PROTOTYPE(pseudo_destructor_template_name_handler);
HANDLER_PROTOTYPE(parenthesized_initializer_handler);
HANDLER_PROTOTYPE(unknown_pragma_handler);
HANDLER_PROTOTYPE(kr_parameter_list_handler);
HANDLER_PROTOTYPE(designated_initializer_handler);
HANDLER_PROTOTYPE(designation_handler);
HANDLER_PROTOTYPE(index_designator_handler);
HANDLER_PROTOTYPE(field_designator_handler);

HANDLER_PROTOTYPE(decltype_handler);

HANDLER_PROTOTYPE(pp_comment_handler);
HANDLER_PROTOTYPE(pp_prepro_token_handler);

// Pragma custom support
HANDLER_PROTOTYPE(pragma_custom_directive_handler);
HANDLER_PROTOTYPE(pragma_custom_construct_handler);
HANDLER_PROTOTYPE(pragma_custom_clause_handler);
HANDLER_PROTOTYPE(pragma_custom_line_handler);

// Custom construct
HANDLER_PROTOTYPE(custom_construct_statement_handler);
HANDLER_PROTOTYPE(custom_construct_header_handler);
HANDLER_PROTOTYPE(custom_construct_parameter);

// OpenMP
HANDLER_PROTOTYPE(omp_generic_construct_handler);
HANDLER_PROTOTYPE(omp_generic_clause_handler_with_expression);
HANDLER_PROTOTYPE(omp_generic_directive_handler);
HANDLER_PROTOTYPE(omp_generic_clause_handler_with_list);
HANDLER_PROTOTYPE(omp_schedule_clause_handler);
HANDLER_PROTOTYPE(omp_reduction_clause_handler);
HANDLER_PROTOTYPE(omp_sections_construct_handler);
HANDLER_PROTOTYPE(omp_section_handler);
HANDLER_PROTOTYPE(omp_threadprivate_directive_handler);
HANDLER_PROTOTYPE(omp_custom_directive_handler);
HANDLER_PROTOTYPE(omp_custom_construct_directive_handler);
HANDLER_PROTOTYPE(omp_custom_clause_handler);
HANDLER_PROTOTYPE(omp_custom_parameter_clause_handler);
HANDLER_PROTOTYPE(omp_critical_directive_handler);
HANDLER_PROTOTYPE(omp_default_custom_clause_handler);

// GCC Extensions
HANDLER_PROTOTYPE(gcc_label_declaration_handler);
HANDLER_PROTOTYPE(gcc_attribute_handler);
HANDLER_PROTOTYPE(gcc_attribute_value_handler);
HANDLER_PROTOTYPE(gcc_asm_definition_handler);
HANDLER_PROTOTYPE(gcc_asm_def_parameters);
HANDLER_PROTOTYPE(gcc_asm_operand_handler);
HANDLER_PROTOTYPE(gcc_type_spec_sequence_handler);
HANDLER_PROTOTYPE(gcc_typeof_handler);
HANDLER_PROTOTYPE(gcc_typeof_expr_handler);
HANDLER_PROTOTYPE(gcc_alignof_type_handler);
HANDLER_PROTOTYPE(gcc_elaborated_type_class_handler);
HANDLER_PROTOTYPE(gcc_elaborated_type_template_handler);
HANDLER_PROTOTYPE(gcc_elaborated_type_template_template_handler);
HANDLER_PROTOTYPE(gcc_elaborated_type_enum_handler);
HANDLER_PROTOTYPE(gcc_init_declarator_handler);
HANDLER_PROTOTYPE(gcc_asm_specification_handler);
HANDLER_PROTOTYPE(gcc_declarator_handler);
HANDLER_PROTOTYPE(gcc_pointer_declarator_handler);
HANDLER_PROTOTYPE(gcc_reference_spec_handler);
HANDLER_PROTOTYPE(gcc_enum_specifier_handler);
HANDLER_PROTOTYPE(gcc_abstract_declarator_handler);
HANDLER_PROTOTYPE(gcc_ptr_abstract_declarator_handler);
HANDLER_PROTOTYPE(gcc_initializer_clause_handler);
HANDLER_PROTOTYPE(gcc_class_head_handler);
HANDLER_PROTOTYPE(gcc_member_declarator_handler);
HANDLER_PROTOTYPE(gcc_bitfield_declarator_handler);
HANDLER_PROTOTYPE(gcc_case_statement_handler);
HANDLER_PROTOTYPE(gcc_condition_handler);
HANDLER_PROTOTYPE(gcc_goto_statement_handler);
HANDLER_PROTOTYPE(gcc_mem_initializer_handler);
HANDLER_PROTOTYPE(gcc_builtin_va_arg_handler);
HANDLER_PROTOTYPE(gcc_builtin_offsetof_handler);
HANDLER_PROTOTYPE(gcc_builtin_choose_expr_handler);
HANDLER_PROTOTYPE(gcc_builtin_types_compatible_p_handler);
HANDLER_PROTOTYPE(gcc_postfix_expression);
HANDLER_PROTOTYPE(gcc_conditional_expression);
HANDLER_PROTOTYPE(gcc_extension_preffix_handler);
HANDLER_PROTOTYPE(gcc_parameter_decl_handler);
HANDLER_PROTOTYPE(gcc_using_directive_handler);
HANDLER_PROTOTYPE(gcc_namespace_definition_handler);
HANDLER_PROTOTYPE(gcc_functional_declarator_handler);
HANDLER_PROTOTYPE(gcc_offsetof_member_designator_handler);
HANDLER_PROTOTYPE(gcc_parenthesized_expression_handler);

HANDLER_PROTOTYPE(gxx_type_traits);

// Mercurium extensions
HANDLER_PROTOTYPE(array_section_handler);

prettyprint_entry_t handlers_list[] =
{
    NODE_HANDLER(AST_TRANSLATION_UNIT, unary_container_handler, NULL),
    NODE_HANDLER(AST_AMBIGUITY, ambiguity_handler, NULL),
    NODE_HANDLER(AST_NODE_LIST, sequence_handler, NULL),
    NODE_HANDLER(AST_SIMPLE_DECLARATION, simple_declaration_handler, NULL),
    NODE_HANDLER(AST_EMPTY_DECL, indented_simple_parameter_handler, ";\n"),
    NODE_HANDLER(AST_INIT_DECLARATOR, init_declarator_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR, unary_container_handler, NULL),
    NODE_HANDLER(AST_POINTER_DECL, pointer_decl_handler, NULL),
    NODE_HANDLER(AST_PARENTHESIZED_DECLARATOR, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_ID_EXPR, unary_container_handler, NULL),
    NODE_HANDLER(AST_GLOBAL_SCOPE, simple_parameter_handler, "::"),
    NODE_HANDLER(AST_NESTED_NAME_SPECIFIER, nested_name_handler, NULL),
    NODE_HANDLER(AST_NESTED_NAME_SPECIFIER_TEMPLATE, nested_name_handler, NULL),
    NODE_HANDLER(AST_SYMBOL, simple_text_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_ID, template_id_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_EXPRESSION_ARGUMENT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_TYPE_ARGUMENT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TYPE_ID, type_id_handler, NULL),
    NODE_HANDLER(AST_ABSTRACT_DECLARATOR, abstract_declarator_handler, NULL),
    NODE_HANDLER(AST_PARENTHESIZED_ABSTRACT_DECLARATOR, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_ABSTRACT_ARRAY, abstract_array_declarator_handler, NULL),
    NODE_HANDLER(AST_ABSTRACT_DECLARATOR_FUNC, abstract_declarator_function_handler, NULL),
    NODE_HANDLER(AST_VARIADIC_ARG, simple_parameter_handler, "..."),
    NODE_HANDLER(AST_EMPTY_PARAMETER_DECLARATION_CLAUSE, null_handler, NULL),
    NODE_HANDLER(AST_PARAMETER_DECL, parameter_decl_handler, NULL),
    NODE_HANDLER(AST_INITIALIZER, prefix_with_parameter_then_son_handler, " = "),
    NODE_HANDLER(AST_PARENTHESIZED_INITIALIZER, parenthesized_initializer_handler, NULL),
    NODE_HANDLER(AST_INITIALIZER_EXPR, unary_container_handler, NULL),
    NODE_HANDLER(AST_INITIALIZER_BRACES, braced_initializer_handler, NULL),
    NODE_HANDLER(AST_POINTER_SPEC, pointer_spec_handler, NULL),
    NODE_HANDLER(AST_REFERENCE_SPEC, simple_parameter_handler, "&"),
    NODE_HANDLER(AST_RVALUE_REFERENCE_SPEC, simple_parameter_handler, "&&"),
    NODE_HANDLER(AST_CONST_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_VOLATILE_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_FUNC, abstract_declarator_function_handler, NULL),
    NODE_HANDLER(AST_DECLARATOR_ARRAY, abstract_array_declarator_handler, NULL),
    NODE_HANDLER(AST_DECL_SPECIFIER_SEQ, decl_specifier_sequence, NULL),
    NODE_HANDLER(AST_TYPE_SPECIFIER_SEQ, decl_specifier_sequence, NULL),
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
    NODE_HANDLER(AST_COMMA_OP, binary_operator_handler, ","),
    NODE_HANDLER(AST_CONDITIONAL_EXPRESSION, conditional_expression_handler, NULL),
    NODE_HANDLER(AST_ASSIGNMENT, binary_operator_handler, "="),
    NODE_HANDLER(AST_MUL_ASSIGNMENT, binary_operator_handler, "*="),
    NODE_HANDLER(AST_DIV_ASSIGNMENT, binary_operator_handler, "/="),
    NODE_HANDLER(AST_ADD_ASSIGNMENT, binary_operator_handler, "+="),
    NODE_HANDLER(AST_SUB_ASSIGNMENT, binary_operator_handler, "-="),
    NODE_HANDLER(AST_SHL_ASSIGNMENT, binary_operator_handler, "<<="),
    NODE_HANDLER(AST_SHR_ASSIGNMENT, binary_operator_handler, ">>="),
    NODE_HANDLER(AST_AND_ASSIGNMENT, binary_operator_handler, "&="),
    NODE_HANDLER(AST_OR_ASSIGNMENT, binary_operator_handler, "|="),
    NODE_HANDLER(AST_XOR_ASSIGNMENT, binary_operator_handler, "^="),
    NODE_HANDLER(AST_MOD_ASSIGNMENT, binary_operator_handler, "%="),
    NODE_HANDLER(AST_THROW_EXPRESSION, throw_expression_handler, NULL),
    NODE_HANDLER(AST_LOGICAL_OR, binary_operator_handler, "||"),
    NODE_HANDLER(AST_LOGICAL_AND, binary_operator_handler, "&&"),
    NODE_HANDLER(AST_BITWISE_OR, binary_operator_handler, "|"),
    NODE_HANDLER(AST_BITWISE_AND, binary_operator_handler, "&"),
    NODE_HANDLER(AST_BITWISE_XOR, binary_operator_handler, "^"),
    NODE_HANDLER(AST_EQUAL_OP, binary_operator_handler, "=="),
    NODE_HANDLER(AST_DIFFERENT_OP, binary_operator_handler, "!="),
    NODE_HANDLER(AST_LOWER_THAN, binary_operator_handler, "<"),
    NODE_HANDLER(AST_GREATER_THAN, binary_operator_handler, ">"),
    NODE_HANDLER(AST_GREATER_OR_EQUAL_THAN, binary_operator_handler, ">="),
    NODE_HANDLER(AST_LOWER_OR_EQUAL_THAN, binary_operator_handler, "<="),
    NODE_HANDLER(AST_SHL_OP, binary_operator_handler, "<<"),
    NODE_HANDLER(AST_SHR_OP, binary_operator_handler, ">>"),
    NODE_HANDLER(AST_MINUS_OP, binary_operator_handler, "-"),
    NODE_HANDLER(AST_ADD_OP, binary_operator_handler, "+"),
    NODE_HANDLER(AST_DIV_OP, binary_operator_handler, "/"),
    NODE_HANDLER(AST_MOD_OP, binary_operator_handler, "%"),
    NODE_HANDLER(AST_MULT_OP, binary_operator_handler, "*"),
    NODE_HANDLER(AST_POINTER_TO_MEMBER, binary_operator_handler, ".*"),
    NODE_HANDLER(AST_POINTER_TO_POINTER_MEMBER, binary_operator_handler, "->*"),
    NODE_HANDLER(AST_CAST_EXPRESSION, cast_expression_handler, NULL),
    NODE_HANDLER(AST_PREDECREMENT, prefix_with_parameter_then_son_handler, "--"),
    NODE_HANDLER(AST_PREINCREMENT, prefix_with_parameter_then_son_handler, "++"),
    NODE_HANDLER(AST_DERREFERENCE, prefix_with_parameter_then_son_handler, "*"),
    NODE_HANDLER(AST_REFERENCE, prefix_with_parameter_then_son_handler, "&"),
    NODE_HANDLER(AST_PLUS_OP, prefix_with_parameter_then_son_handler, "+"),
    NODE_HANDLER(AST_NEG_OP, prefix_with_parameter_then_son_handler, "-"),
    NODE_HANDLER(AST_NOT_OP, prefix_with_parameter_then_son_handler, "!"),
    NODE_HANDLER(AST_COMPLEMENT_OP, prefix_with_parameter_then_son_handler, "~"),
    NODE_HANDLER(AST_SIZEOF, prefix_with_parameter_then_son_handler, "sizeof "),
    NODE_HANDLER(AST_SIZEOF_TYPEID, sizeof_typeid_handler, "sizeof"),
    NODE_HANDLER(AST_NEW_EXPRESSION, new_expression_handler, NULL),
    NODE_HANDLER(AST_NEW_TYPE_ID, new_type_id_handler, NULL),
    NODE_HANDLER(AST_NEW_TYPE_ID_EXPR, new_type_id_expr_handler, NULL),
    NODE_HANDLER(AST_NEW_DECLARATOR, new_type_id_handler, NULL),
    NODE_HANDLER(AST_DIRECT_NEW_DECLARATOR, abstract_array_declarator_handler, NULL),
    NODE_HANDLER(AST_NEW_INITIALIZER, new_initializer_handler, NULL),
    NODE_HANDLER(AST_DELETE_EXPR, delete_expression_handler, NULL),
    NODE_HANDLER(AST_DELETE_ARRAY_EXPR, delete_expression_handler, NULL),
    NODE_HANDLER(AST_ARRAY_SUBSCRIPT, array_subscript_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_CALL, function_call_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_TYPE_CONVERSION, function_call_handler, NULL),
    NODE_HANDLER(AST_TYPENAME_EXPLICIT_TYPE_CONVERSION, typename_explicit_type_conversion, NULL),
    NODE_HANDLER(AST_TYPENAME_TEMPLATE_EXPLICIT_TYPE_CONVERSION, typename_template_explicit_type_conversion_handler, NULL),
    NODE_HANDLER(AST_TYPENAME_TEMPLATE_TEMPLATE_EXPLICIT_TYPE_CONVERSION, typename_template_template_explicit_type_conversion_handler, NULL),
    NODE_HANDLER(AST_CLASS_MEMBER_ACCESS, infix_parameter_handler, "."),
    NODE_HANDLER(AST_POINTER_CLASS_MEMBER_ACCESS, infix_parameter_handler, "->"),
    NODE_HANDLER(AST_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "."),
    NODE_HANDLER(AST_POINTER_CLASS_TEMPLATE_MEMBER_ACCESS, template_member_access, "->"),
    NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_CALL, infix_parameter_handler, "."),
    NODE_HANDLER(AST_POINTER_PSEUDO_DESTRUCTOR_CALL, infix_parameter_handler, "->"),
    NODE_HANDLER(AST_POSTINCREMENT, son_handler_then_suffix_parameter, "++"),
    NODE_HANDLER(AST_POSTDECREMENT, son_handler_then_suffix_parameter, "--"),
    NODE_HANDLER(AST_DYNAMIC_CAST, templated_cast_handler, "dynamic_cast"),
    NODE_HANDLER(AST_STATIC_CAST, templated_cast_handler, "static_cast"),
    NODE_HANDLER(AST_REINTERPRET_CAST, templated_cast_handler, "reinterpret_cast"),
    NODE_HANDLER(AST_CONST_CAST, templated_cast_handler, "const_cast"),
    NODE_HANDLER(AST_TYPEID_EXPR, sizeof_typeid_handler, "typeid"),
    NODE_HANDLER(AST_TYPEID_TYPE, sizeof_typeid_handler, "typeid"),
    NODE_HANDLER(AST_THIS_VARIABLE, simple_parameter_handler, "this"),
    NODE_HANDLER(AST_PARENTHESIZED_EXPRESSION, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_QUALIFIED_ID, qualified_id_handler, NULL),
    NODE_HANDLER(AST_QUALIFIED_TEMPLATE, qualified_template_handler, NULL),
    NODE_HANDLER(AST_DESTRUCTOR_ID, unary_container_handler, NULL),
    NODE_HANDLER(AST_DESTRUCTOR_TEMPLATE_ID, prefix_with_parameter_then_son_handler, "~"),
    NODE_HANDLER(AST_CONVERSION_FUNCTION_ID, prefix_with_parameter_then_son_handler, "operator "),
    NODE_HANDLER(AST_CONVERSION_TYPE_ID, conversion_type_id_handler, NULL),
    NODE_HANDLER(AST_CONVERSION_DECLARATOR, conversion_declarator_handler, NULL),
    NODE_HANDLER(AST_CTOR_INITIALIZER, constructor_initializer_handler, NULL),
    NODE_HANDLER(AST_MEM_INITIALIZER, mem_initializer_handler, NULL),
    NODE_HANDLER(AST_MEM_INITIALIZER_ID, mem_initializer_id_handler, NULL),
    NODE_HANDLER(AST_CLASS_SPECIFIER, class_specifier_handler, NULL),
    NODE_HANDLER(AST_CLASS_KEY_CLASS, simple_parameter_handler, "class"),
    NODE_HANDLER(AST_CLASS_KEY_STRUCT, simple_parameter_handler, "struct"),
    NODE_HANDLER(AST_CLASS_KEY_UNION, simple_parameter_handler, "union"),
    NODE_HANDLER(AST_CLASS_HEAD, class_head_handler, NULL),
    NODE_HANDLER(AST_MEMBER_ACCESS_SPEC, member_access_specifier_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATION_QUALIFIED, qualified_id_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATION_TEMPLATE, qualified_template_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATION, member_declaration_handler, NULL),
    NODE_HANDLER(AST_MEMBER_DECLARATOR, member_declarator_handler, NULL),
    NODE_HANDLER(AST_CONSTANT_INITIALIZER, constant_initializer_handler, NULL),
    NODE_HANDLER(AST_USING_DECL, using_declaration_handler, NULL),
    NODE_HANDLER(AST_USING_DECL_TYPENAME, using_declaration_handler, NULL),
    NODE_HANDLER(AST_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
    NODE_HANDLER(AST_EXPORT_TEMPLATE_DECLARATION, template_declaration_handler, NULL),
    NODE_HANDLER(AST_TYPE_PARAMETER_CLASS, type_parameter_class_or_typename_handler, "class"),
    NODE_HANDLER(AST_TYPE_PARAMETER_TYPENAME, type_parameter_class_or_typename_handler, "typename"),
    NODE_HANDLER(AST_TYPE_PARAMETER_TEMPLATE, type_parameter_template_handler, NULL),
    NODE_HANDLER(AST_SIMPLE_TYPE_SPECIFIER, simple_type_specifier_handler, NULL),
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
    NODE_HANDLER(AST_BASE_SPECIFIER, base_specifier_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_TEMPLATE, base_specifier_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS, base_specifier_access_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS_TEMPLATE, base_specifier_access_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_VIRTUAL, base_specifier_virtual_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_VIRTUAL_TEMPLATE, base_specifier_virtual_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS_VIRTUAL, base_specifier_access_virtual_handler, NULL),
    NODE_HANDLER(AST_BASE_SPECIFIER_ACCESS_VIRTUAL_TEMPLATE, base_specifier_access_virtual_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_CLASS, elaborated_type_class_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_TEMPLATE_CLASS, elaborated_type_template_class_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, elaborated_type_template_template_class_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPE_ENUM, elaborated_type_enum_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPENAME, elaborated_typename_handler, NULL),
    NODE_HANDLER(AST_ELABORATED_TYPENAME_TEMPLATE, elaborated_typename_template_handler, NULL),
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
    NODE_HANDLER(AST_MULT_OPERATOR, simple_parameter_handler, "*"),
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
    NODE_HANDLER(AST_XOR_ASSIGN_OPERATOR, simple_parameter_handler, "^="),
    NODE_HANDLER(AST_OR_ASSIGN_OPERATOR, simple_parameter_handler,  "|="),
    NODE_HANDLER(AST_AND_ASSIGN_OPERATOR, simple_parameter_handler,  "&="),
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
    NODE_HANDLER(AST_AUTO_SPEC, simple_parameter_handler, "auto"),
    NODE_HANDLER(AST_REGISTER_SPEC, simple_parameter_handler, "register"),
    NODE_HANDLER(AST_EXTERN_SPEC, simple_parameter_handler, "extern"),
    NODE_HANDLER(AST_MUTABLE_SPEC, simple_parameter_handler, "mutable"),
    NODE_HANDLER(AST_THREAD_SPEC, simple_parameter_handler, "__thread"),
    NODE_HANDLER(AST_BITFIELD_DECLARATOR, bitfield_declarator_handler, NULL),
    NODE_HANDLER(AST_ENUM_SPECIFIER, enum_specifier_handler, NULL),
    NODE_HANDLER(AST_ENUM_DEF, enum_def_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_INSTANTIATION, explicit_instantiation_handler, NULL),
    NODE_HANDLER(AST_EXPLICIT_SPECIALIZATION, explicit_specialization_handler, NULL),
    NODE_HANDLER(AST_LINKAGE_SPEC, linkage_specification_handler, NULL),
    NODE_HANDLER(AST_LINKAGE_SPEC_DECL, linkage_specification_decl_handler, NULL),
    NODE_HANDLER(AST_NAMESPACE_ALIAS, namespace_alias_definition_handler, NULL),
    NODE_HANDLER(AST_QUALIFIED_NAMESPACE_SPEC, qualified_namespace_spec_handler, NULL),
    NODE_HANDLER(AST_USING_DIRECTIVE, using_directive_handler, NULL),
    NODE_HANDLER(AST_NAMESPACE_DEFINITION, namespace_definition_handler, NULL),
    NODE_HANDLER(AST_NEW_PLACEMENT, parenthesized_son_handler, NULL),
    NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_NAME, pseudo_destructor_name_handler, NULL),
    NODE_HANDLER(AST_PSEUDO_DESTRUCTOR_NAME_TEMPLATE, pseudo_destructor_template_name_handler, NULL),
    NODE_HANDLER(AST_KR_PARAMETER_LIST, kr_parameter_list_handler, NULL),
    NODE_HANDLER(AST_DESIGNATED_INITIALIZER, designated_initializer_handler, NULL),
    NODE_HANDLER(AST_DESIGNATION, designation_handler, NULL),
    NODE_HANDLER(AST_INDEX_DESIGNATOR, index_designator_handler, NULL),
    NODE_HANDLER(AST_FIELD_DESIGNATOR, field_designator_handler, NULL),
    NODE_HANDLER(AST_UNKNOWN_PRAGMA, unknown_pragma_handler, NULL),
    NODE_HANDLER(AST_PP_COMMENT, pp_comment_handler, NULL),
    NODE_HANDLER(AST_PP_TOKEN, pp_prepro_token_handler, NULL),
    // Pragma custom
    NODE_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, pragma_custom_directive_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, pragma_custom_construct_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CLAUSE, pragma_custom_clause_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_LINE, pragma_custom_line_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CLAUSE_ARG, simple_text_handler, NULL),
    // Custom code constructs
    NODE_HANDLER(AST_CUSTOM_CONSTRUCT_STATEMENT, custom_construct_statement_handler, NULL),
    NODE_HANDLER(AST_CUSTOM_CONSTRUCT_HEADER, custom_construct_header_handler, NULL),
    NODE_HANDLER(AST_CUSTOM_CONSTRUCT_PARAMETER, custom_construct_parameter, NULL),
    // OpenMP 2.5
    NODE_HANDLER(AST_OMP_PARALLEL_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_PARALLEL_DIRECTIVE, omp_generic_directive_handler, "parallel"),
    NODE_HANDLER(AST_OMP_FOR_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_FOR_DIRECTIVE, omp_generic_directive_handler, "for"),
    NODE_HANDLER(AST_OMP_PARALLEL_SINGLE_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_PARALLEL_SINGLE_DIRECTIVE, omp_generic_directive_handler, "parallel single"),
    NODE_HANDLER(AST_OMP_PARALLEL_FOR_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_PARALLEL_FOR_DIRECTIVE, omp_generic_directive_handler, "parallel for"),
    NODE_HANDLER(AST_OMP_PARALLEL_SECTIONS_CONSTRUCT, omp_sections_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_PARALLEL_SECTIONS_DIRECTIVE, omp_generic_directive_handler, "parallel sections"),
    NODE_HANDLER(AST_OMP_SECTIONS_CONSTRUCT, omp_sections_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_SECTIONS_DIRECTIVE, omp_generic_directive_handler, "sections"),
    NODE_HANDLER(AST_OMP_SECTION, omp_section_handler, NULL),
    NODE_HANDLER(AST_OMP_SECTION_DIRECTIVE, omp_generic_directive_handler, "section"),
    NODE_HANDLER(AST_OMP_SINGLE_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_SINGLE_DIRECTIVE, omp_generic_directive_handler, "single"),
    NODE_HANDLER(AST_OMP_MASTER_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_MASTER_DIRECTIVE, omp_generic_directive_handler, "master"),
    NODE_HANDLER(AST_OMP_ATOMIC_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_ATOMIC_DIRECTIVE, omp_generic_directive_handler, "atomic"),
    NODE_HANDLER(AST_OMP_ORDERED_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_ORDERED_DIRECTIVE, omp_generic_directive_handler, "ordered"),
    NODE_HANDLER(AST_OMP_CUSTOM_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_CUSTOM_DIRECTIVE, omp_custom_directive_handler, NULL),
    NODE_HANDLER(AST_OMP_CUSTOM_CONSTRUCT_DIRECTIVE, omp_custom_construct_directive_handler, NULL),
    NODE_HANDLER(AST_OMP_CUSTOM_CLAUSE, omp_custom_clause_handler, NULL),
    NODE_HANDLER(AST_OMP_CUSTOM_PARAMETER_CLAUSE, omp_custom_parameter_clause_handler, NULL),
    NODE_HANDLER(AST_OMP_BARRIER_DIRECTIVE, omp_generic_directive_handler, "barrier"),
    NODE_HANDLER(AST_OMP_FLUSH_DIRECTIVE, omp_generic_directive_handler, "flush"),
    NODE_HANDLER(AST_OMP_IF_CLAUSE, omp_generic_clause_handler_with_expression, "if"),
    NODE_HANDLER(AST_OMP_NUM_THREADS_CLAUSE, omp_generic_clause_handler_with_expression, "num_threads"),
    NODE_HANDLER(AST_OMP_NOWAIT_CLAUSE, simple_parameter_handler, "nowait"),
    NODE_HANDLER(AST_OMP_ORDERED_CLAUSE, simple_parameter_handler, "ordered"),
    NODE_HANDLER(AST_OMP_SCHEDULE_CLAUSE, omp_schedule_clause_handler, NULL),
    NODE_HANDLER(AST_OMP_CUSTOM_SCHEDULE, simple_text_handler, NULL),
    NODE_HANDLER(AST_OMP_STATIC_SCHEDULE, simple_parameter_handler, "static"),
    NODE_HANDLER(AST_OMP_DYNAMIC_SCHEDULE, simple_parameter_handler, "dynamic"),
    NODE_HANDLER(AST_OMP_RUNTIME_SCHEDULE, simple_parameter_handler, "runtime"),
    NODE_HANDLER(AST_OMP_GUIDED_SCHEDULE, simple_parameter_handler, "guided"),
    NODE_HANDLER(AST_OMP_SHARED_CLAUSE, omp_generic_clause_handler_with_list, "shared"),
    NODE_HANDLER(AST_OMP_PRIVATE_CLAUSE, omp_generic_clause_handler_with_list, "private"),
    NODE_HANDLER(AST_OMP_FIRSTPRIVATE_CLAUSE, omp_generic_clause_handler_with_list, "firstprivate"),
    NODE_HANDLER(AST_OMP_LASTPRIVATE_CLAUSE, omp_generic_clause_handler_with_list, "lastprivate"),
    NODE_HANDLER(AST_OMP_COPYPRIVATE_CLAUSE, omp_generic_clause_handler_with_list, "copyprivate"),
    NODE_HANDLER(AST_OMP_COPYIN_CLAUSE, omp_generic_clause_handler_with_list, "copyin"),
    NODE_HANDLER(AST_OMP_DEFAULT_SHARED_CLAUSE, simple_parameter_handler, "default(shared)"),
    NODE_HANDLER(AST_OMP_DEFAULT_NONE_CLAUSE, simple_parameter_handler, "default(none)"),
    NODE_HANDLER(AST_OMP_DEFAULT_CUSTOM_CLAUSE, omp_default_custom_clause_handler, NULL),
    NODE_HANDLER(AST_OMP_REDUCTION_CLAUSE, omp_reduction_clause_handler, NULL),
    NODE_HANDLER(AST_OMP_THREADPRIVATE_DIRECTIVE, omp_threadprivate_directive_handler, NULL),
    NODE_HANDLER(AST_OMP_CRITICAL_CONSTRUCT, omp_generic_construct_handler, NULL),
    NODE_HANDLER(AST_OMP_CRITICAL_DIRECTIVE, omp_critical_directive_handler, NULL),
    // GCC Extensions
    NODE_HANDLER(AST_GCC_EXTENSION, gcc_extension_preffix_handler, "__extension__ "),
    NODE_HANDLER(AST_GCC_EXTENSION_EXPR, prefix_with_token_text_then_son_handler, NULL),
    NODE_HANDLER(AST_GCC_LABEL_DECL, gcc_label_declaration_handler, NULL),
    NODE_HANDLER(AST_GCC_ATTRIBUTE, gcc_attribute_handler, NULL),
    NODE_HANDLER(AST_GCC_ATTRIBUTE_EXPR, gcc_attribute_value_handler, NULL),
    NODE_HANDLER(AST_GCC_ASM_DEFINITION, gcc_asm_definition_handler, NULL),
    NODE_HANDLER(AST_GCC_ASM_DEF_PARMS, gcc_asm_def_parameters, NULL),
    NODE_HANDLER(AST_GCC_ASM_OPERAND, gcc_asm_operand_handler, NULL),
    NODE_HANDLER(AST_GCC_COMPLEX_TYPE, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_IMAGINARY_TYPE, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_TYPE_SPECIFIER_SEQ, gcc_type_spec_sequence_handler, NULL),
    NODE_HANDLER(AST_GCC_TYPEOF, gcc_typeof_handler, NULL),
    NODE_HANDLER(AST_GCC_TYPEOF_EXPR, gcc_typeof_expr_handler, NULL),
    NODE_HANDLER(AST_GCC_RESTRICT_SPEC, simple_text_handler, NULL),
    NODE_HANDLER(AST_GCC_PARENTHESIZED_EXPRESSION, gcc_parenthesized_expression_handler, NULL),
    NODE_HANDLER(AST_GCC_REAL_PART, prefix_with_parameter_then_son_handler, "__real__ "),
    NODE_HANDLER(AST_GCC_IMAG_PART, prefix_with_parameter_then_son_handler, "__imag__ "),
    NODE_HANDLER(AST_GCC_ALIGNOF, prefix_with_token_text_then_son_handler, NULL),
    NODE_HANDLER(AST_GCC_ALIGNOF_TYPE, gcc_alignof_type_handler, NULL),
    NODE_HANDLER(AST_GCC_LABEL_ADDR, prefix_with_parameter_then_son_handler, "&&"),
    NODE_HANDLER(AST_GCC_ELABORATED_TYPE_CLASS, gcc_elaborated_type_class_handler, NULL),
    NODE_HANDLER(AST_GCC_ELABORATED_TYPE_TEMPLATE_CLASS, gcc_elaborated_type_template_handler, NULL),
    NODE_HANDLER(AST_GCC_ELABORATED_TYPE_TEMPLATE_TEMPLATE_CLASS, gcc_elaborated_type_template_template_handler, NULL),
    NODE_HANDLER(AST_GCC_ELABORATED_TYPE_ENUM, gcc_elaborated_type_enum_handler, NULL),
    NODE_HANDLER(AST_GCC_INIT_DECLARATOR, gcc_init_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_ASM_SPEC, gcc_asm_specification_handler, NULL),
    NODE_HANDLER(AST_GCC_DECLARATOR, gcc_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_POINTER_DECL, gcc_pointer_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_REFERENCE_SPEC, gcc_reference_spec_handler, NULL),
    NODE_HANDLER(AST_GCC_ENUM_SPECIFIER, gcc_enum_specifier_handler, NULL),
    NODE_HANDLER(AST_GCC_ABSTRACT_DECLARATOR, gcc_abstract_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_PTR_ABSTRACT_DECLARATOR, gcc_ptr_abstract_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_INITIALIZER_CLAUSE, gcc_initializer_clause_handler, NULL),
    NODE_HANDLER(AST_GCC_CLASS_HEAD, gcc_class_head_handler, NULL),
    NODE_HANDLER(AST_GCC_MEMBER_DECLARATOR, gcc_member_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_BITFIELD_DECLARATOR, gcc_bitfield_declarator_handler, NULL),
    NODE_HANDLER(AST_GCC_CASE_STATEMENT, gcc_case_statement_handler, NULL),
    NODE_HANDLER(AST_GCC_CONDITION, gcc_condition_handler, NULL),
    NODE_HANDLER(AST_GCC_GOTO_STATEMENT, gcc_goto_statement_handler, NULL),
    NODE_HANDLER(AST_GCC_MEM_INITIALIZER, gcc_mem_initializer_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_VA_ARG, gcc_builtin_va_arg_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_OFFSETOF, gcc_builtin_offsetof_handler, NULL),
    NODE_HANDLER(AST_GCC_OFFSETOF_MEMBER_DESIGNATOR , gcc_offsetof_member_designator_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_CHOOSE_EXPR, gcc_builtin_choose_expr_handler, NULL),
    NODE_HANDLER(AST_GCC_BUILTIN_TYPES_COMPATIBLE_P, gcc_builtin_types_compatible_p_handler, NULL),
    NODE_HANDLER(AST_GCC_POSTFIX_EXPRESSION, gcc_postfix_expression, NULL),
    NODE_HANDLER(AST_GCC_CONDITIONAL_EXPRESSION, gcc_conditional_expression, NULL),
    NODE_HANDLER(AST_GCC_PARAMETER_DECL, gcc_parameter_decl_handler, NULL),
    NODE_HANDLER(AST_GCC_USING_DIRECTIVE, gcc_using_directive_handler, NULL),
    NODE_HANDLER(AST_GCC_NAMESPACE_DEFINITION, gcc_namespace_definition_handler, NULL),
    NODE_HANDLER(AST_GCC_FUNCTIONAL_DECLARATOR, gcc_functional_declarator_handler, NULL), 
    NODE_HANDLER(AST_GXX_TYPE_TRAITS, gxx_type_traits, NULL),
    // Mercurium extension
    NODE_HANDLER(AST_ARRAY_SECTION, array_section_handler, NULL),
};

typedef
struct prettyprint_behaviour_tag
{
    // States if the output is meant to be internal
    // i.e.: comments and preprocessor tokens will be output with special
    // keeping marks or will be converted to standard syntax
    char internal_output;
} prettyprint_behaviour_t;

// Initial behaviour
static prettyprint_behaviour_t prettyprint_behaviour = 
{ 
    /* .internal_output = */ 1 
};

void prettyprint_set_not_internal_output(void)
{
    prettyprint_behaviour.internal_output = 0;
}

void prettyprint_set_internal_output(void)
{
    prettyprint_behaviour.internal_output = 1;
}

static void prettyprint_level(FILE* f, AST a, int level);

#define HELPER_PARAMETER \
    (handlers_list[ASTType(a)].parameter)

#define HELPER_PARAMETER_STRING \
    ((handlers_list[ASTType(a)].parameter != NULL) ? (handlers_list[ASTType(a)].parameter) : "")

void prettyprint(FILE* f, AST a)
{
    prettyprint_level(f, a, 0);
}

static char* prettyprint_in_buffer_common(AST a, void (*pretty_func)(FILE*, AST))
{
#ifdef HAVE_OPEN_MEMSTREAM
    char *result = NULL;
    size_t size = 0;

    FILE* temporal_stream = open_memstream(&result, &size);
    pretty_func(temporal_stream, a);
    fclose(temporal_stream);

    return result;
#else
    FILE* temporal_file = tmpfile();

    pretty_func(temporal_file, a);

    int bytes_file = ftell(temporal_file) + 20;
    rewind(temporal_file);

    char* result = calloc(bytes_file, sizeof(char));
    fread(result, bytes_file, sizeof(char), temporal_file);

    int c = strlen(result) - 1;

    while (result[c] == '\n')
    {
        result[c] = '\0';
        c--;
    }

    fclose(temporal_file);

    return result;
#endif
}

char* prettyprint_in_buffer(AST a)
{
    return prettyprint_in_buffer_common(a, prettyprint);
}

static void list_handler_adapter(FILE* f, AST a)
{
    list_handler(f, a, 0);
}

char* list_handler_in_buffer(AST a)
{
    return prettyprint_in_buffer_common(a, list_handler_adapter);
}

static int character_level_vfprintf(FILE* stream, const char* format, va_list args)
{
    int result;
    int size = 512;
    char* c = calloc(size, sizeof(char));
    va_list va;

    va_copy(va, args);
    result = vsnprintf(c, size, format, va);
    va_end(va);

    while (result < 0 || result >= size)
    {
        va_copy(va, args);
        size *= 2;
        free(c);
        c = calloc(size, sizeof(char));
        result = vsnprintf(c, size, format, va);
        va_end(va);
    }

    fprintf(stream, "%s", c);
    free(c);

    return result;
}



int token_fprintf(FILE *stream, AST node UNUSED_PARAMETER, const char *format, ...)
{
    int result = 0;
    va_list args;

    va_start(args, format);
    result = character_level_vfprintf(stream, format, args);
    va_end(args);

    return result;
}

static void indent_at_level(FILE* f, AST node, int level)
{
    int i;
    // This 4 should be configurable
    for (i = 0; i < 4*level; i++)
    {
        token_fprintf(f, node, " ");
    }
}

static void prettyprint_level(FILE* f, AST a, int level)
{
    if (a == NULL)
        return;

    prettyprint_handler_t hnd = handlers_list[ASTType(a)].handler;

    if (hnd == NULL)
    {
        fprintf(stderr, "Node '%s' has NULL handler\n", ast_node_names[ASTType(a)]);
        return;
    }
    else
    {
        // fprintf(stderr, "Calling handler of '%s'\n", ast_node_names[ASTType(a)]);
    }

    (*hnd)(f, a, level);
}

static void ambiguity_handler(FILE* f, AST a, int level)
{
    // fprintf(f, "/* Ambiguous node with %d options */ ", a->num_ambig);
    // Print the first ambiguity because all "look like" the same, no matter
    // which one is actually printed
    prettyprint_level(f, ast_get_ambiguity(a, 0), level);
}

static void character_separated_sequence_handler(FILE* f, AST a, int level, 
        const char* separator, prettyprint_handler_t specific_handler)
{
    if (ASTType(a) == AST_AMBIGUITY)
    {
        character_separated_sequence_handler(f, ast_get_ambiguity(a, 0), level, separator, specific_handler);
        return;
    }

    if (ASTSon0(a) != NULL)
    {
        character_separated_sequence_handler(f, ASTSon0(a), level, separator, specific_handler);
        token_fprintf(f, a, separator);
    }

    ((specific_handler == NULL) ? prettyprint_level : specific_handler)(f, ASTSon1(a), level);
}

static void spaced_sequence_handler(FILE* f, AST a, int level)
{
    character_separated_sequence_handler(f, a, level, " ", NULL);
}

static void list_handler(FILE* f, AST a, int level)
{
    character_separated_sequence_handler(f, a, level, ", ", NULL);
}

static void sequence_handler(FILE* f, AST a, int level)
{
    character_separated_sequence_handler(f, a, level, "", NULL);
}

static void simple_declaration_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, " ");
        }
    }

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    
    token_fprintf(f, a, ";\n");
}

static void simple_parameter_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    token_fprintf(f, a, "%s", HELPER_PARAMETER);
}

static void indented_simple_parameter_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    simple_parameter_handler(f, a, level);
}

static void init_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void unary_container_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
}

static void parenthesized_son_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void nested_name_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    token_fprintf(f, a, "::");

    if (ASTSon1(a) != NULL)
    {
        if (ASTType(a) == AST_NESTED_NAME_SPECIFIER_TEMPLATE)
        {
            token_fprintf(f, a, "template ");
        }

        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void simple_text_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    token_fprintf(f, a, "%s", ASTText(a));
}

static void template_id_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    token_fprintf(f, a, "< ");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, " >");
}

static void type_id_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void abstract_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void abstract_declarator_function_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, "(");
    if (ASTType(ASTSon1(a)) != AST_KR_PARAMETER_LIST)
    {
        list_handler(f, ASTSon1(a), level);
    }
    else
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ")");

    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, " ");
        spaced_sequence_handler(f, ASTSon2(a), level);
    }

    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon3(a), level);
    }
}

static void abstract_array_declarator_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, "[");

    // static of C99
    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), level);
        fprintf(f, " ");
    }

    // cv-qualifier-seq of C99
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
        fprintf(f, " ");
    }

    // expression
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, "]");
}

static void null_handler(FILE* f UNUSED_PARAMETER, AST a UNUSED_PARAMETER, int level UNUSED_PARAMETER) { }

static void parameter_decl_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
    
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, " = ");
        prettyprint_level(f, ASTSon2(a), level);
    }
}

static void prefix_with_token_text_then_son_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), level);
}

static void prefix_with_parameter_then_son_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), level);
}

static void braced_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "{");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "}");
}

static void pointer_decl_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void pointer_spec_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
        token_fprintf(f, a, " ");
    }
    
    token_fprintf(f, a, "*");
    
    if (ASTSon2(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon2(a), level);
    }
}

static void decl_specifier_sequence(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon0(a), level);

        if (ASTSon1(a) != NULL || ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, " ");
        }
    }


    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);

        if (ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, " ");
        }
    }

    if (ASTSon2(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon2(a), level);
    }
}

static void binary_operator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " %s ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), level);
}

static void throw_expression_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "throw");

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon0(a), level);
    }
}

static void conditional_expression_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ? ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, " : ");
    prettyprint_level(f, ASTSon2(a), level);
}

static void cast_expression_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ") ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void sizeof_typeid_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s(", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void new_expression_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "new ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
        token_fprintf(f, a, " ");
    }

    prettyprint_level(f, ASTSon2(a), level);

    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon3(a), level);
    }
}

static void new_type_id_expr_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, "new");

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, ")");

    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), level);
    }
}

static void new_type_id_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void new_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, ")");
}

static void delete_expression_handler(FILE* f, AST a, int level)
{
    if(ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }
    
    token_fprintf(f, a, "delete");
    if (ASTType(a) == AST_DELETE_ARRAY_EXPR)
    {
        token_fprintf(f, a, "[]");
    }
    token_fprintf(f, a, " ");

    prettyprint_level(f, ASTSon1(a), level);
}

static void array_subscript_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "[");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, "]");
}

static void function_call_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ")");
}

static void typename_explicit_type_conversion(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "typename ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);

    token_fprintf(f, a, "(");
    if (ASTSon3(a) != NULL)
    {
        list_handler(f, ASTSon3(a), level);
    }
    token_fprintf(f, a, ")");
}

static void typename_template_explicit_type_conversion_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "typename ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    prettyprint_level(f, ASTSon1(a), level);

    prettyprint_level(f, ASTSon2(a), level);

    token_fprintf(f, a, "(");
    if (ASTSon3(a) != NULL)
    {
        list_handler(f, ASTSon3(a), level);
    }
    token_fprintf(f, a, ")");
}

static void typename_template_template_explicit_type_conversion_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "typename ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    prettyprint_level(f, ASTSon1(a), level);

    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon2(a), level);

    token_fprintf(f, a, "(");
    if (ASTSon3(a) != NULL)
    {
        list_handler(f, ASTSon3(a), level);
    }
    token_fprintf(f, a, ")");
}

static void infix_parameter_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "%s", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), level);
}

static void template_member_access(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "%stemplate ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), level);
}

static void son_handler_then_suffix_parameter(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "%s", HELPER_PARAMETER_STRING);
}

static void templated_cast_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s<", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " >");
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void qualified_id_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void qualified_template_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon2(a), level);
}

static void conversion_type_id_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void conversion_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void constructor_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ": ");
    list_handler(f, ASTSon0(a), level);
}

static void mem_initializer_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ")");
}

static void mem_initializer_id_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void class_specifier_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "{\n");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level + 2);
    }
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}");
}

static void class_head_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    token_fprintf(f, a, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), level);
    }
}

static void member_access_specifier_handler(FILE *f, AST a, int level)
{
    indent_at_level(f, a, level-1);
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " :\n");
}

static void using_declaration_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "using ");

    if (ASTType(a) == AST_USING_DECL_TYPENAME)
    {
        token_fprintf(f, a, "typename ");
    }
    
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);

    token_fprintf(f, a, ";\n");
}

static void template_declaration_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    if (ASTType(a) == AST_EXPORT_TEMPLATE_DECLARATION)
    {
        token_fprintf(f, a, "export ");
    }
    token_fprintf(f, a, "template<");
    list_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " >\n");
    prettyprint_level(f, ASTSon1(a), level);
}

static void type_parameter_class_or_typename_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s ", HELPER_PARAMETER_STRING);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " = ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void type_parameter_template_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "template<");
    list_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " > class ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, " = ");
        prettyprint_level(f, ASTSon2(a), level);
    }
}

static void simple_type_specifier_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }
    else if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, "template ");
        prettyprint_level(f, ASTSon3(a), level);
    }
}

static void member_declaration_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, " ");
        }
    }

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ";\n");
}

static void member_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void constant_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "= ");
    prettyprint_level(f, ASTSon0(a), level);
}

static void function_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }

    prettyprint_level(f, ASTSon1(a), level);

    CXX_LANGUAGE()
    {
        if (ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, "\n");
            indent_at_level(f, a, level+1);
            prettyprint_level(f, ASTSon2(a), level+1);
            token_fprintf(f, a, " ");
        }
        token_fprintf(f, a, "\n");
    }

    C_LANGUAGE()
    {
        if (ASTSon2(a) != NULL)
        {
            token_fprintf(f, a, "\n");
            prettyprint_level(f, ASTSon2(a), level+1);
        }
        else
        {
            token_fprintf(f, a, "\n");
        }
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void compound_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "{\n");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level + 1);
    }
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}\n");
}

static void labeled_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " : \n");
    prettyprint_level(f, ASTSon1(a), level);
}

static void case_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "case ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " : \n");
    prettyprint_level(f, ASTSon1(a), level);
}

static void default_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "default : \n");
    prettyprint_level(f, ASTSon0(a), level);
}

static void expression_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ";\n");
}

static void selection_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "switch (");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")\n");
    prettyprint_level(f, ASTSon1(a), level);
}

static void condition_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
        token_fprintf(f, a, " = ");
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void while_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "while (");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")\n");

    if (ASTType(ASTSon1(a)) != AST_COMPOUND_STATEMENT)
    {
        prettyprint_level(f, ASTSon1(a), level+1);
    }
    else
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void do_while_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "do\n");
    prettyprint_level(f, ASTSon0(a), level);
    indent_at_level(f, a, level);
    token_fprintf(f, a, "while (");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ");\n");
}

static void for_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "for (");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), 0);
    }

    if (ASTSon1(a) != NULL)
    {
        indent_at_level(f, a, level+1);
        prettyprint_level(f, ASTSon1(a), 0);
    }
    
    token_fprintf(f, a, ";\n");

    if (ASTSon3(a) != NULL)
    {
        indent_at_level(f, a, level+1);
        prettyprint_level(f, ASTSon2(a), 0);
    }

    token_fprintf(f, a, ")\n");

    prettyprint_level(f, ASTSon3(a), level);
}

static void return_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "return");

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, ";\n");
}

static void goto_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);

    token_fprintf(f, a, "goto ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ";\n");
}

static void try_block_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);

    token_fprintf(f, a, "try\n");
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void catch_handler_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "catch (");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")\n");
    prettyprint_level(f, ASTSon1(a), level);
}

static void exception_declaration_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void base_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, " : ");
    list_handler(f, ASTSon0(a), level);
}

static void base_specifier_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTType(a) == AST_BASE_SPECIFIER_TEMPLATE)
    {
        token_fprintf(f, a, "template ");
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void base_specifier_access_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    if (ASTType(a) == AST_BASE_SPECIFIER_ACCESS_TEMPLATE)
    {
        token_fprintf(f, a, "template ");
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void base_specifier_virtual_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "virtual ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    if (ASTType(a) == AST_BASE_SPECIFIER_VIRTUAL_TEMPLATE)
    {
        token_fprintf(f, a, "template ");
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void base_specifier_access_virtual_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");

    token_fprintf(f, a, "virtual ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    if (ASTType(a) == AST_BASE_SPECIFIER_ACCESS_VIRTUAL_TEMPLATE)
    {
        token_fprintf(f, a, "template ");
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_class_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_template_class_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_template_template_class_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }

    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon3(a), level);
}

static void elaborated_type_enum_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "enum ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void elaborated_typename_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "typename ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void elaborated_typename_template_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "typename ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon2(a), level);
}

static void if_else_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "if (");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")\n");
    
    // Sthetic
    if (ASTType(ASTSon1(a)) != AST_COMPOUND_STATEMENT)
    {
        prettyprint_level(f, ASTSon1(a), level+1);
    }
    else
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    if (ASTSon2(a) != NULL)
    {
        indent_at_level(f, a, level);
        token_fprintf(f, a, "else\n");

        if (ASTType(ASTSon2(a)) != AST_COMPOUND_STATEMENT)
        {
            prettyprint_level(f, ASTSon2(a), level+1);
        }
        else
        {
            prettyprint_level(f, ASTSon2(a), level);
        }
    }
}

static void exception_specification_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "throw (");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, ")");
}

static void operator_function_id_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "operator ");
    prettyprint_level(f, ASTSon0(a), level);
    
    if (ASTType(a) == AST_OPERATOR_FUNCTION_ID_TEMPLATE)
    {
        token_fprintf(f, a, "<");
        if (ASTSon1(a) != NULL)
        {
            list_handler(f, ASTSon1(a), level);
        }
        token_fprintf(f, a, " >");
    }
}

static void asm_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    // Use the exact token (asm or __asm__)
    token_fprintf(f, a, "%s ", ASTText(a));

    // Optional 'volatile' for GCC
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ");\n");
}

static void bitfield_declarator_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }

    token_fprintf(f, a, ": ");

    prettyprint_level(f, ASTSon1(a), level);
}

static void enum_specifier_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "enum ");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "{");

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level+1);
    }
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}");
}

static void enum_def_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " = ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void explicit_specialization_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "template<>\n");

    prettyprint_level(f, ASTSon0(a), level);
}

static void linkage_specification_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "extern ");
    
    prettyprint_level(f, ASTSon0(a), level);
    
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "{\n");

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level+1);
    }
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}\n");
}

static void linkage_specification_decl_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "extern ");
    prettyprint_level(f, ASTSon0(a), level);

    token_fprintf(f, a, "\n");
    prettyprint_level(f, ASTSon1(a), level);

    token_fprintf(f, a, "\n");
}

static void namespace_alias_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "namespace ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " = ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ";\n");
}

static void qualified_namespace_spec_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void using_directive_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "using namespace ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, ";\n");
}

static void namespace_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "namespace ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " {\n");

    prettyprint_level(f, ASTSon1(a), level+1);
    
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}\n");
}

static void pseudo_destructor_name_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "::");
    prettyprint_level(f, ASTSon1(a), level);
}

static void pseudo_destructor_template_name_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "::");
    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void parenthesized_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    list_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void kr_parameter_list_handler(FILE* f, AST a, int level)
{
    AST list = ASTSon0(a);

    list_handler(f, list, level);
}

static void designated_initializer_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void designation_handler(FILE* f, AST a, int level)
{
    sequence_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " = ");
}

static void index_designator_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "[");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "]");
}

static void field_designator_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ".");
    prettyprint_level(f, ASTSon0(a), level);
}

static void decltype_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    token_fprintf(f, a, "decltype(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void unknown_pragma_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    token_fprintf(f, a, "#pragma ");
    token_fprintf(f, a, "%s\n", ASTText(a));
}

static void pp_prepro_token_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    char* text = strdup(ASTText(a));

    if (prettyprint_behaviour.internal_output)
    {
        // Do nothing special
        token_fprintf(f, a, "%s", text);
    }
    else
    {
        char* start = text + strlen("@-P-@");
        char* end = text + strlen(text) - strlen("@-PP-@");

        // Save the end and make it the end of the string
        char temp = *end;
        *end = '\0';

        // Print the preprocessing item
        token_fprintf(f, a, "%s\n", start);

        // And restore it back to the original character
        *end = temp;
    }

    free(text);
}

static void pp_comment_handler(FILE* f, AST a, int level)
{
    char* text = strdup(ASTText(a));

    if (prettyprint_behaviour.internal_output)
    {
        // Do nothing special
        token_fprintf(f, a, "%s", text);
    }
    else
    {
        // Convert it back into a normal comment
        char* end = text + strlen(text) - strlen("@-CC-@");

        // The whole text
        char* current_start = text + strlen("@-C-@");
        char* current_end;

        do
        {
            // Look for the first '\n'
            current_end = strchr(current_start, '\n');

            // If not found use the NULL character
            if (current_end == NULL)
            {
                current_end = end;
            }

            // Now replace the end with an ending character
            char temp = *current_end;
            *current_end = '\0';

            indent_at_level(f, a, level);
            C_LANGUAGE()
            {
                token_fprintf(f, a, "/* %s */\n", current_start);
            }
            CXX_LANGUAGE()
            {
                token_fprintf(f, a, "// %s\n", current_start);
            }

            // And restore the modified character
            *current_end = temp;
            // The next string starts after the current end
            current_start = current_end + 1;
        }
        while (current_end != end);
    }

    free(text);
}

static void custom_construct_statement_handler(FILE *f, AST a, int level)
{
    indent_at_level(f, a, level);
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void custom_construct_header_handler(FILE *f, AST a, int level)
{
    token_fprintf(f, a, "__construct__ %s ", ASTText(a));

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, "\n");
        indent_at_level(f, a, level + 1);
        list_handler(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, "\n");
}

static void custom_construct_parameter(FILE *f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " : ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void pragma_custom_line_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s ", ASTText(a));
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, "(");

        // This is a list inside another list, it cannot be 
        // handled normally
        list_handler(f, ASTSon1(a), level);

        token_fprintf(f, a, ") ");
    }
    if (ASTSon0(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
}

static void pragma_custom_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma %s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void pragma_custom_construct_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma %s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void pragma_custom_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s", ASTText(a));
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, "(");

        // This is a list inside another list, it cannot be 
        // handled normally
        list_handler(f, ASTSon0(a), level);

        token_fprintf(f, a, ")");
    }
}

static void gcc_label_declaration_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "label ");
    list_handler(f, ASTSon0(a), level);

    token_fprintf(f, a, ";\n");
}

static void gcc_attribute_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ASTText(a));
    token_fprintf(f, a, "((");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "))");
}

static void gcc_attribute_value_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL || ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, "(");
        if (ASTSon1(a) != NULL)
        {
            prettyprint_level(f, ASTSon1(a), level);
            if (ASTSon2(a) != NULL)
            {
                token_fprintf(f, a, ", ");
            }
        }
        list_handler(f, ASTSon2(a), level);
        token_fprintf(f, a, ")");
    }
}

static void gcc_asm_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, ASTText(a));
    token_fprintf(f, a, " ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    token_fprintf(f, a, "(");

    prettyprint_level(f, ASTSon1(a), level);

    token_fprintf(f, a, ");\n");
}

static void gcc_asm_def_parameters(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    token_fprintf(f, a, ": ");

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ": ");

    if (ASTSon2(a) != NULL)
    {
        list_handler(f, ASTSon2(a), level);
    }

    if (ASTSon3(a) != NULL)
    {
        // This can appear only if explicitly given
        token_fprintf(f, a, ": ");
        list_handler(f, ASTSon3(a), level);
    }
}

static void gcc_asm_operand_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, "[");
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, "] ");
    }

    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, " (");
    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_type_spec_sequence_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_typeof_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ASTText(a));
    token_fprintf(f, a, " (");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_typeof_expr_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ASTText(a));
    CXX_LANGUAGE()
    {
        token_fprintf(f, a, " ");
    }
    C_LANGUAGE()
    {
        token_fprintf(f, a, "(");
    }
    prettyprint_level(f, ASTSon0(a), level);
    C_LANGUAGE()
    {
        token_fprintf(f, a, ")");
    }
}

static void gcc_elaborated_type_enum_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "enum ");
    spaced_sequence_handler(f, ASTSon3(a), level);

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
}

static void gcc_elaborated_type_class_handler(FILE* f, AST a, int level)
{
    AST elaborated = ASTSon1(a);

    if (ASTSon0(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon0(elaborated), level);
    }

    spaced_sequence_handler(f, ASTSon0(a), level);

    if (ASTSon1(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon1(elaborated), level);
    }

    if (ASTSon2(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon2(elaborated), level);
    }

    prettyprint_level(f, ASTSon3(elaborated), level);
}

static void gcc_elaborated_type_template_handler(FILE* f, AST a, int level)
{
    AST elaborated = ASTSon1(a);

    if (ASTSon0(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon0(elaborated), level);
    }

    spaced_sequence_handler(f, ASTSon0(a), level);

    if (ASTSon1(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon1(elaborated), level);
    }

    if (ASTSon2(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon2(elaborated), level);
    }

    prettyprint_level(f, ASTSon3(elaborated), level);
}

static void gcc_elaborated_type_template_template_handler(FILE* f, AST a, int level)
{
    AST elaborated = ASTSon1(a);

    if (ASTSon0(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon0(elaborated), level);
    }

    spaced_sequence_handler(f, ASTSon0(a), level);

    if (ASTSon1(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon1(elaborated), level);
    }

    if (ASTSon2(elaborated) != NULL)
    {
        prettyprint_level(f, ASTSon2(elaborated), level);
    }

    token_fprintf(f, a, "template ");
    prettyprint_level(f, ASTSon3(elaborated), level);
}

static void gcc_init_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon2(a), level);
    }

    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, " ");
        spaced_sequence_handler(f, ASTSon3(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void gcc_asm_specification_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s (", ASTText(a));
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_declarator_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_pointer_declarator_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(a), level);
    prettyprint_level(f, ASTSon2(a), level);
}

static void gcc_reference_spec_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "& ");
    spaced_sequence_handler(f, ASTSon0(a), level);
}

static void gcc_enum_specifier_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "enum ");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "{");

    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), level+1);
    }
    token_fprintf(f, a, "\n");
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}");

    spaced_sequence_handler(f, ASTSon2(a), level);
}

static void gcc_abstract_declarator_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_ptr_abstract_declarator_handler(FILE* f, AST a, int level)
{
    spaced_sequence_handler(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }
}

static void gcc_initializer_clause_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ":");
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_class_head_handler(FILE* f, AST a, int level)
{
    AST class_head = ASTSon1(a);

    prettyprint_level(f, ASTSon0(class_head), level);

    token_fprintf(f, a, " ");

    if (ASTSon1(class_head) != NULL)
    {
        prettyprint_level(f, ASTSon1(class_head), level);
    }

    spaced_sequence_handler(f, ASTSon0(a), level);

    if (ASTSon2(class_head) != NULL)
    {
        prettyprint_level(f, ASTSon2(class_head), level);
    }

    if (ASTSon3(class_head) != NULL)
    {
        prettyprint_level(f, ASTSon3(class_head), level);
    }
}

static void gcc_member_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    spaced_sequence_handler(f, ASTSon2(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void gcc_bitfield_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " : ");
    spaced_sequence_handler(f, ASTSon2(a), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_case_statement_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "case ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "...");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, " : ");
    prettyprint_level(f, ASTSon2(a), level);
}

static void gcc_condition_handler(FILE* f, AST a, int level)
{
    AST condition_decl = ASTSon1(a);

    prettyprint_level(f, ASTSon0(condition_decl), level);
    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon1(condition_decl), level);
    token_fprintf(f, a, " ");

    if (ASTSon0(a) != NULL)
    {
        spaced_sequence_handler(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }

    token_fprintf(f, a, "= ");

    if (ASTSon2(condition_decl) != NULL)
    {
        prettyprint_level(f, ASTSon2(condition_decl), level);
        token_fprintf(f, a, " ");
    }

    prettyprint_level(f, ASTSon3(condition_decl), level);
}

static void gcc_goto_statement_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "goto * ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ";\n");
}

static void gcc_mem_initializer_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, ")");
}

static void gcc_builtin_va_arg_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "__builtin_va_arg(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ", ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_builtin_offsetof_handler(FILE *f, AST a, int level)
{
    token_fprintf(f, a, "__builtin_offsetof(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ", ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_builtin_choose_expr_handler(FILE *f, AST a, int level)
{
    token_fprintf(f, a, "__builtin_choose_expr(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ", ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ", ");
    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_builtin_types_compatible_p_handler(FILE *f, AST a, int level)
{
    token_fprintf(f, a, "__builtin_types_compatible_p(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ", ");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void gcc_postfix_expression(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
    token_fprintf(f, a, "{");
    list_handler(f, ASTSon1(a), level);
    token_fprintf(f, a, "}");
}

static void gcc_alignof_type_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, ASTText(a));
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void explicit_instantiation_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, " ");
    }
    token_fprintf(f, a, "template ");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
        token_fprintf(f, a, " ");
    }
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), level);
    }
    token_fprintf(f, a, ";\n");
}

static void gcc_conditional_expression(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ? : ");
    prettyprint_level(f, ASTSon1(a), level);
}

static void gcc_extension_preffix_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, ASTText(a));
    token_fprintf(f, a, "\n");

    prettyprint_level(f, ASTSon0(a), level);
}

static void gcc_parameter_decl_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, " ");
        prettyprint_level(f, ASTSon1(a), level);
    }

    token_fprintf(f, a, " ");
    prettyprint_level(f, ASTSon3(a), level);
    
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, " = ");
        prettyprint_level(f, ASTSon2(a), level);
    }
}

static void gcc_using_directive_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "using namespace ");

    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }

    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }

    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, " ");
    spaced_sequence_handler(f, ASTSon3(a), level);
    token_fprintf(f, a, ";\n");
}

static void gcc_namespace_definition_handler(FILE* f, AST a, int level)
{
    indent_at_level(f, a, level);
    token_fprintf(f, a, "namespace ");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    spaced_sequence_handler(f, ASTSon2(a), level);
    token_fprintf(f, a, " {\n");

    prettyprint_level(f, ASTSon1(a), level+1);
    
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}\n");
}

static void gcc_functional_declarator_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, " ");
    spaced_sequence_handler(f, ASTSon1(a), level);
}

static void gcc_offsetof_member_designator_handler(FILE *f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), level);
    }
}

static void gcc_parenthesized_expression_handler(FILE* f, AST a, int level)
{
    // This one is printed specially, otherwise it looks specially messed
    token_fprintf(f, a, "({\n");
    AST compound_statement = ASTSon0(a);
    if (ASTSon0(compound_statement) != NULL)
    {
        prettyprint_level(f, ASTSon0(compound_statement), level + 1);
    }
    indent_at_level(f, a, level);
    token_fprintf(f, a, "})");
}

// OpenMP 2.5

static void omp_generic_construct_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    prettyprint_level(f, ASTSon1(a), level);
}

static void omp_custom_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s", ASTText(a));
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, "(");
        list_handler(f, ASTSon0(a), level);
        token_fprintf(f, a, ")");
    }
}

static void omp_custom_parameter_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "(");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, ")");
}

static void omp_custom_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma omp %s", ASTText(a));
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, " ");
        spaced_sequence_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
}

static void omp_custom_construct_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma omp %s", ASTText(a));
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, " ");
        spaced_sequence_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
}

static void omp_generic_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma omp %s", HELPER_PARAMETER_STRING);
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, " ");
        spaced_sequence_handler(f, ASTSon0(a), level);
    }
    token_fprintf(f, a, "\n");
}

static void omp_generic_clause_handler_with_expression(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s", HELPER_PARAMETER_STRING);
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void omp_generic_clause_handler_with_list(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s", HELPER_PARAMETER_STRING);
    token_fprintf(f, a, "(");
    list_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
}

static void omp_schedule_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "schedule(");
    prettyprint_level(f, ASTSon0(a), level);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, ", ");
        prettyprint_level(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ")");
}

static void omp_critical_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma omp critical ");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, "(");
        prettyprint_level(f, ASTSon0(a), level);
        token_fprintf(f, a, ")");
    }
    token_fprintf(f, a, "\n");
}

static void omp_reduction_clause_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "reduction");
    token_fprintf(f, a, "(");
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, ":");
    list_handler(f, ASTSon1(a), level);
    token_fprintf(f, a, ")");
}

static void omp_sections_construct_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);

    indent_at_level(f, a, level);
    token_fprintf(f, a, "{\n");
    prettyprint_level(f, ASTSon1(a), level+1);
    indent_at_level(f, a, level);
    token_fprintf(f, a, "}\n");
}

static void omp_section_handler(FILE* f, AST a, int level)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), level);
    }
    prettyprint_level(f, ASTSon1(a), level);
}

static void omp_threadprivate_directive_handler(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "#pragma omp threadprivate");
    token_fprintf(f, a, "(");
    list_handler(f, ASTSon0(a), level);
    token_fprintf(f, a, ")");
    token_fprintf(f, a, "\n");
}

static void omp_default_custom_clause_handler(FILE* f, AST a, int level UNUSED_PARAMETER)
{
    token_fprintf(f, a, "default(%s)", ASTText(a));
}

static void array_section_handler(FILE* f, AST a, int level)
{
    prettyprint_level(f, ASTSon0(a), level);
    token_fprintf(f, a, "[");
    prettyprint_level(f, ASTSon1(a), level);
    token_fprintf(f, a, ":");
    prettyprint_level(f, ASTSon2(a), level);
    token_fprintf(f, a, "]");
}

static void gxx_type_traits(FILE* f, AST a, int level)
{
    token_fprintf(f, a, "%s(", ASTText(a));
    prettyprint_level(f, ASTSon0(a), level);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, ", ");
        prettyprint_level(f, ASTSon1(a), level);
    }
    token_fprintf(f, a, ")");
}
