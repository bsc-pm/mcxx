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


#ifndef FORTRAN_PRETTYPRINT_C
  #define FORTRAN_PRETTYPRINT_C
#endif

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif
#ifdef HAVE_OPEN_MEMSTREAM
  // Needed, otherwise open_memstream is not declared
  #define _GNU_SOURCE
#endif

#include "fortran03-prettyprint.h"


#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-prettyprint-internal.h"

HANDLER_PROTOTYPE(access_statement_handler);
HANDLER_PROTOTYPE(allocatable_statement_handler);
HANDLER_PROTOTYPE(allocate_statement_handler);
HANDLER_PROTOTYPE(all_stop_statement_handler);
HANDLER_PROTOTYPE(arithmetic_if_statement_handler);
HANDLER_PROTOTYPE(array_constructor_handler);
HANDLER_PROTOTYPE(array_constructor_spec_handler);
HANDLER_PROTOTYPE(array_ref_handler);
HANDLER_PROTOTYPE(array_spec_handler);
HANDLER_PROTOTYPE(assigned_goto_statement_handler);
HANDLER_PROTOTYPE(associate_construct_handler);
HANDLER_PROTOTYPE(associate_statement_handler);
HANDLER_PROTOTYPE(asynchronous_statement_handler);
HANDLER_PROTOTYPE(attr_spec_handler);
HANDLER_PROTOTYPE(bind_c_spec_handler);
HANDLER_PROTOTYPE(binding_statement_handler);
HANDLER_PROTOTYPE(block_construct_handler);
HANDLER_PROTOTYPE(block_data_program_unit_handler);
HANDLER_PROTOTYPE(block_data_statement_handler);
HANDLER_PROTOTYPE(block_statement_handler);
HANDLER_PROTOTYPE(body_program_unit_handler);
HANDLER_PROTOTYPE(bool_type_handler);
HANDLER_PROTOTYPE(case_selector_handler);
HANDLER_PROTOTYPE(case_statement_handler);
HANDLER_PROTOTYPE(case_value_range_handler);
HANDLER_PROTOTYPE(character_type_handler);
HANDLER_PROTOTYPE(char_selector_handler);
HANDLER_PROTOTYPE(class_default_statement_handler);
HANDLER_PROTOTYPE(class_is_statement_handler);
HANDLER_PROTOTYPE(class_name_handler);
HANDLER_PROTOTYPE(close_statement_handler);
HANDLER_PROTOTYPE(codimension_decl_handler);
HANDLER_PROTOTYPE(codimension_statement_handler);
HANDLER_PROTOTYPE(common_block_item_handler);
HANDLER_PROTOTYPE(common_name_handler);
HANDLER_PROTOTYPE(common_statement_handler);
HANDLER_PROTOTYPE(complex_literal_handler);
HANDLER_PROTOTYPE(complex_type_handler);
HANDLER_PROTOTYPE(component_ref_handler);
HANDLER_PROTOTYPE(computed_goto_statement_handler);
HANDLER_PROTOTYPE(compound_statement_handler);
HANDLER_PROTOTYPE(continue_statement_handler);
HANDLER_PROTOTYPE(critical_construct_handler);
HANDLER_PROTOTYPE(critical_statement_handler);
HANDLER_PROTOTYPE(cycle_statement_handler);
HANDLER_PROTOTYPE(data_component_def_statement_handler);
HANDLER_PROTOTYPE(data_statement_handler);
HANDLER_PROTOTYPE(data_statement_set_handler);
HANDLER_PROTOTYPE(deallocate_statement_handler);
HANDLER_PROTOTYPE(declaration_handler);
HANDLER_PROTOTYPE(declaration_specs_handler);
HANDLER_PROTOTYPE(declaration_statement_handler);
HANDLER_PROTOTYPE(default_statement_handler);
HANDLER_PROTOTYPE(derived_type_constructor_handler);
HANDLER_PROTOTYPE(derived_type_def_body_handler);
HANDLER_PROTOTYPE(derived_type_def_handler);
HANDLER_PROTOTYPE(derived_type_name_handler);
HANDLER_PROTOTYPE(derived_type_statement_handler);
HANDLER_PROTOTYPE(dimension_decl_handler);
HANDLER_PROTOTYPE(dimension_statement_handler);
HANDLER_PROTOTYPE(do_loop_statement_handler);
HANDLER_PROTOTYPE(double_type_handler);
HANDLER_PROTOTYPE(elsewhere_statement_handler);
HANDLER_PROTOTYPE(end_statement_handler);
HANDLER_PROTOTYPE(entry_statement_handler);
HANDLER_PROTOTYPE(enum_def_handler);
HANDLER_PROTOTYPE(enum_def_statement_handler);
HANDLER_PROTOTYPE(enumerator_def_handler);
HANDLER_PROTOTYPE(equivalence_set_handler);
HANDLER_PROTOTYPE(equivalence_statement_handler);
HANDLER_PROTOTYPE(exit_statement_handler);
HANDLER_PROTOTYPE(expression_statement_handler);
HANDLER_PROTOTYPE(external_statement_handler);
HANDLER_PROTOTYPE(final_statement_handler);
HANDLER_PROTOTYPE(float_type_handler);
HANDLER_PROTOTYPE(forall_construct_handler);
HANDLER_PROTOTYPE(forall_construct_statement_handler);
HANDLER_PROTOTYPE(forall_header_handler);
HANDLER_PROTOTYPE(forall_statement_handler);
HANDLER_PROTOTYPE(forall_triplet_spec_handler);
HANDLER_PROTOTYPE(format_statement_handler);
HANDLER_PROTOTYPE(function_call_handler);
HANDLER_PROTOTYPE(function_program_unit_handler);
HANDLER_PROTOTYPE(function_prototype_handler);
HANDLER_PROTOTYPE(function_statement_handler);
HANDLER_PROTOTYPE(goto_statement_handler);
HANDLER_PROTOTYPE(if_then_statement_handler);
HANDLER_PROTOTYPE(image_ref_handler);
HANDLER_PROTOTYPE(implicit_spec_handler);
HANDLER_PROTOTYPE(implicit_statement_handler);
HANDLER_PROTOTYPE(implied_do_control_handler);
HANDLER_PROTOTYPE(implied_do_handler);
HANDLER_PROTOTYPE(import_statement_handler);
HANDLER_PROTOTYPE(intent_statement_handler);
HANDLER_PROTOTYPE(interface_block_handler);
HANDLER_PROTOTYPE(interface_statement_handler);
HANDLER_PROTOTYPE(intrinsic_statement_handler);
HANDLER_PROTOTYPE(int_type_handler);
HANDLER_PROTOTYPE(io_spec_handler);
HANDLER_PROTOTYPE(io_statement_handler);
HANDLER_PROTOTYPE(letter_spec_handler);
HANDLER_PROTOTYPE(lock_statement_handler);
HANDLER_PROTOTYPE(loop_control_handler);
HANDLER_PROTOTYPE(main_program_unit_handler);
HANDLER_PROTOTYPE(mask_elsewhere_part_handler);
HANDLER_PROTOTYPE(module_procedure_handler);
HANDLER_PROTOTYPE(module_procedure_program_unit_handler);
HANDLER_PROTOTYPE(module_procedure_statement_handler);
HANDLER_PROTOTYPE(module_program_unit_handler);
HANDLER_PROTOTYPE(module_statement_handler);
HANDLER_PROTOTYPE(named_pair_spec_handler);
HANDLER_PROTOTYPE(namelist_item_handler);
HANDLER_PROTOTYPE(namelist_statement_handler);
HANDLER_PROTOTYPE(nullify_statement_handler);
HANDLER_PROTOTYPE(open_statement_handler);
HANDLER_PROTOTYPE(operator_name_handler);
HANDLER_PROTOTYPE(optional_statement_handler);
HANDLER_PROTOTYPE(opt_value_handler);
HANDLER_PROTOTYPE(parameter_statement_handler);
HANDLER_PROTOTYPE(parenthesized_expression_handler);
HANDLER_PROTOTYPE(pause_statement_handler);
HANDLER_PROTOTYPE(pixel_type_handler);
HANDLER_PROTOTYPE(pointer_initialization_handler);
HANDLER_PROTOTYPE(pointer_statement_handler);
HANDLER_PROTOTYPE(cray_pointer_statement_handler);
HANDLER_PROTOTYPE(cray_pointer_spec_handler);
HANDLER_PROTOTYPE(print_statement_handler);
HANDLER_PROTOTYPE(proc_component_def_statement_handler);
HANDLER_PROTOTYPE(procedure_decl_statement_handler);
HANDLER_PROTOTYPE(procedure_suffix_handler);
HANDLER_PROTOTYPE(program_statement_handler);
HANDLER_PROTOTYPE(protected_statement_handler);
HANDLER_PROTOTYPE(read_statement_handler);
HANDLER_PROTOTYPE(rename_handler);
HANDLER_PROTOTYPE(return_statement_handler);
HANDLER_PROTOTYPE(save_statement_handler);
HANDLER_PROTOTYPE(select_case_construct_handler);
HANDLER_PROTOTYPE(select_type_construct_handler);
HANDLER_PROTOTYPE(select_type_statement_handler);
HANDLER_PROTOTYPE(sequence_statement_handler);
HANDLER_PROTOTYPE(statement_function_statement_handler);
HANDLER_PROTOTYPE(stop_statement_handler);
HANDLER_PROTOTYPE(submodule_program_unit_handler);
HANDLER_PROTOTYPE(submodule_statement_handler);
HANDLER_PROTOTYPE(subroutine_program_unit_handler);
HANDLER_PROTOTYPE(subroutine_statement_handler);
HANDLER_PROTOTYPE(subscript_triplet_handler);
HANDLER_PROTOTYPE(symbol_handler);
HANDLER_PROTOTYPE(sync_all_statement_handler);
HANDLER_PROTOTYPE(sync_images_statement_handler);
HANDLER_PROTOTYPE(sync_memory_statement_handler);
HANDLER_PROTOTYPE(target_statement_handler);
HANDLER_PROTOTYPE(type_bound_generic_procedure_handler);
HANDLER_PROTOTYPE(type_bound_procedure_part_handler);
HANDLER_PROTOTYPE(type_bound_procedure_statement_handler);
HANDLER_PROTOTYPE(type_guard_part_handler);
HANDLER_PROTOTYPE(type_is_statement_handler);
HANDLER_PROTOTYPE(type_name_handler);
HANDLER_PROTOTYPE(type_parameter_def_statement_handler);
HANDLER_PROTOTYPE(unlock_statement_handler);
HANDLER_PROTOTYPE(use_only_statement_handler);
HANDLER_PROTOTYPE(user_defined_binary_op_handler);
HANDLER_PROTOTYPE(user_defined_unary_op_handler);
HANDLER_PROTOTYPE(use_statement_handler);
HANDLER_PROTOTYPE(value_statement_handler);
HANDLER_PROTOTYPE(vector_type_handler);
HANDLER_PROTOTYPE(volatile_statement_handler);
HANDLER_PROTOTYPE(wait_statement_handler);
HANDLER_PROTOTYPE(where_construct_body_handler);
HANDLER_PROTOTYPE(where_construct_handler);
HANDLER_PROTOTYPE(where_construct_statement_handler);
HANDLER_PROTOTYPE(where_statement_handler);
HANDLER_PROTOTYPE(write_statement_handler);

HANDLER_PROTOTYPE(literal_text_handler);
HANDLER_PROTOTYPE(simple_text_handler);
HANDLER_PROTOTYPE(prefix_with_parameter_then_son_handler);
HANDLER_PROTOTYPE(binary_operator_handler);
HANDLER_PROTOTYPE(unary_operator_handler);
HANDLER_PROTOTYPE(label_assign_statement_handler);
HANDLER_PROTOTYPE(labeled_statement_handler);
HANDLER_PROTOTYPE(end_of_statement_handler);
HANDLER_PROTOTYPE(sequence_handler);
HANDLER_PROTOTYPE(unary_container_handler);
HANDLER_PROTOTYPE(ambiguity_handler);
HANDLER_PROTOTYPE(unknown_pragma_handler);

// Pragma custom support
HANDLER_PROTOTYPE(pragma_custom_directive_handler);
HANDLER_PROTOTYPE(pragma_custom_construct_handler);
HANDLER_PROTOTYPE(pragma_custom_clause_handler);
HANDLER_PROTOTYPE(pragma_custom_line_handler);

static prettyprint_entry_t handlers_list[] = 
{
    NODE_HANDLER(AST_ABSTRACT, simple_text_handler, NULL),
    NODE_HANDLER(AST_ACCESS_STATEMENT, access_statement_handler, NULL),
    NODE_HANDLER(AST_ADD, binary_operator_handler, "+"),
    NODE_HANDLER(AST_ALLOCATABLE_STATEMENT, allocatable_statement_handler, NULL),
    NODE_HANDLER(AST_ALLOCATE_STATEMENT, allocate_statement_handler, NULL),
    NODE_HANDLER(AST_ALL_STOP_STATEMENT, all_stop_statement_handler, NULL),
    NODE_HANDLER(AST_ALTERNATE_RESULT_SPEC, prefix_with_parameter_then_son_handler, "*"),
    NODE_HANDLER(AST_AMBIGUITY, ambiguity_handler, NULL),
    NODE_HANDLER(AST_ARITHMETIC_IF_STATEMENT, arithmetic_if_statement_handler, NULL),
    NODE_HANDLER(AST_ARRAY_CONSTRUCTOR, array_constructor_handler, NULL),
    NODE_HANDLER(AST_ARRAY_CONSTRUCTOR_SPEC, array_constructor_spec_handler, NULL),
    NODE_HANDLER(AST_ARRAY_SUBSCRIPT, array_ref_handler, NULL),
    NODE_HANDLER(AST_ARRAY_SPEC, array_spec_handler, NULL),
    NODE_HANDLER(AST_ASSIGNED_GOTO_STATEMENT, assigned_goto_statement_handler, NULL),
    NODE_HANDLER(AST_ASSIGNMENT, binary_operator_handler, "="),
    NODE_HANDLER(AST_ASSOCIATE_CONSTRUCT, associate_construct_handler, NULL),
    NODE_HANDLER(AST_ASSOCIATE_STATEMENT, associate_statement_handler, NULL),
    NODE_HANDLER(AST_ASYNCHRONOUS_STATEMENT, asynchronous_statement_handler, NULL),
    NODE_HANDLER(AST_ATTR_SPEC, attr_spec_handler, NULL),
    NODE_HANDLER(AST_BINARY_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_BIND_STATEMENT, binding_statement_handler, NULL),
    NODE_HANDLER(AST_BLOCK_CONSTRUCT, block_construct_handler, NULL),
    NODE_HANDLER(AST_BLOCK_DATA_PROGRAM_UNIT, block_data_program_unit_handler, NULL),
    NODE_HANDLER(AST_BLOCK_DATA_STATEMENT, block_data_statement_handler, NULL),
    NODE_HANDLER(AST_BLOCK_STATEMENT, block_statement_handler, NULL),
    NODE_HANDLER(AST_BODY_PROGRAM_UNIT, body_program_unit_handler, NULL),
    NODE_HANDLER(AST_BOOLEAN_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_BOOL_TYPE, bool_type_handler, NULL),
    NODE_HANDLER(AST_BREAK_STATEMENT, exit_statement_handler, NULL),
    NODE_HANDLER(AST_CASE_SELECTOR, case_selector_handler, NULL),
    NODE_HANDLER(AST_CASE_STATEMENT, case_statement_handler, NULL),
    NODE_HANDLER(AST_CASE_VALUE_RANGE, case_value_range_handler, NULL),
    NODE_HANDLER(AST_CHARACTER_TYPE, character_type_handler, NULL),
    NODE_HANDLER(AST_CHAR_SELECTOR, char_selector_handler, NULL),
    NODE_HANDLER(AST_CLASS_DEFAULT_STATEMENT, class_default_statement_handler, NULL),
    NODE_HANDLER(AST_CLASS_IS_STATEMENT, class_is_statement_handler, NULL),
    NODE_HANDLER(AST_CLASS_NAME, class_name_handler, NULL),
    NODE_HANDLER(AST_CLOSE_STATEMENT, close_statement_handler, NULL),
    NODE_HANDLER(AST_CODIMENSION_DECL, codimension_decl_handler, NULL),
    NODE_HANDLER(AST_CODIMENSION_STATEMENT, codimension_statement_handler, NULL),
    NODE_HANDLER(AST_COMMON_BLOCK_ITEM, common_block_item_handler, NULL),
    NODE_HANDLER(AST_COMMON_NAME, common_name_handler, NULL),
    NODE_HANDLER(AST_COMMON_STATEMENT, common_statement_handler, NULL),
    NODE_HANDLER(AST_COMPLEX_LITERAL, complex_literal_handler, NULL),
    NODE_HANDLER(AST_COMPLEX_TYPE, complex_type_handler, NULL),
    NODE_HANDLER(AST_CLASS_MEMBER_ACCESS, component_ref_handler, NULL),
    NODE_HANDLER(AST_COMPUTED_GOTO_STATEMENT, computed_goto_statement_handler, NULL),
    NODE_HANDLER(AST_COMPOUND_STATEMENT, compound_statement_handler, NULL),
    NODE_HANDLER(AST_CONCAT, binary_operator_handler, "//"),
    NODE_HANDLER(AST_CONTINUE_STATEMENT, cycle_statement_handler, NULL),
    NODE_HANDLER(AST_CRITICAL_CONSTRUCT, critical_construct_handler, NULL),
    NODE_HANDLER(AST_CRITICAL_STATEMENT, critical_statement_handler, NULL),
    NODE_HANDLER(AST_DATA_COMPONENT_DEF_STATEMENT, data_component_def_statement_handler, NULL),
    NODE_HANDLER(AST_DATA_STATEMENT, data_statement_handler, NULL),
    NODE_HANDLER(AST_DATA_STATEMENT_SET, data_statement_set_handler, NULL),
    NODE_HANDLER(AST_DEALLOCATE_STATEMENT, deallocate_statement_handler, NULL),
    NODE_HANDLER(AST_DECIMAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_DECLARATION, declaration_handler, NULL),
    NODE_HANDLER(AST_DECLARATION_SPECS, declaration_specs_handler, NULL),
    NODE_HANDLER(AST_DECLARATION_STATEMENT, declaration_statement_handler, NULL),
    NODE_HANDLER(AST_DEFAULT_STATEMENT, default_statement_handler, NULL),
    NODE_HANDLER(AST_DERIVED_TYPE_CONSTRUCTOR, derived_type_constructor_handler, NULL),
    NODE_HANDLER(AST_DERIVED_TYPE_DEF_BODY, derived_type_def_body_handler, NULL),
    NODE_HANDLER(AST_DERIVED_TYPE_DEF, derived_type_def_handler, NULL),
    NODE_HANDLER(AST_DERIVED_TYPE_NAME, derived_type_name_handler, NULL),
    NODE_HANDLER(AST_DERIVED_TYPE_STATEMENT, derived_type_statement_handler, NULL),
    NODE_HANDLER(AST_DIFFERENT, binary_operator_handler, "/="),
    NODE_HANDLER(AST_DIMENSION_DECL, dimension_decl_handler, NULL),
    NODE_HANDLER(AST_DIMENSION_STATEMENT, dimension_statement_handler, NULL),
    NODE_HANDLER(AST_DIV, binary_operator_handler, "/"),
    NODE_HANDLER(AST_DOUBLE_TYPE, double_type_handler, NULL),
    NODE_HANDLER(AST_ELSEWHERE_STATEMENT, elsewhere_statement_handler, NULL),
    NODE_HANDLER(AST_EMPTY_STATEMENT, continue_statement_handler, NULL),
    NODE_HANDLER(AST_END_STATEMENT, end_statement_handler, NULL),
    NODE_HANDLER(AST_ENTRY_STATEMENT, entry_statement_handler, NULL),
    NODE_HANDLER(AST_ENUM_DEF, enum_def_handler, NULL),
    NODE_HANDLER(AST_ENUM_DEF_STATEMENT, enum_def_statement_handler, NULL),
    NODE_HANDLER(AST_ENUMERATOR_DEF, enumerator_def_handler, NULL),
    NODE_HANDLER(AST_EQUAL, binary_operator_handler, "=="),
    NODE_HANDLER(AST_EQUIVALENCE_SET, equivalence_set_handler, NULL),
    NODE_HANDLER(AST_EQUIVALENCE_STATEMENT, equivalence_statement_handler, NULL),
    NODE_HANDLER(AST_EXPRESSION_STATEMENT, expression_statement_handler, NULL),
    NODE_HANDLER(AST_EXTERNAL_STATEMENT, external_statement_handler, NULL),
    NODE_HANDLER(AST_FINAL_STATEMENT, final_statement_handler, NULL),
    NODE_HANDLER(AST_FLOATING_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_FLOAT_TYPE, float_type_handler, NULL),
    NODE_HANDLER(AST_FORALL_CONSTRUCT, forall_construct_handler, NULL),
    NODE_HANDLER(AST_FORALL_CONSTRUCT_STATEMENT, forall_construct_statement_handler, NULL),
    NODE_HANDLER(AST_FORALL_HEADER, forall_header_handler, NULL),
    NODE_HANDLER(AST_FORALL_STATEMENT, forall_statement_handler, NULL),
    NODE_HANDLER(AST_FORALL_TRIPLET_SPEC, forall_triplet_spec_handler, NULL),
    NODE_HANDLER(AST_FORMAT_STATEMENT, format_statement_handler, NULL),
    NODE_HANDLER(AST_FOR_STATEMENT, do_loop_statement_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_CALL, function_call_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_PROGRAM_UNIT, function_program_unit_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_PROTOTYPE, function_prototype_handler, NULL),
    NODE_HANDLER(AST_FUNCTION_STATEMENT, function_statement_handler, NULL),
    NODE_HANDLER(AST_GOTO_STATEMENT, goto_statement_handler, NULL),
    NODE_HANDLER(AST_GREATER_OR_EQUAL_THAN, binary_operator_handler, ">="),
    NODE_HANDLER(AST_GREATER_THAN, binary_operator_handler, ">"),
    NODE_HANDLER(AST_HEXADECIMAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_IF_ELSE_STATEMENT, if_then_statement_handler, NULL),
    NODE_HANDLER(AST_IMAGE_REF, image_ref_handler, NULL),
    NODE_HANDLER(AST_IMPLICIT_SPEC, implicit_spec_handler, NULL),
    NODE_HANDLER(AST_IMPLICIT_STATEMENT, implicit_statement_handler, NULL),
    NODE_HANDLER(AST_IMPLIED_DO_CONTROL, implied_do_control_handler, NULL),
    NODE_HANDLER(AST_IMPLIED_DO, implied_do_handler, NULL),
    NODE_HANDLER(AST_IMPORT_STATEMENT, import_statement_handler, NULL),
    NODE_HANDLER(AST_INTENT_STATEMENT, intent_statement_handler, NULL),
    NODE_HANDLER(AST_INTERFACE_BLOCK, interface_block_handler, NULL),
    NODE_HANDLER(AST_INTERFACE_STATEMENT, interface_statement_handler, NULL),
    NODE_HANDLER(AST_INTRINSIC_STATEMENT, intrinsic_statement_handler, NULL),
    NODE_HANDLER(AST_INT_TYPE, int_type_handler, NULL),
    NODE_HANDLER(AST_IO_SPEC, io_spec_handler, NULL),
    NODE_HANDLER(AST_IO_STATEMENT, io_statement_handler, NULL),
    NODE_HANDLER(AST_LABEL_ASSIGN_STATEMENT, label_assign_statement_handler, NULL),
    NODE_HANDLER(AST_LABELED_STATEMENT, labeled_statement_handler, NULL),
    NODE_HANDLER(AST_LETTER_SPEC, letter_spec_handler, NULL),
    NODE_HANDLER(AST_LOCK_STATEMENT, lock_statement_handler, NULL),
    NODE_HANDLER(AST_LOGICAL_AND, binary_operator_handler, ".AND."),
    NODE_HANDLER(AST_LOGICAL_EQUAL, binary_operator_handler, ".EQV."),
    NODE_HANDLER(AST_LOGICAL_OR, binary_operator_handler, ".OR."),
    NODE_HANDLER(AST_LOOP_CONTROL, loop_control_handler, NULL),
    NODE_HANDLER(AST_LOWER_OR_EQUAL_THAN, binary_operator_handler, "<="),
    NODE_HANDLER(AST_LOWER_THAN, binary_operator_handler, "<"),
    NODE_HANDLER(AST_MAIN_PROGRAM_UNIT, main_program_unit_handler, NULL),
    NODE_HANDLER(AST_MASK_ELSEWHERE_PART, mask_elsewhere_part_handler, NULL),
    NODE_HANDLER(AST_MINUS, binary_operator_handler, "-"),
    NODE_HANDLER(AST_MODULE_PROCEDURE, module_procedure_handler, NULL),
    NODE_HANDLER(AST_MODULE_PROCEDURE_PROGRAM_UNIT, module_procedure_program_unit_handler, NULL),
    NODE_HANDLER(AST_MODULE_PROCEDURE_STATEMENT, module_procedure_statement_handler, NULL),
    NODE_HANDLER(AST_MODULE_PROGRAM_UNIT, module_program_unit_handler, NULL),
    NODE_HANDLER(AST_MODULE_STATEMENT, module_statement_handler, NULL),
    NODE_HANDLER(AST_MUL, binary_operator_handler, "*"),
    NODE_HANDLER(AST_NAMED_PAIR_SPEC, named_pair_spec_handler, NULL),
    NODE_HANDLER(AST_NAMELIST_ITEM, namelist_item_handler, NULL),
    NODE_HANDLER(AST_NAMELIST_STATEMENT, namelist_statement_handler, NULL),
    NODE_HANDLER(AST_NEG, unary_operator_handler, "-"),
    NODE_HANDLER(AST_NODE_LIST, sequence_handler, NULL),
    NODE_HANDLER(AST_LOGICAL_NOT, unary_operator_handler, ".NOT."),
    NODE_HANDLER(AST_NULLIFY_STATEMENT, nullify_statement_handler, NULL),
    NODE_HANDLER(AST_OCTAL_LITERAL, simple_text_handler, NULL),
    NODE_HANDLER(AST_OPEN_STATEMENT, open_statement_handler, NULL),
    NODE_HANDLER(AST_OPERATOR_NAME, operator_name_handler, NULL),
    NODE_HANDLER(AST_OPTIONAL_STATEMENT, optional_statement_handler, NULL),
    NODE_HANDLER(AST_OPT_VALUE, opt_value_handler, NULL),
    NODE_HANDLER(AST_PARAMETER_STATEMENT, parameter_statement_handler, NULL),
    NODE_HANDLER(AST_PARENTHESIZED_EXPRESSION, parenthesized_expression_handler, NULL),
    NODE_HANDLER(AST_PAUSE_STATEMENT, pause_statement_handler, NULL),
    NODE_HANDLER(AST_PIXEL_TYPE, pixel_type_handler, NULL),
    NODE_HANDLER(AST_PLUS, unary_operator_handler, "+"),
    NODE_HANDLER(AST_POINTER_INITIALIZATION, pointer_initialization_handler, NULL),
    NODE_HANDLER(AST_POINTER_STATEMENT, pointer_statement_handler, NULL),
    NODE_HANDLER(AST_CRAY_POINTER_STATEMENT, cray_pointer_statement_handler, NULL),
    NODE_HANDLER(AST_CRAY_POINTER_SPEC, cray_pointer_spec_handler, NULL),
    NODE_HANDLER(AST_POWER, binary_operator_handler, "**"),
    NODE_HANDLER(AST_PRINT_STATEMENT, print_statement_handler, NULL),
    NODE_HANDLER(AST_PROC_COMPONENT_DEF_STATEMENT, proc_component_def_statement_handler, NULL),
    NODE_HANDLER(AST_PROCEDURE_DECL_STATEMENT, procedure_decl_statement_handler, NULL),
    NODE_HANDLER(AST_PROCEDURE_SUFFIX, procedure_suffix_handler, NULL),
    NODE_HANDLER(AST_PROGRAM_STATEMENT, program_statement_handler, NULL),
    NODE_HANDLER(AST_PROTECTED_STATEMENT, protected_statement_handler, NULL),
    NODE_HANDLER(AST_PTR_ASSIGNMENT, binary_operator_handler, "=>"),
    NODE_HANDLER(AST_READ_STATEMENT, read_statement_handler, NULL),
    NODE_HANDLER(AST_RENAME, rename_handler, NULL),
    NODE_HANDLER(AST_RETURN_STATEMENT, return_statement_handler, NULL),
    NODE_HANDLER(AST_SAVE_STATEMENT, save_statement_handler, NULL),
    NODE_HANDLER(AST_SELECT_TYPE_CONSTRUCT, select_type_construct_handler, NULL),
    NODE_HANDLER(AST_SELECT_TYPE_STATEMENT, select_type_statement_handler, NULL),
    NODE_HANDLER(AST_SEQUENCE_STATEMENT, sequence_statement_handler, NULL),
    NODE_HANDLER(AST_STATEMENT_FUNCTION_STATEMENT, statement_function_statement_handler, NULL),
    NODE_HANDLER(AST_STOP_STATEMENT, stop_statement_handler, NULL),
    NODE_HANDLER(AST_STRING_LITERAL, symbol_handler, NULL),
    NODE_HANDLER(AST_SUBMODULE_PROGRAM_UNIT, submodule_program_unit_handler, NULL),
    NODE_HANDLER(AST_SUBMODULE_STATEMENT, submodule_statement_handler, NULL),
    NODE_HANDLER(AST_SUBROUTINE_PROGRAM_UNIT, subroutine_program_unit_handler, NULL),
    NODE_HANDLER(AST_SUBROUTINE_STATEMENT, subroutine_statement_handler, NULL),
    NODE_HANDLER(AST_SUBSCRIPT_TRIPLET, subscript_triplet_handler, NULL),
    NODE_HANDLER(AST_SWITCH_STATEMENT, select_case_construct_handler, NULL),
    NODE_HANDLER(AST_SYMBOL, symbol_handler, NULL),
    NODE_HANDLER(AST_SYNC_ALL_STATEMENT, sync_all_statement_handler, NULL),
    NODE_HANDLER(AST_SYNC_IMAGES_STATEMENT, sync_images_statement_handler, NULL),
    NODE_HANDLER(AST_SYNC_MEMORY_STATEMENT, sync_memory_statement_handler, NULL),
    NODE_HANDLER(AST_TARGET_STATEMENT, target_statement_handler, NULL),
    NODE_HANDLER(AST_TRANSLATION_UNIT, unary_container_handler, NULL),
    NODE_HANDLER(AST_TYPE_BOUND_GENERIC_PROCEDURE, type_bound_generic_procedure_handler, NULL),
    NODE_HANDLER(AST_TYPE_BOUND_PROCEDURE_PART, type_bound_procedure_part_handler, NULL),
    NODE_HANDLER(AST_TYPE_BOUND_PROCEDURE_STATEMENT, type_bound_procedure_statement_handler, NULL),
    NODE_HANDLER(AST_TYPE_GUARD_PART, type_guard_part_handler, NULL),
    NODE_HANDLER(AST_TYPE_IS_STATEMENT, type_is_statement_handler, NULL),
    NODE_HANDLER(AST_TYPE_NAME, type_name_handler, NULL),
    NODE_HANDLER(AST_TYPE_PARAMETER_DEF_STATEMENT, type_parameter_def_statement_handler, NULL),
    NODE_HANDLER(AST_UNLOCK_STATEMENT, unlock_statement_handler, NULL),
    NODE_HANDLER(AST_USE_ONLY_STATEMENT, use_only_statement_handler, NULL),
    NODE_HANDLER(AST_USER_DEFINED_BINARY_OP, user_defined_binary_op_handler, NULL),
    NODE_HANDLER(AST_USER_DEFINED_UNARY_OP, user_defined_unary_op_handler, NULL),
    NODE_HANDLER(AST_USE_STATEMENT, use_statement_handler, NULL),
    NODE_HANDLER(AST_VALUE_STATEMENT, value_statement_handler, NULL),
    NODE_HANDLER(AST_VECTOR_TYPE, vector_type_handler, NULL),
    NODE_HANDLER(AST_VOLATILE_STATEMENT, volatile_statement_handler, NULL),
    NODE_HANDLER(AST_WAIT_STATEMENT, wait_statement_handler, NULL),
    NODE_HANDLER(AST_WHERE_CONSTRUCT_BODY, where_construct_body_handler, NULL),
    NODE_HANDLER(AST_WHERE_CONSTRUCT_STATEMENT, where_construct_statement_handler, NULL),
    NODE_HANDLER(AST_WHERE_CONSTRUCT, where_construct_handler, NULL),
    NODE_HANDLER(AST_WHERE_STATEMENT, where_statement_handler, NULL),
    NODE_HANDLER(AST_WHILE_STATEMENT, do_loop_statement_handler, NULL),
    NODE_HANDLER(AST_WRITE_STATEMENT, write_statement_handler, NULL),
    // Pragma custom
    NODE_HANDLER(AST_UNKNOWN_PRAGMA, unknown_pragma_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_DIRECTIVE, pragma_custom_directive_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CONSTRUCT, pragma_custom_construct_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_CLAUSE, pragma_custom_clause_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CUSTOM_LINE, pragma_custom_line_handler, NULL),
    NODE_HANDLER(AST_PRAGMA_CLAUSE_ARG, literal_text_handler, NULL),
};

static void prettyprint_level(FILE *f, AST a, prettyprint_context_t* pt_ctx);

void fortran_prettyprint(FILE* f, AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);
    prettyprint_level(f, a, &pt_ctx);
}

const char* fortran_prettyprint_in_buffer(AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    return prettyprint_in_buffer_common(a, prettyprint_level, &pt_ctx);
}

const char* fortran_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    pt_ctx.callback = callback;
    pt_ctx.callback_data = data;

    return prettyprint_in_buffer_common(a, prettyprint_level, &pt_ctx);
}

const char* fortran_prettyprint_in_buffer(AST a);

const char* fortran_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data);

static void ambiguity_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    // Print the first ambiguity because all "look like" the same, no matter
    // which one is actually printed
    if (ASTText(a) == NULL
            || strcasecmp(ASTText(a), "IO_CONTROL") != 0)
    {
        prettyprint_level(f, ast_get_ambiguity(a, 0), pt_ctx);
    }
    // This is a special case for io_control_spec, we do not know the exact parameter, so, do not print it :)
    else
    {
        AST ambig = ast_get_ambiguity(a, 0);
        prettyprint_level(f, ASTSon0(ambig), pt_ctx);
    }
}

static void unary_container_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void literal_text_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "%s", ASTText(a));
}

static void simple_text_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "%s", strtoupper(ASTText(a)));
}

static void prefix_with_parameter_then_son_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void binary_operator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " %s ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void unary_operator_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s ", HELPER_PARAMETER_STRING);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void increase_level(prettyprint_context_t *pt_ctx)
{
    pt_ctx->level++;
}

static void prettyprint_level(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (a == NULL)
        return;

    prettyprint_handler_t hnd = handlers_list[ASTKind(a)].handler;

    if (hnd == NULL)
    {
        fprintf(stderr, "Node '%s' has NULL handler\n", ast_node_names[ASTKind(a)]);
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

static void character_separated_sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx, 
        const char* separator, prettyprint_handler_t specific_handler)
{
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

// Unused in Fortran
static void spaced_sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, " ", NULL);
}

static void list_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, ", ", NULL);
}

const char* fortran_list_handler_in_buffer(AST a)
{
    prettyprint_context_t pt_ctx;
    prettyprint_context_init(&pt_ctx);

    return prettyprint_in_buffer_common(a, list_handler, &pt_ctx);
}

static void sequence_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    character_separated_sequence_handler(f, a, pt_ctx, "", NULL);
}

static void indent_at_level(FILE* f, AST node, prettyprint_context_t* pt_ctx)
{
    int offset = pt_ctx->column;
    int real_offset = pt_ctx->level * strlen(pt_ctx->indent_str) - offset;
    
    if (real_offset > 0)
    {
        int times = real_offset / strlen(pt_ctx->indent_str);
        int remainder = real_offset % strlen(pt_ctx->indent_str);

        int i;
        for (i = 0; i < times; i++)
        {
            token_fprintf(f, node, pt_ctx, pt_ctx->indent_str);
        }

        for (i = 0; i < remainder; i++)
        {
            token_fprintf(f, node, pt_ctx, " ");
        }
    }
}

static void end_of_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "\n");
}

static void label_assign_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ASSIGN ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " TO ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void labeled_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s ", ASTText(ASTSon0(a)));
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}


static void access_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " :: ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void allocatable_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ALLOCATABLE :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void allocate_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ALLOCATE (");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " :: ");
    }
    list_handler(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon2(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void all_stop_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ALL STOP");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void arithmetic_if_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "IF (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ") ");
    AST info = ASTSon1(a);
    AST label_0 = ASTSon0(info);
    AST label_1 = ASTSon1(info);
    AST label_2 = ASTSon2(info);

    prettyprint_level(f, label_0, pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, label_1, pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, label_2, pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void array_constructor_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(/");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "/)");
}

static void array_constructor_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "::");
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, " ");
        }
    }
    list_handler(f, ASTSon1(a), pt_ctx);
}

static void array_ref_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void array_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    if ((ASTSon0(a) == NULL
                && ASTSon1(a) == NULL)
            || ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ":");
    }
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void associate_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);

    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void associate_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "ASSOCIATE(");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void assigned_goto_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "GOTO ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", (");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void asynchronous_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ASYNCHRONOUS");
    token_fprintf(f, a, pt_ctx, "::");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void attr_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTText(a)[0] != '_')
    {
        if (strcmp(strtolower(ASTText(a)), "bind") == 0)
        {
            // This is unfortunate
            bind_c_spec_handler(f, a, pt_ctx);
        }
        else
        {
            token_fprintf(f, a, pt_ctx, "%s", strtoupper(ASTText(a)));
            if (ASTSon0(a) != NULL)
            {
                token_fprintf(f, a, pt_ctx, "(");
                if (ASTKind(ASTSon0(a)) == AST_NODE_LIST)
                {
                    list_handler(f, ASTSon0(a), pt_ctx);
                }
                else
                {
                    prettyprint_level(f, ASTSon0(a), pt_ctx);
                }
                token_fprintf(f, a, pt_ctx, ")");
            }
        }
    }
    else
    {
        // Special ones used as placeholders
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void bind_c_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) == NULL)
    {
        token_fprintf(f, a, pt_ctx, "BIND (C)");
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "BIND (C, ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void binding_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "::");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void block_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void block_data_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void block_data_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "BLOCK DATA");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void block_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "BLOCK");
    end_of_statement_handler(f, a, pt_ctx);
}

static void body_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);

        indent_at_level(f, a, pt_ctx);
        token_fprintf(f, a, pt_ctx, "CONTAINS");
        end_of_statement_handler(f, a, pt_ctx);

        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    else
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void bool_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "LOGICAL");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(KIND = ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void case_selector_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) == NULL)
    {
        token_fprintf(f, a, pt_ctx, "DEFAULT");
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "(");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void case_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CASE ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void default_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CASE DEFAULT");
    end_of_statement_handler(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void case_value_range_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ":");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void character_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "CHARACTER");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void char_selector_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "LEN = ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        if (ASTSon1(a) != NULL)
        {
            token_fprintf(f, a, pt_ctx, ", ");
        }
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "KIND = ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void class_default_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CLASS DEFAULT");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void class_is_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CLASS IS (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void class_name_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "CLASS(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void close_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CLOSE(");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void codimension_decl_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "[");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "]");
}

static void codimension_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CODIMENSION :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void common_block_item_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "/ ");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
    token_fprintf(f, a, pt_ctx, "/ ");
    list_handler(f, ASTSon1(a), pt_ctx);
}

static void common_name_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "/ ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " /");
}

static void common_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "COMMON ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void complex_literal_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void complex_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    AST subtype = ASTSon0(a);
    if (subtype != NULL
            && ASTKind(subtype) == AST_DOUBLE_TYPE)
    {
        token_fprintf(f, a, pt_ctx, "DOUBLE ");
    }
    token_fprintf(f, a, pt_ctx, "COMPLEX");
    if (subtype != NULL
            && ASTKind(subtype) != AST_DOUBLE_TYPE
            && (ASTKind(subtype) != AST_FLOAT_TYPE 
                || ASTSon0(subtype) != NULL))
    {
        token_fprintf(f, a, pt_ctx, "(KIND = ");
        if (ASTKind(subtype) == AST_DECIMAL_LITERAL)
        {
            prettyprint_level(f, subtype, pt_ctx);
        }
        else
        {
            prettyprint_level(f, ASTSon0(subtype), pt_ctx);
        }
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void component_ref_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "%%");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void computed_goto_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "GOTO (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ") ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void compound_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void continue_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CONTINUE");
    end_of_statement_handler(f, a, pt_ctx);
}

static void critical_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void critical_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "CRITICAL");
    end_of_statement_handler(f, a, pt_ctx);
}

static void cycle_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "CYCLE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void data_component_def_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void data_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "DATA");
    token_fprintf(f, a, pt_ctx, " ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void data_statement_set_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " / ");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " / ");
}

static void deallocate_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "DEALLOCATE(");
    list_handler(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void declaration_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void declaration_specs_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (a == NULL)
        return;
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "[");
        list_handler(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "]");
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " * (");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    if (ASTSon3(a) != NULL)
    {
        if (ASTKind(ASTSon3(a)) != AST_POINTER_INITIALIZATION)
        {
            token_fprintf(f, a, pt_ctx, " = ");
            prettyprint_level(f, ASTSon3(a), pt_ctx);
        }
        else
        {
            prettyprint_level(f, ASTSon3(a), pt_ctx);
        }
    }
}

static void declaration_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void derived_type_constructor_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
}

static void derived_type_def_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void derived_type_def_body_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    prettyprint_level(f, ASTSon3(a), pt_ctx);
}

static void derived_type_name_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void derived_type_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "TYPE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon2(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void dimension_decl_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "[");
        list_handler(f, ASTSon2(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "]");
    }
}

static void dimension_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "DIMENSION :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void do_loop_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "DO");

    // Do not print the label
    // if (ASTSon3(a) != NULL)
    // {
    //     token_fprintf(f, a, pt_ctx, " ");
    //     prettyprint_level(f, ASTSon3(a), pt_ctx);
    // }

    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        if (ASTKind(a) == AST_WHILE_STATEMENT)
        {
            token_fprintf(f, a, pt_ctx, "WHILE (");
            prettyprint_level(f, ASTSon0(a), pt_ctx);
            token_fprintf(f, a, pt_ctx, ")");
        }
        else
        {
            prettyprint_level(f, ASTSon0(a), pt_ctx);
        }
    }
    end_of_statement_handler(f, a, pt_ctx);

    if (ASTSon1(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    else
    {
        indent_at_level(f, a, pt_ctx);
        token_fprintf(f, a, pt_ctx, "END DO");
        end_of_statement_handler(f, a, pt_ctx);
    }
}

static void double_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "DOUBLE PRECISION");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " * ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
}

static void elsewhere_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ELSEWHERE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void end_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    const char* text = ASTText(a);
    if (text != NULL)
    {
        if (strlen(text) >= 3
                && strncasecmp(text, "end", 3) == 0)
        {
            // Disregard the 'end'
            text += 3;
        }
        token_fprintf(f, a, pt_ctx, "END %s", strtoupper(text));
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "END");
    }
    
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void entry_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ENTRY ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " (");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void enum_def_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void enum_def_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "%s", strtoupper(ASTText(a)));
    end_of_statement_handler(f, a, pt_ctx);
}

static void enumerator_def_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "ENUMERATOR :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void equivalence_set_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void equivalence_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "EQUIVALENCE ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void exit_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "EXIT");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void expression_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTKind(ASTSon0(a)) == AST_FUNCTION_CALL)
    {
        token_fprintf(f, a, pt_ctx, "CALL ");
    }
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void external_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "EXTERNAL :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void final_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "FINAL :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void float_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "REAL");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(KIND = ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void forall_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon0(a), new_ctx);
    }
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void forall_construct_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "FORALL");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void forall_header_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, "::");
    }
    list_handler(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void forall_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "FORALL");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    // We end with a statement, no need for EOS
}

static void forall_triplet_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " = ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " : ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " : ");
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void format_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "FORMAT");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
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

static void function_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void function_prototype_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void function_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
    token_fprintf(f, a, pt_ctx, "FUNCTION ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void goto_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "GOTO ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void if_then_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTParent(a) != NULL)
    {
        AST parent = ASTParent(a);
        if (ASTKind(parent) == AST_LABELED_STATEMENT)
        {
            // do nothing
        }
        else if (ASTKind(parent) == AST_IF_ELSE_STATEMENT)
        {
            token_fprintf(f, a, pt_ctx, "ELSE");
        }
    }
    token_fprintf(f, a, pt_ctx, "IF (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL
            && ASTKind(ASTSon1(a)) != AST_COMPOUND_STATEMENT)
    {
        token_fprintf(f, a, pt_ctx, ") ");
    }
    else
    {
        token_fprintf(f, a, pt_ctx, ") THEN");
        end_of_statement_handler(f, a, pt_ctx);
    }

    if (ASTSon1(a) != NULL)
    {
        if (ASTKind(ASTSon1(a)) == AST_COMPOUND_STATEMENT)
        {
            NEW_PT_CONTEXT(new_ctx, increase_level);
            prettyprint_level(f, ASTSon1(a), new_ctx);
        }
        else
        {
            // One-line if-statement
            prettyprint_level(f, ASTSon1(a), pt_ctx);
        }
    }
    if (ASTSon2(a) != NULL)
    {
        AST else_tree = ASTSon2(a);
        if (ASTKind(else_tree) == AST_LABELED_STATEMENT)
            else_tree = ASTSon1(else_tree);

        indent_at_level(f, a, pt_ctx);
        if (ASTKind(else_tree) != AST_IF_ELSE_STATEMENT)
        {
            token_fprintf(f, a, pt_ctx, "ELSE");
            end_of_statement_handler(f, a, pt_ctx);

            NEW_PT_CONTEXT(new_ctx, increase_level);
            prettyprint_level(f, ASTSon2(a), new_ctx);
        }
        else
        {
            prettyprint_level(f, ASTSon2(a), pt_ctx);
        }
    }
    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void image_ref_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void implicit_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void implicit_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "IMPLICIT ");
    if (ASTSon0(a) == NULL)
    {
        token_fprintf(f, a, pt_ctx, "NONE");
    }
    else
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void implied_do_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void implied_do_control_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " = ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    if (ASTSon3(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon3(a), pt_ctx);
    }
}

static void import_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "IMPORT :: ");
    if (ASTSon0(a) != NULL)
    {
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void intent_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void interface_block_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void interface_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        // ABSTRACT
        token_fprintf(f, a, pt_ctx, "ABSTRACT ");
    }
    token_fprintf(f, a, pt_ctx, "INTERFACE");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void intrinsic_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "INTRINSIC :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void int_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "INTEGER");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(KIND = ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void io_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", strtoupper(ASTText(a)));
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void io_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "%s (", strtoupper(ASTText(a)));
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void letter_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "-");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
}

static void lock_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "LOCK(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void loop_control_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    // An empty loop control
    if (ASTSon0(a) == NULL
            && ASTSon1(a) == NULL
            && ASTSon2(a) == NULL)
        return;

    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void main_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void mask_elsewhere_part_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        NEW_PT_CONTEXT(new_ctx, increase_level);
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
}

static void module_procedure_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "MODULE PROCEDURE ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void module_procedure_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void module_procedure_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "MODULE PROCEDURE ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void module_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), new_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void module_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "MODULE ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void named_pair_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " = ");
    }
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void namelist_item_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    list_handler(f, ASTSon1(a), pt_ctx);
}

static void namelist_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "NAMELIST ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void nullify_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "NULLIFY (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void open_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "OPEN (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void operator_name_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (strcmp(ASTText(a), "=") != 0)
    {
        token_fprintf(f, a, pt_ctx, "OPERATOR(%s)", ASTText(a));
    }
    else
    {
        token_fprintf(f, a, pt_ctx, "ASSIGNMENT(%s)", ASTText(a));
    }
}

static void optional_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "OPTIONAL :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void opt_value_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s = ", strtoupper(ASTText(a)));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void parameter_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PARAMETER (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void parenthesized_expression_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void pause_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PAUSE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void pixel_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "PIXEL");
}

static void pointer_initialization_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, " => ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
}

static void cray_pointer_spec_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void cray_pointer_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "POINTER ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void pointer_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "POINTER :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void print_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PRINT ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void proc_component_def_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PROCEDURE (");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, "), ");
    list_handler(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void procedure_decl_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, " PROCEDURE (");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void procedure_suffix_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
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
        token_fprintf(f, a, pt_ctx, "RESULT (");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
}

static void program_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PROGRAM ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void protected_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PROTECTED :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void read_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "READ (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void rename_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " => ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void return_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "RETURN");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void save_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SAVE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " :: ");
        list_handler(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void select_case_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SELECT CASE(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);

    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);

    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

// static void select_case_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
// {
//     indent_at_level(f, a, pt_ctx);
//     if (ASTSon0(a) != NULL)
//     {
//         prettyprint_level(f, ASTSon0(a), pt_ctx);
//         token_fprintf(f, a, pt_ctx, ": ");
//     }
//     token_fprintf(f, a, pt_ctx, "SELECT CASE(");
//     prettyprint_level(f, ASTSon1(a), pt_ctx);
//     token_fprintf(f, a, pt_ctx, ")");
//     end_of_statement_handler(f, a, pt_ctx);
// }

static void select_type_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void select_type_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "SELECT TYPE (");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " => ");
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void sequence_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SEQUENCE");
    end_of_statement_handler(f, a, pt_ctx);
}

static void statement_function_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, "(");
    if (ASTSon1(a) != NULL)
    {
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ") = ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void stop_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "STOP");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void submodule_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void submodule_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SUBMODULE (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ") ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void subroutine_program_unit_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void subroutine_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " ");
    }
    token_fprintf(f, a, pt_ctx, "SUBROUTINE ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void subscript_triplet_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ":");
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ":");
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
}

static void symbol_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "%s", ASTText(a));
}

static void sync_all_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SYNC ALL");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void sync_images_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SYNC IMAGES");
    token_fprintf(f, a, pt_ctx, "(");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void sync_memory_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "SYNC MEMORY");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        list_handler(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void target_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "TARGET :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void type_bound_generic_procedure_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "GENERIC");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " => ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void type_bound_procedure_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "PROCEDURE");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, "(");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ")");
    }
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void type_bound_procedure_part_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL || ASTSon0(a) != NULL)
    {
        indent_at_level(f, a, pt_ctx);
        token_fprintf(f, a, pt_ctx, "CONTAINS");
        end_of_statement_handler(f, a, pt_ctx);
        NEW_PT_CONTEXT(new_ctx, increase_level);
        if (ASTSon0(a) != NULL)
        {
            prettyprint_level(f, ASTSon0(a), new_ctx);
        }
        if (ASTSon1(a) != NULL)
        {
            prettyprint_level(f, ASTSon1(a), new_ctx);
        }
    }
}

static void type_guard_part_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    NEW_PT_CONTEXT(new_ctx, increase_level);
    prettyprint_level(f, ASTSon1(a), new_ctx);
}

static void type_is_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "TYPE IS (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void type_name_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "TYPE (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void type_parameter_def_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "INTEGER");
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ", ");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " :: ");
    list_handler(f, ASTSon2(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void unlock_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "UNLOCK (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void use_only_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "USE ");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " :: ");
    }
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ", ONLY : "); 
    if (ASTSon2(a) != NULL)
    {
        list_handler(f, ASTSon2(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void user_defined_binary_op_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void user_defined_unary_op_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, " ", ASTText(a));
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void use_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "USE ");
    if (ASTSon0(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", ");
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, " :: ");
    }
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    if (ASTSon2(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, ", "); 
        list_handler(f, ASTSon2(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
}

static void value_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "VALUE :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void vector_type_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "VECTOR (");
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
}

static void volatile_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "VOLATILE :: ");
    list_handler(f, ASTSon0(a), pt_ctx);
    end_of_statement_handler(f, a, pt_ctx);
}

static void wait_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "WAIT (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void where_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    prettyprint_level(f, ASTSon2(a), pt_ctx);
}

static void where_construct_body_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    NEW_PT_CONTEXT(new_ctx, increase_level);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), new_ctx);
    }
    if (ASTSon1(a) != NULL)
    {
        prettyprint_level(f, ASTSon1(a), pt_ctx);
    }
    if (ASTSon2(a) != NULL)
    {
        prettyprint_level(f, ASTSon2(a), pt_ctx);
    }
    if (ASTSon3(a) != NULL)
    {
        prettyprint_level(f, ASTSon3(a), new_ctx);
    }
}

static void where_construct_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "WHERE (");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    end_of_statement_handler(f, a, pt_ctx);
}

static void where_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    if (ASTSon0(a) != NULL)
    {
        prettyprint_level(f, ASTSon0(a), pt_ctx);
        token_fprintf(f, a, pt_ctx, ": ");
    }
    token_fprintf(f, a, pt_ctx, "WHERE (");
    prettyprint_level(f, ASTSon1(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ") ");
    prettyprint_level(f, ASTSon2(a), pt_ctx);
    // We end with a statement, no need for EOS
}

static void write_statement_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    indent_at_level(f, a, pt_ctx);
    token_fprintf(f, a, pt_ctx, "WRITE (");
    list_handler(f, ASTSon0(a), pt_ctx);
    token_fprintf(f, a, pt_ctx, ")");
    if (ASTSon1(a) != NULL)
    {
        token_fprintf(f, a, pt_ctx, " ");
        list_handler(f, ASTSon1(a), pt_ctx);
    }
    end_of_statement_handler(f, a, pt_ctx);
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

static void unknown_pragma_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx UNUSED_PARAMETER)
{
    token_fprintf(f, a, pt_ctx, "%s\n", ASTText(a));
}

static void pragma_custom_directive_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "!$%s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);
}

static void pragma_custom_construct_handler(FILE* f, AST a, prettyprint_context_t* pt_ctx)
{
    token_fprintf(f, a, pt_ctx, "!$%s ", ASTText(a));
    prettyprint_level(f, ASTSon0(a), pt_ctx);
    prettyprint_level(f, ASTSon1(a), pt_ctx);

    // End part
    AST end = ASTSon2(a);
    if (end != NULL)
    {
        token_fprintf(f, a, pt_ctx, "!$%s %s ", ASTText(a), ASTText(end));
        prettyprint_level(f, ASTSon0(end), pt_ctx);
        end_of_statement_handler(f, a, pt_ctx);
    }
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
