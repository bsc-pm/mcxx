/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_BUILDSCOPE_H
#define CXX_BUILDSCOPE_H

#include "cxx-macros.h"
#include "cxx-buildscope-decls.h"
#include "cxx-driver-decls.h"

MCXX_BEGIN_DECLS

#include <stdlib.h>
#include "cxx-ast.h"

char* get_operator_function_name(AST declarator_id);
void build_scope_template_arguments(
        decl_context_t lookup_context, 
        decl_context_t argument_context, 
        AST class_head_id, 
        template_argument_list_t** template_arguments);
void build_scope_decl_specifier_seq(AST a, gather_decl_spec_t* gather_info, 
        type_t** type_info, decl_context_t dctx);
scope_entry_t* build_scope_declarator(AST a, gather_decl_spec_t* gather_info, 
        type_t* type_info, type_t** declarator_type, decl_context_t dctx) DEPRECATED;

void compute_declarator_type(AST a, gather_decl_spec_t* gather_info,
        type_t* type_info, type_t** declarator_type, decl_context_t dctx);

void gather_decl_spec_information(AST a, gather_decl_spec_t* gather_info);
void gather_type_spec_information(AST a, type_t** type_info, decl_context_t dctx);

void build_scope_member_specification(decl_context_t inner_decl_context, AST member_specification_tree, 
        access_specifier_t current_access, type_t* simple_type_info);
void build_scope_base_clause(AST base_clause, type_t* class_type, decl_context_t decl_context);

AST get_declarator_name(AST a, decl_context_t decl_context);
AST get_declarator_id_expression(AST a, decl_context_t decl_context);
AST get_function_declarator_parameter_list(AST funct_declarator, decl_context_t decl_context);
AST get_leftmost_declarator_name(AST a, decl_context_t decl_context);

char* get_conversion_function_name(decl_context_t decl_context, AST conversion_function_id, 
        type_t** result_conversion_type);

void build_scope_dynamic_initializer(void);
void build_scope_statement(AST statement, decl_context_t decl_context);

// Needed for phases
void build_scope_translation_unit(translation_unit_t* translation_unit);
void build_scope_translation_unit_tree_with_global_scope(AST tree, scope_link_t* scope_link, decl_context_t decl_context);
void build_scope_declaration_sequence_with_scope_link(AST a, decl_context_t decl_context, scope_link_t* scope_link);
void build_scope_statement_seq_with_scope_link(AST a, decl_context_t decl_context, scope_link_t* scope_link);
void build_scope_member_specification_with_scope_link(
        decl_context_t class_context,
        scope_link_t* scope_link,
        AST member_specification_tree, 
        access_specifier_t current_access,
        type_t* simple_type_info);

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_H
