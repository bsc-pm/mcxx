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




#ifndef CXX_BUILDSCOPE_H
#define CXX_BUILDSCOPE_H

#include <stdlib.h>

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-buildscope-decls.h"
#include "cxx-instantiation-decls.h"
#include "cxx-driver-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN const char* get_operator_function_name(AST declarator_id);

LIBMCXX_EXTERN const char* get_literal_operator_name(const char* name);

LIBMCXX_EXTERN void build_scope_template_parameters(
        const decl_context_t* lookup_context,
        const decl_context_t* argument_context,
        AST class_head_id,
        template_parameter_list_t** template_parameters);
LIBMCXX_EXTERN void build_scope_decl_specifier_seq(AST a, gather_decl_spec_t* gather_info,
        struct type_tag** type_info,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void compute_declarator_type(AST a,
        gather_decl_spec_t* gather_info,
        struct type_tag* type_info,
        struct type_tag** declarator_type,
        const decl_context_t* dctx,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN char get_is_inside_pack_expansion(void);
LIBMCXX_EXTERN void set_is_inside_pack_expansion(char b);

LIBMCXX_EXTERN AST get_declarator_name(AST a, const decl_context_t* decl_context);
LIBMCXX_EXTERN AST get_declarator_id_expression(AST a, const decl_context_t* decl_context);
LIBMCXX_EXTERN AST get_function_declarator_parameter_list(AST funct_declarator, const decl_context_t* decl_context);

LIBMCXX_EXTERN AST get_declarator_id_pack(AST a, const decl_context_t* decl_context);
LIBMCXX_EXTERN char type_does_not_contain_any_template_parameter_pack(type_t* t, const locus_t* locus);

LIBMCXX_EXTERN const char* get_conversion_function_name(const decl_context_t* decl_context, AST conversion_function_id,
        struct type_tag** result_conversion_type);

LIBMCXX_EXTERN void build_scope_member_specification_first_step(const decl_context_t* inner_decl_context,
        AST member_specification_tree,
        access_specifier_t default_current_access,
        type_t* type_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t* gather_decl_spec_list);

LIBMCXX_EXTERN void build_scope_dynamic_initializer(void);
LIBMCXX_EXTERN void build_scope_statement(AST statement, const decl_context_t* decl_context, nodecl_t* nodecl_output);

// Needed for phases
LIBMCXX_EXTERN void initialize_translation_unit_scope(translation_unit_t* translation_unit, const decl_context_t** decl_context);
LIBMCXX_EXTERN void c_initialize_translation_unit_scope(translation_unit_t* translation_unit);

LIBMCXX_EXTERN void c_initialize_builtin_symbols(const decl_context_t* decl_context);

LIBMCXX_EXTERN nodecl_t build_scope_translation_unit(translation_unit_t* translation_unit);

LIBMCXX_EXTERN void build_scope_declaration_sequence(AST list,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output_list);
LIBMCXX_EXTERN void build_scope_declaration(AST a,
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output, 
        scope_entry_list_t** declared_symbols,
        gather_decl_spec_list_t *gather_decl_spec_list);


LIBMCXX_EXTERN void hide_using_declarations(type_t* class_info, scope_entry_t* currently_declared);

LIBMCXX_EXTERN void finish_class_type(struct type_tag* class_type, struct type_tag* type_info, const decl_context_t* decl_context,
        const locus_t* locus, nodecl_t* nodecl_output);

LIBMCXX_EXTERN scope_entry_t* finish_anonymous_class(scope_entry_t* class_symbol, const decl_context_t* decl_context);

LIBMCXX_EXTERN void gather_type_spec_information(AST a, struct type_tag** type_info,
        gather_decl_spec_t *gather_info, const decl_context_t* dctx, nodecl_t* nodecl_output);

LIBMCXX_EXTERN void enter_class_specifier(void);
LIBMCXX_EXTERN void leave_class_specifier(nodecl_t*);

LIBMCXX_EXTERN nodecl_t internal_expression_parse(const char *source, const decl_context_t* decl_context);

LIBMCXX_EXTERN void build_scope_template_header(AST template_parameter_list,
        const decl_context_t* decl_context, decl_context_t* *template_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN scope_entry_t* entry_advance_aliases(scope_entry_t* entry);

LIBMCXX_EXTERN void insert_members_in_enclosing_nonanonymous_class(
        scope_entry_t* class_symbol,
        scope_entry_list_t* member_list);

LIBMCXX_EXTERN void introduce_using_entities_in_class(
        nodecl_t nodecl_name,
        scope_entry_list_t* used_entities,
        const decl_context_t* decl_context,
        scope_entry_t* current_class,
        access_specifier_t current_access,
        char is_typename,
        const locus_t* locus);

LIBMCXX_EXTERN scope_entry_t* get_function_declaration_proxy(void);

void build_scope_friend_declarator(const decl_context_t* decl_context,
        gather_decl_spec_t *gather_info,
        type_t* class_type,
        type_t* member_type,
        AST declarator);

LIBMCXX_EXTERN char function_is_copy_constructor(scope_entry_t* entry, type_t* class_type);
LIBMCXX_EXTERN char function_is_copy_assignment_operator(scope_entry_t* entry, type_t* class_type);
LIBMCXX_EXTERN char function_is_move_constructor(scope_entry_t* entry, type_t* class_type);
LIBMCXX_EXTERN char function_is_move_assignment_operator(scope_entry_t* entry, type_t* class_type);

LIBMCXX_EXTERN void set_function_type_for_lambda(type_t** declarator_type,
        gather_decl_spec_t* gather_info,
        AST parameters_and_qualifiers,
        const decl_context_t* decl_context,
        const decl_context_t* *lambda_block_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void push_extra_declaration_symbol(scope_entry_t* entry);
LIBMCXX_EXTERN scope_entry_t* pop_extra_declaration_symbol(void);

LIBMCXX_EXTERN void set_parameters_as_related_symbols(scope_entry_t* entry,
        gather_decl_spec_t* gather_info,
        char is_definition,
        const locus_t* locus);

LIBMCXX_EXTERN int get_vla_counter(void);

LIBMCXX_EXTERN nodecl_t instantiate_statement(nodecl_t orig_tree,
        const decl_context_t* orig_decl_context,
        const decl_context_t* new_decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map);

LIBMCXX_EXTERN nodecl_t flush_instantiated_entities(void);
LIBMCXX_EXTERN void push_instantiated_entity(scope_entry_t* entry);

// Only to be called from cxx-instantiation.c
nodecl_t instantiate_function_code(nodecl_t orig_tree,
        const decl_context_t* orig_decl_context,
        const decl_context_t* new_decl_context,
        scope_entry_t* orig_function_instantiated,
        scope_entry_t* new_function_instantiated,
        instantiation_symbol_map_t* instantiation_symbol_map);

type_t* compute_underlying_type_enum(
        const_value_t* min_value,
        const_value_t* max_value,
        type_t* underlying_type,
        char short_enums);

type_t* compute_type_of_decltype(
        AST a,
        const decl_context_t* decl_context);

void build_scope_friend_class_declaration(
        type_t* type_of_declaration,
        const char* declared_name,
        const decl_context_t* decl_context,
        const locus_t* locus);

void register_symbol_this_in_class_scope(scope_entry_t* class_entry);

// Only to be called from cxx-exprtype.c
char check_constexpr_function(scope_entry_t* entry, const locus_t* locus,
        char diagnose,
        char emit_error);
char check_constexpr_constructor(scope_entry_t* entry,
        const locus_t* locus,
        nodecl_t nodecl_initializer_list,
        char diagnose,
        char emit_error);
char check_constexpr_function_code(scope_entry_t* entry,
        nodecl_t nodecl_body,
        char diagnose,
        char emit_error);
scope_entry_t* add_label_if_not_found(const char* label_text, const decl_context_t* decl_context, const locus_t* locus);

void check_nodecl_member_initializer_list(
        nodecl_t nodecl_cxx_member_init_list,
        scope_entry_t* function_entry,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output);

void register_symbol_this(const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        const locus_t* locus);

void update_symbol_this(scope_entry_t* entry,
        const decl_context_t* block_context);

void build_scope_nodecl_compound_statement(
        nodecl_t nodecl_statement_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_output);

void build_scope_nodecl_static_assert(nodecl_t nodecl_predicate,
        nodecl_t nodecl_message,
        const decl_context_t* decl_context,
        nodecl_t *nodecl_single_assert);

scope_entry_t* register_mercurium_pretty_print(scope_entry_t* entry, const decl_context_t* block_context);

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_H
