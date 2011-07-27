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



#ifndef CXX_BUILDSCOPE_H
#define CXX_BUILDSCOPE_H

#include <stdlib.h>

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-buildscope-decls.h"
#include "cxx-driver-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN const char* get_operator_function_name(struct AST_tag* declarator_id);
LIBMCXX_EXTERN void build_scope_template_parameters(
        decl_context_t lookup_context, 
        decl_context_t argument_context, 
        struct AST_tag* class_head_id, 
        template_parameter_list_t** template_parameters);
LIBMCXX_EXTERN void build_scope_decl_specifier_seq(struct AST_tag* a, gather_decl_spec_t* gather_info, 
        struct type_tag** type_info, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void compute_declarator_type(struct AST_tag* a, gather_decl_spec_t* gather_info,
        struct type_tag* type_info, 
        struct type_tag** declarator_type, 
        decl_context_t dctx,
        nodecl_t* nodecl_output);


LIBMCXX_EXTERN void build_scope_base_clause(struct AST_tag* base_clause, struct type_tag* class_type, decl_context_t decl_context);

LIBMCXX_EXTERN struct AST_tag* get_declarator_name(struct AST_tag* a, decl_context_t decl_context);
LIBMCXX_EXTERN struct AST_tag* get_declarator_id_expression(struct AST_tag* a, decl_context_t decl_context);
LIBMCXX_EXTERN struct AST_tag* get_function_declarator_parameter_list(struct AST_tag* funct_declarator, decl_context_t decl_context);
LIBMCXX_EXTERN struct AST_tag* get_leftmost_declarator_name(struct AST_tag* a, decl_context_t decl_context);

LIBMCXX_EXTERN char* get_conversion_function_name(decl_context_t decl_context, struct AST_tag* conversion_function_id, 
        struct type_tag** result_conversion_type, nodecl_t* nodecl_output);
LIBMCXX_EXTERN const char *get_operation_function_name(AST operation_tree);

LIBMCXX_EXTERN void build_scope_member_specification_first_step(decl_context_t inner_decl_context,
        AST member_specification_tree,
        access_specifier_t default_current_access,
        type_t* type_info,
        nodecl_t* nodecl_output,
        scope_entry_list_t** declared_symbols);

LIBMCXX_EXTERN void build_scope_dynamic_initializer(void);
LIBMCXX_EXTERN void build_scope_statement(struct AST_tag* statement, decl_context_t decl_context, nodecl_t* nodecl_output);

// Needed for phases
LIBMCXX_EXTERN void initialize_translation_unit_scope(translation_unit_t* translation_unit, decl_context_t* decl_context);
LIBMCXX_EXTERN void c_initialize_translation_unit_scope(translation_unit_t* translation_unit);

LIBMCXX_EXTERN void build_scope_translation_unit(translation_unit_t* translation_unit);
LIBMCXX_EXTERN void build_scope_translation_unit_tree_with_global_scope(struct AST_tag* tree, scope_link_t* scope_link, decl_context_t decl_context);
LIBMCXX_EXTERN void build_scope_declaration_sequence_with_scope_link(struct AST_tag* a, 
        decl_context_t decl_context, 
        scope_link_t* scope_link,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void build_scope_statement_seq_with_scope_link(struct AST_tag* a, 
        decl_context_t decl_context, 
        scope_link_t* scope_link,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void build_scope_member_specification_with_scope_link(
        decl_context_t class_context,
        scope_link_t* scope_link,
        struct AST_tag* member_specification_tree, 
        access_specifier_t current_access,
        struct type_tag* simple_type_info,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN scope_entry_t* build_scope_function_definition(AST a, scope_entry_t* previous_symbol, 
        decl_context_t decl_context, 
        char is_template,
        char is_explicit_instantiation,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void finish_class_type(struct type_tag* class_type, struct type_tag* type_info, decl_context_t decl_context,
        const char *filename, int line, nodecl_t* nodecl_output);

LIBMCXX_EXTERN void gather_type_spec_information(struct AST_tag* a, struct type_tag** type_info, 
        gather_decl_spec_t *gather_info, decl_context_t dctx, nodecl_t* nodecl_output);

LIBMCXX_EXTERN void enter_class_specifier(void);
LIBMCXX_EXTERN void leave_class_specifier(nodecl_t*);

LIBMCXX_EXTERN unsigned long long int buildscope_used_memory(void);

LIBMCXX_EXTERN AST internal_expression_parse(const char *source, decl_context_t decl_context);

LIBMCXX_EXTERN void build_scope_template_header(AST template_parameter_list, 
        decl_context_t decl_context, decl_context_t *template_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN scope_entry_t* entry_advance_aliases(scope_entry_t* entry);

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_H
