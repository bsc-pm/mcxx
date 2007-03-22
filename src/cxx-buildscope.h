#ifndef CXX_BUILDSCOPE_H
#define CXX_BUILDSCOPE_H

#include "cxx-macros.h"
#include "cxx-buildscope-decls.h"
#include "cxx-driver-decls.h"

MCXX_BEGIN_DECLS

#include <stdlib.h>
#include "cxx-ast.h"

char* get_operator_function_name(AST declarator_id);
void build_scope_template_arguments(AST class_head_id, 
        scope_t* primary_template_scope,
        scope_t* arguments_scope, 
        scope_t* template_scope, 
        template_argument_list_t** template_arguments,
        decl_context_t decl_context);
void build_scope_decl_specifier_seq(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
        type_t** type_info, decl_context_t dctx);
scope_entry_t* build_scope_declarator(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
        type_t* type_info, type_t** declarator_type, decl_context_t dctx);

void gather_decl_spec_information(AST a, scope_t* st, gather_decl_spec_t* gather_info);
void gather_type_spec_information(AST a, scope_t* st, type_t* type_info, decl_context_t dctx);

void build_scope_member_specification(scope_t* inner_scope, AST member_specification_tree, 
        access_specifier_t current_access, type_t* simple_type_info, 
        decl_context_t decl_context);
void build_scope_base_clause(AST base_clause, scope_t* st, scope_t* class_scope, class_info_t* class_info,
        decl_context_t decl_context);

extern const decl_context_t default_decl_context;

AST get_declarator_name(AST a, scope_t* st, decl_context_t decl_context);
AST get_function_declarator_parameter_list(AST funct_declarator, scope_t* st, decl_context_t decl_context);
AST get_leftmost_declarator_name(AST a, decl_context_t decl_context);

char* get_conversion_function_name(AST conversion_function_id, scope_t* st, 
        type_t** result_conversion_type, decl_context_t decl_context);

void build_scope_dynamic_initializer(void);


// Needed for phases
void build_scope_translation_unit(translation_unit_t* translation_unit);
void build_scope_translation_unit_tree_with_global_scope(AST tree, scope_t* global_scope, scope_link_t* scope_link, decl_context_t decl_context);
void build_scope_declaration_sequence_with_scope_link(AST a, scope_t* st, decl_context_t decl_context, scope_link_t* scope_link);
void build_scope_statement_seq_with_scope_link(AST a, scope_t* st, decl_context_t decl_context, scope_link_t* scope_link);
void build_scope_member_specification_with_scope_link(scope_t* inner_scope, AST member_specification_tree, 
        access_specifier_t current_access, type_t* simple_type_info, 
        decl_context_t decl_context, scope_link_t* scope_link);

MCXX_END_DECLS

#endif // CXX_BUILDSCOPE_H
