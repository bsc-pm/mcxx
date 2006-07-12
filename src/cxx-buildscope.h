#ifndef CXX_BUILDSCOPE_H
#define CXX_BUILDSCOPE_H

#include <stdlib.h>
#include "cxx-ast.h"
#include "cxx-scope.h"

void build_scope_translation_unit(AST a);

typedef struct gather_decl_spec_tag {
	char is_auto;
	char is_register;
	char is_static;
	char is_extern;
	char is_mutable;
	char is_thread;
	char is_friend;
	char is_typedef;
	char is_signed;
	char is_unsigned;
	char is_short;
	char is_long;
	char is_const;
	char is_volatile;
	char is_inline;
	char is_virtual;
	char is_explicit;
} gather_decl_spec_t;

#define BITMAP(x) (1 << (x))

typedef enum decl_flags_tag
{
	DF_NONE = 0,
	DF_TEMPLATE = BITMAP(0),
	DF_CONSTRUCTOR = BITMAP(1),
	DF_NO_DECLARATORS = BITMAP(2),
	DF_FRIEND = BITMAP(3),
} decl_flags_t;

// Inherited attributes
typedef struct decl_context_tag
{
	// Several declaration flags
	decl_flags_t decl_flags;

	// Template nesting level
	int template_nesting;

	// Template parameter information
	template_parameter_t** template_param_info;
	int num_template_parameters;

	// Template argument information (for instantiation purposes)
	template_argument_list_t* template_argument_list;
} decl_context_t;

#undef BITMAP

char* get_operator_function_name(AST declarator_id);
void build_scope_template_arguments(AST class_head_id, 
		scope_t* primary_template_scope,
		scope_t* arguments_scope, 
		scope_t* template_scope, 
		template_argument_list_t** template_arguments);
void build_scope_decl_specifier_seq(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
		type_t** type_info, decl_context_t dctx);
scope_entry_t* build_scope_declarator(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
		type_t* type_info, type_t** declarator_type, decl_context_t dctx);

void gather_decl_spec_information(AST a, scope_t* st, gather_decl_spec_t* gather_info);
void gather_type_spec_information(AST a, scope_t* st, type_t* type_info, decl_context_t dctx);

void build_scope_member_specification(scope_t* inner_scope, AST member_specification_tree, 
		access_specifier_t current_access, type_t* simple_type_info, 
		decl_context_t decl_context);

extern const decl_context_t default_decl_context;

AST get_declarator_name(AST a);
AST get_leftmost_declarator_name(AST a);

#endif // CXX_BUILDSCOPE_H
