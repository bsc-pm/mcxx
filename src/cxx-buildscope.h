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
	DF_CONSTRUCTOR = BITMAP(1)
} decl_flags_t;

#undef BITMAP

char* get_operator_function_name(AST declarator_id);
void build_scope_template_arguments(AST a, scope_t* st, template_argument_list_t** template_arguments);
void build_scope_decl_specifier_seq(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t** type_info, decl_flags_t decl_flags);
scope_entry_t* build_scope_declarator(AST a, scope_t* st, gather_decl_spec_t* gather_info, 
		simple_type_t* type_info, type_t** declarator_type, decl_flags_t decl_flags);

void gather_decl_spec_information(AST a, scope_t* st, gather_decl_spec_t* gather_info);
void gather_type_spec_information(AST a, scope_t* st, simple_type_t* type_info, decl_flags_t decl_flags);

#endif // CXX_BUILDSCOPE_H
