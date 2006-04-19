#ifndef CXX_BUILDSYMTAB_H
#define CXX_BUILDSYMTAB_H

#include <stdlib.h>
#include "cxx-ast.h"
#include "cxx-symtab.h"

void build_symtab_translation_unit(AST a);

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

char* name_from_id_expression(AST a);

#endif // CXX_BUILDSYMTAB_H
