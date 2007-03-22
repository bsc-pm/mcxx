#ifndef CXX_SCOPELINK_H
#define CXX_SCOPELINK_H

#include "cxx-macros.h"
#include "cxx-scopelink-decls.h"
#include "cxx-buildscope.h"
#include "cxx-ast.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

scope_link_t* scope_link_new(void);

void scope_link_set(scope_link_t* sl, AST a, scope_t* st, decl_context_t decl_context);

scope_t* scope_link_get_scope(scope_link_t* sl, AST a);
decl_context_t scope_link_get_decl_context(scope_link_t* sl, AST a);

AST duplicate_ast_with_scope_link(AST a, scope_link_t* orig, scope_link_t* new_sl);

MCXX_END_DECLS

#endif // CXX_SCOPELINK_H
