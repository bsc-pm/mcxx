#ifndef CXX_SCOPELINK_H
#define CXX_SCOPELINK_H

#include "cxx-macros.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "hash.h"

MCXX_BEGIN_DECLS

typedef struct scope_link_tag
{
	Hash* h;
} scope_link_t;

scope_link_t* scope_link_new(void);
void scope_link_set(scope_link_t* sl, AST a, scope_t* st);
scope_t* scope_link_get(scope_link_t* sl, AST a);

MCXX_END_DECLS

#endif // CXX_SCOPELINK_H
