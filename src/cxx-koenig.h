#ifndef CXX_KOENIG_H
#define CXX_KOENIG_H

#include "cxx-ast.h"
#include "cxx-scope.h"

scope_entry_list_t* lookup_unqualified_function(scope_t* st, char* name, AST arguments);

#endif // CXX_KOENIG_H
