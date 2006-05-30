#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "cxx-ast.h"
#include "cxx-scope.h"

scope_entry_t* resolve_overload(scope_t* st, AST argument_list, scope_entry_list_t* candidate_functions);

#endif // CXX_OVERLOAD
