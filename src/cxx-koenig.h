#ifndef CXX_KOENIG_H
#define CXX_KOENIG_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

scope_entry_list_t* lookup_unqualified_function(scope_t* st, char* name, AST arguments);

MCXX_END_DECLS

#endif // CXX_KOENIG_H
