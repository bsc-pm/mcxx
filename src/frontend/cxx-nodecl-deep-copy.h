#ifndef CXX_NODECL_DEEP_COPY_H
#define CXX_NODECL_DEEP_COPY_H

#include "cxx-nodecl.h"

nodecl_t nodecl_deep_copy(nodecl_t, decl_context_t, void* info, scope_entry_t* (map)(scope_entry_t*, void*));

#endif // CXX_NODECL_DEEP_COPY_H
