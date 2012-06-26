#ifndef CXX_SYMBOL_DEEP_COPY_H
#define CXX_SYMBOL_DEEP_COPY_H

#include "cxx-scope.h"

void symbol_deep_copy(scope_entry_t* dest, scope_entry_t* source,
        decl_context_t new_decl_context,
        void *info, scope_entry_t* (*map)(scope_entry_t*, void*));

#endif
