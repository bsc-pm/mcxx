#ifndef CXX_SYMBOL_DEEP_COPY_H
#define CXX_SYMBOL_DEEP_COPY_H

#include "cxx-macros.h"
#include "cxx-nodecl-deep-copy-fwd.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

void symbol_deep_copy(
        scope_entry_t* dest,
        scope_entry_t* source,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map);

void symbol_deep_copy_compute_maps(
        scope_entry_t* dest,
        scope_entry_t* source,
        decl_context_t new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

MCXX_END_DECLS

#endif
