#ifndef CXX_NODECL_DEEP_COPY_H
#define CXX_NODECL_DEEP_COPY_H

#include "cxx-nodecl.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

struct symbol_map_tag
{
    scope_entry_t* (*map)(symbol_map_t*, scope_entry_t*);
    void (*dtor)(symbol_map_t*);
};

nodecl_t nodecl_deep_copy(nodecl_t, decl_context_t, symbol_map_t*);

typedef struct nested_symbol_map_tag nested_symbol_map_t;
nested_symbol_map_t* new_nested_symbol_map(symbol_map_t* enclosing_map);
void nested_map_add(nested_symbol_map_t* nested_symbol_map, scope_entry_t* source, scope_entry_t* target);

MCXX_END_DECLS

#endif // CXX_NODECL_DEEP_COPY_H
