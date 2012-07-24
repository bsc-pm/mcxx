#ifndef CXX_NODECL_DEEP_COPY_H
#define CXX_NODECL_DEEP_COPY_H

#include "cxx-nodecl.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

typedef struct symbol_map_tag symbol_map_t;

struct symbol_map_tag
{
    scope_entry_t* (*map)(symbol_map_t*, scope_entry_t*);
    void (*dtor)(symbol_map_t*);
};

nodecl_t nodecl_deep_copy(nodecl_t, decl_context_t, symbol_map_t*);

// Copies local entities like functions 
void copy_fortran_program_unit(scope_entry_t* new_program_unit,
        scope_entry_t* original_program_unit,
        symbol_map_t** out_symbol_map);

MCXX_END_DECLS

#endif // CXX_NODECL_DEEP_COPY_H
