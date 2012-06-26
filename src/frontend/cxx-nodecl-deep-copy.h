#ifndef CXX_NODECL_DEEP_COPY_H
#define CXX_NODECL_DEEP_COPY_H

#include "cxx-nodecl.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

nodecl_t nodecl_deep_copy(nodecl_t, decl_context_t, void* info, scope_entry_t* (map)(scope_entry_t*, void*));

// Copies local entities like functions 
void copy_fortran_program_unit(scope_entry_t* new_program_unit,
        scope_entry_t* original_program_unit,
        void **out_info,
        scope_entry_t* (**out_map)(scope_entry_t*, void*),
        void (**free_fun)(void*));

MCXX_END_DECLS

#endif // CXX_NODECL_DEEP_COPY_H
