#ifndef FORTRAN03_INTRINSICS_H
#define FORTRAN03_INTRINSICS_H

#include "cxx-scope-decls.h"

void fortran_init_intrinsics(decl_context_t decl_context);

scope_entry_t* fortran_intrinsic_solve_call(scope_entry_t* symbol, 
        const char** actual_arguments_keywords, 
        nodecl_t* nodecl_actual_arguments,
        int num_actual_arguments,
        nodecl_t* nodecl_simplified);

#endif // FORTRAN03_INTRINSICS_H
