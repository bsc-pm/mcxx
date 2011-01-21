#ifndef FORTRAN03_SCOPE_H
#define FORTRAN03_SCOPE_H

#include "cxx-scope-decls.h"
#include "fortran03-scope-decls.h"

decl_context_t new_program_unit_context(decl_context_t);

scope_entry_t* query_name(decl_context_t, const char* name);

#endif // FORTRAN03_SCOPE_H
