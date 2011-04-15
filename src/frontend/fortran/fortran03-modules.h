#ifndef FORTRAN03_MODULES_H
#define FORTRAN03_MODULES_H

#include "cxx-scope-decls.h"

void dump_module_info(scope_entry_t* module);
void load_module_info(const char* module_name, scope_entry_t** module);

#endif // FORTRAN03_MODULES_H
