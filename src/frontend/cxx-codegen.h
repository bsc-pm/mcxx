#ifndef CXX_CODEGEN_H
#define CXX_CODEGEN_H

#include <stdio.h>
#include "cxx-ast.h"
#include "cxx-scopelink.h"
#include "libmcxx-common.h"

void c_cxx_codegen_translation_unit(FILE* f, nodecl_t a, scope_link_t* sl);
char* c_cxx_codegen_to_str(nodecl_t node);

#endif // CXX_CODEGEN_H
