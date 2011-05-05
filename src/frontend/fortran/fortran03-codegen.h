#ifndef FORTRAN03_CODEGEN_H
#define FORTRAN03_CODEGEN_H

#include <stdio.h>
#include "cxx-ast.h"
#include "cxx-scopelink.h"
#include "libmf03-common.h"

LIBMCXX_EXTERN void fortran_codegen_translation_unit(FILE* f, AST a, scope_link_t* sl);

#endif // FORTRAN03_CODEGEN_H

