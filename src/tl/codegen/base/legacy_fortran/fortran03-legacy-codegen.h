#ifndef FORTRAN03_CODEGEN_H
#define FORTRAN03_CODEGEN_H

#include <stdio.h>
#include "cxx-ast.h"
#include "libmf03-common.h"

// We need this for codegen_to_str
#include "cxx-compilerphases.hpp"

MCXX_BEGIN_DECLS

char* _fortran_codegen_to_str(nodecl_t node);
void _fortran_codegen_translation_unit(FILE* f, nodecl_t node);


MCXX_END_DECLS

#endif // FORTRAN03_CODEGEN_H

