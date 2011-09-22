#ifndef FORTRAN03_CODEGEN_H
#define FORTRAN03_CODEGEN_H

#include <stdio.h>
#include "cxx-ast.h"
#include "libmf03-common.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void fortran_codegen_translation_unit(FILE* f, nodecl_t a);
LIBMCXX_EXTERN char* fortran_codegen_to_str(nodecl_t node);

MCXX_END_DECLS

#endif // FORTRAN03_CODEGEN_H

