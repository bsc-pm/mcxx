#ifndef FORTRAN03_PRETTYPRINT_H
#define FORTRAN03_PRETTYPRINT_H

#include "cxx-prettyprint.h"
#include "cxx-ast.h"

void fortran_prettyprint(FILE* f, AST a);

const char* fortran_prettyprint_in_buffer(AST a);

char* fortran_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data);


#endif // FORTRAN03_PRETTYPRINT_H
