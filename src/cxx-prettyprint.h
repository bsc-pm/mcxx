#ifndef CXX_PRETTYPRINT_H
#define CXX_PRETTYPRINT_H

#include <stdio.h>
#include <gc.h>
#include "cxx-ast.h"

void prettyprint(FILE* f, AST a);
char* prettyprint_in_buffer(AST a);

#endif // CXX_PRETTYPRINT_H
