#ifndef FORTRAN95_SPLIT_H
#define FORTRAN95_SPLIT_H

#include "cxx-macros.h"
#include "libmf03-common.h"
#include <stdio.h>

MCXX_BEGIN_DECLS

void fortran_split_lines(FILE* input, FILE* output, int width);

MCXX_END_DECLS

#endif
