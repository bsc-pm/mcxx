#ifndef FORTRAN03_TYPEUTILS_H
#define FORTRAN03_TYPEUTILS_H

#include "libmf03-common.h"
#include "cxx-typeutils.h"

LIBMF03_EXTERN const char* fortran_print_type_str(type_t*);

LIBMF03_EXTERN char is_pointer_to_array(type_t*);
LIBMF03_EXTERN int get_rank_of_type(type_t* t);
LIBMF03_EXTERN type_t* get_rank0_type(type_t* t);

#endif // FORTRAN03_TYPEUTILS_H
