#ifndef FORTRAN03_TYPEENVIRON_H
#define FORTRAN03_TYPEENVIRON_H

#include "fortran03-typeenviron-decls.h"

#include "cxx-typeutils.h"

struct fortran_array_descriptor_t
{
    const char* descriptor_id;
    const char* descriptor_name;
    _size_t (*get_size)(type_t*, int rank);
    _size_t (*get_alignment)(type_t*, int rank);
};

LIBMCXX_EXTERN _size_t fortran_size_of_array_descriptor(type_t* t, int rank);
LIBMCXX_EXTERN _size_t fortran_alignment_of_array_descriptor(type_t* t, int rank);

LIBMCXX_EXTERN fortran_array_descriptor_t* fortran_array_descriptor_list[];
LIBMCXX_EXTERN fortran_array_descriptor_t* default_fortran_array_descriptor;

#endif // FORTRAN03_TYPEENVIRON_H
