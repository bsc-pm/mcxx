#ifndef FORTRAN03_MANGLING_H
#define FORTRAN03_MANGLING_H

#include "cxx-macros.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

typedef struct fortran_name_mangling_t fortran_name_mangling_t;

struct fortran_name_mangling_t
{
    const char* descriptor_id;
    const char* descriptor_name;
    const char* (*mangle)(scope_entry_t* entry);
};

LIBMCXX_EXTERN fortran_name_mangling_t* fortran_name_mangling_list[];
LIBMCXX_EXTERN fortran_name_mangling_t* default_fortran_name_mangling;

const char* fortran_mangle_symbol(scope_entry_t* entry);

MCXX_END_DECLS

#endif // FORTRAN03_MANGLING_H
