#ifndef FORTRAN03_MODULES_DATA_H
#define FORTRAN03_MODULES_DATA_H

#include "cxx-tltype.h"

// This is a generic structure used in TL to add arbitrary info to a module file

typedef struct fortran_modules_data_item_tag fortran_modules_data_item_t;

typedef struct fortran_modules_data_tag fortran_modules_data_t;
struct fortran_modules_data_tag
{
    const char* name;
    int num_items;
    tl_type_t* items;
};

struct fortran_modules_data_set_tag
{
    int num_data;
    fortran_modules_data_t **data;
};

#endif // FORTRAN03_MODULES_DATA_H
