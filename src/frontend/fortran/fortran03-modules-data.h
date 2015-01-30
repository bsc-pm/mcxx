/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/


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
