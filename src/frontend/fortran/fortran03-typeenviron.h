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


#ifndef FORTRAN03_TYPEENVIRON_H
#define FORTRAN03_TYPEENVIRON_H

#include "fortran03-typeenviron-decls.h"

#include "cxx-macros.h"
#include "cxx-typeutils.h"

MCXX_BEGIN_DECLS

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

MCXX_END_DECLS

#endif // FORTRAN03_TYPEENVIRON_H
