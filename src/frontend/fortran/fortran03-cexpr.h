/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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


#ifndef FORTRAN03_CEXPR_H
#define FORTRAN03_CEXPR_H

#include "cxx-cexpr.h"
#include "cxx-macros.h"
#include "libmf03-common.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN int fortran_flatten_array_count_elements(const_value_t* v);
LIBMF03_EXTERN const_value_t* fortran_flatten_array(const_value_t* v);

LIBMF03_EXTERN int fortran_flatten_array_count_elements_with_mask(const_value_t* v, const_value_t* mask);
LIBMF03_EXTERN const_value_t* fortran_flatten_array_with_mask(const_value_t* v, const_value_t* mask);

LIBMF03_EXTERN const_value_t* fortran_const_value_rank_zero(const_value_t* v);

MCXX_END_DECLS

#endif // FORTRAN03_CEXPR_H
