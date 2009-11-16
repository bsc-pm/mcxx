/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef MEM_CTL_H
#define MEM_CTL_H

#include "libutils-common.h"
#include <stdlib.h>

#define NEW(type)               (type *) malloc(sizeof(type))
#define NEW_ARRAY(type,size)    (type *)calloc(size, sizeof(type))
#define FREE(ptr)               

#ifdef __cplusplus
extern "C" {
#endif

LIBUTILS_EXTERN void noop_free(void* v);

#ifdef __cplusplus
}
#endif

#endif
