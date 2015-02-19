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

#ifndef CXX_TYPEUTILS_FWD_H
#define CXX_TYPEUTILS_FWD_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

struct type_tag;
typedef struct type_tag type_t;

struct parameter_info_tag;
typedef struct parameter_info_tag parameter_info_t;

struct standard_conversion_tag;
typedef struct standard_conversion_tag standard_conversion_t;

MCXX_END_DECLS

#endif // CXX_TYPEUTILS_FWD_H
