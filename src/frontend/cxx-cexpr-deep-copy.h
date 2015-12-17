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



#ifndef CXX_CEXPR_DEEP_COPY_H
#define CXX_CEXPR_DEEP_COPY_H

#include "cxx-cexpr-decls.h"
#include "cxx-nodecl-deep-copy-fwd.h"

const_value_t* const_value_deep_copy(const_value_t* source,
        symbol_map_t* symbol_map);

#endif // CXX_CEXPR_DEEP_COPY_H
