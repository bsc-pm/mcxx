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


#ifndef CXX_SYMBOL_DEEP_COPY_H
#define CXX_SYMBOL_DEEP_COPY_H

#include "cxx-macros.h"
#include "cxx-nodecl-deep-copy-fwd.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

void symbol_deep_copy(
        scope_entry_t* dest,
        scope_entry_t* source,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map);

void symbol_deep_copy_compute_maps(
        scope_entry_t* dest,
        scope_entry_t* source,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

MCXX_END_DECLS

#endif
