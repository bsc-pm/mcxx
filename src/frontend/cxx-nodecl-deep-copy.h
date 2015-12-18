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


#ifndef CXX_NODECL_DEEP_COPY_H
#define CXX_NODECL_DEEP_COPY_H

#include "cxx-nodecl.h"
#include "cxx-scope.h"

MCXX_BEGIN_DECLS

struct symbol_map_tag
{
    scope_entry_t* (*map)(symbol_map_t*, scope_entry_t*);
    void (*dtor)(symbol_map_t*);
};

nodecl_t nodecl_deep_copy(nodecl_t, const decl_context_t*, symbol_map_t*);

nodecl_t nodecl_deep_copy_compute_maps(nodecl_t n,
        const decl_context_t* new_decl_context,
        symbol_map_t* symbol_map,
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        symbol_deep_copy_map_t* symbol_deep_copy_map);

nodecl_deep_copy_map_t* nodecl_deep_copy_map_new(void);
void nodecl_deep_copy_map_free(nodecl_deep_copy_map_t* symbol_deep_copy_map);

symbol_deep_copy_map_t* symbol_deep_copy_map_new(void);
void symbol_deep_copy_map_free(symbol_deep_copy_map_t* symbol_deep_copy_map);

void nodecl_deep_copy_map_traverse(
        nodecl_deep_copy_map_t* nodecl_deep_copy_map,
        void* info,
        void (*fn)(nodecl_t orig, nodecl_t copied, void* info));

void symbol_deep_copy_map_traverse(
        symbol_deep_copy_map_t* symbol_deep_copy_map,
        void* info,
        void (*fn)(scope_entry_t* orig, scope_entry_t* copied, void* info));

typedef struct nested_symbol_map_tag nested_symbol_map_t;
nested_symbol_map_t* new_nested_symbol_map(symbol_map_t* enclosing_map);
void nested_map_add(nested_symbol_map_t* nested_symbol_map, scope_entry_t* source, scope_entry_t* target);

void symbol_deep_copy_map_add(symbol_deep_copy_map_t* symbol_deep_copy_map,
        scope_entry_t *orig,
        scope_entry_t *copied);

MCXX_END_DECLS

#endif // CXX_NODECL_DEEP_COPY_H
