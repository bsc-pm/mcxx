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
#ifndef CXX_ENTRYLIST_H
#define CXX_ENTRYLIST_H

#include "cxx-entrylist-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

// Basic creation and destruction

scope_entry_list_t* entry_list_new(scope_entry_t*);

scope_entry_list_t* entry_list_add(scope_entry_list_t* list, 
        scope_entry_t* entry);

void entry_list_free(scope_entry_list_t* list);

// Queries
unsigned int entry_list_size(scope_entry_list_t* list);

// Head, for singleton lists
scope_entry_t* entry_list_head(scope_entry_list_t* list);

// Pin, unpin

void entry_list_pin(scope_entry_list_t* list);
void entry_list_unpin(scope_entry_list_t* list);

// Iterator

scope_entry_list_iterator_t* entry_list_iterator_begin(scope_entry_list_t* list);

scope_entry_t* entry_list_iterator_current(scope_entry_list_iterator_t* it);
void entry_list_iterator_next(scope_entry_list_iterator_t* it);

char entry_list_iterator_end(scope_entry_list_iterator_t* it);

void entry_list_iterator_free(scope_entry_list_iterator_t* it);

// Other ops

scope_entry_list_t* entry_list_merge(scope_entry_list_t* list1, 
        scope_entry_list_t* list2);

MCXX_END_DECLS

#endif // CXX_ENTRYLIST_H
