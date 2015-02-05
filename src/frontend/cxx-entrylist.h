/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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



#ifndef CXX_ENTRYLIST_H
#define CXX_ENTRYLIST_H

#include "cxx-entrylist-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

// Basic creation and destruction

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_new(scope_entry_t*);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_add(scope_entry_list_t* list, 
        scope_entry_t* entry);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_prepend(scope_entry_list_t* list, 
        scope_entry_t* entry);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_remove(scope_entry_list_t* list, 
        scope_entry_t* entry);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_add_once(scope_entry_list_t* list, 
        scope_entry_t* entry);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_add_after(scope_entry_list_t* list, 
        scope_entry_t* position,
        scope_entry_t* entry);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_add_before(scope_entry_list_t* list, 
        scope_entry_t* position,
        scope_entry_t* entry);

LIBMCXX_EXTERN void entry_list_free(scope_entry_list_t* list);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_copy(const scope_entry_list_t* list);

// Queries
LIBMCXX_EXTERN int entry_list_size(const scope_entry_list_t* list);

// Head, for singleton lists
LIBMCXX_EXTERN scope_entry_t* entry_list_head(const scope_entry_list_t* list);

// Iterator
LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_iterator_t* entry_list_iterator_begin(const scope_entry_list_t* list);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_t* entry_list_iterator_current(scope_entry_list_iterator_t* it);
LIBMCXX_EXTERN void entry_list_iterator_next(scope_entry_list_iterator_t* it);

LIBMCXX_EXTERN char entry_list_iterator_end(scope_entry_list_iterator_t* it);

LIBMCXX_EXTERN void entry_list_iterator_free(scope_entry_list_iterator_t* it);

// Other ops
LIBMCXX_EXTERN scope_entry_list_t* entry_list_merge(const scope_entry_list_t* list1, 
        const scope_entry_list_t* list2);

LIBMCXX_EXTERN char entry_list_contains(const scope_entry_list_t* l, 
        scope_entry_t* entry);

LIBMCXX_EXTERN void entry_list_to_symbol_array(scope_entry_list_t*, scope_entry_t*** array, int* num_items);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_from_symbol_array(int num_items, scope_entry_t**);

LIBMCXX_EXTERN WARN_UNUSED scope_entry_list_t* entry_list_concat(const scope_entry_list_t*, const scope_entry_list_t*);

MCXX_END_DECLS

#endif // CXX_ENTRYLIST_H
