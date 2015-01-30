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


#ifndef DHASH_PTR_H
#define DHASH_PTR_H

// Hash for strings

#ifdef __cplusplus
extern "C" {
#endif

typedef void* dhash_ptr_info_t;

typedef struct dhash_ptr_tag dhash_ptr_t;

dhash_ptr_t* dhash_ptr_new(int initial_size);
void dhash_ptr_destroy(dhash_ptr_t*);
void* dhash_ptr_query(dhash_ptr_t*, const char* key);
void dhash_ptr_insert(dhash_ptr_t*, const char* key, dhash_ptr_info_t info);
void dhash_ptr_remove(dhash_ptr_t*, const char* key);

typedef void dhash_ptr_walk_fn(const char* key, void* info, void *walk_info);

void dhash_ptr_walk(dhash_ptr_t*, dhash_ptr_walk_fn walk_fn, void* walk_info);

#ifdef __cplusplus
}
#endif

#endif // DHASH_PTR_H
