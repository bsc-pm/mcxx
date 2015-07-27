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


#ifndef MEM_H
#define MEM_H

#include <string.h>
#include <stdlib.h>

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined(__GNUC__) && __GNUC__ >= 4
  #if defined(__GNUC_MINOR__) && __GNUC_MINOR__ >= 4
    #define MEM_WARN_UNUSED __attribute__((warn_unused_result))
    #define MEM_MALLOC_RETURN __attribute__((malloc))
    #else
    #define MEM_WARN_UNUSED
    #define MEM_MALLOC_RETURN
  #endif
#endif


#ifdef __cplusplus
extern "C" {
#endif

void *xmalloc(size_t size) MEM_WARN_UNUSED MEM_MALLOC_RETURN;
void xfree(void *ptr);
void *xcalloc(size_t nmemb, size_t size) MEM_WARN_UNUSED MEM_MALLOC_RETURN;
void *xrealloc(void *ptr, size_t size) MEM_WARN_UNUSED; // realloc does not have malloc property
char *xstrdup(const char *s) MEM_WARN_UNUSED MEM_MALLOC_RETURN;

// Guaranteed to call free
void c_free(void *ptr);

#ifdef __cplusplus
}
#endif

// In C++11 there are inline functions that use malloc
#if !defined(HAVE_CXX11)
// Some systems redefine these as macros
#undef malloc
#undef calloc
#undef free
#undef realloc
#undef strdup

#define malloc (+use_xmalloc_instead)
#define calloc (+use_xcalloc_instead)
#define free   (+use_xfree_instead)
#define realloc (+use_xrealloc_instead)
#define strdup (+use_xstrdup_instead)
#endif

#undef NEW
#undef NEW0
#undef NEW_VEC
#undef NEW_VEC0

#undef DELETE

#undef NEW_REALLOC

#define NEW(t) ((t*)xmalloc(sizeof(t)))
#define NEW0(t) ((t*)xcalloc(1, sizeof(t)))

#define NEW_VEC(t, n) ((t*)xmalloc(sizeof(t) * (n)))
#define NEW_VEC0(t, n) ((t*)xcalloc((n), sizeof(t)))

#define NEW_REALLOC(t, x, n) ((t*)xrealloc((x), (n) * sizeof(t)))

#define DELETE(x) xfree((x))


#endif // MEM_H
