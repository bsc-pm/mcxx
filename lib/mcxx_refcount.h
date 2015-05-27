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




#ifndef MCXX_REFCOUNT_H
#define MCXX_REFCOUNT_H

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

typedef
enum mcxx_ref_colour_tag
{
    _MCXX_BLACK  = 0, // In use or free
    _MCXX_GRAY   = 1, // Possible member of cycle
    _MCXX_WHITE  = 2, // Member of garbage cycle
    _MCXX_PURPLE = 3, // Possible root of cycle
} mcxx_ref_colour_t;

typedef void (*mcxx_walk_fun_t)(void *, void (*)(void*));
typedef void (*mcxx_internal_dealloc_fun_t)(void *);

typedef
struct mcxx_refcount_descriptor_tag
{
    mcxx_internal_dealloc_fun_t dealloc;
    mcxx_walk_fun_t walk;
} mcxx_refcount_descriptor_t;

#define MCXX_REFCOUNT_OBJECT \
    mcxx_refcount_descriptor_t *desc;\
    int count; \
    bool buffered:1; \
    mcxx_ref_colour_t colour:2 \

// #define ALIGN_REFCOUNT_OBJECT __attribute__((aligned(16))) 

typedef
struct mcxx_base_refcount_tag
{
    MCXX_REFCOUNT_OBJECT;
} mcxx_base_refcount_t, *p_mcxx_base_refcount_t;

#define LIBUTILS_EXTERN extern

LIBUTILS_EXTERN void mcxx_increment(void *p);
LIBUTILS_EXTERN void mcxx_decrement(void *p);

LIBUTILS_EXTERN void mcxx_collectcycles(void);

// helper macros (do not use)
#define DESCRIPTOR_NAME(base_type) base_type##_descriptor_var

#define REFCOUNTED_TYPE(base_type) ref_counted_##base_type

#define REF_CALLOC_ADAPTER(x) xcalloc(1, x)

#define REF_NEW_GENERIC(t, alloc_fun) ({ \
        REFCOUNTED_TYPE(t) *p = alloc_fun(sizeof(*p)); \
        p->ref_count_.buffered = 0; \
        p->ref_count_.count = 0; \
        p->ref_count_.desc = &DESCRIPTOR_NAME(t); \
        mcxx_increment(&p->data); \
        &(p->data); \
        })

// Put this in headers
#define DECLARE_REFCOUNTED_TYPE(base_type) \
    extern mcxx_refcount_descriptor_t DESCRIPTOR_NAME(base_type); \
    \
    typedef struct { \
        mcxx_base_refcount_t ref_count_; \
        base_type data; \
    } REFCOUNTED_TYPE(base_type)

// Put this in the implementation file
#define DEFINE_REFCOUNTED_TYPE(base_type, dealloc, walk_fun) \
    mcxx_refcount_descriptor_t DESCRIPTOR_NAME(base_type) = { (mcxx_internal_dealloc_fun_t)dealloc, (mcxx_walk_fun_t)walk_fun }

// Create a new ref counted object
#define REF_NEW(t) REF_NEW_GENERIC(t, xmalloc)
#define REF_NEW0(t) REF_NEW_GENERIC(t, REF_CALLOC_ADAPTER)

// Increase/decrease the reference and return the passed pointer value
#define REF(p) ({ __typeof__((p)) q = (p); mcxx_increment(q); q; })
#define UNREF(p) ({ __typeof__((p)) q = (p); mcxx_decrement(q); q; })

// Convenience macro that simplifies setting refcounted pointers
#define REF_SET(a, b) ({ void *tmp = (a); (a) = REF(b); UNREF(tmp); (a); })

#endif // MCXX_REFCOUNT_H
