/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

typedef
enum _mcxx_ref_colour_tag
{
    _MCXX_BLACK  = 0, // In use or free
    _MCXX_GRAY   = 1, // Possible member of cycle
    _MCXX_WHITE  = 2, // Member of garbage cycle
    _MCXX_PURPLE = 3, // Possible root of cycle
} _mcxx_ref_colour_t;

typedef void (*_mcxx_children_fun)(void *, void (*)(void*));


// We can keep up to 536,870,912 (2^29) references per object which should be enough
// for almost every application. This way MCXX_REFCOUNT_OBJECT payload is just 8 bytes
// in 32-bit and 12 in 64-bit

#define MCXX_REFCOUNT_OBJECT \
    _mcxx_children_fun _mcxx_children; \
    int _mcxx_refcount:29; \
    char _mcxx_buffered:1; \
    _mcxx_ref_colour_t _mcxx_colour:2 \

typedef
struct _mcxx_base_refcount_tag
{
    MCXX_REFCOUNT_OBJECT;
} *_p_mcxx_base_refcount_t, _mcxx_base_refcount_t;

#define LIBUTILS_EXTERN

LIBUTILS_EXTERN void *_mcxx_calloc(size_t nmemb, size_t size);

LIBUTILS_EXTERN void _mcxx_increment(void *p);
LIBUTILS_EXTERN void _mcxx_decrement(void *p);

LIBUTILS_EXTERN void _mcxx_collectcycles(void);
LIBUTILS_EXTERN _mcxx_children_fun *_mcxx_children(void *p);

#define MCXX_CHILDREN(x) (*_mcxx_children(x))

#define MCXX_REF(x) ({ _mcxx_increment(x); x; })
#define MCXX_UNREF(x) _mcxx_decrement(x)

#define MCXX_NEW(_type_name) \
    ({ void* p = _mcxx_calloc(1, sizeof(_type_name)); \
      ((_p_mcxx_base_refcount_t)p)->_mcxx_children = _type_name##_children; \
     _mcxx_increment(p); \
     (_type_name*)p; })

// Sets 'x' to point 'y'
#define MCXX_UPDATE_TO(x, y) \
    do { \
        if ((x) != NULL) \
        { \
            MCXX_UNREF(x); \
        } \
        (x) = MCXX_REF(y); \
    } while (0)

// Fixed arrays

typedef
struct mcxx_refcount_array_tag
{
    MCXX_REFCOUNT_OBJECT;
    unsigned int num_items;
    void *data[];
} mcxx_refcount_array_t;

void mcxx_refcount_array_t_children(void*, void(*)(void*));

#define MCXX_NEW_VEC(_type_name, _num_items) \
    ({ mcxx_refcount_array_t* p = calloc(1, sizeof(mcxx_refcount_array_t) + (_num_items * sizeof(void*))); \
      ((_p_mcxx_base_refcount_t)p)->_mcxx_children = mcxx_refcount_array_t_children; \
     _mcxx_increment(p); \
     fprintf(stderr, "NEW -> %p\n", p); \
       p->num_items = _num_items; \
       int _; \
       for (_ = 0; _ < p->num_items; _++) \
       { p->data[_] = MCXX_NEW(_type_name); } \
       (_type_name**)(p->data); })

#define MCXX_UNREF_VEC(x) \
      ({ void * p = ((char*)x) - offsetof(mcxx_refcount_array_t, data); \
     fprintf(stderr, "DELETE -> %p\n", p); \
       MCXX_UNREF(p); })


#endif // MCXX_REFCOUNT_H
