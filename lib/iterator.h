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



#ifndef ITERATOR_H
#define ITERATOR_H

#include "libutils-common.h"
#include "s_types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _iterator Iterator;
typedef struct _iterator_ops IteratorOps;

#define ITERATOR(it) ((Iterator *)it)

/**
 *  @internal
 */

struct _iterator_ops
{
    void (*first) (Iterator *);
      bool_type(*finished) (Iterator *);
    void *(*item) (Iterator *);
    void (*next) (Iterator *);
    void (*remove) (Iterator *);
    void (*end) (Iterator *);
    void (*free) (Iterator *);
    int (*items) (Iterator *);
};

struct _iterator
{
    IteratorOps *ops;
};

LIBUTILS_EXTERN void iterator_init(Iterator * i, IteratorOps * ops);
LIBUTILS_EXTERN void iterator_end(Iterator * i);
LIBUTILS_EXTERN void iterator_destroy(Iterator * i);
LIBUTILS_EXTERN void iterator_first(Iterator * i);
LIBUTILS_EXTERN bool_type iterator_finished(Iterator * i);
LIBUTILS_EXTERN void iterator_next(Iterator * i);
LIBUTILS_EXTERN void *iterator_item(Iterator * i);
LIBUTILS_EXTERN void iterator_remove(Iterator * i);
LIBUTILS_EXTERN int iterator_items(Iterator * i);

#ifdef __cplusplus
}
#endif

#endif
