/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef ITERATOR_H
#define ITERATOR_H

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
#if 1                           /* JAIRO */
    int (*items) (Iterator *);
#endif
};

struct _iterator
{
    IteratorOps *ops;
};

void iterator_init(Iterator * i, IteratorOps * ops);
void iterator_end(Iterator * i);
void iterator_destroy(Iterator * i);
void iterator_first(Iterator * i);
bool_type iterator_finished(Iterator * i);
void iterator_next(Iterator * i);
void *iterator_item(Iterator * i);
void iterator_remove(Iterator * i);

#if 1                           /* JAIRO */
int iterator_items(Iterator * i);
#endif

#ifdef __cplusplus
}
#endif

#endif
