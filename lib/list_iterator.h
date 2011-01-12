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



#ifndef LIST_ITERATOR_H
#define LIST_ITERATOR_H

#include "libutils-common.h"
#include "s_types.h"
#include "list.h"
#include "iterator.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _list_iterator ListIterator;

struct _list_iterator
{
    Iterator iterator;
    ListNode *act;
    List *list;
};

LIBUTILS_EXTERN void list_iterator_init(ListIterator * i, List * l);
LIBUTILS_EXTERN ListIterator *list_iterator_create(List * l);

LIBUTILS_EXTERN void list_riterator_init(ListIterator * i, List * l);
LIBUTILS_EXTERN ListIterator *list_riterator_create(List * l);

#ifdef __cplusplus
}
#endif

#endif
