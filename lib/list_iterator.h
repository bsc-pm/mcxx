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
