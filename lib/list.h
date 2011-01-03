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



#ifndef LIST_H
#define LIST_H

#include "libutils-common.h"
#include "mem_ctl.h"
#include "s_types.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _list_node ListNode;

enum
{ LIST_FREE_ITEMS, LIST_ITEMS_DESTRUCTOR }
ListOps;

struct _list_node
{
    void *data;
    int no_references;
    bool_type is_deleted;
    ListNode *next;
    ListNode *prev;
};

typedef struct _list List;

struct _list
{
    ListNode *first;
    ListNode *last;
    int no_items;

    bool_type free_items;
    delete_func *df_items;
};

#define node_data(node) (node->data)
#define LIST(l) ((List *)(l))

LIBUTILS_EXTERN void list_init(List * l);
LIBUTILS_EXTERN List *list_create(void);
LIBUTILS_EXTERN void list_set(List * l, int op, void *value);
LIBUTILS_EXTERN void list_dump(List * l);
LIBUTILS_EXTERN void list_destroy(List * l);
LIBUTILS_EXTERN void node_destroy(List * l, ListNode * node);
LIBUTILS_EXTERN ListNode *list_first(List * l);
LIBUTILS_EXTERN ListNode *list_last(List * l);
LIBUTILS_EXTERN int list_num_items(List * l);
LIBUTILS_EXTERN void list_append(List * l, void *item);
LIBUTILS_EXTERN void list_prepend(List * l, void *item);
LIBUTILS_EXTERN void list_insert_pre(List * l, ListNode * node_post, void *item);
LIBUTILS_EXTERN void node_lock(ListNode * node);
LIBUTILS_EXTERN void node_unlock(List * l, ListNode * node);
LIBUTILS_EXTERN ListNode *node_create(void *Item);
LIBUTILS_EXTERN ListNode *node_next(List * l, ListNode * node, bool_type unlock);
LIBUTILS_EXTERN ListNode *node_prev(List * l, ListNode * node, bool_type unlock);
LIBUTILS_EXTERN void list_delete(List * l, ListNode * node);
LIBUTILS_EXTERN void list_delete_item(List * l, void *item);
LIBUTILS_EXTERN void *list_get(List * l, int pos);

#ifdef __cplusplus
}
#endif

#endif
