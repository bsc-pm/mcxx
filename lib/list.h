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
#ifndef LIST_H
#define LIST_H

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

void list_init(List * l);
List *list_create(void);
void list_set(List * l, int op, void *value);
void list_dump(List * l);
void list_destroy(List * l);
void node_destroy(List * l, ListNode * node);
ListNode *list_first(List * l);
ListNode *list_last(List * l);
int list_num_items(List * l);
void list_append(List * l, void *item);
void list_prepend(List * l, void *item);
void list_insert_pre(List * l, ListNode * node_post, void *item);
void node_lock(ListNode * node);
void node_unlock(List * l, ListNode * node);
ListNode *node_create(void *Item);
ListNode *node_next(List * l, ListNode * node, bool_type unlock);
ListNode *node_prev(List * l, ListNode * node, bool_type unlock);
void list_delete(List * l, ListNode * node);
void list_delete_item(List * l, void *item);
void *list_get(List * l, int pos);

#ifdef __cplusplus
}
#endif

#endif
