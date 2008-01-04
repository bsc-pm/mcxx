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
#include "list_iterator.h"
#include "mem_ctl.h"

/* Forward iterator functions */

static void list_iterator_end(ListIterator * i)
{
    if (i->act)
        node_unlock(i->list, i->act);
}

static void list_iterator_first(ListIterator * i)
{
    if (i->list)
    {
        i->act = list_first(i->list);
        if (i->act)
            node_lock(i->act);
    }
}

static bool_type list_iterator_finished(ListIterator * i)
{
    return i->act == NULL;
}

static void list_iterator_next(ListIterator * i)
{
    i->act = node_next(i->list, i->act, true);
}

static void *list_iterator_item(ListIterator * i)
{
    if (!i->act)
        return NULL;

    return node_data(i->act);
}

static void list_iterator_remove(ListIterator * i)
{
    if (i->act)
        list_delete(i->list, i->act);
}

static ListIterator fast_it;
static int fi = 0;

static void list_iterator_free(ListIterator * i)
{
    if (i == &fast_it)
    {
        fi = 0;
    }
    else
    {
        FREE(i);
    }
}

#if 1                           /* JAIRO */
static int list_iterator_num_items(ListIterator * i)
{
    if (i->list)
        return list_num_items(i->list);
    else
        return -1;
}
#endif

IteratorOps list_iterator_ops = {
    (void (*)(Iterator *)) list_iterator_first,
    (bool_type(*)(Iterator *)) list_iterator_finished,
    (void *(*)(Iterator *)) list_iterator_item,
    (void (*)(Iterator *)) list_iterator_next,
    (void (*)(Iterator *)) list_iterator_remove,
    (void (*)(Iterator *)) list_iterator_end,
    (void (*)(Iterator *)) list_iterator_free,
#if 1                           /* JAIRO */
    (int (*)(Iterator *)) list_iterator_num_items
#endif
};

void list_iterator_init(ListIterator * i, List * l)
{
    iterator_init(ITERATOR(i), &list_iterator_ops);
    i->list = l;
    i->act = NULL;
}

ListIterator *list_iterator_create(List * l)
{
    ListIterator *i;

    if (fi)
        i = NEW(ListIterator);
    else
    {
        fi = 1;
        i = &fast_it;
    }

    list_iterator_init(i, l);

    return i;
}


/* Reverse iterator functions */

static void list_riterator_first(ListIterator * i)
{
    if (i->list)
    {
        i->act = list_last(i->list);
        if (i->act)
            node_lock(i->act);
    }
}

static void list_riterator_next(ListIterator * i)
{
    i->act = node_prev(i->list, i->act, true);
}


IteratorOps list_riterator_ops = {
    (void (*)(Iterator *)) list_riterator_first,
    (bool_type(*)(Iterator *)) list_iterator_finished,
    (void *(*)(Iterator *)) list_iterator_item,
    (void (*)(Iterator *)) list_riterator_next,
    (void (*)(Iterator *)) list_iterator_remove,
    (void (*)(Iterator *)) list_iterator_end,
    (void (*)(Iterator *)) noop_free,
    (int (*)(Iterator *)) NULL
};

void list_riterator_init(ListIterator * i, List * l)
{
    iterator_init(ITERATOR(i), &list_riterator_ops);
    i->list = l;
    i->act = NULL;
}

ListIterator *list_riterator_create(List * l)
{
    ListIterator *i;

    i = NEW(ListIterator);
    list_riterator_init(i, l);

    return i;
}
