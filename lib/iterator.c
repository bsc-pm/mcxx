/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "iterator.h"
#include "mem_ctl.h"

void iterator_init(Iterator * i, IteratorOps * ops)
{
    i->ops = ops;
}

void iterator_end(Iterator * i)
{
    if (!i)
        return;

    if (i->ops->end)
        i->ops->end(i);
}

void iterator_destroy(Iterator * i)
{
    if (!i)
        return;

    iterator_end(i);
    if (i->ops->free)
    {
        i->ops->free(i);
    }
    else
    {
        FREE(i);
    }
}

void iterator_first(Iterator * i)
{
    if (!i || !i->ops->first)
        return;

    i->ops->first(i);
}

bool_type iterator_finished(Iterator * i)
{
    if (!i || !i->ops->finished)
        return false;

    return i->ops->finished(i);
}

void iterator_next(Iterator * i)
{
    if (!i || !i->ops->next)
        return;

    i->ops->next(i);
}

void *iterator_item(Iterator * i)
{
    if (!i || !i->ops->item)
        return NULL;

    return i->ops->item(i);
}

void iterator_remove(Iterator * i)
{
    if (!i || !i->ops->remove)
        return;

    i->ops->remove(i);
}

#if 1                           /* JAIRO */
int iterator_items(Iterator * i)
{
    if (!i || !i->ops->items)
        return -1;

    return (i->ops->items(i));
}
#endif
