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

#include "hash_iterator.h"
#include "mem_ctl.h"

#ifdef __GNUC__
  #define UNUSED_PARAM __attribute__((unused))
#endif

static void
hash_iterator_end (HashIterator * i UNUSED_PARAM)
{
}

static void
hash_iterator_first (HashIterator * i)
{
    if (i->hash)
    {
        i->table = 0;

        for (i->act = NULL; !i->act && i->table < i->hash->size; i->table++)
            i->act = i->hash->table[i->table];
    }
}

static bool_type
hash_iterator_finished (HashIterator * i)
{
  return i->act == NULL;
}

static void
hash_iterator_next (HashIterator * i)
{
    for (i->act = i->act->next; !i->act && i->table < i->hash->size; i->table++)
        i->act = i->hash->table[i->table];
}

static void *
hash_iterator_item (HashIterator * i)
{
  if (!i->act)
    return NULL;

  return i->act->item;
}

static void
hash_iterator_remove (HashIterator * i UNUSED_PARAM)
{
/* Not implemented until be have iterator locking support for hashing */
}

IteratorOps hash_iterator_ops = {
  (void (*)(Iterator *)) hash_iterator_first,
  (bool_type (*)(Iterator *)) hash_iterator_finished,
  (void *(*)(Iterator *)) hash_iterator_item,
  (void (*)(Iterator *)) hash_iterator_next,
  (void (*)(Iterator *)) hash_iterator_remove,
  (void (*)(Iterator *)) hash_iterator_end,
  (void (*)(Iterator *)) noop_free,
  (int (*)(Iterator *)) NULL
};

void
hash_iterator_init (HashIterator * i, Hash * h)
{
  iterator_init (ITERATOR (i), &hash_iterator_ops);
  i->hash = h;
  i->act = NULL;
}

HashIterator *
hash_iterator_create (Hash * h)
{
  HashIterator *i;

  i = NEW (HashIterator);
  hash_iterator_init (i, h);

  return i;
}
