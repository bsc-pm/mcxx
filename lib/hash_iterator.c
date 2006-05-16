/***************************************************************************
  PhantasienMud Development Team 2001 (c)

  hash_iterator.c - hash iteration functions

  ChangeLog:

  Sat Jan 20 2001 - Created by Ingwe

***************************************************************************/

#include "hash_iterator.h"
#include "mem_ctl.h"

static void
hash_iterator_end (HashIterator * i)
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
hash_iterator_remove (HashIterator * i)
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
  (void (*)(Iterator *)) noop_free
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
