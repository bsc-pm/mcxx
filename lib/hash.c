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
#include "hash.h"

void
hash_init (Hash * h, int size, HASH_FUNC * hash_func,
       KEYCMP_FUNC * keycmp_func)
{
  int i;
  h->size = size;
  h->hash_func = hash_func;
  h->keycmp_func = keycmp_func;
  h->table = NEW_ARRAY (HashNode *, size);
  for (i = 0; i < size; i++)
    h->table[i] = NULL;
  h->no_items = 0;
  h->free_keys = true;
  h->free_items = false;
  h->df_keys = NULL;
  h->df_items = NULL;
}

void
hash_set (Hash * h, int op, void *value)
{
  switch (op)
    {
    case HASH_FREE_KEYS:
      h->free_keys = (bool_type) value;
      break;
    case HASH_FREE_ITEMS:
      h->free_items = (bool_type) value;
      break;
    case HASH_KEYS_DESTRUCTOR:
      h->df_keys = value;
      break;
    case HASH_ITEMS_DESTRUCTOR:
      h->df_items = value;
      break;
    }
}

Hash *
hash_create (int size, HASH_FUNC * hash_func, KEYCMP_FUNC * keycmp_func)
{
  Hash *new_hash;

  new_hash = NEW (Hash);
  hash_init (new_hash, size, hash_func, keycmp_func);

  return new_hash;
}

void
hash_dump (Hash * h)
{
  int i;
  HashNode *node, *node_next;

  for (i = 0; i < h->size; i++)
    for (node = h->table[i]; node; node = node_next)
      {
    node_next = node->next;
    if (h->free_items)
      {
        if (h->df_items)
          (*h->df_items) (node->item);
        FREE (node->item);
      }
    if (h->free_keys)
      {
        if (h->df_keys)
          (*h->df_keys) (node->key);
        FREE (node->key);
      }
    FREE (node);
      }

  FREE (h->table);
  h->size = 0;
  h->no_items = 0;
}

void
hash_destroy (Hash * h)
{
  if (!h)
    return;

  hash_dump (h);
  FREE (h);
}

void
hash_put (Hash * h, const void *key, void *item)
{
  int i;
  HashNode *node;

  if (!h)
    return;

  i = (*h->hash_func) (key, h->size);

  node = NEW (HashNode);
  node->key = key;
  node->item = item;
  node->prev = NULL;

  if (h->table[i])
    h->table[i]->prev = node;
  node->next = h->table[i];
  h->table[i] = node;
  h->no_items++;
}

HashNode *
hash_getnode (Hash * h, int i, const void *key)
{
  HashNode *node;

  if (i < 0 || i >= h->size)
    return NULL;

  for (node = h->table[i]; node; node = node->next)
    if (!(*h->keycmp_func) (key, node->key))
      return node;
  return NULL;
}

void *
hash_get (Hash * h, const void *key)
{
  HashNode *node;

  if (!h)
    return NULL;
  node = hash_getnode (h, (*h->hash_func) (key, h->size), key);
  if (!node)
    return NULL;

  return node->item;
}

void *
hash_delete (Hash * h, const void *key)
{
  HashNode *node;
  void *item;
  int i;

  if (!h)
    return NULL;
  i = (*h->hash_func) (key, h->size);
  node = hash_getnode (h, i, key);
  if (!node)
    return NULL;

  if (node->next)
    node->next->prev = node->prev;
  if (node->prev)
    node->prev->next = node->next;

  if (h->table[i] == node)
    h->table[i] = node->next;

  if (h->free_items)
    {
      if (h->df_items)
    (*h->df_items) (node->item);
      FREE (node->item);
      item = NULL;
    }
  else
    item = node->item;

  if (h->free_keys)
    {
      if (h->df_keys)
    (*h->df_keys) (node->key);
      FREE (node->key);
    }
  FREE (node);

  h->no_items--;

  return item;
}

/* common hash functions */

#include <ctype.h>
#include <string.h>
#include <stdint.h>

int
hash_string (const char *name, int size)
{
  if (!name || name[0] == '\0')
    return 0;
  if (strlen (name) < 3)
    return name[0] % size;

  return (name[0] + name[1] + name[3]) % size;
}

int pointer_hash(const void* key, int size)
{
    intptr_t v = (intptr_t)(key);

    return (v % size);
}

int
hash_caseless_string (const char *name, int size)
{
  if (!name || name[0] == '\0')
    return 0;

  return (tolower (name[0]) + tolower (name[1]) + tolower (name[3])) % size;
}

int prime_hash(const char* key, int hash_size)
{
    int length = strlen(key);
    int result = 0;
    int i;

    for (i = 0; i < length; i++)
    {
        result += key[i];
    }

    return (result % hash_size);
}

int integer_comp (void *key1, void *key2)
{
    intptr_t a = (intptr_t)(key1);
    intptr_t b = (intptr_t)(key2);

    if (a == b)
    {
        return 0;
    }
    else if (a < b)
    {
        return -1;
    }
    else return 1;
}
