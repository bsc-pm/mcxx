/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef HASH_H
#define HASH_H

#include "libutils-common.h"
#include "mem_ctl.h"
#include "s_types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct _hashnode HashNode;
typedef struct _hash Hash;
typedef int HASH_FUNC (const void *key, int size);
typedef int KEYCMP_FUNC (const void *key1, const void *key2);

enum HashOps_tag
{ HASH_FREE_KEYS, HASH_FREE_ITEMS, HASH_KEYS_DESTRUCTOR,
  HASH_ITEMS_DESTRUCTOR
};

#define HASHFUNC(f) ((HASH_FUNC *)f)
#define KEYCMPFUNC(f) ((KEYCMP_FUNC *)f)

struct _hashnode
{
  const void *key;
  void *item;

  HashNode *next;
  HashNode *prev;
};

struct _hash
{
  int size;
  int no_items;
  HASH_FUNC *hash_func;
  KEYCMP_FUNC *keycmp_func;
  HashNode **table;

  bool_type free_keys;
  delete_func *df_keys;
  bool_type free_items;
  delete_func *df_items;
};

LIBUTILS_EXTERN void hash_init (Hash * h, int size, HASH_FUNC * hash_func,
        KEYCMP_FUNC * keycmp_func);
LIBUTILS_EXTERN void hash_set (Hash * h, int op, void *value);
#define hash_set_bool(h,op,val) hash_set(h,op,(void *)val)
LIBUTILS_EXTERN Hash *hash_create (int size, HASH_FUNC * hash_func,
           KEYCMP_FUNC * keycmp_func);
LIBUTILS_EXTERN void hash_dump (Hash * h);
LIBUTILS_EXTERN void hash_destroy (Hash * h);
LIBUTILS_EXTERN void hash_put (Hash * h, const void *key, void *item);
LIBUTILS_EXTERN HashNode *hash_getnode (Hash * h, int i, const void *key);
LIBUTILS_EXTERN void *hash_get (Hash * h, const void *key);
LIBUTILS_EXTERN void *hash_delete (Hash * h, const void *key);

/* useful hash functions */

LIBUTILS_EXTERN int hash_string (const char *name, int size);
LIBUTILS_EXTERN int hash_caseless_string (const char *name, int size);
LIBUTILS_EXTERN int prime_hash(const char* key, int hash_size);
LIBUTILS_EXTERN int pointer_hash(const void* key, int size);

// Useful things for string hashes
#define HASH_SIZE (23)

LIBUTILS_EXTERN int integer_comp(void *, void*);

LIBUTILS_EXTERN unsigned long long hash_used_memory(void);

#ifdef __cplusplus
}
#endif

#endif
