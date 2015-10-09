/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#include "cxx-locus.h"
#include "mem.h"
#include <string.h>
#include "uniquestr.h"
#include "string_utils.h"

// Heavily inspired in lib/char_hash.c contributed by Jan Hoogerbrugge

typedef
struct locus_item_tag
{
    unsigned int hash;
    locus_t *locus;
} locus_item_t;

typedef
struct locus_bucket_tag
{
    int num;
    int capacity;
    locus_item_t* items;
} locus_bucket_t;


static locus_bucket_t *hash_table[49999];

static unsigned int hash_locus(const char *filename, unsigned int line, unsigned int col)
{
    unsigned int hash = 0;
    const char *p;

    for (p = filename; *p; p++)
        hash = ((hash << 5) + hash) ^ *p;

    hash = ((hash << 5) + hash) ^ line;
    hash = ((hash << 5) + hash) ^ col;

    return hash;
}

enum { POOL_SIZE = 1024 };

typedef
struct pool_locus_tag
{
    int num;
    locus_t* pool;
    struct pool_locus_tag *prev;
} pool_locus_t;

static pool_locus_t *pool_locus;

static locus_t* pool_locus_alloc(void)
{
    if (pool_locus == NULL
            || pool_locus->num == POOL_SIZE)
    {
        pool_locus_t* new_pool = NEW(pool_locus_t);
        new_pool->num = 0;
        new_pool->pool = NEW_VEC(locus_t, POOL_SIZE);
        new_pool->prev = pool_locus;

        pool_locus = new_pool;
    }

    locus_t* result = &pool_locus->pool[pool_locus->num];
    pool_locus->num++;

    return result;
}

const locus_t* make_locus(const char* filename, unsigned int line, unsigned int col)
{
    if (filename == NULL)
        filename = "";

    unsigned int hash = hash_locus(filename, line, col);
    unsigned int hash_index = hash % (sizeof(hash_table) / sizeof(hash_table[0]));

    if (hash_table[hash_index] == NULL)
    {
        hash_table[hash_index] = NEW0(locus_bucket_t);
    }

    locus_bucket_t* bucket = hash_table[hash_index];

    int i, n = bucket->num;
    locus_item_t *items = bucket->items;

    for (i = 0; i < n; i++)
    {
        if (items[i].hash == hash
                && strcmp(items[i].locus->filename, filename) == 0
                && items[i].locus->line == line
                && items[i].locus->col == col)
        {
            return items[i].locus;
        }
    }

    if (bucket->num == bucket->capacity)
    {
        if (bucket->capacity == 0)
            bucket->capacity = 16;
        else
            bucket->capacity = 2*(bucket->capacity + 1);

        bucket->items =
            xrealloc(bucket->items, bucket->capacity * sizeof(*(bucket->items)));

        items = bucket->items;
    }

    items[n].hash = hash;
    items[n].locus = pool_locus_alloc();
    items[n].locus->filename = uniquestr(filename);
    items[n].locus->line = line;
    items[n].locus->col = col;

    bucket->num++;

    return items[n].locus;
}

