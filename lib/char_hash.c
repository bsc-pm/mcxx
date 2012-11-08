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



#include "uniquestr.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct string_link
{
    unsigned int hash;
    const char *string;
    struct string_link *next;
};

static struct string_link *hash_table[49999];
static unsigned long long int bytes_used = 0;

unsigned long long int char_trie_used_memory(void)
{
    return bytes_used;
}

static unsigned int hash_string(const char *string)
{
    unsigned int hash = 0;
    const char *p;

    for (p = string; *p; p++)
        hash = ((hash << 5) + hash) ^ *p;

    return hash;
}

const char *uniquestr(const char *string)
{
    if (string == NULL)
        return NULL;

    unsigned int hash = hash_string(string);
    unsigned int hash_index = hash % (sizeof(hash_table) / sizeof(hash_table[0]));
    struct string_link *p, *p_prev = 0, *new_link;

    for (p = hash_table[hash_index]; p; p_prev = p, p = p->next)
    {
        if (p->hash == hash && !strcmp(p->string, string))
        {
            if (p != hash_table[hash_index])
            {
                p_prev->next = p->next;
                p->next = hash_table[hash_index];
                hash_table[hash_index] = p;
            }

            return p->string;
        }
    }

    bytes_used += sizeof(struct string_link) + strlen(string) + 1;

    new_link = malloc(sizeof(struct string_link));
    new_link->string = strdup(string);
    new_link->hash = hash;
    new_link->next = hash_table[hash_index];
    hash_table[hash_index] = new_link;

    return new_link->string;
}
