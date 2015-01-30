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

struct locus_link
{
    unsigned int hash;
    locus_t* locus;
    struct locus_link* next;
};

static struct locus_link *hash_table[49999];

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

const locus_t* make_locus(const char* filename, unsigned int line, unsigned int col)
{
    if (filename == NULL)
        filename = "";

    unsigned int hash = hash_locus(filename, line, col);
    unsigned int hash_index = hash % (sizeof(hash_table) / sizeof(hash_table[0]));


    struct locus_link *p, *p_prev = 0, *new_link;

    for (p = hash_table[hash_index]; p; p_prev = p, p = p->next)
    {
        if (p->hash == hash
                && strcmp(p->locus->filename, filename) == 0
                && p->locus->line == line
                && p->locus->col == col)
        {
            // Move to the head of the list to favour temporal locality
            if (p != hash_table[hash_index])
            {
                p_prev->next = p->next;
                p->next = hash_table[hash_index];
                hash_table[hash_index] = p;
            }

            return p->locus;
        }
    }

    new_link = xmalloc(sizeof(*new_link));
    new_link->locus = xmalloc(sizeof(*new_link->locus));
    new_link->locus->filename = uniquestr(filename);
    new_link->locus->line = line;
    new_link->locus->col = col;
    new_link->hash = hash; 
    new_link->next = hash_table[hash_index];
    hash_table[hash_index] = new_link; 

    return new_link->locus;
}

