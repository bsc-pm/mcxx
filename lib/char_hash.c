/*--------------------------------------------------------------------
  (C) Copyright 2012-2012 Jan Hoogerbrugge

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
#include <limits.h>
#include <string.h>
#include <math.h>

#include "mem.h"

typedef struct string_link_tag string_link_t;

struct string_link_tag
{
    unsigned int hash;
    const char *string;
    string_link_t *next;
};

enum { HASH_LENGTH = 76003 };

static string_link_t *hash_table[HASH_LENGTH];
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
    string_link_t *p, *p_prev = 0, *new_link;

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

    bytes_used += sizeof(string_link_t) + strlen(string) + 1;

    new_link = NEW(string_link_t);
    new_link->string = xstrdup(string); 
    new_link->hash = hash; 
    new_link->next = hash_table[hash_index];
    hash_table[hash_index] = new_link; 

    return new_link->string;
}

void uniquestr_stats(void)
{
    unsigned long long bucket_length[HASH_LENGTH];
    unsigned long long number_of_strings = 0;
    unsigned long long number_of_bytes = 0;

    int i, j;
    for (i = 0; i < HASH_LENGTH; i++)
    {
        string_link_t *p = hash_table[i];

        j = 0;
        while (p != NULL)
        {
            // fprintf(stderr, "%d.%d |%s|\n", i, j, p->string);
            number_of_strings++;
            number_of_bytes += strlen(p->string) + 1; // +1 for NULL

            p = p->next;
            j++;
        }

        bucket_length[i] = j;
    }

    unsigned long long max_bucket = 0;
    unsigned long long min_bucket = ULLONG_MAX;
    unsigned long long sum_buckets = 0;
    unsigned long long num_empty_buckets = 0;

    for (i = 0; i < HASH_LENGTH; i++)
    {
        if (bucket_length[i] >= max_bucket)
            max_bucket = bucket_length[i];
        if (bucket_length[i] <= min_bucket)
            min_bucket = bucket_length[i];

        sum_buckets += bucket_length[i];

        num_empty_buckets += (bucket_length[i] == 0);
    }

    float avg_bucket = 0.0f;
    avg_bucket = ((float)sum_buckets / (float)HASH_LENGTH);

    float stddev_bucket = 0.0f;
    for (i = 0; i < HASH_LENGTH; i++)
    {
        stddev_bucket += fabsf(bucket_length[i] - avg_bucket);
    }
    stddev_bucket = sqrtf(stddev_bucket / (float)(HASH_LENGTH - 1));

    fprintf(stderr, "String table statistics\n");
    fprintf(stderr, "=======================\n\n");

    fprintf(stderr, "Size of hash: %d\n", HASH_LENGTH);
    fprintf(stderr, "Number of strings: %llu\n", number_of_strings);
    fprintf(stderr, "Number of bytes taken by the strings: %llu\n", number_of_bytes);
    fprintf(stderr, "Number of empty buckets: %llu\n", num_empty_buckets);
    fprintf(stderr, "Minimum bucket length: %llu\n", min_bucket);
    fprintf(stderr, "Maximum bucket length: %llu\n", max_bucket);
    fprintf(stderr, "Average bucket length: %.2f\n", avg_bucket);
    fprintf(stderr, "StdDev  bucket length: %.2f\n", stddev_bucket);
}
