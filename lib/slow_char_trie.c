/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "mem.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// See below for public interface of uniqstr
// -- Private implementation of uniqstr
//

typedef struct char_trie_tag char_trie_t;

typedef struct char_trie_element_tag
{
    unsigned char elem;
    // Only valid when elem == 0
    const char *str;
    char_trie_t* next;
} char_trie_element_t;

struct char_trie_tag
{
    int num_elements;
    char_trie_element_t* elements;
};

static struct char_trie_tag const CHAR_TRIE_INITIALIZER =
{
    .num_elements = 0,
    .elements = NULL
};

static char_trie_element_t *lookup_element(const char_trie_t* char_trie, unsigned char entity);

static const char* lookup_list(const char_trie_t* char_trie, const char* str, int length)
{
    if (length == 0)
    {
        if (char_trie->num_elements > 0
                && char_trie->elements[0].elem == 0)
        {
            return char_trie->elements[0].str;
        }
        return NULL;
    }
    else
    {
        char_trie_element_t* elem = lookup_element(char_trie, *str);
        if (elem == NULL)
            return NULL;
        else
            return lookup_list(elem->next, str + 1, length - 1);
    }
}

static const char* create_elements(char_trie_t* char_trie, const char *orig_str, const char* str, int length);


static const char* insert_list_rec(char_trie_t* char_trie, const char *orig_str, const char *str, int length)
{
    if (length == 0)
    {
        if ((char_trie->num_elements == 0)
                || (char_trie->elements[0].elem != 0))
        {
            return create_elements(char_trie, orig_str, str, length);
        }
        return NULL;
    }
    else
    {
        char_trie_element_t* elem = lookup_element(char_trie, *str);
        if (elem == NULL)
        {
            // Create all the remaining elements since they will not be found
            // anymore
            return create_elements(char_trie, orig_str, str, length);
        }
        else
        {
            return insert_list_rec(elem->next, orig_str, str + 1, length - 1);
        }
    }
}

static const char* insert_list(char_trie_t* char_trie, const char *orig_str, int length)
{
    return insert_list_rec(char_trie, orig_str, orig_str, length);
}

static char_trie_element_t *lookup_element_rec(const char_trie_t* char_trie, unsigned char entity, int lower, int upper)
{
    if (lower > upper)
        return NULL;

    int middle = (lower + upper) / 2;

    unsigned char i_middle = char_trie->elements[middle].elem;
    unsigned char i_entity = entity;

    if (i_entity < i_middle)
        return lookup_element_rec(char_trie, entity, lower, middle - 1);
    else if (i_middle < i_entity)
        return lookup_element_rec(char_trie, entity, middle + 1, upper);
    else 
        return &(char_trie->elements[middle]);
}

static char_trie_element_t *lookup_element(const char_trie_t* char_trie, unsigned char entity)
{
    return lookup_element_rec(char_trie, entity, 0, char_trie->num_elements - 1);
}

static const char* create_elements(char_trie_t* char_trie, const char* orig_str, const char* str, int length)
{
    char_trie->num_elements++;
    char_trie->elements = NEW_REALLOC(char_trie_element_t,
            char_trie->elements, 
            char_trie->num_elements);

    // Locate place where the element would go
    int lower = 0;
    int upper = char_trie->num_elements - 2;

    unsigned char i_entity = '\0';
    if (length != 0)
        i_entity = *str;

    while (lower <= upper)
    {
        int middle = (lower + upper) / 2;
        unsigned char i_middle = char_trie->elements[middle].elem;

        if (i_entity < i_middle)
        {
            upper = middle - 1;
        }
        else if (i_middle < i_entity)
        {
            lower = middle + 1;
        }
        // This should never happen
        else 
        {
            abort();
        }
    }

    // lower contains the position where the element should go
    // shift all elements rightwards one position
    int i;
    for (i = char_trie->num_elements - 1; i > lower; i--)
    {
        char_trie->elements[i] = char_trie->elements[i - 1];
    }

    if (length == 0)
    {
        char_trie->elements[lower].elem = 0;
        // No next after the "end of list"
        char_trie->elements[lower].next = NULL;
        // Store the original string (this should be the unique strdup ever)
        const char* p = xstrdup(orig_str);
        char_trie->elements[lower].str = p;

        _bytes_used_char_trie += (strlen(orig_str) + 1);

        return p;
    }
    else
    {
        char_trie->elements[lower].elem = *str;
        char_trie->elements[lower].next = NEW0(char_trie_t);
        _bytes_used_char_trie += sizeof(char_trie_t);

        const char* result =
        create_elements(char_trie->elements[lower].next,
                orig_str,
                str + 1, length - 1);

        return result;
    }
}

#if 0
typedef
struct char_stack_tag
{
    int pos;
    unsigned char elem;
} char_stack_t;

static void print_trie_rec(const char_trie_t* char_trie, int level, char_stack_t* stack)
{
    int i;

    for (i = 0; i < char_trie->num_elements; i++)
    {
        if (char_trie->elements[i].elem == 0)
        {
            int j;
            for (j = 0; j < level; j++)
            {
                fprintf(stderr, "[%d] %c ", stack[j].pos, stack[j].elem);
            }
            fprintf(stderr, "<end>\n");
        }
        else
        {
            stack[level].pos = i;
            stack[level].elem = char_trie->elements[i].elem;

            print_trie_rec(char_trie->elements[i].next, level + 1, stack);
        }
    }
}

static void print_trie(const char_trie_t* char_trie)
{
    char_stack_t stack[256];
    print_trie_rec(char_trie, 0, stack);
}
#endif

// -- Public interface for uniqstr
static char_trie_t _global_char_trie =
{
    .num_elements = 0,
    .elements = NULL
};

const char *uniquestr(const char* c)
{
    if (c == NULL)
        return NULL;

    const char* result = NULL;
    result = lookup_list(&_global_char_trie, c, strlen(c));
    if (result == NULL)
    {
        result = insert_list(&_global_char_trie, c, strlen(c));
    }

    return result;
}
