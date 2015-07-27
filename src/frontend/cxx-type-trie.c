/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#include "cxx-type-trie.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cxx-typeutils.h"

typedef struct type_trie_element_tag
{
    const type_t* elem;
    // Only valid when next == NULL
    const type_t *function_type;
    type_trie_t* next;
} type_trie_element_t;

struct type_trie_tag
{
    int capacity;
    int num_elements;
    type_trie_element_t* elements;
};

static type_trie_element_t *lookup_element(const type_trie_t* type_trie, const type_t** type_seq);

static const type_t* lookup_list(const type_trie_t* type_trie, const type_t** type_seq, int length)
{
    if (length == 0)
    {
        if (type_trie->num_elements > 0
                && type_trie->elements[0].elem == NULL)
        {
            return type_trie->elements[0].function_type;
        }
        return NULL;
    }
    else
    {
        type_trie_element_t* elem = lookup_element(type_trie, type_seq);
        if (elem == NULL)
            return NULL;
        else
            return lookup_list(elem->next, type_seq + 1, length - 1);
    }
}

static const type_t* create_elements(type_trie_t* type_trie, const type_t **type_seq, const type_t* function_type, int length);

static const type_t* insert_list_rec(type_trie_t* type_trie, const type_t **type_seq, const type_t* function_type, int length)
{
    if (length == 0)
    {
        if ((type_trie->num_elements == 0)
                || (type_trie->elements[0].elem != 0))
        {
            return create_elements(type_trie, type_seq, function_type, length);
        }
        return NULL;
    }
    else
    {
        type_trie_element_t* elem = lookup_element(type_trie, type_seq);
        if (elem == NULL)
        {
            // Create all the remaining elements since they will not be found
            // anymore
            return create_elements(type_trie, type_seq, function_type, length);
        }
        else
        {
            return insert_list_rec(elem->next, type_seq + 1, function_type, length - 1);
        }
    }
}

static const type_t* insert_list(type_trie_t* type_trie, const type_t** type_seq, const type_t *function_type, int length)
{
    return insert_list_rec(type_trie, type_seq, function_type, length);
}

static int elements_compare_fun(const type_t* i1, const type_t* i2)
{
    if (i1 == i2)
        return 0;
    else if (i1 < i2)
        return -1;
    else 
        return 1;
}

static type_trie_element_t *lookup_element_rec(const type_trie_t* type_trie, const type_t *entity, int lower, int upper)
{
    if (lower > upper)
        return NULL;

    int middle = (lower + upper) / 2;

    const type_t* i_middle = type_trie->elements[middle].elem;
    const type_t* i_entity = entity;

    int cmp = elements_compare_fun(i_entity, i_middle);

    if (cmp < 0)
        return lookup_element_rec(type_trie, entity, lower, middle - 1);
    else if (0 < cmp)
        return lookup_element_rec(type_trie, entity, middle + 1, upper);
    else 
        return &(type_trie->elements[middle]);
}

static type_trie_element_t *lookup_element(const type_trie_t* type_trie, const type_t** type_seq)
{
    return lookup_element_rec(type_trie, *type_seq, 0, type_trie->num_elements - 1);
}

static const type_t* create_elements(type_trie_t* type_trie, const type_t** type_seq, const type_t* function_type, int length)
{
    type_trie->num_elements++;
    if (type_trie->capacity == type_trie->num_elements)
    {
        type_trie->capacity *= 2;
        type_trie->elements = NEW_REALLOC(type_trie_element_t, type_trie->elements, type_trie->capacity);
    }

    // Locate place where the element would go
    int lower = 0;
    int upper = type_trie->num_elements - 2;

    const type_t* i_entity = NULL;

    if (length != 0)
        i_entity = *type_seq;

    while (lower <= upper)
    {
        int middle = (lower + upper) / 2;
        const type_t* i_middle = type_trie->elements[middle].elem;

        int cmp = elements_compare_fun(i_entity, i_middle);
        if (cmp < 0)
        {
            upper = middle - 1;
        }
        else if (0 < cmp)
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
    for (i = type_trie->num_elements - 1; i > lower; i--)
    {
        type_trie->elements[i] = type_trie->elements[i - 1];
    }

    if (length == 0)
    {
        type_trie->elements[lower].elem = 0;
        // No next after the "end of list"
        type_trie->elements[lower].next = NULL;
        // Create the type itself
        type_trie->elements[lower].function_type = function_type;

        return type_trie->elements[lower].function_type;
    }
    else
    {
        type_trie->elements[lower].elem = *type_seq;
        type_trie->elements[lower].next = allocate_type_trie();

        const type_t* result =
        create_elements(type_trie->elements[lower].next,
                type_seq + 1,
                function_type, length - 1);

        return result;
    }
}

type_trie_t* allocate_type_trie(void)
{
    type_trie_t* t = NEW0(type_trie_t);
    t->capacity = 1;
    t->elements = NEW_VEC0(type_trie_element_t, t->capacity);
    return t;
}

void insert_type_trie(type_trie_t* trie, const type_t** type_seq, int num_types, const type_t* funct_type)
{
    insert_list(trie, type_seq, funct_type, num_types);
}

const type_t* lookup_type_trie(type_trie_t* trie, const type_t** type_seq, int num_types)
{
    return lookup_list(trie, type_seq, num_types);
}
