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



#include "cxx-entrylist.h"
#include "cxx-utils.h"
#include <string.h>
#include <stdint.h>

#define NUM_IMMEDIATE 4

unsigned long long _bytes_entry_lists;

struct scope_entry_list_node_tag
{
    scope_entry_t* list[NUM_IMMEDIATE];
    scope_entry_list_node_t* next;
};

struct scope_entry_list_tag
{
    int num_items_list;
    scope_entry_list_node_t* next;
};

static scope_entry_list_node_t* entry_list_node_allocate(void)
{
    return NEW0(scope_entry_list_node_t);
}

static scope_entry_list_t* entry_list_allocate(void)
{
    scope_entry_list_t* new_entry_list = NEW0(scope_entry_list_t);

    new_entry_list->next = entry_list_node_allocate();

    return new_entry_list;
}

scope_entry_list_t* entry_list_new(scope_entry_t* entry)
{
    scope_entry_list_t* result = entry_list_allocate();
    result->num_items_list = 1;
    result->next->list[0] = entry;

    return result;
}

static void entry_list_add_to_pos_rec(scope_entry_list_node_t* list,
        scope_entry_t* entry, int num_pos)
{
    if (num_pos < NUM_IMMEDIATE)
    {
        list->list[num_pos] = entry;
    }
    else
    {
        if (list->next != NULL)
        {
            entry_list_add_to_pos_rec(list->next, entry, num_pos - NUM_IMMEDIATE);
        }
        else
        {
            // We need to create a new entry_list_node
            scope_entry_list_node_t* new_node = entry_list_node_allocate();
            list->next = new_node;
            entry_list_add_to_pos_rec(list->next, entry, num_pos - NUM_IMMEDIATE);
        }
    }
}

static scope_entry_list_node_t* entry_list_prepend_rec(scope_entry_list_node_t* list,
        scope_entry_t* entry,
        int num_items)
{
    if (list == NULL)
    {
        scope_entry_list_node_t* new_entry_list = entry_list_node_allocate();
        new_entry_list->list[0] = entry;
        return new_entry_list;
    }
    else
    {
        scope_entry_t* last = list->list[NUM_IMMEDIATE - 1];
        int i;
        // -2 because we do not want to write past the last element
        for (i = NUM_IMMEDIATE - 2; i >= 0; i--)
        {
            list->list[i+1] = list->list[i];
        }
        list->list[0] = entry;

        if (num_items >= NUM_IMMEDIATE)
        {
            list->next = entry_list_prepend_rec(list->next, last, num_items - NUM_IMMEDIATE);
        }
        return list;
    }
}

scope_entry_list_t* entry_list_prepend(scope_entry_list_t* list,
        scope_entry_t* entry)
{
    scope_entry_list_t* result = NULL;
    if (list == NULL)
    {
        result = entry_list_allocate();
        result->next->list[0] = entry;
        result->num_items_list = 1;
        return result;
    }
    else
    {
        list->next = entry_list_prepend_rec(list->next, entry, list->num_items_list);
        list->num_items_list++;
        return list;
    }
}

scope_entry_list_t* entry_list_add(scope_entry_list_t* list,
        scope_entry_t* entry)
{
    if (list == NULL)
    {
        return entry_list_new(entry);
    }
    else
    {
        entry_list_add_to_pos_rec(list->next, entry, list->num_items_list);
        list->num_items_list++;

        return list;
    }
}

scope_entry_list_t* entry_list_add_once(scope_entry_list_t* list,
        scope_entry_t* entry)
{
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(list);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* current = entry_list_iterator_current(it);

        if (current == entry)
        {
            entry_list_iterator_free(it);
            return list;
        }
    }
    entry_list_iterator_free(it);

    return entry_list_add(list, entry);
}

static void insert_and_shift_right(scope_entry_list_node_t* entry_list, int pos,
        scope_entry_t* new_entry)
{
    if (entry_list == NULL)
        return;

    scope_entry_t* keep = entry_list->list[NUM_IMMEDIATE-1];

    int j;
    for (j = NUM_IMMEDIATE - 2; j >= pos; j--)
    {
        entry_list->list[j + 1] = entry_list->list[j];
    }

    entry_list->list[pos] = new_entry;

    if (entry_list->next != NULL)
    {
        insert_and_shift_right(entry_list->next, 0, keep);
    }
    else if (entry_list->next == NULL && keep != NULL)
    {
        entry_list->next = entry_list_node_allocate();
        entry_list_add_to_pos_rec(entry_list->next, keep, 0);
    }
}

static char entry_list_add_after_rec(scope_entry_list_node_t* list,
        scope_entry_t* position,
        scope_entry_t* entry)
{
    if (list == NULL)
        return 0;

    char found = 0;
    int j;
    for (j = 0; j < NUM_IMMEDIATE; j++)
    {
        if (list->next->list[j] == position)
        {
            found = 1;
            break;
        }
    }

    if (!found)
    {
        // The current node does not contain the symbol 'position'.
        // Try with the next node.
        return entry_list_add_after_rec(list->next, position, entry);
    }

    // The symbol 'position' has been found

    // If this symbol is not in the last position of the current node we add
    // entry in the current node
    if (j < (NUM_IMMEDIATE-1))
    {
        insert_and_shift_right(list, j+1, entry);
    }
    // Otherwise, the symbol should be added in the next node
    else
    {
        //if the next node exists, we add the 'entry' symbol to its list
        if (list->next != NULL)
        {
            insert_and_shift_right(list->next, 0, entry);
        }
        // Otherwise, we create a new node and add 'entry' symbol to its list
        else
        {
            list->next = entry_list_node_allocate();
            entry_list_add_to_pos_rec(list->next, entry, 0);
        }
    }
    return 1;
}

scope_entry_list_t* entry_list_add_after(scope_entry_list_t* list,
        scope_entry_t* position,
        scope_entry_t* entry)
{
    char added_an_element = entry_list_add_after_rec(list->next, position, entry);

    if (added_an_element)
        list->num_items_list++;

    return list;
}

static char entry_list_add_before_rec(
        scope_entry_list_node_t* list,
        scope_entry_t* position,
        scope_entry_t* entry)
{
    if (list == NULL)
        return 0;

    char found = 0;
    int j;
    for (j = 0; j < NUM_IMMEDIATE; j++)
    {
        if (list->list[j] == position)
        {
            found = 1;
            break;
        }
    }

    if (!found)
    {
        return entry_list_add_before_rec(list->next, position, entry);
    }

    // The symbol 'position' has been found

    insert_and_shift_right(list, j, entry);

    return 1;
}

scope_entry_list_t* entry_list_add_before(scope_entry_list_t* list,
        scope_entry_t* position,
        scope_entry_t* entry)
{
    char added_an_element = entry_list_add_before_rec(list->next, position, entry);

    if (added_an_element)
        list->num_items_list++;

    return list;
}

scope_entry_list_t* entry_list_copy(const scope_entry_list_t* list)
{
    if (list == NULL)
        return NULL;

    scope_entry_list_t* result = NEW0(scope_entry_list_t);

    result->num_items_list = list->num_items_list;
    scope_entry_list_node_t* it = list->next;
    scope_entry_list_node_t** current = &(result->next);

    while (it != NULL)
    {
        *current = NEW0(scope_entry_list_node_t);
        int i;
        for (i = 0; i < NUM_IMMEDIATE; i++)
        {
            (*current)->list[i] = it->list[i];
        }
        current = &((*current)->next);
        it = it->next;
    }

    return result;
}
static void entry_list_node_free(scope_entry_list_node_t* list)
{
    if (list == NULL)
        return;

    entry_list_node_free(list->next);
    memset(list, 0, sizeof(*list));
    DELETE(list);
}

void entry_list_free(scope_entry_list_t* list)
{
    if (list == NULL)
        return;

    entry_list_node_free(list->next);
    memset(list, 0, sizeof(*list));
    DELETE(list);
}

// -

int entry_list_size(const scope_entry_list_t* list)
{
    if (list != NULL)
        return list->num_items_list;
    else
        return 0;
}

scope_entry_t* entry_list_head(const scope_entry_list_t* list)
{
    return list->next->list[0];
}

// -

struct scope_entry_list_iterator_tag
{
    int current_pos;
    int total_pos;
    const scope_entry_list_node_t* current_list;
    const scope_entry_list_t* first_list;
};

static scope_entry_list_iterator_t* entry_list_iterator_allocate(void)
{
    return NEW0(scope_entry_list_iterator_t);
}

scope_entry_list_iterator_t* entry_list_iterator_begin(const scope_entry_list_t* list)
{
    scope_entry_list_iterator_t* result = entry_list_iterator_allocate();
    result->first_list = list;
    result->current_list = (list != NULL) ? list->next : NULL;
    result->current_pos = 0;
    result->total_pos = 0;

    return result;
}

scope_entry_t* entry_list_iterator_current(scope_entry_list_iterator_t* it)
{
    return it->current_list->list[it->current_pos];
}

void entry_list_iterator_next(scope_entry_list_iterator_t* it)
{
    it->current_pos++;
    it->total_pos++;
    if (it->current_pos == NUM_IMMEDIATE)
    {
        it->current_pos = 0;
        it->current_list = it->current_list->next;
    }
}

char entry_list_iterator_end(scope_entry_list_iterator_t* it)
{
    return (it->first_list == NULL
            || (it->total_pos >= it->first_list->num_items_list));
}

void entry_list_iterator_free(scope_entry_list_iterator_t* it)
{
    if (it != NULL)
    {
        memset(it, 0, sizeof(*it));
        DELETE(it);
    }
}

static int ptr_comp(const void* p1, const void* p2)
{
    intptr_t i1 = (intptr_t)p1;
    intptr_t i2 = (intptr_t)p2;
    
    if (i1 < i2)
        return -1;
    else if (i1 > i2)
        return 1;
    else
        return 0;
}

scope_entry_list_t* entry_list_merge(const scope_entry_list_t* list1, 
        const scope_entry_list_t* list2)
{
    int size1 = (list1 != NULL ? entry_list_size(list1) : 0);
    scope_entry_t** elems1 = NEW_VEC0(scope_entry_t*, size1 + 1);
    scope_entry_t** p = elems1;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(list1);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        *p = entry;
        p++;
    }
    entry_list_iterator_free(it);

    int size2 = (list2 != NULL ? entry_list_size(list2) : 0);
    scope_entry_t** elems2 = NEW_VEC0(scope_entry_t*, size2 + 1);

    scope_entry_t** q = elems2;

    for (it = entry_list_iterator_begin(list2);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        scope_entry_t* entry = entry_list_iterator_current(it);
        *q = entry;
        q++;
    }
    entry_list_iterator_free(it);

    //   void qsort(void *base, size_t nmemb, size_t size,
    //  int(*compar)(const void *, const void *));
    qsort(elems1, size1, sizeof(*elems1), ptr_comp);
    qsort(elems2, size2, sizeof(*elems2), ptr_comp);

    p = elems1;
    q = elems2;

    scope_entry_list_t* result = NULL;

    while (*p != NULL
            && *q != NULL)
    {
        // Advance repeated elements inside each list
        // (they will be grouped together, right?)
        while (*p == *(p+1))
            p++;
        while (*q == *(q+1))
            q++;

        if (*p < *q)
        {
            result = entry_list_add(result, *p);
            p++;
        }
        else if (*q < *p)
        {
            result = entry_list_add(result, *q);
            q++;
        }
        else
        {
            // They are equal, add it just once
            result = entry_list_add(result, *p);
            p++;
            q++;
        }
    }

    while (*p != NULL)
    {
        result = entry_list_add(result, *p);
        p++;
    }
    while (*q != NULL)
    {
        result = entry_list_add(result, *q);
        q++;
    }

    DELETE(elems2);
    DELETE(elems1);

    return result;
}

char entry_list_contains(const scope_entry_list_t* list, 
        scope_entry_t* entry)
{
    if (list == NULL)
        return 0;

    char result = 0;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(list);
            !entry_list_iterator_end(it) && !result;
            entry_list_iterator_next(it))
    {
        if (entry_list_iterator_current(it) == entry)
        {
            result = 1;
        }
    }
    entry_list_iterator_free(it);

    return result;
}

static scope_entry_t* shift_left_all_elements_from_pos(scope_entry_list_node_t* entry_list, int pos)
{
    if (entry_list == NULL)
        return NULL;

    int j;
    scope_entry_t* first_element = entry_list->list[0];
    for (j = pos + 1; j < NUM_IMMEDIATE; j++)
    {
        entry_list->list[j - 1] = entry_list->list[j];
    }

    entry_list->list[NUM_IMMEDIATE - 1] = shift_left_all_elements_from_pos(entry_list->next, 0);

    return first_element;
}


static int entry_list_remove_rec(scope_entry_list_node_t* entry_list, scope_entry_t* entry)
{
    if (entry_list == NULL)
        return 0;

    int i;
    int number_of_matches = 0;
    for (i = 0; i < NUM_IMMEDIATE; i++)
    {
        if (entry_list->list[i] == entry)
        {
            number_of_matches++;
            shift_left_all_elements_from_pos(entry_list, i);
        }
    }

    return (entry_list_remove_rec(entry_list->next, entry) + number_of_matches);
}

scope_entry_list_t* entry_list_remove(scope_entry_list_t* entry_list, scope_entry_t* entry)
{
    if (entry_list == NULL)
        return entry_list;

    int number_of_matches = entry_list_remove_rec(entry_list->next, entry);

    entry_list->num_items_list -= number_of_matches;
    return entry_list;
}

void entry_list_to_symbol_array(scope_entry_list_t* list, scope_entry_t*** array, int* num_items)
{
    int size = entry_list_size(list);
    *array = NEW_VEC0(scope_entry_t*, size);

    *num_items = 0;
    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(list); !entry_list_iterator_end(it); entry_list_iterator_next(it))
    {
        (*array)[*num_items] = entry_list_iterator_current(it);
        (*num_items)++;
    }
    entry_list_iterator_free(it);
}

scope_entry_list_t* entry_list_from_symbol_array(int num_items, scope_entry_t** list)
{
    if(num_items <= 0)
    {
        return NULL;
    }

    scope_entry_list_t* result = entry_list_allocate();
    int ind;
    for (ind = 0; ind < num_items; ++ind)
    {
        result = entry_list_add(result, list[ind]);
    }
    return result;
}

scope_entry_list_t* entry_list_concat(const scope_entry_list_t* a, const scope_entry_list_t* b)
{
    scope_entry_list_t* result = NULL;

    scope_entry_list_iterator_t* it = NULL;
    for (it = entry_list_iterator_begin(a);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        result = entry_list_add(result, entry_list_iterator_current(it));
    }
    entry_list_iterator_free(it);

    for (it = entry_list_iterator_begin(b);
            !entry_list_iterator_end(it);
            entry_list_iterator_next(it))
    {
        result = entry_list_add(result, entry_list_iterator_current(it));
    }
    entry_list_iterator_free(it);

    return result;
}
