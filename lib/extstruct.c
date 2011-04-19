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



#include "extstruct.h"
#include "uniquestr.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "red_black_tree.h"

#define warning_message(...) \
{ \
  fprintf(stderr, "%s:%d(%s) ", __FILE__, __LINE__, __FUNCTION__); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
}

static int strcmp_vptr(const void* v1, const void *v2)
{
    return strcmp((const char*)v1, (const char*)v2);
}

static void null_dtor_func(const void *v) { }

void extensible_struct_init(extensible_struct_t** extensible_struct)
{
    (*extensible_struct) = calloc(1, sizeof((*extensible_struct)));
    (*extensible_struct)->hash = rb_tree_create(strcmp_vptr, null_dtor_func, null_dtor_func);
}

void extensible_struct_set_field(extensible_struct_t* extensible_struct, 
        const char* field_name,
        void *data)
{
    rb_tree_insert(extensible_struct->hash, uniquestr(field_name), data);
}

void* extensible_struct_get_field(extensible_struct_t* extensible_struct, 
        const char* field_name)
{
    if (extensible_struct->hash == NULL)
        return NULL;

    rb_red_blk_node* n = rb_tree_query(extensible_struct->hash, uniquestr(field_name));
    if (n != NULL)
    {
        return rb_node_get_info(n);
    }
    else
    {
        return NULL;
    }
}

typedef
struct data_tag
{
    int num_fields;
    const char **keys;
    const void **data;
} data_t;

static void walk_func(const void* key, void* value, void* p)
{
    data_t* d = (data_t*)p;

    d->num_fields++;

    d->keys = realloc(d->keys, d->num_fields * sizeof(*d->keys));
    d->keys[d->num_fields - 1] = (const char*)key;
    d->data = realloc(d->data, d->num_fields * sizeof(*d->data));
    d->data[d->num_fields - 1] = value;
}

void extensible_struct_get_all_data(extensible_struct_t* extensible_struct,
        int *num_fields,
        const char ***keys,
        const void ***data)
{
    data_t d;
    memset(&d, 0, sizeof(d));

    rb_tree_walk(extensible_struct->hash, walk_func, &d);

    *num_fields = d.num_fields;
    *keys = d.keys;
    *data = d.data;
}
