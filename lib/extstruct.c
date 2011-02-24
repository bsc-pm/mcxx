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
#include "hash_iterator.h"

#define warning_message(...) \
{ \
  fprintf(stderr, "%s:%d(%s) ", __FILE__, __LINE__, __FUNCTION__); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
}

#define HASH_SIZE (23)

void extensible_schema_init(extensible_schema_t* schema)
{
    memset(schema, 0, sizeof(*schema));
    schema->hash = hash_create(HASH_SIZE, HASHFUNC(prime_hash), KEYCMPFUNC(strcmp));
}

// Adds a new field into the schema
int extensible_schema_add_field(extensible_schema_t* schema, 
        const char* field_name, 
        size_t field_size)
{
    extensible_schema_item_t* schema_item = calloc(1, sizeof(*schema_item));

    const char* c = uniquestr(field_name);

    schema_item->size = field_size;
    schema_item->field_name = c;
    schema_item->field_order = schema->num_fields;

    hash_put(schema->hash, c, schema_item);

    schema->num_fields++;

    return schema->num_fields;
}

char extensible_schema_extended_field_exists(extensible_schema_t *schema,
        const char *field_name)
{
    extensible_schema_item_t* schema_item = 
        (extensible_schema_item_t*)hash_get(schema->hash, field_name);

    return (schema_item != NULL);
}

// Adds a new field if it did not exist already
int extensible_schema_add_field_if_needed(extensible_schema_t* schema,
        const char *field_name,
        size_t field_size)
{
    extensible_schema_item_t* schema_item = 
        (extensible_schema_item_t*)hash_get(schema->hash, field_name);

    int result = schema->num_fields;
    if (schema_item == NULL)
    {
        result = extensible_schema_add_field(schema, field_name, field_size);
    }
    else
    {
        if (schema_item->size != field_size)
        {
            fprintf(stderr, "Warning: Field '%s', size of existing field %zd does not match the requested one %zd. Ignoring new size.\n",
                    field_name,
                    schema_item->size, 
                    field_size);
        }
    }

    return result;
}

static
extensible_schema_item_t* extensible_schema_get_field_order(extensible_schema_t* schema,
        const char* field_name)
{
    extensible_schema_item_t* schema_item = NULL;
    schema_item = (extensible_schema_item_t*)hash_get(schema->hash, /* safe */(char*)field_name);

    return schema_item;
}

void extensible_struct_init(extensible_struct_t* extensible_struct, extensible_schema_t* schema)
{
    memset(extensible_struct, 0, sizeof(*extensible_struct));
    extensible_struct->schema = schema;
}

void *extensible_struct_get_field_pointer_lazy(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct,
        const char* field_name,
        char* is_found)
{
    if (is_found != NULL)
        *is_found = 0;

    if (schema == NULL || schema->hash == NULL)
    {
        warning_message("Schema is NULL\n");
        return NULL;
    }

    if (extensible_struct == NULL)
    {
        warning_message("Extensible struct is NULL\n");
        return NULL;
    }

    extensible_schema_item_t *schema_item = extensible_schema_get_field_order(schema, field_name);

    if (schema_item == NULL)
    {
        return NULL;
    }

    int schema_field_order = schema_item->field_order;
    // size_t schema_field_size = schema_item->size;

    if (is_found != NULL)
        *is_found = 1;
    
    int i;
    for (i = 0; i < extensible_struct->num_items; i++)
    {
        if (extensible_struct->items[i].schema_index == schema_field_order)
        {
            return extensible_struct->items[i].data;
        }
    }

    // If it does not exist, do not allocate it
    return NULL;
}


void *extensible_struct_get_field_pointer(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct,
        const char* field_name)
{
    if (schema == NULL || schema->hash == NULL)
    {
        warning_message("Schema is NULL\n");
        return NULL;
    }

    if (extensible_struct == NULL)
    {
        warning_message("Extensible struct is NULL\n");
        return NULL;
    }

    // First get the order within the schema of this field
    extensible_schema_item_t *schema_item = extensible_schema_get_field_order(schema, field_name);

    if (schema_item == NULL)
    {
        warning_message("Field '%s' not found in the schema", field_name);
        return NULL;
    }

    int schema_field_order = schema_item->field_order;
    size_t schema_field_size = schema_item->size;

    int i;
    for (i = 0; i < extensible_struct->num_items; i++)
    {
        if (extensible_struct->items[i].schema_index == schema_field_order)
        {
            return extensible_struct->items[i].data;
        }
    }

    // Allocate the field
    extensible_struct->num_items++;
    extensible_struct->items = realloc(extensible_struct->items, 
            extensible_struct->num_items * sizeof(*extensible_struct->items));

    extensible_data_item_t * new_data = &(extensible_struct->items[extensible_struct->num_items - 1]);

    new_data->schema_index = schema_field_order;
    new_data->data = calloc(1, schema_field_size);

    return new_data->data;
}

int extensible_struct_get_num_fields(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct)
{
    return extensible_struct->num_items;
}

const char* extensible_struct_get_field_num(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct,
        int num)
{
    // FIXME - This is so inefficient, having to traverse all the schema fields
    // just to know its field index and then get the field_name!
    Iterator *it = (Iterator*)hash_iterator_create(schema->hash);

    for (iterator_first(it); !iterator_finished(it); iterator_next(it))
    {
        extensible_schema_item_t* schema_item = NULL;
        schema_item = (extensible_schema_item_t*)iterator_item(it);

        if (extensible_struct->items[num].schema_index == schema_item->field_order)
        {
            return schema_item->field_name;
        }
    }

    return NULL;
}

