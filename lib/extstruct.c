/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "extstruct.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define warning_message(...) \
{ \
  fprintf(stderr, "%s:%d(%s) ", __FILE__, __LINE__, __FUNCTION__); \
  fprintf(stderr, __VA_ARGS__); \
  fprintf(stderr, "\n"); \
}

extensible_schema_t extensible_schema_new()
{
    extensible_schema_t result;

    extensible_schema_init(&result);

    return result;
}

void extensible_schema_init(extensible_schema_t* schema)
{
    memset(schema, 0, sizeof(*schema));
}

// Adds a new field into the schema
int extensible_schema_add_field(extensible_schema_t* schema, 
        const char* field_name, 
        size_t field_size)
{
    int num_fields = schema->num_fields+1;

    // Realloc the arrays
    schema->field_names = realloc(schema->field_names, 
            sizeof(*(schema->field_names))*num_fields);
    schema->field_sizes = realloc(schema->field_sizes, 
            sizeof(*(schema->field_sizes))*num_fields);

    // Store the information of this new field
    schema->field_names[num_fields-1] = strdup(field_name);
    schema->field_sizes[num_fields-1] = field_size;

    schema->num_fields++;

    return (num_fields-1);
}

int extensible_schema_get_field_order(extensible_schema_t* schema,
        const char* field_name)
{
    int schema_order = -1;
    int i;

    for (i = 0; (i < schema->num_fields) && (schema_order < 0); i++)
    {
        if (strcmp(schema->field_names[i], field_name) == 0)
        {
            schema_order = i;
        }
    }

    return schema_order;
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
    *is_found = 0;
    if (schema == NULL)
    {
        warning_message("Schema is NULL\n");
        return NULL;
    }

    if (extensible_struct == NULL)
    {
        warning_message("Extensible struct is NULL\n");
        return NULL;
    }

    int schema_field_order = extensible_schema_get_field_order(schema, field_name);

    if (schema_field_order < 0)
    {
        warning_message("Field '%s' not found in the schema", field_name);
        return NULL;
    }

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
    if (schema == NULL)
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
    int schema_field_order = extensible_schema_get_field_order(schema, field_name);

    if (schema_field_order < 0)
    {
        warning_message("Field '%s' not found in the schema", field_name);
        return NULL;
    }

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
    new_data->data = calloc(1, schema->field_sizes[schema_field_order]);

    return new_data->data;
}
