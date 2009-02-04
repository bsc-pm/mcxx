/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef EXTSTRUCT_H
#define EXTSTRUCT_H

#include <stdlib.h>
#include "hash.h"

#ifdef _WIN32
  #ifdef LIBEXTSTRUCT_DLL_EXPORT
    #define LIBEXTSTRUCT_EXTERN extern __declspec(dllexport)
  #else
    #define LIBEXTSTRUCT_EXTERN extern __declspec(dllimport)
  #endif
#else
  #define LIBEXTSTRUCT_EXTERN extern
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This is the extensible_schema_t
 * it stores a list of field_names and a list
 * of field_sizes.
 *
 * Every field_name is a char* and every field_size
 * is a size_t*
 */

struct extensible_schema_item_tag
{
    size_t size;
    int field_order;
};

typedef struct extensible_schema_item_tag extensible_schema_item_t;

struct extensible_schema_tag
{
    Hash *hash;
    int num_fields;
};

#define EMPTY_EXTENSIBLE_SCHEMA {.hash = 0, .num_fields = 0}

typedef struct extensible_schema_tag extensible_schema_t;

struct extensible_data_item_tag
{
    // Index of the field in the schema
    int schema_index;

    // The data
    char* data;
};

typedef struct extensible_data_item_tag extensible_data_item_t;

struct extensible_struct_tag
{
    // The related schema of this extensible_struct_t
    extensible_schema_t *schema;

    int num_items;
    extensible_data_item_t *items;
};

typedef struct extensible_struct_tag extensible_struct_t;

// Schema operations
LIBEXTSTRUCT_EXTERN void extensible_schema_init(extensible_schema_t* schema);
LIBEXTSTRUCT_EXTERN int extensible_schema_add_field(extensible_schema_t* schema, 
        const char* field_name, 
        size_t field_size);
LIBEXTSTRUCT_EXTERN int extensible_schema_add_field_if_needed(extensible_schema_t* schema,
        const char *field_name,
        size_t field_size);
LIBEXTSTRUCT_EXTERN char extensible_schema_extended_field_exists(extensible_schema_t *schema,
        const char *field_name);

// Extensible struct operations
LIBEXTSTRUCT_EXTERN void extensible_struct_init(extensible_struct_t* extensible_struct, extensible_schema_t* schema);
LIBEXTSTRUCT_EXTERN void* extensible_struct_get_field_pointer(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct,
        const char* field_name);
LIBEXTSTRUCT_EXTERN void *extensible_struct_get_field_pointer_lazy(extensible_schema_t* schema,
        extensible_struct_t* extensible_struct,
        const char* field_name,
        char* is_found);

#ifdef __cplusplus
}
#endif

#endif // EXTSTRUCT_H
