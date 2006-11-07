#ifndef EXTSTRUCT_H
#define EXTSTRUCT_H

#include <stdlib.h>

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

struct extensible_schema_tag
{
	int num_fields;
	char** field_names;
	size_t* field_sizes;
};

typedef struct extensible_schema_tag extensible_schema_t;

/*
 * This is the extensible_struct_t
 * for a given schema it stores the related information.
 *
 * Not all fields of the schema are saved, only those that have
 * been written or read to are saved.
 *
 * Fields are stored in the "data" array. They are given offsets sequentially
 * in this array when they are referenced for the first time.
 *
 *   num_active_fields - stores the number of fields that have offset_data
 *                       (even if it is -1)
 *   offsets_data - array that stores the offset we have to add to 'data' to get
 *                  the information of that field. If this field is -1 this means
 *                  the field has not been given allocation in the 'data' array
 *                  (this happens if the user references a field defined later
 *                  in the schema before referencing the previous field)
 *   last_offset -  stores the last offset assigned to an active field. This is used
 *                  when allocating a field that has got active.
 *
 */
struct extensible_struct_tag
{
	// The related schema of this extensible_struct_t
	extensible_schema_t* schema;

	// Number of fields 
	int num_active_fields;

	// Offsets of structs within 'data' array
    int* offsets_data;

	// The data
	int data_size;
	char* data;
};

typedef struct extensible_struct_tag extensible_struct_t;


// Schema operations
extensible_schema_t extensible_schema_new();
void extensible_schema_init(extensible_schema_t* schema);
int extensible_schema_add_field(extensible_schema_t* schema, 
		const char* field_name, 
		size_t field_size);
int extensible_schema_get_field_order(extensible_schema_t* schema,
		const char* field_name);

// Extensible struct operations
void extensible_struct_init(extensible_struct_t* extensible_struct, extensible_schema_t* schema);
void extensible_struct_allocate_field(extensible_schema_t* schema,
		extensible_struct_t* extensible_struct,
		int schema_field_order);
void extensible_struct_activate_field(extensible_schema_t* schema,
		extensible_struct_t* extensible_struct,
		int schema_field_order);
void* extensible_struct_get_field_pointer(extensible_schema_t* schema,
		extensible_struct_t* extensible_struct,
		const char* field_name);

#ifdef __cplusplus
}
#endif

#endif // EXTSTRUCT_H
