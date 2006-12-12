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

void extensible_struct_allocate_field(extensible_schema_t* schema,
		extensible_struct_t* extensible_struct,
		int schema_field_order)
{
	int field_size = schema->field_sizes[schema_field_order];

	extensible_struct->data = realloc(extensible_struct->data,
			(extensible_struct->data_size + field_size) * sizeof(char));

	// Clear the field data
	int i;
	for (i = extensible_struct->data_size; i < (extensible_struct->data_size + field_size); i++)
	{
		extensible_struct->data[i] = 0;
	}

	extensible_struct->offsets_data[schema_field_order] = extensible_struct->data_size;

	extensible_struct->data_size += field_size;
}

void extensible_struct_activate_field(extensible_schema_t* schema,
		extensible_struct_t* extensible_struct,
		int schema_field_order)
{
	// We have to reallocate up to this field
	if (schema_field_order >= extensible_struct->num_active_fields)
	{
		int previous_active_fields = extensible_struct->num_active_fields;
		extensible_struct->num_active_fields = schema_field_order + 1;
		extensible_struct->offsets_data = 
			realloc(extensible_struct->offsets_data, 
					(extensible_struct->num_active_fields)*(sizeof(*(extensible_struct->offsets_data))));

		// Set the unallocated to -1
		int i = previous_active_fields;
		while (i < extensible_struct->num_active_fields)
		{
			extensible_struct->offsets_data[i] = -1;
			i++;
		}
	}

	if (extensible_struct->offsets_data[schema_field_order] == -1)
	{
		extensible_struct_allocate_field(schema, extensible_struct, schema_field_order);
	}
}

void* extensible_struct_get_field_pointer(extensible_schema_t* schema,
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

	// Now check if it is active, otherwise activate it
	extensible_struct_activate_field(schema, extensible_struct, schema_field_order);

	return &(extensible_struct->data[extensible_struct->offsets_data[schema_field_order]]);
}
