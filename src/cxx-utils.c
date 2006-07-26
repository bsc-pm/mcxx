#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <signal.h>
#include <gc.h>

#include "cxx-driver.h"
#include "cxx-utils.h"

void debug_message(const char* message, const char* kind, const char* source_file, int line, const char* function_name, ...)
{
	va_list ap;
	char* sanitized_message = GC_STRDUP(message);

	// Remove annoying \n at the end. This will make this function
	// interchangeable with fprintf(stderr, 
	int length = strlen(sanitized_message);

	length--;
	while (length > 0 && sanitized_message[length] == '\n')
	{
		sanitized_message[length] = '\0';
		length--;
	}
	
	char* source_file_copy = GC_STRDUP(source_file);
	
	fprintf(stderr, "%s%s:%d %s: ", kind, basename(source_file_copy), line, function_name);
	va_start(ap, function_name);
	vfprintf(stderr, sanitized_message, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

void running_error(char* message, ...)
{
	va_list ap;
	
	fprintf(stderr, "Error: ");
	va_start(ap, message);
	vfprintf(stderr, message, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	exit(EXIT_FAILURE);
}

int prime_hash(char* key, int hash_size)
{
	int length = strlen(key);
	int result = 0;
	int i;

	for (i = 0; i < length; i++)
	{
		result += key[i];
	}

	return (result % hash_size);
}

char* strappend(char* orig, char* appended)
{
	int total = strlen(orig) + strlen(appended) + 1;

	char* result = GC_CALLOC(total, sizeof(*result));

	strcat(result, orig);
	strcat(result, appended);
	
	return result;
}

char* strprepend(char* orig, char* prepended)
{
	int total = strlen(orig) + strlen(prepended) + 1;

	char* result = GC_CALLOC(total, sizeof(*result));

	strcat(result, prepended);
	strcat(result, orig);

	return result;
}

char* GC_STRDUP(const char* str)
{
	char* result = GC_CALLOC(strlen(str) + 1, sizeof(char));

	strcpy(result, str);

	return result;
}

char* get_unique_name(void)
{
	static int num_var = 100;
	char* result = GC_CALLOC(15, sizeof(char));

	snprintf(result, 14, "$.anon%05d", num_var);

	num_var++;

	return result;
}

char** comma_separate_values(char* value, int *num_elems)
{
	char* comma_string = GC_STRDUP(value);

	char** result = NULL;
	*num_elems = 0;
	
	char* current = strtok(comma_string, ",");

	while (current != NULL)
	{
		P_LIST_ADD(result, *num_elems, GC_STRDUP(current));
		current = strtok(NULL, ",");
	}

	P_LIST_ADD(result, *num_elems, NULL);

	// Do not count the last one
	(*num_elems)--;

	return result;
}

// Temporal files handling routines

typedef struct temporal_file_list_tag
{
	temporal_file_t info;
	struct temporal_file_list_tag* next;
}* temporal_file_list_t;

static temporal_file_list_t temporal_file_list = NULL;

temporal_file_t new_temporal_file()
{
	char* template;
#ifndef _WIN32
	template = strdup("/tmp/mf95_XXXXXX");
	// Create the temporal file
	int file_descriptor = mkstemp(template);

	if (file_descriptor < 0) 
	{
		free(template);
		return NULL;
	}
#else
	template = _tempnam(NULL, "mf95");
	if (template == NULL)
		return NULL;
#endif

	// Save the info of the new file
	temporal_file_t result = calloc(sizeof(*result), 1);
	result->name = template;
	// Get a FILE* descriptor
#ifndef _WIN32
	result->file = fdopen(file_descriptor, "w+");
#else
	result->file = fopen(template, "w+");
#endif
	if (result->file == NULL)
	{
		running_error("Cannot create temporary file (%s)", strerror(errno));
	}

	// Link to the temporal_file_list
	temporal_file_list_t new_file_element = calloc(sizeof(*new_file_element), 1);
	new_file_element->info = result;
	new_file_element->next = temporal_file_list;
	temporal_file_list = new_file_element;

	return result;
}

void temporal_files_cleanup(void)
{
	temporal_file_list_t iter = temporal_file_list;

	while (iter != NULL)
	{
		// close the descriptor
		fclose(iter->info->file);

		// If no keep, remove file
		if (!compilation_options.keep_files)
		{
			remove(iter->info->name);
		}

		temporal_file_list_t old = iter;
		iter = iter->next;
		free(old->info->name);
		free(old);
	}

	temporal_file_list = NULL;
}

