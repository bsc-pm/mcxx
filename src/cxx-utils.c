#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <signal.h>

#include "cxx-utils.h"

void debug_message(const char* message, const char* kind, const char* source_file, int line, const char* function_name, ...)
{
	va_list ap;
	char* sanitized_message = strdup(message);

	// Remove annoying \n at the end. This will make this function
	// interchangeable with fprintf(stderr, 
	int length = strlen(sanitized_message);

	length--;
	while (length > 0 && sanitized_message[length] == '\n')
	{
		sanitized_message[length] = '\0';
		length--;
	}
	
	char* source_file_copy = strdup(source_file);
	
	fprintf(stderr, "%s%s:%d %s: ", kind, basename(source_file_copy), line, function_name);
	va_start(ap, function_name);
	vfprintf(stderr, sanitized_message, ap);
	va_end(ap);
	fprintf(stderr, "\n");

	free(sanitized_message);
	free(source_file_copy);
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
