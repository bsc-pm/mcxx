/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#include "cxx-process.h"
#include "cxx-utils.h"
#include "uniquestr.h"
#include "filename.h"

#include <string.h>
#include <stdarg.h>

// Compilation options
compilation_process_t compilation_process = { 0 };

void add_new_file_to_compilation_process(
        compilation_file_process_t* current_file_process,
        const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration)
{
    translation_unit_t* translation_unit = (translation_unit_t*)calloc(1, sizeof(*translation_unit));
    // Initialize with the translation unit root tree
    translation_unit->input_filename = uniquestr(file_path);

    compilation_file_process_t *new_compiled_file = (compilation_file_process_t*) calloc(1, sizeof(*new_compiled_file));

    configuration->verbose = CURRENT_CONFIGURATION->verbose;
    configuration->do_not_link = CURRENT_CONFIGURATION->do_not_link;
    configuration->do_not_compile = CURRENT_CONFIGURATION->do_not_compile;
    configuration->do_not_prettyprint = CURRENT_CONFIGURATION->do_not_prettyprint;

    new_compiled_file->translation_unit = translation_unit;
    new_compiled_file->compilation_configuration = configuration;

    if ((configuration->do_not_link
            || configuration->do_not_compile)
            && output_file != NULL)
    {
        translation_unit->output_filename = uniquestr(output_file);
    }

    if (current_file_process == NULL)
    {
        P_LIST_ADD(compilation_process.translation_units, 
                compilation_process.num_translation_units, 
                new_compiled_file);
    }
    else
    {
        P_LIST_ADD(current_file_process->secondary_translation_units,
                current_file_process->num_secondary_translation_units,
                new_compiled_file);
    }
}

unsigned long long int _bytes_dynamic_lists = 0;

unsigned long long dynamic_lists_used_memory(void)
{
    return _bytes_dynamic_lists;
}

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

#define LONG_MESSAGE_SIZE 512
    char* long_message = calloc(sizeof(char), LONG_MESSAGE_SIZE);

    va_start(ap, function_name);
    vsnprintf(long_message, LONG_MESSAGE_SIZE-1, sanitized_message, ap);
    long_message[LONG_MESSAGE_SIZE-1] = '\0';
#undef LONG_MESSAGE_SIZE

    char* kind_copy = strdup(kind);

    char *start, *end;

    start = kind_copy;

    while (*start != '\0'
            && (end = strchr(start, '\n')) != NULL)
    {
        *end = '\0';
        fprintf(stderr, "%s:%d(%s): %s\n", give_basename(source_file), line, function_name, start);
        start = end + 1;
    }

    if (*start != '\0')
    {
        fprintf(stderr, "%s:%d(%s): %s\n", give_basename(source_file), line, function_name, start);
    }

    start = long_message;

    while (*start != '\0'
            && (end = strchr(start, '\n')) != NULL)
    {
        *end = '\0';
        fprintf(stderr, "%s:%d(%s): %s\n", give_basename(source_file), line, function_name, start);
        start = end + 1;
    }

    if (*start != '\0')
    {
        fprintf(stderr, "%s:%d(%s): %s\n", give_basename(source_file), line, function_name, start);
    }

    free(kind_copy);
    free(sanitized_message);
    free(long_message);
}

void running_error(const char* message, ...)
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
    
    va_start(ap, message);
    vfprintf(stderr, sanitized_message, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    if (CURRENT_CONFIGURATION->debug_options.abort_on_ice)
        raise(SIGABRT);

    free(sanitized_message);

    exit(EXIT_FAILURE);
}
