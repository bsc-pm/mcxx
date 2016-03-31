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

#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <execinfo.h>

#include <unistd.h>

#include "cxx-process.h"
#include "cxx-utils.h"
#include "uniquestr.h"
#include "filename.h"

#include "mem.h"

// Compilation options
compilation_process_t compilation_process;

#define BACKTRACE_SIZE 1024
static void *backtrace_buffer[BACKTRACE_SIZE];

translation_unit_t* add_new_file_to_compilation_process(
        compilation_file_process_t* current_file_process,
        const char* file_path,
        const char* output_file,
        compilation_configuration_t* configuration,
        int tag)
{
    ERROR_CONDITION(tag < 0, "Invalid tag", 0);

    translation_unit_t* translation_unit = NEW0(translation_unit_t);
    // Initialize with the translation unit root tree
    translation_unit->input_filename = uniquestr(file_path);

    compilation_file_process_t *new_compiled_file = NEW0(compilation_file_process_t);

    configuration->verbose = CURRENT_CONFIGURATION->verbose;
    configuration->do_not_link = CURRENT_CONFIGURATION->do_not_link;
    configuration->do_not_compile = CURRENT_CONFIGURATION->do_not_compile;
    configuration->do_not_prettyprint = CURRENT_CONFIGURATION->do_not_prettyprint;

    new_compiled_file->translation_unit = translation_unit;
    new_compiled_file->compilation_configuration = configuration;
    new_compiled_file->tag = tag;

    if ((configuration->do_not_link
            || configuration->do_not_compile)
            && output_file != NULL)
    {
        translation_unit->output_filename = uniquestr(output_file);
    }

    // Give a fallback value
    if (configuration->do_not_link
            && configuration->do_not_compile
            && translation_unit->output_filename == NULL)
    {
        translation_unit->output_filename = "-";
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
    return translation_unit;
}

void debug_message(const char* message, const char* kind, const char* source_file, unsigned int line, const char* function_name, ...)
{
    va_list ap;
    char* sanitized_message = xstrdup(message);

    // Remove annoying \n at the end. This will make this function
    // interchangeable with fprintf(stderr, 
    int length = strlen(sanitized_message);

    length--;
    while (length > 0 && sanitized_message[length] == '\n')
    {
        sanitized_message[length] = '\0';
        length--;
    }

    char *long_message = NULL;

    va_start(ap, function_name);
    int ret = vasprintf(&long_message, sanitized_message, ap);
    if (ret < 0)
    {
        // Desperate message
        const char *oom_message = "allocation failure in vasprintf\n";
        int r = write(fileno(stderr), oom_message, strlen(oom_message));
        if (r < 0)
        {
            // Drama. Resort to perror and hope for the best
            perror("write");
        }
        abort();
    }

    char* kind_copy = xstrdup(kind);

    char *start, *end;

    start = kind_copy;

    while (*start != '\0'
            && (end = strchr(start, '\n')) != NULL)
    {
        *end = '\0';
        fprintf(stderr, "%s:%u(%s): %s\n", give_basename(source_file), line, function_name, start);
        start = end + 1;
    }

    if (*start != '\0')
    {
        fprintf(stderr, "%s:%u(%s): %s\n", give_basename(source_file), line, function_name, start);
    }

    start = long_message;

    while (*start != '\0'
            && (end = strchr(start, '\n')) != NULL)
    {
        *end = '\0';
        fprintf(stderr, "%s:%u(%s): %s\n", give_basename(source_file), line, function_name, start);
        start = end + 1;
    }

    if (*start != '\0')
    {
        fprintf(stderr, "%s:%u(%s): %s\n", give_basename(source_file), line, function_name, start);
    }

    DELETE(kind_copy);
    DELETE(sanitized_message);
    DELETE(long_message);

#if defined(HAVE_BACKTRACE) && defined(HAVE_BACKTRACE_SYMBOLS_FD)
    if (debug_options.backtrace_on_ice)
    {
        int nptrs = backtrace(backtrace_buffer, BACKTRACE_SIZE);

        /* The call backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO)
           would produce similar output to the following: */
        backtrace_symbols_fd(backtrace_buffer, nptrs, fileno(stderr));
    }
#endif
}

void fatal_vprintf(const char* message, va_list ap)
{
    char* sanitized_message = xstrdup(message);

    // Remove annoying \n at the end. This will make this function
    // interchangeable with fprintf(stderr, 
    int last = strlen(sanitized_message) - 1;
    while (last > 0 && sanitized_message[last] == '\n')
    {
        sanitized_message[last] = '\0';
        last--;
    }
    
    vfprintf(stderr, sanitized_message, ap);
    fprintf(stderr, "\n");

    if (debug_options.backtrace_on_ice)
    {
        int nptrs = backtrace(backtrace_buffer, BACKTRACE_SIZE);

        /* The call backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO)
           would produce similar output to the following: */
        backtrace_symbols_fd(backtrace_buffer, nptrs, fileno(stderr));
    }

    if (debug_options.abort_on_ice)
        raise(SIGABRT);

    DELETE(sanitized_message);

    exit(EXIT_FAILURE);
}

void fatal_error(const char* message, ...)
{
    va_list ap;

    va_start(ap, message);
    fatal_vprintf(message, ap);
    va_end(ap);
}

// Useful for debugging sessions
extern void _enable_debug(void)
{
    debug_options.enable_debug_code = 1;
}

extern void _disable_debug(void)
{
    debug_options.enable_debug_code = 0;
}
