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
    
    fprintf(stderr, "%s%s:%d %s: ", kind, give_basename(source_file_copy), line, function_name);
    va_start(ap, function_name);
    vfprintf(stderr, sanitized_message, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    free(source_file_copy);
    free(sanitized_message);
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

    if (CURRENT_CONFIGURATION(debug_options.abort_on_ice))
        raise(SIGABRT);

    free(sanitized_message);

    exit(EXIT_FAILURE);
}


const char* strappend(const char* orig, const char* appended)
{
    int total = strlen(orig) + strlen(appended) + 1;

    char append_tmp[total];
    append_tmp[0] = '\0';

    strcat(append_tmp, orig);
    strcat(append_tmp, appended);

    return uniquestr(append_tmp);
}

const char* strprepend(const char* orig, const char* prepended)
{
    return strappend(prepended, orig);
}

const char* get_unique_name(void)
{
    static int num_var = 100;
    char result[15];

    snprintf(result, 14, "$.anon%05d", num_var);

    return uniquestr(result);
}

const char** comma_separate_values(const char* value, int *num_elems)
{
    const char** result = NULL;
    *num_elems = 0;

    if (value != NULL)
    {
        char* comma_string = strdup(value);
        char* current = strtok(comma_string, ",");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, uniquestr(current));
            current = strtok(NULL, ",");
        }

        free(comma_string);
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}

const char** blank_separate_values(const char* value, int *num_elems)
{
    const char** result = NULL;
    *num_elems = 0;

    if (value != NULL)
    {
        char* comma_string = strdup(value);
        char* current = strtok(comma_string, " \t");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, strdup(current));
            current = strtok(NULL, " \t");
        }

        free(comma_string);
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}

const char* give_basename(const char* c)
{
    char *tmp = strdup(c);
    char *basename_tmp = basename(tmp);

    const char* result = uniquestr(basename_tmp);
    free(tmp);

    return result;
}

const char* give_dirname(const char* c)
{
    char *tmp = strdup(c);
    char *dirname_tmp = dirname(tmp);

    const char* result = uniquestr(dirname_tmp);
    free(tmp);

    return result;
}

// Temporal files handling routines

static char is_blank(char c)
{
    return (c == ' ' || c == '\t');
}

char is_blank_string(const char* c)
{
    char result = 1;

    while (result && (c != '\0'))
    {
        result &= is_blank(*c);
        c++;
    }

    return result;
}

void *counted_calloc(size_t nmemb, size_t size, unsigned long long *counter)
{
    if (counter != NULL)
    {
        (*counter) += size;
    }

    return calloc(nmemb, size);
}
