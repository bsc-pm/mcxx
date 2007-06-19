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
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <signal.h>


#include "cxx-driver.h"
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
}

void running_error(char* message, ...)
{
    va_list ap;
    
    fprintf(stderr, "Error: ");
    va_start(ap, message);
    vfprintf(stderr, message, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    if (CURRENT_CONFIGURATION(debug_options.abort_on_ice))
        raise(SIGABRT);

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

char* strappend(const char* orig, const char* appended)
{
    int total = strlen(orig) + strlen(appended) + 1;

    char* result = calloc(total, sizeof(*result));

    strcat(result, orig);
    strcat(result, appended);
    
    return result;
}

char* strprepend(const char* orig, const char* prepended)
{
    int total = strlen(orig) + strlen(prepended) + 1;

    char* result = calloc(total, sizeof(*result));

    strcat(result, prepended);
    strcat(result, orig);

    return result;
}


char* get_unique_name(void)
{
    static int num_var = 100;
    char* result = calloc(15, sizeof(char));

    snprintf(result, 14, "$.anon%05d", num_var);

    num_var++;

    return result;
}

char** comma_separate_values(char* value, int *num_elems)
{
    char** result = NULL;
    *num_elems = 0;

    if (value != NULL)
    {
        char* comma_string = strdup(value);
        char* current = strtok(comma_string, ",");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, strdup(current));
            current = strtok(NULL, ",");
        }
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}

char** blank_separate_values(char* value, int *num_elems)
{
    char** result = NULL;
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
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}

char* give_basename(const char* c)
{
    char* result = basename(strdup(c));
    return strdup(result);
}

char* give_dirname(const char* c)
{
    char* result = dirname(strdup(c));
    return strdup(result);
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
    template = compilation_process.exec_basename;
    template = strappend("/tmp/", template);
    template = strappend(template, "_XXXXXX");

    // Create the temporal file
    int file_descriptor = mkstemp(template);

    if (file_descriptor < 0) 
    {
        return NULL;
    }
#else
    template = _tempnam(NULL, "mcxx");
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
        if (!CURRENT_CONFIGURATION(keep_files))
        {
            if (iter->info != NULL)
            {
                if (CURRENT_CONFIGURATION(verbose))
                {
                    fprintf(stderr, "Removing temporal filename '%s'\n", iter->info->name);
                }
                remove(iter->info->name);
            }
        }

        iter = iter->next;
    }

    temporal_file_list = NULL;
}

char* get_extension_filename(char* filename)
{
    return strrchr(filename, '.');
}

int execute_program(char* program_name, char** arguments)
{
    int num = count_null_ended_array((void**)arguments);

    char** execvp_arguments = calloc(num + 1 + 1, sizeof(char*));

    execvp_arguments[0] = program_name;

    int i;
    for (i = 0; i < num; i++)
    {
        execvp_arguments[i+1] = arguments[i];
    }

    execvp_arguments[i+1] = NULL;

    if (CURRENT_CONFIGURATION(verbose))
    {
        int j = 0;
        while (execvp_arguments[j] != NULL)
        {
            fprintf(stderr, "%s ", execvp_arguments[j]);
            j++;
        }
        fprintf(stderr, "\n");
    }

    // This routine is UNIX-only
    pid_t spawned_process;
    spawned_process = fork();
    if (spawned_process < 0) 
    {
        running_error("Could not fork to execute subprocess '%s' (%s)", program_name, strerror(errno));
    }
    else if (spawned_process == 0) // I'm the spawned process
    {
        execvp(program_name, execvp_arguments);

        // Execvp should not return
        running_error("Execution of subprocess '%s' failed (%s)", program_name, strerror(errno));
    }
    else // I'm the parent
    {
        // Wait for my son
        int status;
        wait(&status);
        if (WIFEXITED(status))
        {
            return (WEXITSTATUS(status));
        }
        else if (WIFSIGNALED(status))
        {
            fprintf(stderr, "Subprocess '%s' was ended with signal %d\n",
                    program_name, WTERMSIG(status));

            return 1;
        }
        else
        {
            internal_error(
                    "Subprocess '%s' ended but neither by normal exit nor signal", 
                    program_name);
        }
    }
}

int count_null_ended_array(void** v)
{
    int result = 0;
    if (v == NULL)
    {
        return result;
    }

    while (v[result] != NULL)
    {
        result++;
    }

    return result;
}

void seen_filename(char* filename)
{
    if (reference_to_seen_filename(filename) != NULL)
        return;

    P_LIST_ADD(seen_file_names, num_seen_file_names, strdup(filename));
}

char* reference_to_seen_filename(char* filename)
{
    int i;
    for (i = 0; i < num_seen_file_names; i++)
    {
        if (strcmp(seen_file_names[i], filename) == 0)
        {
            return seen_file_names[i];
        }
    }

    return NULL;
}

void timing_start(timing_t* t)
{
    memset(t, 0, sizeof(*t));
    
    gettimeofday(&(t->start), NULL);
}

void timing_end(timing_t* t)
{
    gettimeofday(&(t->end), NULL);

    double start_value = t->start.tv_sec*1e6 + t->start.tv_usec;
    double end_value = t->end.tv_sec*1e6 + t->end.tv_usec;

    double diff_value = end_value - start_value;

    t->elapsed_time = diff_value / 1e6;
}

double timing_elapsed(const timing_t* t)
{
    return (t->elapsed_time);
}

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
