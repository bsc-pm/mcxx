/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




/*
 * Tiny preprocessor
 *
 * A preprocessor intended to regenerate specific versions
 * from common files using conditional compilation
 *
 * Roger Ferrer Ibáñez <roger.ferrer@bsc.es>
 *
 */

#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <regex.h>


#define HELP_MESSAGE \
"Syntax: \n" \
"  tpp -o output_file [-l] [-D define...] [-I directory...] input_file" \
"\n"

#define MAX_TEXT_LINE 1024
#define MAX_INCLUDE_NESTING 32
#define MAX_IF_NESTING 32
#define MAX_INCLUDE_DIRS 32
#define MAX_DEFINES 32

static int num_include_dirs = 1;
static char* include_dirs[MAX_INCLUDE_DIRS] = {"."};

static char enable_line_markers = 0;

static int num_defines = 0;
static char* defines[MAX_DEFINES];

static char* input_file = NULL;
static char* output_file = NULL;

static void help_message(char* error_message)
{
    if (error_message != NULL)
    {
        fprintf(stderr, "Error: %s\n\n", error_message);
    }
    fprintf(stderr, HELP_MESSAGE);
    exit(EXIT_FAILURE);
}

#define GETOPT_OPTIONS "o:D:I:l"
static void parse_arguments(int argc, char* argv[])
{
    int n;
    while ((n = getopt(argc, argv, GETOPT_OPTIONS)) != -1)
    {
        char c = (char)n;

        switch (c)
        {
            case 'o' :
                {
                    if (output_file != NULL)
                    {
                        help_message("Output file more than once specified\n");
                    }

                    output_file = strdup(optarg);
                    break;
                }
           case 'D' :
                {
                    if (num_defines == MAX_DEFINES)
                    {
                        fprintf(stderr, "Too many defines\n");
                        exit(EXIT_FAILURE);
                    }

                    defines[num_defines] = strdup(optarg);
                    num_defines++;
                    break;
                }
            case 'I' :
                {
                    if (num_include_dirs == MAX_INCLUDE_DIRS)
                    {
                        fprintf(stderr, "Too many include directories\n");
                        exit(EXIT_FAILURE);
                    }

                    include_dirs[num_include_dirs] = strdup(optarg);
                    num_include_dirs++;
                    break;
                }
           case 'l' :
                {
                    enable_line_markers = 1;
                    break;
                }
           default :
                {
                    break;
                }
        }
    }

    if (output_file == NULL)
    {
        help_message("Output file not specified");
    }

    if (optind >= argc)
    {
        help_message("Input file not specified");
    }

    if ((optind + 1) < argc)
    {
        help_message("More than one input file specified");
    }

    input_file = strdup(argv[optind]);
}

typedef struct file_description_tag
{
    FILE* file;
    char name[512];
    int line;
} file_description_t;

static void conditional_process(char* input_filename, char* output_filename)
{
    regex_t if_regex;
    regex_t ifnot_regex;
    regex_t endif_regex;
    regex_t include_regex;
    regex_t define_regex;
    regex_t undefine_regex;

    if (regcomp(&if_regex, "^[[:blank:]]*/[*]!if[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling if regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&ifnot_regex, "^[[:blank:]]*/[*]!ifnot[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling if not regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&endif_regex, "^[[:blank:]]*/[*]!endif[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NOSUB | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling endif regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&include_regex, "^[[:blank:]]*/[*]!include[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
                REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling include regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&define_regex, "^[[:blank:]]*/[*]!define[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
                REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling define regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&undefine_regex, "^[[:blank:]]*/[*]!undefine[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
                REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling undefine regular expression\n");
        exit(EXIT_FAILURE);
    }

    FILE* input = NULL;
    if (strcmp(input_filename, "-") == 0)
    {
        input = stdin;
    }
    else
    {
        input = fopen(input_filename, "r");
    }

    if (input == NULL)
    {
        fprintf(stderr, "Could not open input file '%s': %s\n",
                input_filename, strerror(errno));
        exit(EXIT_FAILURE);
    }

    FILE* output = NULL;
    if (strcmp(output_filename, "-") == 0)
    {
        output = stdout;
    }
    else
    {
        output = fopen(output_filename, "w");
    }

    if (output == NULL)
    {
        fprintf(stderr, "Could not open output file '%s': %s\n",
                output_filename, strerror(errno));
        exit(EXIT_FAILURE);
    }


    char buffer[MAX_TEXT_LINE];

    int block_nesting = 0;
    char output_enable_stack[MAX_IF_NESTING] = { 0 };
    output_enable_stack[block_nesting] = 1;

    file_description_t input_stack[MAX_INCLUDE_NESTING];
    memset(input_stack, 0, sizeof(input_stack));

    int top_input_stack = 0;

    input_stack[top_input_stack].file = input;
    strncpy(input_stack[top_input_stack].name, input_filename, 511);
    input_stack[top_input_stack].line = 0;

    while (top_input_stack >= 0)
    {
        while (fgets(buffer, MAX_TEXT_LINE, input_stack[top_input_stack].file) != NULL)
        {
            input_stack[top_input_stack].line++;
            regmatch_t offsets[2];

            if (output_enable_stack[block_nesting]
                    && (regexec(&include_regex, buffer, 2, offsets, 0) == 0))
            {
                char include_name[MAX_TEXT_LINE] = { 0 };

                int start = offsets[1].rm_so;

                int length = offsets[1].rm_eo - offsets[1].rm_so;
                length = (length > (MAX_TEXT_LINE-1)) ? (MAX_TEXT_LINE - 1) : length;

                strncpy(include_name, &(buffer[start]), length);

                if (top_input_stack == (MAX_INCLUDE_NESTING-1))
                {
                    fprintf(stderr, "Too much include nesting\n");
                    exit(EXIT_FAILURE);
                }
                FILE* new_input; 

                char full_path[512];
                int current_include_dir = 0;
                do 
                {
                    memset(full_path, 0, sizeof(full_path));
                    snprintf(full_path, 511, "%s/%s", include_dirs[current_include_dir], include_name);
                    new_input = fopen(full_path, "r");
                    current_include_dir++;
                } while ((new_input == NULL) && (current_include_dir < num_include_dirs));

                if (new_input == NULL)
                {
                    fprintf(stderr, "Could not open included file '%s': %s\n", include_name, strerror(errno));
                    exit(EXIT_FAILURE);
                }

                top_input_stack++;
                input_stack[top_input_stack].file = new_input;
                strncpy(input_stack[top_input_stack].name, full_path, 511);
                input_stack[top_input_stack].line = 0;

                if (enable_line_markers)
                {
                    fprintf(output, "#line 1 \"%s\"\n", input_stack[top_input_stack].name);
                }
                else
                {
                    fprintf(output, "\n");
                }
            }
            else if (output_enable_stack[block_nesting]
                    && (regexec(&define_regex, buffer, 2, offsets, 0) == 0))
            {
                char define_name[MAX_TEXT_LINE] = { 0 };

                int start = offsets[1].rm_so;

                int length = offsets[1].rm_eo - offsets[1].rm_so;
                length = (length > (MAX_TEXT_LINE - 1)) ? (MAX_TEXT_LINE - 1) : length;

                strncpy(define_name, &(buffer[start]), length);

                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                    }
                }

                // If not found register as a new define
                if (!found)
                {
                    if (num_defines == MAX_DEFINES)
                    {
                        fprintf(stderr, "Too many defines\n");
                        exit(EXIT_FAILURE);
                    }
                    defines[num_defines] = strdup(define_name);
                    num_defines++;
                }

                fprintf(output, "\n");
            }
            else if (output_enable_stack[block_nesting]
                    && (regexec(&undefine_regex, buffer, 2, offsets, 0) == 0))
            {
                char define_name[MAX_TEXT_LINE] = { 0 };

                int start = offsets[1].rm_so;

                int length = offsets[1].rm_eo - offsets[1].rm_so;
                length = (length > (MAX_TEXT_LINE - 1)) ? (MAX_TEXT_LINE - 1) : length;

                strncpy(define_name, &(buffer[start]), length);

                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                        break;
                    }
                }

                // If found unregister it as a define
                if (found)
                {
                    free(defines[j]);
                    int k;
                    for (k = j; k < num_defines-1; k++)
                    {
                        defines[k] = defines[k+1];
                    }
                    num_defines--;
                }

                fprintf(output, "\n");
            }
            else if (regexec(&if_regex, buffer, 2, offsets, 0) == 0)
            {
                block_nesting++;
                char define_name[MAX_TEXT_LINE] = { 0 };

                int start = offsets[1].rm_so;

                int length = offsets[1].rm_eo - offsets[1].rm_so;
                length = (length > (MAX_TEXT_LINE - 1)) ? (MAX_TEXT_LINE - 1) : length;

                strncpy(define_name, &(buffer[start]), length);

                // If output is enabled, then we have to ensure this macro is defined
                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                    }
                }

                output_enable_stack[block_nesting] = found && output_enable_stack[block_nesting - 1];

                fprintf(output, "\n");
            }
            else if (regexec(&ifnot_regex, buffer, 2, offsets, 0) == 0)
            {
                block_nesting++;
                char define_name[MAX_TEXT_LINE] = { 0 };

                int start = offsets[1].rm_so;

                int length = offsets[1].rm_eo - offsets[1].rm_so;
                length = (length > (MAX_TEXT_LINE - 1)) ? (MAX_TEXT_LINE - 1) : length;

                strncpy(define_name, &(buffer[start]), length);

                // If output is enabled, then we have to ensure this macro is undefined
                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                    }
                }

                output_enable_stack[block_nesting] = !found && output_enable_stack[block_nesting - 1];

                fprintf(output, "\n");
            }
            else if (regexec(&endif_regex, buffer, 0, NULL, 0) == 0)
            {
                block_nesting--;
                if (block_nesting < 0)
                {
                    fprintf(stderr, "Too many !endif\n");
                    exit(EXIT_FAILURE);
                }
                fprintf(output, "\n");
            }
            else if (output_enable_stack[block_nesting])
            {
                fprintf(output, "%s", buffer);
            }
            else if (!output_enable_stack[block_nesting])
            {
                fprintf(output, "\n");
            }
        }

        fclose(input_stack[top_input_stack].file);
        top_input_stack--;

        if (top_input_stack >= 0
                && enable_line_markers)
        {
            fprintf(output, "#line %d \"%s\"\n", 
                    input_stack[top_input_stack].line + 1,
                    input_stack[top_input_stack].name);
        }
    }

    if (block_nesting > 0)
    {
        fprintf(stderr, "Unpaired number of !if/!endif (%d)\n", block_nesting);
        exit(EXIT_FAILURE);
    }

    fclose(output);
}

int main(int argc, char* argv[])
{
    fprintf(stderr, "tpp - a tiny preprocessor for " PACKAGE " " VERSION "\n");
    parse_arguments(argc, argv);
    conditional_process(input_file, output_file);
    return 0;
}
