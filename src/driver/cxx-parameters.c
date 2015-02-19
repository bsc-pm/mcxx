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




#include "cxx-parameters.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static 
struct command_line_parameter_t invalid_parameter = {
    .flag = CLP_INVALID,
    .value = 0,
    .argument = (void*)0
};

static
char starts_short_option(const char* str)
{
    return (str[0] == '-'
            && str[1] != '-'
            && str[1] != '\0');
}

static
char starts_long_option(const char* str)
{
    return (str[0] == '-'
            && str[1] == '-'
            && str[2] != '\n');
}

static
char starts_option(const char *str)
{
    return starts_short_option(str)
        || starts_long_option(str);
}

static struct command_line_parameter_t parse_short_option(int *current_index, 
       const char *short_options_spec, int argc, const char *argv[])
{
    /*
     * Precondition: starts_short_option(argv[*current_index]) is true
     */
    const char *current_arg = argv[*current_index];
    char option = current_arg[1];

    struct command_line_parameter_t result = invalid_parameter;

    int i;
    int length = strlen(short_options_spec);
    for (i = 0; i < length; i++)
    {
        if (short_options_spec[i] == option)
        {
            if (short_options_spec[i+1] == ':')
            {
                if (short_options_spec[i + 2] == ':')
                {
                    result.flag = CLP_OPTIONAL_ARGUMENT;
                    // Jump this second ':'
                    i++;
                }
                else
                {
                    result.flag = CLP_REQUIRED_ARGUMENT;
                }
                // Jump this ':'
                i++;
            }
            else
            {
                result.flag = CLP_NO_ARGUMENT;
            }
            result.value = option;

            break;
        }
        else
        {
            if (short_options_spec[i+1] == ':')
            {
                if (short_options_spec[i + 2] == ':')
                {
                    // Jump this second ':'
                    i++;
                }
                // Jump this ':'
                i++;
            }
        }
    }

    if ((result.flag == CLP_REQUIRED_ARGUMENT)
            || (result.flag == CLP_OPTIONAL_ARGUMENT))
    {
        // Since this is a short flag argument is simply the remainder of the string
        // or the next parameter

        if (current_arg[2] == '\0')
        {
            // There are still parameters
            if ((((*current_index) + 1) < argc)
                    // And does not start one
                    && !(starts_option(argv[(*current_index) + 1])))
            {
                result.argument = argv[(*current_index) + 1];

                // Advance the parameter
                (*current_index)++;
            }
            else
            {
                if (result.flag == CLP_REQUIRED_ARGUMENT)
                {
                    fprintf(stderr, "warning: option '%s' requires an argument\n",
                            current_arg);
                }
            }
        }
        else
        {
            result.argument = &current_arg[2];
        }
    }

    // Only advance the index if things were fine, otherwise let the user choose
    if (result.flag != CLP_INVALID)
        (*current_index)++;

    return result;
}

static struct command_line_parameter_t parse_long_option(int *current_index, 
        struct command_line_long_options *long_options, 
        int argc, const char *argv[])
{
    /*
     * Precondition: starts_long_option(argv[*current_index]) is true
     */

    const char *current_arg = argv[*current_index];

    // Get the option name
    // current_arg[0] == '-'
    // current_arg[1] == '-'
    // current_arg[2] != '\0'
    current_arg = &current_arg[2];

    const char *p = current_arg;
    unsigned int length_option_name = 0;

    while (((*p) != '\0')
            && ((*p) != '='))
    {
        length_option_name++;
        p++;
    }

    // Now check
    char already_matched = 0;

    struct command_line_long_options *current_long_option = long_options;

    struct command_line_parameter_t result = invalid_parameter;

    while (current_long_option->option_name != NULL)
    {
        if ((strncmp(current_arg, current_long_option->option_name, length_option_name) == 0)
                && (strlen(current_long_option->option_name) == length_option_name))
        {
            if (!already_matched)
            {
                result.value = current_long_option->value;
                result.flag = current_long_option->flag;

                if (result.flag == CLP_OPTIONAL_ARGUMENT
                        || result.flag == CLP_REQUIRED_ARGUMENT)
                {
                    // Note 'p' is still meaningful here
                    const char *argument = NULL;
                    if (*p == '=')
                    {
                        argument = (p + 1);
                    }
                    else 
                    {
                        if (((*current_index) + 1) < argc)
                        {
                            argument = argv[(*current_index) + 1];
                            // Do not allow the argument to start an option
                            if (!starts_short_option(argument)
                                    && !starts_long_option(argument))
                            {
                                // Advance this index
                                (*current_index)++;
                            }
                            else
                            {
                                argument = NULL;
                            }
                        }
                    }

                    if (argument == NULL
                            && result.flag == CLP_REQUIRED_ARGUMENT)
                    {
                        char c[255];
                        strncpy(c, current_arg, length_option_name);
                        c[length_option_name] = '\0';

                        fprintf(stderr, "warning: option '%s' requires an argument\n", c);
                    }
                    else
                    {
                        result.argument = argument;
                    }
                }

                already_matched = 1;
            }
            else
            {
                result = invalid_parameter;
                break;
            }
        }

        current_long_option++;
    }

    if (result.flag != CLP_INVALID)
        (*current_index)++;

    return result;
}


char command_line_get_next_parameter(
        int *index, 
        struct command_line_parameter_t *parameter_info,
        const char *short_options_spec,
        struct command_line_long_options *long_options, 
        int argc, const char *argv[]
        )
{
    if (*index >= argc)
        return 0;

    if (starts_short_option(argv[*index]))
    {
        *parameter_info = parse_short_option(index, short_options_spec, argc, argv);
    }
    else if (starts_long_option(argv[*index]))
    {
        *parameter_info = parse_long_option(index, long_options, argc, argv);
    }
    else
    {
        memset(parameter_info, 0, sizeof(*parameter_info));
        parameter_info->flag = CLP_PLAIN_PARAMETER;
        parameter_info->argument = argv[*index];
        // Advance the index
        (*index)++;
    }
    
    return 1;
}
