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

#include "uniquestr.h"
#include "cxx-configfile.h"
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-driver-utils.h"
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include <stdio.h>
#include <string.h>

// Returns 0, 1 or -1 if an error
static void parse_boolean(const char *c, int *value)
{
    char * valid_trues[] = { "1", "yes", "true", NULL };
    char * valid_falses[] = { "0", "no", "false", NULL };

    *value = -1;

    // Maybe is true
    int i = 0;
    while (valid_trues[i] != NULL)
    {
        if (strcasecmp(c, valid_trues[i]) == 0)
        {
            *value = 1;
            return;
        }
        i++;
    }
    
    // Maybe is false
    i = 0;
    while (valid_falses[i] != NULL)
    {
        if (strcasecmp(c, valid_falses[i]) == 0)
        {
            *value = 0;
            return;
        }
        i++;
    }
}

// Set source language
int config_set_language(struct compilation_configuration_tag* config, const char* value)
{
    if (strcasecmp(value, "c") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_C;
    }
    else if (strcasecmp(value, "c++") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_CXX;
    }
    else
    {
        fprintf(stderr, "Unknown language '%s' assuming C++\n", value);
        config->source_language = SOURCE_LANGUAGE_CXX;
    }
    return 0;
}

// Set additional mcxx options
int config_set_options(struct compilation_configuration_tag* config, const char* value)
{
    int num;
    const char** blank_separated_options = blank_separate_values(value, &num);

    num++;
    const char** real_options = calloc(num, sizeof(*real_options));

    int i;
    for (i = 1; i < num; i++)
    {
        real_options[i] = blank_separated_options[i - 1];
    }

    // Change the current configuration otherwise we will handle the parameters
    // in the wrong profile
    struct compilation_configuration_tag* previous = CURRENT_CONFIGURATION;
    SET_CURRENT_CONFIGURATION(config);

    real_options[0] = uniquestr("mcxx");

    parse_arguments(num, real_options, /* from_command_line= */ 0, /* parse_implicits_only*/ 0);

    // Restore the original one
    SET_CURRENT_CONFIGURATION(previous);

    return 0;
}

// Set preprocessor name
int config_set_preprocessor_name(struct compilation_configuration_tag* config, const char* value)
{
    config->preprocessor_name = uniquestr(value);
    return 0;
}

// Set preprocessor options
int config_set_preprocessor_options(struct compilation_configuration_tag* config, const char* value)
{
    int num;
    const char** blank_separated_options = blank_separate_values(value, &num);

    add_to_parameter_list(&config->preprocessor_options, blank_separated_options, num);

    return 0;
}

int config_set_preprocessor_uses_stdout(struct compilation_configuration_tag * config, const char *value)
{
    int bool_value = -1;

    parse_boolean(value, &bool_value);

    if (bool_value == -1)
    {
        fprintf(stderr, "Warning: value given for 'preprocessor_uses_stdout' is not a valid boolean value. Skipping\n");
    }
    else
    {
        config->preprocessor_uses_stdout = bool_value;
    }

    return 0;
}

// Set native compiler name
int config_set_compiler_name(struct compilation_configuration_tag* config, const char* value)
{
    config->native_compiler_name = uniquestr(value);
    return 0;
}

// Set native compiler options
int config_set_compiler_options(struct compilation_configuration_tag* config, const char* value)
{
    int num;
    const char **blank_separated_options = blank_separate_values(value, &num);

    add_to_parameter_list(&config->native_compiler_options, blank_separated_options, num);
    return 0;
}

// Set linker name
int config_set_linker_name(struct compilation_configuration_tag* config, const char* value)
{
    config->linker_name = uniquestr(value);
    return 0;
}

// Set linker options
int config_set_linker_options(struct compilation_configuration_tag* config, const char* value)
{
    int num;
    const char **blank_separated_options = blank_separate_values(value, &num);

    add_to_parameter_list(&config->linker_options, blank_separated_options, num);
    return 0;
}

int config_add_compiler_phase(struct compilation_configuration_tag* config, const char* value)
{
    const char* library_name = uniquestr(value);
    P_LIST_ADD(config->compiler_phases, 
            config->num_compiler_phases, 
            library_name);

    return 0;
}

int config_add_preprocessor_prefix(struct compilation_configuration_tag* config, const char* value)
{
    const char *reserved[] = {
        "gcc", 
        "mcc",
        "mcxx",
        /* sentinel */ NULL
    };
    
    int i;
    for (i = 0; reserved[i] != NULL; i++)
    {
        if (strcasecmp(value, reserved[i]) == 0)
        {
            fprintf(stderr, "%s is a reserved pragma prefix\n", reserved[i]);
            return 1;
        }
    }
    
    // Reuse P_LIST_ADD
    int num_prefixes = config->num_pragma_custom_prefix;

    P_LIST_ADD(config->pragma_custom_prefix,
            config->num_pragma_custom_prefix,
            uniquestr(value));

    // Allocate pragma directive info
    pragma_directive_set_t* new_info = calloc(1, sizeof(*new_info));

    P_LIST_ADD(config->pragma_custom_prefix_info,
            num_prefixes, new_info);

    return 0;
}

int config_set_environment(struct compilation_configuration_tag* config, const char* value)
{
    type_environment_t* chosen_env = get_environment(value);
    config->type_environment = chosen_env;
    return 0;
}

char config_file_parse(const char *filename)
{
    if (open_configuration_file_for_scan(filename))
    {
        int n = configfileparse();
        close_configuration_file_for_scan();

        if (n != 0)
        {
            fprintf(stderr, "warning: parsing of configuration file '%s' failed\n", filename);
        }
        return (n == 0);
    }
    return 0;
}
