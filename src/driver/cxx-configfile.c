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




#include "uniquestr.h"
#include "string_utils.h"

#include "cxx-configfile.h"
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-driver-utils.h"
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include "cxx-compilerphases.hpp"
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
int config_set_language(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    if (strcasecmp(value, "c") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_C;
    }
    else if (strcasecmp(value, "c++") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_CXX;
    }
    else if (strcasecmp(value, "c++11") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_CXX;
        config->enable_cxx11 = 1;
    }
    else if (strcasecmp(value, "fortran") == 0)
    {
        config->source_language = SOURCE_LANGUAGE_FORTRAN;
    }
    else
    {
        fprintf(stderr, "Unknown language '%s' assuming C++\n", value);
        config->source_language = SOURCE_LANGUAGE_CXX;
    }
    return 0;
}


//! This function expands the profile environment variables '${VAR_NAME}'
const char* expand_profile_environment_variables(const char* value)
{
    const char *expanded_value = value;
    if (value)
    {
        strbuilder_t* str_builder = strbuilder_new();

        const char *next_chunk = value;
        char *next_environment_variable;

        char found = 0;

        // ${VAR_NAME} ...
        while((next_environment_variable = strchr(next_chunk,'$')) != 0)
        {
            found = 1;

            // We are modifying the original char *, so at some point we will have
            // to restore the original value!
            *next_environment_variable = '\0';

            strbuilder_append(str_builder, next_chunk);

            // Restoring original value
            *next_environment_variable = '$';

            // {
            char* opening = next_environment_variable + 1;
            if (opening == 0 || *opening != '{')
                fprintf(stderr, "Error reading an environment variable, expecting '{'\n");

            // VAR_NAME
            next_chunk = opening + 1;

            // }
            char *closing = strchr(next_chunk, '}');
            if (closing == 0 || *closing != '}')
                fprintf(stderr, "Error reading an environment variable, expecting '}'\n");

            // We are modifying the original char *, so at some point we will have
            // to restore the original value!
            *closing = '\0';

            char *env_variable = getenv(next_chunk);
            if(env_variable == 0)
                fprintf(stderr, "warning: undefined environment variable '%s'\n", next_chunk);
            else
                strbuilder_append(str_builder, env_variable);

            // Restoring original value
            *closing = '}';

            next_chunk = closing + 1;
        }

        // If there are no environment variables we should return the original value
        if (found)
        {
            if (next_chunk != 0)
                strbuilder_append(str_builder, next_chunk);

            expanded_value = uniquestr(strbuilder_str(str_builder));
        }

        strbuilder_free(str_builder);
    }
    return expanded_value;
}

// Set additional mcxx options
int config_set_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char** blank_separated_options = blank_separate_values(value, &num);

    num++;
    const char** real_options = NEW_VEC0(const char*, num);

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
int config_set_preprocessor_name(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    config->preprocessor_name = uniquestr(value);
    return 0;
}

// Set preprocessor options
int config_set_preprocessor_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->preprocessor_options, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

int config_set_fortran_preprocessor_name(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    config->fortran_preprocessor_name = uniquestr(value);
    return 0;
}

int config_set_fortran_preprocessor_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->fortran_preprocessor_options, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

int config_set_preprocessor_uses_stdout(struct compilation_configuration_tag * config, const char* index, const char *value)
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

int config_set_prescanner_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char** blank_separated_options = blank_separate_values(value, &num);

    add_to_parameter_list(&config->prescanner_options, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

// Set native compiler name
int config_set_compiler_name(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    config->native_compiler_name = uniquestr(value);
    return 0;
}

// Set native compiler options
int config_set_compiler_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->native_compiler_options, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

// Set linker name
int config_set_linker_name(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    config->linker_name = uniquestr(value);
    return 0;
}

// Set linker options
int config_set_linker_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->linker_options, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

// Set linker options pre
int config_set_linker_options_pre(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->linker_options_pre, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

// Set linker options post
int config_set_linker_options_post(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    int num;
    const char *expanded_value = expand_profile_environment_variables(value);
    const char **blank_separated_options = blank_separate_values(expanded_value, &num);

    add_to_parameter_list(&config->linker_options_post, blank_separated_options, num);
    DELETE(blank_separated_options);

    return 0;
}

int config_add_compiler_phase(struct compilation_configuration_tag* config, const char* index, const char* value)
{
	compiler_phase_loader_t* cl = NEW0(compiler_phase_loader_t);
	cl->func = compiler_regular_phase_loader;
	cl->data = (void*)uniquestr(value);

    P_LIST_ADD(config->phase_loader, 
            config->num_compiler_phases,
			cl);

    return 0;
}

int config_add_preprocessor_prefix(struct compilation_configuration_tag* config, const char* index, const char* value)
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
    for (i = 0; i < num_prefixes; i++)
    {
        if (strcasecmp(value, config->pragma_custom_prefix[i]) == 0)
        {
            // Already registered, ignore
            return 0;
        }
    }

    P_LIST_ADD(config->pragma_custom_prefix,
            config->num_pragma_custom_prefix,
            uniquestr(value));

    // Allocate pragma directive info
    pragma_directive_set_t* new_info = NEW0(pragma_directive_set_t);

    P_LIST_ADD(config->pragma_custom_prefix_info,
            num_prefixes, new_info);

    return 0;
}

int config_set_environment(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    type_environment_t* chosen_env = get_environment(value);
    if (chosen_env != NULL)
    {
        config->type_environment = chosen_env;
    }
    return 0;
}

int config_set_fortran_array_descriptor(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    fortran_array_descriptor_t* chosen_array_descriptor = get_fortran_array_descriptor(value);
    if (chosen_array_descriptor != NULL)
    {
        config->fortran_array_descriptor = chosen_array_descriptor;
    }
    return 0;
}

static void enable_sublink(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_sublink = 1;
}

static void disable_sublink(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_sublink = 0;
}

static void combine_spuelf(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_combining = 1;
    options->combining_mode = COMBINING_MODE_SPU_ELF;
}

static void combine_incbin(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_combining = 1;
    options->combining_mode = COMBINING_MODE_INCBIN;
}

static void disable_combine(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_combining = 0;
}

static void disable_embed(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_embedding = 0;
}

static void embed_bfd(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_embedding = 1;
    options->embedding_mode = EMBEDDING_MODE_BFD;
}

static void embed_partial_linking(
        target_options_map_t* options,
        const char** opts UNUSED_PARAMETER, int *i UNUSED_PARAMETER)
{
    options->do_embedding = 1;
    options->embedding_mode = EMBEDDING_MODE_PARTIAL_LINKING;
}

struct target_options_t
{
    const char* target_opt_name;
    void (*p)(target_options_map_t* options, const char** opts, int *i);
    const char* desc;
};

static 
struct target_options_t available_target_options[] =
{
    // Sublink
    { "no_sublink", disable_sublink, "Disable sublinking when linking files" },
    { "sublink",    enable_sublink , "Enable sublinking when linking files" },
    // Combine
    { "no_combine",      disable_combine, "Disables combination of sublinking output into link output" },
    { "combine:spu_elf", combine_spuelf,  "Enables combination of sublinking output using ppu-spuembed" },
    { "combine:incbin", combine_incbin, "Enables combination of sublinking output using GNU as .incbin" },
    // Embedder
    { "no_embed",  disable_embed, "Disables embedding secondary object outputs into the main object output" },
    { "embed:bfd", embed_bfd,     "Enables embedding secondary objects outputs into the main object output using BFD tools" },
    { "embed:partial_linking", embed_partial_linking, "Enables embedding secondary objects outputs into the main object output using partial linking."
                                                      "Note that this only works for objects of the same architecture"},
    { NULL, NULL, NULL }
};

#define TARGET_OPTIONS_MESSAGE \
"Help for 'target_options' option in configuration file\n" \
"\n" \
" Syntax: target_options[target-profile] = option1 [option2 ...]\n" \
"\n" \
"  target-profile must be a valid profile registered by the compiler.\n" \
"  When an instance of execution of target-profile creates secondary\n" \
"  objects, the target_options of the secondary files profile is used\n" \
"\n" \
" Valid options are:\n" \
"\n"

void print_help_target_options(void)
{
    fprintf(stdout, "%s", TARGET_OPTIONS_MESSAGE);

    int i;
    for (i = 0; available_target_options[i].target_opt_name != NULL; i++)
    {
        fprintf(stdout, " %s\n", available_target_options[i].target_opt_name);
        fprintf(stdout, " %s\n\n", available_target_options[i].desc);
    }
}

static void parse_target_options(target_options_map_t* target_options, const char* value)
{
    int num;
    const char **blank_separated_options = blank_separate_values(value, &num);

    int i = 0;
    for (i = 0; i < num; i++)
    {
        int j = 0;
        for (j = 0; available_target_options[j].target_opt_name != NULL; j++)
        {
            if (strcmp(available_target_options[j].target_opt_name, blank_separated_options[i]) == 0)
            {
                (available_target_options[j].p)(target_options, blank_separated_options, &i);
                break;
            }
        }
    }
}

// target_options_map_t* get_target_options(compilation_configuration_t* configuration, 
//         const char* configuration_name)
int config_set_target_options(struct compilation_configuration_tag* config, const char* index, const char* value)
{
    target_options_map_t* target_options = get_target_options(config, index);

    if (target_options == NULL)
    {
        target_options = NEW0(target_options_map_t);
        target_options->profile = index;

        P_LIST_ADD(config->target_options_maps, config->num_target_option_maps,
                target_options);
    }

    parse_target_options(target_options, value);

    return 0;
}

int config_set_compiler_dto(struct compilation_configuration_tag* config, const char* index, const char* value)
{
	compiler_phase_loader_t* cl = NEW0(compiler_phase_loader_t);
	cl->func = compiler_special_phase_set_dto;
	cl->data = (void*)uniquestr(value);

    P_LIST_ADD(config->phase_loader, 
            config->num_compiler_phases,
			cl);

    return 0;
}

int config_set_codegen_phase(compilation_configuration_t* config, const char* index, const char* value)
{
	compiler_phase_loader_t* cl = NEW0(compiler_phase_loader_t);
	cl->func = compiler_special_phase_set_codegen;
	cl->data = (void*)uniquestr(value);

    P_LIST_ADD(config->phase_loader, 
            config->num_compiler_phases,
			cl);

    return 0;
}

int config_set_target_objcopy(compilation_configuration_t* config, const char* index UNUSED_PARAMETER, const char* value)
{
    config->target_objcopy = value;
    return 0;
}

int config_set_target_objdump(compilation_configuration_t* config, const char* index UNUSED_PARAMETER, const char* value)
{
    config->target_objdump = value;
    return 0;
}

int config_set_target_ar(compilation_configuration_t* config, const char* index UNUSED_PARAMETER, const char* value)
{
    config->target_ar = value;
    return 0;
}

int config_set_error_message(compilation_configuration_t* config, const char* index UNUSED_PARAMETER, const char* value)
{
    P_LIST_ADD(config->error_messages, config->num_errors, uniquestr(value));
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
