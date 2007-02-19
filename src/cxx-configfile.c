#include "cxx-configfile.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include <stdio.h>
#include <string.h>

// Set source language
int config_set_language(char* value)
{
    if (strcasecmp(value, "c") == 0)
    {
        compilation_options.source_language = SOURCE_LANGUAGE_C;
    }
    else if (strcasecmp(value, "c++") == 0)
    {
        compilation_options.source_language = SOURCE_LANGUAGE_CXX;
    }
    else
    {
        fprintf(stderr, "Unknown language '%s' assuming C++\n", value);
        compilation_options.source_language = SOURCE_LANGUAGE_CXX;
    }
    return 0;
}

// Set additional mcxx options
int config_set_options(char* value)
{
    int num;
    char** comma_options = blank_separate_values(value, &num);

    parse_arguments(num, comma_options, /* from_command_line= */ 0);

    return 0;
}

// Set preprocessor name
int config_set_preprocessor_name(char* value)
{
    compilation_options.preprocessor_name = strdup(value);
    return 0;
}

// Set preprocessor options
int config_set_preprocessor_options(char* value)
{
    int num;
    compilation_options.preprocessor_options = blank_separate_values(value, &num);
    return 0;
}

// Set native compiler name
int config_set_compiler_name(char* value)
{
    compilation_options.native_compiler_name = strdup(value);
    return 0;
}

// Set native compiler options
int config_set_compiler_options(char* value)
{
    int num;
    compilation_options.native_compiler_options = blank_separate_values(value, &num);
    return 0;
}

// Set linker name
int config_set_linker_name(char* value)
{
    compilation_options.linker_name = strdup(value);
    return 0;
}

// Set linker options
int config_set_linker_options(char* value)
{
    int num;
    compilation_options.linker_options = blank_separate_values(value, &num);
    return 0;
}

int config_add_compiler_phase(char* value)
{
    char* library_name = strdup(value);
    P_LIST_ADD(compilation_options.compiler_phases, 
            compilation_options.num_compiler_phases, 
            library_name);

    return 0;
}

int config_add_preprocessor_prefix(char* value)
{
    if (strcasecmp(value, "gcc") == 0)
    {
        fprintf(stderr, "gcc is a reserved pragma prefix\n");
        return 1;
    }
    if (strcasecmp(value, "omp") == 0)
    {
        fprintf(stderr, "omp is a reserved pragma prefix\n");
        return 1;
    }

    P_LIST_ADD(compilation_options.pragma_custom_prefix,
            compilation_options.num_pragma_custom_prefix,
            strdup(value));

    return 0;
}
