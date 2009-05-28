#include "cxx-process.h"
#include "cxx-profile.h"
#include <string.h>
#include <stdlib.h>
#include "uniquestr.h"

#include <stdio.h>

compilation_configuration_t* new_compilation_configuration(
        const char* name,
        compilation_configuration_t* base)
{
    compilation_configuration_t* result = calloc(1, sizeof(*result));

    result->configuration_name = uniquestr(name);
    result->base_configuration = base;

    return result;
}

static void initialize_with_base_config(compilation_configuration_t* dst, 
        compilation_configuration_t const* base);

void initialize_with_base(compilation_configuration_t* config)
{
    if (config->base_configuration != NULL)
    {
        initialize_with_base_config(config, config->base_configuration);
    }
}


static const char** copy_null_ended_const_char_array(const char** orig)
{
    const char** result = NULL;
    int num = 0;

    while (*orig != NULL)
    {
        result = realloc(result, sizeof(char*)*(num + 1));
        result[num] = *orig;

        num++;
        orig++;
    }

    return result;
}

#define DEF_COPY_ARRAY(_fun_name, element_type) \
static element_type** _fun_name(element_type** orig, int num_elems) \
{ \
    element_type** result = calloc(num_elems, sizeof(*result)); \
    int i; \
    for (i = 0; i < num_elems; i++) \
    { \
        result[i] = orig[i]; \
    } \
    return result; \
}

DEF_COPY_ARRAY(copy_const_char_array, const char)
DEF_COPY_ARRAY(copy_external_vars, external_var_t)
DEF_COPY_ARRAY(copy_pragma_directive_set, pragma_directive_set_t)

static void initialize_with_base_config(compilation_configuration_t* dst, 
        compilation_configuration_t const * base)
{
    // Bitwise-copy for all fields that can be safely bitwise-copied
    // but keep some that should not be overwritten
    const char* configuration_name = dst->configuration_name;
    int num_configuration_lines = dst->num_configuration_lines;
    struct compilation_configuration_line ** configuration_lines = dst->configuration_lines;

    // Bitwise copy
    *dst = *base;

    // Get back fields that should not be overwritten
    dst->configuration_name = configuration_name;
    dst->num_configuration_lines = num_configuration_lines;
    dst->configuration_lines = configuration_lines;

    // Copy those fields requiring special copies
    dst->preprocessor_options = copy_null_ended_const_char_array(base->preprocessor_options);
    dst->native_compiler_options = copy_null_ended_const_char_array(base->native_compiler_options);
    dst->linker_options = copy_null_ended_const_char_array(base->linker_options);

    // Note, fields with the length of the arrays have been copied in the
    // bitwise copy so there is no need to set them again
    dst->compiler_phases = copy_const_char_array(base->compiler_phases, 
            base->num_compiler_phases);
    dst->external_vars = copy_external_vars(base->external_vars, 
            base->num_external_vars);
    dst->pragma_custom_prefix_info = copy_pragma_directive_set(base->pragma_custom_prefix_info, 
            base->num_pragma_custom_prefix);
}

compilation_configuration_t* get_compilation_configuration(const char* config_name)
{
    // Check repeated configurations
    int i;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        if (strcmp(compilation_process.configuration_set[i]->configuration_name, config_name) == 0)
        {
            return compilation_process.configuration_set[i];
        }
    }
    return NULL;
}
