#ifndef CXX_PARAMETERS_H
#define CXX_PARAMETERS_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

enum command_line_parameters_flag_t
{
    // Error value
    CLP_INVALID = 0,
    // This option does not have an argument
    CLP_NO_ARGUMENT,
    // This option has a mandatory argument
    CLP_REQUIRED_ARGUMENT,
    // This option have an optional
    CLP_OPTIONAL_ARGUMENT,
    // This is not an option, just a plain parameter
    CLP_PLAIN_PARAMETER
};


struct command_line_parameter_t
{
    enum command_line_parameters_flag_t flag;
    int value;
    char *argument;
};

struct command_line_long_options
{
    char *option_name;
    enum command_line_parameters_flag_t flag;
    int value;
};

char command_line_get_next_parameter(
        int *index, 
        struct command_line_parameter_t *parameter_info,
        char *short_options_spec,
        struct command_line_long_options *long_options, 
        int argc, char *argv[]
        );

MCXX_END_DECLS

#endif // CXX_PARAMETERS_H
