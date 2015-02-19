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
    const char *argument;
};

struct command_line_long_options
{
    const char *option_name;
    enum command_line_parameters_flag_t flag;
    int value;
};

char command_line_get_next_parameter(
        int *index, 
        struct command_line_parameter_t *parameter_info,
        const char *short_options_spec,
        struct command_line_long_options *long_options, 
        int argc, const char *argv[]
        );

MCXX_END_DECLS

#endif // CXX_PARAMETERS_H
