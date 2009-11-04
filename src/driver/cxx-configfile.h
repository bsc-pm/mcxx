/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_CONFIGFILE
#define CXX_CONFIGFILE

#include "cxx-driver-decls.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

char config_file_parse(const char *filename);

typedef int (option_function_t)(struct compilation_configuration_tag*, const char* value);

option_function_t config_set_language;
option_function_t config_set_options;
option_function_t config_set_preprocessor_name;
option_function_t config_set_preprocessor_options;
option_function_t config_set_preprocessor_uses_stdout;
option_function_t config_set_compiler_name;
option_function_t config_set_compiler_options;
option_function_t config_set_linker_name;
option_function_t config_set_linker_options;
option_function_t config_add_compiler_phase;
option_function_t config_add_preprocessor_prefix;
option_function_t config_set_environment;

MCXX_END_DECLS

#endif // CXX_CONFIGFILE
