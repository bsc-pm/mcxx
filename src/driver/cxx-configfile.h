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




#ifndef CXX_CONFIGFILE
#define CXX_CONFIGFILE

#include "cxx-driver-decls.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

char config_file_parse(const char *filename);

char flag_expr_eval(flag_expr_t* flag_expr);

typedef int (option_function_t)(struct compilation_configuration_tag*, const char* index, const char* value);

option_function_t config_set_language;
option_function_t config_set_options;
option_function_t config_set_preprocessor_name;
option_function_t config_set_preprocessor_options;
option_function_t config_set_preprocessor_uses_stdout;
option_function_t config_set_prescanner_options;
option_function_t config_set_compiler_name;
option_function_t config_set_compiler_options;
option_function_t config_set_linker_name;
option_function_t config_set_linker_options_pre;
option_function_t config_set_linker_options_post;
option_function_t config_set_linker_options;
option_function_t config_add_compiler_phase;
option_function_t config_add_preprocessor_prefix;
option_function_t config_set_environment;
#if 0
option_function_t config_set_embedder;
option_function_t config_set_identifier;
#endif
option_function_t config_set_target_options;
option_function_t config_set_compiler_dto;
option_function_t config_set_codegen_phase;

option_function_t config_set_fortran_array_descriptor;
option_function_t config_set_fortran_preprocessor_name;
option_function_t config_set_fortran_preprocessor_options;

option_function_t config_set_target_objcopy;
option_function_t config_set_target_objdump;
option_function_t config_set_target_ar;

option_function_t config_set_error_message;

void print_help_target_options(void);

MCXX_END_DECLS

#endif // CXX_CONFIGFILE
