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

#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include "cxx-process.h"
#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-scopelink.h"
#include "cxx-driver-decls.h"
#include "cxx-macros.h"
#include "cxx-parameters.h"

MCXX_BEGIN_DECLS

struct extensions_table_t*
fileextensions_lookup (register const char *str, 
        register unsigned int len);

struct configuration_directive_t*
configoptions_lookup (register const char *str, 
        register unsigned int len);

int parse_arguments(int argc, const char* argv[], 
        char from_command_line, char parse_implicits_only);

struct debug_flags_list_t** list_of_debug_flags(void);

struct debug_flags_list_t *
debugflags_lookup (register const char *str, register unsigned int len);

// Internal between cxx-driver.c and cxx-configfile.c, do not use elsewhere
void add_to_parameter_list(const char*** existing_options, const char **parameters, int num_parameters);
type_environment_t* get_environment(const char* env_id);

MCXX_END_DECLS

#endif // CXX_DRIVER_H
