/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_DRIVER_H
#define CXX_DRIVER_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-scopelink.h"
#include "cxx-driver-decls.h"
#include "cxx-macros.h"
#include "cxx-parameters.h"

MCXX_BEGIN_DECLS

extern compilation_process_t compilation_process;

extern int mcxx_flex_debug;
extern int mc99_flex_debug;

extern int yyparse(AST* parsed_tree);

struct extensions_table_t*
fileextensions_lookup (register const char *str, 
        register unsigned int len);

struct configuration_directive_t*
configoptions_lookup (register const char *str, 
        register unsigned int len);

int parse_arguments(int argc, const char* argv[], char from_command_line);

extern int mcxxdebug;
extern int mcxxparse(AST* a);

extern int mc99debug;
extern int mc99parse(AST* a);

struct debug_flags_list_t** list_of_debug_flags(void);

struct debug_flags_list_t *
debugflags_lookup (register const char *str, register unsigned int len);

void add_new_file_to_compilation_process(const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration);

pragma_directive_kind_t lookup_pragma_directive(const char* prefix, const char* directive);

void register_new_directive(const char* prefix, const char* directive, char is_construct);

// Internal between cxx-driver.c and cxx-configfile.c, do not use elsewhere
void add_to_parameter_list(const char*** existing_options, const char **parameters, int num_parameters);

MCXX_END_DECLS

#endif // CXX_DRIVER_H
