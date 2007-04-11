/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_DRIVER_DECLS_H
#define CXX_DRIVER_DECLS_H

#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-scopelink-decls.h"
#include <getopt.h>

MCXX_BEGIN_DECLS

// Options for command line arguments
typedef 
enum COMMAND_LINE_OPTIONS_TAG
{
    OPTION_UNDEFINED = 1024,
    OPTION_VERSION,
    OPTION_PREPROCESSOR_NAME,
    OPTION_NATIVE_COMPILER_NAME,
    OPTION_LINKER_NAME,
    OPTION_DEBUG_FLAG,
    OPTION_HELP_DEBUG_FLAGS,
    OPTION_OUTPUT_DIRECTORY,
    OPTION_NO_OPENMP,
    OPTION_EXTERNAL_VAR,
    OPTION_PROFILE
} COMMAND_LINE_OPTIONS;

// Kind of source 
typedef 
enum source_kind_tag
{
    SOURCE_KIND_UNKNOWN = 0,
    SOURCE_KIND_NOT_PREPROCESSED,
    SOURCE_KIND_PREPROCESSED
} source_kind_t;

typedef 
enum source_language_tag
{
    SOURCE_LANGUAGE_UNKNOWN = 0,
    SOURCE_LANGUAGE_C,
    SOURCE_LANGUAGE_CXX,
    SOURCE_LANGUAGE_LINKER_DATA
} source_language_t;

extern char* source_language_names[];

// File extensions table
struct extensions_table_t
{
    char* name;
    source_language_t source_language;
    source_kind_t source_kind;
};

// Valid command line parameters
extern struct option getopt_long_options[];


// Represents one translation unit
typedef 
struct translation_unit_tag
{
    char* input_filename;
    char* output_filename;

    AST parsed_tree;
    scope_t* global_scope;
    scope_link_t* scope_link;
} translation_unit_t;

// Configuration file directives
struct configuration_directive_t
{
    char* name;
    int (*funct)(char* value);
};

struct debug_flags_list_t
{
    char* name;
    char* flag_pointer;
    char* description;
};

typedef 
struct debug_options_tag
{
    char abort_on_ice;
    char print_scope;
    char print_scope_brief;
    char enable_debug_code;
    char debug_lexer;
    char debug_parser;
    char print_ast;
} debug_options_t;

typedef 
struct external_var_tag 
{
    char* name;
    char* value;
} external_var_t;

// Global compiler options
typedef
struct compilation_options_tag
{
    // For further use
    int argc;
    char** argv;
    char* exec_basename;

    // Options
    char verbose;
    char keep_files;
    char check_dates;
    char do_not_link;
    char do_not_compile;
    char do_not_prettyprint;
    char disable_openmp;

    debug_options_t debug_options;
    
    // Source language information
    source_language_t source_language;

    // List of translation units
    translation_unit_t** translation_units;
    int num_translation_units;

    // Result of the execution
    int execution_result;

    // Config file
    char* config_file;

    // This makes things non reentrant (but globally accessable without
    // parameter cluttering)
    scope_t* global_scope;
    scope_link_t* scope_link;

    // Output filename
    char* linked_output_filename;
    
    // Toolchain information
    char* preprocessor_name;
    char** preprocessor_options;

    char* native_compiler_name;
    char** native_compiler_options;

    char* linker_name;
    char** linker_options;

    char* output_directory;

    int num_compiler_phases;
    char** compiler_phases;

    // Pragma prefixes
    int num_pragma_custom_prefix;
    char** pragma_custom_prefix;

    // External vars for compiler pipeline
    int num_external_vars;
    external_var_t** external_vars;
} compilation_options_t;

MCXX_END_DECLS

#endif // CXX_DRIVER_DECLS_H
