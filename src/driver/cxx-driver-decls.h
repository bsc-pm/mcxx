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
#ifndef CXX_DRIVER_DECLS_H
#define CXX_DRIVER_DECLS_H

#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-scopelink-decls.h"
#include "cxx-buildscope-decls.h"
#include <stddef.h>

MCXX_BEGIN_DECLS

// Options for command line arguments
typedef enum 
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
    OPTION_CONFIG_FILE,
    OPTION_CONFIG_DIR,
    OPTION_PROFILE,
    OPTION_TYPECHECK,
    OPTION_PREPROCESSOR_USES_STDOUT,
    OPTION_DISABLE_GXX_TRAITS,
    OPTION_PASS_THROUGH,
    OPTION_DISABLE_SIZEOF,
    OPTION_SET_ENVIRONMENT,
    OPTION_LIST_ENVIRONMENTS,
    OPTION_PRINT_CONFIG_FILE,
    OPTION_PRINT_CONFIG_DIR,
    OPTION_ENABLE_UPC,
} COMMAND_LINE_OPTIONS;

// Kind of source 
typedef enum source_kind_tag
{
    SOURCE_KIND_UNKNOWN = 0,
    SOURCE_KIND_NOT_PREPROCESSED,
    SOURCE_KIND_PREPROCESSED
} source_kind_t;

typedef enum source_language_tag
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
    const char* name;
    source_language_t source_language;
    source_kind_t source_kind;
};

typedef struct top_level_include_tag
{
    const char *included_file;
    char system_include;
} top_level_include_t;

// Represents one translation unit
typedef struct translation_unit_tag
{
    const char* input_filename;
    const char* output_filename;

    struct AST_tag* parsed_tree;
    decl_context_t global_decl_context;
    scope_link_t* scope_link;

    int num_top_level_includes;
    top_level_include_t **top_level_include_list;

    // Opaque pointer used when running compiler phases
    void *dto;
} translation_unit_t;

struct compilation_configuration_tag;

// Configuration file directives
struct configuration_directive_t
{
    const char* name;
    int (*funct)(struct compilation_configuration_tag*, const char* value);
};

struct debug_flags_list_t
{
    const char* name;
    size_t flag_offset;
    const char* description;
};

typedef struct debug_options_tag
{
    char abort_on_ice;
    char print_scope;
    char enable_debug_code;
    char debug_lexer;
    char debug_parser;
    char print_ast;
    char print_memory_report;
    char print_memory_report_in_bytes;
    char debug_sizeof;
    char do_not_run_gdb;
} debug_options_t;

typedef struct external_var_tag {
    const char* name;
    const char* value;
} external_var_t;

typedef enum pragma_directive_kind_tag
{
    PDK_NONE = 0,
    PDK_DIRECTIVE,
    PDK_CONSTRUCT
} pragma_directive_kind_t;

typedef struct pragma_directive_set_tag
{
    int num_directives;
    const char **directive_names;
    pragma_directive_kind_t *directive_kinds;
} pragma_directive_set_t;

struct compilation_file_process_tag;

typedef struct parameter_flags_tag
{
    const char *name;
    char value;
} parameter_flags_t;

typedef struct compilation_process_tag
{
    // Result of the execution
    int execution_result;

    // Config file
    const char *config_file;

    // Config dir
    const char *config_dir;
    
    // List of translation units
    struct compilation_file_process_tag** translation_units;
    int num_translation_units;
    
    // For further use
    int argc;
    const char** argv;
    const char* exec_basename;
    const char* home_directory;

    // The set of configurations as defined by the user in the configuration file
    int num_configurations;
    struct compilation_configuration_tag** configuration_set;

    // The set of flags as defined implicitly in the configuration file
    int num_parameter_flags;
    struct parameter_flags_tag **parameter_flags;

    // The compiler will switch these because compilation is always serialized (never nest it!)
    struct compilation_configuration_tag *current_compilation_configuration;
    struct translation_unit_tag *current_translation_unit;
} compilation_process_t;

typedef struct compilation_configuration_conditional_flags
{
    const char *flag;
    char value;
} compilation_configuration_conditional_flags_t;

typedef struct compilation_configuration_line
{
    const char *name;
    const char *value;

    int num_flags;
    struct compilation_configuration_conditional_flags *flags;
} compilation_configuration_line_t;

typedef struct compilation_configuration_tag
{
    const char *configuration_name;

    // Configuration lines, this information is used
    // before configuration commit
    int num_configuration_lines;
    struct compilation_configuration_line ** configuration_lines;

    // These options are filled in configuration commit time
    // Options
    char verbose;
    char keep_files;
    char check_dates;
    char do_not_process_files;
    char do_not_parse;
    char do_not_prettyprint;
    char do_not_compile;
    char do_not_link;
    char disable_openmp;
	char force_language;

    debug_options_t debug_options;
    
    // Source language information
    source_language_t source_language;

    // This makes things non reentrant (but globally accessable without
    // parameter cluttering)
    scope_link_t* scope_link;

    // Output filename
    const char* linked_output_filename;
    
    // Toolchain information
    const char* preprocessor_name;
    const char** preprocessor_options;
    char preprocessor_uses_stdout;

    const char* native_compiler_name;
    const char** native_compiler_options;

    const char* linker_name;
    const char** linker_options;

    const char* output_directory;

    int num_compiler_phases;
    const char** compiler_phases;
    
    // External vars for compiler pipeline of this configuration
    int num_external_vars;
    external_var_t** external_vars;

    // Pragma configuration
    // OMP pragmae
    pragma_directive_set_t pragma_omp_info;
    // Custom pragmae
    int num_pragma_custom_prefix;
    const char** pragma_custom_prefix;
    pragma_directive_set_t **pragma_custom_prefix_info;

    // Enable strict typecheck (will fail if something can't be verified or fails)
    char strict_typecheck;

    // Disable g++ 4.3 type traits
    char disable_gxx_type_traits;

    // Disable 'sizeof' computation
    char disable_sizeof;

    // Mimic all the process but preprocess and parsing
    char pass_through;

    // Type environment
    struct type_environment_tag* type_environment;

    // Unified Parallel C (UPC)
    char enable_upc;
    // If this is not null, this should be constant expression
    const char *upc_threads;
} compilation_configuration_t;


typedef struct compilation_file_process_tag
{
    translation_unit_t *translation_unit;
    compilation_configuration_t *compilation_configuration;

    char already_compiled;
} compilation_file_process_t;

#define CURRENT_CONFIGURATION(x) (compilation_process.current_compilation_configuration->x)
#define CURRENT_COMPILED_FILE(x) (compilation_process.current_translation_unit->x)


MCXX_END_DECLS

#endif // CXX_DRIVER_DECLS_H
