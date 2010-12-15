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
    OPTION_HELP_TARGET_OPTIONS,
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
    OPTION_ENABLE_HLT,
    OPTION_DO_NOT_UNLOAD_PHASES,
    OPTION_INSTANTIATE_TEMPLATES,
    OPTION_COLUMN_WIDTH,
    OPTION_VERBOSE
} COMMAND_LINE_OPTIONS;

// Kind of source 
#define BITMAP(X) (1<<X)
typedef enum source_kind_tag
{
    SOURCE_KIND_UNKNOWN = 0,
    SOURCE_KIND_NOT_PREPROCESSED = BITMAP(0),
    SOURCE_KIND_PREPROCESSED = BITMAP(1),
    SOURCE_KIND_FIXED_FORM = BITMAP(2),
    SOURCE_KIND_FREE_FORM = BITMAP(3),
    SOURCE_KIND_NOT_PARSED = BITMAP(4),
} source_kind_t;
#undef BITMAP

typedef enum source_language_tag
{
    SOURCE_LANGUAGE_UNKNOWN = 0,
    SOURCE_LANGUAGE_C,
    SOURCE_LANGUAGE_CXX,
    SOURCE_LANGUAGE_FORTRAN,
    SOURCE_LANGUAGE_CUDA,
    SOURCE_LANGUAGE_ASSEMBLER,
    SOURCE_LANGUAGE_LINKER_DATA,
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
    int (*funct)(struct compilation_configuration_tag*, const char* index, const char* value);
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
    char print_ast_graphviz;
    char print_ast_html;
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
    parameter_flags_t **parameter_flags;

    // The configuration chosen at command line
    struct compilation_configuration_tag *command_line_configuration;

    // The compiler will switch these because compilation is always serialized (never nest it!)
    struct compilation_file_process_tag* current_file_process;
    struct compilation_configuration_tag *current_compilation_configuration;
} compilation_process_t;

typedef struct compilation_configuration_conditional_flags
{
    const char *flag;
    char value;
} compilation_configuration_conditional_flags_t;

typedef struct flag_expr_tag flag_expr_t;

typedef struct compilation_configuration_line
{
    const char *name;
    const char *index;
    const char *value;

    flag_expr_t* flag_expr;
} compilation_configuration_line_t;

#if 0
typedef struct embed_map_tag
{
    const char* profile;
    const char* command;
} embed_map_t;

typedef struct identifier_map_tag
{
    const char* profile;
    const char* action;
} identifier_map_t;
#endif

typedef struct target_options_map_tag
{
    const char* profile;

    // Sublinking
    char do_sublink;

    // Embedding
    char do_embedding;
    enum 
    {
        EMBEDDING_MODE_INVALID = 0,
        EMBEDDING_MODE_BFD = 1,
    } embedding_mode;

    // Combining
    char do_combining;
    enum
    {
        COMBINING_MODE_INVALID = 0,
        COMBINING_MODE_SPU_ELF, 
        COMBINING_MODE_INCBIN,
    } combining_mode;
} target_options_map_t;

typedef struct code_shape_tag
{
    char short_enums;
} code_shape_t;


// Compiler phases: 
// the ones that are loaded from files and the one that modifies the dto
typedef struct compiler_phase_loader_tag compiler_phase_loader_t;

typedef struct compilation_configuration_tag
{
    const char *configuration_name;
    struct compilation_configuration_tag* base_configuration;

    // Configuration lines, this information is used
    // before configuration commit
    int num_configuration_lines;
    struct compilation_configuration_line ** configuration_lines;

    // These options are filled in configuration commit time
    // Options
    char verbose;
    char keep_files;
    char keep_temporaries;
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

#ifdef FORTRAN_SUPPORT
    // Fortran prescanner
    const char** prescanner_options;
    int column_width;
#endif

    const char* native_compiler_name;
    const char** native_compiler_options;

    const char* linker_name;
    const char** linker_options;

    const char* output_directory;

    // Include directories
    int num_include_dirs;
    const char** include_dirs;

    int num_compiler_phases;
	compiler_phase_loader_t** phase_loader;

    // States whether the phases of this compiler were loaded
    char phases_loaded;
    
    // External vars for compiler pipeline of this configuration
    int num_external_vars;
    external_var_t** external_vars;

    // Pragma configuration
    // Custom pragmae
    int num_pragma_custom_prefix;
    const char** pragma_custom_prefix;
    pragma_directive_set_t **pragma_custom_prefix_info;

    // Enable strict typecheck (will fail if something can't be verified or fails)
    char strict_typecheck;

    // Disable g++ 4.3 type traits
    char disable_gxx_type_traits;

    // Enable explicit instantiation
    char explicit_instantiation;

    // Disable 'sizeof' computation
    char disable_sizeof;

    // Mimic all the process but preprocess and parsing
    char pass_through;

    // Type environment
    struct type_environment_tag* type_environment;

    // Flags affecting some bits of the language
    code_shape_t code_shape;

    // Unified Parallel C (UPC)
    char enable_upc;
    // If this is not null, this should be a constant expression
    const char *upc_threads;

    // Enable HLT
    char enable_hlt;

    // Enable C++1x
    char enable_cxx1x;

    // Target options
    int num_target_option_maps;
    target_options_map_t** target_options_maps;
} compilation_configuration_t;

struct compiler_phase_loader_tag
{
    void (*func)(compilation_configuration_t* compilation_configuration, const char* data);
    const char* data;
};

typedef struct compilation_file_process_tag
{
    translation_unit_t *translation_unit;
    compilation_configuration_t *compilation_configuration;

    char already_compiled;

    int num_secondary_translation_units;
    struct compilation_file_process_tag **secondary_translation_units;
} compilation_file_process_t;

// These castings are here to convert these expressions in lvalues so people won't modify them
#define CURRENT_CONFIGURATION ((compilation_configuration_t*)compilation_process.current_compilation_configuration)
#define CURRENT_COMPILED_FILE ((translation_unit_t*)compilation_process.current_file_process->translation_unit)
#define CURRENT_FILE_PROCESS ((compilation_file_process_t*)compilation_process.current_file_process)

// Whenever you modify SET_CURRENT_FILE_PROCESS update also
// SET_CURRENT_CONFIGURATION to its configuration
#define SET_CURRENT_FILE_PROCESS(_x) (compilation_process.current_file_process = _x)
#define SET_CURRENT_CONFIGURATION(_x) (compilation_process.current_compilation_configuration = _x)


MCXX_END_DECLS

#endif // CXX_DRIVER_DECLS_H
