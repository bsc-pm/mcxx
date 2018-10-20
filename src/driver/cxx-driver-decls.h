/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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




#ifndef CXX_DRIVER_DECLS_H
#define CXX_DRIVER_DECLS_H

#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-nodecl-decls.h"
#include "fortran03-typeenviron-decls.h"
#include <stddef.h>

MCXX_BEGIN_DECLS

typedef enum codegen_parameter_tag
{
    CODEGEN_PARAM_NONTYPE_TEMPLATE_ARGUMENT = 0,
}
codegen_parameter_t;

// Kind of source
#define BITMAP(X) (1<<X)
typedef enum source_kind_tag
{
    SOURCE_KIND_UNKNOWN = 0,
    SOURCE_KIND_NOT_PREPROCESSED = BITMAP(0),
    SOURCE_KIND_PREPROCESSED = BITMAP(1),
    SOURCE_KIND_FIXED_FORM = BITMAP(2),
    SOURCE_KIND_FREE_FORM = BITMAP(3),
    SOURCE_KIND_DO_NOT_PROCESS = BITMAP(4),
    SOURCE_KIND_DO_NOT_COMPILE = BITMAP(5),
    SOURCE_KIND_DO_NOT_LINK = BITMAP(6),
    SOURCE_KIND_DO_NOT_EMBED = BITMAP(7),
} source_kind_t;
#undef BITMAP

typedef enum source_language_tag
{
    SOURCE_LANGUAGE_UNKNOWN     = 0,
    SOURCE_LANGUAGE_C           = 1,
    SOURCE_LANGUAGE_CXX         = 2,
    SOURCE_LANGUAGE_FORTRAN     = 3,
    SOURCE_LANGUAGE_ASSEMBLER   = 4,
    SOURCE_LANGUAGE_LINKER_DATA = 5,

    /* Bit tag for sublanguages */
    SOURCE_IS_SUBLANGUAGE       = 16,

    SOURCE_SUBLANGUAGE_CUDA     = (SOURCE_IS_SUBLANGUAGE | 1),
    SOURCE_SUBLANGUAGE_OPENCL   = (SOURCE_IS_SUBLANGUAGE | 2),
} source_language_t;

// List of valid vendors
// NATIVE_VENDOR(NAME, FLAG) is a template, where:
//      * NAME is concatenated to NATIVE_VENDOR_ to generate a new enumerator for each vendor
//      * FLAG represents the value of the '--native-vendor' flag for each vendor
#define NATIVE_VENDORS_LIST \
    NATIVE_VENDOR(GNU, gnu) \
    NATIVE_VENDOR(INTEL, intel) \
    NATIVE_VENDOR(IBM, ibm) \
    NATIVE_VENDOR(NVIDIA, nvidia) \
    NATIVE_VENDOR(CRAY, cray) \

typedef enum native_vendor_tag
{
    NATIVE_VENDOR_UNKNOWN = 0,
#define NATIVE_VENDOR(NAME, STR) NATIVE_VENDOR_##NAME,
    NATIVE_VENDORS_LIST
#undef NATIVE_VENDOR
} native_vendor_t;

typedef struct sublanguage_profile_tag
{
    source_language_t sublanguage;
    const char* profile;
} sublanguage_profile_t;

extern sublanguage_profile_t sublanguage_profiles[];

extern const char* source_language_names[];

// File extensions table
struct extensions_table_t
{
    const char* name;
    source_language_t source_language;
    source_kind_t source_kind;
};

typedef struct include_tag
{
    const char *included_file;
    char system_include;
} include_t;

typedef struct module_to_wrap_info_tag
{
    const char* module_name;
    const char* native_file;
    const char* mercurium_file;
} module_to_wrap_info_t;

// Represents one translation unit
typedef struct translation_unit_tag
{
    const char* input_filename;
    const char* output_filename;

    struct AST_tag* parsed_tree;
    nodecl_t nodecl;
    const decl_context_t* global_decl_context;

    int num_includes;
    include_t **include_list;

    // This is a cache of module files actually opened and loaded
    rb_red_blk_tree *module_file_cache;

    int num_modules_to_wrap;
    module_to_wrap_info_t** modules_to_wrap;

    int num_module_files_to_hide;
    const char** module_files_to_hide;

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
    char abort_on_error_message;
    char backtrace_on_ice;
    char print_scope;
    char enable_debug_code;
    char debug_lexer;
    char debug_parser;
    char print_nodecl_graphviz;
    char print_nodecl_html;
    char print_ast_graphviz;
    char print_memory_report;
    char print_memory_report_in_bytes;
    char debug_sizeof;
    char do_not_run_gdb;
    char binary_check;
    // Analysis flags. Those are not handled by the driver, but by the analysis phase.
    char analysis_verbose;
    char ranges_verbose;
    char tdg_verbose;
    char analysis_perf;
    char analysis_info;
    char print_pcfg;
    char print_pcfg_w_context;
    char print_pcfg_w_analysis;
    char print_pcfg_full;
    char print_tdg;
    char tdg_to_json;
    // Others
    char do_not_codegen;
    char show_template_packs;
    char vectorization_verbose;
    char stats_string_table;
} debug_options_t;

extern debug_options_t debug_options;

typedef struct external_var_tag {
    const char* name;
    const char* value;
} external_var_t;

typedef enum pragma_directive_kind_tag
{
    PDK_NONE = 0,
    PDK_DIRECTIVE,
    PDK_CONSTRUCT,
    PDK_CONSTRUCT_NOEND
} pragma_directive_kind_t;

typedef struct pragma_directive_set_tag
{
    int num_directives;
    const char **directive_names;
    pragma_directive_kind_t *directive_kinds;
} pragma_directive_set_t;

struct compilation_file_process_tag;

typedef
enum parameter_flag_value_tag
{
    PFV_UNDEFINED = 0,
    PFV_FALSE = 1,
    PFV_TRUE = 2,
} parameter_flag_value_t;

typedef struct parameter_flags_tag
{
    const char *name;
    parameter_flag_value_t value;
} parameter_flags_t;

typedef struct subgoal_tag
{
    const char* linked_subgoal_filename;
    struct compilation_configuration_tag* configuration;
} subgoal_t;

typedef struct compilation_process_tag
{
    // Result of the execution
    int execution_result;

    // Config dir
    const char *config_dir;

    // List of translation units
    struct compilation_file_process_tag** translation_units;
    int num_translation_units;

    // Meaningful only if we are going to link
    const char *linked_output_filename;

    // Used for sublinking
    int num_subgoals;
    subgoal_t *subgoals;

    // For further use. They can be modified as we need
    int argc;
    const char** argv;

    // These should not be modified
    int original_argc;
    const char** original_argv;

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

    // Flags
    char parallel_process; // enables features allowing parallel compilation
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

    // location
    const char *filename;
    int line;

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
        EMBEDDING_MODE_PARTIAL_LINKING = 2,
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

typedef struct parameter_linker_command_tag
{
    const char *argument;
    translation_unit_t *translation_unit;
} parameter_linker_command_t;


typedef const char* (*print_vector_type_fun)(const decl_context_t*, type_t*, print_symbol_callback_t, void*);

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
    char do_not_process_files;
    char do_not_parse;
    char do_not_prettyprint;
    char do_not_compile;
    char do_not_link;
    char generate_assembler;
    char enable_openmp;
	char force_language;

    // -Werror
    char warnings_as_errors;

    // Source language information
    source_language_t source_language;

    // Toolchain information
    const char* preprocessor_name;
    const char** preprocessor_options;
    char preprocessor_uses_stdout;

    // Fortran preprocessor
    const char* fortran_preprocessor_name;
    const char** fortran_preprocessor_options;

    // Fortran prescanner
    const char* prescanner_name;
    const char** prescanner_options;
    int input_column_width;
    int output_column_width;

    // Disable Fortran intrinsics
    int num_disabled_intrinsics;
    const char ** disabled_intrinsics_list;

    // Fortran module wrapping
    char do_not_wrap_fortran_modules;

    // Directories where we look for modules
    int num_module_dirs;
    const char** module_dirs;

    // Directory where we generate modules
    const char* module_out_dir;
    // Pattern used to tell the native compiler
    // where we want the modules be generated
    const char* module_out_pattern;

    // Directory where we unwrap the native modules
    const char* module_native_dir;

    // Directory where we hold module locks
    const char* lock_dir;
    char disable_locking;

    // Directory where we extract multifile info
    const char* multifile_dir;

    // Fortran default kinds
    int default_integer_kind;
    int default_real_kind;
    int default_logical_kind;
    int default_character_kind;
    int doubleprecision_kind;

    source_kind_t force_source_kind;

    const char* native_compiler_name;
    const char** native_compiler_options;

    const char* linker_name;

    int num_args_linker_command;
    parameter_linker_command_t** linker_command;

    const char** linker_options;
    // --Wr,
    // Added before all linker options
    const char** linker_options_pre;
    // --WL,
    // Added after all linker options
    const char** linker_options_post;

    // Toolchain tools of the target
    const char* target_objcopy;
    const char* target_objdump;
    const char* target_ld;
    const char* target_ar;

    const char* output_directory;

    // Include directories
    int num_include_dirs;
    const char** include_dirs;

    int num_compiler_phases;
	compiler_phase_loader_t** phase_loader;

    // Codegen phase for this profile
    // This is a void* because it points to a C++ class
    void* codegen_phase;

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

    // Disable g++ 4.3 type traits
    char disable_gxx_type_traits;

    // Enable MS builtin types
    // __int8, __int16, __int32 and __int64
    char enable_ms_builtin_types;

    // Enable Intel C/C++ builtins syntax
    char enable_intel_builtins_syntax;

    // Enable Intel C/C++ builtin intrinsics
    char enable_intel_intrinsics;

    // Enable special vector types for Intel SSE/AVX
    // struct __m128, struct __m256
    char enable_intel_vector_types;

    // Enable explicit instantiation
    char explicit_instantiation;

    // Disable 'sizeof' computation
    char disable_sizeof;

    // Mimic all the process but preprocess and parsing
    char pass_through;

    // Type environment
    struct type_environment_tag* type_environment;

    // Fortran array descriptor (compiler dependent)
    struct fortran_array_descriptor_t* fortran_array_descriptor;

    // Fortran mangling (compiler dependent)
    struct fortran_name_mangling_t* fortran_name_mangling;

    // Flags affecting some bits of the language
    code_shape_t code_shape;

    // Unified Parallel C (UPC)
    char enable_upc;
    // If this is not null, this should be a constant expression
    const char *upc_threads;

    // States whether the -std flag was explicitly specified
    char explicit_std_version;

    // Enable C11
    char enable_c11;

    // Enable C++11
    char enable_cxx11;

    // Enable C++14
    char enable_cxx14;

    // Enable full Fortran 2003
    char enable_f03;

    // Enable full Fortran 2008
    char enable_f08;

    // Enable CUDA
    char enable_cuda;

    // Enable OPENCL
    char enable_opencl;

    //OpenCL Build options
    const char* opencl_build_options;

    // Target options
    int num_target_option_maps;
    target_options_map_t** target_options_maps;

    // Fortran lexing
    char disable_empty_sentinels;

    // Vector flavor
    print_vector_type_fun print_vector_type;
    print_vector_type_fun print_mask_type;

    // C/C++ FE does not preserve parentheses
    // unless this flag is set
    char preserve_parentheses;

    // Do not make an extra pass to resolve externall calls to functions in the
    // same file
    char fortran_no_whole_file;

    // Specifies the vendor of the backend compiler
    native_vendor_t native_vendor;

    // Emit line markers in the output files
    char line_markers;

    // List of profile errors
    int num_errors;
    const char** error_messages;
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
    int tag; // zero by default

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
