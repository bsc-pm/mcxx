/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#ifdef WIN32_BUILD
  #include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>
#include <malloc.h>
#include <errno.h>
#include <unistd.h>

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
#include <signal.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-driver-utils.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-html.h"
#include "cxx-prettyprint.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-typeenviron.h"
#include "cxx-lexer.h"
#include "cxx-dyninit.h"
#include "cxx-printscope.h"
#include "cxx-exprtype.h"
#include "cxx-typededuc.h"
#include "cxx-overload.h"
#include "cxx-lexer.h"
#include "cxx-parser.h"
#include "c99-parser.h"
#include "cxx-upc.h"
#include "cxx-configfile.h"
#include "cxx-profile.h"
#include "cxx-multifile.h"
#include "cxx-nodecl.h"
#include "cxx-nodecl-output.h"
// It does not include any C++ code in the header
#include "cxx-compilerphases.hpp"

#include "filename.h"

#ifdef FORTRAN_SUPPORT
#include "fortran03-parser.h"
#include "fortran03-lexer.h"
#include "fortran03-prettyprint.h"
#include "fortran03-split.h"
#include "fortran03-buildscope.h"
#include "fortran03-nodecl.h"
#endif

/* ------------------------------------------------------------------ */
#define HELP_STRING \
"Options: \n" \
"  -h, --help               Shows this help and quits\n" \
"  --version                Shows version and quits\n" \
"  --verbose                Runs verbosely, displaying the programs\n" \
"                           invoked by the compiler\n" \
"  -o, --output=<file>      Sets <file> as the output file\n" \
"  -c                       Does not link, just compile\n" \
"  -E                       Does not compile, just preprocess\n" \
"  -I <dir>                 Adds <dir> into the searched include\n" \
"                           directories\n" \
"  -L <dir>                 Adds <dir> into the searched library\n" \
"                           directories\n" \
"  -l <name>                Adds lib<name> into the final link\n" \
"  -g                       Enables debug for the native compilation\n" \
"  -D<macro>                Passes <macro> to the preprocessor\n" \
"  -U<macro>                Undefines <macro> to the preprocessor\n" \
"  -O -O0 -O1 -O2 -O3       Sets optimization level to the\n" \
"                           preprocessor and native compiler\n" \
"  -y                       File will be parsed but it will not be\n" \
"                           compiled not linked.\n" \
"  -x lang                  Override language detection to <lang>\n" \
"  -k, --keep-files         Do not remove intermediate files\n" \
"  -K, --keep-all-files     Do not remove any generated file, including\n" \
"                           temporal files\n" \
"  --output-dir=<dir>       Prettyprinted files will be left in\n" \
"                           directory <dir>. Otherwise the input\n" \
"                           file directory is used\n" \
"  --debug-flags=<flags>    Comma-separated list of flags for used\n" \
"                           when debugging. Valid flags can be listed\n" \
"                           with --help-debug-flags\n" \
"  --cpp=<name>             Preprocessor <name> will be used for\n" \
"                           preprocessing\n" \
"  --cxx=<name>             Compiler <name> will be used for native\n" \
"                           compilation\n" \
"  --cc=<name>              Another name for --cxx=<name>\n" \
"  --ld=<name>              Linker <name> will be used for linking\n" \
"  --fpc=<name>             Fortran prescanner <name> will be used\n" \
"                           for fixed form prescanning\n" \
"                           This flag is only meaningful for Fortran\n" \
"  --W<flags>,<options>     Pass comma-separated <options> on to\n" \
"                           the several programs invoked by the driver\n" \
"                           Flags is a sequence of 'p', 'n', 's', or 'l'\n" \
"                              p: preprocessor\n"  \
"                              s: Fortran prescanner\n" \
"                              n: native compiler\n" \
"                              l: linker\n" \
"  --Wx:<profile>:<flags>,options\n" \
"                           Like --W<flags>,<options> but for\n" \
"                           a specific compiler profile\n" \
"  --no-openmp              Disables all OpenMP support\n" \
"  --config-file=<file>     Uses <file> as config file.\n" \
"                           Use --print-config-file to get the\n" \
"                           default path\n" \
"  --print-config-file      Prints the path of the default\n" \
"                           configuration file and finishes.\n" \
"  --config-dir=<dir>       Sets <dir> as the configuration directory\n" \
"                           Use --print-config-dir to get the\n" \
"                           default path\n" \
"  --print-config-dir       Prints the path of the default\n" \
"                           configuration directory and finishes.\n" \
"  --profile=<name>         Selects profile compilation to be <name>\n" \
"  --variable=<name:value>  Defines variable 'name' with value\n" \
"                           'value' to be used in the compiler\n" \
"                           phases pipeline\n" \
"  --typecheck              Strict typechecking. If an expression\n" \
"                           cannot be checked compilation fails.\n" \
"  --disable-gxx-traits     Disables g++ 4.3 type traits. Required\n" \
"                           if you use g++ 4.2 or previous.\n" \
"  --env=<env-name>         Sets <env-name> as the specific\n" \
"                           environment. Use --list-env to show\n" \
"                           currently supported environments\n" \
"  --list-env               Lists currently supported environments\n" \
"                           and does nothing else.\n" \
"  --pass-through           Disables preprocessing and parsing but\n" \
"                           invokes remaining steps. A previous\n" \
"                           invocation with --keep is required.\n" \
"                           This flag also implies --keep.\n" \
"  --disable-sizeof         Disable sizeof computation. Use it\n" \
"                           only if the compiler has problems when\n" \
"                           computing size of types. Please report\n" \
"                           a bug, you should not need this option.\n" \
"  --upc[=THREADS]          Enables UPC 1.2 syntactic support.\n" \
"                           Optionally you can define a static \n" \
"                           number of THREADS.\n" \
"  --cuda                   Enables experimental support for CUDA\n" \
"  --hlt                    Enable High Level Transformations\n" \
"                           This enables '#pragma hlt'\n" \
"  --do-not-unload-phases   If the compiler crashes when unloading\n" \
"                           phases, use this flag to avoid the\n" \
"                           compiler to unload them.\n" \
"  --help-debug-flags       Shows debug flags valid for option\n" \
"                           --debug-flags\n" \
"  --help-target-options    Shows valid target options for\n" \
"                           'target_options' option of configuration\n" \
"                           file.\n" \
"  --instantiate            Instantiate explicitly templates. This is\n" \
"                           an unsupported experimental feature\n" \
"  --pp[=on]                Preprocess files\n"\
"                           This is the default for files ending with\n"\
"                           C/C++: .c, .cc, .C, .cp, .cpp, .cxx, .c++\n"\
"                           Fortran: .F, .F77, .F90, .F95\n"\
"  --pp=off                 Disables preprocessing\n"\
"                           This is the default for files ending with\n"\
"                           C/C++: .i, .ii\n"\
"                           Fortran: .f, .f77, .f90, .f95\n"\
"  --pp-stdout              Preprocessor uses stdout for output\n" \
"  --width=<width>          Fortran column width. By default 132\n" \
"  --free                   Force Fortran free form regardless of\n" \
"                           extension.\n"\
"                           This is the default for files ending with\n"\
"                           .f90, .F90, .f95, .F95, .f03 or .F03\n"\
"  --fixed                  Force Fortran fixed form regardless of\n" \
"                           extension\n"\
"                           This is the default for files ending with\n"\
"                           .f, .F, .f77, .F77\n"\
"  --fpp                    An alias for --pp=on\n"\
"  --sentinels=on|off       Enables or disables empty sentinels\n" \
"                           Empty sentinels are enabled by default\n" \
"                           This flag is only meaningful for Fortran\n" \
"  --disable-intrinsics     Ignore all known Fortran intrinsics\n" \
"  -J <dir>                 Sets <dir> as the output module directory\n" \
"                           This flag is only meaningful for Fortran\n" \
"  --nodecl                 Nodecl processing (EXPERIMENTAL)\n" \
"\n" \
"gcc compatibility flags:\n" \
"\n" \
"  -v\n" \
"  -f<name>\n" \
"  -m<name>\n" \
"  -M\n" \
"  -MM\n" \
"  -MF <file>\n" \
"  -MG <file>\n" \
"  -MP\n" \
"  -MT <target>\n" \
"  -MD\n" \
"  -MMD\n" \
"  -static\n" \
"  -shared\n" \
"  -std=<option>\n" \
"  -rdynamic\n" \
"  -export-dynamic\n" \
"  -w\n" \
"  -W<option>\n" \
"  -pthread\n" \
"  -Xpreprocessor OPTION\n" \
"  -Xlinker OPTION\n" \
"  -Xassembler OPTION\n" \
"  -S\n" \
"  -dA\n" \
"  -dD\n" \
"  -dH\n" \
"  -dm\n" \
"  -dp\n" \
"  -dP\n" \
"  -dv\n" \
"  -dx\n" \
"  -dy\n" \
"\n" \
"These gcc flags are passed verbatim to preprocessor, compiler and\n" \
"linker. Some of them may disable compilation and linking to be\n" \
"compatible with gcc and applications expecting gcc behaviour.\n" \
"\n"
/* ------------------------------------------------------------------ */

// alternate signal stack
#if !defined(WIN32_BUILD)
static char *_alternate_signal_stack;
#endif

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
    OPTION_ENABLE_CUDA,
    OPTION_ENABLE_HLT,
    OPTION_DO_NOT_UNLOAD_PHASES,
    OPTION_INSTANTIATE_TEMPLATES,
    OPTION_ALWAYS_PREPROCESS,
    OPTION_FORTRAN_COLUMN_WIDTH,
    OPTION_FORTRAN_FIXED,
    OPTION_FORTRAN_FREE,
    OPTION_EMPTY_SENTINELS,
    OPTION_DISABLE_INTRINSICS,
    OPTION_NODECL,
    OPTION_FORTRAN_PRESCANNER,
    OPTION_VERBOSE
} COMMAND_LINE_OPTIONS;


// It mimics getopt
#define SHORT_OPTIONS_STRING "vkKacho:EyI:J:L:l:gD:U:x:"
// This one mimics getopt_long but with one less field (the third one is not given)
struct command_line_long_options command_line_long_options[] =
{
    {"help",        CLP_NO_ARGUMENT, 'h'},
    {"version",     CLP_NO_ARGUMENT, OPTION_VERSION},
    {"v",           CLP_NO_ARGUMENT, OPTION_VERBOSE},
    {"verbose",     CLP_NO_ARGUMENT, OPTION_VERBOSE},
    {"keep-files",  CLP_NO_ARGUMENT, 'k'},
    {"keep-all-files", CLP_NO_ARGUMENT, 'K'},
    {"check-dates", CLP_NO_ARGUMENT, 'a'},
    {"output",      CLP_REQUIRED_ARGUMENT, 'o'},
    // This option has a chicken-and-egg problem. If we delay till getopt_long
    // to open the configuration file we overwrite variables defined in the
    // command line. Thus "load_configuration" is invoked before command line parsing
    // and looks for "--config-file" / "-m" in the arguments
    {"config-file", CLP_REQUIRED_ARGUMENT, OPTION_CONFIG_FILE},
    // This option has a chicken-and-egg similar to the --config-file.
    // It is handled in "load_configuration"
    {"profile", CLP_REQUIRED_ARGUMENT, OPTION_PROFILE},
    {"output-dir",  CLP_REQUIRED_ARGUMENT, OPTION_OUTPUT_DIRECTORY},
    {"cc", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_COMPILER_NAME},
    {"cxx", CLP_REQUIRED_ARGUMENT, OPTION_NATIVE_COMPILER_NAME},
    {"cpp", CLP_REQUIRED_ARGUMENT, OPTION_PREPROCESSOR_NAME},
    {"ld", CLP_REQUIRED_ARGUMENT, OPTION_LINKER_NAME},
    {"debug-flags",  CLP_REQUIRED_ARGUMENT, OPTION_DEBUG_FLAG},
    {"help-debug-flags", CLP_NO_ARGUMENT, OPTION_HELP_DEBUG_FLAGS},
    {"help-target-options", CLP_NO_ARGUMENT, OPTION_HELP_TARGET_OPTIONS},
    {"no-openmp", CLP_NO_ARGUMENT, OPTION_NO_OPENMP},
    {"variable", CLP_REQUIRED_ARGUMENT, OPTION_EXTERNAL_VAR},
    {"typecheck", CLP_NO_ARGUMENT, OPTION_TYPECHECK},
    {"pp-stdout", CLP_NO_ARGUMENT, OPTION_PREPROCESSOR_USES_STDOUT},
    {"disable-gxx-traits", CLP_NO_ARGUMENT, OPTION_DISABLE_GXX_TRAITS},
    {"pass-through", CLP_NO_ARGUMENT, OPTION_PASS_THROUGH}, 
    {"disable-sizeof", CLP_NO_ARGUMENT, OPTION_DISABLE_SIZEOF},
    {"env", CLP_REQUIRED_ARGUMENT, OPTION_SET_ENVIRONMENT},
    {"list-env", CLP_NO_ARGUMENT, OPTION_LIST_ENVIRONMENTS},
    {"print-config-file", CLP_NO_ARGUMENT, OPTION_PRINT_CONFIG_FILE},
    {"print-config-dir", CLP_NO_ARGUMENT, OPTION_PRINT_CONFIG_DIR},
    {"upc", CLP_OPTIONAL_ARGUMENT, OPTION_ENABLE_UPC},
    {"cuda", CLP_NO_ARGUMENT, OPTION_ENABLE_CUDA},
    {"hlt", CLP_NO_ARGUMENT, OPTION_ENABLE_HLT},
    {"do-not-unload-phases", CLP_NO_ARGUMENT, OPTION_DO_NOT_UNLOAD_PHASES},
    {"instantiate", CLP_NO_ARGUMENT, OPTION_INSTANTIATE_TEMPLATES},
    {"pp", CLP_OPTIONAL_ARGUMENT, OPTION_ALWAYS_PREPROCESS},
    {"fpp", CLP_NO_ARGUMENT, OPTION_ALWAYS_PREPROCESS},
    {"width", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_COLUMN_WIDTH},
    {"fixed", CLP_NO_ARGUMENT, OPTION_FORTRAN_FIXED},
    {"free", CLP_NO_ARGUMENT, OPTION_FORTRAN_FREE},
    {"sentinels", CLP_REQUIRED_ARGUMENT, OPTION_EMPTY_SENTINELS},
    {"disable-intrinsics", CLP_NO_ARGUMENT, OPTION_DISABLE_INTRINSICS},
    {"fpc", CLP_REQUIRED_ARGUMENT, OPTION_FORTRAN_PRESCANNER },
    {"nodecl", CLP_NO_ARGUMENT, OPTION_NODECL },
    // sentinel
    {NULL, 0, 0}
};

char* source_language_names[] =
{
    [SOURCE_LANGUAGE_UNKNOWN] = "unknown",
    [SOURCE_LANGUAGE_C] = "C",
    [SOURCE_LANGUAGE_CXX] = "C++",
    [SOURCE_LANGUAGE_CUDA] = "CUDA C/C++",
    [SOURCE_LANGUAGE_FORTRAN] = "Fortran",
    [SOURCE_LANGUAGE_ASSEMBLER] = "assembler",
};

static void print_version(void);
static void driver_initialization(int argc, const char* argv[]);
static void initialize_default_values(void);
static void load_configuration(void);
static void finalize_committed_configuration(void);
static void commit_configuration(void);
static void compile_every_translation_unit(void);

static void compiler_phases_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit, 
        const char* parsed_filename);
static void compiler_phases_pre_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit,
        const char* parsed_filename);
static const char* preprocess_file(translation_unit_t* translation_unit, const char* input_filename);
static void parse_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename);
static void initialize_semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename);
static void semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename);
static const char* prettyprint_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename);
static void native_compilation(translation_unit_t* translation_unit, 
        const char* prettyprinted_filename, char remove_input);

#ifdef FORTRAN_SUPPORT
static const char *fortran_prescan_file(translation_unit_t* translation_unit, const char *parsed_filename);
#endif

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig);
#endif
static char check_tree(AST a);
static char check_for_ambiguities(AST a, AST* ambiguous_node);

static void embed_files(void);
static void link_objects(void);

static void add_to_parameter_list_str(const char*** existing_options, const char* str);
static void parse_subcommand_arguments(const char* arguments);

static void enable_debug_flag(const char* flag);

static void load_compiler_phases(compilation_configuration_t* config);

static void help_message(void);

static void print_memory_report(void);

static int parse_special_parameters(int *should_advance, int argc, 
        const char* argv[], char dry_run);
static int parse_implicit_parameter_flag(int *should_advance, const char *special_parameter);

static void list_environments(void);

static char do_not_unload_phases = 0;
static char show_help_message = 0;

int main(int argc, char* argv[])
{
    timing_t timing_global;
    timing_start(&timing_global);

    // Initialization of the driver
    driver_initialization(argc, (const char**)argv);

    // Default values
    initialize_default_values();

    // Load configuration files and the profiles defined there Here we get all
    // the implicit parameters defined in configuration files and we switch to
    // the main profile of the compiler. Profiles are not yet fully populated.
    load_configuration();
    
    // Parse arguments just to get the implicit parameters passed in the
    // command line. We need those to properly populate profiles.
    parse_arguments(compilation_process.argc,
            compilation_process.argv, 
            /* from_command_line= */ 1,
            /* parse_implicits_only */ 1);

    // This commits the profiles using the implicit parameters passed in the
    // command line that we have just fetched.  Committing a profile means
    // filling its values.
    commit_configuration();

    // Parse arguments again, but this time ignore implicit ones and
    // update the chosen profile
    char parse_arguments_error;
    parse_arguments_error = parse_arguments(compilation_process.argc,
            compilation_process.argv, 
            /* from_command_line= */ 1,
            /* parse_implicits_only */ 0);
    
    if (parse_arguments_error)
    {
        if (show_help_message)
        {
            help_message();
        }
        exit(EXIT_FAILURE);
    }

    // This performs additional steps depending on some enabled features
    finalize_committed_configuration();

    // Compiler phases can define additional dynamic initializers
    // (besides the built in ones)
    run_dynamic_initializers();
    
    // Compilation of every specified translation unit
    compile_every_translation_unit();

    // Embed files
    embed_files();

    // Link all generated objects
    link_objects();

    // Unload phases
    if (!do_not_unload_phases)
    {
        unload_compiler_phases();
    }

    timing_end(&timing_global);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Whole process took %.2f seconds to complete\n",
                timing_elapsed(&timing_global));
    }

    if (CURRENT_CONFIGURATION->debug_options.print_memory_report)
    {
        print_memory_report();
    }

    return compilation_process.execution_result;
}

static volatile char in_cleanup_routine = 0;

static void cleanup_routine(void)
{
    in_cleanup_routine = 1;
    // Switch to the command_line_configuration so we honour command line flags
    SET_CURRENT_CONFIGURATION(compilation_process.command_line_configuration);
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

// Basic initialization prior to argument parsing and configuration loading
static void driver_initialization(int argc, const char* argv[])
{
    atexit(cleanup_routine);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    // Define alternate stack
#if !defined (__CYGWIN__)
    // Cygwin does not have alternate stack
    stack_t alternate_stack;

    // Allocate a maximum of 1 Mbyte or more if MINSIGSTKSZ was
    // bigger than that (this is unlikely)
    int allocated_size = 1024 * 1024;
    if (MINSIGSTKSZ > 1024*1024)
    {
        allocated_size = MINSIGSTKSZ;
    }

    _alternate_signal_stack = malloc(allocated_size);

    alternate_stack.ss_flags = 0;
    alternate_stack.ss_size = allocated_size;
    alternate_stack.ss_sp = (void*)_alternate_signal_stack;

    if (alternate_stack.ss_sp == 0
            || sigaltstack(&alternate_stack, /* oss */ NULL) != 0)
    {
        running_error("Setting alternate signal stack failed (%s)\n",
                strerror(errno));
    }
#endif

    // Program signals
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    // Use alternate stack and we want the signal be reset when it happens
    terminating_sigaction.sa_flags = SA_RESETHAND;
#if !defined(__CYGWIN__)
    terminating_sigaction.sa_flags |= SA_ONSTACK;
#endif
    // Block all blockable signals while handling the termination
    sigfillset(&terminating_sigaction.sa_mask);

    int result = 0;
    result |= sigaction(SIGSEGV, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGQUIT, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGINT,  &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGTERM, &terminating_sigaction, /* old_sigaction */ NULL);
    result |= sigaction(SIGABRT, &terminating_sigaction, /* old_sigaction */ NULL);
    
    if (result != 0)
    {
        running_error("Signal programming failed with '%s'\n", strerror(errno));
    }
#endif

    memset(&compilation_process, 0, sizeof(compilation_process));
    compilation_process.argc = argc;

    // Copy argv strings
    compilation_process.argv = calloc(compilation_process.argc, sizeof(const char*));
    memcpy((void*)compilation_process.argv, argv, sizeof(const char*) * compilation_process.argc);

    // Original versions
    compilation_process.original_argc = argc;
    compilation_process.original_argv = calloc(compilation_process.argc, sizeof(const char*));
    memcpy((void*)compilation_process.original_argv, argv, sizeof(const char*) * compilation_process.argc);

    compilation_process.exec_basename = give_basename(argv[0]);

    // Find my own directory
    compilation_process.home_directory = find_home(argv[0]);
}

static void help_message(void)
{
    fprintf(stdout, "Usage: %s options file [file..]\n", compilation_process.argv[0]);
    fprintf(stdout, HELP_STRING);

    // We need to load the phases to show their help
    load_compiler_phases(CURRENT_CONFIGURATION);
    phases_help(CURRENT_CONFIGURATION);

    fprintf(stdout, "\n");
}

static void print_debug_flags_list(void)
{
    fprintf(stderr, "Debug flag list:\n\n");

    struct debug_flags_list_t** debug_flag_list = list_of_debug_flags();

    while (*debug_flag_list != NULL)
    {
        fprintf(stderr, " * %s\n", (*debug_flag_list)->name);
        fprintf(stderr, " %s\n\n", (*debug_flag_list)->description);
        debug_flag_list++;
    }
}

static void options_error(char* message)
{
    fprintf(stderr, "Error : %s\n", message);
    fprintf(stderr, "\n");
    exit(EXIT_FAILURE);
}



// Returns nonzero if an error happened. In that case we would show the help
// Messages issued here must be ended with \n for sthetic reasons
// if parse_implicits_only is true, then only implicit parameters are
// considered, all other should be ignored
int parse_arguments(int argc, const char* argv[], 
        char from_command_line,
        char parse_implicits_only)
{
    const char* output_file = NULL;

    // It is 1 because we ignore the first argument, since it is the name of
    // the program invocation
    int parameter_index = 1;

    // Flags -E/-y and -c are incompatible
    static char c_specified = 0;
    static char E_specified = 0;
    static char y_specified = 0;
    static char v_specified = 0;

    char native_verbose = 0;

    const char **input_files = NULL;
    int num_input_files = 0;

    char linker_files_seen = 0;

    struct command_line_parameter_t parameter_info;

    while (command_line_get_next_parameter(&parameter_index, 
                &parameter_info,
                SHORT_OPTIONS_STRING,
                command_line_long_options,
                argc, argv))
    {
        // An invalid option 
        // It might be -fXXX -mXXX options that we freely
        // allow for better compatibility with gcc
        if (parameter_info.flag == CLP_INVALID)
        {
            // This function should advance parameter_index if needed
            char used_flag = 0;

            int should_advance_1 = 0;
            used_flag |= !parse_implicit_parameter_flag(&should_advance_1, argv[parameter_index]);
            int should_advance_2 = 0;
            used_flag |= !parse_special_parameters(&should_advance_2, 
                    parameter_index, argv,
                    /* dry_run */ parse_implicits_only);

            if (!used_flag
                    && parse_implicits_only)
            {
                fprintf(stderr, "%s: parameter '%s' ignored\n", 
                        compilation_process.exec_basename,
                        argv[parameter_index]);
                // Advance one parameter if we do not know anything
                parameter_index++;
            }
            else
            {
                // Advance the maximum one
                int actual_advance = 
                    (should_advance_1 > should_advance_2) 
                    ? should_advance_1 
                    : should_advance_2;

                parameter_index += actual_advance;
            }
        }
        // A plain parameter (not under the hood of any option)
        else if (parameter_info.flag == CLP_PLAIN_PARAMETER)
        {
            if (parse_implicits_only)
                continue;

            if (!from_command_line)
            {
                fprintf(stderr, "%s: invalid non-option bound argument '%s'"
                        " specified in the configuration file\n",
                        compilation_process.exec_basename,
                        parameter_info.argument);
                continue;
            }
            // Ignore spurious parameters
            if ((parameter_info.argument != NULL)
                    && (strlen(parameter_info.argument) > 0))
            {
                // Be a bit smart here
                const char* extension = get_extension_filename(parameter_info.argument);
                if (extension == NULL 
                        || (fileextensions_lookup(extension, strlen(extension))) == NULL)
        {
            fprintf(stderr, "%s: file '%s' not recognized as a valid input. Passing verbatim on to the linker.\n", 
                    compilation_process.exec_basename,
                    parameter_info.argument);
            add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, parameter_info.argument);
            linker_files_seen = 1;
        }
                else
                {
                    P_LIST_ADD(input_files, num_input_files, parameter_info.argument);
                }
            }
        }
        // A known option
        else 
        {
            // Put here those flags that for some reason have special meanings
            // and at the same time they modify an implicit flag.
            // Currently only --no-openmp and --hlt behave this way
            char already_handled = 1;
            switch (parameter_info.value)
            {
                case OPTION_NO_OPENMP :
                    {
                        CURRENT_CONFIGURATION->disable_openmp = 1;
                        // If 'openmp' is in the parameter flags, set it to false, otherwise add it as false
                        int i;
                        char found = 0;
                        for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
                        {
                            if (strcmp(compilation_process.parameter_flags[i]->name, "openmp") == 0)
                            {
                                found = 1;
                                compilation_process.parameter_flags[i]->value = 0;
                            }
                        }
                        if (!found)
                        {
                            internal_error("'openmp' implicit flag was not properly registered", 0);
                        }
                        break;
                    }
                case OPTION_ENABLE_HLT :
                    {
                        CURRENT_CONFIGURATION->enable_hlt = 1;
                        // If 'hlt' is in the parameter flags, set it to false, otherwise add it as false
                        int i;
                        char found = 0;
                        for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
                        {
                            if (strcmp(compilation_process.parameter_flags[i]->name, "hlt") == 0)
                            {
                                found = 1;
                                compilation_process.parameter_flags[i]->value = 1;
                            }
                        }
                        if (!found)
                        {
                            internal_error("'hlt' implicit flag was not properly registered", 0);
                        }
                        break;
                    }
                default:
                    {
                        // Do nothing for this case, it is not an error
                        already_handled = 0;
                    }
            }

            // Do nothing else since we were already handled
            // otherwise the switch below will complain as if we had
            // forgotten to handle a known option
            if (already_handled)
                continue;

            // From now, ignore non-implicit flags
            if (parse_implicits_only)
                continue;

            // Put here all normal flags that do not require to be checked at implicit flag time
            switch (parameter_info.value)
            {
                case OPTION_VERSION : // --version
                    {
                        // Special case where nothing should be done
                        print_version();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_VERBOSE : // --verbose || --v
                    {
                        v_specified = 1;
                        CURRENT_CONFIGURATION->verbose = 1;
                        break;
                    }
                case 'v' : // Native compiler/Linker verbose
                    {
                        native_verbose = 1;
                        break;
                    }
                case 'k' : // --keep-files || -k
                    {
                        CURRENT_CONFIGURATION->keep_files = 1;
                        break;
                    }
                case 'K' : // --keep-all-files || -K
                    {
                        CURRENT_CONFIGURATION->keep_files = 1;
                        CURRENT_CONFIGURATION->keep_temporaries = 1;
                        break;
                    }
                case 'c' : // -c
                    {
                        if (y_specified || E_specified)
                        {
                            fprintf(stderr, "%s: parameter -c cannot be used together with -E or -y\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        c_specified = 1;

                        CURRENT_CONFIGURATION->do_not_link = 1;
                        break;
                    }
                case 'E' : // -E
                    {
                        if (c_specified || y_specified)
                        {
                            fprintf(stderr, "%s: parameter -E cannot be used together with -c or -y\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        E_specified = 1;

                        CURRENT_CONFIGURATION->do_not_compile = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                        CURRENT_CONFIGURATION->do_not_parse = 1;
                        break;
                    }
                case 'y' : // -y
                    {
                        if (c_specified || E_specified)
                        {
                            fprintf(stderr, "%s: parameter -y cannot be used together with -c or -E\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }

                        y_specified = 1;

                        CURRENT_CONFIGURATION->do_not_compile = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                        break;
                    }
                case OPTION_CONFIG_FILE :
                case OPTION_CONFIG_DIR:
                    {
                        // These options are handled in "load_configuration"
                        // and ignored here
                        break;
                    }
                case 'o' :
                    {
                        if (output_file != NULL)
                        {
                            fprintf(stderr, "%s: output file specified twice\n",
                                    compilation_process.exec_basename);
                            return 1;
                        }
                        else
                        {
                            if ((num_input_files > 1) 
                                    && c_specified)
                            {
                                fprintf(stderr, "%s: cannot specify -o when -c once given more than one file",
                                        compilation_process.exec_basename);
                                return 1;
                            }
                            output_file = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case 'I' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-I%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
#ifdef FORTRAN_SUPPORT
                        P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                                uniquestr(parameter_info.argument));
#endif
                        break;
                    }
                case 'J':
                    {
#ifdef FORTRAN_SUPPORT
                        if (CURRENT_CONFIGURATION->module_out_dir != NULL)
                        {
                            fprintf(stderr, "warning: -J flag passed more than once. This will overwrite the first directory\n");
                        }
                        P_LIST_ADD(CURRENT_CONFIGURATION->module_dirs, CURRENT_CONFIGURATION->num_module_dirs,
                                uniquestr(parameter_info.argument));
                        CURRENT_CONFIGURATION->module_out_dir = uniquestr(parameter_info.argument);
#else
                        running_error("Option -J is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                case 'L' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-L%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, temp);
                        break;
                    }
                case 'l' : 
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-l%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, temp);
                        break;
                    }
                case 'D' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-D%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
                        break;
                    }
                case 'U' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-U%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, temp);
                        break;
                    }
                case 'g' :
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, "-g");
                        break;
                    }
                case 'x' :
                    {
                        if (strcasecmp(parameter_info.argument, "C") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_C;
                        }
                        else if (strcasecmp(parameter_info.argument, "C++") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_CXX;
                        }
                        else if (strcasecmp(parameter_info.argument, "FORTRAN") == 0)
                        {
                            CURRENT_CONFIGURATION->force_language = 1;
                            CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_FORTRAN;
                        }
                        else
                        {
                            fprintf(stderr, "%s: invalid language specification in -x, valid options are 'C', 'C++' or 'FORTRAN'. Ignoring\n",
                                    compilation_process.exec_basename);
                        }

                        break;
                    }
                case OPTION_PREPROCESSOR_NAME :
                    {
                        CURRENT_CONFIGURATION->preprocessor_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_NATIVE_COMPILER_NAME :
                    {
                        CURRENT_CONFIGURATION->native_compiler_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_LINKER_NAME :
                    {
                        CURRENT_CONFIGURATION->linker_name = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_DEBUG_FLAG :
                    {
                        enable_debug_flag(uniquestr(parameter_info.argument));
                        break;
                    }
                case OPTION_OUTPUT_DIRECTORY :
                    {
                        CURRENT_CONFIGURATION->output_directory = uniquestr(parameter_info.argument);
                        break;
                    }
                case OPTION_HELP_DEBUG_FLAGS :
                    {
                        print_debug_flags_list();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_HELP_TARGET_OPTIONS:
                    {
                        print_help_target_options();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_PROFILE :
                    {
                        break;
                    }
                case OPTION_EXTERNAL_VAR :
                    {
                        if (strchr(parameter_info.argument, ':') == NULL)
                        {
                            fprintf(stderr, "%s: external variable '%s' definition is missing a colon. It will be ignored\n",
                                    compilation_process.exec_basename,
                                    parameter_info.argument);
                            break;
                        }

                        char temp[256];
                        strncpy(temp, parameter_info.argument, 255);
                        temp[255] = '\0';

                        char* name = temp;
                        char* value = strchr(name, ':');
                        *value = '\0';
                        value++;

                        external_var_t* new_external_var = calloc(1, sizeof(*new_external_var));

                        new_external_var->name = uniquestr(name);
                        new_external_var->value = uniquestr(value);

                        P_LIST_ADD(CURRENT_CONFIGURATION->external_vars, CURRENT_CONFIGURATION->num_external_vars,
                                new_external_var);

                        break;
                    }
                case OPTION_TYPECHECK :
                    {
                        CURRENT_CONFIGURATION->strict_typecheck = 1;
                        break;
                    }
                case OPTION_PREPROCESSOR_USES_STDOUT :
                    {
                        CURRENT_CONFIGURATION->preprocessor_uses_stdout = 1;
                        break;
                    }
                case OPTION_DISABLE_GXX_TRAITS:
                    {
                        CURRENT_CONFIGURATION->disable_gxx_type_traits = 1;
                        break;
                    }
                case OPTION_PASS_THROUGH:
                    {
                        CURRENT_CONFIGURATION->pass_through = 1;
                        // Otherwise we will wipe files that might be being modified
                        CURRENT_CONFIGURATION->keep_files = 1;
                        break;
                    }
                case OPTION_DISABLE_SIZEOF:
                    {
                        CURRENT_CONFIGURATION->disable_sizeof = 1;
                        fprintf(stderr, "%s: option '--disable-sizeof' should be used only to work around problems. Please, report a bug.\n",
                                compilation_process.exec_basename);
                        break;
                    }
                case OPTION_SET_ENVIRONMENT:
                    {
                        type_environment_t * chosen_env = get_environment(parameter_info.argument);
                        if (chosen_env != NULL)
                        {
                            CURRENT_CONFIGURATION->type_environment = chosen_env;
                        }
                        break;
                    }
                case OPTION_LIST_ENVIRONMENTS:
                    {
                        list_environments();
                        break;
                    }
                case 'h' :
                    {
                        show_help_message = 1;
                        return 1;
                    }
                case OPTION_PRINT_CONFIG_FILE:
                    {
                        printf("Default config file: %s%s\n", compilation_process.home_directory, CONFIG_RELATIVE_PATH);
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_PRINT_CONFIG_DIR:
                    {
                        printf("Default config directory: %s%s\n", compilation_process.home_directory, DIR_CONFIG_RELATIVE_PATH);
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case OPTION_ENABLE_UPC :
                    {
                        CURRENT_CONFIGURATION->enable_upc = 1;
                        if (parameter_info.argument != NULL)
                        {
                            fprintf(stderr, "%s: UPC static THREADS=%s\n", 
                                    compilation_process.exec_basename,
                                    parameter_info.argument);
                            CURRENT_CONFIGURATION->upc_threads = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case OPTION_ENABLE_CUDA:
                    {
                        CURRENT_CONFIGURATION->enable_cuda = 1;
                        break;
                    }
                case OPTION_DO_NOT_UNLOAD_PHASES:
                    {
                        do_not_unload_phases = 1;
                        break;
                    }
                case OPTION_INSTANTIATE_TEMPLATES:
                    {
                        CURRENT_CONFIGURATION->explicit_instantiation = 1;
                        break;
                    }
                case OPTION_ALWAYS_PREPROCESS:
                    {
                        if (parameter_info.argument == NULL
                                || strcmp(parameter_info.argument, "on") == 0)
                        {
                            CURRENT_CONFIGURATION->force_source_kind &= ~SOURCE_KIND_PREPROCESSED;
                            CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_NOT_PREPROCESSED;
                        }
                        else if (strcmp(parameter_info.argument, "off") == 0)
                        {
                            CURRENT_CONFIGURATION->force_source_kind &= ~SOURCE_KIND_NOT_PREPROCESSED;
                            CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_PREPROCESSED;
                        }
                        else
                        {
                            fprintf(stderr, "Invalid value given for --pp option, valid values are 'on' or 'off'\n");
                        }
                        break;
                    }
                case OPTION_DISABLE_INTRINSICS:
                    {
#ifdef FORTRAN_SUPPORT
                        CURRENT_CONFIGURATION->disable_intrinsics = 1;
#else
                        running_error("Option --width is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                case OPTION_FORTRAN_COLUMN_WIDTH:
                    {
#ifdef FORTRAN_SUPPORT
                        CURRENT_CONFIGURATION->column_width = atoi(parameter_info.argument);
#else
                        running_error("Option --width is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                case OPTION_FORTRAN_FIXED:
                    {
#ifdef FORTRAN_SUPPORT
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FIXED_FORM;
#else
                        running_error("Option --fixed is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                case OPTION_FORTRAN_FREE:
                    {
#ifdef FORTRAN_SUPPORT
                        CURRENT_CONFIGURATION->force_source_kind |= SOURCE_KIND_FREE_FORM;
#else
                        running_error("Option --free is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                case OPTION_EMPTY_SENTINELS:
                    {
#ifndef FORTRAN_SUPPORT
                        running_error("Option --sentinels is only valid when Fortran is enabled\n", 0);
#else
                        if (strcasecmp(parameter_info.argument, "on") == 0)
                        {
                            CURRENT_CONFIGURATION->disable_empty_sentinels = 0;
                        }
                        else if (strcasecmp(parameter_info.argument, "off") == 0)
                        {
                            CURRENT_CONFIGURATION->disable_empty_sentinels = 1;
                        }
                        else
                        {
                            fprintf(stderr, "Option --sentinels requires a value of 'on' or 'off'. Ignoring\n");
                        }
#endif
                        break;
                    }
                case OPTION_NODECL:
                    {
                        CURRENT_CONFIGURATION->enable_nodecl = 1;
                        break;
                    }
                case OPTION_FORTRAN_PRESCANNER:
                    {
#ifdef FORTRAN_SUPPORT
                        CURRENT_CONFIGURATION->prescanner_name = uniquestr(parameter_info.argument);
#else
                        running_error("Option --fpc is only valid when Fortran is enabled\n", 0);
#endif
                        break;
                    }
                default:
                    {
                        internal_error("Unhandled known option\n", 0);
                    }
            }
        }
    }

    if (!from_command_line
            || parse_implicits_only)
    {
        return 0;
    }

    if (num_input_files == 0
            && !linker_files_seen
            && !v_specified
            && !native_verbose
            && !CURRENT_CONFIGURATION->do_not_process_files)
    {
        fprintf(stderr, "%s: you must specify an input file\n", compilation_process.exec_basename);
        return 1;
    }

    if (num_input_files == 0
            && !linker_files_seen
            && !native_verbose
            && v_specified)
    {
        // -v has been given with nothing else
        print_version();
        exit(EXIT_SUCCESS);
    }

    // "-o -" is not valid when compilation or linking will be done
    if (output_file != NULL
            && (strcmp(output_file, "-") == 0)
            && !E_specified
            && !y_specified
            && !CURRENT_CONFIGURATION->do_not_process_files)
    {
        fprintf(stderr, "%s: specifying stdout by means of '-o -' is only valid with -y or -E\n", compilation_process.exec_basename);
        return 1;
    }

    // If -E has been specified and no output file has been, assume it is "-"
    if (output_file == NULL
            && (E_specified || y_specified)
            && !CURRENT_CONFIGURATION->do_not_process_files)
                        // Do not process anything
    {
        fprintf(stderr, "%s: assuming stdout as default output since -E has been specified\n", compilation_process.exec_basename);
        output_file = uniquestr("-");
    }

    // When -c and -o are given only one file is valid
    if (output_file != NULL
            && c_specified
            && (num_input_files > 1))
    {
        fprintf(stderr, "%s: cannot specify -o and -c with multiple files (second file '%s')", 
                compilation_process.exec_basename,
                argv[(parameter_index - 1)]);
        return 1;
    }

    if (CURRENT_CONFIGURATION->do_not_process_files)
    {
        // Neither do preprocessign nor compilation, just linking process without any files
        CURRENT_CONFIGURATION->do_not_parse = 1;
        CURRENT_CONFIGURATION->do_not_compile = 1;
        CURRENT_CONFIGURATION->do_not_prettyprint = 1;

        CURRENT_CONFIGURATION->do_not_link = 0;
        num_input_files = 0;
        output_file = NULL;
    }

    // This is the right place to sign in input files, never before of complete
    // command parsing
    int i;
    for (i = 0; i < num_input_files; i++)
    {
        add_new_file_to_compilation_process(/* add to the global file process */ NULL,
                input_files[i], output_file, CURRENT_CONFIGURATION);
    }

    // If some output was given by means of -o and we are linking (so no -c neither -E nor -y)
    // then, this output is the overall compilation process output
    if (output_file != NULL)
    {
        if (!CURRENT_CONFIGURATION->do_not_link)
        {
            CURRENT_CONFIGURATION->linked_output_filename = output_file;
        }
    }

    if (native_verbose)
    {
        const char* minus_v = uniquestr("-v");
        if (CURRENT_CONFIGURATION->do_not_link)
        {
            add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, minus_v);
        }
        else
        {
            add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, minus_v);
        }
    }

    return 0;
}

static void add_parameter_all_toolchain(const char *argument, char dry_run)
{
    if (!dry_run)
    {
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->native_compiler_options, argument);
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->linker_options, argument);
    }
}

static int parse_implicit_parameter_flag(int * should_advance, const char *parameter_flag)
{
    // Check whether this flag is of the implicitly registered flags 
    // because of the configuration
    int failure = 0;

    // Always advance one
    *should_advance = 1;

    const char *p = parameter_flag;

    char negative_flag = 0;

    if (strlen(parameter_flag) <= 1)
    {
        failure = 1;
    }
    else
    {
        // Check that this actually starts with '-'
        if (*p != '-')
        {
            failure = 1;
        }
        else
        {
            p++;

            // Allow a second optional '-'
            if (*p == '-')
            {
                p++;
            }

            // Check if this is a negative flag
            char * no_prefix = "no-";
            unsigned int length_of_no_prefix = strlen(no_prefix);

            if ((strlen(p) >= length_of_no_prefix)
                    && (strncmp(p, no_prefix, length_of_no_prefix) == 0))
            {
                negative_flag = 1;
                p += length_of_no_prefix;
            }

            // Now check parameter flags
            int i;
            char found = 0;
            for (i = 0; !found && (i < compilation_process.num_parameter_flags); i++)
            {
                struct parameter_flags_tag *parameter_flag = compilation_process.parameter_flags[i];
                if (strcmp(parameter_flag->name, p) == 0)
                {
                    found = 1;
                    if (!negative_flag)
                    {
                        parameter_flag->value = 1;
                    }
                    else
                    {
                        parameter_flag->value = 0;
                    }
                }
            }

            if (!found)
            {
                failure = 1;
            }
        }
    }

    return failure;
}

static char strprefix(const char* str, const char *prefix)
{
    if (strlen(prefix) > strlen(str))
        return 0;
    return (strncmp(str, prefix, strlen(prefix)) == 0);
}

static int parse_special_parameters(int *should_advance, int parameter_index,
        const char* argv[], char dry_run)
{
    // If dry_run is true do not change any configuration!  just make what is
    // needed to advance along the parameters
    // FIXME: This function should use gperf-ectionated
    // This code can be written better
    int failure = 0;

    const char *argument = argv[parameter_index];

    // argument[0] == '-'
    switch (argument[1])
    {
        // GCC parameters
        case 'n' :
            {
                if (strcmp(argument, "-nostdlib") == 0
                        || strcmp(argument, "-nostdinc") == 0
                        || strcmp(argument, "-nostdinc++") == 0)
                {
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'f':
        case 'm':
            {
                add_parameter_all_toolchain(argument, dry_run);
                if (!dry_run)
                {
                    if (strcmp(argument, "-fshort-enums") == 0)
                    {
                        CURRENT_CONFIGURATION->code_shape.short_enums = 1;
                    }
                }
                (*should_advance)++;
                break;
            }
        case 'd':
            {
                if (strcmp(argument, "-dumpspecs") == 0
                        || strcmp(argument, "-dumpversion") == 0
                        || strcmp(argument, "-dumpmachine") == 0)
                {
                    if (!dry_run)
                    {
                        CURRENT_CONFIGURATION->do_not_process_files = 1;
                        add_parameter_all_toolchain(argument, dry_run);
                    }
                    (*should_advance)++;
                }
                else if (strlen(argument) == 3 // -dX
                    && (argument[2] == 'A'
                        || argument[2] == 'D'
                        || argument[2] == 'H'
                        || argument[2] == 'm'
                        || argument[2] == 'p'
                        || argument[2] == 'P'
                        || argument[2] == 'v'
                        || argument[2] == 'x'
                        || argument[2] == 'y'))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'S' :
            {
                if (strlen(argument) == 2) // -S
                {
                    // This disables linking
                    if (!dry_run)
                    {
                        CURRENT_CONFIGURATION->generate_assembler = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                    }
                    (*should_advance)++;
                }
                break;
            }
        case 'M' :
            {
                if ((argument[2] == '\0') // -M
                        || ((argument[2] == 'P') && (argument[3] == '\0')) // -MP
                        || ((argument[2] == 'D') && (argument[3] == '\0')) // -MD
                        || ((argument[2] == 'M') && (argument[3] == '\0')) // -MM
                        || ((argument[2] == 'M') && (argument[3] == 'D') && (argument[4] == '\0'))) // -MMD
                {
                    // -M and -MM are special since they disable 
                    // mcxx parsing, native compilation and linking
                    // since it is more of a preprocessor 'affair' 
                    if (strcmp(&argument[1], "M") == 0
                            || strcmp(&argument[1], "MM") == 0)
                    {
                        if (!dry_run)
                        {
                            CURRENT_CONFIGURATION->do_not_parse = 1;
                            CURRENT_CONFIGURATION->do_not_compile = 1;
                            CURRENT_CONFIGURATION->do_not_link = 1;
                        }
                    }

                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;
                }
                else if (((argument[2] == 'F') && (argument[3] == '\0')) // -MF
                        || ((argument[2] == 'G') && (argument[3] == '\0')) // -MG
                        || ((argument[2] == 'T') && (argument[3] == '\0'))) // -MT
                {
                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;

                    // Pass the next argument too
                    argument = argv[parameter_index + 1];
                    if (!dry_run)
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, argument);
                    }
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 's':
            {
                // -std=xx
                if ((strlen(argument) > strlen("-std="))
                        && argument[2] == 't'
                        && argument[3] == 'd'
                        && argument[4] == '=') 
                { 
                    if (strcmp(&argument[5], "c++0x") == 0
                            || strcmp(&argument[5], "gnu++0x") == 0)
                    {
                        CURRENT_CONFIGURATION->enable_cxx1x = 1;
                    }
                }
                else if (strcmp(argument, "-static") == 0) { }
                else if (strcmp(argument, "-shared") == 0) { }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }

                break;
            }
        case 'r' :
            {
                if (strcmp(argument, "-rdynamic") == 0) { }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'O' :
            {
                // Optimization is a special one since it can be -O or -Os -O0 -O1 -O2 -O3
                if (argument[2] != '\0'
                        && argument[2] != 's')
                {
                    char *error = NULL;
                    long int value = strtol(&(argument[2]), &error, 10);

                    if (*error != '\0'
                            || value < 0)
                    {
                        failure = 1;
                    }
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'e' :
            {
                if (strcmp(argument, "-export-dynamic") == 0)
                {
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'p':
            {
                if (strcmp(argument, "-pthread") == 0)
                {
                }
                else if ((strcmp(argument, "-print-search-dirs") == 0)
                        || (strcmp(argument, "-print-libgcc-file-name") == 0)
                        || (strcmp(argument, "-print-multi-directory") == 0)
                        || (strcmp(argument, "-print-multi-lib") == 0)
                        || (strcmp(argument, "-print-multi-os-directory") == 0)
                        || (strcmp(argument, "-print-multi-os-directory") == 0)
                        || (strcmp(argument, "-print-sysroot") == 0)
                        || (strcmp(argument, "-print-sysroot-headers-suffix") == 0)
                        )
                {
                    // Do not process anything
                    if (!dry_run)
                        CURRENT_CONFIGURATION->do_not_process_files = 1;
                }
                else if (strprefix(argument, "-print-prog-name")
                        || strprefix(argument, "-print-file-name"))
                {
                    // KLUDGE: -print-prog-name and -print-file-name have the same length
                    const char *p = argument + strlen("-print-prog-name");

                    if (*p == '=' 
                            && *(p+1) != '\0')
                    {
                        // Do not process anything
                        if (!dry_run)
                            CURRENT_CONFIGURATION->do_not_process_files = 1;
                    }
                    else
                    {
                        failure = 1;
                    }
                }
                else
                {
                    failure = 1;
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                break;
            }
        case 'W' :
            {
                if (strlen(argument) > strlen("-W"))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'w':
            {
                if (strlen(argument) == strlen("-w"))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case 'X' :
            {
                if ((strcmp(&argument[2], "preprocessor") == 0)
                        || (strcmp(&argument[2], "linker") == 0)
                        || (strcmp(&argument[2], "assembler") == 0))
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;

                    // Pass the next argument too
                    argument = argv[parameter_index + 1];
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }

                break;
            }
        case '-' :
        {
            if (argument[2] == 'W')
            {
                // Check it is of the form -W*,
                const char *p = strchr(&argument[2], ',');
                if (p != NULL)
                {
                    // Check there is something after the first comma ','
                    if (*(p+1) != '\0')
                    {
                        if (!dry_run)
                            parse_subcommand_arguments(&argument[3]);
                        (*should_advance)++;
                    }
                }
                break;
            }
            else
            {
                failure = 1;
            }
            break;
        }
        default:
            {
                failure = 1;
                break;
            }
    }

    return failure;
}

static void enable_debug_flag(const char* flags)
{
    int num_flags = 0;
    const char** flag_list = comma_separate_values(flags, &num_flags);

    int i;
    for (i = 0; i < num_flags; i++)
    {
        const char* flag = flag_list[i];

        struct debug_flags_list_t* flag_option = 
            debugflags_lookup (flag, strlen(flag));

        if (flag_option != NULL)
        {
            // *(flag_option->flag_pointer) = 1;
            *((char*)(&CURRENT_CONFIGURATION->debug_options) + flag_option->flag_offset) = 1;
        }
        else
        {
            fprintf(stderr, "%s: debug flag '%s' unknown. Ignoring it\n", 
                    compilation_process.exec_basename,
                    flag);
        }
    }
}

static void add_to_parameter_list_str(const char*** existing_options, const char* str)
{
    const char* d_str = uniquestr(str);
    add_to_parameter_list(existing_options, &d_str, 1);
}

void add_to_parameter_list(const char*** existing_options, const char **parameters, int num_parameters)
{
    int num_existing_options = count_null_ended_array((void**)(*existing_options));

    (*existing_options) = realloc((*existing_options), sizeof(char*)*(num_existing_options + num_parameters + 1));

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        (*existing_options)[num_existing_options + i] = parameters[i];
    }
    (*existing_options)[num_existing_options + i] = NULL;
}

static void parse_subcommand_arguments(const char* arguments)
{
    char prepro_flag = 0;
    char native_flag = 0;
    char linker_flag = 0;
#ifdef FORTRAN_SUPPORT
    char prescanner_flag = 0;
#endif

    compilation_configuration_t* configuration = CURRENT_CONFIGURATION;

    // We are understanding and we will assume --Wpp, is --Wp, 
    // likewise with --Wpnp, will be like --Wpn
    const char* p = arguments;

    if (*p == 'x')
    {
        // Advance 'x'
        p++;
        if (*p != ':')
        {
            options_error("Option --W is of the form --Wx, in this case the proper syntax is --Wx:profile-name:m,");
        }
        // Advance ':'
        p++;

#define MAX_PROFILE_NAME 256
        char profile_name[MAX_PROFILE_NAME] = { 0 };
        char* q = profile_name;

        while (*p != ':'
                && *p != '\0')
        {
            if ((q - profile_name) > MAX_PROFILE_NAME)
            {
                running_error("Profile name too long in option '--W%s'\n", arguments);
            }

            *q = *p;
            q++;
            p++;
        }
        *q = '\0';

        if (*p != ':')
        {
            options_error("Option --W is of the form --Wx, in this case the proper syntax is --Wx:profile-name:m,");
        }

        configuration = get_compilation_configuration(profile_name);

        if (configuration == NULL)
        {
            fprintf(stderr, "%s: no compiler configuration '%s' has been loaded, parameter '--W%s' will be ignored\n",
                    compilation_process.exec_basename,
                    profile_name, arguments);
            return;
        }

        // Advance ':'
        p++;
    }

    while (*p != '\0'
            && *p != ',')
    {
        switch (*p)
        {
            case 'p' : 
                prepro_flag = 1;
                break;
            case 'n' : 
                native_flag = 1;
                break;
            case 'l' : 
                linker_flag = 1;
                break;
#ifdef FORTRAN_SUPPORT
            case 's':
                prescanner_flag = 1;
                break;
#endif
            default:
                fprintf(stderr, "%s: invalid flag character %c for --W option only 'p', 'n', 's' or 'l' are allowed, ignoring\n",
                        compilation_process.exec_basename,
                        *p);
                break;
        }
        p++;
    }

    if (p == arguments)
    {
        options_error("Option --W is of the form '--W,' or '--W' and must be '--Wm,x'");
    }

    if (*p == '\0' || 
            *(p+1) == '\0')
    {
        options_error("Option --W is of the form '--Wm,' and must be '--Wm,x'");
    }

    // Advance ','
    p++;

    int num_parameters = 0;
    const char** parameters = comma_separate_values(p, &num_parameters);

    if (prepro_flag)
        add_to_parameter_list(
                &configuration->preprocessor_options,
                parameters, num_parameters);
    if (native_flag)
        add_to_parameter_list(
                &configuration->native_compiler_options,
                parameters, num_parameters);
    if (linker_flag)
        add_to_parameter_list(
                &configuration->linker_options,
                parameters, num_parameters);
#ifdef FORTRAN_SUPPORT
    if (prescanner_flag)
        add_to_parameter_list(
                &configuration->prescanner_options,
                parameters, num_parameters);
#endif
}

static compilation_configuration_t minimal_default_configuration;

// This functions initializes a minimal configuration and the default
// path to search the configuration file
static void initialize_default_values(void)
{
    int dummy = 0;
    // Initialize here all default values
    compilation_process.config_file = strappend(compilation_process.home_directory, CONFIG_RELATIVE_PATH);
    compilation_process.config_dir = strappend(compilation_process.home_directory, DIR_CONFIG_RELATIVE_PATH);
    compilation_process.num_translation_units = 0;

    // The minimal default configuration
    memset(&minimal_default_configuration, 0, sizeof(minimal_default_configuration));
    SET_CURRENT_CONFIGURATION(&minimal_default_configuration);

    if (default_environment == NULL)
    {
        default_environment = get_environment(DEFAULT_TYPE_ENVIRONMENT);

        if (default_environment == NULL)
        {
            internal_error("Invalid default environment", 0);
        }
    }

    CURRENT_CONFIGURATION->type_environment = default_environment;

    CURRENT_CONFIGURATION->source_language = SOURCE_LANGUAGE_CXX;

    CURRENT_CONFIGURATION->preprocessor_name = uniquestr("c++");
    CURRENT_CONFIGURATION->preprocessor_options = comma_separate_values(uniquestr("-E"), &dummy);

    CURRENT_CONFIGURATION->native_compiler_name = uniquestr("c++");
    CURRENT_CONFIGURATION->native_compiler_options = NULL;

    CURRENT_CONFIGURATION->linker_name = uniquestr("c++");
    CURRENT_CONFIGURATION->linker_options = NULL;

#ifdef FORTRAN_SUPPORT
    CURRENT_CONFIGURATION->column_width = 132;
#endif

    // Add openmp as an implicit flag
    struct parameter_flags_tag *new_parameter_flag = calloc(1, sizeof(*new_parameter_flag));

    new_parameter_flag->name = uniquestr("openmp");
    new_parameter_flag->value = 1;

    P_LIST_ADD(compilation_process.parameter_flags, 
            compilation_process.num_parameter_flags,
            new_parameter_flag);

    // Add hlt as an implicit flag
    new_parameter_flag = calloc(1, sizeof(*new_parameter_flag));

    new_parameter_flag->name = uniquestr("hlt");
    // This is redundant because of calloc, but make it explicit here anyway
    new_parameter_flag->value = 0;

    P_LIST_ADD(compilation_process.parameter_flags, 
            compilation_process.num_parameter_flags,
            new_parameter_flag);
}

static void print_version(void)
{
    fprintf(stdout, PACKAGE " " VERSION " (" MCXX_BUILD_VERSION ")\n");
    fprintf(stdout, "Configured with: %s\n", MCXX_CONFIGURE_ARGS);
}

static void load_configuration_file(const char *filename)
{
    config_file_parse(filename);
}

static void remove_parameter_from_argv(int i)
{
    int j;
    for (j = i; (j + 1) < compilation_process.argc; j++)
    {
        compilation_process.argv[j] = compilation_process.argv[j + 1];
    }
    compilation_process.argc--;
}


static void load_configuration(void)
{
    // Solve here the egg and chicken problem of the option --config-file
    int i;
    char restart = 1;

    // We will restart the scan whenever we remove an item from argv
    while (restart)
    {
        restart = 0;
        for (i = 1; i < compilation_process.argc; i++)
        {
            if (strncmp(compilation_process.argv[i], 
                        "--config-file=", strlen("--config-file=")) == 0)
            {
                const char *config_file = NULL;
                config_file = compilation_process.config_file = 
                    uniquestr(&(compilation_process.argv[i][strlen("--config-file=") ]));

                // Load the configuration file at this point should the user have
                // specified more than one config file
                load_configuration_file(config_file);

                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
            else if (strncmp(compilation_process.argv[i], 
                        "--config-dir=", strlen("--config-dir=")) == 0)
            {
                compilation_process.config_dir = 
                    uniquestr(&(compilation_process.argv[i][strlen("--config-dir=") ]));

                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
            else if (strncmp(compilation_process.argv[i], 
                        "--profile=", strlen("--profile=")) == 0)
            {
                // Change the basename, from now it will look like the compiler
                // has been called as this basename
                compilation_process.exec_basename =
                    uniquestr(&(compilation_process.argv[i][strlen("--profile=") ]));

                remove_parameter_from_argv(i);
                restart = 1;
                break;
            }
        }
    }

    // Now load all files in the config_dir
    DIR* config_dir = opendir(compilation_process.config_dir);
    if (config_dir == NULL)
    {
        if (errno == ENOENT)
        {
            // Only give an error if it does exist
            fprintf(stderr, "%s: could not open configuration directory '%s' (%s)\n", 
                    compilation_process.exec_basename,
                    compilation_process.config_dir,
                    strerror(errno));
        }
    }
    else
    {
        struct dirent *dir_entry;

        dir_entry = readdir(config_dir);
        while (dir_entry != NULL)
        {
            struct stat buf;
            memset(&buf, 0, sizeof(buf));

            // Ignore hidden files and backups of many editors
            if ((dir_entry->d_name[0] != '.')
                    && (dir_entry->d_name[strlen(dir_entry->d_name)-1] != '~'))
            {
                const char * full_path =
                    strappend(
                            strappend(
                                compilation_process.config_dir, DIR_SEPARATOR),
                            dir_entry->d_name);

                stat(full_path, &buf);
                if (S_ISREG(buf.st_mode))
                {
                    load_configuration_file(full_path);
                }
            }

            dir_entry = readdir(config_dir);
        }

        closedir(config_dir);
    }
    
    // Now set the configuration as stated by the basename
    SET_CURRENT_CONFIGURATION(NULL);
    SET_CURRENT_CONFIGURATION(get_compilation_configuration(compilation_process.exec_basename));

    if (CURRENT_CONFIGURATION == NULL)
    {
        fprintf(stderr, "%s: no suitable configuration defined for %s. Setting to C++ built-in configuration\n",
               compilation_process.exec_basename,
               compilation_process.exec_basename);
        SET_CURRENT_CONFIGURATION(&minimal_default_configuration);
    }
    
    compilation_process.command_line_configuration = CURRENT_CONFIGURATION;
}


static void commit_configuration(void)
{
    // For every configuration commit its options depending on flags
    int i;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        struct compilation_configuration_tag* configuration 
            = compilation_process.configuration_set[i];

        // First preinitialize it with the base information (if any). Since
        // they have been introduced in order, bases have been
        // initialized/committed before
        initialize_with_base(configuration);

        int j;
        for (j = 0; j < configuration->num_configuration_lines; j++)
        {
            struct compilation_configuration_line* configuration_line = configuration->configuration_lines[j];

            struct configuration_directive_t* config_directive =
                configoptions_lookup(configuration_line->name, strlen(configuration_line->name));

            if (config_directive == NULL)
            {
                fprintf(stderr, "%s: configuration directive '%s' skipped since it is unknown\n", 
                        compilation_process.exec_basename,
                        configuration_line->name);
                continue;
            }

            // Check the value of flags before processing this configuration line
            char can_be_committed = (configuration_line->flag_expr == NULL)
                || (flag_expr_eval(configuration_line->flag_expr));

            if (can_be_committed)
            {
                config_directive->funct(configuration, configuration_line->index, configuration_line->value);
            }
        }
    }

    DEBUG_CODE()
    {
        if (!CURRENT_CONFIGURATION->disable_sizeof)
        {
            fprintf(stderr, "DRIVER: Using type environment '%s' for type size calculation\n",
                    CURRENT_CONFIGURATION->type_environment->environ_name);
        }
    }
}

static void register_upc_pragmae(void);
static void enable_hlt_phase(void);

static void finalize_committed_configuration(void)
{
    // OpenMP support involves omp pragma
    if (!CURRENT_CONFIGURATION->disable_openmp)
    {
        config_add_preprocessor_prefix(CURRENT_CONFIGURATION, /* index */ NULL, "omp");
    }
    else
    {
#ifdef FORTRAN_SUPPORT
        // Disable empty sentinels
        CURRENT_CONFIGURATION->disable_empty_sentinels = 1;
#endif
    }

    // UPC support involves some specific pragmae
    if (CURRENT_CONFIGURATION->enable_upc)
    {
        register_upc_pragmae();
    }

    // HLT additional support
    if (CURRENT_CONFIGURATION->enable_hlt)
    {
        enable_hlt_phase();
    }
}

static void enable_hlt_phase(void)
{
    // -hlt is like adding the compiler phase of hlt and registering '#pragma hlt'
    // Register '#pragma hlt'
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, /* index */ NULL, "hlt");

    add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, "-D_MERCURIUM_HLT");

    // When loading the compiler phase a proper extension will be added
    const char* library_name = "libtl-hlt-pragma";
	compiler_phase_loader_t* cl = calloc(1, sizeof(*cl));
	cl->func = compiler_phase_loader;
	cl->data = (void*)uniquestr(library_name);
    P_LIST_ADD_PREPEND(CURRENT_CONFIGURATION->phase_loader, 
            CURRENT_CONFIGURATION->num_compiler_phases,
			cl);

    /*P_LIST_ADD_PREPEND(CURRENT_CONFIGURATION->compiler_phases, 
            CURRENT_CONFIGURATION->num_compiler_phases, 
            library_name);*/
}

// FIXME: This should be in cxx-upc.c, but that file belongs to the frontend
// where we cannot call driver functions, so we will implement here
// maybe a better file to put it would be cxx-upc-driver.c
static void register_upc_pragmae(void)
{
    // Register '#pragma upc'
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, /* index */ NULL, "upc");
    // Lexer already uses CURRENT_CONFIGURATION this is why it is not specified here
    // Register '#pragma upc relaxed'
    register_new_directive("upc", "relaxed", /* is_construct */ 0, /* bound_to_single_stmt */ 0);
    // Register '#pragma upc strict'
    register_new_directive("upc", "strict", /* is_construct */ 0, /* bound_to_single_stmt */ 0);

    // mfarrera's + IBM UPC extension that annoyingly it is not prefixed with
    // 'upc' (as it ought to be!)
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, /* index */ NULL, "distributed");
    // Register the empty directive since the syntax is '#pragma distributed'
    register_new_directive("distributed", "", /* is_construct */ 0, /* bound_to_single_stmt */ 0);
}

static void compile_every_translation_unit_aux_(int num_translation_units,
        compilation_file_process_t** translation_units)
{
    // This is just to avoid having a return in this function by mistake
#define return 1 = 1;
    // Save the old current file
    compilation_file_process_t* saved_file_process = CURRENT_FILE_PROCESS;
    compilation_configuration_t* saved_configuration = CURRENT_CONFIGURATION;

    int i;
    for (i = 0; i < num_translation_units; i++)
    {
        compilation_file_process_t* file_process = translation_units[i];

        // Ensure we do not get in a strange loop
        if (file_process->already_compiled)
            continue;

        // Note: This is the only place where
        // CURRENT_{FILE_PROCESS,CONFIGURATION} can be changed. Everywhere else
        // these two variables are constants. 
        // Whenever you modify SET_CURRENT_FILE_PROCESS update also
        // SET_CURRENT_CONFIGURATION to its configuration
        SET_CURRENT_FILE_PROCESS(file_process);
        // This looks a bit redundant but it turns that the compiler has a
        // configuration even before of any file
        SET_CURRENT_CONFIGURATION(file_process->compilation_configuration);

        translation_unit_t* translation_unit = CURRENT_COMPILED_FILE;

        // Ensure phases are loaded for current profile
        load_compiler_phases(CURRENT_CONFIGURATION);

        // First check the file type
        const char* extension = get_extension_filename(translation_unit->input_filename);

        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        // Linker data is not processed anymore
        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
        {
            file_process->already_compiled = 1;
            continue;
        }

#ifndef FORTRAN_SUPPORT
        if (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN)
        {
            running_error("%s: sorry: Fortran support not enabled", 
                    translation_unit->input_filename);
        }
#endif

        if (!CURRENT_CONFIGURATION->force_language
                && (current_extension->source_language != CURRENT_CONFIGURATION->source_language)
                && (!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PARSED)))
        {
            fprintf(stderr, "%s: %s was configured for %s language but file '%s' looks %s language (it will be compiled anyways)\n",
                    compilation_process.exec_basename,
                    compilation_process.exec_basename, 
                    source_language_names[CURRENT_CONFIGURATION->source_language],
                    translation_unit->input_filename,
                    source_language_names[current_extension->source_language]);
        }

        char old_cuda_flag = CURRENT_CONFIGURATION->enable_cuda;
        // For cuda enable CUDA
        if (current_extension->source_language == SOURCE_LANGUAGE_CUDA)
        {
            if (!old_cuda_flag)
            {
                fprintf(stderr, "%s: info: enabling experimental CUDA support\n",
                        translation_unit->input_filename);
                CURRENT_CONFIGURATION->enable_cuda = 1;
            }
        }

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Compiling file '%s'\n", translation_unit->input_filename);
        }

        const char* parsed_filename = translation_unit->input_filename;
        // If the file is not preprocessed or we've ben told to preprocess it
        if (((BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PREPROCESSED)
                    || BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_NOT_PREPROCESSED))
                    && !BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_PREPROCESSED))
                && !CURRENT_CONFIGURATION->pass_through)
        {
            timing_t timing_preprocessing;

            timing_start(&timing_preprocessing);
            parsed_filename = preprocess_file(translation_unit, translation_unit->input_filename);
            timing_end(&timing_preprocessing);

            if (parsed_filename != NULL
                    && CURRENT_CONFIGURATION->verbose)
            {
                fprintf(stderr, "File '%s' preprocessed in %.2f seconds\n",
                        translation_unit->input_filename, 
                        timing_elapsed(&timing_preprocessing));
            }

            if (parsed_filename == NULL)
            {
                running_error("Preprocess failed for file '%s'", translation_unit->input_filename);
            }
        }

#ifdef FORTRAN_SUPPORT
        if (current_extension->source_language == SOURCE_LANGUAGE_FORTRAN
                && (BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_FIXED_FORM)
                    || BITMAP_TEST(CURRENT_CONFIGURATION->force_source_kind, SOURCE_KIND_FIXED_FORM))
                && !CURRENT_CONFIGURATION->pass_through)
        {
            timing_t timing_prescanning;
            
            timing_start(&timing_prescanning);
            parsed_filename = fortran_prescan_file(translation_unit, parsed_filename);
            timing_end(&timing_prescanning);

            if (parsed_filename != NULL
                    && CURRENT_CONFIGURATION->verbose)
            {
                fprintf(stderr, "File '%s' converted from fixed to free form in %.2f seconds\n",
                        parsed_filename,
                        timing_elapsed(&timing_prescanning));
            }

            if (parsed_filename == NULL)
            {
                running_error("Conversion from fixed Fortran form to free Fortran form failed for file '%s'\n",
                        translation_unit->input_filename);
            }
        }
#endif

        if (!CURRENT_CONFIGURATION->do_not_parse)
        {
            if (!CURRENT_CONFIGURATION->pass_through
                    && (!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PARSED)))
            {
                // 0. Do this before open for scan since we might to internally parse some sources
                mcxx_flex_debug = mc99_flex_debug = CURRENT_CONFIGURATION->debug_options.debug_lexer;
                mcxxdebug = mc99debug = CURRENT_CONFIGURATION->debug_options.debug_parser;
#ifdef FORTRAN_SUPPORT
                mf03_flex_debug = CURRENT_CONFIGURATION->debug_options.debug_lexer;
                mf03debug = CURRENT_CONFIGURATION->debug_options.debug_parser;
#endif

                initialize_semantic_analysis(translation_unit, parsed_filename);

                // 1. Open file
                CXX_LANGUAGE()
                {
                    if (mcxx_open_file_for_scanning(parsed_filename, translation_unit->input_filename) != 0)
                    {
                        running_error("Could not open file '%s'", parsed_filename);
                    }
                }

                C_LANGUAGE()
                {
                    if (mc99_open_file_for_scanning(parsed_filename, translation_unit->input_filename) != 0)
                    {
                        running_error("Could not open file '%s'", parsed_filename);
                    }
                }

#ifdef FORTRAN_SUPPORT
                FORTRAN_LANGUAGE()
                {
                    if (mf03_open_file_for_scanning(parsed_filename, translation_unit->input_filename) != 0)
                    {
                        running_error("Could not open file '%s'", parsed_filename);
                    }
                }
#endif

                // 2. Parse file
                parse_translation_unit(translation_unit, parsed_filename);
                // 3. Close file
                close_scanned_file();

                // 4. TL::pre_run
                compiler_phases_pre_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // 5. Semantic analysis
                semantic_analysis(translation_unit, parsed_filename);

                if (CURRENT_CONFIGURATION->enable_nodecl)
                {
                    AST simplified_tree = NULL;
                    if (IS_C_LANGUAGE
                            || IS_CXX_LANGUAGE)
                    {
                        c_simplify_tree_translation_unit(translation_unit->parsed_tree, &simplified_tree);
                    }
#ifdef FORTRAN_SUPPORT
                    else if (IS_FORTRAN_LANGUAGE)
                    {
                        fortran_simplify_tree_translation_unit(translation_unit->parsed_tree, &simplified_tree);
                    }
#endif
                    else
                    {
                        internal_error("Invalid language", 0);
                    }

                    ast_dump_graphviz(simplified_tree, stdout);
                }

                // 6. TL::run and TL::phase_cleanup
                compiler_phases_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // 7. print ast if requested
                if (CURRENT_CONFIGURATION->debug_options.print_ast_graphviz)
                {
                    fprintf(stderr, "Printing AST in graphviz format\n");

                    ast_dump_graphviz(translation_unit->parsed_tree, stdout);
                }
                else if (CURRENT_CONFIGURATION->debug_options.print_ast_html)
                {
                    fprintf(stderr, "Printing AST in HTML format\n");
                    ast_dump_html(translation_unit->parsed_tree, stdout);
                }

                // 8. print symbol table if requested
                if (CURRENT_CONFIGURATION->debug_options.print_scope)
                {
                    fprintf(stderr, "============ SYMBOL TABLE ===============\n");
                    print_scope(translation_unit->global_decl_context);
                    fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
                }
            }


            const char* prettyprinted_filename = NULL;
            if (!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PARSED))
            {
                prettyprinted_filename
                    = prettyprint_translation_unit(translation_unit, parsed_filename);
            }

            // Process secondary translation units
            if (file_process->num_secondary_translation_units != 0)
            {
                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "\nThere are secondary translation units for '%s'. Processing.\n",
                            translation_unit->input_filename);
                }
                compile_every_translation_unit_aux_(
                        file_process->num_secondary_translation_units,
                        file_process->secondary_translation_units);

                if (CURRENT_CONFIGURATION->verbose)
                {
                    fprintf(stderr, "All secondary translation units of '%s' have been processed\n\n",
                            translation_unit->input_filename);
                }
            }

            if (!BITMAP_TEST(current_extension->source_kind, SOURCE_KIND_NOT_PARSED))
            {
                native_compilation(translation_unit, prettyprinted_filename, /* remove_input */ 1);
            }
            else
            {
                // Do not parse
                native_compilation(translation_unit, translation_unit->input_filename, /* remove_input */ 0);
            }
        }

        // Restore CUDA flag
        CURRENT_CONFIGURATION->enable_cuda = old_cuda_flag;

        file_process->already_compiled = 1;
    }

    // Recover previous information
    SET_CURRENT_FILE_PROCESS(saved_file_process);
    SET_CURRENT_CONFIGURATION(saved_configuration);
#undef return
}

static void compile_every_translation_unit(void)
{
    compile_every_translation_unit_aux_(compilation_process.num_translation_units,
            compilation_process.translation_units);
}

static void compiler_phases_pre_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit,
        const char* parsed_filename UNUSED_PARAMETER)
{
    timing_t time_phases;
    timing_start(&time_phases);

    start_compiler_phase_pre_execution(config, translation_unit);

    timing_end(&time_phases);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Early compiler phases pipeline executed in %.2f seconds\n", timing_elapsed(&time_phases));
    }
}

static void compiler_phases_execution(
        compilation_configuration_t* config,
        translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    timing_t time_phases;
    timing_start(&time_phases);

    start_compiler_phase_execution(config, translation_unit);

    timing_end(&time_phases);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Compiler phases pipeline executed in %.2f seconds\n", timing_elapsed(&time_phases));
    }
}

static void parse_translation_unit(translation_unit_t* translation_unit, const char* parsed_filename)
{
    timing_t timing_parsing;

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Parsing file '%s' ('%s')\n", 
                translation_unit->input_filename, parsed_filename);
    }

    timing_start(&timing_parsing);

    AST parsed_tree = NULL;

    int parse_result = 0;
    CXX_LANGUAGE()
    {
        parse_result = mcxxparse(&parsed_tree);
    }

    C_LANGUAGE()
    {
        parse_result = mc99parse(&parsed_tree);
    }

#ifdef FORTRAN_SUPPORT
    FORTRAN_LANGUAGE()
    {
        parse_result = mf03parse(&parsed_tree);
    }
#endif

    if (parse_result != 0)
    {
        running_error("Compilation failed for file '%s'\n", translation_unit->input_filename);
    }

    // Store the parsed tree as the unique child of AST_TRANSLATION_UNIT
    // initialized in function initialize_semantic_analysis
    ast_set_child(translation_unit->parsed_tree, 0, parsed_tree);

    timing_end(&timing_parsing);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') parsed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_parsing));
    }

}

static AST get_translation_unit_node(void)
{
    return ASTMake1(AST_TRANSLATION_UNIT, NULL, NULL, 0, NULL);
}

static void initialize_semantic_analysis(translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    // This one is implemented in cxx-buildscope.c
    translation_unit->parsed_tree = get_translation_unit_node();
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        c_initialize_translation_unit_scope(translation_unit);
    }
#ifdef FORTRAN_SUPPORT
    else if (IS_FORTRAN_LANGUAGE)
    {
        fortran_initialize_translation_unit_scope(translation_unit);
    }
#endif
    else
    {
        internal_error("Invalid language", 0);
    }
}

static void semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename)
{
    timing_t timing_semantic;

    timing_start(&timing_semantic);
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        build_scope_translation_unit(translation_unit);
    }
#ifdef FORTRAN_SUPPORT
    else if (IS_FORTRAN_LANGUAGE)
    {
        build_scope_fortran_translation_unit(translation_unit);
    }
#endif
    else
    {
        internal_error("%s: invalid language kind\n", 
                parsed_filename);
    }
    timing_end(&timing_semantic);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') semantically analyzed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_semantic));
    }

    check_tree(translation_unit->parsed_tree);
}

static const char* prettyprint_translation_unit(translation_unit_t* translation_unit, 
        const char* parsed_filename UNUSED_PARAMETER)
{
    if (CURRENT_CONFIGURATION->do_not_prettyprint)
    {
        return NULL;
    }

    FILE* prettyprint_file = NULL;
    const char* output_filename = NULL;

    if (CURRENT_CONFIGURATION->do_not_compile
            && CURRENT_CONFIGURATION->do_not_link)
    {
        if (strcmp(translation_unit->output_filename, "-") == 0)
        {
            prettyprint_file = stdout;
            output_filename = "(stdout)";
        }
        else
        {
            output_filename = translation_unit->output_filename;
        }
    }
    else
    {
        const char* input_filename_basename = NULL;
        input_filename_basename = give_basename(translation_unit->input_filename);

        const char* preffix = strappend(compilation_process.exec_basename, "_");

        const char* output_filename_basename = NULL; 

#ifdef FORTRAN_SUPPORT
        if (IS_FORTRAN_LANGUAGE)
        {
            // Change the extension to be .f95 always
            const char * ext = strrchr(input_filename_basename, '.');
            ERROR_CONDITION(ext == NULL, "Expecting extension", 0);

            char c[strlen(input_filename_basename) + 1];
            memset(c, 0, sizeof(c));

            strncpy(c, input_filename_basename, (size_t)(ext - input_filename_basename));
            c[ext - input_filename_basename + 1] = '\0';

            input_filename_basename = strappend(c, ".f95");
        }
#endif

        output_filename_basename = strappend(preffix,
                input_filename_basename);

        if (CURRENT_CONFIGURATION->output_directory != NULL)
        {
            output_filename = strappend(CURRENT_CONFIGURATION->output_directory, "/");
            output_filename = strappend(output_filename, output_filename_basename);
        }
        else
        {
            output_filename = output_filename_basename;
        }
    }

    if (CURRENT_CONFIGURATION->pass_through)
        return output_filename;

    // Open it, unless was an already opened descriptor
    if (prettyprint_file == NULL)
        prettyprint_file = fopen(output_filename, "w");

    if (prettyprint_file == NULL)
    {
        running_error("Cannot create output file '%s' (%s)", output_filename,
                strerror(errno));
    }

    timing_t time_print;
    timing_start(&time_print);

    // This will be used by a native compiler
    prettyprint_set_not_internal_output();
    if (IS_C_LANGUAGE
            || IS_CXX_LANGUAGE)
    {
        prettyprint(prettyprint_file, translation_unit->parsed_tree);
    }
#ifdef FORTRAN_SUPPORT
    else if (IS_FORTRAN_LANGUAGE)
    {
        if (CURRENT_CONFIGURATION->column_width != 0)
        {
            temporal_file_t raw_prettyprint = new_temporal_file();
            FILE *raw_prettyprint_file = fopen(raw_prettyprint->name, "w+");
            if (raw_prettyprint_file == NULL)
            {
                running_error("Cannot create temporal file '%s' %s\n", raw_prettyprint->name, strerror(errno));
            }
            fortran_prettyprint(raw_prettyprint_file, translation_unit->parsed_tree);
            rewind(raw_prettyprint_file);

            fortran_split_lines(raw_prettyprint_file, prettyprint_file, CURRENT_CONFIGURATION->column_width);
            fclose(raw_prettyprint_file);
        }
        else
        {
            fortran_prettyprint(prettyprint_file, translation_unit->parsed_tree);
        }
    }
#endif
    else
    {
        internal_error("Invalid language kind", 0);
    }

    timing_end(&time_print);
    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Prettyprinted into file '%s' in %.2f seconds\n", output_filename, timing_elapsed(&time_print));
    }

    if (prettyprint_file != stdout)
    {
        fclose(prettyprint_file);
    }

    return output_filename;
}

static char str_ends_with(const char* str, const char* name)
{
    if (strlen(str) >= strlen(name))
    {
        if (strcmp(&str[strlen(str) - strlen(name)], name) == 0)
        {
            return 1;
        }
    }
    return 0;
}

// This function warns for some known preprocessors, if we find that no proper flag has been passed
static void warn_preprocessor_flags(
        const char* input_filename,
        int num_arguments)
{
    // Since this is easy to forget we will warn the user
    struct prepro_flags 
    {
        const char * flag;
        char seen;
    };

    struct prepro_flags known_prepro_flags[] =
    {
        { "-E", 0 },
        { "-P", 0 },
        { "-EP", 0 },
        // Sentinel
        { NULL, 0 },
    };

    struct prepro_info 
    {
        const char * end_name;
        const char ** required_args;
    };

    // gcc requires -E
    const char* gcc_args[] = { "-E", NULL };
    // icc and xlc will require either -E, -P 
    // icc allows also -EP
    const char* icc_args[] = { "-E", "-EP", "-P", NULL };
    const char* xlc_args[] = { "-E", "-P", NULL };

    struct prepro_info known_prepros[] =
    {
        // GCC
        { "gcc", gcc_args },
        { "g++", gcc_args },
        // Intel C/C++
        { "icc", icc_args },
        { "icpc", icc_args },
        // IBM XL C/C++
        { "xlc", xlc_args },
        { "xlC", xlc_args },
        // Sentinel
        { NULL, NULL }
    };

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        int j;
        for (j = 0; known_prepro_flags[j].flag != NULL; j++)
        {
            if (strcmp(CURRENT_CONFIGURATION->preprocessor_options[i], 
                        known_prepro_flags[j].flag) == 0)
            {
                known_prepro_flags[j].seen = 1;
            }
        }
    }

    // Check whether this is a known preprocessor
    const char* prepro_name = CURRENT_CONFIGURATION->preprocessor_name;
    char is_known_prepro = 0;
    struct prepro_info* current_prepro = NULL;
    char flag_seen = 0;

    for (i = 0; (known_prepros[i].end_name != NULL) && !is_known_prepro && !flag_seen; i++)
    {
        if (str_ends_with(prepro_name, known_prepros[i].end_name))
        {
            // We known this prepro
            is_known_prepro = 1;
            current_prepro = &known_prepros[i];
            // Now check if any of the required flags have been set
            int j;
            for (j = 0; (current_prepro->required_args[j] != NULL) && !flag_seen; j++)
            {
                const char * required_arg = current_prepro->required_args[j];

                int k;
                for (k = 0; (known_prepro_flags[k].flag != NULL) && !flag_seen; k++)
                {
                    struct prepro_flags* current_prepro_flag = &known_prepro_flags[k];

                    if (current_prepro_flag->seen 
                            && (strcmp(current_prepro_flag->flag, required_arg) == 0))
                    {
                        flag_seen = 1;
                    }
                }
            }
        }
    }

    if (is_known_prepro
            && !flag_seen)
    {
        const char* nice_flags_list = "(empty)";
        int num_flags = 0;
        int j;
        for (j = 0; (current_prepro->required_args[j] != NULL); j++)
        {
            num_flags++;
        }

        // Needed to format a proper english message that does not look like a
        // telegram
        const char* none_of = "";
        const char* either = "";
        const char* verb = "has not";
        if (num_flags > 1)
        {
            none_of = "none of ";
            verb = "have";
            either = "either ";
        }

        if (num_flags == 1)
        {
            nice_flags_list = current_prepro->required_args[0];
        }
        else if (num_flags > 1)
        {
            nice_flags_list = current_prepro->required_args[0];
            for (j = 1; j < num_flags - 1; j++)
            {
                nice_flags_list = 
                    strappend(nice_flags_list, 
                            strappend(", ", current_prepro->required_args[j]));
            }
            nice_flags_list = 
                strappend(nice_flags_list, 
                        strappend(" or ", current_prepro->required_args[num_flags - 1]));
        }

        // Count flags
        fprintf(stderr,
                "%s: warning: %s%s %s been passed to %s preprocessor. This is likely to fail\n"
                "%s: note: ensure 'preprocessor_options' of your configuration file includes %s%s\n",
                input_filename,
                none_of,
                nice_flags_list,
                verb,
                current_prepro->end_name,
                input_filename,
                either,
                nice_flags_list);
    }
}

static const char* preprocess_file(translation_unit_t* translation_unit,
        const char* input_filename)
{
    // Add guarding macros
    C_LANGUAGE()
    {
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, "-D_MCC");
    }
    CXX_LANGUAGE()
    {
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, "-D_MCXX");
    }
#ifdef FORTRAN_SUPPORT
    FORTRAN_LANGUAGE()
    {
        add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, "-D_MF03");
    }
#endif
    add_to_parameter_list_str(&CURRENT_CONFIGURATION->preprocessor_options, "-D_MERCURIUM");

    int num_arguments = count_null_ended_array((void**)CURRENT_CONFIGURATION->preprocessor_options);

    char uses_stdout = CURRENT_CONFIGURATION->preprocessor_uses_stdout;

    int num_parameters = num_arguments;

    if (!uses_stdout)
    {
        // input -o output
        num_parameters += 3;
    }
    else
    {
        // input
        num_parameters += 1;
    }

    // NULL
    num_parameters += 1;

    const char* preprocessor_options[num_parameters];
    memset(preprocessor_options, 0, sizeof(preprocessor_options));

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        preprocessor_options[i] = CURRENT_CONFIGURATION->preprocessor_options[i];
    }

    // This started as being something small, and now has grown to a full fledged check
    warn_preprocessor_flags(input_filename, num_arguments);

    const char *preprocessed_filename = NULL;

    if (!CURRENT_CONFIGURATION->do_not_parse)
    {
        temporal_file_t preprocessed_file = new_temporal_file();
        preprocessed_filename = preprocessed_file->name;
    }
    else
    {
        // If we are not going to parse use the original output filename
        if (translation_unit->output_filename == NULL)
        {
            // Send it to stdout
            preprocessed_filename = uniquestr("-");
        }
        else
        {
            // Or to the specified output
            preprocessed_filename = translation_unit->output_filename;
        }
    }

    const char *stdout_file = NULL;

    if (!uses_stdout)
    {
        preprocessor_options[i] = uniquestr("-o"); 
        i++;
        preprocessor_options[i] = preprocessed_filename;
        i++;
        preprocessor_options[i] = input_filename;
        i++;
    }
    else
    {
        stdout_file = preprocessed_filename;

        preprocessor_options[i] = input_filename;
        i++;
    }

    if (CURRENT_CONFIGURATION->pass_through)
    {
        return preprocessed_filename;
    }

    int result_preprocess = execute_program_flags(CURRENT_CONFIGURATION->preprocessor_name,
            preprocessor_options, stdout_file, /* stderr_f */ NULL);

    if (result_preprocess == 0)
    {
        return preprocessed_filename;
    }
    else
    {
        fprintf(stderr, "Preprocessing failed. Returned code %d\n",
                result_preprocess);
        return NULL;
    }
}

#ifdef FORTRAN_SUPPORT
static const char* fortran_prescan_file(translation_unit_t* translation_unit, const char *parsed_filename)
{
    temporal_file_t prescanned_file = new_temporal_file();
    const char* prescanned_filename = prescanned_file->name;

    int prescanner_args = count_null_ended_array((void**)CURRENT_CONFIGURATION->prescanner_options);

    int num_arguments = prescanner_args;
    // -q input -o output
    num_arguments += 5;
    // NULL
    num_arguments += 1;

    const char* mf03_prescanner = "mf03-prescanner";
    int full_path_length = 0;
    if (CURRENT_CONFIGURATION->prescanner_name == NULL)
    {
        full_path_length = strlen(compilation_process.home_directory) + 1 + strlen(mf03_prescanner) + 1;
    }
    else
    {
        full_path_length = strlen(CURRENT_CONFIGURATION->prescanner_name) + 1;
    }
    char full_path[full_path_length];
    if (CURRENT_CONFIGURATION->prescanner_name == NULL)
    {
        memset(full_path, 0, sizeof(full_path));

        snprintf(full_path, sizeof(full_path), "%s/%s", 
                compilation_process.home_directory,
                mf03_prescanner);
    }
    else
    {
        strncpy(full_path, CURRENT_CONFIGURATION->prescanner_name, strlen(CURRENT_CONFIGURATION->prescanner_name));
    }
    full_path[full_path_length-1] = '\0';

    const char* prescanner_options[num_arguments];
    memset(prescanner_options, 0, sizeof(prescanner_options));

    int i;
    for (i = 0; i < prescanner_args; i++)
    {
        prescanner_options[i] = CURRENT_CONFIGURATION->prescanner_options[i];
    }

    prescanner_options[i] = uniquestr("-l");
    i++;
    prescanner_options[i] = uniquestr("-q");
    i++;
    prescanner_options[i] = uniquestr("-o");
    i++;
    prescanner_options[i] = prescanned_filename;
    i++;
    prescanner_options[i] = parsed_filename;

    int result_prescan = execute_program(full_path, prescanner_options);
    if (result_prescan == 0)
    {
        return prescanned_filename;
    }
    else
    {
        fprintf(stderr, "Conversion from fixed to free form failed. Returned code %d\n",
                result_prescan);
        return NULL;
    }
}
#endif

static void native_compilation(translation_unit_t* translation_unit, 
        const char* prettyprinted_filename, 
        char remove_input)
{
    if (CURRENT_CONFIGURATION->do_not_compile)
        return;

    if (remove_input)
    {
        mark_file_for_cleanup(prettyprinted_filename);
    }

    const char* output_object_filename = NULL;

    if (translation_unit->output_filename == NULL
            || !CURRENT_CONFIGURATION->do_not_link)
    {
        char temp[256];
        strncpy(temp, give_basename(translation_unit->input_filename), 255);
        temp[255] = '\0';
        char* p = strrchr(temp, '.');
        if (p != NULL)
        {
            *p = '\0';
        }

        if (!CURRENT_CONFIGURATION->generate_assembler)
        {
            output_object_filename = strappend(temp, ".o");
        }
        else
        {
            output_object_filename = strappend(temp, ".s");
        }

        translation_unit->output_filename = output_object_filename;
    }
    else
    {
        output_object_filename = translation_unit->output_filename;
    }

    int num_args_compiler = count_null_ended_array((void**)CURRENT_CONFIGURATION->native_compiler_options);

    int num_arguments = num_args_compiler;
    // -c -o output input
    num_arguments += 4;
    // NULL
    num_arguments += 1;

    const char* native_compilation_args[num_arguments];
    memset(native_compilation_args, 0, sizeof(native_compilation_args));

    int i;
    for (i = 0; i < num_args_compiler; i++)
    {
        native_compilation_args[i] = CURRENT_CONFIGURATION->native_compiler_options[i];
    }

    if (!CURRENT_CONFIGURATION->generate_assembler)
    {
        native_compilation_args[i] = uniquestr("-c");
    }
    else
    {
        native_compilation_args[i] = uniquestr("-S");
    }
    i++;
    native_compilation_args[i] = uniquestr("-o");
    i++;
    native_compilation_args[i] = output_object_filename;
    i++;
    native_compilation_args[i] = prettyprinted_filename;

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Performing native compilation of '%s' into '%s'\n",
                prettyprinted_filename, output_object_filename);
    }

    timing_t timing_compilation;
    timing_start(&timing_compilation);

    if (execute_program(CURRENT_CONFIGURATION->native_compiler_name, native_compilation_args) != 0)
    {
        running_error("Native compilation failed for file '%s'", translation_unit->input_filename);
    }
    timing_end(&timing_compilation);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "File '%s' ('%s') natively compiled in %.2f seconds\n", 
                translation_unit->input_filename,
                prettyprinted_filename,
                timing_elapsed(&timing_compilation));
    }
}

static void embed_files(void)
{
    if (CURRENT_CONFIGURATION->do_not_compile)
        return;

    char there_are_secondary_files = 0;
    int i;
    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        if (compilation_process.translation_units[i]->num_secondary_translation_units != 0)
        {
            there_are_secondary_files = 1;
        }
    }

    if (!there_are_secondary_files)
        return;


    // Create the temporal directory
    temporal_file_t temp_dir = new_temporal_dir();

    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        int num_secondary_translation_units = 
            compilation_process.translation_units[i]->num_secondary_translation_units;
        compilation_file_process_t** secondary_translation_units = 
            compilation_process.translation_units[i]->secondary_translation_units;

        const char* extension = get_extension_filename(compilation_process.translation_units[i]->translation_unit->input_filename);
        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        // We do not have to embed linker data
        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
            continue;

        const char *output_filename = compilation_process.translation_units[i]->translation_unit->output_filename;

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Embedding secondary files into '%s'\n", output_filename);
        }

        // For each translation unit create the profile directory if needed
        int j;
        for (j = 0; j < num_secondary_translation_units; j++)
        {
            compilation_file_process_t* secondary_compilation_file = secondary_translation_units[j];
            translation_unit_t* current_secondary = secondary_compilation_file->translation_unit;
            char dir_path[1024];
            snprintf(dir_path, 1023, "%s%s%s", 
                    temp_dir->name, 
                    DIR_SEPARATOR, 
                    secondary_compilation_file->compilation_configuration->configuration_name);
            dir_path[1023] = '\0';

            struct stat buf;
            int res = stat(dir_path, &buf);

            if (res != 0)
            {
                if (errno == ENOENT)
                {
                    // Create the directory if it does not exist
                    if (mkdir(dir_path, 0700) != 0)
                    {
                        running_error("When creating multifile archive, cannot create directory '%s': %s\n",
                                dir_path,
                                strerror(errno));
                    }
                }
                else
                {
                    running_error("Stat failed on '%s': %s\n",
                            dir_path,
                            strerror(errno));
                }
            }
            else
            {
                if (!S_ISDIR(buf.st_mode))
                {
                    running_error("When creating multifile archive, path '%s' is not a directory\n",
                            dir_path);
                }
            }

            // Now move the secondary file

            char dest_path[1024];
            snprintf(dest_path, 1023, "%s%s%s", 
                    dir_path, 
                    DIR_SEPARATOR, 
                    give_basename(current_secondary->output_filename));

            if (move_file(current_secondary->output_filename, dest_path) != 0)
            {
                running_error("When creating multifile archive, file '%s' could not be moved to '%s'\n",
                        current_secondary->output_filename,
                        dest_path);
            }
        }
        // Now all files have been moved into the temporal directory, run the tar there
        temporal_file_t new_tar_file = new_temporal_file_extension(".tar");
        const char* tar_args[] =
        {
            "cf",
            new_tar_file->name,
            "-C", temp_dir->name,
            ".",
            NULL
        };

        if (execute_program("tar", tar_args) != 0)
        {
            running_error("When creating multifile archive, 'tar' failed\n");
        }

        // Now we have tar that we are going to embed into the .o file

        // objcopy --add-section .mercurium=architectures.tar --set-section-flags .mercurium=alloc,readonly prova.o

        char multifile_section_and_file[1024], multifile_section_and_flags[1024];

        snprintf(multifile_section_and_file, 1023, "%s=%s",
                MULTIFILE_SECTION, new_tar_file->name);
        multifile_section_and_file[1023] = '\0';

        snprintf(multifile_section_and_flags, 1023, "%s=alloc,readonly",
                MULTIFILE_SECTION);
        multifile_section_and_flags[1023] = '\0';

        const char* objcopy_args[] =
        {
            "--add-section", multifile_section_and_file, 
            "--set-section-flags", multifile_section_and_flags,
            output_filename,
            NULL,
        };

        if (execute_program("objcopy", objcopy_args) != 0)
        {
            running_error("When creating multifile archive, 'objcopy' failed\n");
        }

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Secondary files successfully embedded into '%s' file\n", 
                    output_filename);
        }
    }
}

static void link_files(const char** file_list, int num_files,
        const char** additional_files, int num_additional_files,
        compilation_configuration_t* compilation_configuration)
{
    int num_args_linker = count_null_ended_array((void**)compilation_configuration->linker_options);

    int num_arguments = num_args_linker + num_files + num_additional_files;
    // -o output
    num_arguments += 2;
    // NULL
    num_arguments += 1;

    const char* linker_args[num_arguments];
    memset(linker_args, 0, sizeof(linker_args));

    int i = 0;
    int j = 0;

    if (compilation_configuration->linked_output_filename != NULL)
    {
        linker_args[i] = uniquestr("-o");
        i++;
        linker_args[i] = compilation_configuration->linked_output_filename;
        i++;
    }

    for (j = 0; j < num_additional_files; j++)
    {
        linker_args[i] = additional_files[j];
        i++;
    }

    for (j = 0; j < num_files; j++)
    {
        linker_args[i] = file_list[j];
        i++;
    }

    for (j = 0; j < num_args_linker; j++)
    {
        linker_args[i] = compilation_configuration->linker_options[j];
        i++;
    }

    timing_t timing_link;
    timing_start(&timing_link);
    if (execute_program(compilation_configuration->linker_name, linker_args) != 0)
    {
        running_error("Link failed", 0);
    }
    timing_end(&timing_link);

    if (compilation_configuration->verbose)
    {
        fprintf(stderr, "Link performed in %.2f seconds\n", 
                timing_elapsed(&timing_link));
    }
}

target_options_map_t* get_target_options(compilation_configuration_t* configuration, 
        const char* configuration_name)
{
    int i;
    for (i = 0 ; i < configuration->num_target_option_maps; i++)
    {
        if (strcmp(configuration->target_options_maps[i]->profile, 
                    configuration_name) == 0)
        {
            return configuration->target_options_maps[i];
        }
    }

    return NULL;
}

static void do_combining(target_options_map_t* target_map,
        compilation_configuration_t* configuration)
{
    if (!target_map->do_combining)
        return;

    temporal_file_t temp_outfile = new_temporal_file_extension(".o");
    const char* output_filename = temp_outfile->name;

    switch (target_map->combining_mode)
    {
        case COMBINING_MODE_SPU_ELF:
            {

                // Usage: embedspu [flags] symbol_name input_filename output_filename
                const char* args[] =
                {
                    // We will stick to CSS convention of calling the symbol 'spe_prog'
                    /* FIXME: no flags at the moment */
                    "spe_prog", 
                    configuration->linked_output_filename,
                    output_filename,
                    NULL,
                };

                if (execute_program("ppu-spuembed", args) != 0)
                {
                    running_error("Error when embedding SPU executable", 0);
                }

                remove(configuration->linked_output_filename);
                configuration->linked_output_filename = output_filename;
                break;
            }
        case COMBINING_MODE_INCBIN:
            {
                temporal_file_t temp_file_as = new_temporal_file_extension(".s");
                
                FILE* temp_file_fd = fopen(temp_file_as->name, "w");

                if (temp_file_fd == NULL)
                {
                    running_error("Cannot create temporal assembler file '%s': %s\n",
                            temp_file_as->name,
                            strerror(errno));
                }

                fprintf(temp_file_fd,
                        ".data\n"
                        ".global _%s_start\n"
                        ".global _%s_end\n"
                        ".align 16\n"
                        "_%s_start:\n"
                        ".incbin \"%s\"\n"
                        "_%s_end:\n",
                        configuration->configuration_name,
                        configuration->configuration_name,
                        configuration->configuration_name,
                        configuration->linked_output_filename,
                        configuration->configuration_name
                        );

                fclose(temp_file_fd);

                const char* args[] =
                {
                    "-c",
                    "-o", output_filename,
                    "-x", "assembler",
                    temp_file_as->name,
                    NULL
                };

                if (execute_program(CURRENT_CONFIGURATION->native_compiler_name,
                            args) != 0)
                {
                    running_error("Error when complining embedding assembler", 0);
                }

                remove(configuration->linked_output_filename);
                configuration->linked_output_filename = output_filename;
                break;
            }
        default:
            {
                internal_error("Invalid combining mode\n", 0);
                break;
            }
    }
}

static void extract_files_and_sublink(const char** file_list, int num_files,
        const char*** additional_files, int *num_additional_files,
        compilation_configuration_t* target_configuration)
{
    multifile_init_dir();

    char no_multifile_info = 1;

    int i;
    for (i = 0; i < num_files; i++)
    {
        if (multifile_object_has_extended_info(file_list[i]))
        {
            no_multifile_info = 0;
            multifile_extract_extended_info(file_list[i]);
        }
    }

    if (no_multifile_info)
        return;

    const char** multifile_profiles = NULL;
    int num_multifile_profiles = 0;
    multifile_get_extracted_profiles(&multifile_profiles, &num_multifile_profiles);

    for (i = 0; i < num_multifile_profiles; i++)
    {
        compilation_configuration_t* configuration = get_compilation_configuration(multifile_profiles[i]);

        if (configuration == NULL)
        {
            running_error("Multifile needs a profile '%s' not defined in the configuration\n",
                    multifile_profiles[i]);
        }

        target_options_map_t* target_map = get_target_options(configuration, target_configuration->configuration_name);

        if (target_map == NULL)
        {
            running_error("There are no target options defined from profile '%s' to profile '%s' in the configuration\n",
                    configuration->configuration_name, 
                    target_configuration->configuration_name);
        }

        const char** multifile_file_list = NULL;
        int multifile_num_files = 0;

        multifile_get_profile_file_list(
                multifile_profiles[i],
                &multifile_file_list, 
                &multifile_num_files);

        if (!target_map->do_sublink)
        {
            // Now add the linked output as an additional link file
            int j;
            for (j = 0; j < multifile_num_files; j++)
            {
                P_LIST_ADD((*additional_files), (*num_additional_files), 
                        multifile_file_list[j]);
            }
        }
        else
        {
            // Create a name for sublinking
#ifndef WIN32_BUILD
            // Following decades of UNIX tradition
            const char* linked_output_suffix = "a.out";
#else
            const char* linked_output_suffix = "a.exe";
#endif
            if (CURRENT_CONFIGURATION->linked_output_filename != NULL)
            {
                linked_output_suffix = give_basename(CURRENT_CONFIGURATION->linked_output_filename);
            }

            configuration->linked_output_filename =
                strappend(configuration->configuration_name, linked_output_suffix);

            link_files(multifile_file_list, multifile_num_files, 
                    /* additional files */ NULL, /* num_additional_files */ 0,
                    configuration);

            do_combining(target_map, configuration);

            // Now add the linked output as an additional link file
            P_LIST_ADD((*additional_files), 
                    (*num_additional_files), 
                    configuration->linked_output_filename);
        }
    }
}

static void link_objects(void)
{
    if (CURRENT_CONFIGURATION->do_not_link)
        return;

    const char * file_list[compilation_process.num_translation_units];

    int j;
    for (j = 0; j < compilation_process.num_translation_units; j++)
    {
        translation_unit_t* translation_unit = compilation_process.translation_units[j]->translation_unit;

        const char* extension = get_extension_filename(translation_unit->input_filename);
        struct extensions_table_t* current_extension = fileextensions_lookup(extension, strlen(extension));

        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
        {
            file_list[j] = translation_unit->input_filename;
        }
        else
        {
            file_list[j] = translation_unit->output_filename;
            mark_file_as_temporary(file_list[j]);
        }
    }

    int num_additional_files = 0;
    const char** additional_files = NULL;
    extract_files_and_sublink(file_list, compilation_process.num_translation_units, 
            &additional_files, &num_additional_files, CURRENT_CONFIGURATION);

    link_files(file_list, compilation_process.num_translation_units, 
            additional_files, 
            num_additional_files, 
            CURRENT_CONFIGURATION);
}


#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig)
{
    fprintf(stderr, "Signal handler called (signal=%s). Exiting.\n", strsignal(sig));

    // Switch to the command_line_configuration so we honour command line flags
    SET_CURRENT_CONFIGURATION(compilation_process.command_line_configuration);

    if (CURRENT_CONFIGURATION != NULL
            && !CURRENT_CONFIGURATION->debug_options.do_not_run_gdb
            // Do not call the debugger for Ctrl-C
            && sig != SIGINT)
        run_gdb();

    if (!in_cleanup_routine)
        cleanup_routine();

    raise(sig);
}
#endif

static char check_tree(AST a)
{
    AST ambiguous_node = NULL;
    if (!check_for_ambiguities(a, &ambiguous_node))
    {
        fprintf(stderr, "============================\n");
        fprintf(stderr, "  Ambiguities not resolved\n");
        fprintf(stderr, "============================\n");
        if (IS_C_LANGUAGE
                || IS_CXX_LANGUAGE)
        {
            prettyprint(stderr, ambiguous_node);
        }
#ifdef FORTRAN_SUPPORT
        else if (IS_FORTRAN_LANGUAGE)
        {
            fortran_prettyprint(stderr, ambiguous_node);
        }
#endif
        else
        {
            internal_error("Invalid language kind", 0);
        }
        fprintf(stderr, "\n============================\n");
        fprintf(stderr, " at %s\n", ast_location(ambiguous_node));
        fprintf(stderr, "============================\n");
        ast_dump_graphviz(ambiguous_node, stderr);
        fprintf(stderr, "============================\n");
        internal_error("Tree still contains ambiguities", 0);

        return 0;
    }

    // Check consistency of links
    if (!ast_check(a))
    {
        internal_error("Tree is inconsistent\n", 0);
    }

    return 1;
}

static char check_for_ambiguities(AST a, AST* ambiguous_node)
{
    if (a == NULL)
        return 1;

    if (ASTType(a) == AST_AMBIGUITY)
    {
        *ambiguous_node = a;
        return 0;
    }
    else
    {
        return check_for_ambiguities(ASTSon0(a), ambiguous_node)
            && check_for_ambiguities(ASTSon1(a), ambiguous_node)
            && check_for_ambiguities(ASTSon2(a), ambiguous_node)
            && check_for_ambiguities(ASTSon3(a), ambiguous_node);
    }
}


static void load_compiler_phases(compilation_configuration_t* config)
{
    // Do nothing if they were already loaded 
    // This is also checked (and set) in cxx-compilerphases.cpp but here we
    // avoid showing the timing message as well
    if (config->phases_loaded)
    {
        return;
    }

    if (config->verbose)
    {
        fprintf(stderr, "Loading compiler phases for profile '%s'\n", 
                CURRENT_CONFIGURATION->configuration_name);
    }

    timing_t loading_phases;
    timing_start(&loading_phases);

    // This invokes a C++ routine that will dlopen all libraries, get the proper symbol
    // and fill an array of compiler phases
    load_compiler_phases_cxx(config);

    timing_end(&loading_phases);

    if (config->verbose)
    {
        fprintf(stderr, "Compiler phases for profile '%s' loaded in %.2f seconds\n", 
                CURRENT_CONFIGURATION->configuration_name,
                timing_elapsed(&loading_phases));
    }

}


// Useful for debugging sessions
void _enable_debug(void)
{
    CURRENT_CONFIGURATION->debug_options.enable_debug_code = 1;
}


#ifdef HAVE_MALLINFO
static char* power_suffixes[9] = 
{
    "",
    "K",
    "M",
    "G",
    "T",
    "P",
    "E",
    "Z",
    "Y"
};

static void print_human(char *dest, unsigned long long num_bytes_)
{
    if (CURRENT_CONFIGURATION->debug_options.print_memory_report_in_bytes)
    {
        sprintf(dest, "%llu", num_bytes_);
    }
    else
    {
        double num_bytes = num_bytes_;
        int i = 0;

        while ((num_bytes > 1024) && (i <= 8))
        {
            i++;
            num_bytes /= 1024;
        }

        if (i == 0)
        {
            sprintf(dest, "%llu", num_bytes_);
        }
        else
        {
            sprintf(dest, "%.2f%s", num_bytes, power_suffixes[i]);
        }
    }
}

static void compute_tree_breakdown(AST a, int breakdown[MAX_AST_CHILDREN + 1], int breakdown_real[MAX_AST_CHILDREN + 1], int *num_nodes)
{
    if (a == NULL)
        return;

    int num_real = 0;
    int num_intent = ASTNumChildren(a);

    (*num_nodes)++;

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        if (ASTChild(a, i) != NULL)
        {
            num_real++;
        }
        compute_tree_breakdown(ASTChild(a, i), breakdown, breakdown_real, num_nodes);
    }

    if (num_intent <= (MAX_AST_CHILDREN + 1))
        breakdown[num_intent]++;

    if (num_real <= (MAX_AST_CHILDREN + 1))
        breakdown_real[num_real]++;
}
#endif

static void print_memory_report(void)
{
#ifdef HAVE_MALLINFO
    char c[256];

    struct mallinfo mallinfo_report = mallinfo();

    fprintf(stderr, "\n");
    fprintf(stderr, "Memory report\n");
    fprintf(stderr, "-------------\n");
    fprintf(stderr, "\n");

    print_human(c, mallinfo_report.arena);
    fprintf(stderr, " - Total size of memory allocated with sbrk: %s\n",
            c);

    fprintf(stderr, " - Number of chunks not in use: %lu\n",
            (unsigned long)mallinfo_report.ordblks);
    fprintf(stderr, " - Number of chunks allocated with mmap: %lu\n",
            (unsigned long)mallinfo_report.hblks);

    print_human(c, mallinfo_report.hblkhd);
    fprintf(stderr, " - Total size allocated with mmap: %s\n",
            c);

    print_human(c, mallinfo_report.uordblks);
    fprintf(stderr, " - Total size of memory occupied by chunks handed out by malloc: %s\n",
            c);

    print_human(c, mallinfo_report.fordblks);
    fprintf(stderr, " - Total size of memory occupied by free (not in use) chunks: %s\n",
            c);

    print_human(c, mallinfo_report.keepcost);
    fprintf(stderr, " - Size of the top most releasable chunk: %s\n",
            c);

    fprintf(stderr, "\n");

    unsigned long long accounted_memory = 0;
    //
    // -- AST

    accounted_memory += ast_astmake_used_memory();
    print_human(c, ast_astmake_used_memory());
    fprintf(stderr, " - Memory used to create AST nodes: %s\n", c);

    accounted_memory += ast_instantiation_used_memory();
    print_human(c, ast_instantiation_used_memory());
    fprintf(stderr, " - Memory used to copy AST nodes when instantiating: %s\n", c);

    // -- AST

    accounted_memory += type_system_used_memory();
    print_human(c, type_system_used_memory());
    fprintf(stderr, " - Memory usage due to type system: %s\n", c);

    {
        fprintf(stderr, " - Type system breakdown:\n");
        fprintf(stderr, "    - Size of type node (bytes): %zu\n", get_type_t_size());
        fprintf(stderr, "    - Number of enum types: %d\n", get_enum_type_counter());
        fprintf(stderr, "    - Number of class types: %d\n", get_class_type_counter());
        fprintf(stderr, "    - Number of function types: %d\n", get_function_type_counter());
        fprintf(stderr, "    - Number of reused function types: %d\n", get_function_type_reused());
        fprintf(stderr, "    - Number of array types: %d\n", get_array_type_counter());
        fprintf(stderr, "    - Number of pointer types: %d\n", get_pointer_type_counter());
        fprintf(stderr, "    - Number of pointer to member types: %d\n", get_pointer_to_member_type_counter());
        fprintf(stderr, "    - Number of reference types: %d\n", get_reference_type_counter());
        fprintf(stderr, "    - Number of template types: %d\n", get_template_type_counter());
        fprintf(stderr, "    - Number of qualified variants: %d\n", get_qualified_type_counter());
        fprintf(stderr, "    - Number of vector types: %d\n", get_vector_type_counter());
    }
    
    accounted_memory += char_trie_used_memory();
    print_human(c, char_trie_used_memory());
    fprintf(stderr, " - Memory usage due to global string table: %s\n", c);

    accounted_memory += buildscope_used_memory();
    print_human(c, buildscope_used_memory());
    fprintf(stderr, " - Memory usage due to scope building: %s\n", c);

    accounted_memory += symbols_used_memory();
    print_human(c, symbols_used_memory());
    fprintf(stderr, " - Memory usage due to symbols: %s\n", c);

    accounted_memory += scope_used_memory();
    print_human(c, scope_used_memory());
    fprintf(stderr, " - Memory usage due to scopes: %s\n", c);

    accounted_memory += scopelink_used_memory();
    print_human(c, scopelink_used_memory());
    fprintf(stderr, " - Memory usage due to scope links: %s\n", c);

    accounted_memory += exprtype_used_memory();
    print_human(c, exprtype_used_memory());
    fprintf(stderr, " - Memory usage due to expression type check: %s\n", c);

    accounted_memory += typeunif_used_memory();
    print_human(c, typeunif_used_memory());
    fprintf(stderr, " - Memory usage due to type unification: %s\n", c);

    accounted_memory += typededuc_used_memory();
    print_human(c, typededuc_used_memory());
    fprintf(stderr, " - Memory usage due to type deduction: %s\n", c);

    accounted_memory += overload_used_memory();
    print_human(c, overload_used_memory());
    fprintf(stderr, " - Memory usage due to overload resolution: %s\n", c);

    fprintf(stderr, "\n");

    print_human(c, accounted_memory);
    fprintf(stderr, " - Total accounted memory: %s\n", c);

    fprintf(stderr, "\n");
    fprintf(stderr, "Abstract Syntax Tree(s) breakdown\n");
    fprintf(stderr, "---------------------------------\n");
    fprintf(stderr, "\n");

    int children_count[MAX_AST_CHILDREN + 1];
    int children_real_count[MAX_AST_CHILDREN + 1];

    int num_nodes = 0;

    int i;
    for (i = 0; i < MAX_AST_CHILDREN + 1; i++)
    {
        children_count[i] = 0;
        children_real_count[i] = 0;
    }

    // Per every translation unit
    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        compilation_file_process_t* file_process = compilation_process.translation_units[i];
        translation_unit_t* translation_unit = file_process->translation_unit;

        compute_tree_breakdown(translation_unit->parsed_tree, children_count, children_real_count, &num_nodes);
    }

    fprintf(stderr, " - AST node size (bytes): %d\n", ast_node_size());
    fprintf(stderr, " - Total number of AST nodes: %d\n", num_nodes);

    for (i = 0; i < MAX_AST_CHILDREN + 1; i++)
    {
        fprintf(stderr, " - Nodes with %d children: %d\n", i, children_count[i]);
    }
    for (i = 0; i < MAX_AST_CHILDREN + 1; i++)
    {
        fprintf(stderr, " - Nodes with %d real children: %d\n", i, children_real_count[i]);
    }

    fprintf(stderr, "\n");
#else
    fprintf(stderr, "Memory statistics are not available in this environment\n");
#endif
}

type_environment_t* get_environment(const char* env_id)
{
    type_environment_t** type_env = type_environment_list;

    for (type_env = type_environment_list;
            (*type_env) != NULL;
            type_env++)
    {
        if (strcmp(env_id, (*type_env)->environ_id) == 0)
        {
            return (*type_env);
        }
    }

    fprintf(stderr, "Unknown environments '%s'. "
            "Use '--list-env' to get a list of supported environments\n", env_id);
    return NULL;
}

static void list_environments(void)
{
    fprintf(stdout, "Supported environments:\n\n");

    type_environment_t** type_env = type_environment_list;

    for (type_env = type_environment_list;
            (*type_env) != NULL;
            type_env++)
    {
        fprintf(stdout, "  %-20s (%s)\n",
                (*type_env)->environ_id,
                (*type_env)->environ_name);
    }

    fprintf(stdout, "\n");
    fprintf(stdout, "Command line parameter --env=<env-id> can be used to choose a particular architecture.\n");
    fprintf(stdout, "If not specified, default environment is '%s' (%s)\n",
            default_environment->environ_id,
            default_environment->environ_name);

    exit(EXIT_SUCCESS);
}

