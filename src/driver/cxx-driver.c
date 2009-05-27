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
// It does not include any C++ code in the header
#include "cxx-compilerphases.hpp"
#include "mcfg.h"

/* ------------------------------------------------------------------ */
#define HELP_STRING \
"Options: \n" \
"  -h, --help               Shows this help and quits\n" \
"  --version                Shows version and quits\n" \
"  -v, --verbose            Runs verbosely, displaying the programs\n" \
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
"  -O -O0 -O1 -O2 -O3       Sets optimization level to the\n" \
"                           preprocessor and native compiler\n" \
"  -y                       File will be parsed but it will not be\n" \
"                           compiled not linked.\n" \
"  -x lang                  Override language detection to <lang>\n" \
"  -k, --keep-files         Do not remove intermediate temporary\n" \
"                           files\n" \
"  -a, --check-dates        Checks dates before regenerating files\n" \
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
"  --pp-stdout              Preprocessor uses stdout for output\n" \
"  --Wp,<options>           Pass comma-separated <options> on to\n" \
"                           the preprocessor\n" \
"  --Wn,<options>           Pass comma-separated <options> on to\n" \
"                           the native compiler\n" \
"  --Wl,<options>           Pass comma-separated <options> on to\n" \
"                           the linker\n" \
"  --no-openmp              Disables OpenMP 2.5 support\n" \
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
"  --hlt                    Enable High Level Transformations\n" \
"                           This enables '#pragma hlt'\n" \
"\n" \
"gcc compatibility flags:\n" \
"\n" \
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
"  -W<option>\n" \
"  -pthread\n" \
"  -Xpreprocessor OPTION\n" \
"  -Xlinker OPTION\n" \
"  -Xassembler OPTION\n" \
"\n" \
"These gcc flags are passed verbatim to preprocessor, compiler and\n" \
"linker.\n" \
"\n"
/* ------------------------------------------------------------------ */

// Alternate signal stack of 32 KB (sometimes the compiler uses a huge stack!)
static char _alternate_signal_stack[32*1024];

// It mimics getopt
#define SHORT_OPTIONS_STRING "vkacho:EyI:L:l:gD:x:"
// This one mimics getopt_long but with one less field (the third one is not given)
struct command_line_long_options command_line_long_options[] =
{
    {"help",        CLP_NO_ARGUMENT, 'h'},
    {"version",     CLP_NO_ARGUMENT, OPTION_VERSION},
    {"verbose",     CLP_NO_ARGUMENT, 'v'},
    {"keep-files",  CLP_NO_ARGUMENT, 'k'},
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
    {"hlt", CLP_NO_ARGUMENT, OPTION_ENABLE_HLT},
    // sentinel
    {NULL, 0, 0}
};

char* source_language_names[] =
{
    [SOURCE_LANGUAGE_UNKNOWN] = "unknown",
    [SOURCE_LANGUAGE_C] = "C",
    [SOURCE_LANGUAGE_CXX] = "C++"
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
static void native_compilation(translation_unit_t* translation_unit, const char* prettyprinted_filename);

static const char* find_home(void);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig);
#endif
static char check_tree(AST a);
static char check_for_ambiguities(AST a, AST* ambiguous_node);

static void link_objects(void);

static void add_to_parameter_list_str(const char*** existing_options, const char* str);
static void parse_subcommand_arguments(const char* arguments);

static void enable_debug_flag(const char* flag);

static void load_compiler_phases(compilation_configuration_t* config);

static void register_default_initializers(void);

static void help_message(void);

static void print_memory_report(void);

static int parse_special_parameters(int *should_advance, int argc, 
        const char* argv[], char dry_run);
static int parse_implicit_parameter_flag(int *should_advance, const char *special_parameter);

static void list_environments(void);


static char show_help_message = 0;

int main(int argc, char* argv[])
{
    timing_t timing_global;
    timing_start(&timing_global);

    // Initialization of the driver
    driver_initialization(argc, (const char**)argv);

    // Default values
    initialize_default_values();

    // Register default initializers
    register_default_initializers();

    // Load configuration files and the profiles defined there Here we get all
    // the implicit parameters defined in configuration files and we switch to
    // the main profile of the compiler. Profiles are not yet fully populated.
    load_configuration();
    
    // Parse arguments just to get the implicit parameters passed in the
    // command line. We need those to properly populate profiles.
    parse_arguments(compilation_process.argc,
            compilation_process.argv, 
            /* from_command_line= */1,
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

    // Link all generated objects
    link_objects();

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
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

// Basic initialization prior to argument parsing and configuration loading
static void driver_initialization(int argc, const char* argv[])
{
    atexit(cleanup_routine);

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    // Define alternate stack
    stack_t alternate_stack;

    alternate_stack.ss_flags = 0;
    alternate_stack.ss_size = sizeof(_alternate_signal_stack);
    alternate_stack.ss_sp = (void*)_alternate_signal_stack;

    if (alternate_stack.ss_sp == 0
            || sigaltstack(&alternate_stack, /* oss */ NULL) != 0)
    {
        running_error("Setting alternate signal stack failed\n");
    }

    // Program signals
    struct sigaction terminating_sigaction;
    memset(&terminating_sigaction, 0, sizeof(terminating_sigaction));

    terminating_sigaction.sa_handler = terminating_signal_handler;
    // Use alternate stack and we want the signal be reset when it happens
    terminating_sigaction.sa_flags = SA_RESETHAND | SA_ONSTACK;
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
    compilation_process.argv = (const char**)argv;
    compilation_process.exec_basename = give_basename(argv[0]);

    // Find my own directory
    compilation_process.home_directory = find_home();
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

    const char **input_files = NULL;
    int num_input_files = 0;

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
                fprintf(stderr, "Warning: Parameter '%s' skipped.\n", argv[parameter_index]);
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
                fprintf(stderr, "Invalid non-option binded argument '%s'"
                        " specified in the configuration file\n",
                        parameter_info.argument);
                continue;
            }
            // Ignore spurious parameters
            if ((parameter_info.argument != NULL)
                    && (strlen(parameter_info.argument) > 0))
            {
                P_LIST_ADD(input_files, num_input_files, parameter_info.argument);
            }
        }
        // A known option
        else 
        {
            // Ignore normal flags since we are parsing only implicits
            if (parse_implicits_only)
                continue;

            switch (parameter_info.value)
            {
                case OPTION_VERSION : // --version
                    {
                        // Special case where nothing should be done
                        print_version();
                        exit(EXIT_SUCCESS);
                        break;
                    }
                case 'v' : // --verbose || -v
                    {
                        v_specified = 1;
                        CURRENT_CONFIGURATION->verbose = 1;
                        break;
                    }
                case 'k' : // --keep-files || -k
                    {
                        CURRENT_CONFIGURATION->keep_files = 1;
                        break;
                    }
                case 'c' : // -c
                    {
                        if (y_specified || E_specified)
                        {
                            fprintf(stderr, "Parameter -c cannot be used together with -E or -y\n");
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
                            fprintf(stderr, "Parameter -E cannot be used together with -c or -y\n");
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
                            fprintf(stderr, "Parameter -y cannot be used together with -c or -E\n");
                            return 1;
                        }

                        y_specified = 1;

                        CURRENT_CONFIGURATION->do_not_compile = 1;
                        CURRENT_CONFIGURATION->do_not_link = 1;
                        break;
                    }
                case 'a' : // --check-dates || -a
                    {
                        CURRENT_CONFIGURATION->check_dates = 1;
                        break;
                    }
                case OPTION_CONFIG_FILE :
                    {
                        // This option is handled in "load_configuration"
                        // and ignored here for getopt_long happiness
                        break;
                    }
                case 'o' :
                    {
                        if (output_file != NULL)
                        {
                            fprintf(stderr, "Output file specified twice\n");
                            return 1;
                        }
                        else
                        {
                            if ((num_input_files > 1) 
                                    && c_specified)
                            {
                                fprintf(stderr, "Cannot specify -o when -c once given more than one file");
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
                        else
                        {
                            fprintf(stderr, "Invalid language specification in -x, valid options are 'C' or 'C++'. Ignoring\n");
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
                case OPTION_NO_OPENMP :
                    {
                        CURRENT_CONFIGURATION->disable_openmp = 1;
                        break;
                    }
                case OPTION_HELP_DEBUG_FLAGS :
                    {
                        print_debug_flags_list();
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
                            fprintf(stderr, "External variable '%s' definition is missing a colon. It will be ignored\n",
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
                        fprintf(stderr, "Option '--disable-sizeof' should be used only to work around problems. Please, report a bug.\n");
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
                            fprintf(stderr, "UPC static THREADS=%s\n", parameter_info.argument);
                            CURRENT_CONFIGURATION->upc_threads = uniquestr(parameter_info.argument);
                        }
                        break;
                    }
                case OPTION_ENABLE_HLT :
                    {
                        CURRENT_CONFIGURATION->enable_hlt = 1;
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
            && !v_specified
            && !CURRENT_CONFIGURATION->do_not_process_files)
    {
        fprintf(stderr, "You must specify an input file\n");
        return 1;
    }

    if (num_input_files == 0
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
        fprintf(stderr, "Specifying stdout by means of '-o -' is only valid with -y or -E\n");
        return 1;
    }

    // If -E has been specified and no output file has been, assume it is "-"
    if (output_file == NULL
            && (E_specified || y_specified)
            && !CURRENT_CONFIGURATION->do_not_process_files)
                        // Do not process anything
    {
        fprintf(stderr, "Assuming stdout as default output since -E has been specified\n");
        output_file = uniquestr("-");
    }

    // When -c and -o are given only one file is valid
    if (output_file != NULL
            && c_specified
            && (num_input_files > 1))
    {
        fprintf(stderr, "Cannot specify -o and -c with multiple files (second file '%s')", 
                argv[(parameter_index - 1)]);
        return 1;
    }

    if (CURRENT_CONFIGURATION->do_not_process_files)
    {
        // Neither do preprocessign nor compilation, just linking process without any files
        CURRENT_CONFIGURATION->do_not_parse = 1;
        CURRENT_CONFIGURATION->do_not_compile = 1;
        CURRENT_CONFIGURATION->do_not_prettyprint = 1;
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
        add_new_file_to_compilation_process(input_files[i],
                output_file, CURRENT_CONFIGURATION);
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
                (*should_advance)++;
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

                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;
                }
                else if (((argument[2] == 'F') && (argument[3] == '\0')) // -MF
                        || ((argument[2] == 'G') && (argument[3] == '\0')) // -MG
                        || ((argument[2] == 'T') && (argument[3] == '\0'))) // -MT
                {
                    add_parameter_all_toolchain(argument, dry_run);
                    (*should_advance)++;

                    // Pass the next argument too
                    argument = argv[parameter_index + 1];
                    add_parameter_all_toolchain(argument, dry_run);
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
                        && argument[4] == '=') { }
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
                else if (strcmp(argument, "-print-search-dirs") == 0)
                {
                    // Do not process anything
                    if (!dry_run)
                        CURRENT_CONFIGURATION->do_not_process_files = 1;
                }
                else if (strprefix(argument, "-print-prog-name"))
                {
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
            if (argument[2] == 'W'
                    && (strlen(argument) > strlen("--Wx,")))
            {
                if (!dry_run)
                    parse_subcommand_arguments(&argument[3]);
                (*should_advance)++;
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
            fprintf(stderr, "Debug flag '%s' unknown. Ignoring it\n", flag);
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
    if ((strlen(arguments) <= 2)
            || (arguments[1] != ',')
            || (arguments[0] != 'n'
                && arguments[0] != 'p'
                && arguments[0] != 'l'))
    {
        options_error("Option -W is of the form -Wx, where 'x' can be 'n', 'p' or 'l'");
    }

    int num_parameters = 0;
    const char** parameters = comma_separate_values(&arguments[2], &num_parameters);

    const char*** existing_options = NULL;

    switch (arguments[0])
    {
        case 'n' :
            existing_options = &CURRENT_CONFIGURATION->native_compiler_options;
            break;
        case 'p' :
            existing_options = &CURRENT_CONFIGURATION->preprocessor_options;
            break;
        case 'l' :
            existing_options = &CURRENT_CONFIGURATION->linker_options;
            break;
        default:
            {
                internal_error("Unknown '%c' switch\n", arguments[0]);
            }
    }

    add_to_parameter_list(existing_options, parameters, num_parameters);
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
}

static void register_default_initializers(void)
{
    register_dynamic_initializer(build_scope_dynamic_initializer);
    register_dynamic_initializer(scope_entry_dynamic_initializer);
}

static void print_version(void)
{
    fprintf(stdout, PACKAGE " " VERSION " (" MCXX_BUILD_VERSION ")\n");
}

// Callback called for every [section] in the config file
static int section_callback(const char* sname, const char* base_name)
{
    char section_name[128];
    strncpy(section_name, sname, 127);
    section_name[127] = '\0';

    compilation_configuration_t* base_config = NULL; 
    if (base_name != NULL)
    {
        base_config = get_compilation_configuration(base_name);

        if (base_config == NULL)
        {
            fprintf(stderr, "Base configuration '%s' does not exist. Ignoring\n",
                    base_name);
        }
    }

    // Create the new configuration
    compilation_configuration_t* new_configuration = 
        new_compilation_configuration(section_name, base_config);

    new_configuration->type_environment = default_environment;

    // Set now as the current compilation configuration (kludgy)
    SET_CURRENT_CONFIGURATION(new_configuration);

    if (get_compilation_configuration(section_name) != NULL)
    {
        fprintf(stderr, "Warning: configuration profile '%s' already exists!\n",
                section_name);
    }

    P_LIST_ADD(compilation_process.configuration_set, 
            compilation_process.num_configurations, 
            new_configuration);

    return 0;
}

// Callback called in every parameter=value line in the  config file
static int parameter_callback(const char* parameter, const char* value, int num_flags, const char** flags)
{
    if (value == NULL)
    {
        fprintf(stderr, "Value of configuration directive '%s' is empty and will be ignored\n",
                parameter);
        return 0;
    }

    struct compilation_configuration_line * new_configuration_line;

    // Create a new configuration line
    new_configuration_line = calloc(1, sizeof(*new_configuration_line));

    new_configuration_line->name = uniquestr(parameter);
    new_configuration_line->value = uniquestr(value);

    new_configuration_line->num_flags = num_flags;
    new_configuration_line->flags = 
        calloc(new_configuration_line->num_flags, sizeof(*new_configuration_line->flags));

    // Associate its flags
    {
        int i;
        for (i = 0; i < new_configuration_line->num_flags; i++)
        {
            const char *current_flag = NULL;
            char is_negative = 0;

            // If the flag is '!flag'
            if (flags[i][0] == '!')
            {
                // Do not copy '!'
                current_flag = uniquestr(&(flags[i][1]));
                // And state it is negative
                is_negative = 1;
            }
            else
            {
                // Otherwise just keep the flag
                current_flag = uniquestr(flags[i]);
            }

            new_configuration_line->flags[i].flag = current_flag;
            new_configuration_line->flags[i].value = !is_negative;

            {
                // Now register in compilation process as valid flag
                char found = 0;
                int j;
                for (j = 0; !found && (j < compilation_process.num_parameter_flags); j++)
                {
                    found |= (strcmp(current_flag, compilation_process.parameter_flags[j]->name) == 0);
                }

                if (!found)
                {
                    struct parameter_flags_tag *new_parameter_flag = calloc(1, sizeof(*new_parameter_flag));

                    new_parameter_flag->name = current_flag;
                    // This is redundant because of calloc, but make it explicit here anyway
                    new_parameter_flag->value = 0;

                    P_LIST_ADD(compilation_process.parameter_flags, 
                            compilation_process.num_parameter_flags,
                            new_parameter_flag);
                }
            }
        }
    }

    P_LIST_ADD(CURRENT_CONFIGURATION->configuration_lines,
            CURRENT_CONFIGURATION->num_configuration_lines,
            new_configuration_line);

    return 0;
}

static void load_configuration_file(const char *filename)
{
    // Will invoke section_callback and parameter_callback for every section
    // and parameter
    int result = param_process(filename, section_callback, parameter_callback);

    switch (result)
    {
        case PPR_OPEN_FILE_ERROR :
            {
                fprintf(stderr, "Configuration file '%s' could not be opened. Skipping\n",
                        filename);
                // This has already been done in initialize_default_values
                break;
            }
        case PPR_PARSE_ERROR :
            {
                fprintf(stderr, "Configuration file '%s' is ill-formed. Check its syntax. Skipping\n",
                        filename);
                break;
            }
        case PPR_MALLOC_ERROR :
            {
                internal_error("Could not allocate memory for configuration file parsing", 0);
                break;
            }
        case PPR_SUCCESS :
            {
                // Everything went well for this file
                break;
            }
       default :
            {
                internal_error("Function param_process returned an invalid value %d", result);
            }
    }
}

static void load_configuration(void)
{
    // Solve here the egg and chicken problem of the option --config-file
    // FIXME - save config directory properly
    int i;
    for (i = 1; i < compilation_process.argc; i++)
    {
        if (strncmp(compilation_process.argv[i], 
                    "--config-file=", strlen("--config-file=")) == 0)
        {
            compilation_process.config_file = 
                uniquestr(&(compilation_process.argv[i][strlen("--config-file=") ]));
        }
        else if (strncmp(compilation_process.argv[i], 
                    "--profile=", strlen("--profile=")) == 0)
        {
            // Change the basename, from now it will look like the compiler
            // has been called as this basename
            compilation_process.exec_basename =
                uniquestr(&(compilation_process.argv[i][strlen("--profile=") ]));
        }
    }

    load_configuration_file(compilation_process.config_file);

    // Now load all files in the config_dir
    DIR* config_dir = opendir(compilation_process.config_dir);
    if (config_dir == NULL)
    {
        if (errno != ENOENT)
        {
            // Only give an error if it does exist
            fprintf(stderr, "Could not open configuration directory (%s)\n", 
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
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        SET_CURRENT_CONFIGURATION(get_compilation_configuration(compilation_process.exec_basename));
    }

    if (CURRENT_CONFIGURATION == NULL)
    {
        fprintf(stderr, "No suitable configuration defined for %s. Setting to C++ built-in configuration\n",
               compilation_process.exec_basename);
        SET_CURRENT_CONFIGURATION(&minimal_default_configuration);
    }
    
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
                fprintf(stderr, "Configuration directive '%s' skipped since it is unknown\n", configuration_line->name);
                continue;
            }

            // Check the value of flags before processing this configuration line
            char can_be_committed = 1;

            {
                // This is an ugly and stupid O(n^2) algorithm
                int k;
                // For every flag of this configuration line
                for (k = 0; can_be_committed && (k < configuration_line->num_flags); k++)
                {
                    // Check it against every seen parameter flag
                    int q;
                    char found = 0;
                    for (q = 0; can_be_committed && (q < compilation_process.num_parameter_flags); q++)
                    {
                        struct parameter_flags_tag *parameter_flag = compilation_process.parameter_flags[q];

                        if (strcmp(parameter_flag->name, configuration_line->flags[k].flag) == 0)
                        {
                            found = 1;
                            can_be_committed = can_be_committed && 
                                (parameter_flag->value == configuration_line->flags[k].value);
                        }
                    }

                    if (!found)
                    {
                        // If not found, then this is an error, so do not commit
                        WARNING_MESSAGE("Not found an implicit flag '%s'", configuration_line->flags[k].flag);
                        can_be_committed = 0;
                    }
                }
            }

            if (can_be_committed)
            {
                config_directive->funct(configuration, configuration_line->value);
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
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, "hlt");
    // When loading the compiler phase a proper extension will be added
    const char* library_name = "libtl-hlt-pragma";
    P_LIST_ADD(CURRENT_CONFIGURATION->compiler_phases, 
            CURRENT_CONFIGURATION->num_compiler_phases, 
            library_name);
}

// FIXME: This should be in cxx-upc.c, but that file belongs to the frontend
// where we cannot call driver functions, so we will implement here
// maybe a better file to put it would be cxx-upc-driver.c
static void register_upc_pragmae(void)
{
    // Register '#pragma upc'
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, "upc");
    // Lexer already uses CURRENT_CONFIGURATION this is why it is not specified here
    // Register '#pragma upc relaxed'
    register_new_directive("upc", "relaxed", /* is_construct */ 0);
    // Register '#pragma upc strict'
    register_new_directive("upc", "strict", /* is_construct */ 0);

    // mfarrera's + IBM UPC extension that annoyingly it is not prefixed with
    // 'upc' (as it ought to be!)
    config_add_preprocessor_prefix(CURRENT_CONFIGURATION, "distributed");
    // Register the empty directive since the syntax is '#pragma distributed'
    register_new_directive("distributed", "", /* is_construct */ 0);
}

static void compile_every_translation_unit(void)
{
    int i;
    for (i = 0; i < compilation_process.num_translation_units; i++)
    {
        compilation_file_process_t* file_process = compilation_process.translation_units[i];

        // Ensure we do not get in a strange loop
        if (file_process->already_compiled)
            continue;

        // Note: This is the only place where CURRENT_CONFIGURATION can be
        // changed everywhere else these two variables are constants
        SET_CURRENT_CONFIGURATION(file_process->compilation_configuration);
        SET_CURRENT_COMPILED_FILE(file_process->translation_unit);

        translation_unit_t* translation_unit = CURRENT_COMPILED_FILE;

        // Ensure phases are loaded for current profile
        load_compiler_phases(CURRENT_CONFIGURATION);
        
        // First check the file type
        const char* extension = get_extension_filename(translation_unit->input_filename);

        struct extensions_table_t* current_extension = NULL;

        if (extension == NULL 
                || ((current_extension =
                        fileextensions_lookup(extension, strlen(extension))) == NULL))
        {
            fprintf(stderr, "File '%s' not recognized as a valid input. Passing verbatim on to the linker.\n", 
                    translation_unit->input_filename);
            translation_unit->output_filename = translation_unit->input_filename;
            continue;
        }

        if (current_extension->source_language == SOURCE_LANGUAGE_LINKER_DATA)
        {
            translation_unit->output_filename = translation_unit->input_filename;
            continue;
        }

        if (!CURRENT_CONFIGURATION->force_language
				&& (current_extension->source_language != CURRENT_CONFIGURATION->source_language))
        {
            fprintf(stderr, "%s was configured for %s language but file '%s' looks %s language. Skipping it.\n",
                    compilation_process.exec_basename, 
                    source_language_names[CURRENT_CONFIGURATION->source_language],
                    translation_unit->input_filename,
                    source_language_names[current_extension->source_language]);
            continue;
        }

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Compiling file '%s'\n", translation_unit->input_filename);
        }

        const char* parsed_filename = translation_unit->input_filename;
        if (current_extension->source_kind == SOURCE_KIND_NOT_PREPROCESSED
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

        if (!CURRENT_CONFIGURATION->do_not_parse)
        {
            if (!CURRENT_CONFIGURATION->pass_through)
            {
                // 0. Do this before open for scan since we might to internally parse some sources
                mcxx_flex_debug = mc99_flex_debug = CURRENT_CONFIGURATION->debug_options.debug_lexer;
                mcxxdebug = mc99debug = CURRENT_CONFIGURATION->debug_options.debug_parser;

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
                // 2. Parse file
                parse_translation_unit(translation_unit, parsed_filename);
                // 3. Close file
                close_scanned_file();

                // 4. TL::pre_run
                compiler_phases_pre_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // 5. Semantic analysis
                semantic_analysis(translation_unit, parsed_filename);

                // 6. TL::run
                compiler_phases_execution(CURRENT_CONFIGURATION, translation_unit, parsed_filename);

                // 7. print ast if requested
                if (CURRENT_CONFIGURATION->debug_options.print_ast)
                {
                    fprintf(stderr, "Printing AST in graphviz format\n");

                    ast_dump_graphviz(translation_unit->parsed_tree, stdout);
                }

                // 8. print symbol table if requested
                if (CURRENT_CONFIGURATION->debug_options.print_scope)
                {
                    fprintf(stderr, "============ SYMBOL TABLE ===============\n");
                    print_scope(translation_unit->global_decl_context);
                    fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
                }
            }

            const char* prettyprinted_filename = prettyprint_translation_unit(translation_unit, parsed_filename);
            native_compilation(translation_unit, prettyprinted_filename);
        }

        file_process->already_compiled = 1;
    }
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

    if (parse_result != 0)
    {
        running_error("Compilation failed for file '%s'\n", translation_unit->input_filename);
    }

    // --
    // Concatenate trees
    AST existing_list_of_decls = ASTSon0(translation_unit->parsed_tree);
    AST concatenated = ast_list_concat(existing_list_of_decls, parsed_tree);
    ast_set_child(translation_unit->parsed_tree, 0, concatenated);
    // --

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
    initialize_translation_unit_scope(translation_unit);
}

static void semantic_analysis(translation_unit_t* translation_unit, const char* parsed_filename)
{
    timing_t timing_semantic;

    timing_start(&timing_semantic);
    build_scope_translation_unit(translation_unit);
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
        output_filename_basename = strappend(preffix,
                input_filename_basename);

        if (CURRENT_CONFIGURATION->output_directory == NULL)
        {
            const char* input_filename_dirname = give_dirname(translation_unit->input_filename);
            input_filename_dirname = strappend(input_filename_dirname, "/");

            output_filename = strappend(input_filename_dirname,
                    output_filename_basename);
        }
        else
        {
            output_filename = strappend(CURRENT_CONFIGURATION->output_directory, "/");
            output_filename = strappend(output_filename, output_filename_basename);
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
    prettyprint(prettyprint_file, translation_unit->parsed_tree);

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
    int num_arguments = count_null_ended_array((void**)CURRENT_CONFIGURATION->preprocessor_options);

    char uses_stdout = CURRENT_CONFIGURATION->preprocessor_uses_stdout;

    int num_parameters;

    if (!uses_stdout)
    {
        num_parameters = num_arguments + 3 + 1;
    }
    else
    {
        // '-o' 'file' are not passed
        num_parameters = num_arguments + 1 + 1;
    }

    const char** preprocessor_options = calloc(num_parameters, sizeof(char*));

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

static void native_compilation(translation_unit_t* translation_unit, 
        const char* prettyprinted_filename)
{
    if (CURRENT_CONFIGURATION->do_not_compile)
        return;

    const char* output_object_filename;

    if (translation_unit->output_filename == NULL
            || !CURRENT_CONFIGURATION->do_not_link)
    {
        char temp[256];
        strncpy(temp, translation_unit->input_filename, 255);
        temp[255] = '\0';
        char* p = strrchr(temp, '.');
        if (p != NULL)
        {
            *p = '\0';
        }

        output_object_filename = strappend(temp, ".o");

        translation_unit->output_filename = output_object_filename;
    }
    else
    {
        output_object_filename = translation_unit->output_filename;
    }

    int num_args_compiler = count_null_ended_array((void**)CURRENT_CONFIGURATION->native_compiler_options);

    const char** native_compilation_args = calloc(num_args_compiler + 4 + 1, sizeof(*native_compilation_args));

    int i;
    for (i = 0; i < num_args_compiler; i++)
    {
        native_compilation_args[i] = CURRENT_CONFIGURATION->native_compiler_options[i];
    }

    native_compilation_args[i] = uniquestr("-c");
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

    if (!CURRENT_CONFIGURATION->keep_files)
    {
        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Removing prettyprinted file '%s'\n", prettyprinted_filename);
        }
        remove(prettyprinted_filename);
    }
}

static void link_objects(void)
{
    if (CURRENT_CONFIGURATION->do_not_link)
        return;

    int num_args_linker = count_null_ended_array((void**)CURRENT_CONFIGURATION->linker_options);

    const char** linker_args = calloc(num_args_linker
            + compilation_process.num_translation_units + 2 + 1, 
            sizeof(*linker_args));

    int i = 0;
    int j = 0;

    if (CURRENT_CONFIGURATION->linked_output_filename != NULL)
    {
        linker_args[i] = uniquestr("-o");
        i++;
        linker_args[i] = CURRENT_CONFIGURATION->linked_output_filename;
        i++;
    }

    for (j = 0; j < compilation_process.num_translation_units; j++)
    {
        linker_args[i] = compilation_process.translation_units[j]->translation_unit->output_filename;
        i++;
    }

    for (j = 0; j < num_args_linker; j++)
    {
        linker_args[i] = CURRENT_CONFIGURATION->linker_options[j];
        i++;
    }

    timing_t timing_link;
    timing_start(&timing_link);
    if (execute_program(CURRENT_CONFIGURATION->linker_name, linker_args) != 0)
    {
        running_error("Link failed", 0);
    }
    timing_end(&timing_link);

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Link performed in %.2f seconds\n", 
                timing_elapsed(&timing_link));
    }
}


#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
static void terminating_signal_handler(int sig)
{
    fprintf(stderr, "Signal handler called (signal=%d). Exiting.\n", sig);

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
        prettyprint(stderr, ambiguous_node);
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
    // This is also checked in cxx-compilerphases.cpp but here we avoid showing
    // the timing message as well
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


#ifndef WIN32_BUILD
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
#ifndef WIN32_BUILD
    char c[256];

    struct mallinfo mallinfo_report = mallinfo();

    fprintf(stderr, "\n");
    fprintf(stderr, "Memory report\n");
    fprintf(stderr, "-------------\n");
    fprintf(stderr, "\n");

    print_human(c, mallinfo_report.arena);
    fprintf(stderr, " - Total size of memory allocated with sbrk: %s\n",
            c);

    fprintf(stderr, " - Number of chunks not in use: %d\n",
            mallinfo_report.ordblks);
    fprintf(stderr, " - Number of chunks allocated with mmap: %d\n",
            mallinfo_report.hblks);

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

    accounted_memory += hash_used_memory();
    print_human(c, hash_used_memory());
    fprintf(stderr, " - Memory usage due to hash tables: %s\n", c);

    accounted_memory += dynamic_lists_used_memory();
    print_human(c, dynamic_lists_used_memory());
    fprintf(stderr, " - Memory usage due to dynamic lists: %s\n", c);

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
    fprintf(stderr, "Memory statistics are not implemented in Windows\n");
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

#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
// This function is Linux only!
// Note: cygwin also provides a useful /proc (amazing!)
static char* getexename(char* buf, size_t size)
{
	char linkname[64]; /* /proc/<pid>/exe */
	pid_t pid;
	int ret;
	
	/* Get our PID and build the name of the link in /proc */
	pid = getpid();
	
	if (snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid) < 0)
		{
		/* This should only happen on large word systems. I'm not sure
		   what the proper response is here.
		   Since it really is an assert-like condition, aborting the
		   program seems to be in order. */
		abort();
		}

	
	/* Now read the symbolic link */
	ret = readlink(linkname, buf, size);
	
	/* In case of an error, leave the handling up to the caller */
	if (ret == -1)
		return NULL;
	
	/* Report insufficient buffer size */
	if (ret >= size)
		{
		errno = ERANGE;
		return NULL;
		}
	
	/* Ensure proper NUL termination */
	buf[ret] = 0;
	
	return buf;
}

static const char* find_home_linux(void)
{
    char c[1024];
    if (getexename(c, sizeof(c)) == NULL)
    {
        internal_error("Error when running getexename = %s\n", strerror(errno));
    }

    return uniquestr(dirname(c));
}

#else 
// Version for mingw
static const char* find_home_win32(void)
{
    char c[1024];
    GetModuleFileName(0, c, sizeof(c));

    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];

    _splitpath(c, drive, dir, fname, ext);

    const char* result = strappend(drive, dir);
    return result;
}
#endif

static const char* find_home(void)
{
#if !defined(WIN32_BUILD) || defined(__CYGWIN__)
    return find_home_linux();
#else
    return find_home_win32();
#endif
}
