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
#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>

#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-lexer.h"
#include "cxx-dyninit.h"
#include "cxx-printscope.h"
// It does not include any C++ code in the header
#include "cxx-compilerphases.hpp"
#include "mcfg.h"


// Compilation options
compilation_process_t compilation_process;

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
"  -y                       Only parsing will be performed\n" \
"                           No file will be generated\n" \
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
"  --Wp,<options>           Pass comma-separated <options> on to\n" \
"                           the preprocessor\n" \
"  --Wn,<options>           Pass comma-separated <options> on to\n" \
"                           the native compiler\n" \
"  --Wl,<options>           Pass comma-separated <options> on to\n" \
"                           the linker\n" \
"  --no-openmp              Disables OpenMP 2.5 support\n" \
"  --config-file=<file>     Uses <file> as config file, otherwise\n" \
"                           '" PKGDATADIR "/config.mcxx'\n" \
"                           will be used\n" \
"  --profile=<name>         Selects profile compilation to be <name>\n" \
"  --variable=<name:value>  Defines variable 'name' with value\n" \
"                           'value' to be used in the compiler\n" \
"                           phases pipeline\n" \
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
"\n" \
"These gcc flags are passed verbatim to preprocessor, compiler and\n" \
"linker.\n" \
"\n"
/* ------------------------------------------------------------------ */

// It mimics getopt
#define SHORT_OPTIONS_STRING "vkadcho:EyI:L:l:gD:x:"
// This one mimics getopt_long but with one less field (the third one is not given)
struct command_line_long_options command_line_long_options[] =
{
    {"help",        CLP_NO_ARGUMENT, 'h'},
    {"version",     CLP_NO_ARGUMENT, OPTION_VERSION},
    {"verbose",     CLP_NO_ARGUMENT, 'v'},
    {"keep-files",  CLP_NO_ARGUMENT, 'k'},
    {"check-dates", CLP_NO_ARGUMENT, 'a'},
    {"debug",       CLP_NO_ARGUMENT, 'd'},
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
    // sentinel
    {NULL, 0, 0}
};

char* source_language_names[] =
{
    [SOURCE_LANGUAGE_UNKNOWN] = "unknown",
    [SOURCE_LANGUAGE_C] = "C",
    [SOURCE_LANGUAGE_CXX] = "C++"
};

int num_seen_file_names;
char** seen_file_names;

static void print_version(void);
static void driver_initialization(int argc, char* argv[]);
static void initialize_default_values(void);
static void load_configuration(void);
static void compile_every_translation_unit(void);

static void compiler_phases_execution(translation_unit_t* translation_unit, char* parsed_filename);
static char* preprocess_file(translation_unit_t* translation_unit, char* input_filename);
static void parse_translation_unit(translation_unit_t* translation_unit, char* parsed_filename);
static char* prettyprint_translation_unit(translation_unit_t* translation_unit, char* parsed_filename);
static void native_compilation(translation_unit_t* translation_unit, char* prettyprinted_filename);

static void terminating_signal_handler(int sig);
static char check_tree(AST a);
static char check_for_ambiguities(AST a, AST* ambiguous_node);

static void link_objects(void);

static void add_to_parameter_list_str(char*** existing_options, char* str);
static void parse_subcommand_arguments(char* arguments);

static void enable_debug_flag(char* flag);

static void load_compiler_phases(void);

static void register_default_initializers(void);

static void help_message(void);

static int parse_special_parameters(int *index, int argc, char* argv[]);

int main(int argc, char* argv[])
{
    timing_t timing_global;
    timing_start(&timing_global);

    // Initialization of the driver
    driver_initialization(argc, argv);

    // Default values
    initialize_default_values();

    // Register default initializers
    register_default_initializers();

    // Load configuration
    load_configuration();
    
    // Parse arguments
    char parse_arguments_error;
    parse_arguments_error = parse_arguments(compilation_process.argc,
            compilation_process.argv, /* from_command_line= */1);
    
    // Loads the compiler phases
    load_compiler_phases();

    if (parse_arguments_error)
    {
        help_message();
        exit(EXIT_FAILURE);
    }

    // Compiler phases can define additional dynamic initializers
    // (besides the built in ones)
    run_dynamic_initializers();
    
    // Compilation of every specified translation unit
    compile_every_translation_unit();

    // Link all generated objects
    link_objects();

    timing_end(&timing_global);
    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Whole process took %.2f seconds to complete\n",
                timing_elapsed(&timing_global));
    }

    return compilation_process.execution_result;
}

static volatile char in_cleanup_routine = 0;

static void cleanup_routine(void)
{
    in_cleanup_routine = 1;
    signal(SIGSEGV, SIG_DFL);
    signal(SIGQUIT, SIG_DFL);
    signal(SIGINT,  SIG_DFL);
    signal(SIGTERM, SIG_DFL);
    temporal_files_cleanup();
    in_cleanup_routine = 0;
}

static void driver_initialization(int argc, char* argv[])
{
    // Basic initialization prior to argument parsing and configuration loading
    atexit(cleanup_routine);
    signal(SIGSEGV, terminating_signal_handler);
    signal(SIGQUIT, terminating_signal_handler);
    signal(SIGINT,  terminating_signal_handler);
    signal(SIGTERM, terminating_signal_handler);

    memset(&compilation_process, 0, sizeof(compilation_process));
    compilation_process.argc = argc;
    compilation_process.argv = argv;
    compilation_process.exec_basename = give_basename(argv[0]);

    num_seen_file_names = 0;
    seen_file_names = 0;
}

static void help_message(void)
{
    fprintf(stderr, "Usage: %s options file [file..]\n", compilation_process.argv[0]);
    fprintf(stderr, HELP_STRING);

    phases_help();

    fprintf(stderr, "\n");
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
    help_message();
    exit(EXIT_FAILURE);
}



// Returns nonzero if an error happened. In that case we would show the help
// Messages issued here must be ended with \n for sthetic reasons
int parse_arguments(int argc, char* argv[], char from_command_line)
{
    char* output_file = NULL;

    // It is 1 because we ignore the first argument, since it is the name of
    // the program invocation
    int parameter_index = 1;

    // Flags -E/-y and -c are incompatible
    static char c_specified = 0;
    static char E_specified = 0;
    static char y_specified = 0;

    char **input_files = NULL;
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
            if (parse_special_parameters(&parameter_index, argc, argv))
            {
                fprintf(stderr, "Unknown parameter '%s'\n", argv[parameter_index]);
                return 1;
            }
        }
        // A plain parameter (not under the hood of any option)
        else if (parameter_info.flag == CLP_PLAIN_PARAMETER)
        {
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
                        CURRENT_CONFIGURATION(verbose) = 1;
                        break;
                    }
                case 'k' : // --keep-files || -k
                    {
                        CURRENT_CONFIGURATION(keep_files) = 1;
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

                        CURRENT_CONFIGURATION(do_not_link) = 1;
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

                        CURRENT_CONFIGURATION(do_not_compile) = 1;
                        CURRENT_CONFIGURATION(do_not_link) = 1;
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

                        CURRENT_CONFIGURATION(do_not_compile) = 1;
                        CURRENT_CONFIGURATION(do_not_link) = 1;
                        CURRENT_CONFIGURATION(do_not_prettyprint) = 1;
                        break;
                    }
                case 'a' : // --check-dates || -a
                    {
                        CURRENT_CONFIGURATION(check_dates) = 1;
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
                            output_file = strdup(parameter_info.argument);
                        }
                        break;
                    }
                case 'I' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-I%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION(preprocessor_options), temp);
                        break;
                    }
                case 'L' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-L%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION(linker_options), temp);
                        break;
                    }
                case 'l' : 
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-l%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION(linker_options), temp);
                        break;
                    }
                case 'D' :
                    {
                        char temp[256] = { 0 };
                        snprintf(temp, 255, "-D%s", parameter_info.argument);
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION(preprocessor_options), temp);
                        break;
                    }
                case 'g' :
                    {
                        add_to_parameter_list_str(&CURRENT_CONFIGURATION(native_compiler_options), "-g");
                        break;
                    }
                case 'x' :
                    {
                        if (strcasecmp(parameter_info.argument, "C") == 0)
                        {
                            CURRENT_CONFIGURATION(force_language) = 1;
                            CURRENT_CONFIGURATION(source_language) = SOURCE_LANGUAGE_C;
                        }
                        else if (strcasecmp(parameter_info.argument, "C++") == 0)
                        {
                            CURRENT_CONFIGURATION(force_language) = 1;
                            CURRENT_CONFIGURATION(source_language) = SOURCE_LANGUAGE_CXX;
                        }
                        else
                        {
                            fprintf(stderr, "Invalid language specification in -x, valid options are 'C' or 'C++'. Ignoring\n");
                        }

                        break;
                    }
                case OPTION_PREPROCESSOR_NAME :
                    {
                        CURRENT_CONFIGURATION(preprocessor_name) = strdup(parameter_info.argument);
                        break;
                    }
                case OPTION_NATIVE_COMPILER_NAME :
                    {
                        CURRENT_CONFIGURATION(native_compiler_name) = strdup(parameter_info.argument);
                        break;
                    }
                case OPTION_LINKER_NAME :
                    {
                        CURRENT_CONFIGURATION(linker_name) = strdup(parameter_info.argument);
                        break;
                    }
                case OPTION_DEBUG_FLAG :
                    {
                        enable_debug_flag(strdup(parameter_info.argument));
                        break;
                    }
                case OPTION_OUTPUT_DIRECTORY :
                    {
                        CURRENT_CONFIGURATION(output_directory) = strdup(parameter_info.argument);
                        break;
                    }
                case OPTION_NO_OPENMP :
                    {
                        CURRENT_CONFIGURATION(disable_openmp) = 1;
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

                        char* name = strdup(parameter_info.argument);
                        char* value = strchr(name, ':');
                        *value = '\0';
                        value++;

                        external_var_t* new_external_var = calloc(1, sizeof(*new_external_var));

                        new_external_var->name = name;
                        new_external_var->value = value;

                        P_LIST_ADD(CURRENT_CONFIGURATION(external_vars), CURRENT_CONFIGURATION(num_external_vars),
                                new_external_var);
                        break;
                    }
                case 'h' :
                    {
                        return 1;
                    }
                default:
                    {
                        internal_error("Unhandled option\n", 0);
                    }
            }
        }
    }

    if (!from_command_line)
    {
        return 0;
    }


    // Create translation units
    
    if (num_input_files == 0)
    {
        fprintf(stderr, "You must specify an input file\n");
        return 1;
    }
    // "-o -" is not valid when compilation or linking will be done
    if (output_file != NULL
            && (strcmp(output_file, "-") == 0)
            && !E_specified)
    {
        fprintf(stderr, "You must specify an output file.\n");
        return 1;
    }
    // If -E has been specified and no output file has been, assume it is "-"
    if (output_file == NULL
            && E_specified)
    {
        fprintf(stderr, "Assuming stdout as default output since -E has been specified\n");
        output_file = strdup("-");
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

    // This is the right place to sign in input files, never before of complete
    // command parsing
    int i;
    for (i = 0; i < num_input_files; i++)
    {
        add_new_file_to_compilation_process(input_files[i],
                output_file, compilation_process.current_compilation_configuration);
    }

    // If some output was given by means of -o and we are linking (so no -c neither -E)
    // then, this output is the overall compilation process output
    if (output_file != NULL)
    {
        if (!CURRENT_CONFIGURATION(do_not_link))
        {
            CURRENT_CONFIGURATION(linked_output_filename) = output_file;
        }
    }

    return 0;
}

static void add_parameter_all_toolchain(char *argument)
{
    add_to_parameter_list_str(&CURRENT_CONFIGURATION(preprocessor_options), argument);
    add_to_parameter_list_str(&CURRENT_CONFIGURATION(native_compiler_options), argument);
    add_to_parameter_list_str(&CURRENT_CONFIGURATION(linker_options), argument);
}

static int parse_special_parameters(int *index, int argc, char* argv[])
{
    // FIXME: This function should use gperf-ectionated
    // This code can be written better
    int failure = 0;

    char *argument = argv[*index];

    // argument[0] == '-'
    switch (argument[1])
    {
        // GCC parameters
        case 'f':
        case 'm':
            {
                add_parameter_all_toolchain(argument);
                (*index)++;
                break;
            }
        case 'M' :
            {
                if ((argument[2] == '\0') // -M
                        || ((argument[2] == 'P') && (argument[3] == '\0')) // -MP
                        || ((argument[2] == 'D') && (argument[3] == '\0')) // -MD
                        || ((argument[2] == 'M') && (argument[3] == 'D') && (argument[4] == '\0'))) // -MMD
                {
                    (*index)++;
                    add_parameter_all_toolchain(argument);
                }
                else if (((argument[2] == 'F') && (argument[3] == '\0')) // -MF
                        || ((argument[2] == 'G') && (argument[3] == '\0')) // -MG
                        || ((argument[2] == 'T') && (argument[3] == '\0'))) // -MT
                {
                    add_parameter_all_toolchain(argument);
                    (*index)++;

                    // Pass the next argument too
                    argument = argv[*index];
                    add_parameter_all_toolchain(argument);
                    (*index)++;
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
                    add_parameter_all_toolchain(argument);
                    (*index)++;
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
                    add_parameter_all_toolchain(argument);
                    (*index)++;
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
                    strtol(&(argument[2]), &error, 10);

                    if (*error != '\0')
                    {
                        failure = 1;
                    }
                }

                if (!failure)
                {
                    add_parameter_all_toolchain(argument);
                    (*index)++;
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
                    add_parameter_all_toolchain(argument);
                    (*index)++;
                }
                break;
            }
        case 'W' :
            {
                if (strlen(argument) > strlen("-W"))
                {
                    add_parameter_all_toolchain(argument);
                    (*index)++;
                }
                else
                {
                    failure = 1;
                }
                break;
            }
        case '-' :
        {
            if (argument[2] == 'W'
                    && (strlen(argument) > strlen("--Wx,")))
            {
                parse_subcommand_arguments(&argument[3]);
                (*index)++;
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

static void enable_debug_flag(char* flags)
{
    int num_flags = 0;
    char** flag_list = comma_separate_values(flags, &num_flags);

    int i;
    for (i = 0; i < num_flags; i++)
    {
        char* flag = flag_list[i];

        struct debug_flags_list_t* flag_option = 
            debugflags_lookup (flag, strlen(flag));

        if (flag_option != NULL)
        {
            // *(flag_option->flag_pointer) = 1;
            *((char*)(&CURRENT_CONFIGURATION(debug_options)) + flag_option->flag_offset) = 1;
        }
        else
        {
            fprintf(stderr, "Debug flag '%s' unknown. Ignoring it\n", flag);
        }
    }
}

static void add_to_parameter_list_str(char*** existing_options, char* str)
{
    char* d_str = strdup(str);
    add_to_parameter_list(existing_options, &d_str, 1);
}

void add_to_parameter_list(char*** existing_options, char **parameters, int num_parameters)
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

static void parse_subcommand_arguments(char* arguments)
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
    char** parameters = comma_separate_values(&arguments[2], &num_parameters);

    char*** existing_options = NULL;

    switch (arguments[0])
    {
        case 'n' :
            existing_options = &CURRENT_CONFIGURATION(native_compiler_options);
            break;
        case 'p' :
            existing_options = &CURRENT_CONFIGURATION(preprocessor_options);
            break;
        case 'l' :
            existing_options = &CURRENT_CONFIGURATION(linker_options);
            break;
        default:
            {
                internal_error("Unknown '%c' switch\n", arguments[0]);
            }
    }

    add_to_parameter_list(existing_options, parameters, num_parameters);
}

static compilation_configuration_t minimal_default_configuration;
static void initialize_default_values(void)
{
    int dummy = 0;
    // Initialize here all default values
    compilation_process.config_file = PKGDATADIR "/config.mcxx";
    compilation_process.num_translation_units = 0;

    // The minimal default configuration
    memset(&minimal_default_configuration, 0, sizeof(minimal_default_configuration));
    compilation_process.current_compilation_configuration = &minimal_default_configuration;

    CURRENT_CONFIGURATION(source_language) = SOURCE_LANGUAGE_CXX;

    CURRENT_CONFIGURATION(preprocessor_name) = strdup("c++");
    CURRENT_CONFIGURATION(preprocessor_options) = comma_separate_values(strdup("-E"), &dummy);

    CURRENT_CONFIGURATION(native_compiler_name) = strdup("c++");
    CURRENT_CONFIGURATION(native_compiler_options) = NULL;

    CURRENT_CONFIGURATION(linker_name) = strdup("c++");
    CURRENT_CONFIGURATION(linker_options) = NULL;
}

static void register_default_initializers(void)
{
    register_dynamic_initializer(build_scope_dynamic_initializer);
}

static void print_version(void)
{
    fprintf(stderr, PACKAGE " - " VERSION " " MCXX_BUILD_VERSION "\n");
}

// Callback called for every [section] in the config file
static int section_callback(char* sname)
{
    char section_name[128];
    strncpy(section_name, sname, 127);
    section_name[127] = '\0';

    // Create the new configuration
    compilation_configuration_t* new_compilation_configuration = calloc(1, sizeof(*new_compilation_configuration));
    new_compilation_configuration->configuration_name = strdup(section_name);

    // Set now as the current compilation configuration
    compilation_process.current_compilation_configuration = new_compilation_configuration;

    // Check repeated configurations
    int i;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        if (strcmp(compilation_process.configuration_set[i]->configuration_name, section_name) == 0)
        {
            fprintf(stderr, "Warning: configuration for '%s' defined more than once\n", section_name);
        }
    }

    P_LIST_ADD(compilation_process.configuration_set, 
            compilation_process.num_configurations, 
            new_compilation_configuration);

    return 0;
}

// Callback called in every parameter=value line in the  config file
static int parameter_callback(char* parameter, char* value)
{
    if (value == NULL)
    {
        fprintf(stderr, "Value of configuration directive '%s' is empty and will be ignored\n",
                parameter);
        return 0;
    }

    struct configuration_directive_t* config_directive =
        configoptions_lookup(parameter, strlen(parameter));

    if (config_directive == NULL)
    {
        fprintf(stderr, "Configuration directive '%s' skipped since it is unknown\n", parameter);
        return 0;
    }

    config_directive->funct(value);
    return 0;
}

static void load_configuration(void)
{
    // Solve here the egg and chicken problem of the option --config-file / -m
    int i;
    for (i = 1; i < compilation_process.argc; i++)
    {
        if (strncmp(compilation_process.argv[i], 
                    "--config-file=", strlen("--config-file=")) == 0)
        {
            compilation_process.config_file = 
                strdup(&(compilation_process.argv[i][strlen("--config-file=") ]));
        }
        else if (strncmp(compilation_process.argv[i], 
                    "--profile=", strlen("--profile=")) == 0)
        {
            // Change the basename, from now it will look like the compiler
            // has been called as this basename
            compilation_process.exec_basename =
                strdup(&(compilation_process.argv[i][strlen("--profile=") ]));
        }
    }

    // Will invoke section_callback and parameter_callback for every section
    // and parameter
    int result = param_process(compilation_process.config_file, MS_STYLE, 
            section_callback, parameter_callback);

    switch (result)
    {
        case PPR_OPEN_FILE_ERROR :
            {
                fprintf(stderr, "Setting to C++ built-in configuration\n");
                // This has already been done in initialize_default_values
                break;
            }
        case PPR_PARSE_ERROR :
            {
                fprintf(stderr, "Config file is ill-formed. Check its syntax\n");
                break;
            }
        case PPR_MALLOC_ERROR :
            {
                internal_error("Could not allocate memory for configuration file parsing", 0);
                break;
            }
        case PPR_SUCCESS :
            {
                // Everything went well
                break;
            }
       default :
            {
                internal_error("Function param_process returned an invalid value %d", result);
            }
    }

    // Now set the configuration as stated by the basename
    compilation_process.current_compilation_configuration = NULL;
    for (i = 0; i < compilation_process.num_configurations; i++)
    {
        if (strcmp(compilation_process.configuration_set[i]->configuration_name, 
                    compilation_process.exec_basename) == 0)
        {
            compilation_process.current_compilation_configuration = compilation_process.configuration_set[i];
        }
    }

    if (compilation_process.current_compilation_configuration == NULL)
    {
        fprintf(stderr, "No suitable configuration defined for %s. Setting to C++ built-in configuration\n",
               compilation_process.exec_basename);
        compilation_process.current_compilation_configuration = &minimal_default_configuration;
    }
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

        compilation_process.current_compilation_configuration = file_process->compilation_configuration;
        compilation_process.current_translation_unit = file_process->translation_unit;

        translation_unit_t* translation_unit = compilation_process.current_translation_unit;
        
        // First check the file type
        char* extension = get_extension_filename(translation_unit->input_filename);

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

        if (!CURRENT_CONFIGURATION(force_language)
				&& (current_extension->source_language != CURRENT_CONFIGURATION(source_language)))
        {
            fprintf(stderr, "%s was configured for %s language but file '%s' looks %s language. Skipping it.\n",
                    compilation_process.exec_basename, 
                    source_language_names[CURRENT_CONFIGURATION(source_language)],
                    translation_unit->input_filename,
                    source_language_names[current_extension->source_language]);
            continue;
        }

        if (CURRENT_CONFIGURATION(verbose))
        {
            fprintf(stderr, "Compiling file '%s'\n", translation_unit->input_filename);
        }

        char* parsed_filename = translation_unit->input_filename;
        if (current_extension->source_kind == SOURCE_KIND_NOT_PREPROCESSED)
        {
            timing_t timing_preprocessing;

            timing_start(&timing_preprocessing);
            parsed_filename = preprocess_file(translation_unit, translation_unit->input_filename);
            timing_end(&timing_preprocessing);

            if (parsed_filename != NULL
                    && CURRENT_CONFIGURATION(verbose))
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

        parse_translation_unit(translation_unit, parsed_filename);

        compiler_phases_execution(translation_unit, parsed_filename);

        if (CURRENT_CONFIGURATION(debug_options.print_ast))
        {
            fprintf(stderr, "Printing AST in graphviz format\n");

            ast_dump_graphviz(translation_unit->parsed_tree, stdout);
        }

        if (CURRENT_CONFIGURATION(debug_options.print_scope))
        {
            fprintf(stderr, "============ SYMBOL TABLE ===============\n");
            print_scope(translation_unit->global_decl_context);
            fprintf(stderr, "========= End of SYMBOL TABLE ===========\n");
        }

        char* prettyprinted_filename = prettyprint_translation_unit(translation_unit, parsed_filename);

        native_compilation(translation_unit, prettyprinted_filename);

        file_process->already_compiled = 1;
    }
}

static void compiler_phases_execution(translation_unit_t* translation_unit, 
        char* parsed_filename)
{
    timing_t time_phases;
    timing_start(&time_phases);

    start_compiler_phase_execution(translation_unit);

    timing_end(&time_phases);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Compiler phases pipeline executed in %.2f seconds\n", timing_elapsed(&time_phases));
    }
}

static void parse_translation_unit(translation_unit_t* translation_unit, char* parsed_filename)
{
    mcxx_flex_debug = mc99_flex_debug = CURRENT_CONFIGURATION(debug_options.debug_lexer);
    mcxxdebug = mc99debug = CURRENT_CONFIGURATION(debug_options.debug_parser);

    timing_t timing_parsing;

    timing_start(&timing_parsing);

    int parse_result = 0;
    CXX_LANGUAGE()
    {
        parse_result = mcxxparse(&(translation_unit->parsed_tree));
    }

    C_LANGUAGE()
    {
        parse_result = mc99parse(&(translation_unit->parsed_tree));
    }

    if (parse_result != 0)
    {
        running_error("Compilation failed for file '%s'\n", translation_unit->input_filename);
    }

    timing_end(&timing_parsing);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "File '%s' ('%s') parsed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_parsing));
    }

    timing_t timing_semantic;

    timing_start(&timing_semantic);
    build_scope_translation_unit(translation_unit);
    timing_end(&timing_semantic);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "File '%s' ('%s') semantically analyzed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_semantic));
    }


    check_tree(translation_unit->parsed_tree);
}

static char* prettyprint_translation_unit(translation_unit_t* translation_unit, char* parsed_filename)
{
    if (CURRENT_CONFIGURATION(do_not_prettyprint))
    {
        return NULL;
    }

    FILE* prettyprint_file;
    char* output_filename = NULL;

    if (CURRENT_CONFIGURATION(do_not_compile)
            && CURRENT_CONFIGURATION(do_not_link)
            && strcmp(translation_unit->output_filename, "-") == 0)
    {
        prettyprint_file = stdout;
        output_filename = "(stdout)";
    }
    else
    {
        char* input_filename_basename = NULL;
        input_filename_basename = give_basename(translation_unit->input_filename);

        char* preffix = strappend(compilation_process.exec_basename, "_");

        char* output_filename_basename = NULL; 
        output_filename_basename = strappend(preffix,
                input_filename_basename);

        if (CURRENT_CONFIGURATION(output_directory) == NULL)
        {
            char* input_filename_dirname = give_dirname(translation_unit->input_filename);
            input_filename_dirname = strappend(input_filename_dirname, "/");

            output_filename = strappend(input_filename_dirname,
                    output_filename_basename);
        }
        else
        {
            output_filename = strappend(CURRENT_CONFIGURATION(output_directory), "/");
            output_filename = strappend(output_filename, output_filename_basename);
        }
        prettyprint_file = fopen(output_filename, "w");
    }


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
    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Prettyprinted into file '%s' in %.2f seconds\n", output_filename, timing_elapsed(&time_print));
    }

    fclose(prettyprint_file);

    return output_filename;
}

static char* preprocess_file(translation_unit_t* translation_unit, char* input_filename)
{
    int num_arguments = count_null_ended_array((void**)CURRENT_CONFIGURATION(preprocessor_options));

    char** preprocessor_options = calloc(num_arguments + 3 + 1, sizeof(char*));

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        preprocessor_options[i] = CURRENT_CONFIGURATION(preprocessor_options[i]);
    }

    temporal_file_t preprocessed_file = new_temporal_file();
    preprocessor_options[i] = strdup("-o"); 
    i++;
    preprocessor_options[i] = preprocessed_file->name;
    i++;
    preprocessor_options[i] = input_filename;

    int result_preprocess = execute_program(CURRENT_CONFIGURATION(preprocessor_name),
            preprocessor_options);

    if (result_preprocess == 0)
    {
        return preprocessed_file->name;
    }
    else
    {
        fprintf(stderr, "Preprocessing failed. Returned code %d\n",
                result_preprocess);
        return NULL;
    }
}

static void native_compilation(translation_unit_t* translation_unit, 
        char* prettyprinted_filename)
{
    if (CURRENT_CONFIGURATION(do_not_compile))
        return;

    char* output_object_filename;

    if (translation_unit->output_filename == NULL
            || !CURRENT_CONFIGURATION(do_not_link))
    {
        output_object_filename = strdup(translation_unit->input_filename);
        char* extension = get_extension_filename(output_object_filename);
        *extension = '\0';

        output_object_filename = strappend(output_object_filename, ".o");

        translation_unit->output_filename = output_object_filename;
    }
    else
    {
        output_object_filename = translation_unit->output_filename;
    }

    int num_args_compiler = count_null_ended_array((void**)CURRENT_CONFIGURATION(native_compiler_options));

    char** native_compilation_args = calloc(num_args_compiler + 4 + 1, sizeof(*native_compilation_args));

    int i;
    for (i = 0; i < num_args_compiler; i++)
    {
        native_compilation_args[i] = CURRENT_CONFIGURATION(native_compiler_options[i]);
    }

    native_compilation_args[i] = strdup("-c");
    i++;
    native_compilation_args[i] = strdup("-o");
    i++;
    native_compilation_args[i] = output_object_filename;
    i++;
    native_compilation_args[i] = prettyprinted_filename;

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Performing native compilation of '%s' into '%s'\n",
                prettyprinted_filename, output_object_filename);
    }

    timing_t timing_compilation;
    timing_start(&timing_compilation);

    if (execute_program(CURRENT_CONFIGURATION(native_compiler_name), native_compilation_args) != 0)
    {
        running_error("Native compilation failed for file '%s'", translation_unit->input_filename);
    }
    timing_end(&timing_compilation);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "File '%s' ('%s') natively compiled in %.2f seconds\n", 
                translation_unit->input_filename,
                prettyprinted_filename,
                timing_elapsed(&timing_compilation));
    }

    if (!CURRENT_CONFIGURATION(keep_files))
    {
        if (CURRENT_CONFIGURATION(verbose))
        {
            fprintf(stderr, "Removing prettyprinted file '%s'\n", prettyprinted_filename);
        }
        remove(prettyprinted_filename);
    }
}

static void link_objects(void)
{
    if (CURRENT_CONFIGURATION(do_not_link))
        return;

    int num_args_linker = count_null_ended_array((void**)CURRENT_CONFIGURATION(linker_options));

    char** linker_args = calloc(num_args_linker
            + compilation_process.num_translation_units + 2 + 1, 
            sizeof(*linker_args));

    int i = 0;
    int j = 0;

    if (CURRENT_CONFIGURATION(linked_output_filename) != NULL)
    {
        linker_args[i] = strdup("-o");
        i++;
        linker_args[i] = CURRENT_CONFIGURATION(linked_output_filename);
        i++;
    }

    for (j = 0; j < compilation_process.num_translation_units; j++)
    {
        linker_args[i] = compilation_process.translation_units[j]->translation_unit->output_filename;
        i++;
    }

    for (j = 0; j < num_args_linker; j++)
    {
        linker_args[i] = CURRENT_CONFIGURATION(linker_options)[j];
        i++;
    }

    timing_t timing_link;
    timing_start(&timing_link);
    if (execute_program(CURRENT_CONFIGURATION(linker_name), linker_args) != 0)
    {
        running_error("Link failed", 0);
    }
    timing_end(&timing_link);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Link performed in %.2f seconds\n", 
                timing_elapsed(&timing_link));
    }
}

static void terminating_signal_handler(int sig)
{
    signal(sig, SIG_DFL);

    fprintf(stderr, "Signal handler called (signal=%d). Exiting.\n", sig);
    // Do cleanup that will not be done
    // because of SIGSEGV

    // If this routine SIGSEGVs exact behaviour
    // depends on the libc blocking this handler
    // or disabling it
    if (!in_cleanup_routine)
        cleanup_routine();

    // Reraise the signal
    raise(sig);
}

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
        fprintf(stderr, " at %s\n", node_information(ambiguous_node));
        fprintf(stderr, "============================\n");
        ast_dump_graphviz(ambiguous_node, stderr);
        fprintf(stderr, "============================\n");
        internal_error("Tree still contains ambiguities", 0);

        return 0;
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


extern void load_compiler_phases_cxx(void);

static void load_compiler_phases(void)
{
    timing_t loading_phases;
    timing_start(&loading_phases);

    // This invokes a C++ routine that will dlopen all libraries, get the proper symbol
    // and fill an array of compiler phases
    load_compiler_phases_cxx();

    timing_end(&loading_phases);

    if (CURRENT_CONFIGURATION(verbose))
    {
        fprintf(stderr, "Compiler phases loaded in %.2f seconds\n", timing_elapsed(&loading_phases));
    }
}

void add_new_file_to_compilation_process(const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration)
{
    translation_unit_t* translation_unit = (translation_unit_t*)calloc(1, sizeof(*translation_unit));
    translation_unit->input_filename = strdup(file_path);

    compilation_file_process_t *new_compiled_file = (compilation_file_process_t*) calloc(1, sizeof(*new_compiled_file));

    configuration->verbose = CURRENT_CONFIGURATION(verbose);
    configuration->do_not_link = CURRENT_CONFIGURATION(do_not_link);
    configuration->do_not_compile = CURRENT_CONFIGURATION(do_not_compile);
    configuration->do_not_prettyprint = CURRENT_CONFIGURATION(do_not_prettyprint);

    new_compiled_file->translation_unit = translation_unit;
    new_compiled_file->compilation_configuration = configuration;

    if ((configuration->do_not_link
            || configuration->do_not_compile)
            && output_file != NULL)
    {
        translation_unit->output_filename = strdup(output_file);
    }

    P_LIST_ADD(compilation_process.translation_units, 
            compilation_process.num_translation_units, 
            new_compiled_file);
}

// Useful for debugging sessions
void _enable_debug(void)
{
    CURRENT_CONFIGURATION(debug_options.enable_debug_code) = 1;
}

static void register_new_directive_inner(pragma_directive_set_t* pragma_directive_set,
        const char* directive, pragma_directive_kind_t kind)
{
    int num_directives = pragma_directive_set->num_directives;
    P_LIST_ADD(pragma_directive_set->directive_names,
            num_directives,
            strdup(directive));
    P_LIST_ADD(pragma_directive_set->directive_kinds,
            pragma_directive_set->num_directives,
            kind);
}

void register_new_directive(const char* prefix, const char* directive, char is_construct)
{
    pragma_directive_kind_t kind = (is_construct ? PDK_CONSTRUCT : PDK_DIRECTIVE);

    if (strcmp(prefix, "omp") == 0)
    {
        // OpenMP is handled special
        register_new_directive_inner(&CURRENT_CONFIGURATION(pragma_omp_info), directive, kind);
        return;
    }

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION(num_pragma_custom_prefix); i++)
    {
        if (strcmp(CURRENT_CONFIGURATION(pragma_custom_prefix)[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION(pragma_custom_prefix_info)[i];

            int j;
            for (j = 0; j < pragma_directive_set->num_directives; j++)
            {
                if (strcmp(pragma_directive_set->directive_names[j], directive) == 0)
                {
                    fprintf(stderr, "Warning, directive or construct "
                            "'%s' already registered for pragma '%s'"
                            ", ignoring additional registrations\n",
                            directive, prefix);
                    return;
                }
            }

            register_new_directive_inner(pragma_directive_set, directive, kind);
        }
    }
}

static pragma_directive_kind_t lookup_pragma_directive_inner(pragma_directive_set_t* pragma_directive_set, 
        const char *directive)
{
    int j;
    for (j = 0; j < pragma_directive_set->num_directives; j++)
    {
        if (strcmp(pragma_directive_set->directive_names[j], directive) == 0)
        {
            return pragma_directive_set->directive_kinds[j];
        }
    }

    return PDK_NONE;
}

pragma_directive_kind_t lookup_pragma_directive(const char* prefix, const char* directive)
{
    if (strcmp(prefix, "omp") == 0)
    {
        // OpenMP is handled special
        return lookup_pragma_directive_inner(&CURRENT_CONFIGURATION(pragma_omp_info), directive);
    }

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION(num_pragma_custom_prefix); i++)
    {
        if (strcmp(CURRENT_CONFIGURATION(pragma_custom_prefix)[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION(pragma_custom_prefix_info)[i];
            return lookup_pragma_directive_inner(pragma_directive_set, directive);
        }
    }

    return PDK_NONE;
}
