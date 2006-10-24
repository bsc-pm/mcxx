#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libgen.h>

#include "gc.h"
#include "getopt.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"
#include "cxx-lexer.h"
#include "cxx-dyninit.h"
#include "mcfg.h"


// Compilation options
compilation_options_t compilation_options;

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
"  -y                       Only parsing will be performed\n" \
"                           No file will be generated\n" \
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
"  -Wp,<options>            Pass comma-separated <options> on to\n" \
"                           the preprocessor\n" \
"  -Wn,<options>            Pass comma-separated <options> on to\n" \
"                           the native compiler\n" \
"  -Wl,<options>            Pass comma-separated <options> on to\n" \
"                           the linker\n" \
"  --no-openmp              Disables OpenMP 2.5 support\n" \
"  --config-file=<file>     Uses <file> as config file, otherwise\n" \
"                           '" PKGDATADIR "/config.mcxx'\n" \
"                           will be used\n" \
"\n"

// Remember to update GETOPT_STRING if needed
#define GETOPT_STRING "vkadcho:m:W:Ey"
struct option getopt_long_options[] =
{
    {"help",        no_argument, NULL, 'h'},
    {"version",     no_argument, NULL, OPTION_VERSION},
    {"verbose",     no_argument, NULL, 'v'},
    {"keep-files",  no_argument, NULL, 'k'},
    {"check-dates", no_argument, NULL, 'a'},
    {"debug",       no_argument, NULL, 'd'},
    {"output",      required_argument, NULL, 'o'},
	// This option has a chicken-and-egg problem. If we delay till getopt_long
	// to open the configuration file we overwrite variables defined in the
	// command line. Thus "load_configuration" is invoked before command line parsing
	// and looks for "--config-file" / "-m" in the arguments
    {"config-file", required_argument, NULL, 'm'},
    {"output-dir",  required_argument, NULL, OPTION_OUTPUT_DIRECTORY},
    {"cc", required_argument, NULL, OPTION_NATIVE_COMPILER_NAME},
    {"cxx", required_argument, NULL, OPTION_NATIVE_COMPILER_NAME},
    {"cpp", required_argument, NULL, OPTION_PREPROCESSOR_NAME},
    {"ld", required_argument, NULL, OPTION_LINKER_NAME},
    {"debug-flags",  required_argument, NULL, OPTION_DEBUG_FLAG},
    {"help-debug-flags", no_argument, NULL, OPTION_HELP_DEBUG_FLAGS},
	{"no-openmp", no_argument, NULL, OPTION_NO_OPENMP},
    // sentinel
    {NULL, 0, NULL, 0}
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

static void parse_subcommand_arguments(char* arguments);

static void enable_debug_flag(char* flag);

static void load_compiler_phases(void);

static void register_default_initializers(void);

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

	// Loads the compiler phases
	load_compiler_phases();

	// Compiler phases can define additional dynamic initializers
	// (besides the built in ones)
	run_dynamic_initializers();
	
    // Parse arguments
    parse_arguments(compilation_options.argc, 
        compilation_options.argv, /* from_command_line= */1);

    // Compilation of every specified translation unit
    compile_every_translation_unit();

    // Link all generated objects
    link_objects();

    timing_end(&timing_global);
    if (compilation_options.verbose)
    {
        fprintf(stderr, "Whole process took %.2f seconds to complete\n",
                timing_elapsed(&timing_global));
    }

    return compilation_options.execution_result;
}

static void driver_initialization(int argc, char* argv[])
{
    // Initialize GC
    GC_init();
    // Basic initialization prior to argument parsing and configuration loading
    atexit(temporal_files_cleanup);
    signal(SIGSEGV, terminating_signal_handler);
    signal(SIGQUIT, terminating_signal_handler);
    signal(SIGINT,  terminating_signal_handler);
    signal(SIGTERM, terminating_signal_handler);

    memset(&compilation_options, 0, sizeof(compilation_options));
    compilation_options.argc = argc;
    compilation_options.argv = argv;
    compilation_options.exec_basename = give_basename(argv[0]);

    num_seen_file_names = 0;
    seen_file_names = 0;
}

static void help_message(void)
{
    fprintf(stderr, "Usage: %s options file [file..]\n", compilation_options.argv[0]);
    fprintf(stderr, HELP_STRING);
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

void parse_arguments(int argc, char* argv[], char from_command_line)
{
    int c;
    int indexptr;
    char* output_file = NULL;

    while ((c = getopt_long (argc, argv, GETOPT_STRING, 
                    getopt_long_options, 
                    &indexptr)) != -1)
    {
        switch (c)
        {
            case OPTION_VERSION : // --version
                {
                    print_version();
                    exit(0);
                    break;
                }
            case 'v' : // --verbose || -v
                {
                    compilation_options.verbose = 1;
                    break;
                }
            case 'k' : // --keep-files || -k
                {
                    compilation_options.keep_files = 1;
                    break;
                }
            case 'c' : // -c
                {
                    compilation_options.do_not_link = 1;
                    break;
                }
            case 'E' : // -E
                {
                    compilation_options.do_not_compile = 1;
                    compilation_options.do_not_link = 1;
                    break;
                }
            case 'y' : // -y
                {
                    compilation_options.do_not_compile = 1;
                    compilation_options.do_not_link = 1;
                    compilation_options.do_not_prettyprint = 1;
                    break;
                }
            case 'a' : // --check-dates || -a
                {
                    compilation_options.check_dates = 1;
                    break;
                }
            case 'm' :
                {
					// This option is handled in "load_configuration"
					// and ignore here for getopt_long happiness
                    break;
                }
            case 'o' :
                {
                    if (output_file != NULL)
                    {
                        running_error("Output file specified twice", 0);
                    }
                    else
                    {
                        output_file = GC_STRDUP(optarg);
                    }
                    break;
                }
            case 'W' :
                {
                    parse_subcommand_arguments(optarg);
                    break;
                }
            case OPTION_PREPROCESSOR_NAME :
                {
                    compilation_options.preprocessor_name = GC_STRDUP(optarg);
                    break;
                }
            case OPTION_NATIVE_COMPILER_NAME :
                {
                    compilation_options.native_compiler_name = GC_STRDUP(optarg);
                    break;
                }
            case OPTION_LINKER_NAME :
                {
                    compilation_options.linker_name = GC_STRDUP(optarg);
                    break;
                }
            case OPTION_DEBUG_FLAG :
                {
                    enable_debug_flag(GC_STRDUP(optarg));
                    break;
                }
            case OPTION_OUTPUT_DIRECTORY :
                {
                    compilation_options.output_directory = GC_STRDUP(optarg);
                    break;
                }
			case OPTION_NO_OPENMP :
				{
					compilation_options.disable_openmp = 1;
					break;
				}
            case OPTION_HELP_DEBUG_FLAGS :
                {
                    print_debug_flags_list();
                    exit(EXIT_SUCCESS);
					break;
                }
            case 'h' :
                {
                    help_message();
                    exit(EXIT_SUCCESS);
                }
        }
    }

    if (!from_command_line)
    {
        return;
    }

    if (argc == optind)
    {
        options_error("You must specify an input file.");
    }

    int i = 1;
    while (optind < argc)
    {
        // Ignore spurious blank parameters
        if (strlen(argv[optind]) != 0
                && !is_blank_string(argv[optind]))
        {
            if ((i > 1) 
                    && compilation_options.do_not_link
                    && output_file != NULL)
            {
                running_error("Cannot specify -o with -c with multiple files (second file '%s')", argv[optind]);
            }
            else
            {
                translation_unit_t* translation_unit = GC_CALLOC(1, sizeof(*translation_unit));
                translation_unit->input_filename = GC_STRDUP(argv[optind]);

                if (compilation_options.do_not_link)
                {
                    translation_unit->output_filename = output_file;
                }

                P_LIST_ADD(compilation_options.translation_units, 
                        compilation_options.num_translation_units, 
                        translation_unit);
            }
        }
        i++;
        optind++;
    }

    if (output_file != NULL
            && !compilation_options.do_not_link)
    {
        compilation_options.linked_output_filename = output_file;
    }
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
            *(flag_option->flag_pointer) = 1;
        }
        else
        {
            fprintf(stderr, "Debug flag '%s' unknown. Ignoring it\n", flag);
        }
    }

    // Fix scope printing
    compilation_options.debug_options.print_scope |= 
        compilation_options.debug_options.print_scope_brief;
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
            existing_options = &compilation_options.native_compiler_options;
            break;
        case 'p' :
            existing_options = &compilation_options.preprocessor_options;
            break;
        case 'l' :
            existing_options = &compilation_options.linker_options;
            break;
        default:
            {
                internal_error("Unknown '%c' switch\n", arguments[0]);
            }
    }

    int num_existing_options = count_null_ended_array((void**)(*existing_options));
    (*existing_options) = GC_REALLOC((*existing_options), sizeof(char*)*(num_existing_options + num_parameters + 1));

    int i;
    for (i = 0; i < num_parameters; i++)
    {
        (*existing_options)[num_existing_options + i] = parameters[i];
    }
}

static void initialize_default_values(void)
{
    int dummy = 0;
    // Initialize here all default values
    compilation_options.config_file = PKGDATADIR "/config.mcxx";
    compilation_options.num_translation_units = 0;

    compilation_options.source_language = SOURCE_LANGUAGE_CXX;

    compilation_options.preprocessor_name = GC_STRDUP("c++");
    compilation_options.preprocessor_options = comma_separate_values(GC_STRDUP("-E"), &dummy);

    compilation_options.native_compiler_name = GC_STRDUP("c++");
    compilation_options.native_compiler_options = NULL;

    compilation_options.linker_name = GC_STRDUP("c++");
    compilation_options.linker_options = NULL;
}

static void register_default_initializers(void)
{
	register_dynamic_initializer(build_scope_dynamic_initializer);
}

static void print_version(void)
{
    fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
}

static char section_name[128];
static int section_callback(char* sname)
{
    strncpy(section_name, sname, 127);
    section_name[127] = '\0';
    return 0;
}

static int parameter_callback(char* parameter, char* value)
{
    if (strcmp(compilation_options.exec_basename, section_name) == 0)
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
    }
    return 0;
}

static void load_configuration(void)
{
	// Solve here the egg and chicken problem of the option --config-file / -m
	int i;
	for (i = 1; i < compilation_options.argc; i++)
	{
		// First case "-m" "file"
		if (strcmp(compilation_options.argv[i], "-m") == 0)
		{
			if ((i + 1) < compilation_options.argc)
			{
				compilation_options.config_file = 
					GC_STRDUP(compilation_options.argv[i+1]);
				i++;
			}
		}
		// Second case -mfile
		else if (strncmp(compilation_options.argv[i], "-m", strlen("-m")) == 0)
		{
			compilation_options.config_file = 
				GC_STRDUP(&(compilation_options.argv[i][strlen("-m")]));
		}
		// Third case --config-file=file
		// FIXME: GNU getopt_long is kind enough to allow you to specify just a
		// preffix of the option. At the moment do not try to emulate this feature here
		else if (strncmp(compilation_options.argv[i], 
					"--config-file=", strlen("--config-file=")) == 0)
		{
			compilation_options.config_file = 
				GC_STRDUP(&(compilation_options.argv[i][strlen("--config-file=") ]));
		}
	}

    int result = param_process(compilation_options.config_file, MS_STYLE, 
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
}

static void compile_every_translation_unit(void)
{
    int i;
    for (i = 0; i < compilation_options.num_translation_units; i++)
    {
        translation_unit_t* translation_unit = compilation_options.translation_units[i];
        
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

        if (current_extension->source_language != compilation_options.source_language)
        {
            fprintf(stderr, "%s was configured for %s language but file '%s' looks %s language. Skipping it.\n",
                    compilation_options.exec_basename, 
                    source_language_names[compilation_options.source_language],
                    translation_unit->input_filename,
                    source_language_names[current_extension->source_language]);
            continue;
        }

        if (compilation_options.verbose)
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
                    && compilation_options.verbose)
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

        char* prettyprinted_filename = prettyprint_translation_unit(translation_unit, parsed_filename);

        native_compilation(translation_unit, prettyprinted_filename);
    }
}

void start_compiler_phase_execution(translation_unit_t* translation_unit);

static void compiler_phases_execution(translation_unit_t* translation_unit, 
		char* parsed_filename)
{
	start_compiler_phase_execution(translation_unit);
}

static void parse_translation_unit(translation_unit_t* translation_unit, char* parsed_filename)
{
    mcxx_flex_debug = mc99_flex_debug = compilation_options.debug_options.debug_lexer;
    mcxxdebug = mc99debug = compilation_options.debug_options.debug_parser;

    timing_t timing_parsing;

    timing_start(&timing_parsing);

    CXX_LANGUAGE()
    {
        mcxxparse(&(translation_unit->parsed_tree));
    }

    C_LANGUAGE()
    {
        mc99parse(&(translation_unit->parsed_tree));
    }

    timing_end(&timing_parsing);

    if (compilation_options.verbose)
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

    if (compilation_options.verbose)
    {
        fprintf(stderr, "File '%s' ('%s') semantically analyzed in %.2f seconds\n", 
                translation_unit->input_filename,
                parsed_filename,
                timing_elapsed(&timing_semantic));
    }

	if (compilation_options.debug_options.print_ast)
	{
		fprintf(stderr, "Printing AST in graphviz format\n");

		ast_dump_graphviz(translation_unit->parsed_tree, stdout);
	}

    check_tree(translation_unit->parsed_tree);
}

static char* prettyprint_translation_unit(translation_unit_t* translation_unit, char* parsed_filename)
{
    if (compilation_options.do_not_prettyprint)
    {
        return NULL;
    }

    char* input_filename_basename = NULL;
    input_filename_basename = give_basename(translation_unit->input_filename);

    char* preffix = strappend(compilation_options.exec_basename, "_");

    char* output_filename_basename = NULL; 
    output_filename_basename = strappend(preffix,
            input_filename_basename);

    char* output_filename = NULL;

    if (compilation_options.output_directory == NULL)
    {
        char* input_filename_dirname = give_dirname(translation_unit->input_filename);
        input_filename_dirname = strappend(input_filename_dirname, "/");

        output_filename = strappend(input_filename_dirname,
                output_filename_basename);
    }
    else
    {
        output_filename = strappend(compilation_options.output_directory, "/");
        output_filename = strappend(output_filename, output_filename_basename);
    }

    FILE* prettyprint_file = fopen(output_filename, "w");

    if (prettyprint_file == NULL)
    {
        running_error("Cannot create output file '%s' (%s)", output_filename,
                strerror(errno));
    }
    
    if (compilation_options.verbose)
    {
        fprintf(stderr, "Prettyprinting into file '%s'\n", output_filename);
    }

    prettyprint(prettyprint_file, translation_unit->parsed_tree);

    fclose(prettyprint_file);

    return output_filename;
}

static char* preprocess_file(translation_unit_t* translation_unit, char* input_filename)
{
    int num_arguments = count_null_ended_array((void**)compilation_options.preprocessor_options);

    char** preprocessor_options = GC_CALLOC(num_arguments + 3 + 1, sizeof(char*));

    int i;
    for (i = 0; i < num_arguments; i++)
    {
        preprocessor_options[i] = compilation_options.preprocessor_options[i];
    }

    temporal_file_t preprocessed_file = new_temporal_file();
    preprocessor_options[i] = GC_STRDUP("-o"); 
    i++;
    preprocessor_options[i] = preprocessed_file->name;
    i++;
    preprocessor_options[i] = input_filename;

    int result_preprocess = execute_program(compilation_options.preprocessor_name,
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
    if (compilation_options.do_not_compile)
        return;

    char* output_object_filename;

    if (translation_unit->output_filename == NULL
            || !compilation_options.do_not_link)
    {
        output_object_filename = GC_STRDUP(translation_unit->input_filename);
        char* extension = get_extension_filename(output_object_filename);
        *extension = '\0';

        output_object_filename = strappend(output_object_filename, ".o");

        translation_unit->output_filename = output_object_filename;
    }
    else
    {
        output_object_filename = translation_unit->output_filename;
    }

    int num_args_compiler = count_null_ended_array((void**)compilation_options.native_compiler_options);

    char** native_compilation_args = GC_CALLOC(num_args_compiler + 4 + 1, sizeof(*native_compilation_args));

    int i;
    for (i = 0; i < num_args_compiler; i++)
    {
        native_compilation_args[i] = compilation_options.native_compiler_options[i];
    }

    native_compilation_args[i] = GC_STRDUP("-c");
    i++;
    native_compilation_args[i] = GC_STRDUP("-o");
    i++;
    native_compilation_args[i] = output_object_filename;
    i++;
    native_compilation_args[i] = prettyprinted_filename;

    if (compilation_options.verbose)
    {
        fprintf(stderr, "Performing native compilation of '%s' into '%s'\n",
                prettyprinted_filename, output_object_filename);
    }

    timing_t timing_compilation;
    timing_start(&timing_compilation);

    if (execute_program(compilation_options.native_compiler_name, native_compilation_args) != 0)
    {
        running_error("Native compilation failed for file '%s'", translation_unit->input_filename);
    }
    timing_end(&timing_compilation);

    if (compilation_options.verbose)
    {
        fprintf(stderr, "File '%s' ('%s') natively compiled in %.2f seconds\n", 
                translation_unit->input_filename,
                prettyprinted_filename,
                timing_elapsed(&timing_compilation));
    }
}

static void link_objects(void)
{
    if (compilation_options.do_not_link)
        return;

    int num_args_linker = count_null_ended_array((void**)compilation_options.linker_options);

    char** linker_args = GC_CALLOC(num_args_linker
            + compilation_options.num_translation_units + 2 + 1, 
            sizeof(*linker_args));

    int i = 0;
    int j = 0;

    if (compilation_options.linked_output_filename != NULL)
    {
        linker_args[i] = GC_STRDUP("-o");
        i++;
        linker_args[i] = compilation_options.linked_output_filename;
        i++;
    }

    for (j = 0; j < compilation_options.num_translation_units; j++)
    {
        linker_args[i] = compilation_options.translation_units[j]->output_filename;
        i++;
    }

    for (j = 0; j < num_args_linker; j++)
    {
        linker_args[i] = compilation_options.linker_options[j];
        i++;
    }

    timing_t timing_link;
    timing_start(&timing_link);
    if (execute_program(compilation_options.linker_name, linker_args) != 0)
    {
        running_error("Link failed", 0);
    }
    timing_end(&timing_link);

    if (compilation_options.verbose)
    {
        fprintf(stderr, "Link performed in %.2f seconds\n", 
                timing_elapsed(&timing_link));
    }
}

static void terminating_signal_handler(int sig)
{
    // Do cleanup that will not be done
    // because of SIGSEGV

    // If this routine SIGSEGVs exact behaviour
    // depends on the libc blocking this handler
    // or disabling it
    temporal_files_cleanup();

    // Reraise the signal
    signal(sig, SIG_DFL);
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
	// This invokes a C++ routine that will dlopen all libraries, get the proper symbol
	// and fill an array of compiler phases
	load_compiler_phases_cxx();
}

