#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"

#include <getopt.h>

#include <gc.h>

#include <mcfg.h>

#include <libgen.h>


// Compilation options
compilation_options_t compilation_options;

/* ------------------------------------------------------------------ */
#define HELP_STRING \
"Options: \n" \
"  -h, --help               Shows this help and quits\n" \
"  --version                Shows version and quits\n" \
"  -c                       Does not link, just compile.\n" \
"  -k, --keep-files         Do not remove intermediate temporary\n" \
"                           files.\n" \
"  -a, --check-dates        Checks dates before regenerating files\n" \
"  -g, --graphviz           Outputs AST in graphviz format\n" \
"  -d, --debug              Prints lots of debugging information\n" \
"  --config-file=<file>     Uses <file> as config file, otherwise\n" \
"                           '" PKGDATADIR "/config.mcxx'\n" \
"                           will be used\n" \
"\n"

// Remember to update GETOPT_STRING if needed
#define GETOPT_STRING "vkagdch"
struct option getopt_long_options[] =
{
	{"help", no_argument, NULL, 'h'},
	{"version", no_argument, NULL, OPTION_VERSION},
	{"verbose", no_argument, NULL, 'v'},
	{"keep-files", no_argument, NULL, 'k'},
	{"check-dates", no_argument, NULL, 'a'},
	{"graphviz", no_argument, NULL, 'g'},
	{"debug", no_argument, NULL, 'd'},
	{"config-file", required_argument, NULL, 'm'},
	// sentinel
	{NULL, 0, NULL, 0}
};

static void print_version(void);
static void driver_initialization(int argc, char* argv[]);
static void initialize_default_values(void);
static void load_configuration(void);
static void compile_every_translation_unit(void);

static void terminating_signal_handler(int sig);

int main(int argc, char* argv[])
{
	driver_initialization(argc, argv);

	// Argument parsing
	parse_arguments(compilation_options.argc, compilation_options.argv);

	// Load configuration
	load_configuration();

	compile_every_translation_unit();

	return compilation_options.execution_result;
}

static void driver_initialization(int argc, char* argv[])
{
	// Basic initialization prior to argument parsing and configuration loading
	atexit(temporal_files_cleanup);
	signal(SIGSEGV, terminating_signal_handler);
	signal(SIGQUIT, terminating_signal_handler);
	signal(SIGINT,  terminating_signal_handler);
	signal(SIGTERM, terminating_signal_handler);

	memset(&compilation_options, 0, sizeof(compilation_options));
	compilation_options.argc = argc;
	compilation_options.argv = argv;
	compilation_options.exec_basename = basename(argv[0]);
}

static void help_message()
{
	fprintf(stderr, "Usage: %s options file [file..]\n", compilation_options.argv[0]);
	fprintf(stderr, HELP_STRING);
}

static void options_error(char* message)
{
	fprintf(stderr, "Error : %s\n", message);
	fprintf(stderr, "\n");
	help_message();
	exit(EXIT_FAILURE);
}

void parse_arguments(int argc, char* argv[])
{
	int c;
	int indexptr;

	initialize_default_values();

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
			case 'a' : // --check-dates || -a
				{
					compilation_options.check_dates = 1;
					break;
				}
			case 'd' : // --debug || -d
				{
					compilation_options.debug_level++;
					break;
				}
			case 'm' :
				{
					compilation_options.config_file = GC_STRDUP(optarg);
					break;
				}
			case 'h' :
				{
					help_message();
					exit(EXIT_SUCCESS);
				}
		}
	}

    if (argc == optind)
    {
        options_error("You must specify an input file.");
    }

	compilation_options.num_translation_units = 0;
	while (optind < argc)
	{
		translation_unit_t* translation_unit = GC_CALLOC(1, sizeof(*translation_unit));
		translation_unit->input_filename = GC_STRDUP(argv[optind]);

		P_LIST_ADD(compilation_options.translation_units, 
				compilation_options.num_translation_units, 
				translation_unit);
		optind++;
	}
}

static void initialize_default_values(void)
{
	// Initialize here all default values
	compilation_options.config_file = PKGDATADIR "/config.mcxx";
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
	paramProcess(compilation_options.config_file, MS_STYLE, 
			section_callback, parameter_callback);
}

static void compile_every_translation_unit(void)
{
	int i;
	for (i = 0; i < compilation_options.num_translation_units; i++)
	{
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


#if 0
int main(int argc, char* argv[])
{
	int result = 0;
	fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
	mcxx_flex_debug = yydebug = 0;
	// yydebug = 1;

	yyparse(&compilation_options.parsed_tree);

	if (!ASTCheck(compilation_options.parsed_tree))
	{
		fprintf(stderr, "*** INCONSISTENT TREE DETECTED ***\n");
		exit(EXIT_FAILURE);
	}

	if (argc > 1)
	{
		if (strcmp(argv[1], "-a") == 0)
		{
			char lines = 0;
			if (argc > 2 && (strcmp(argv[2], "-l") == 0))
			{
				lines = 1;
			}
			print_ambiguities(compilation_options.parsed_tree, lines);
		}
		else if (strcmp(argv[1], "-g") == 0)
		{
			ast_dump_graphviz(compilation_options.parsed_tree, stdout);
		}
		else if (strcmp(argv[1], "-y") == 0)
		{
			build_scope_translation_unit(compilation_options.parsed_tree);

			if (argc > 2 && (strcmp(argv[2], "-g") == 0))
			{
				ast_dump_graphviz(compilation_options.parsed_tree, stdout);
			}

			if (!check_for_ambiguities(compilation_options.parsed_tree))
			{
				fprintf(stderr, "***** THERE ARE UNRESOLVED AMBIGUITIES !!! *****\n");
				print_ambiguities(compilation_options.parsed_tree, 1);
				result = 1;
			}

			prettyprint(stderr, compilation_options.parsed_tree);
		}
	}
	else
	{
		prettyprint(stdout, compilation_options.parsed_tree);
	}

	if (!ASTCheck(compilation_options.parsed_tree))
	{
		fprintf(stderr, "*** INCONSISTENT TREE DETECTED ***\n");
		result = 1;
	}

	return result;
}

char check_for_ambiguities(AST a)
{
	if (a == NULL)
		return 1;

	if (ASTType(a) == AST_AMBIGUITY)
	{
		return 0;
	}
	else
	{
		return check_for_ambiguities(ASTSon0(a))
			&& check_for_ambiguities(ASTSon1(a))
			&& check_for_ambiguities(ASTSon2(a))
			&& check_for_ambiguities(ASTSon3(a));
	}
}

void print_ambiguities(AST a, char lines)
{
	if (a == NULL) 
		return;

	// fprintf(stderr, "%p ", a);
	// fprintf(stderr, "[%d] %s\n", ASTNumChildren(a), ast_print_node_type(ASTType(a)));
	switch (ASTType(a))
	{
		case AST_AMBIGUITY :
			{
				int i;

				for (i = 0; i < a->num_ambig; i++)
				{
					print_ambiguities(a->ambig[i], lines);
				}
				
				fprintf(stderr, "%d ", a->num_ambig);
				for (i = 0; i < a->num_ambig; i++)
				{
					if (!lines)
					{
						fprintf(stderr, "%s | ", ast_print_node_type(ASTType(a->ambig[i])));
					}
					else
					{
						fprintf(stderr, "%s (%d) | ", ast_print_node_type(ASTType(a->ambig[i])), ASTLine(a->ambig[i]));
					}
				}
				fprintf(stderr, "\n");
				fprintf(stderr, "------\n");
				prettyprint(stderr, a);
				fprintf(stderr, "\n------\n");
				break;
			}
		default :
			{
				int i;
				for (i = 0; i < ASTNumChildren(a); i++)
				{
					print_ambiguities(ASTChild(a, i), lines);
				}
				break;
			}
	}
}
#endif
