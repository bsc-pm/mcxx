#ifdef HAVE_CONFIG_H
 #include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-driver.h"
#include "cxx-ast.h"
#include "cxx-graphviz.h"
#include "cxx-prettyprint.h"
#include "cxx-buildscope.h"

#include <getopt.h>

// Compilation options
compilation_options_t compilation_options;

// Remember to update GETOPT_STRING if needed
#define GETOPT_STRING "vkagdc"
struct option getopt_long_options[] =
{
	{"version", no_argument, NULL, OPTION_VERSION},
	{"verbose", no_argument, NULL, 'v'},
	{"keep-files", no_argument, NULL, 'k'},
	{"check-dates", no_argument, NULL, 'a'},
	{"graphviz", no_argument, NULL, 'g'},
	{"debug", no_argument, NULL, 'd'},
	// sentinel
	{NULL, 0, NULL, 0}
};

static void print_version(void);
static void parse_arguments(void);
static void driver_initialization(int argc, char* argv[]);
static void initialize_default_values(void);

int main(int argc, char* argv[])
{
	driver_initialization(argc, argv);

	// Argument parsing
	parse_arguments();

	return compilation_options.execution_result;
}

static void driver_initialization(int argc, char* argv[])
{
	memset(&compilation_options, 0, sizeof(compilation_options));
	compilation_options.argc = argc;
	compilation_options.argv = argv;
}

static void parse_arguments(void)
{
	int c;
	int indexptr;

	initialize_default_values();

	while ((c = getopt_long (compilation_options.argc, 
					compilation_options.argv, 
					GETOPT_STRING, 
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
		}
	}
}

static void initialize_default_values(void)
{
	// Initialize here all default values
}

static void print_version(void)
{
	fprintf(stderr, PACKAGE " - " VERSION " (experimental)\n");
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
