#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "config.h"
#include "prescanner-driver.h"
#include "prescanner-process.h"

#define GETOPT_STRING "o:w:hdmapI:r:"

#define HELP_STRING \
"Usage: " PACKAGE " [options] -o file file\n" \
"Options: \n" \
"  -m       \tInstead of appending continuated lines\n" \
"           \tuse Fortran 95 continuation marks\n" \
"  -a       \tDo not pad unended strings prior to \n" \
"           \tcontinuated lines with spaces\n" \
"  -d       \tEnable debugging\n" \
"  -o file  \tPlace the output into <file>\n" \
"  -w width \tColumn width\n" \
"  -p       \tEnable OpenMP 2.5 preprocessing\n" \
"  -I <dir> \tConsiders directory <dir> when looking\n" \
"           \tfor included files\n" \
"  -r <dir> \tRegenerates headers in directory <dir>.\n" \
"           \tIt will be also searched for includes\n" \
"\n"

static prescanner_t prescanner;

static void show_help(void);
static void parse_parameters(int argc, char* argv[]);

int main(int argc, char* argv[])
{
	fprintf(stderr, PACKAGE " Prescanner - Version " VERSION "\n");

	parse_parameters(argc, argv);

	fortran_prescanner_run(&prescanner);
	return 0;
}

static void show_help(void)
{
	fprintf(stderr, HELP_STRING);
}

static void parse_parameters(int argc, char* argv[])
{
	char c;
	// By default
	prescanner.debug = 0;
	prescanner.width = 72;
	prescanner.append = 1;
	prescanner.openmp_processing = 0;
	prescanner.pad_strings = 1;
	prescanner.output_filename = NULL;
	prescanner.num_include_directories = 1;
	prescanner.include_directories = calloc(1, sizeof(*prescanner.include_directories));
	prescanner.include_directories[0] = strdup(".");
	prescanner.output_include_directory = NULL;

	char error = 0;
	while ((c = getopt(argc, argv, GETOPT_STRING)) != (char) -1)
	{
		switch (c)
		{
			case 'o' :
				{
					prescanner.output_filename = strdup(optarg);
					break;
				}
			case 'w' :
				{
					prescanner.width = atoi(optarg);
					if (prescanner.width == 0)
					{
						fprintf(stderr, "Column width cannot be 0\n");
						error = 1;
					}
					break;
				}
			case 'd' :
				{
					prescanner.debug = 1;
					break;
				}
			case 'a' :
				{
					prescanner.pad_strings = 0;
					break;
				}
			case 'm' :
				{
					prescanner.append = 0;
					break;
				}
			case 'p' :
				{
					prescanner.openmp_processing = 1;
					break;
				}
			case 'h' :
				{
					show_help();
					exit(EXIT_SUCCESS);
					break;
				}
			case 'I' :
				{
					char* directory = strdup(optarg);
					prescanner.num_include_directories++;
					prescanner.include_directories = realloc(prescanner.include_directories, 
							prescanner.num_include_directories * sizeof(*prescanner.include_directories));
					prescanner.include_directories[prescanner.num_include_directories-1] = directory;

					break;
				}
			case 'r' :
				{
					if (prescanner.output_include_directory != NULL)
					{
						fprintf(stderr, "Do not specify -r more than once\n");
						show_help();
						exit(EXIT_FAILURE);
						break;
					}

					char* directory = strdup(optarg);
					prescanner.output_include_directory = directory;

					prescanner.num_include_directories++;
					prescanner.include_directories = realloc(prescanner.include_directories, 
							prescanner.num_include_directories * sizeof(*prescanner.include_directories));
					prescanner.include_directories[prescanner.num_include_directories-1] = directory;
					break;
				}
			case '?' :
				{
					error = 1;
					break;
				}
			default :
				error = 1;
				break;
		}
	}

	if (prescanner.output_filename == NULL)
	{
		fprintf(stderr, "You must specify an output file\n");
		error = 1;
	}

	if (argc == optind)
	{
		fprintf(stderr, "You must specify an input file.\n");
		error = 1;
	}

	if ((argc - optind) > 1)
	{
		fprintf(stderr, "Too many parameters.");
		error = 1;
	}

	if (error == 1)
	{
		show_help();
		exit(EXIT_FAILURE);
	}

	prescanner.input_filename = strdup(argv[optind]);
}
