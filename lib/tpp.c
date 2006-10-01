/*
 * Tiny preprocessor
 *
 * A preprocessor intended to regenerate specific versions
 * from common files using conditional compilation
 *
 * Roger Ferrer Ibáñez <roger.ferrer@bsc.es>
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <regex.h>


#define HELP_MESSAGE \
"Syntax: tpp -o output_file [-D define...] input_file" \
"\n"

static int num_defines = 0;
#define MAX_DEFINES 32
static char* defines[MAX_DEFINES];
static char* input_file = NULL;
static char* output_file = NULL;

static void help_message(char* error_message)
{
    if (error_message != NULL)
    {
        fprintf(stderr, "%s\n\n", error_message);
    }
    fprintf(stderr, HELP_MESSAGE);
    exit(EXIT_FAILURE);
}

static void parse_arguments(int argc, char* argv[])
{
    int i;
    for (i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "-D") == 0)
        {
            if ((i + 1) >= argc)
            {
                help_message("Macro name not specified");
            }

            defines[num_defines] = argv[i+1];
            num_defines++;
            i++;
        }
        else if (strcmp(argv[i], "-o") == 0)
        {
            if ((i + 1) >= argc)
            {
                help_message("Output filename not specified");
            }

            if (output_file != NULL)
            {
                help_message("Output file more than once specified\n");
            }

            output_file = argv[i+1];
            i++;
        }
        else
        {
            if (input_file != NULL)
            {
                help_message("Input filename more than once specified\n");
            }

            input_file = argv[i];
        }
    }

    if ((input_file == NULL)
        || (output_file == NULL))
    {
        help_message("Missing parameters");
    }
}

static void conditional_process(char* input_filename, char* output_filename)
{
    regex_t if_regex;
    regex_t ifnot_regex;
    regex_t endif_regex;

    if (regcomp(&if_regex, "^[[:blank:]]*/[*]!if[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling if regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&ifnot_regex, "^[[:blank:]]*/[*]!ifnot[[:blank:]]+([^[:blank:]*]+)[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling if regular expression\n");
        exit(EXIT_FAILURE);
    }

    if (regcomp(&endif_regex, "^[[:blank:]]*/[*]!endif[[:blank:]]*[*]/[[:blank:]]*$",
            REG_EXTENDED | REG_NOSUB | REG_NEWLINE) != 0)
    {
        fprintf(stderr, "Error when compiling endif regular expression\n");
        exit(EXIT_FAILURE);
    }

    FILE* input = NULL;
    if (strcmp(input_filename, "-") == 0)
    {
        input = stdin;
    }
    else
    {
        input = fopen(input_filename, "r");
    }

    if (input == NULL)
    {
        fprintf(stderr, "Could not open input file '%s': %s\n",
                input_filename, strerror(errno));
        exit(EXIT_FAILURE);
    }

    FILE* output = NULL;
    if (strcmp(output_filename, "-") == 0)
    {
        output = stdout;
    }
    else
    {
        output = fopen(output_filename, "w");
    }

    if (output == NULL)
    {
        fprintf(stderr, "Could not open output file '%s': %s\n",
                output_filename, strerror(errno));
        exit(EXIT_FAILURE);
    }

    char output_enabled = 1;
    int block_nesting = 0;
    char buffer[1024];

    while (fgets(buffer, 1024, input) != NULL)
    {
        regmatch_t offsets[2];

        if (regexec(&if_regex, buffer, 2, offsets, 0) == 0)
        {
            block_nesting++;
            char define_name[256] = { 0 };

            int start = offsets[1].rm_so;

            int length = offsets[1].rm_eo - offsets[1].rm_so;
            length = (length > 255) ? 255 : length;

            strncpy(define_name, &(buffer[start]), length);

            // If output is enabled, then we have to ensure this macro is defined
            if (output_enabled)
            {
                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                    }
                }

                if (!found)
                {
                    output_enabled = 0;
                }
            }
        }
        else if (regexec(&ifnot_regex, buffer, 2, offsets, 0) == 0)
        {
            block_nesting++;
            char define_name[256];

            int start = offsets[1].rm_so;

            int length = offsets[1].rm_eo - offsets[1].rm_so;
            length = (length > 255) ? 255 : length;

            strncpy(define_name, &(buffer[start]), length);

            // If output is enabled, then we have to ensure this macro is undefined
            if (output_enabled)
            {
                int j;
                char found = 0;;
                for (j = 0; (j < num_defines) && !found; j++)
                {
                    if (strcmp(defines[j], define_name) == 0)
                    {
                        found = 1;
                    }
                }

                if (found)
                {
                    output_enabled = 0;
                }
            }
        }
        else if (regexec(&endif_regex, buffer, 0, NULL, 0) == 0)
        {
            block_nesting--;

            if (block_nesting == 0)
            {
                output_enabled = 1;
            }
        }
        else if (output_enabled)
        {
            fprintf(output, "%s", buffer);
        }
    }

    if (block_nesting != 0)
    {
        fprintf(stderr, "Unpaired !if when reached the end of the file (%d)\n", block_nesting);
        exit(EXIT_FAILURE);
    }

    fclose(output);
    fclose(input);
}

int main(int argc, char* argv[])
{
    parse_arguments(argc, argv);
    conditional_process(input_file, output_file);
    return 0;
}
