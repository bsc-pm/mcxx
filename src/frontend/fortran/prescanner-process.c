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



#include <config.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#ifdef USE_BUNDLED_REGEX
  #include "mf95-regex.h"
#else
  #include <regex.h>
#endif

#include <errno.h>
#include <ctype.h>

#include "cxx-utils.h"

#include "filename.h"
#include "string_utils.h"

#include "prescanner-process.h"
#include "prescanner-identifier.h"

/*
   $Id: prescanner-process.c,v 1.1 2007/12/27 14:38:02 luism Exp $

   Prescanner Processor that converts Fortran 95 fixed form to a free form

   a) reads lines into memory
   b) cuts them to fit the column width and pads if necessary with trailing blanks
   c) convert whole line comments
   d) join continuated lines while appended lines get inline comments trimmed

 */

typedef struct line_node
{
    char* line;
    unsigned int line_number;
    struct line_node* next;
    unsigned int joined_lines;
} line_t;

static line_t* file_lines;
static line_t* last_line;

static void open_files(prescanner_t*);
static void close_files(prescanner_t*);

static void read_lines(prescanner_t*);
static void print_lines(prescanner_t*);
static void cut_lines(prescanner_t*);
static void join_continuated_lines(prescanner_t*);
static void convert_whole_line_comments(void);
#if 0
static void remove_inlined_comments(void);
#endif
static void normalize_line(prescanner_t*, char** line, unsigned int line_num);
static void convert_lines(prescanner_t*);
static void continuate_lines(prescanner_t*);
static void free_line_t(line_t* l);

static void fortran_prescanner_process(prescanner_t* prescanner);

void fortran_prescanner_run(prescanner_t* prescanner)
{
    open_files(prescanner);

    fortran_prescanner_process(prescanner);
}

static void fortran_prescanner_process(prescanner_t* prescanner)
{
    file_lines = NULL;
    last_line = file_lines;

    // Load all the file into memory
    read_lines(prescanner);

    // Cut lines to be of width 'prescanner.width'
    cut_lines(prescanner);

    // We used to remove comments here but it is better to convert them
    convert_whole_line_comments();

    // Join continuated lines
    join_continuated_lines(prescanner);

#if 0
    // Lines that were not appended any continuation line have not been trimmed
    // their inline comments
    remove_inlined_comments();
#endif

    convert_lines(prescanner);

    if (!prescanner->append)
    {
        continuate_lines(prescanner);
    }

    print_lines(prescanner);

    close_files(prescanner);
}

static void open_files(prescanner_t* prescanner)
{
    if (strcmp(prescanner->input_filename, "-") != 0)
    {
        prescanner->input_file = fopen(prescanner->input_filename, "r");

        if (prescanner->input_file == NULL)
        {
            fatal_error("Cannot open input file '%s' (%s)\n", prescanner->input_filename,
                    strerror(errno));
        }
    }
    else 
    {
        prescanner->input_file = stdin;
        prescanner->input_filename = "(stdin)";
    }

    if (strcmp(prescanner->output_filename, "-") != 0)
    {
        prescanner->output_file = fopen(prescanner->output_filename, "w");

        if (prescanner->output_file == NULL)
        {
            fatal_error("Cannot open output file '%s' (%s)\n", prescanner->output_filename,
                    strerror(errno));
        }
    }
    else
    {
        prescanner->output_file = stdout;
        prescanner->output_filename = "(stdout)";
    }
}

static void close_files(prescanner_t* prescanner)
{
    if (prescanner->input_file != stdin)
    {
        fclose(prescanner->input_file);
    }
    if (prescanner->output_file != stdout)
    {
        fclose(prescanner->output_file);
    }
}

static const char* get_filename_include(char* c, regmatch_t sub_matching[]);
static const char* fortran_literal(const char* c);
static const char* create_new_include_filename(const char* c);
static void manage_included_file(
        prescanner_t* prescanner,
        const char* included_filename,
        const char* new_included_filename,
        FILE** handle_included_file, 
        FILE** handle_output_file);

static void handle_include_line(
        prescanner_t* prescanner,
        line_t* iter,
        regmatch_t *sub_matching)
{
    static int maximum_nesting_level = 0;
    // We save current information
    line_t* current_file_lines = file_lines;
    line_t* current_last_line = last_line;
    FILE* current_input_file = prescanner->input_file;
    FILE* current_output_file = prescanner->output_file;
    const char* current_filename = prescanner->input_filename;

    if (maximum_nesting_level > 99)
    {
        fatal_error("%s:%d: error: too many levels of nesting (> %d)",
                prescanner->input_filename,
                iter->line_number,
                maximum_nesting_level);
    }
    maximum_nesting_level++;

    // Get the filename
    const char* included_filename = get_filename_include(iter->line, sub_matching);
    const char* new_included_filename = create_new_include_filename(included_filename);
    const char* fortran_literal_filename = fortran_literal(new_included_filename);

    // We replace the include with the new file
    iter->line[0] = '\0';
    strcat(iter->line, "      INCLUDE \"");
    strcat(iter->line, fortran_literal_filename);
    strcat(iter->line, "\"");

    DEBUG_CODE()
    {
        fprintf(stderr, "DEBUG: INCLUDE LINE-> Opening file '%s'\n", included_filename);
    }

    manage_included_file(prescanner, included_filename, new_included_filename, &prescanner->input_file, &prescanner->output_file);

    if (prescanner->input_file == NULL)
    {
        fatal_error("%s:%d: error: cannot open included file '%s' (%s)\n",
                prescanner->input_filename,
                iter->line_number,
                included_filename,
                strerror(errno));
    }

    // Now, recursive processing
    fortran_prescanner_process(prescanner);

    maximum_nesting_level--;

    // We restore saved information
    file_lines = current_file_lines;
    last_line = current_last_line;
    prescanner->input_filename = current_filename;
    prescanner->input_file = current_input_file;
    prescanner->output_file = current_output_file;
}

// Don't modify this regex without updating where will the filename go (currently in sub_matching[1])
#define INCLUDE_CONSTANT_STRING "((['](([^']|[']['])+)['])|([\"](([^\"]|[\"][\"])+)[\"]))"
#define INCLUDE_KEYWORD "i[[:blank:]]*n[[:blank:]]*c[[:blank:]]*l[[:blank:]]*u[[:blank:]]*d[[:blank:]]*e"
#define INCLUDE_DIRECTIVE_REGEX \
"^[[:blank:]]*"INCLUDE_KEYWORD"[[:blank:]]*"INCLUDE_CONSTANT_STRING"[[:blank:]]*(!.*)?$"

#define INCLUDES_PREFIX "mf95_"

static void convert_lines(prescanner_t* prescanner)
{
    // INCLUDE stuff
    regex_t match_include_directive;
    // It is enough with 2
    regmatch_t sub_matching[2];

    int code;
    if ((code = regcomp(&match_include_directive, INCLUDE_DIRECTIVE_REGEX, REG_EXTENDED | REG_ICASE)) != 0)
    {
        char error_message[120];
        regerror(code, &match_include_directive, error_message, 120);
        internal_error("Error when compiling regular expression (%s)\n", error_message);
    }

    // This is a global variable with local scope
    static language_level next = LANG_TOP_LEVEL;
    line_t* iter = file_lines;

    while (iter != NULL)
    {
        if (regexec(&match_include_directive, iter->line, 2, sub_matching, 0) == 0)
        {
            handle_include_line(prescanner, iter, sub_matching);
        }
        else
        {
            next = convert_line(prescanner, next, &iter->line, iter->line_number);
        }
        iter = iter->next;
    }
}

static void convert_whole_line_comments(void)
{
    regex_t match_starting_comment;
    line_t* iter = file_lines;
    int code;

    // D in the first column is a common extension in many compilers, we will accept it
    if ((code = regcomp(&match_starting_comment, "^(([dc*])|(([ ]{0,4}|[ ]{6,})[!]))", REG_EXTENDED | REG_NOSUB | REG_ICASE)) != 0)
    {
        char error_message[120];
        regerror(code, &match_starting_comment, error_message, 120);
        internal_error("Error when compiling regular expression (%s)\n", error_message);
    }

    while (iter != NULL)
    {
        if (regexec(&match_starting_comment, iter->line, 0, NULL, 0) == 0)
        {
            // If starts with 'd', 'c' or '*' we will switch it to '!'
            if (tolower(iter->line[0]) == 'c' || tolower(iter->line[0]) == 'd' || iter->line[0] == '*')
            {
                iter->line[0] = '!';
            }

            // If it does not have a "!" in the first column it is a "    !"
            // comment.  We will swap the ! as it will make it easier to detect
            // this as a comment
            if (iter->line[0] != '!')
            {
                int k = 0;
                while (iter->line[k] != '!') k++;

                iter->line[0] = '!';
                iter->line[k] = ' ';
            }
        }
        iter = iter->next;
    }
}

static void join_two_lines(prescanner_t* prescanner, line_t* starting_line, line_t* continuation_line);
static void join_continuated_lines(prescanner_t* prescanner)
{
    int code;
    line_t* iter = file_lines;
    regex_t match_continuated;

    line_t* previous_line = NULL;

    // First we prepare a regex to match continuated lines
    if ((code = regcomp(&match_continuated, "^(([ ]{5})|(![$](omp))|(![$][ ]{3}))[^0[:blank:]]", REG_EXTENDED | REG_NOSUB | REG_ICASE)) != 0)
    {
        char error_message[120];
        regerror(code, &match_continuated, error_message, 120);
        internal_error("Error when compiling regular expression (%s)\n", error_message);
    }

    while (iter != NULL)
    {
        if (regexec(&match_continuated, iter->line, 0, NULL, 0) == 0)
        {
            if (previous_line == NULL)
            {
                fatal_error("%s:%d: error: cannot continuate the first non-comment line", 
                        prescanner->input_filename,
                        iter->line_number);
            }

            // Pay attention to not to b0rk 'iter'
            join_two_lines(prescanner, previous_line, iter);

            line_t* old_line = iter;
            iter = iter->next;

            free_line_t(old_line);
        }
        else // Not a continuating one
        {
            // We ignore all non continuating lines except some cases checked
            // below
            char ignored_line = 1;

            const char* first_nonblank = iter->line;
            while (*first_nonblank == ' ' || *first_nonblank == '\t')
                first_nonblank++;

            if (*first_nonblank == '#')
            {
                // If this is an information line, ignore it
            }
            else if (iter->line[0] == '!')
            {
                // We ignore this comment unless it is a coco line or an omp line
                if ((strncmp(iter->line, "!$ ", 3) == 0)
                        || (strncasecmp(iter->line, "!$omp", 5) == 0))
                {
                    ignored_line = 0;
                }
            }
            else
            {
                ignored_line = 0;
            }

            if (!ignored_line)
            {
                // It could have an annoying zero 
                if (iter->line[5] == '0')
                {
                    iter->line[5] = ' ';
                }

                previous_line = iter;
            }
            iter = iter->next;
        }
    }

    regfree(&match_continuated);
}

static void trim_inline_comment(line_t* trimmed_line, char* in_string);
static void trim_right(char* c);

/*
When joining lines, unended strings play an important role as it means how does
join has to be made. E.g,

+----------- fortran 77 column width ----------------------------------+
      PRINT * , "A 
     CB"

has to be converted into Fortran 95 style as (note trailing spaces 
to match original fortran 77 columns)

PRINT *, "A                                                      B"

*/
static void join_two_lines(prescanner_t* prescanner, line_t* starting_line, line_t* continuation_line)
{
    // We don't want to append the next line onto a comment unless this is a coco line
    // or an omp line (where trimming would cause havoc to the line)
    char unended_string = 0;
    if (strncmp(starting_line->line, "!$ ", 3) != 0 &&
            ((strncasecmp(starting_line->line, "!$omp", 5) != 0)))
    {
        trim_inline_comment(starting_line, &unended_string);
    }
    // unfortunately comments may appear within a OMP or coco line so we do a dirty trick here,
    // we will trim after the omp sentinel and then restore the line
    else
    {
        starting_line->line = starting_line->line+6;
        trim_inline_comment(starting_line, &unended_string);
        starting_line->line = starting_line->line-6;
    }

    // This is the easy one
    // First we reallocate starting_line because we will append continuation_line to it
    int new_length = strlen(starting_line->line) + strlen(continuation_line->line) + 1;

    starting_line->line = NEW_REALLOC(char, starting_line->line, new_length);

    // If we pad the lines but we are not in a string, the padding that
    // cut_lines added is undesirable and we remove it
    if (prescanner->pad_strings && !unended_string)
    {
        trim_right(starting_line->line);
    }

    // We ignore 6 first characters (as they contain "     C" or "!$OMP C" or "!$    C")
    strcat(starting_line->line, &continuation_line->line[6]);

    if (starting_line->next != continuation_line)
    {
        // This can happen if there are comments between the
        // continuated lines, we must free them or they will be
        // leaked
        line_t* discarded;
        line_t* comment = starting_line->next;
        while (comment != continuation_line)
        {
            discarded = comment;
            comment = comment->next;
            // Discard comment
            free_line_t(discarded);
            // Each comment counts as one line
            starting_line->joined_lines++;
        }
    }

    // Relink properly starting_line
    starting_line->next = continuation_line->next;

    // We must increment the number of joined_lines
    starting_line->joined_lines++;
}

/*
   This function trims inline comments and optionally can
   say if there is an opened string

     PRINT *, "A" !BC  -------> PRINT *, "A"
                                (ended string)

     PRINT *, "A  !BC  -------> PRINT *, "A  !BC
                                (unended string)
 */
static void trim_inline_comment(line_t* trimmed_line, char* unended_string)
{
    // First we scan left to right to see if trimmed_line has left a constant string opened
    // and we trim inline comments in trimmed_line
    char in_string = 0, delim_string = 0;
    char* p;

    for (p = trimmed_line->line; *p != 0; p++)
    {
        // We don't have to take into account whole line comments as the have been removed
        // previously

        if (!in_string && (*p == '\'' || *p == '"'))
        {
            // If we are not in a string and we see a delimiter
            // we have found the start of a string_constant
            in_string = 1;
            delim_string = *p;
        }
        else if (!in_string && (*p == '!'))
        {
            // Great, we have the starting of an inline comment
            // so we "zap" the line here but only if it is not followed
            // by $
            if (*(p+1) != '$')
            {
                *p = 0;
                // Dirty hack to stop the loop in the next iteration
                *(p + 1) = 0;
            }
        }
        else if (in_string && *p == delim_string)
        {
            // We may have found the end of a string_constant
            if (*(p + 1) == delim_string) 
            {
                // The string did not ended here, it was only the literal of the delimiter character
                // We advance the pointer as we are not interested in reading the delimiter again
                p++;
            }
            else 
            {
                // The string has ended 
                // Ok, if it was continuated and this was the last column
                // it could still be open, but for joining is enough like this
                in_string = 0;
            }
        }
    }

    if (unended_string) *unended_string = in_string;
}

#if 0
static void remove_inlined_comments(void)
{
    line_t* iter = file_lines;
    char unended_string;
    while (iter != NULL)
    {
        // Getting here that a string is unended 
        // would be a scanning error
        trim_inline_comment(iter, &unended_string);

        if (unended_string != 0)
        {
            fatal_error("%s:%d: error: unended string at line %d !\n", 
                    prescanner->input_filename,
                    iter->line_number);
        }
        iter = iter->next;
    }
}
#endif

static void print_lines(prescanner_t* prescanner)
{
    line_t* iter = file_lines;
    while (iter != NULL)
    {
        unsigned int i = 0;
        if (prescanner->line_marks)
        {
            fprintf(prescanner->output_file, "#line %d \"%s\"\n",
                    iter->line_number,
                    prescanner->input_filename);
        }
        fprintf(prescanner->output_file, "%s", iter->line);
        if (iter->next != NULL) 
        {
            fprintf(prescanner->output_file, "\n");
        }
        if (!prescanner->line_marks)
        {
            for (i = 0; i < iter->joined_lines; i++)
            {
                fprintf(prescanner->output_file, "\n");
            }
        }
        iter = iter->next;
    }

    // End always with a newline
    fprintf(prescanner->output_file, "\n");
}

static char is_tab_form(const char* c)
{
    int i;
    for (i = 0; i < 6; i++)
    {
        if (c[i] == '\t')
            return 1;
    }

    return 0;
}

static void cut_lines(prescanner_t* prescanner)
{
    line_t* iter = file_lines;

    while (iter != NULL)
    {
        const char *p = &(iter->line[0]);
        while (*p == ' ' || *p == '\t')
            p++;

        // This is a line information, do not cut
        if (*p == '#')
        {
            iter = iter->next;
            continue;
        }

        // First we normalize tab initiated lines as they are really annoying later
        if (is_tab_form(iter->line))
        {
            normalize_line(prescanner, &iter->line, iter->line_number);
        }

        iter->line[prescanner->width] = '\0';
        if (prescanner->pad_strings)
        {
            // Pad lines with spaces until the column width.
            // This will make easier to join unended strings
            // with padding
            int actual_length = strlen(iter->line);
            for (; actual_length < prescanner->width; actual_length++)
            {
                iter->line[actual_length] = ' ';
            }
        }

        iter = iter->next;
    }
}

static void read_lines(prescanner_t* prescanner)
{
    unsigned int line_number = 1;
    while (!feof(prescanner->input_file))
    {
        // We will start with a width + 10 buffer size
        int buffer_size = prescanner->width + 10;
        char* line_buffer = NEW_VEC0(char, buffer_size);

        // Read till '\n' or till buffer_size-1
        if (fgets(line_buffer, buffer_size, prescanner->input_file) == NULL)
        {
            if (ferror(prescanner->input_file))
            {
                fatal_error("error while reading line");
            }
        }

        // How many characters have we read
        int length_read = strlen(line_buffer);

        // Have we read all the line ? (we could have reach the end of the file ...)
        char was_eof = feof(prescanner->input_file);

        // The line is not '\n'-ended and we are not in the EOF
        // so we must enlarge the buffer
        while ((length_read > 0) && line_buffer[length_read-1] != '\n' && !was_eof)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "DEBUG: Enlarging after having read @%s|\n", line_buffer);
            }
            // Enlarge exponentially
            line_buffer = NEW_REALLOC(char, line_buffer, 2*buffer_size);

            // We read from the former end
            if (fgets(&line_buffer[length_read], buffer_size, prescanner->input_file) == NULL)
            {
                if (ferror(prescanner->input_file))
                {
                    fatal_error("error while reading line");
                }
            }
            
            buffer_size = buffer_size * 2;
            length_read = strlen(line_buffer);
            was_eof = feof(prescanner->input_file);
        }

        // Remove '\n'
        if (!was_eof)
        {
            line_buffer[length_read-1] = '\0';
            length_read--;
        }

        if (length_read > 0)
        {
            // We found \r\n, kill the '\r' as well
            if (line_buffer[length_read-1] == '\r')
            {
                line_buffer[length_read-1] = '\0';
                length_read--;
            }
        }

        if (is_blank_string(line_buffer))
        {
            // Merrily ignore this line
            DELETE(line_buffer);
        }
        else
        {

            line_t* new_line = NEW0(line_t);

            DEBUG_CODE()
            {
                fprintf(stderr, "DEBUG: We have read @%s|\n", line_buffer);
            }

            new_line->line = line_buffer;
            new_line->line_number = line_number;
            new_line->next = NULL;
            new_line->joined_lines = 0;

            if (file_lines == NULL)
            {
                file_lines = new_line;
                last_line = new_line;
            }
            else
            {
                last_line->next = new_line;
                last_line = new_line;
            }

        }

        line_number++;
    }
}

static void trim_right(char* c)
{
    int length = strlen(c);

    if (length > 0)
    {
        length--;
        while ((length >= 0) && (c[length] == ' ')) length--;

        c[length + 1] = '\0';
    }
}

static const char* get_filename_include(char* c, regmatch_t sub_matching[])
{
    // Interesting information is in sub_matching[1]
    int i;
    char* filename = NEW_VEC0(char, (sub_matching[1].rm_eo - sub_matching[1].rm_so));
    char delim = c[sub_matching[1].rm_so];

    // +1 and -1sub_matching[1].rm_so to jump over delimiters
    int j = 0;
    for (i = (sub_matching[1].rm_so+1); i < (sub_matching[1].rm_eo-1); i++)
    {
        filename[j] = c[i];
        // Jump over the next delimiter
        if (c[i] == delim)
        {
            i++;
        }
        j++;
    }
    filename[j] = '\0';

    const char* result = uniquestr(filename);
    DELETE(filename);

    return result;
}

static const char* fortran_literal(const char* c)
{
    const char* p = c;
    int num_quotes = 0;
    while (*p != 0)
    {
        if (*p == '"') num_quotes++;
        p++;
    }

    char* result = NEW_VEC0(char, strlen(c) + num_quotes + 1);
    int i = 0;
    p = c;  

    while (*p != 0)
    {
        if (*p == '"')
        {
            result[i] = '"';
            i++;
        }
        result[i] = *p;
        i++;
        p++;
    }
    result[i] = '\0';

    const char* literal = uniquestr(result);
    return literal;
}

static const char* create_new_include_filename(const char* c)
{
    const char* result = NULL;
    const char* module_basename = give_basename(c);

    static int num_module = 0;
    uniquestr_sprintf(&result, INCLUDES_PREFIX "%d_%s", num_module, module_basename);
    num_module++;

    return result;
}

/*
12345CLALA
*/

static void normalize_line(prescanner_t* prescanner, char** line, unsigned int line_num)
{
    char* normalized_string;
    // The new string has to be at least "width" long
    int real_size = (int)strlen(*line) < prescanner->width ? prescanner->width + 10 : (int)strlen(*line) + 10;

    normalized_string = NEW_VEC0(char, real_size);

    const char* p = *line;
    char *q = normalized_string;

    int i;
    for (i = 0; i < 5; i++)
    {
        if (*p != '\t')
        {
            *q = *p;
            p++; q++;
        }
        else
        {
            // Ignore tab and leave the loop
            p++;
            break;
        }
    }

    // Note that the sixth column (i == 5) is filled conditionally depending on
    // what went after the tab
    while (i < 5)
    {
        *q = ' ';
        q++;
        i++;
    }

    // Now we fill the 6th column (i == 5)
    // This is a continuation line if the tab was followed by a nonzero digit
    if (!isdigit(*p) || *p == '0')
    {
        // Not a digit or a zero, add a blank to skip the continuation column
        *q = ' ';
        q++;
    }
    else
    {
        // Use this as the continuation
        *q = *p;
        p++;
        q++;

        // If this is a continuation, all previous characters should have been
        // blanks, clear them but emit a warning
        char some_was_nonblank = 0;
        for (i = 0; i < 5; i++)
        {
            some_was_nonblank = some_was_nonblank || (normalized_string[i] != ' ');
            normalized_string[i] = ' ';
        }

        if (some_was_nonblank)
        {
            fprintf(stderr, "%s:%u: warning: continuation line contains nonblank characters before continuation column\n",
                    prescanner->input_filename,
                    line_num);
        }
    }

    // The remainder of the line
    strcat(normalized_string, p);

    DELETE(*line);
    *line = normalized_string;
}

static void continuate_lines(prescanner_t* prescanner)
{
    line_t* iter = file_lines;
    while (iter != NULL)
    {
        // Make sure there are not remaining spaces
        trim_right(iter->line);
        if ((int)strlen(iter->line) > prescanner->width)
        {
            // We must cut this line
            // TODO - Check for overruns
            char* c = NEW_VEC0(char, strlen(&iter->line[prescanner->width]) + 2);
            sprintf(c, "&%s", &iter->line[prescanner->width]);
            iter->line[prescanner->width] = '\0';
            strcat(iter->line, "&");

            line_t* new_line = NEW0(line_t);
            new_line->line = c;
            new_line->line_number = 0;
            new_line->next = iter->next;
            new_line->joined_lines = (iter->joined_lines > 0) ? (iter->joined_lines - 1) : 0;
            iter->next = new_line;
            iter->joined_lines = 0;

            // Next iteration will handle new_line
        }
        iter = iter->next;
    }
}

static void free_line_t(line_t* l)
{
    DELETE(l->line);
    DELETE(l);
}

static void manage_included_file(
        prescanner_t* prescanner, 
        const char* included_filename, 
        const char* new_included_filename,
        FILE** handle_included_file, 
        FILE** handle_output_file)
{
    *handle_included_file = NULL;
    *handle_output_file = NULL;

    int i;
    for (i = 0; i < prescanner->num_include_directories; i++)
    {
        FILE* handle = NULL;

        const char* full_name = NULL;
        uniquestr_sprintf(&full_name, "%s/%s", prescanner->include_directories[i], included_filename);

        handle = fopen(full_name, "r");

        if (handle != NULL)
        {
            prescanner->input_filename = full_name;
            // We got to open the include then create the output filename
            // 
            // If the user specified a directory for include regeneration use it
            // otherwise use the original directory of this include
            const char* full_output_name = NULL;
            if (prescanner->output_include_directory == NULL)
            {
                uniquestr_sprintf(&full_output_name, "%s/%s",
                        prescanner->include_directories[i],
                        new_included_filename);
            }
            else
            {
                uniquestr_sprintf(&full_output_name, "%s/%s",
                        prescanner->output_include_directory,
                        new_included_filename);
            }

            FILE* handle_output;

            handle_output = fopen(full_output_name, "w");

            if (handle_output == NULL)
            {
                fatal_error("Cannot open output of included file '%s' (%s)\n", 
                        full_output_name, strerror(errno));
            }

            *handle_included_file = handle;
            *handle_output_file = handle_output;
            return;
        }
    }
}
