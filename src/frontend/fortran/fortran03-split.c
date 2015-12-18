/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
 #include "config.h"
#endif

#include <string.h>
#include <errno.h>
#include <signal.h>
#include "cxx-ast.h"
#include "fortran03-utils.h"
#include "fortran03-split.h"
#include "fortran03-lexer.h"
#include "fortran03-parser-internal.h"
#include "cxx-utils.h"
#include "cxx-driver-utils.h"

static char check_for_comment(char* c);
static char check_for_construct(char *c, char *prefix, int max_length);

static void double_continuate(FILE* output, const char* c, int width, int* column);
static void double_continuate_construct(FILE* output, 
        const char* prefix, 
        const char* c, int width, int* column);
static char* read_whole_line(FILE* input);
static void trim_right_line(char* c);

/* 
   Ugly hacks because of the unavailable modularization of Flex
   */
typedef void* YY_BUFFER_STATE;

#ifdef FORTRAN_NEW_SCANNER
extern int mf03_prepare_string_for_scanning(const char* str);
#else
extern YY_BUFFER_STATE mf03_scan_string (const char *yy_str);
extern void mf03_switch_to_buffer (YY_BUFFER_STATE new_buffer);
extern void mf03_delete_buffer(YY_BUFFER_STATE b);
#endif
extern int mf03lex(void);

/* End of flex hacky section */

extern YYSTYPE mf03lval;

void fortran_split_lines(FILE* input, FILE* output, int width)
{
    ERROR_CONDITION(width <= 0, "Invalid width = %d\n", width);

	int length;
	char* line;

	while ((line = read_whole_line(input)) != NULL)
	{
		// We must remove trailing spaces before "\n" (if any)
		// since we cannot continuate to an empty line
		trim_right_line(line);

		// Comments that will reach here are those created within the compiler 
		// (e.g. TPL) because scanner always trims them
        char prefix[33] = { 0 };
        char is_construct = check_for_construct(line, prefix, 32);
		char is_comment = check_for_comment(line);

		length = strlen(line);
		// Many times we will fall here by means of length <= width
		if ((length <= width))
		{
			fputs(line, output);
		}
        else if (is_construct)
        {
            // Do not complicate ourselves, rely on a double continuation
            int column = 1;
            double_continuate_construct(output, prefix, line, width, &column);
            fprintf(output, "\n");
        }
        else if (is_comment)
        {
			fputs(line, output);
        }
		else
		{
			int column, next_column;
			char* position;
			char* next_position;

#ifdef FORTRAN_NEW_SCANNER
            mf03_prepare_string_for_scanning(line);
#else
			YY_BUFFER_STATE scan_line = mf03_scan_string(line);
			mf03_switch_to_buffer(scan_line);
#endif

			// Initialize stuff
			column = 1;
			position = line;

			// Scan
			int token = mf03lex();
			while (token != EOS)
			{
				// Find the token as there can be spaces
				// next_position has the first character of the token
				next_position = strstr(position, mf03lval.token_atrib.token_text);

				if (next_position == NULL)
				{
					fatal_error("Serious problem when splitting line. '%s' not found:\n\n %s", mf03lval.token_atrib.token_text, position);
				}

				// Next column has the column where the token will start
				next_column = column + (next_position - position);


				// Check if we have reached the last column or if spaces plus
				// token will not fit in this line
				if (column == width
						|| (next_column + (int)strlen(mf03lval.token_atrib.token_text) >= width))
				{
					DEBUG_CODE() DEBUG_MESSAGE("Cutting at '%s'", mf03lval.token_atrib.token_text);
					// Nothing fits here already
                    fprintf(output, "&\n");
                    column = 1;
				}

				// Write the blanks
				char* c;
				for (c = position; c < next_position; c++)
				{
					DEBUG_CODE() DEBUG_MESSAGE("%d - Blank - '%c'", column, *c);
					fprintf(output, "%c", *c);
					column++;
				}

				if ((column + (int)strlen(mf03lval.token_atrib.token_text)) >= width)
				{
					// We are very unlucky, the whole token still does not fit
					// in this line !
					double_continuate(output, mf03lval.token_atrib.token_text, width, &column);
				}
				else
				{
					// Write the token
					DEBUG_CODE() DEBUG_MESSAGE("%d - Token '%s'", column, mf03lval.token_atrib.token_text);
					fprintf(output, "%s", mf03lval.token_atrib.token_text);
					column += strlen(mf03lval.token_atrib.token_text);
				}

				// Update state to be coherent before entering the next iteration
				// column has been updated before
				position = next_position + strlen(mf03lval.token_atrib.token_text);
				token = mf03lex();
			}

			// The EOS
			fprintf(output, "\n");

#ifdef FORTRAN_NEW_SCANNER
            // Do nothing
#else
			mf03_delete_buffer(scan_line);
#endif
		}
		
		DELETE(line);
	}
}

static void double_continuate(FILE* output, const char* c, int width, int* column)
{
	// This is a naive but easy-to-reason-about-it implementation
	// It refuses to reuse the last column for other than continuation,
	// it will put a continuation even if only one character remains
	// this avoids having *column > width.
	for (; *c != '\0'; c++)
	{
		// If we are at the last column but this is not an EOS
		if ((*column == width) && (*c != '\n'))
		{
			// Double continue
			fprintf(output, "&\n&");
			*column = 2;
			DEBUG_CODE() DEBUG_MESSAGE("Cutting at '%c'", *c);
		}
		DEBUG_CODE() DEBUG_MESSAGE("%d - Letter - '%c'", *column, *c);
		fprintf(output, "%c", *c);
		(*column)++;
	}
}

static void double_continuate_construct(FILE* output, 
        const char* prefix, 
        const char* c, int width, int* column)
{
	// This is a naive but easy-to-reason-about-it implementation
	// It refuses to reuse the last column for other than continuation,
	// it will put a continuation even if only one character remains
	// this avoids having *column > width.
    char prefix_start[64];
    snprintf(prefix_start, 63, "!$%s&", prefix);
	for (; *c != '\0'; c++)
	{
		// If we are at the last column but this is not an EOS
		if ((*column == width) && (*c != '\n'))
		{
			// Double continue
			fprintf(output, "&\n%s", prefix_start);
			*column = 1 + strlen(prefix_start);
			DEBUG_CODE() DEBUG_MESSAGE("Cutting at '%c'", *c);
		}
		DEBUG_CODE() DEBUG_MESSAGE("%d - Letter - '%c'", *column, *c);
		fprintf(output, "%c", *c);
		(*column)++;
	}
}

static char check_for_comment(char* c)
{
	char* iter = c;
	while (*iter == ' ' || *iter == '\t') iter++;
	return (*iter == '!');
}

static char check_for_construct(char *c, char *prefix, int max_length)
{
    char* iter = c;
    while (*iter == ' ' || *iter == '\t') iter++;
    if (*iter != '!')
        return 0;
    iter++;

    if (*iter != '$')
        return 0;
    iter++;

    char *q = prefix;
    *q = '\0';

    int length = 0;
    while (*iter != ' ' 
            && *iter != '\t' 
            && *iter != '\0')
    {
        // Disregard such a long prefix
        if (length >= max_length)
        {
            return 0;
        }
        *q = *iter;

        q++;
        iter++;
        length++;
    }

    int i;
    char found = 0;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcasecmp(prefix, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
        {
            found = 1;
            break;
        }
    }

    return found;
}

static char* read_whole_line(FILE* input)
{
	// It should be enough
	int buffer_size = 1024;
	int was_eof;
	int length_read;
	char* temporal_buffer = NEW_VEC0(char, buffer_size);
	// We read buffer_size-1 characters
	if (fgets(temporal_buffer, buffer_size, input) == NULL)
    {
        if (ferror(input))
        {
            fatal_error("error: while starting to split file\n");
        }
    }

	if (temporal_buffer[0] == '\0')
	{
		DELETE(temporal_buffer);
		return NULL;
	}

	length_read = strlen(temporal_buffer);
	was_eof = feof(input);

	while ((temporal_buffer[length_read - 1] != '\n') && !was_eof)
	{
		temporal_buffer = NEW_REALLOC(char, temporal_buffer, 2*buffer_size);
		if (fgets(&temporal_buffer[length_read], buffer_size, input) == NULL)
        {
            if (ferror(input))
            {
                fatal_error("error: while splitting file\n");
            }
        }

		length_read = strlen(temporal_buffer);
		buffer_size = buffer_size * 2;
		was_eof = feof(input);
	}

	return temporal_buffer;
}

static void trim_right_line(char* c)
{
	int save_newline = 0;
	int length = strlen(c);

	if (c[length-1] == '\n')
	{
		length--;
		save_newline = 1;
	}

	if (length > 0)
	{
		length--;
		while ((length >= 0) && (c[length] == ' ')) length--;

		if (!save_newline)
		{
			c[length + 1] = '\0';
		}
		else
		{
			c[length + 1] = '\n';
			c[length + 2] = '\0';
		}
	}
}

