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


%{
/**
  Flex scanner of mf03

  $Id: fortran95.l,v 1.12 2008/01/09 09:26:20 luism Exp $
*/
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include "cxx-ast.h"
#include "cxx-driver-utils.h"
#include "cxx-diagnostic.h"
#include "cxx-utils.h"
#include "cxx-limits.h"
#include "filename.h"
#include "fortran03-utils.h"
#include "fortran03-lexer.h"
#include "fortran03-parser-internal.h"

static void parse_token_text(void);
static void parse_token_text_str(const char* c);

// static const char* main_input_filename = NULL;

/*
   Include stack. Based on an example in the GNU Flex manual

   The maxim level of nesting is not defined in Fortran 95 standard.
   We have set it to 99.
 */
#define MAX_INCLUDE_DEPTH 99

static int include_counter = 0;

static int include_stack_size = 0;
static int pragma_custom_var_list_parentheses = 0;
static const char *current_pragma_prefix = NULL;

struct scan_file_descriptor 
{
    // This is the (physical) filename being scanned
    const char* filename;

    // This is the logical filename that we are scanning.
    // current_filename != filename only in Fortran fixed-form because we scan
    // the output of prescanner
    const char* current_filename;

    FILE* file_descriptor;
    struct yy_buffer_state* scanning_buffer;

    // Line of current token
    unsigned int line_number;
    // Column where the current token starts
    unsigned column_number;
    // Fortran: After a joined line we have to move to this line if new_line > 0 
    unsigned int new_line; 
    // Fortran: Number of joined lines so far
    unsigned int joined_lines;
};

static struct scan_file_descriptor include_stack[MAX_INCLUDE_DEPTH];
static struct scan_file_descriptor* fortran_scanning_now = &include_stack[0];

#define MAX_PRAGMA_CONSTRUCT_STACK 64
static const char* pragma_construct_stack[MAX_PRAGMA_CONSTRUCT_STACK] = { 0 };
static const char* format_pragma_string(const char* c);
static int pragma_construct_stack_idx = 0;

#define MAX_NONBLOCK_DO_STACK 64
static int nonblock_do_stack[MAX_NONBLOCK_DO_STACK] = { 0 };
static int nonblock_do_stack_idx = 0;

static char* get_included_filename(char* linia_include, char delim);
static void open_included_file(char* include_filename);

enum continuation_kind_tag
{
    INVALID_CONTINUATION = 0,
    SINGLE_CONTINUATION,
    SINGLE_CONTINUATION_UNENDED_STRING_LITERAL,
    DOUBLE_CONTINUATION
};

static char check_continued_sentinel_line(const char* line,
        const char** sentinel,
        enum continuation_kind_tag continuation_kind);
static char is_valid_pragma_custom_prefix(const char* sentinel);
static const char* get_real_second_line(const char* second_line, 
        const char** filename, 
        unsigned int* new_line, 
        unsigned int* num_joined_lines, 
        bool ignore_comments);
static void reintroduce_continued_line(char* split_line,
        enum continuation_kind_tag continuation_kind,
        const char *sentinel);
static char end_of_file(void);
static void trim_inline_comment(char* line);

typedef
struct line_marker_flags_tag
{
    _Bool start_of_new_file:1;
    _Bool return_of_a_file:1;
    _Bool system_header_file:1;
    _Bool extern_c_block:1;
} line_marker_flags_t;

static void parse_line_marker(const char* str, const char** filename, unsigned int *line, line_marker_flags_t *flags);

static const char* return_pragma_prefix_longest_match(const char* prefix, 
        const char* lexed_directive,
        pragma_directive_kind_t* kind,
        char* original_directive);

static void increment_line_number(struct scan_file_descriptor* current_file)
{
    if (current_file->new_line > 0)
    {
        current_file->line_number = current_file->new_line + current_file->joined_lines;
        current_file->new_line = 0;
        current_file->joined_lines = 0;
    }
    else
    {
        current_file->line_number += 1 + current_file->joined_lines;
        current_file->joined_lines = 0;
    }
}

static int is_format_statement(char* token);

static char last_eos = 1;

// Hollerith
static char hollerith_buffer[MCXX_MAX_LENGTH_HOLLERITH_CONSTANT] = { };
static int length_hollerith_buffer = 0;
static int count_hollerith_constant = 0;

#define RETURN_TOKEN(x) do { last_eos = 0; return (x); }  while(0)
#define RETURN_EOS { if (!last_eos) { last_eos = 1; return (EOS); } }
#define RETURN_PRAGMA_EOS { if (!last_eos) { last_eos = 1; return (PRAGMA_CUSTOM_NEWLINE); } }


%}

%option noyyalloc noyyrealloc noyyfree noinput

 /* Compatibility with different file formats unix/mac/dos */
newline ([\n]|[\r]|([\r][\n]))

INCLUDE_CONSTANT_STRING_A (([0-9]+)_)?[']([^'\n\r]|(['][']))+[']
INCLUDE_CONSTANT_STRING_B (([0-9]+)_)?["]([^"\n\r]|(["]["]))+["]

QUOTE_STRING ([']([^'\n\r]|(['][']))*['])
DOUBLEQUOTE_STRING (["]([^"\n\r]|(["]["]))*["])

FORTRAN_STRING ({QUOTE_STRING}|{DOUBLEQUOTE_STRING})
UNENDED_QUOTE_STRING       ([']([^'\n\r]|(['][']))*)
UNENDED_DOUBLEQUOTE_STRING (["]([^"\n\r]|(["]["]))*)

UNENDED_STRING ({UNENDED_QUOTE_STRING}|{UNENDED_DOUBLEQUOTE_STRING})

hexdigit        [a-fA-F0-9]
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
digit       [0-9]
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
nondigit        ([_a-zA-Z]|uchar)
idnondigit  ({nondigit}|{uchar})
identifier ([a-z]([a-z0-9_]*))
kind (({digit}+)|({identifier}))

 /* Sentinels as much as 32 letters */
sentinel [!][$][a-zA-Z0-9_]{0,32}

empty_sentinel [!][$]
nonempty_sentinel [!][$][a-zA-Z0-9_]{1,32}

comment_not_sentinel [!]([^$][^\n\r]*)?

 /* Minimal C stuff */
c_octaldigit      [0-7]
c_hexdigit        [a-fA-F0-9]
c_escape_seq      ((\\["'?eabfnrtv\\])|(\\{c_octaldigit}{1,3})|((\\x)+{c_hexdigit}+))
schar       ([^\\"\n\r]|{c_escape_seq}|(\\["]))

 /* Continuations */
tail_of_continued ([[:blank:]]*(![^\n\r]*)?)
tail_of_continued_string ([[:blank:]]*)
blank_of_continuation ([[:blank:]]*([#!][^\n\r]*)?)
next_line_single_continued ([[:blank:]]*[^&][^\n\r]*)
pad_continuation {tail_of_continued}{newline}({blank_of_continuation}{newline})*
pad_continuation_str {tail_of_continued_string}{newline}({blank_of_continuation}{newline})*
 /* The second line SHOULD have something NON-BLANK after the & */
next_line_double_continued ([[:blank:]]*&[^\n\r]+)

%x preprocess
%x COMMENT
%x SEEN_FORMAT
%x NONBLOCK_DO
%x pragma_line
%x unknown_pragma
%x pragma_custom_directive
%x pragma_custom_clause_first
%x pragma_custom_clause
%x pragma_custom_var_list
%x pragma_custom_comment
%x hollerith_constant

%%

^[ \t]*# { 
    BEGIN(preprocess); 
}

<preprocess>([ ]*line)?[ ]+[[:digit:]]+([ ]+\"{schar}*\")?[^\n\r]* {
    const char *directive = yytext;

    unsigned int line = 0;
    const char* filename = NULL;
    line_marker_flags_t flags;

    parse_line_marker(directive, &filename, &line, &flags);

    // Update the line number, note that it is line_num - 1 
    // because {end-of-line} is not handled here, but in another rule
    fortran_scanning_now->line_number = line - 1;

    if (filename != NULL)
    {
        fortran_scanning_now->current_filename = filename;
    }
}

<preprocess>[ ]{digit}+[ ]+["][^"]+["][^\n\r]* {
	char* directive = yytext;

    unsigned int line = 0;
    const char* filename = NULL;
    line_marker_flags_t flags;

    parse_line_marker(directive, &filename, &line, &flags);

    if (flags.system_header_file)
    {
        filename = give_basename(filename);
    }

    if (flags.start_of_new_file 
            || flags.system_header_file)
    {
        if (include_counter == 0)
        {
            top_level_include_t *new_top_level_include = NEW0(top_level_include_t);

            new_top_level_include->included_file = uniquestr(filename);

            if (flags.system_header_file)
            {
                new_top_level_include->system_include = 1;
            }

            P_LIST_ADD(CURRENT_COMPILED_FILE->top_level_include_list,
                    CURRENT_COMPILED_FILE->num_top_level_includes,
                    new_top_level_include);
        }
    }

    if (flags.start_of_new_file)
    {
        include_counter++;
    }

    if (flags.return_of_a_file)
    {
        include_counter = (include_counter > 0) ? (include_counter - 1) : 0;
    }

    // Update the line number, note that it is line - 1 
    // because {newline} is not handled here, but in another rule
	fortran_scanning_now->line_number = (line - 1);
    // Update file
	fortran_scanning_now->current_filename = uniquestr(filename);
}

<preprocess>.       { /* ignore line */  }
<preprocess>{newline}  { 
    increment_line_number(fortran_scanning_now);
    BEGIN(INITIAL); 
}

<<EOF>> {
	if (!last_eos)
	{
		last_eos = 1;
		return (EOS);
	}
	else
	{
		yyterminate();
	}
}

^[[:blank:]]*INCLUDE[[:blank:]]*{INCLUDE_CONSTANT_STRING_A}[[:blank:]]*([!][^\n\r]*)?{newline} {
	char* filename;

	filename = get_included_filename(yytext, '\'');

	// This must be done before changing the buffer
	fortran_scanning_now->line_number++;

	open_included_file(filename);

	DELETE(filename);
}

^[[:blank:]]*INCLUDE[[:blank:]]*{INCLUDE_CONSTANT_STRING_B}[[:blank:]]*([!][^\n\r]*)?{newline} {
	char* filename;

	filename = get_included_filename(yytext, '"');

	// This must be done before changing the buffer
	fortran_scanning_now->line_number++;

	open_included_file(filename);

	DELETE(filename);
}


^[[:blank:]]*[!][$]MCC[[:blank:]]+enable_debug[[:blank:]]*{newline} {
    // This is only for debugging Mercurium itself
    extern void _enable_debug(void);
    _enable_debug();
    fortran_scanning_now->line_number++;
    // Do not return anything
}

^[[:blank:]]*[!][$]MCC[[:blank:]]+disable_debug[[:blank:]]*{newline} {
    // This is only for debugging Mercurium itself
    extern void _disable_debug(void);
    _disable_debug();
    fortran_scanning_now->line_number++;
    // Do not return anything
}

^[[:blank:]]*{sentinel}[[:blank:]]+(([^\n\r!'"])|{FORTRAN_STRING})+&[[:blank:]]*(![^\n\r]*)?{newline}([[:blank:]]*({comment_not_sentinel})?{newline})*[[:blank:]]*{sentinel}[[:blank:]]*&[^\n\r]+{newline} {
	// !$SENTINEL INT&
	// !$SENTINEL   &EGE&
	// !$SENTINEL &R :: A=3

	const char* sentinel = NULL;
	if (check_continued_sentinel_line(yytext, &sentinel, DOUBLE_CONTINUATION))
	{
	    reintroduce_continued_line(yytext, DOUBLE_CONTINUATION, sentinel);
	}
	else
	{
        if (is_valid_pragma_custom_prefix(sentinel))
        {
            warn_printf_at(
                    make_locus(
                        fortran_scanning_now->current_filename, 
                        fortran_scanning_now->line_number,
                        0),
                    "invalid continued line with mismatching sentinels, moving on\n");
        }
	    REJECT;
	}
}

^(([^\n\r!'"])|{FORTRAN_STRING})+&{pad_continuation}{next_line_double_continued}{newline} |
^(([^\n\r!'"])|{FORTRAN_STRING})*{UNENDED_STRING}&{pad_continuation_str}{next_line_double_continued}{newline} {
	// The first regex allows comments because all strings are closed
    // The second one does not allows them because the last string is opened
    // This recognizes double continued lines
	//
	// INT&
	//   &EGE&
	// &R :: A=3
	//
	// CHARACTER :: C(10) = "Hennessy &&
	//  & Patterson "

	reintroduce_continued_line(yytext, DOUBLE_CONTINUATION, NULL);
}

^[[:blank:]]*{sentinel}[[:blank:]]+(([^\n\r!'"])|{FORTRAN_STRING})+&([[:blank:]]*(![^\n\r]*)?{newline})([[:blank:]]*({comment_not_sentinel})?{newline})*[[:blank:]]*{sentinel}[[:blank:]]+[^&][^\n\r]*{newline} {
	// !$SENTINEL INTEGER &
	// !$SENTINEL :: A=3

	const char* sentinel = NULL;
	if (check_continued_sentinel_line(yytext, &sentinel, SINGLE_CONTINUATION))
	{
	    reintroduce_continued_line(yytext, SINGLE_CONTINUATION, sentinel);
	}
	else
	{

        if (is_valid_pragma_custom_prefix(sentinel))
        {
            warn_printf_at(
                    make_locus(
                        fortran_scanning_now->current_filename, 
                        fortran_scanning_now->line_number,
                        0),
                    "invalid continued line with mismatching sentinels, moving on\n");
        }
        REJECT;
	}
}

^(([^\n\r!'"])|{FORTRAN_STRING})+&{pad_continuation}{next_line_single_continued}{newline} {
	// This regex allows comments at the end because all strings are closed
    //
    // SUBROUTINE&
    // S
    // INTEGER &
    // :: A = 3

	reintroduce_continued_line(yytext, SINGLE_CONTINUATION, NULL);
}
^(([^\n\r!'"])|{FORTRAN_STRING})*{UNENDED_STRING}&{pad_continuation_str}{next_line_single_continued}{newline} {
    // This regex does not allow comments at the end them because the last
    // string is opened
    //
    // CHARACTER :: C (10) = "A&
    //         B"
    //
    // The above statement is equivalent to
    //
    // CHARACTER :: C (10) = "A&
    //         B"

	reintroduce_continued_line(yytext, SINGLE_CONTINUATION_UNENDED_STRING_LITERAL, NULL);
}

[0-9]+[[:blank:]]+format([^'"\n\r;]|([']([^'\n\r]|(['][']))*['])|(["]([^"\n\r]|(["]["]))*["]))*([;]|{newline})  {
	// Format preanalisis
	// Format must be scanned different and we must recognize it first

/*
	scanning_format = is_format_statement(yytext);
	DEBUG_CODE() DEBUG_MESSAGE("Test if '%s' is FORMAT = %d", yytext, scanning_format);
	if (scanning_format)
	{
		DEBUG_CODE() DEBUG_MESSAGE("We have spotted a FORMAT statement! '%s'", yytext);
	}
*/

    if (is_format_statement(yytext))
    {
        BEGIN(SEEN_FORMAT);
        yyless(0);
    }
    else
    {
        // We will reject it always in a ugly way of doing preanalysis
        // This reduces efficiency of this scanner though
        REJECT;
    }
}

<SEEN_FORMAT>{

    [0-9]+ {
        parse_token_text();
        RETURN_TOKEN(DECIMAL_LITERAL);
    }

    [\t ] { }

    format {
        parse_token_text();
        RETURN_TOKEN(TOKEN_FORMAT);
    }

    [(]([^'"\n\r;]|([']([^'\n\r]|(['][']))*['])|(["]([^"\n\r]|(["]["]))*["]))*[)][[:blank:]]*/([;]|{newline}) {

        BEGIN(0);
        parse_token_text();
        RETURN_TOKEN(FORMAT_SPEC);
    }
}

[!] {
	BEGIN(COMMENT);
}

^[[:blank:]]*{nonempty_sentinel} {
    // Custom pragmas
    const char *sentinel = yytext;

    while (*sentinel == ' ' || *sentinel == '\t') 
        sentinel++;

    ERROR_CONDITION(*sentinel != '!', "Invalid sentinel '%s'", yytext);
    sentinel++;
    ERROR_CONDITION(*sentinel != '$', "Invalid sentinel '%s'", yytext);
    sentinel++;

    int i;
    char found = 0;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcasecmp(sentinel, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
        {
            found = 1;
            break;
        }
    }

    if (found)
    {
        BEGIN(pragma_custom_directive);
        parse_token_text_str(sentinel);
        current_pragma_prefix = CURRENT_CONFIGURATION->pragma_custom_prefix[i];
        //return PRAGMA_CUSTOM;
        RETURN_TOKEN(PRAGMA_CUSTOM);
    }
    else
    {
        warn_printf_at(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "ignoring unknown '!$%s' directive\n",
                sentinel);
        // Ignore this sentinel
        BEGIN(unknown_pragma);
        yyless(0);
    }
}

^[[:blank:]]*{empty_sentinel}/[ \t] {
    // !$ foo
    // This is a zero-length sentinel used for conditional compilation in OpenMP
    if (!CURRENT_CONFIGURATION->disable_empty_sentinels)
    {
        // Move on as if nothing happened
        // This should look like as if this had never happened
        yy_set_bol(1);
        last_eos = 1;
        BEGIN(INITIAL);
    }
    else
    {
        // Handle this as a comment
        BEGIN(COMMENT);
    }
}

<unknown_pragma>{
[^\n\r]* {

  /* This will also catch #pragma gcc */
  // Fortran does not work with pragmas so, if it's failed matching a prefix, the whole line will be considered a comment.
	parse_token_text();
	BEGIN(COMMENT);
	RETURN_TOKEN(UNKNOWN_PRAGMA);
}

}

<pragma_custom_directive>{

{identifier}([ \t]+{identifier})* {
	BEGIN(pragma_custom_clause_first);

    const char* directive = yytext;

    char is_end_directive = (strlen(directive) > 3
            && strncasecmp(directive, "end", 3) == 0);

    if (is_end_directive)
    {
        directive += 3;
        while (*directive == ' ' || *directive == '\t')
            directive++;
    }

    pragma_directive_kind_t directive_kind = PDK_NONE; 
    // This call will unput appropriately what was lexed too much
    char original_directive[strlen(yytext) + 1];
    memset(original_directive, 0, sizeof(original_directive));

    const char* longest_match = return_pragma_prefix_longest_match(
            current_pragma_prefix, directive, &directive_kind, original_directive);

    if (!is_end_directive 
            && directive_kind == PDK_NONE)
    {
        // Give a chance to the empty "directive" since some ill-designed
        // pragmas might need this
        directive_kind = lookup_pragma_directive(current_pragma_prefix, "");
        parse_token_text(); 
    }
    else
    {
        if (!is_end_directive)
        {
            parse_token_text_str(original_directive);
        }
        else
        {
            // prepend 'end '
            char c[64] = { 0 };

            snprintf(c, 63, 
                    "%c%c%c %s", 
                    yytext[0], yytext[1], yytext[2],
                    original_directive);
            c[63] = '\0';
            parse_token_text_str(c);
        }
    }

    int token = 0;
    switch (directive_kind)
    {
        case PDK_DIRECTIVE : 
            {
                if (!is_end_directive)
                {
                    token = PRAGMA_CUSTOM_DIRECTIVE;
                }
                else
                {
                    fatal_printf_at(
                            make_locus(
                                fortran_scanning_now->current_filename,
                                fortran_scanning_now->line_number,
                                0),
                            "'!$%s %s' directive cannot be '!$%s END %s'\n", 
                            strtoupper(current_pragma_prefix),
                            strtoupper(longest_match),
                            strtoupper(current_pragma_prefix),
                            strtoupper(longest_match));
                }
                break;
            }
        case PDK_CONSTRUCT_NOEND :
            {
                if (!is_end_directive)
                {
                    token = PRAGMA_CUSTOM_CONSTRUCT_NOEND;
                }
                else
                {
                    token = PRAGMA_CUSTOM_END_CONSTRUCT_NOEND;
                }
                break;
            }
        case PDK_CONSTRUCT :
            {
                if (!is_end_directive)
                {
                    ERROR_CONDITION(pragma_construct_stack_idx == MAX_PRAGMA_CONSTRUCT_STACK,
                            "Too many pragmas nested %d", MAX_PRAGMA_CONSTRUCT_STACK);
                    pragma_construct_stack[pragma_construct_stack_idx] = longest_match;
                    pragma_construct_stack_idx++;
                    token = PRAGMA_CUSTOM_CONSTRUCT;
                }
                else
                {
                    if (pragma_construct_stack_idx > 0)
                    {
                        if (strcmp(pragma_construct_stack[pragma_construct_stack_idx-1], longest_match) != 0)
                        {
                            fatal_printf_at(
                                    make_locus(
                                        fortran_scanning_now->current_filename,
                                        fortran_scanning_now->line_number,
                                        0),
                                    "invalid nesting for '!$%s %s', expecting '!$%s END %s'\n", 
                                    strtoupper(current_pragma_prefix), 
                                    strtoupper(longest_match),
                                    strtoupper(current_pragma_prefix), 
                                    strtoupper(format_pragma_string(pragma_construct_stack[pragma_construct_stack_idx - 1])));
                        }
                        else
                        {
                            pragma_construct_stack_idx--;
                            token = PRAGMA_CUSTOM_END_CONSTRUCT;
                        }
                    }
                    else
                    {
                        fatal_printf_at(
                                make_locus(
                                    fortran_scanning_now->current_filename,
                                    fortran_scanning_now->line_number,
                                    0),
                                "bad nesting for '!$%s %s'\n",
                                strtoupper(current_pragma_prefix), 
                                strtoupper(longest_match));
                    }
                }
                break;
            }
        case PDK_NONE :
            {
                fatal_printf_at(
                        make_locus(
                            fortran_scanning_now->current_filename,
                            fortran_scanning_now->line_number,
                            0),
                        "unknown directive '!$%s %s'. Maybe you forgot to register it?",
                        fortran_scanning_now->current_filename,
                        fortran_scanning_now->line_number,
                        current_pragma_prefix, strtoupper(yytext)
                        );
            }
        default:
            internal_error("Invalid pragma directive kind kind=%d", directive_kind);
    }

    current_pragma_prefix = NULL;
    RETURN_TOKEN(token);
}

[ \t] {
	// This blank must be eaten and should be mandatory
}

}


<pragma_custom_clause_first>{

{newline} {
    fortran_scanning_now->line_number++;
	BEGIN(INITIAL);
	RETURN_PRAGMA_EOS;
}

{identifier} {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text();
	//return PRAGMA_CUSTOM_CLAUSE; 
	RETURN_TOKEN(PRAGMA_CUSTOM_CLAUSE);
}

[ \t] {
	// This blank must be eaten
}

[(] {
        // Parameter
	pragma_custom_var_list_parentheses = 1;
    BEGIN(pragma_custom_var_list);
    //return '(';
    RETURN_TOKEN('(');
}

[!] {
    BEGIN(pragma_custom_comment);
}

}

<pragma_custom_clause>{

{newline} {
    fortran_scanning_now->line_number++;
	BEGIN(INITIAL);
	RETURN_PRAGMA_EOS;
}

{identifier} {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text();
	//return PRAGMA_CUSTOM_CLAUSE; 
	RETURN_TOKEN(PRAGMA_CUSTOM_CLAUSE);
}

[ \t] {
	// This blank must be eaten
}

"," {
	parse_token_text();
    RETURN_TOKEN(',');
}

[!] {
    BEGIN(pragma_custom_comment);
}

}

<pragma_custom_var_list>{

[(] {
	parse_token_text();
	if (pragma_custom_var_list_parentheses == 0)
    {
        pragma_custom_var_list_parentheses++;
        RETURN_TOKEN('(');
    }
    else
    {
        pragma_custom_var_list_parentheses++;
        RETURN_TOKEN(PRAGMA_CLAUSE_ARG_TEXT);
    }
}

[)] {
	parse_token_text();
	pragma_custom_var_list_parentheses--;
	if (pragma_custom_var_list_parentheses == 0)
	{
		BEGIN(pragma_custom_clause);
        RETURN_TOKEN(')');
	}
    else
    {
        RETURN_TOKEN(PRAGMA_CLAUSE_ARG_TEXT);
    }
}

[ \t]+ {
    // Ignore the blank if we are not yet within any parentheses
	if (pragma_custom_var_list_parentheses > 0)
    {
        parse_token_text();
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
}

 /* Identifier */
[a-z]([a-z0-9_]*) {
    if (pragma_custom_var_list_parentheses > 0)
    {
        parse_token_text();
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
    else
    {
        // We are in .
        // #pragma foo bar . doe
        // and 'doe' must be lexed as another custom clause
        BEGIN(pragma_custom_var_list);
        parse_token_text(); 
        RETURN_TOKEN(PRAGMA_CUSTOM_CLAUSE);
    }
}

{newline} {
    // We found a newline
    if (pragma_custom_var_list_parentheses != 0)
    {
        // Well, we found the newline but parentheses were not 
        // properly nested
        warn_printf_at(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "unended custom clause. Skipping.\n");
    }
    // Head back to initial state and finish this pragma
    fortran_scanning_now->line_number++;
    BEGIN(INITIAL);
    RETURN_PRAGMA_EOS;
}

[!] {
    BEGIN(pragma_custom_comment);
}

  /* Catch all rule */
[^\n\r] {
    parse_token_text();
    RETURN_TOKEN(PRAGMA_CLAUSE_ARG_TEXT);
}

}

<pragma_custom_comment>
{

{newline} {
    fortran_scanning_now->line_number++;
	BEGIN(INITIAL);
	RETURN_PRAGMA_EOS;
}

. { 
    // Ignore everything in the comment
}

}

@NODECL-LITERAL-EXPR@ {
    parse_token_text();
    RETURN_TOKEN(NODECL_LITERAL_EXPR);
}

@NODECL-LITERAL-STMT@ {
    parse_token_text();
    RETURN_TOKEN(NODECL_LITERAL_STMT);
}

@PROGRAM-UNIT@ {
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_PROGRAM_UNIT);
}

@TYPE-LITERAL-REF@ {
    parse_token_text();
    RETURN_TOKEN(TYPE_LITERAL_REF);
}

@SYMBOL-LITERAL-REF@ {
    parse_token_text();
    RETURN_TOKEN(SYMBOL_LITERAL_REF);
}

@EXPRESSION@ {
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_EXPRESSION);
}

@STATEMENT@ {
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_STATEMENT);
}

@GLOBAL@ {
    parse_token_text();
    RETURN_TOKEN(GLOBAL);
}

@END[ ]+GLOBAL@ {
    parse_token_text();
    RETURN_TOKEN(END_GLOBAL);
}

@TYPEDEF@ {
    parse_token_text();
    RETURN_TOKEN(TYPEDEF);
}

@IS_VARIABLE@ {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IS_VARIABLE);
}

@OMP-DECLARE-REDUCTION@ {
    parse_token_text();
    return SUBPARSE_OPENMP_DECLARE_REDUCTION;
}

@OMP-DEPEND-ITEM@ {
    parse_token_text();
    return SUBPARSE_OPENMP_DEPEND_ITEM;
}

@OMPSS-DEPENDENCY-EXPR@ {
    parse_token_text();
    return SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION;
}

 /* Special tokens for placeholders */
@STATEMENT-PH::0[xX]{hexdigit}+@ { parse_token_text(); RETURN_TOKEN(STATEMENT_PLACEHOLDER); }

<COMMENT>. {
	// Eat everything in the comment
}

<COMMENT>{newline} {
	// We have arrived at the end
    increment_line_number(fortran_scanning_now);
	BEGIN(0);
	RETURN_EOS;
}

{newline} {
    increment_line_number(fortran_scanning_now);
	parse_token_text();
	BEGIN(INITIAL);
	RETURN_EOS;
}

[;] {
	parse_token_text();
	RETURN_EOS;
}


b['][0-1]+['] {
	parse_token_text();
	RETURN_TOKEN(BINARY_LITERAL);
}

b["][0-1]+["] {
	parse_token_text();
	RETURN_TOKEN(BINARY_LITERAL);
}

['][0-1]+[']b {
	parse_token_text();
	RETURN_TOKEN(BINARY_LITERAL);
}

["][0-1]+["]b {
	parse_token_text();
	RETURN_TOKEN(BINARY_LITERAL);
}

o['][0-7]+['] {
	parse_token_text();
	RETURN_TOKEN(OCTAL_LITERAL);
}

o["][0-7]+["] {
	parse_token_text();
	RETURN_TOKEN(OCTAL_LITERAL);
}

['][0-7]+[']o {
	parse_token_text();
	RETURN_TOKEN(OCTAL_LITERAL);
}

["][0-7]+["]o {
	parse_token_text();
	RETURN_TOKEN(OCTAL_LITERAL);
}

z['][0-9a-f]+['] {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

z["][0-9a-f]+["] {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

['][0-9a-f]+[']x {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

["][0-9a-f]+["]x {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

x['][0-9a-f]+['] {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

x["][0-9a-f]+["] {
	parse_token_text();
	RETURN_TOKEN(HEX_LITERAL);
}

[0-9]+[.][0-9]+([edq][+-]?[0-9]+)(_{kind})? {
	// Reals with integer part, fraction, exponent and
	// optionally kind_param
	// 123.456e789
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+[.][0-9]+(_{kind})? {
	// Reals with integer part, fraction
	// and optionally kind_param
	// 123.456
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[.][0-9]+([edq][+-]?[0-9]+)(_{kind})? {
	// Reals with fraction, exponent
	// and optionally kind_param
	// .456e789
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[.][0-9]+(_{kind})? {
	// Reals with fraction and
	// optionally kind_param
	// .456
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+[.]?([edq][+-]?[0-9]+)(_{kind})? {
	// Reals with integer part, exponent
	// and optionally kind_param
	// 123.e456
	// 123e456
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+/[.][a-z]+[.] {
	// This looks like a real but it is not
	// it is an integer followed by a defined_operator
	parse_token_text();
	RETURN_TOKEN(DECIMAL_LITERAL);
}

[0-9]+[.](_{kind})? {
	// Real with only integer part
	// and optionally kind_param
	// 123.
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+H {

    const char *q;
    {
        char* p = xstrdup(yytext);
        q = p;

        while (*p != 'H'
                && *p != 'h')
            p++;
        *p = '\0';
    }

    length_hollerith_buffer = 0;
    count_hollerith_constant = atoi(q);

    DELETE((void*)q);

    if (count_hollerith_constant == 0)
    {
        error_printf_at(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "ignoring invalid Hollerith constant of length 0\n");
    }
    else
    {
        BEGIN(hollerith_constant);
    }
}

<hollerith_constant>{

. {
    hollerith_buffer[length_hollerith_buffer] = *yytext;
    length_hollerith_buffer++;
    count_hollerith_constant--;

    if (count_hollerith_constant == 0)
    {
        hollerith_buffer[length_hollerith_buffer] = '\0';

        BEGIN(INITIAL);
        parse_token_text_str(hollerith_buffer);
        return TOKEN_HOLLERITH_CONSTANT;
    }
}

}

({kind}_)?["]([^"\n\r]|["]["])*["] {
	parse_token_text();
	RETURN_TOKEN(CHAR_LITERAL);
}

({kind}_)?[']([^'\n\r]|[']['])*['] {
	parse_token_text();
	RETURN_TOKEN(CHAR_LITERAL);
}

[0-9]+ {
    // Handle here nonblock
    if (last_eos 
            && nonblock_do_stack_idx > 0)
    {
        const char * q = yytext;

        int label = atoi(q);

        if (nonblock_do_stack[nonblock_do_stack_idx - 1] == label)
        {
            nonblock_do_stack_idx--;
            // Now unput everything again so we will get a DECIMAL_LITERAL later
            yyless(0);
            RETURN_TOKEN(TOKEN_END_NONBLOCK_DO);
        }
    }
    parse_token_text();
    RETURN_TOKEN(DECIMAL_LITERAL);
}

[0-9]+_{kind} {
	parse_token_text();
	RETURN_TOKEN(DECIMAL_LITERAL);
}

format {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FORMAT);
}

generic {
    parse_token_text();
    RETURN_TOKEN(TOKEN_GENERIC);
}

volatile {
    parse_token_text();
    RETURN_TOKEN(TOKEN_VOLATILE);
}

equivalence {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EQUIVALENCE);
}

eor {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EOR);
}

protected {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PROTECTED);
}

pad {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PAD);
}

file {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FILE);
}

unformatted {
    parse_token_text();
    RETURN_TOKEN(TOKEN_UNFORMATTED);
}

named {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NAMED);
}

subroutine {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SUBROUTINE);
}

opened {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OPENED);
}

delim {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DELIM);
}

source {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SOURCE);
}

critical {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CRITICAL);
}

action {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ACTION);
}

import {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IMPORT);
}

block {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BLOCK);
}

enum {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENUM);
}

overridable {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OVERRIDABLE);
}

module {
    parse_token_text();
    RETURN_TOKEN(TOKEN_MODULE);
}

sign {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SIGN);
}

extends {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EXTENDS);
}

pass {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PASS);
}

go {
    parse_token_text();
    RETURN_TOKEN(TOKEN_GO);
}

entry {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENTRY);
}

where {
    parse_token_text();
    RETURN_TOKEN(TOKEN_WHERE);
}

endwhere {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDWHERE);
}

procedure {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PROCEDURE);
}

name {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NAME);
}

double {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DOUBLE);
}

contains {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CONTAINS);
}

logical {
    parse_token_text();
    RETURN_TOKEN(TOKEN_LOGICAL);
}

newunit {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NEWUNIT);
}

nullify {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NULLIFY);
}

deferred {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DEFERRED);
}

only {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ONLY);
}

exist {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EXIST);
}

interface {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INTERFACE);
}

save {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SAVE);
}

advance {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ADVANCE);
}

stat {
    parse_token_text();
    RETURN_TOKEN(TOKEN_STAT);
}

return {
    parse_token_text();
    RETURN_TOKEN(TOKEN_RETURN);
}

readwrite {
    parse_token_text();
    RETURN_TOKEN(TOKEN_READWRITE);
}

assign {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ASSIGN);
}

assignment {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ASSIGNMENT);
}

print {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PRINT);
}

iostat {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IOSTAT);
}

sync {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SYNC);
}

complex {
    parse_token_text();
    RETURN_TOKEN(TOKEN_COMPLEX);
}

asynchronous {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ASYNCHRONOUS);
}

images {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IMAGES);
}

end {
    parse_token_text();
    RETURN_TOKEN(TOKEN_END);
}

final {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FINAL);
}

inout {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INOUT);
}

nextrec {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NEXTREC);
}

kind {
    parse_token_text();
    RETURN_TOKEN(TOKEN_KIND);
}

common {
    parse_token_text();
    RETURN_TOKEN(TOKEN_COMMON);
}

blank {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BLANK);
}

iolength {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IOLENGTH);
}

integer {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INTEGER);
}

deallocate {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DEALLOCATE);
}

function {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FUNCTION);
}

c {
    parse_token_text();
    RETURN_TOKEN(TOKEN_C);
}

enumerator {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENUMERATOR);
}

recursive {
    parse_token_text();
    RETURN_TOKEN(TOKEN_RECURSIVE);
}

sequence {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SEQUENCE);
}

inquire {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INQUIRE);
}

flush {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FLUSH);
}

else {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ELSE);
}

intent {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INTENT);
}

external {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EXTERNAL);
}

operator {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OPERATOR);
}

optional {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OPTIONAL);
}

unit {
    parse_token_text();
    RETURN_TOKEN(TOKEN_UNIT);
}

size {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SIZE);
}

nopass {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NOPASS);
}

type {
    parse_token_text();
    RETURN_TOKEN(TOKEN_TYPE);
}

mold {
    parse_token_text();
    RETURN_TOKEN(TOKEN_MOLD);
}

precision {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PRECISION);
}

pending {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PENDING);
}

continue {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CONTINUE);
}

result {
    parse_token_text();
    RETURN_TOKEN(TOKEN_RESULT);
}

real {
    parse_token_text();
    RETURN_TOKEN(TOKEN_REAL);
}

then {
    parse_token_text();
    RETURN_TOKEN(TOKEN_THEN);
}

stream {
    parse_token_text();
    RETURN_TOKEN(TOKEN_STREAM);
}

do {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DO);
}

do/[[:blank:]]+[0-9]+ {
    parse_token_text();

    BEGIN(NONBLOCK_DO);
    RETURN_TOKEN(TOKEN_DO);
}

<NONBLOCK_DO>{
    
[:blank:]+ { 
}

[0-9]+ {
    parse_token_text();
    int label = atoi(yytext);

    if (nonblock_do_stack_idx > 0
            && nonblock_do_stack[nonblock_do_stack_idx - 1] == label)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "%s:%d: debug: shared do label '%d'\n", 
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number, 
                    label);
        }
        BEGIN(INITIAL);
        RETURN_TOKEN(TOKEN_SHARED_LABEL);
    }
    else
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "%s:%d: debug: new do label '%d'\n", 
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number, 
                    label);
        }
        ERROR_CONDITION(nonblock_do_stack_idx == MAX_NONBLOCK_DO_STACK, 
                "Too many non block do statements %d\n", MAX_NONBLOCK_DO_STACK);
        nonblock_do_stack[nonblock_do_stack_idx] = label;
        nonblock_do_stack_idx++;
    }

    BEGIN(INITIAL);
    RETURN_TOKEN(DECIMAL_LITERAL);
}

}

default {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DEFAULT);
}

contiguous {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CONTIGUOUS);
}

stop {
    parse_token_text();
    RETURN_TOKEN(TOKEN_STOP);
}

while {
    parse_token_text();
    RETURN_TOKEN(TOKEN_WHILE);
}

program {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PROGRAM);
}

rec {
    parse_token_text();
    RETURN_TOKEN(TOKEN_REC);
}

open {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OPEN);
}

case {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CASE);
}

recl {
    parse_token_text();
    RETURN_TOKEN(TOKEN_RECL);
}

dimension {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DIMENSION);
}

elemental {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ELEMENTAL);
}

forall {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FORALL);
}

position {
    parse_token_text();
    RETURN_TOKEN(TOKEN_POSITION);
}

fmt {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FMT);
}

read {
    parse_token_text();
    RETURN_TOKEN(TOKEN_READ);
}

out {
    parse_token_text();
    RETURN_TOKEN(TOKEN_OUT);
}

lock {
    parse_token_text();
    RETURN_TOKEN(TOKEN_LOCK);
}

decimal {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DECIMAL);
}

select {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SELECT);
}

selectcase {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SELECTCASE);
}

direct {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DIRECT);
}

write {
    parse_token_text();
    RETURN_TOKEN(TOKEN_WRITE);
}

codimension {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CODIMENSION);
}

close {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CLOSE);
}

id {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ID);
}

wait {
    parse_token_text();
    RETURN_TOKEN(TOKEN_WAIT);
}

all {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ALL);
}

elsewhere {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ELSEWHERE);
}

len {
    parse_token_text();
    RETURN_TOKEN(TOKEN_LEN);
}

impure {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IMPURE);
}

exit {
    parse_token_text();
    RETURN_TOKEN(TOKEN_EXIT);
}

rewind {
    parse_token_text();
    RETURN_TOKEN(TOKEN_REWIND);
}

backspace {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BACKSPACE);
}

nml {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NML);
}

intrinsic {
    parse_token_text();
    RETURN_TOKEN(TOKEN_INTRINSIC);
}

public {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PUBLIC);
}

errmsg {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ERRMSG);
}

namelist {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NAMELIST);
}

use {
    parse_token_text();
    RETURN_TOKEN(TOKEN_USE);
}

submodule {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SUBMODULE);
}

err {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ERR);
}

encoding {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENCODING);
}

acquired {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ACQUIRED);
}

abstract {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ABSTRACT);
}

character {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CHARACTER);
}

number {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NUMBER);
}

concurrent {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CONCURRENT);
}

convert {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CONVERT);
}

buffered {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BUFFERED);
}

endfile {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDFILE);
}

parameter {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PARAMETER);
}

data {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DATA);
}

pos {
    parse_token_text();
    RETURN_TOKEN(TOKEN_POS);
}

private {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PRIVATE);
}

round {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ROUND);
}

to {
    parse_token_text();
    RETURN_TOKEN(TOKEN_TO);
}

sequential {
    parse_token_text();
    RETURN_TOKEN(TOKEN_SEQUENTIAL);
}

allocate {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ALLOCATE);
}

target {
    parse_token_text();
    RETURN_TOKEN(TOKEN_TARGET);
}

class {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CLASS);
}

cycle {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CYCLE);
}

status {
    parse_token_text();
    RETURN_TOKEN(TOKEN_STATUS);
}

iomsg {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IOMSG);
}

form {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FORM);
}

is {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IS);
}

none {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NONE);
}

value {
    parse_token_text();
    RETURN_TOKEN(TOKEN_VALUE);
}

unlock {
    parse_token_text();
    RETURN_TOKEN(TOKEN_UNLOCK);
}

formatted {
    parse_token_text();
    RETURN_TOKEN(TOKEN_FORMATTED);
}

in {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IN);
}

implicit {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IMPLICIT);
}

if {
    parse_token_text();
    RETURN_TOKEN(TOKEN_IF);
}

associate {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ASSOCIATE);
}

bind {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BIND);
}

allocatable {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ALLOCATABLE);
}

access {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ACCESS);
}

call {
    parse_token_text();
    RETURN_TOKEN(TOKEN_CALL);
}

pure {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PURE);
}

memory {
    parse_token_text();
    RETURN_TOKEN(TOKEN_MEMORY);
}

pointer {
    parse_token_text();
    RETURN_TOKEN(TOKEN_POINTER);
}

non_overridable {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NON_OVERRIDABLE);
}

non_intrinsic {
    parse_token_text();
    RETURN_TOKEN(TOKEN_NON_INTRINSIC);
}

vector {
    parse_token_text();
    RETURN_TOKEN(TOKEN_VECTOR);
}

pause {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PAUSE);
}

pixel {
    parse_token_text();
    RETURN_TOKEN(TOKEN_PIXEL);
}

 /* Combined versions */
allstop {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ALLSTOP);
}

endif {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDIF);
}

endfunction {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDFUNCTION);
}

enddo {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDDO);
}

blockdata  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_BLOCKDATA);
}

endmodule {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDMODULE);
}

doubleprecision  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DOUBLEPRECISION);
}

doublecomplex  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_DOUBLECOMPLEX);
}

endinterface {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDINTERFACE);
}

elseif  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ELSEIF );
}

endprocedure {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDPROCEDURE);
}

elsewhere  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ELSEWHERE);
}

endprogram {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDPROGRAM);
}

endassociate  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDASSOCIATE);
}

endselect {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDSELECT);
}

endblock  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDBLOCK);
}

endsubmodule {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDSUBMODULE);
}

endblockdata  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDBLOCKDATA);
}

endsubroutine {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDSUBROUTINE);
}

endcritical  {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDCRITICAL);
}

endtype {
    parse_token_text();
    RETURN_TOKEN(TOKEN_ENDTYPE);
}

goto {
    parse_token_text();
    RETURN_TOKEN(TOKEN_GOTO);
}

 /* Non keyword tokens */

{identifier} {
	parse_token_text();
	RETURN_TOKEN(IDENTIFIER);
}

"**" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_RAISE);
}

"*" {
	parse_token_text();
	RETURN_TOKEN('*');
}


"/" {
	parse_token_text();
	RETURN_TOKEN('/');
}

"//" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_DOUBLE_SLASH);
}

"(" {
	parse_token_text();
 	RETURN_TOKEN('(');
 }

")" {
	parse_token_text();
 	RETURN_TOKEN(')');
}

"[" {
	parse_token_text();
 	RETURN_TOKEN('[');
 }

"]" {
	parse_token_text();
 	RETURN_TOKEN(']');
}

"(/" { 
    parse_token_text();
    RETURN_TOKEN(TOKEN_LPARENT_SLASH); 
}

"/)" { 
    parse_token_text();
    RETURN_TOKEN(TOKEN_SLASH_RPARENT); 
}

"==" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_EQUAL);
}

".eq." {
	parse_token_text_str("==");
	RETURN_TOKEN(TOKEN_EQUAL);
}

".eqv." {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOGICAL_EQUIVALENT);
}

"/=" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_NOT_EQUAL);
}

".neqv." {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOGICAL_NOT_EQUIVALENT);
}

".ne." {
	parse_token_text_str("/=");
	RETURN_TOKEN(TOKEN_NOT_EQUAL);
}

".lt." {
	parse_token_text_str("<");
	RETURN_TOKEN(TOKEN_LOWER_THAN);
}

"<" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOWER_THAN);
}

".le." {
	parse_token_text_str("<=");
	RETURN_TOKEN(TOKEN_LOWER_OR_EQUAL_THAN);
}

"<=" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOWER_OR_EQUAL_THAN);
}

".gt." {
	parse_token_text_str(">");
	RETURN_TOKEN(TOKEN_GREATER_THAN);
}

">" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_GREATER_THAN);
}

".ge." {
	parse_token_text_str(">=");
	RETURN_TOKEN(TOKEN_GREATER_OR_EQUAL_THAN);
}

">=" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_GREATER_OR_EQUAL_THAN);
}

".and." {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOGICAL_AND);
}

".or." {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOGICAL_OR);
}

".not." {
	parse_token_text();
	RETURN_TOKEN(TOKEN_LOGICAL_NOT);
}

".true."(_{kind})? {
	parse_token_text();
	RETURN_TOKEN(TOKEN_TRUE);
}

".false."(_{kind})? {
	parse_token_text();
	RETURN_TOKEN(TOKEN_FALSE);
}

"," {
	parse_token_text();
	RETURN_TOKEN(',');
}

"=>" {
	parse_token_text();
	RETURN_TOKEN(TOKEN_POINTER_ACCESS);
}

"=" {
	parse_token_text();
	RETURN_TOKEN('=');
}

":" {
	parse_token_text();
	RETURN_TOKEN(':');
}

"+" {
	parse_token_text();
	RETURN_TOKEN('+');
}

"-" {
	parse_token_text();
	RETURN_TOKEN('-');
}

"%" {
	parse_token_text();
	RETURN_TOKEN('%');
}

"{" {
	parse_token_text();
	RETURN_TOKEN('{');
}

"}" {
	parse_token_text();
	RETURN_TOKEN('}');
}

\.[a-z]+\. {
	parse_token_text();
	RETURN_TOKEN(USER_DEFINED_OPERATOR);
}

<*>[[:blank:]] {
	// A blank should be uninteresting here
}

<*>. {
	if (isprint(yytext[0]))
	{
        error_printf_at(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "unexpected character: `%c' (0x%X)\n", 
                yytext[0], yytext[0]);
	}
	else
	{
        error_printf_at(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "unexpected character: 0x%X\n\n", 
                yytext[0]);
	}
  }

%%

// Some flex implementations undef too many things here
#ifndef yytext_ptr
	#define yytext_ptr yytext
#endif

#define FLEX_LVAL mf03lval
#define FLEX_LLOC mf03lloc

static void parse_token_text(void)
{
    parse_token_text_str(yytext);
}

UNUSED_PARAMETER static void parse_token_text_str(const char* c)
{
    FLEX_LVAL.token_atrib.token_text = uniquestr(c);

    FLEX_LLOC.first_filename = uniquestr(fortran_scanning_now->current_filename);
    FLEX_LLOC.first_line = fortran_scanning_now->line_number;
    FLEX_LLOC.first_column = fortran_scanning_now->column_number;
}

static void reintroduce_continued_line(char* continued_line,
        enum continuation_kind_tag continuation_kind,
        const char* sentinel)
{
    char* temporal = NULL;
    unsigned int new_line_number = 0;
    unsigned int num_joined_lines = 1;
    const char* filename = NULL;

    if (sentinel == NULL)
    {
        temporal = xstrdup(continued_line);
    }
    else
    {
        // We need to skip the two sentinels and create a new string without them
        temporal = NEW_VEC0(char, strlen(continued_line) + 1);
        char *q = temporal;
        const char *p = continued_line;

        // Jump blanks
        while (*p == ' ' || *p == '\t') p++;

        ERROR_CONDITION(*p != '!', "Invalid continued line with sentinel '%s'\n", sentinel);
        // Now ignore the sentinel of the first line until a blank
        while (*p != ' ' && *p != '\t') p++;

        while (*p != '\n'
                && *p != '\r')
        {
            // Copy all until the \n (or \r)
            *q = *p;
            q++; p++;
        }

        if (*p == '\r'
                && *(p+1) == '\n') // DOS CR-NL
        {
            // Ignore \r
            p++;
        }

        // And the newline as well
        *q = *p;
        q++; p++;

        // Ignore comments and empty lines
        const char* second_line = get_real_second_line(p, &filename, &new_line_number, &num_joined_lines, /* ignore_comments */ true);
        p = second_line;

        // Jump blanks again
        while (*p == ' ' || *p == '\t') p++;

        ERROR_CONDITION(*p != '!', "Invalid continued line with sentinel '%s'\n", sentinel);
        // Now ignore the sentinel of the second line (until a blank or &)
        while (*p != ' ' && *p != '\t' && *p != '&') p++;
        while (*p != '\0')
        {
            // Copy all until the end of the string
            *q = *p;
            q++; p++;
        }
        // And the end of string as well
        *q = *p;
    }

    // Copy all the string springing over &\n&

    // Find the first newline
    char* newline = strchr(temporal, '\n');

    ERROR_CONDITION(newline == NULL, "There must be a new-line\n", 0);

    char* limit_first_line = NULL; 
    const char* limit_second_line = NULL;
        
    // We will look for the rightmost & and the leftmost &
    // so we cut here
    *newline = '\0';

    // First line
    char * first_line = temporal;
    // We remove any final comment in the first line
    trim_inline_comment(first_line);
    // Now we can safely look for rightmost "&"
    limit_first_line = strrchr(first_line, '&'); 
    
    // Second line
    // The (potential) second line starts here
    // Ignore comments and empty lines
    const char* second_line = get_real_second_line(newline + 1, 
            &filename, &new_line_number, &num_joined_lines , /* ignore_comments */ false);

    if (continuation_kind == DOUBLE_CONTINUATION)
    {
        // Look for the leftmost "&"
        limit_second_line = strchr(second_line, '&');
        // We are not interested in "&"
        limit_second_line++;
    }
    else
    {
        limit_second_line = second_line;
    }

    if (limit_first_line && limit_second_line)
    {
        // Let's reintroduce all the whole line
        *limit_first_line = '\0';

        const char* last_character = limit_second_line + strlen(limit_second_line);

        // We cannot do this in an empty string
        if (last_character != limit_second_line) 
        {
            // Introducing the second line
            last_character--; // Spring over the NULL
            while (last_character >= limit_second_line)
            {
                unput(*last_character);
                last_character--;
            }

            if (continuation_kind == SINGLE_CONTINUATION)
            {
                // Add an extra blank to separate tokens because this
                // continuation does not end with an open string literal
                unput(' ');
            }
        }

        // Introduce first line
        last_character = limit_first_line;
        last_character--; // Spring over the NULL
        while (last_character >= temporal)
        {
            unput(*last_character);
            last_character--;
        }
    }

    if (sentinel != NULL)
    {
        // A separating blank
        unput(' ');
        // Reintroduce the original sentinel
        const char* q = &(sentinel[strlen(sentinel) - 1]);

        while (q >= sentinel)
        {
            unput(*q);
            q--;
        }
    }

    // This line was joined, we do not update line_number but the number of joined_lines (at least 1, but may be more)
    fortran_scanning_now->new_line = new_line_number;
    fortran_scanning_now->joined_lines += num_joined_lines;
    DEBUG_CODE() DEBUG_MESSAGE("Incrementing joined_lines (%d)\n", fortran_scanning_now->joined_lines);
    // Free
    DELETE(temporal);
}

static char* get_included_filename(char* include_line, char delim)
{
	char* start;
	char* final;	
	char* result;
	char* i;
	int j;

	// Take the left delimiter
	start = strchr(include_line, delim);
	start++;

	// We would take rightmost delimiter but we do it after an hypothetic
	// comment mark placed rightmost
	if ((final = strchr(include_line, '!')) != NULL)
	{
		// There is a comment after the include, we will look for the delimiter
		// at the left of the comment mark
		for (; *final != delim; final--);
	}
	else
	{
		// There is no comment, it has to be the rightmost delimiter
		final = strrchr(include_line, delim);
	}
	
	result = NEW_VEC0(char, final-start+1);

	// Remove delimiters inside the string
	//     o''callaghan -> o'callaghan
	//     o""callaghan -> o"callaghan

	j = 0;
	for (i = start; i < final; i++)
	{
		if ((*i != delim) || 
		   (i+1 == final) || 
		   (*(i+1) != delim)) 
		{
			result[j] = *i;
			j++;
		}
	}
	result[j] = '\0';
	
	return result;
}

static void open_included_file(char* include_name)
{
	if (include_stack_size >= MAX_INCLUDE_DEPTH)
	{
		fatal_error("Too many levels (>%d) of inclusion at %s:%d", MAX_INCLUDE_DEPTH,
				fortran_scanning_now->filename, fortran_scanning_now->line_number);
	}	

	DEBUG_CODE() DEBUG_MESSAGE("Opening included file %s at %s:%d", include_name, 
			fortran_scanning_now->filename, fortran_scanning_now->line_number);
	const char* include_filename = fortran_find_file_in_directories(
            CURRENT_CONFIGURATION->num_include_dirs,
            CURRENT_CONFIGURATION->include_dirs, 
            include_name,
            /* origin */ fortran_scanning_now->current_filename);
	if (include_filename == NULL)
    {
        fatal_error(
                make_locus(
                    fortran_scanning_now->current_filename, 
                    fortran_scanning_now->line_number,
                    0),
                "cannot open included file '%s'\n", 
                include_name);
    }

	temporal_file_t temporal_include_filename = new_temporal_file();
	if (!temporal_include_filename)
	{
		fatal_error(
                make_locus(
                    fortran_scanning_now->current_filename,
                    fortran_scanning_now->line_number,
                    0),
                "cannot create temporal included file");
	}

	if (copy_file(include_filename, temporal_include_filename->name) != 0)
    {
        fatal_error("Failure during copy of '%s' to '%s'. %s\n", 
                include_filename, 
                temporal_include_filename->name, 
                strerror(errno));
    }
	// This avoids problems with not-EOL ended files

    FILE* included_file = fopen(temporal_include_filename->name, "r+");
    if (included_file == NULL)
    {
		fatal_error("Cannot open for modification temporal included file '%s'. %s\n", 
                temporal_include_filename->name,
                strerror(errno));
    }
    // Go to end and ensure there is '\n'
    fseek(included_file, 0, SEEK_END);
	fputs("\n", included_file);
    fclose(included_file);

    included_file = fopen(temporal_include_filename->name, "r");
    if (included_file == NULL)
    {
		fatal_error("Cannot open for reading temporal included file '%s'. %s\n", 
                temporal_include_filename->name,
                strerror(errno));
    }

	// Allocate
	struct scan_file_descriptor* new_scanned_file = &include_stack[include_stack_size+1];
    memset(new_scanned_file, 0, sizeof(*new_scanned_file));

	// Initialize a new scanning element
	new_scanned_file->filename = uniquestr(include_filename);
	new_scanned_file->line_number = 1;
	new_scanned_file->current_filename = uniquestr(include_filename);
	new_scanned_file->scanning_buffer = yy_create_buffer(included_file, YY_BUF_SIZE);
	new_scanned_file->file_descriptor = included_file;
	new_scanned_file->joined_lines = 0;

	// Update the stack
	include_stack_size++;
	fortran_scanning_now = &include_stack[include_stack_size];

	// Switch the buffer of flex
	yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);
    yy_set_bol(1);
    ERROR_CONDITION(!YY_AT_BOL(), "Invalid bol state!\n", 0);
    last_eos = 1;
}

static char end_of_file(void)
{
	// Are we in the last file?
	if (include_stack_size == 0)
	{
		return 1;
	}
	else
	{
		DEBUG_CODE() DEBUG_MESSAGE("End of included file %s switching back to %s", 
				fortran_scanning_now->filename, include_stack[include_stack_size-1].filename);

		yy_delete_buffer(fortran_scanning_now->scanning_buffer);
        fclose(fortran_scanning_now->file_descriptor);

		include_stack_size--;
		fortran_scanning_now = &include_stack[include_stack_size];
		yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);
        last_eos = 1;

		// if (include_stack_size == 0)
		// {
		// 	CURRENT_COMPILED_FILE->in_include = 0;
		// }

		return 0;
	}
}

int yywrap()
{
	return end_of_file();
}

int mf03_open_file_for_scanning(const char* scanned_filename, const char* input_filename,
        char is_fixed_form)
{
    ERROR_CONDITION(is_fixed_form, "This scanner does not support fixed form", 0);

	FILE* file;

	file = fopen(scanned_filename, "r");

	if (file == NULL)
	{
		fatal_printf_at(NULL, "cannot open file '%s' (%s)", scanned_filename, strerror(errno));
	}

    fortran_scanning_now = &include_stack[0];
	memset(fortran_scanning_now, 0, sizeof(*fortran_scanning_now));
	fortran_scanning_now->filename = uniquestr(scanned_filename);
	fortran_scanning_now->file_descriptor = file;
	fortran_scanning_now->line_number = 1;
	fortran_scanning_now->column_number = 0; // Not implemented yet

    fortran_scanning_now->current_filename = uniquestr(input_filename);

	fortran_scanning_now->scanning_buffer = yy_create_buffer(file, YY_BUF_SIZE);

	yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);
    yy_set_bol(1);
    ERROR_CONDITION(!YY_AT_BOL(), "Invalid bol state!\n", 0);
    last_eos = 1;

	return 0;
}

static const char* const TL_SOURCE_STRING = "MERCURIUM_INTERNAL_SOURCE";
// maybe this is not the best place to change the configuration but it must work
compilation_configuration_t* string_scanning_configuration = 0;

int mf03_prepare_string_for_scanning(const char* str)
{
	static int num_string = 0;

	DEBUG_CODE()
    {
        fprintf(stderr, "* Going to parse string in Fortran\n");
        fprintf(stderr, "%s\n", str);
        fprintf(stderr, "* End of parsed string\n");
    }
	include_stack_size = 0;
	fortran_scanning_now = &include_stack[include_stack_size];
	fortran_scanning_now->line_number = 1;
	fortran_scanning_now->column_number = 0; // Not implemented yet
	fortran_scanning_now->new_line = 0;
	fortran_scanning_now->joined_lines = 0;
	
	const char* current_filename = CURRENT_COMPILED_FILE->input_filename;
	fortran_scanning_now->filename = NEW_VEC0(char, strlen(TL_SOURCE_STRING) + strlen(current_filename) + 10);
    char filename[256];
	snprintf(filename, 255, "%s-%s-%d", TL_SOURCE_STRING, current_filename, num_string);
    filename[255] = '\0';
    fortran_scanning_now->filename = uniquestr(filename);
	
	// Change the configuration for scanning strings:
	if (!string_scanning_configuration)
    {
		string_scanning_configuration = NEW(compilation_configuration_t);
		*string_scanning_configuration = *(compilation_process.current_compilation_configuration);
	}
	
	num_string++;
	fortran_scanning_now->scanning_buffer = yy_scan_string(str);
	yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);
    last_eos = 1;

	return 0;
}

static void trim_inline_comment(char* line)
{
	// First we scan left to right to see if trimmed_line has left a constant string opened
	// and we trim inline comments in trimmed_line
	char in_string = 0, delim_string = '\0';
	char* p;

	for (p = line; *p != 0; p++)
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
			*p = 0;
			// Dirty hack to stop the loop in the next iteration
			*(p + 1) = 0;
		}
		else if (*p == delim_string)
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
}

static char check_continued_sentinel_line(const char* line, const char** sentinel,
        enum continuation_kind_tag continuation_kind)
{
    *sentinel = NULL;

    // Get the first sentinel
    char sentinel1[33] = { 0 };
    char *q = sentinel1;
    // Find the sentinel of the first line (aka sentinel1)
    const char* p = line;

    while (*p == ' ' || *p == '\t')
        p++;
    ERROR_CONDITION(*p != '!', "Invalid regex (1)\n", 0);

    while (*p != ' ' && *p != '\t')
    {
        *q = *p;
        p++; q++;
    }

    unsigned int dummy_num_joined_lines = 0;
    unsigned int dummy_new_line_number = 0;
    const char* dummy_filename = 0;
    char* newline = strchr(yytext, '\n');
    // Ignore comments and empty lines
    const char* second_line = get_real_second_line(newline + 1, 
            &dummy_filename, &dummy_new_line_number, &dummy_num_joined_lines, /* ignore_comments */ true);

    // Get the second sentinel
    char sentinel2[33] = { 0 };
    p = second_line;
    ERROR_CONDITION(p == NULL, "Invalid regex (2)\n", 0);

    q = sentinel2;

    while (*p == ' ' || *p == '\t')
        p++;
    ERROR_CONDITION(*p != '!', "Invalid regex (3)\n", 0);

    while (*p != ' '
            && *p != '\t'
            && (continuation_kind != DOUBLE_CONTINUATION
                || *p != '&'))
    {
        *q = *p;
        p++; q++;
    }

    *sentinel = uniquestr(sentinel1);
    if (strcmp(sentinel1, sentinel2) != 0)
    {
        DEBUG_CODE()
        {
            fprintf(stderr, "Sentinel mismatch: '%s' != '%s'\n", sentinel1, sentinel2);
        }
        return 0;
    }

    return 1;
}

static char is_valid_pragma_custom_prefix(const char* sentinel)
{
    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcasecmp(sentinel, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
        {
            return 1;
        }
    }
    return 0;
}

static int is_format_statement(char* token)
{
	int level;
	char delim, in_string;
	char* p = token;

	// We have to check that this is really a FORMAT statement

	// Skip label
	while (isdigit(*p)) p++;

	// Skip blanks after label
	while (*p == ' ' || *p == '\t') p++;

	// Skip "format"
	p += strlen("format");

	// Here we are no more protected by the regex so we have to deal
	// with anything. Fortunately the token ends with ';' or '\n'

	if (*p == ';' || *p == '\n' || *p == '\r') 
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format ends too early", "");
		return 0;
	}

	// Advance possible blanks
	while (*p == ' ' || *p == '\t') p++;


	// We want '('
	if (*p != '(')
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format lacks of '('", "");
		return 0;
	}

	// We have already read '('
	p++;
	level = 1;
	in_string = 0;

	// Now find the matching )
	while ((*p != '\0') && (level > 0))
	{
		if (!in_string)
		{
			if (*p == '(')
			{
				level++;
			}
			else if (*p == ')')
			{
				level--;
			}
			else if (*p == '\'' || *p == '"')
			{
				delim = *p;
				in_string = 1;
			}
		}
		else
		{
			if (*p == delim)
			{
				if (*(p+1) != delim)
				{
					in_string = 0;
				}
				else // *(p+1) == delim
				{
					// Skip the delimiter as we do not want
					// to see again
					p++;
				}
			}
		}
		p++;
	}

	// Unbalanced parentheses or opened string
	if ((level > 0) || (in_string == 1)) 
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format is unbalanced or has an opened string (level = %d, in_string = %d)", level, in_string);
		return 0;
	}

	// Only blanks till the end of the line
	while (*p == ' ' || *p == '\t') p++;

	if (*p == '!')
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format has a comment, we trim it", "");
		while (*p != '\0')
		{
			*p = ' ';
			p++;
		}
		*(p-1) = '\n';
		return 1;
	}

    if (*p != ';'
            && *p != '\n'
            // DOS CR-NL
            && !(*p == '\r'
                && *(p+1) == '\n'))
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format statement does not end with an EOS (*p = '%c')", *p);
		return 0;
	}

	if (*p == ';'
            || *p == '\r') *p = '\n';

	return 1;
}

static int compute_length_match(const char* lexed_directive,
        const char* available_directive,
        const char **discard_source)
{
    *discard_source = NULL;
    const char *p = lexed_directive;
    const char *q = available_directive;

    while (*p != '\0'
            && *q != '\0')
    {
        if (*q == '|')
        {
            // A '|' should match like [[:blank:]]* 
            while (*p == ' ' || *p == '\t')
                p++;
            // We advanced too much
            p--;
        }
        else if (*q != tolower(*p))
        {
            return 0;
        }
        q++;
        p++;
    }

    *discard_source = p;
    return (q - available_directive);
}

static const char* return_pragma_prefix_longest_match_inner(pragma_directive_set_t* pragma_directive_set,
        const char* lexed_directive,
        const char **discard_source,
        pragma_directive_kind_t* directive_kind)
{
    const char* longest_match_so_far = NULL;
    int length_match = 0;

    int j;
    char exact_match = 0;
    int size_lexed_directive = strlen(lexed_directive);
    for (j = 0; j < pragma_directive_set->num_directives && !exact_match; j++)
    {
        const char * current_discard_source = NULL;

        int current_match = compute_length_match(lexed_directive, pragma_directive_set->directive_names[j], 
                &current_discard_source);
        
        if (current_match >= length_match && current_match != 0)
        {
            int size_directive = strlen(pragma_directive_set->directive_names[j]);
            if (current_match == size_lexed_directive && size_lexed_directive == size_directive)
            {
               exact_match = 1;    
            }
            length_match = current_match;
            longest_match_so_far = pragma_directive_set->directive_names[j];
            *discard_source = current_discard_source;
            *directive_kind = pragma_directive_set->directive_kinds[j];
        }
    }

    return longest_match_so_far;
}

static const char* return_pragma_prefix_longest_match(const char* prefix, 
        const char* lexed_directive,
        pragma_directive_kind_t* kind,
        char *original_directive)
{
    const char* longest_match = NULL;
    const char* discard_source = NULL;

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcmp(CURRENT_CONFIGURATION->pragma_custom_prefix[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION->pragma_custom_prefix_info[i];
            longest_match = return_pragma_prefix_longest_match_inner(pragma_directive_set, 
                    lexed_directive, &discard_source, kind);
        }
    }

    const char *p = lexed_directive;
    char *q = original_directive;

    while (p != discard_source
            && *p != '\0')
    {
        *q = *p;
        q++;
        p++;
    }
    *q = '\0';

    if (discard_source != NULL
            && strlen(discard_source) > 0)
    {
        // Discard the characters "too much lexed"
        const char* end = discard_source;

        while (*end != '\0')
            end++;

        end--;

        while (end != discard_source)
        {
            unput(*end);
            end--;
        }
    }

    return longest_match;
}

static const char* format_pragma_string(const char* c)
{
    char *tmp = xstrdup(c);

    char* p = tmp;

    while (*p != '\0')
    {
        if (*p == '|')
            *p = ' ';
        p++;
    }

    const char* res = uniquestr(tmp);
    DELETE(tmp);

    return res;
}

static const char* get_real_second_line(const char* second_line, 
        const char** filename, 
        unsigned int* new_line, 
        unsigned int* num_joined_lines, 
        bool ignore_comments)
{
    char not_real_second_line = 1;
    while (not_real_second_line)
    {
        // Let's assume for now it is the real second line we want
        not_real_second_line = 0;
        // Now advance blanks
        while (*second_line == ' '
                || *second_line == '\t')
        {
            second_line++;
        }

        // It is a comment, just ignore up to the newline
        if (*second_line == '!'
                // Note that a comment cannot be followed by a $ here
                && (!ignore_comments
                    || *(second_line + 1) != '$'))
        {
            while (*second_line != '\0')
            {
                if (*second_line == '\n'
                        || (*second_line == '\r' // DOS CR-NL
                            && *(second_line + 1) == '\n'))
                {
                    if (*second_line == '\r')
                        second_line++;

                    break;
                }
                second_line++;
            }
        }
        else if (*second_line == '#')
        {
            line_marker_flags_t flags;
            parse_line_marker(second_line, filename, new_line, &flags);
            
            *num_joined_lines = 0;

            while (*second_line != '\0'
                    && *second_line != '\n')
            {
                second_line++;
            }
        }

        if (*second_line == '\n')
        {
            // If we advanced the second line up to an newline it means
            // that it was empty. This is not the real second line we wanted
            not_real_second_line = 1;

            // Inore '\n'
            second_line++;
            
            // Account skipped lines
            *num_joined_lines += 1;
        }
    }
    return second_line;
}

static void parse_line_marker(const char* str, const char** filename, unsigned int *line, line_marker_flags_t *flags)
{
    ERROR_CONDITION(flags == NULL, "Invalid parameter", 0);
    memset(flags, 0, sizeof(*flags));

// We allow the following syntax
// ^([ ]*#)?([ ]*line)?[ ]+[0-9]+[ ]*("[^"]*"[ ]+([1234][ ]+)*$
    const char* p = str;

    // Skip blanks
    while (*p == ' ' || *p == '\t') p++;

    // Skip *p
    if (*p == '#')
        p++;

    // Skip blanks
    while (*p == ' ' || *p == '\t') p++;

    // Skip line
    if (*p == 'l')
    {
        p += strlen("line");
    }

    // Skip blanks
    while (*p == ' ' || *p == '\t') p++;

    // Parse line
    *line = 0;

    if (!('0' <= *p
                && *p <= '9'))
        internal_error("Invalid line marker '%s'. Expecting a digit but got '%c'", str, *p);

    while ('0' <= *p
                && *p <= '9')
    {
        *line = *line*10 + (*p - '0');
        p++;
    }
    
    // There are no zero lines, so if for some reason we end with a 0 line, fix it to 1
    *line = (*line == 0) ? 1 : *line;
    
    // Skip blanks
    while (*p == ' ' || *p == '\t') p++;

    if (*p == '"')
    {
        int length = 64;
        char *c = NEW_VEC(char, length);
        char *last_char = &c[length - 1];
        *last_char = '\0';

        char *q = c;

        // Skip "
        p++;

        while (*p != '"'
                && *p != '\0')
        {
            if (q >= last_char)
            {
                // Give up if the path is unrealistically long
                if (length > 4096)
                    break;

                // Too long, realloc
                int old_length = length;
                length *= 2;
                c = NEW_REALLOC(char, c, length);
                last_char = &c[length - 1];
                *last_char = '\0';
                q = &c[old_length - 1];
            }

            *q = *p;
            q++;
            p++;
        }
        if (*p != '"')
        {
            internal_error("Invalid line marker '%s'. Expecting a \"", str);
        }
        p++;

        // Note that q >= last_char is fine if q == last_char since it would be *last_char = '\0'
        ERROR_CONDITION(q > last_char, "Overflow", 0);
        // Finish the temporary string
        *q = '\0';

        *filename = uniquestr(c);
        DELETE(c);

        // Skip blanks
        while (*p == ' ' || *p == '\t') p++;

        if (*p != '\0' 
                && *p != '\n')
        {
            if (!('1' <= *p
                        && *p <= '4'))
            {
                internal_error("Invalid line marker '%s'. Expecting a flag", str);
            }

            switch (*p)
            {
                case '1':
                    {
                        flags->start_of_new_file = 1;
                        break;
                    }
                case '2':
                    {
                        flags->return_of_a_file = 2;
                        break;
                    }
                case '3':
                    {
                        flags->system_header_file = 1;
                        break;
                    }
                case '4':
                    {
                        flags->extern_c_block = 1;
                        break;
                    }
                default:
                    {
                        internal_error("Code unreachable", 0);
                    }
            }

            p++;
            if ( *p != ' '
                    && *p != '\t'
                    && *p != '\n'
                    && *p != '\0')
            {
                internal_error("Invalid line marker '%s'. Expecting a blank", str);
            }

            // Skip blanks
            while (*p == ' ' || *p == '\t') p++;
        }
    }
    else if (*p != '\0' 
            && *p != '\n')
    {
        internal_error("Junk at the end of a line marker '%s'. Junk -> '%s'", str, p);
    }
}

void * mf03alloc (size_t bytes)
{
    return xmalloc(bytes);
}
void * mf03realloc (void * ptr, size_t bytes)
{
    return xrealloc(ptr, bytes);
}
void   mf03free (void * ptr)
{
    DELETE(ptr);
}
