%{
/**
  Flex scanner of mf03

  $Id: fortran95.l,v 1.12 2008/01/09 09:26:20 luism Exp $
*/
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "cxx-ast.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"
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

static struct scan_file_descriptor include_stack[MAX_INCLUDE_DEPTH];
struct scan_file_descriptor* fortran_scanning_now = &include_stack[0];

static char* get_included_filename(char* linia_include, char delim);
static void open_included_file(char* include_filename);
static void reintroduce_continued_line(char* linia_tallada, char doble_continuador);
#if 0
static void reintroduce_continued_omp_line(char* continued_line, const char* sentinel);
static void reintroduce_continued_coco_omp_line(char* continued_line);
#endif
static char end_of_file(void);
static void trim_inline_comment(char* line);

static int is_format_statement(char* token);

static char last_eos = 1;

char* actual_line = NULL;

#define RETURN_TOKEN(x) do { last_eos = 0; return (x); }  while(0)
#define RETURN_EOS { if (!last_eos) { last_eos = 1; fortran_scanning_now->joined_lines = 0; return (EOS); } }

#define DOUBLE_CONTINUATION  1
#define SINGLE_CONTINUATION  0

%}

INCLUDE_CONSTANT_STRING_A (([0-9]+)_)?[']([^'\n]|(['][']))+[']
INCLUDE_CONSTANT_STRING_B (([0-9]+)_)?["]([^"\n]|(["]["]))+["]

QUOTE_STRING ([']([^'\n]|(['][']))*['])
DOUBLEQUOTE_STRING (["]([^"\n]|(["]["]))*["])

FORTRAN_STRING ({QUOTE_STRING}|{DOUBLEQUOTE_STRING})
UNENDED_QUOTE_STRING       ([']([^'\n]|(['][']))*)
UNENDED_DOUBLEQUOTE_STRING (["]([^"\n]|(["]["]))*)

UNENDED_STRING ({UNENDED_QUOTE_STRING}|{UNENDED_DOUBLEQUOTE_STRING})

hexdigit        [a-fA-F0-9]
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
digit       [0-9]
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
nondigit        ([_a-zA-Z]|uchar)
idnondigit  ({nondigit}|{uchar})

 /* Minimal C stuff */
c_octaldigit      [0-7]
c_hexdigit        [a-fA-F0-9]
c_escape_seq      ((\\["'?eabfnrtv\\])|(\\{c_octaldigit}{1,3})|((\\x)+{c_hexdigit}+))
schar       ([^\\"\n]|{c_escape_seq}|(\\["]))


%x preprocess
%x COMMENT
%x SEEN_FORMAT
%x pragma_line
%x unknown_pragma
%x pragma_custom_directive
%x pragma_custom_clause_first
%x pragma_custom_clause
%s pragma_custom_var_list

%%

"#" { 
    BEGIN(preprocess); 
}

<preprocess>[ ]*line[ ]+[[:digit:]]+([ ]+\"{schar}*\")?[^\n]* {
 const char *p = yytext;

 // Jump initial blanks (if any)
 while (*p == ' ')
    p++;

 // Jump "line" string
 p += strlen("line");

 // Jump blanks
 while (*p == ' ')
   p++;

 // Compute line
 int line_num = 0;
 while (*p != ' ' 
         && *p != '\0' // The filename location is optional
         )
 {
     if (isdigit(*p))
     {
         line_num = line_num*10 + ((*p) - '0');
     }
     else
     {
         internal_error("Digit expected here but '%c' (%x) found", *p, *p);
     }
     p++;
 }

   // Update the line number, note that it is line_num - 1 
   // because \n is not handled here, but in another rule
 fortran_scanning_now->line_number = (line_num - 1);

 // Jump blanks (if any)
 while (*p == ' ')
   p++;

 // If there is a filename, parse it
 if (*p == '"')
 {
     // Now we are on the quote "
     p++;
 
     char filename[256];
     memset(filename, 0, 256);
 
     char *f = filename;
 
     while (*p != '"')
     {
         if (f < &(filename[255]))
         {
             (*f) = *p;
             f++;
         }
         p++;
     }

     // Update the file 
	fortran_scanning_now->current_filename = uniquestr(filename);
 }
}

<preprocess>[ ]{digit}+[ ]+["][^"]+["][^\n]* {
	char* directive = yytext;

	// Jump the blank
	directive++;

	int line_num = 0;
	while (*directive != ' ')
	{
		if (isdigit(*directive))
		{
			line_num = line_num*10 + ((*directive) - '0');
		}
		else
		{
			internal_error("Digit expected here but '%c' found", *directive);
		}
		directive++;
	}

	// Now directive is over the blank after the digits. Jump the blank
	directive++;

	// Now we are over the doublequote. Jump it
	directive++;

	char filename[256];
	memset(filename, 0, 256);

	char* p = filename;

	while (*directive != '"')
	{
        if (p < &(filename[255]))
        {
            *p = *directive;
            p++;
        }
		directive++;
	}

	// Now we have the new number line and the new file name

    // Check include flags from GNU cpp and other preprocessors.
    // We are on the the doublequote, jump it.
    directive++;

    // Advance the directive till the next nonblank
    while (*directive == ' ')
    {
        directive++;
    }

    // Get the flags
    char start_of_new_file = 0;
    char return_of_a_file = 0;
    char system_header_file = 0;
    char extern_c_block = 0;
    while (*directive == ' '
            || isdigit(*directive))
    {
        int current_flag = 0;
        while (isdigit(*directive))
        {
            current_flag = current_flag * 10 +  (*directive - '0');
            directive++;
        }

        switch (current_flag)
        {
            case 1:
                {
                    start_of_new_file = 1;
                    break;
                }
            case 2:
                {
                    return_of_a_file = 1;
                    break;
                }
            case 3:
                {
                    system_header_file = 1;
                    break;
                }
            case 4:
                {
                    extern_c_block = 1;
                    break;
                }
            default:
                // Ignore it
                break;
        }

        // Advance the directive till the next nonblank
        while (*directive == ' ')
        {
            directive++;
        }
    }

    if (system_header_file)
    {
        char path[256];
        strncpy(path, filename, 255);

        const char *name_without_path = give_basename(path);
        strncpy(filename, name_without_path, 255);
    }

    if (start_of_new_file 
            || system_header_file)
    {
        if (include_counter == 0)
        {
            top_level_include_t *new_top_level_include = calloc(1, sizeof(*new_top_level_include));

            new_top_level_include->included_file = uniquestr(filename);

            if (system_header_file)
            {
                new_top_level_include->system_include = 1;
            }

            P_LIST_ADD(CURRENT_COMPILED_FILE->top_level_include_list,
                    CURRENT_COMPILED_FILE->num_top_level_includes,
                    new_top_level_include);
        }
    }

    if (start_of_new_file)
    {
        include_counter++;
    }

    if (return_of_a_file)
    {
        include_counter = (include_counter > 0) ? (include_counter - 1) : 0;
    }

    // Update the line number, note that it is line_num - 1 
    // because \n is not handled here, but in another rule
	fortran_scanning_now->line_number = (line_num - 1);
    // Update file
	fortran_scanning_now->current_filename = uniquestr(filename);
}

<preprocess>.       { /* ignore line */  }
<preprocess>\n  { fortran_scanning_now->line_number++; BEGIN(INITIAL); }

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

^[^\n]*$ {
	// Read all the line we are going to scan
	if (actual_line != NULL) 
	{
		free(actual_line);
	}
	actual_line = strdup(yytext);

	// And reject
	REJECT;
}

^[[:blank:]]*INCLUDE[[:blank:]]*{INCLUDE_CONSTANT_STRING_A}[[:blank:]]*([!][^\n]*)?\n {
	char* filename;

	filename = get_included_filename(yytext, '\'');

	// This must be done before changing the buffer
	fortran_scanning_now->line_number++;

	open_included_file(filename);

	free(filename);
}

^[[:blank:]]*INCLUDE[[:blank:]]*{INCLUDE_CONSTANT_STRING_B}[[:blank:]]*([!][^\n]*)?\n {
	char* filename;

	filename = get_included_filename(yytext, '"');

	// This must be done before changing the buffer
	fortran_scanning_now->line_number++;

	open_included_file(filename);

	free(filename);
}

^(([^\n!'"])|{FORTRAN_STRING})+&[[:blank:]]*(![^\n]*)?\n[[:blank:]]*&[^\n]+\n |
^(([^\n!'"])|{FORTRAN_STRING})*{UNENDED_STRING}&[[:blank:]]*\n[[:blank:]]*&[^\n]+\n {
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

	reintroduce_continued_line(yytext, DOUBLE_CONTINUATION);
	fortran_scanning_now->line_number++;
}

^(([^\n!'"])|{FORTRAN_STRING})+&[[:blank:]]*(![^\n]*)?\n[[:blank:]]*[^&][^\n]*\n |
^(([^\n!'"])|{FORTRAN_STRING})*{UNENDED_STRING}&[[:blank:]]*\n[[:blank:]]*[^&][^\n]*\n {
	// The first regex allows comments because all strings are closed
    // The second one does not allows them because the last string is opened
    // This recognizes continued lines with continuation mark at the end
    //
    // INTEGER &
    // :: A = 3

	// Note that we do not handle the forbidden continuation of END statements
	reintroduce_continued_line(yytext, SINGLE_CONTINUATION);

	fortran_scanning_now->line_number++;

	// At this point all continuated lines should already be joined
}

[0-9]+[[:blank:]]+format([^'"\n;]|([']([^'\n]|(['][']))*['])|(["]([^"\n]|(["]["]))*["]))*[\n;]  {
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
	}

	// We will reject it always in a ugly way of doing preanalysis
	// This reduces efficiency of this scanner though
	REJECT;
}

<SEEN_FORMAT>[(]([^'"\n;]|([']([^'\n]|(['][']))*['])|(["]([^"\n]|(["]["]))*["]))*[)][[:blank:]]*/[;\n] {

	BEGIN(0);
	parse_token_text();
	RETURN_TOKEN(FORMAT_SPEC);
}

[!] {
	BEGIN(COMMENT);
}


<pragma_line>{idnondigit}({idnondigit}|{digit})* {
	// Unknown pragmas
	int i;
	char found = 0;

	char matched = 0;

	if (!matched)
	{
		for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
		{
			if (strcasecmp(yytext, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
			    {
				found = 1;
				break;
			    }
		}

		if (!found)
		{
			fprintf(stderr, "Warning, unknown '!$%s' at line %d\n", 
					yytext,
//					fortran_scanning_now->current_filename,
					fortran_scanning_now->line_number);
			BEGIN(unknown_pragma);
			// Ignore this prefix as an unknown pragma
			yymore();
			matched = 1;
		}
		else
		{
			BEGIN(pragma_custom_directive);
			parse_token_text();
            current_pragma_prefix = CURRENT_CONFIGURATION->pragma_custom_prefix[i];
			//return PRAGMA_CUSTOM;
			RETURN_TOKEN(PRAGMA_CUSTOM);
		}
	}
}

<unknown_pragma>{
[^\n]* {
  
  /* This will also catch #pragma gcc */
  // Fortran does not work with pragmas so, if it's failed matching a prefix, the whole line will be considered a comment.
	parse_token_text();
//	BEGIN(preprocess);
	BEGIN(COMMENT);
	//return UNKNOWN_PRAGMA;
	RETURN_TOKEN(UNKNOWN_PRAGMA);
}

}

<pragma_custom_directive>{

{idnondigit}({idnondigit}|{digit})* {
	BEGIN(pragma_custom_clause_first);

    pragma_directive_kind_t directive_kind = lookup_pragma_directive(current_pragma_prefix, yytext);

	parse_token_text();

    int token = 0;
    switch (directive_kind)
    {
        case PDK_DIRECTIVE : 
            {
                token = PRAGMA_CUSTOM_DIRECTIVE;
                break;
            }
        case PDK_CONSTRUCT :
            {
                token = PRAGMA_CUSTOM_CONSTRUCT;
                break;
            }
        case PDK_NONE :
            {
                running_error("Unknown directive '%s' for pragma '%s'. Maybe you forgot to register it?"
                        " at line %d.", 
                        yytext,
                        current_pragma_prefix,
//                        fortran_scanning_now->current_filename, 
                        fortran_scanning_now->line_number);
            }
        default:
            internal_error("Invalid pragma directive kind kind=%d", directive_kind);
    }

    current_pragma_prefix = NULL;
    //return token;
    RETURN_TOKEN(token);
}

[ \t] {
	// This blank must be eaten and should be mandatory
}

}


<pragma_custom_clause_first>{

\n {
    fortran_scanning_now->line_number++;
	BEGIN(INITIAL);
	RETURN_EOS;
}

{idnondigit}({idnondigit}|{digit})* {
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

}

<pragma_custom_clause>{

\n {
    fortran_scanning_now->line_number++;
	BEGIN(INITIAL);
	RETURN_EOS;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text();
	//return PRAGMA_CUSTOM_CLAUSE; 
	RETURN_TOKEN(PRAGMA_CUSTOM_CLAUSE);
}

[ \t] {
	// This blank must be eaten
}

}

<pragma_custom_var_list>{

[(] {
	parse_token_text();
	pragma_custom_var_list_parentheses++;
	//return '(';
	RETURN_TOKEN('(');
}

[)] {
	parse_token_text();
	pragma_custom_var_list_parentheses--;
	if (pragma_custom_var_list_parentheses == 0)
	{
		BEGIN(pragma_custom_clause);
	}
	//return ')';
	RETURN_TOKEN(')');
}

[^,({[\]})\n ]+ {
	parse_token_text();
    //return PRAGMA_CLAUSE_ARG_TEXT;
    RETURN_TOKEN(PRAGMA_CLAUSE_ARG_TEXT);
}

[,][ \t]* {
	parse_token_text();
    //return ',';
    RETURN_TOKEN(',');
}

[ \t] { 
	parse_token_text();
    //return PRAGMA_CLAUSE_ARG_TEXT;
    RETURN_TOKEN(PRAGMA_CLAUSE_ARG_TEXT);
}

}

@PROGRAM_UNIT@ {		// CSS / mf03: SUBPARSING
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_PROGRAM_UNIT);
}

@EXPRESSION@ {			// CSS / mf03: SUBPARSING
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_EXPRESSION);
}

@STATEMENT@ {			// CSS / mf03: SUBPARSING
	parse_token_text();
	RETURN_TOKEN(SUBPARSE_STATEMENT);
}

<COMMENT>. {
	// Eat everything in the comment
}

<COMMENT>\n {
	// We have arrived at the end
	fortran_scanning_now->line_number++;
	BEGIN(0);
	RETURN_EOS;
}

[\n] {
	fortran_scanning_now->line_number++;
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

[0-9]+[.][0-9]+([edq][+-]?[0-9]+)(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	// Reals with integer part, fraction, exponent and
	// optionally kind_param
	// 123.456e789
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+[.][0-9]+(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	// Reals with integer part, fraction
	// and optionally kind_param
	// 123.456
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[.][0-9]+([edq][+-]?[0-9]+)(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	// Reals with fraction, exponent
	// and optionally kind_param
	// .456e789
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[.][0-9]+(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	// Reals with fraction and
	// optionally kind_param
	// .456
	
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

[0-9]+[.]?([edq][+-]?[0-9]+)(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
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

[0-9]+[.](_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	// Real with only integer part
	// and optionally kind_param
	// 123.
	parse_token_text();
	RETURN_TOKEN(REAL_LITERAL);
}

(([a-z][a-z0-9]{0,30}|[0-9]+)_)?["]([^"\n]|["]["])*["] {
	parse_token_text();
	RETURN_TOKEN(CHAR_LITERAL);
}

(([a-z][a-z0-9]{0,30}|[0-9]+)_)?[']([^'\n]|[']['])*['] {
	parse_token_text();
	RETURN_TOKEN(CHAR_LITERAL);
}

[0-9]+(_([a-z][a-z0-9]{0,30}|[0-9]+))? {
	parse_token_text();
	RETURN_TOKEN(DECIMAL_LITERAL);
}

<SEEN_FORMAT,INITIAL>format {
    parse_token_text();
    RETURN_TOKEN(TOK_FORMAT);
}

generic {
    parse_token_text();
    RETURN_TOKEN(TOK_GENERIC);
}

volatile {
    parse_token_text();
    RETURN_TOKEN(TOK_VOLATILE);
}

equivalence {
    parse_token_text();
    RETURN_TOKEN(TOK_EQUIVALENCE);
}

eor {
    parse_token_text();
    RETURN_TOKEN(TOK_EOR);
}

protected {
    parse_token_text();
    RETURN_TOKEN(TOK_PROTECTED);
}

pad {
    parse_token_text();
    RETURN_TOKEN(TOK_PAD);
}

file {
    parse_token_text();
    RETURN_TOKEN(TOK_FILE);
}

unformatted {
    parse_token_text();
    RETURN_TOKEN(TOK_UNFORMATTED);
}

named {
    parse_token_text();
    RETURN_TOKEN(TOK_NAMED);
}

subroutine {
    parse_token_text();
    RETURN_TOKEN(TOK_SUBROUTINE);
}

opened {
    parse_token_text();
    RETURN_TOKEN(TOK_OPENED);
}

delim {
    parse_token_text();
    RETURN_TOKEN(TOK_DELIM);
}

source {
    parse_token_text();
    RETURN_TOKEN(TOK_SOURCE);
}

critical {
    parse_token_text();
    RETURN_TOKEN(TOK_CRITICAL);
}

action {
    parse_token_text();
    RETURN_TOKEN(TOK_ACTION);
}

import {
    parse_token_text();
    RETURN_TOKEN(TOK_IMPORT);
}

block {
    parse_token_text();
    RETURN_TOKEN(TOK_BLOCK);
}

enum {
    parse_token_text();
    RETURN_TOKEN(TOK_ENUM);
}

overridable {
    parse_token_text();
    RETURN_TOKEN(TOK_OVERRIDABLE);
}

module {
    parse_token_text();
    RETURN_TOKEN(TOK_MODULE);
}

sign {
    parse_token_text();
    RETURN_TOKEN(TOK_SIGN);
}

extends {
    parse_token_text();
    RETURN_TOKEN(TOK_EXTENDS);
}

pass {
    parse_token_text();
    RETURN_TOKEN(TOK_PASS);
}

go {
    parse_token_text();
    RETURN_TOKEN(TOK_GO);
}

entry {
    parse_token_text();
    RETURN_TOKEN(TOK_ENTRY);
}

where {
    parse_token_text();
    RETURN_TOKEN(TOK_WHERE);
}

procedure {
    parse_token_text();
    RETURN_TOKEN(TOK_PROCEDURE);
}

name {
    parse_token_text();
    RETURN_TOKEN(TOK_NAME);
}

double {
    parse_token_text();
    RETURN_TOKEN(TOK_DOUBLE);
}

contains {
    parse_token_text();
    RETURN_TOKEN(TOK_CONTAINS);
}

logical {
    parse_token_text();
    RETURN_TOKEN(TOK_LOGICAL);
}

newunit {
    parse_token_text();
    RETURN_TOKEN(TOK_NEWUNIT);
}

nullify {
    parse_token_text();
    RETURN_TOKEN(TOK_NULLIFY);
}

deferred {
    parse_token_text();
    RETURN_TOKEN(TOK_DEFERRED);
}

only {
    parse_token_text();
    RETURN_TOKEN(TOK_ONLY);
}

exist {
    parse_token_text();
    RETURN_TOKEN(TOK_EXIST);
}

interface {
    parse_token_text();
    RETURN_TOKEN(TOK_INTERFACE);
}

save {
    parse_token_text();
    RETURN_TOKEN(TOK_SAVE);
}

advance {
    parse_token_text();
    RETURN_TOKEN(TOK_ADVANCE);
}

stat {
    parse_token_text();
    RETURN_TOKEN(TOK_STAT);
}

return {
    parse_token_text();
    RETURN_TOKEN(TOK_RETURN);
}

readwrite {
    parse_token_text();
    RETURN_TOKEN(TOK_READWRITE);
}

assign {
    parse_token_text();
    RETURN_TOKEN(TOK_ASSIGN);
}

assignment {
    parse_token_text();
    RETURN_TOKEN(TOK_ASSIGNMENT);
}

print {
    parse_token_text();
    RETURN_TOKEN(TOK_PRINT);
}

iostat {
    parse_token_text();
    RETURN_TOKEN(TOK_IOSTAT);
}

sync {
    parse_token_text();
    RETURN_TOKEN(TOK_SYNC);
}

complex {
    parse_token_text();
    RETURN_TOKEN(TOK_COMPLEX);
}

asynchronous {
    parse_token_text();
    RETURN_TOKEN(TOK_ASYNCHRONOUS);
}

images {
    parse_token_text();
    RETURN_TOKEN(TOK_IMAGES);
}

end {
    parse_token_text();
    RETURN_TOKEN(TOK_END);
}

final {
    parse_token_text();
    RETURN_TOKEN(TOK_FINAL);
}

inout {
    parse_token_text();
    RETURN_TOKEN(TOK_INOUT);
}

nextrec {
    parse_token_text();
    RETURN_TOKEN(TOK_NEXTREC);
}

kind {
    parse_token_text();
    RETURN_TOKEN(TOK_KIND);
}

common {
    parse_token_text();
    RETURN_TOKEN(TOK_COMMON);
}

blank {
    parse_token_text();
    RETURN_TOKEN(TOK_BLANK);
}

iolength {
    parse_token_text();
    RETURN_TOKEN(TOK_IOLENGTH);
}

integer {
    parse_token_text();
    RETURN_TOKEN(TOK_INTEGER);
}

deallocate {
    parse_token_text();
    RETURN_TOKEN(TOK_DEALLOCATE);
}

function {
    parse_token_text();
    RETURN_TOKEN(TOK_FUNCTION);
}

c {
    parse_token_text();
    RETURN_TOKEN(TOK_C);
}

enumerator {
    parse_token_text();
    RETURN_TOKEN(TOK_ENUMERATOR);
}

recursive {
    parse_token_text();
    RETURN_TOKEN(TOK_RECURSIVE);
}

sequence {
    parse_token_text();
    RETURN_TOKEN(TOK_SEQUENCE);
}

inquire {
    parse_token_text();
    RETURN_TOKEN(TOK_INQUIRE);
}

flush {
    parse_token_text();
    RETURN_TOKEN(TOK_FLUSH);
}

else {
    parse_token_text();
    RETURN_TOKEN(TOK_ELSE);
}

intent {
    parse_token_text();
    RETURN_TOKEN(TOK_INTENT);
}

external {
    parse_token_text();
    RETURN_TOKEN(TOK_EXTERNAL);
}

operator {
    parse_token_text();
    RETURN_TOKEN(TOK_OPERATOR);
}

optional {
    parse_token_text();
    RETURN_TOKEN(TOK_OPTIONAL);
}

unit {
    parse_token_text();
    RETURN_TOKEN(TOK_UNIT);
}

size {
    parse_token_text();
    RETURN_TOKEN(TOK_SIZE);
}

nopass {
    parse_token_text();
    RETURN_TOKEN(TOK_NOPASS);
}

type {
    parse_token_text();
    RETURN_TOKEN(TOK_TYPE);
}

mold {
    parse_token_text();
    RETURN_TOKEN(TOK_MOLD);
}

precision {
    parse_token_text();
    RETURN_TOKEN(TOK_PRECISION);
}

pending {
    parse_token_text();
    RETURN_TOKEN(TOK_PENDING);
}

continue {
    parse_token_text();
    RETURN_TOKEN(TOK_CONTINUE);
}

result {
    parse_token_text();
    RETURN_TOKEN(TOK_RESULT);
}

real {
    parse_token_text();
    RETURN_TOKEN(TOK_REAL);
}

then {
    parse_token_text();
    RETURN_TOKEN(TOK_THEN);
}

stream {
    parse_token_text();
    RETURN_TOKEN(TOK_STREAM);
}

do {
    parse_token_text();
    RETURN_TOKEN(TOK_DO);
}

default {
    parse_token_text();
    RETURN_TOKEN(TOK_DEFAULT);
}

contiguous {
    parse_token_text();
    RETURN_TOKEN(TOK_CONTIGUOUS);
}

stop {
    parse_token_text();
    RETURN_TOKEN(TOK_STOP);
}

while {
    parse_token_text();
    RETURN_TOKEN(TOK_WHILE);
}

program {
    parse_token_text();
    RETURN_TOKEN(TOK_PROGRAM);
}

rec {
    parse_token_text();
    RETURN_TOKEN(TOK_REC);
}

open {
    parse_token_text();
    RETURN_TOKEN(TOK_OPEN);
}

case {
    parse_token_text();
    RETURN_TOKEN(TOK_CASE);
}

recl {
    parse_token_text();
    RETURN_TOKEN(TOK_RECL);
}

dimension {
    parse_token_text();
    RETURN_TOKEN(TOK_DIMENSION);
}

elemental {
    parse_token_text();
    RETURN_TOKEN(TOK_ELEMENTAL);
}

forall {
    parse_token_text();
    RETURN_TOKEN(TOK_FORALL);
}

position {
    parse_token_text();
    RETURN_TOKEN(TOK_POSITION);
}

fmt {
    parse_token_text();
    RETURN_TOKEN(TOK_FMT);
}

read {
    parse_token_text();
    RETURN_TOKEN(TOK_READ);
}

out {
    parse_token_text();
    RETURN_TOKEN(TOK_OUT);
}

lock {
    parse_token_text();
    RETURN_TOKEN(TOK_LOCK);
}

decimal {
    parse_token_text();
    RETURN_TOKEN(TOK_DECIMAL);
}

select {
    parse_token_text();
    RETURN_TOKEN(TOK_SELECT);
}

direct {
    parse_token_text();
    RETURN_TOKEN(TOK_DIRECT);
}

write {
    parse_token_text();
    RETURN_TOKEN(TOK_WRITE);
}

codimension {
    parse_token_text();
    RETURN_TOKEN(TOK_CODIMENSION);
}

close {
    parse_token_text();
    RETURN_TOKEN(TOK_CLOSE);
}

id {
    parse_token_text();
    RETURN_TOKEN(TOK_ID);
}

wait {
    parse_token_text();
    RETURN_TOKEN(TOK_WAIT);
}

all {
    parse_token_text();
    RETURN_TOKEN(TOK_ALL);
}

elsewhere {
    parse_token_text();
    RETURN_TOKEN(TOK_ELSEWHERE);
}

len {
    parse_token_text();
    RETURN_TOKEN(TOK_LEN);
}

impure {
    parse_token_text();
    RETURN_TOKEN(TOK_IMPURE);
}

exit {
    parse_token_text();
    RETURN_TOKEN(TOK_EXIT);
}

rewind {
    parse_token_text();
    RETURN_TOKEN(TOK_REWIND);
}

backspace {
    parse_token_text();
    RETURN_TOKEN(TOK_BACKSPACE);
}

nml {
    parse_token_text();
    RETURN_TOKEN(TOK_NML);
}

intrinsic {
    parse_token_text();
    RETURN_TOKEN(TOK_INTRINSIC);
}

public {
    parse_token_text();
    RETURN_TOKEN(TOK_PUBLIC);
}

errmsg {
    parse_token_text();
    RETURN_TOKEN(TOK_ERRMSG);
}

namelist {
    parse_token_text();
    RETURN_TOKEN(TOK_NAMELIST);
}

use {
    parse_token_text();
    RETURN_TOKEN(TOK_USE);
}

submodule {
    parse_token_text();
    RETURN_TOKEN(TOK_SUBMODULE);
}

err {
    parse_token_text();
    RETURN_TOKEN(TOK_ERR);
}

encoding {
    parse_token_text();
    RETURN_TOKEN(TOK_ENCODING);
}

acquired {
    parse_token_text();
    RETURN_TOKEN(TOK_ACQUIRED);
}

abstract {
    parse_token_text();
    RETURN_TOKEN(TOK_ABSTRACT);
}

character {
    parse_token_text();
    RETURN_TOKEN(TOK_CHARACTER);
}

number {
    parse_token_text();
    RETURN_TOKEN(TOK_NUMBER);
}

concurrent {
    parse_token_text();
    RETURN_TOKEN(TOK_CONCURRENT);
}

endfile {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDFILE);
}

parameter {
    parse_token_text();
    RETURN_TOKEN(TOK_PARAMETER);
}

data {
    parse_token_text();
    RETURN_TOKEN(TOK_DATA);
}

pos {
    parse_token_text();
    RETURN_TOKEN(TOK_POS);
}

private {
    parse_token_text();
    RETURN_TOKEN(TOK_PRIVATE);
}

round {
    parse_token_text();
    RETURN_TOKEN(TOK_ROUND);
}

to {
    parse_token_text();
    RETURN_TOKEN(TOK_TO);
}

sequential {
    parse_token_text();
    RETURN_TOKEN(TOK_SEQUENTIAL);
}

allocate {
    parse_token_text();
    RETURN_TOKEN(TOK_ALLOCATE);
}

target {
    parse_token_text();
    RETURN_TOKEN(TOK_TARGET);
}

class {
    parse_token_text();
    RETURN_TOKEN(TOK_CLASS);
}

cycle {
    parse_token_text();
    RETURN_TOKEN(TOK_CYCLE);
}

status {
    parse_token_text();
    RETURN_TOKEN(TOK_STATUS);
}

iomsg {
    parse_token_text();
    RETURN_TOKEN(TOK_IOMSG);
}

form {
    parse_token_text();
    RETURN_TOKEN(TOK_FORM);
}

is {
    parse_token_text();
    RETURN_TOKEN(TOK_IS);
}

none {
    parse_token_text();
    RETURN_TOKEN(TOK_NONE);
}

value {
    parse_token_text();
    RETURN_TOKEN(TOK_VALUE);
}

unlock {
    parse_token_text();
    RETURN_TOKEN(TOK_UNLOCK);
}

formatted {
    parse_token_text();
    RETURN_TOKEN(TOK_FORMATTED);
}

in {
    parse_token_text();
    RETURN_TOKEN(TOK_IN);
}

implicit {
    parse_token_text();
    RETURN_TOKEN(TOK_IMPLICIT);
}

if {
    parse_token_text();
    RETURN_TOKEN(TOK_IF);
}

associate {
    parse_token_text();
    RETURN_TOKEN(TOK_ASSOCIATE);
}

bind {
    parse_token_text();
    RETURN_TOKEN(TOK_BIND);
}

allocatable {
    parse_token_text();
    RETURN_TOKEN(TOK_ALLOCATABLE);
}

access {
    parse_token_text();
    RETURN_TOKEN(TOK_ACCESS);
}

call {
    parse_token_text();
    RETURN_TOKEN(TOK_CALL);
}

pure {
    parse_token_text();
    RETURN_TOKEN(TOK_PURE);
}

memory {
    parse_token_text();
    RETURN_TOKEN(TOK_MEMORY);
}

pointer {
    parse_token_text();
    RETURN_TOKEN(TOK_POINTER);
}

non_overridable {
    parse_token_text();
    RETURN_TOKEN(TOK_NON_OVERRIDABLE);
}

non_intrinsic {
    parse_token_text();
    RETURN_TOKEN(TOK_NON_INTRINSIC);
}

vector {
    parse_token_text();
    RETURN_TOKEN(TOK_VECTOR);
}

pixel {
    parse_token_text();
    RETURN_TOKEN(TOK_PIXEL);
}

 /* Combined versions */
allstop {
    parse_token_text();
    RETURN_TOKEN(TOK_ALLSTOP);
}

endif {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDIF);
}

endfunction {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDFUNCTION);
}

enddo {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDDO);
}

blockdata  {
    parse_token_text();
    RETURN_TOKEN(TOK_BLOCKDATA);
}

endmodule {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDMODULE);
}

doubleprecision  {
    parse_token_text();
    RETURN_TOKEN(TOK_DOUBLEPRECISION);
}

doublecomplex  {
    parse_token_text();
    RETURN_TOKEN(TOK_DOUBLECOMPLEX);
}

endinterface {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDINTERFACE);
}

elseif  {
    parse_token_text();
    RETURN_TOKEN(TOK_ELSEIF );
}

endprocedure {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDPROCEDURE);
}

elsewhere  {
    parse_token_text();
    RETURN_TOKEN(TOK_ELSEWHERE);
}

endprogram {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDPROGRAM);
}

endassociate  {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDASSOCIATE);
}

endselect {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDSELECT);
}

endblock  {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDBLOCK);
}

endsubmodule {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDSUBMODULE);
}

endblockdata  {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDBLOCKDATA);
}

endsubroutine {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDSUBROUTINE);
}

endcritical  {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDCRITICAL);
}

endtype {
    parse_token_text();
    RETURN_TOKEN(TOK_ENDTYPE);
}

goto {
    parse_token_text();
    RETURN_TOKEN(TOK_GOTO);
}

 /* Non keyword tokens */

[a-z]([a-z0-9_]*) {
	parse_token_text();
	RETURN_TOKEN(IDENTIFIER);
}

"**" {
	parse_token_text();
	RETURN_TOKEN(TOK_RAISE);
}

"*" {
	parse_token_text();
	RETURN_TOKEN('*');
}


"/" {
	parse_token_text();
	RETURN_TOKEN('/');
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

"==" {
	parse_token_text();
	RETURN_TOKEN(TOK_EQUAL);
}

".eq." {
	parse_token_text();
	RETURN_TOKEN(TOK_EQUAL);
}

".eqv." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOGICAL_EQUIVALENT);
}

"/=" {
	parse_token_text();
	RETURN_TOKEN(TOK_NOT_EQUAL);
}

".neqv." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOGICAL_NOT_EQUIVALENT);
}

".ne." {
	parse_token_text();
	RETURN_TOKEN(TOK_NOT_EQUAL);
}

".lt." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOWER_THAN);
}

"<" {
	parse_token_text();
	RETURN_TOKEN(TOK_LOWER_THAN);
}

".le." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOWER_OR_EQUAL_THAN);
}

"<=" {
	parse_token_text();
	RETURN_TOKEN(TOK_LOWER_OR_EQUAL_THAN);
}

".gt." {
	parse_token_text();
	RETURN_TOKEN(TOK_GREATER_THAN);
}

">" {
	parse_token_text();
	RETURN_TOKEN(TOK_GREATER_THAN);
}

".ge." {
	parse_token_text();
	RETURN_TOKEN(TOK_GREATER_OR_EQUAL_THAN);
}

".and." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOGICAL_AND);
}

".or." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOGICAL_OR);
}

".not." {
	parse_token_text();
	RETURN_TOKEN(TOK_LOGICAL_NOT);
}

".true." { 
	parse_token_text();
	RETURN_TOKEN(TOK_TRUE);
}

".false." {
	parse_token_text();
	RETURN_TOKEN(TOK_FALSE);
}


">=" {
	parse_token_text();
	RETURN_TOKEN(TOK_GREATER_OR_EQUAL_THAN);
}


"," {
	parse_token_text();
	RETURN_TOKEN(',');
}

"=>" {
	parse_token_text();
	RETURN_TOKEN(TOK_POINTER_ACCESS);
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

\.[a-z]+\. {
	parse_token_text();
	RETURN_TOKEN(USER_DEFINED_OPERATOR);
}

<*>[[:blank:]] {
	// A blank should be uninteresting here
}

. {
	if (isprint(yytext[0]))
	{
		fprintf(stderr, "Line %d. Error. Unexpected character: `%c' (0x%X)\n", fortran_scanning_now->line_number, yytext[0], yytext[0]);
	}
	else
	{
		fprintf(stderr, "Line %d. Error. Unexpected character: 0x%X\n", fortran_scanning_now->line_number, yytext[0]);
	}
	exit(EXIT_FAILURE);
  }

%%

// Some flex implementations undef too many things here
#ifndef yytext_ptr
	#define yytext_ptr yytext
#endif

#define FLEX_LVAL mf03lval

static void parse_token_text(void)
{
    FLEX_LVAL.token_atrib.token_line = fortran_scanning_now->line_number;
    FLEX_LVAL.token_atrib.token_file = uniquestr(fortran_scanning_now->current_filename);
    FLEX_LVAL.token_atrib.token_text = uniquestr(yytext);
}

UNUSED_PARAMETER static void parse_token_text_str(const char* c)
{
    FLEX_LVAL.token_atrib.token_line = fortran_scanning_now->line_number;
    FLEX_LVAL.token_atrib.token_file = uniquestr(fortran_scanning_now->current_filename);
    FLEX_LVAL.token_atrib.token_text = uniquestr(c);
}

static void reintroduce_continued_line(char* continued_line, char double_continuation)
{
	char* temporal = strdup(continued_line);

	// Copy all the string springing over &\n&

	// Find the newline, there is only one, regex enforces this
	char* newline = strchr(temporal, '\n');
	char* limit_first_line; 
	char* limit_second_line;
	char second_line_is_comment = 0;
	if (newline != 0)
	{
		// We will look for the rightmost & and the leftmost &
		// so we cut here
		*newline = '\0';
		
		// We remove any comment 
		trim_inline_comment(temporal);

		// Look for rightmost "&"
		limit_first_line = strrchr(temporal, '&'); 

		if (double_continuation)
		{
			// Look for the leftmost "&"
			limit_second_line = strchr(newline + 1, '&');
			// We are not interested in "&"
			limit_second_line++;
		}
		else
		{
			limit_second_line = newline+1;

			// Advance blanks
			while (*limit_second_line == ' ' || *limit_first_line == '\t') 
			{
				limit_second_line++;
			}

			// This is a comment !
			if (*limit_second_line == '!')
			{
				second_line_is_comment = 1;
			}
		}

		if (limit_first_line && limit_second_line)
		{
			// Let's reintroduce all the whole line
			*limit_first_line = '\0';
			if (!second_line_is_comment)
			{
				DEBUG_CODE() DEBUG_MESSAGE("Let's reintroduce -%s%s-\n", temporal, limit_second_line);
			}
			else
			{
				DEBUG_CODE() DEBUG_MESSAGE("Let's reintroduce -%s%s-\n", temporal, "&\n");
			}
			char* last_character = limit_second_line + strlen(limit_second_line);

			// But only if the second line was something useful
			if (!second_line_is_comment)
			{
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
				}
			}
			else
			{
				// If it was a comment we have to reintroduce "&\n"
				// to enable "continuation magic" for the incoming line
				unput('\n');
				unput('&');
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
	}

	// This line was joined
	fortran_scanning_now->joined_lines++;
	DEBUG_CODE() DEBUG_MESSAGE("Incrementing joined_lines (%d)\n", fortran_scanning_now->joined_lines);
	// Free
	free(temporal);
}

#if 0
static void reintroduce_continued_omp_line(char* continued_line, const char* sentinel)
{
	// The strategy here is to reuse reintroduce_continued_line
	// removing all the !$omp (or they will be trimmed)
	char* iter = continued_line;

	while (*iter != '!') iter++;

	// Jump $OMP
	iter += 5;

	char* temporal = strdup(iter);
	
	char* newline = strchr(temporal, '\n');

	*newline = '\0';
	newline++;

	// Now look for a !
	while (*newline != '!') newline++;
	
	// Jump $OMP
	newline += 5;

	// Now advance till we find something that is not blank
	while ((*newline == ' ') || (*newline == '\t')) newline++;

	char double_continuation = (*newline == '&');

	// Now join the second line to the first and let reintroduce_continued_line
	// to kindly do the job
	strcat(temporal, "\n");
	strcat(temporal, newline);

	reintroduce_continued_line(temporal, double_continuation);

	// Now reintroduce sentinel
	int i;
	int length = strlen(sentinel);
	for (i = length-1; i >= 0; i--)
	{
		unput(sentinel[i]);
	}

	free(temporal);
}

static void reintroduce_continued_coco_omp_line(char* continued_line)
{
	// The strategy here is to reuse reintroduce_continued_line
	// removing all the !$omp (or they will be trimmed)
	char* iter = continued_line;

	while (*iter != '!') iter++;

	// Jump $
	iter += 2;

	char* temporal = strdup(iter);
	
	char* newline = strchr(temporal, '\n');

	*newline = '\0';
	newline++;

	// Now look for a !
	while (*newline != '!') newline++;
	
	// Jump $
	newline += 2;

	// Now advance till we find something that is not blank
	while ((*newline == ' ') || (*newline == '\t')) newline++;

	char double_continuation = (*newline == '&');

	// Now join the second line to the first and let reintroduce_continued_line
	// to kindly do the job
	strcat(temporal, "\n");
	strcat(temporal, newline);

	reintroduce_continued_line(temporal, double_continuation);

	// Now reintroduce !$
	unput(' ');
	unput('$');
	unput('!');

	free(temporal);
}
#endif

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
	

	result = calloc((size_t)(final-start+1), sizeof(char));

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
		running_error("Too many levels (>%d) of inclusion at %s:%d", MAX_INCLUDE_DEPTH,
				fortran_scanning_now->filename, fortran_scanning_now->line_number);
	}	

	DEBUG_CODE() DEBUG_MESSAGE("Opening included file %s at %s:%d", include_name, 
			fortran_scanning_now->filename, fortran_scanning_now->line_number);
	const char* include_filename = find_file_in_directories(
            CURRENT_CONFIGURATION->num_include_dirs,
            CURRENT_CONFIGURATION->include_dirs, 
            include_name,
            /* origin */ fortran_scanning_now->current_filename);
	if (include_filename == NULL)
	{
		running_error("%s:%d: error: cannot open included file '%s'\n", 
                fortran_scanning_now->current_filename, 
                fortran_scanning_now->line_number,
				include_name);
	}

	temporal_file_t temporal_include_filename = new_temporal_file();
	if (!temporal_include_filename)
	{
		running_error("%s:%d: error: cannot create temporal included file",
                fortran_scanning_now->current_filename, 
                fortran_scanning_now->line_number);
	}

	if (copy_file(include_filename, temporal_include_filename->name) != 0)
    {
        running_error("Failure during copy of '%s' to '%s'. %s\n", 
                include_filename, 
                temporal_include_filename->name, 
                strerror(errno));
    }
	// This avoids problems with not-EOL ended files

    FILE* included_file = fopen(temporal_include_filename->name, "r+");
    if (included_file == NULL)
    {
		running_error("Cannot open for modification temporal included file '%s'. %s\n", 
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
		running_error("Cannot open for reading temporal included file '%s'. %s\n", 
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

int mf03_open_file_for_scanning(const char* scanned_filename, const char* input_filename)
{
	FILE* file;

	file = fopen(scanned_filename, "r");

	if (file == NULL)
	{
		running_error("error: cannot open file '%s' (%s)", scanned_filename, strerror(errno));
	}

    fortran_scanning_now = &include_stack[0];
	memset(fortran_scanning_now, 0, sizeof(*fortran_scanning_now));
	fortran_scanning_now->filename = uniquestr(scanned_filename);
	fortran_scanning_now->file_descriptor = file;
	fortran_scanning_now->line_number = 1;

    fortran_scanning_now->current_filename = uniquestr(input_filename);

	fortran_scanning_now->scanning_buffer = yy_create_buffer(file, YY_BUF_SIZE);

	// yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);

	return 0;
}

char* TL_SOURCE_STRING = "(tl-source-string)";
// maybe this is not the best place to change the configuration but it must work
compilation_configuration_t* string_scanning_configuration = 0;

int mf03_prepare_string_for_scanning(const char* str)
{
	static int num_string = 0;
	include_stack_size = 0;
	fortran_scanning_now = &include_stack[include_stack_size];
	fortran_scanning_now->line_number = 1;
	fortran_scanning_now->joined_lines = 0;
	
	const char* current_filename = CURRENT_COMPILED_FILE->input_filename;
	fortran_scanning_now->filename = calloc(strlen(TL_SOURCE_STRING) + strlen(current_filename) + 10, sizeof(char));
    char filename[256];
	snprintf(filename, 255, "%s%s-%d", TL_SOURCE_STRING, current_filename, num_string);
    filename[255] = '\0';
    fortran_scanning_now->filename = uniquestr(filename);
	
	// Change the configuration for scanning strings:
	if (!string_scanning_configuration){
		string_scanning_configuration = (compilation_configuration_t*)malloc(sizeof(compilation_configuration_t));
		*string_scanning_configuration = *(compilation_process.current_compilation_configuration);
	}
	// Now we can do what we want on the CURRENT_CONFIGURATION
	// CURRENT_CONFIGURATION->expand_includes = 0;
	
	// CURRENT_COMPILED_FILE->length_include_table = 1;
	// CURRENT_COMPILED_FILE->index_include_table = 0;
	// CURRENT_COMPILED_FILE->include_table = (char**) calloc(1, sizeof(*CURRENT_COMPILED_FILE->include_table));
	// CURRENT_COMPILED_FILE->include_table[0] = strdup(fortran_scanning_now->filename);
	// CURRENT_COMPILED_FILE->in_include = 0;
	
	num_string++;
	fortran_scanning_now->scanning_buffer = yy_scan_string(str);
	yy_switch_to_buffer(fortran_scanning_now->scanning_buffer);
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

	if (*p == ';' || *p == '\n') 
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

	if (*p != ';' && *p != '\n')
	{
		DEBUG_CODE() DEBUG_MESSAGE("This format statement does not end with an EOS (*p = '%c')", *p);
		return 0;
	}
	if (*p == ';') *p = '\n';

	return 1;
}
