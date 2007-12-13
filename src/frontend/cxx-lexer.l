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
%{
/* 
   This file should be preprocessed with tpp (available in lib)
   in order to produce c99.l and cxx03.l
*/
/*!if CPLUSPLUS*/
/*
   Lexer of ISO/IEC 14882:2003 - C++
*/
/*!endif*/
/*!if C99*/
/*
   Lexer of ISO/IEC 9899:1999 - C
*/
/*!endif*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <libgen.h>
#include "cxx-driver.h"
#include "cxx-utils.h"
#include "cxx-lexer.h"
#include "cxx-ast.h"
/*!if C99*/
#include "c99-parser.h"
/*!endif*/
/*!if CPLUSPLUS*/
#include "cxx-parser.h"
/*!endif*/

// For Garbage collection support

static int yywrap(void);

static void parse_token_text(void);

struct scan_file_descriptor scanning_now;

static char* main_input_filename;

static int omp_var_list_parentheses = 0;
static int omp_schedule_parentheses = 0;
static char omp_seen_schedule_kind = 0;

static int pragma_custom_var_list_parentheses = 0;

static char *current_pragma_prefix = NULL;

static int num_new_lines(char* c)
{
    int result = 0;

    while (*c)
    {
        if (*c == '\n') 
            result++;
        c++;
    }

    return result;
}

static int include_counter;

%}

 /* Lexical conventions A.2 */
 /* A.2.1 */
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
 /* A.2.2 */
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
 /* A.2.12 */
nondigit        ([_a-zA-Z]|uchar)
 /* A.2.13 */
digit       [0-9]
 /* A.2.20 */
nonzero         [1-9]
 /* A.2.21 */
octaldigit      [0-7]
 /* A.2.22 */
hexdigit        [a-fA-F0-9]
 /* A.2.23 includes GNU extension of long long */
integersuffix   ({longsuffix}{unsignedsuffix}?|{unsignedsuffix}{longsuffix}?|{unsignedsuffix}{longlongsuffix}|{longlongsuffix}{unsignedsuffix}?)
 /* A.2.24 */
unsignedsuffix  [uU]
 /* A.2.25 */
longsuffix  [lL]

 /* GNU Extension */
longlongsuffix [lL][lL]


 /* A.2.28 */
cchar       ([^\\'\n]|{escape_seq}|(\\[']))

 /* A.2.29 */
 /* \e is a GNU extension */
escape_seq      ((\\["'?eabfnrtv\\])|(\\{octaldigit}{1,3})|((\\x)+{hexdigit}+))
 /* A.2.34 */
fractconst  (({digit}*\.{digit}+)|({digit}+(\.)?))
 /* A.2.35 */
exponent    ([eE][+-]?{digit}+)
 /* A.2.38 */
floatingsuffix  [flFL]
 /* A.2.40 */
schar       ([^\\"\n]|{escape_seq}|(\\["]))

idnondigit  ({nondigit}|{uchar})
hexprefix   0[xX]

 /* C99 additional */
hexadecimal_fractional_constant (({hexdigit}*\.{hexdigit}+)|({hexdigit}\.))
binary_exponent_part ([pP]([+-])?{digit}+)

 /* states */
%x longcomment
%x linecomment
%x preprocess
%x pragma_line
%x openmp_directive
%x openmp_clause
%x openmp_default
%x unknown_pragma
%x pragma_custom_directive
%x pragma_custom_clause
%x pragma_custom_clause_first

%s openmp_var_list
%s openmp_schedule
%s pragma_custom_var_list

%%
 /* preprocess */

"#" { 
    BEGIN(preprocess); 
}

<preprocess>[ ]*pragma[ ]+ {
	BEGIN(pragma_line);
}

<pragma_line>{idnondigit}({idnondigit}|{digit})* {
	// Unknown pragmas
	int i;
	char found = 0;

	char matched = 0;

	// OpenMP
	if (strcmp(yytext, "omp") == 0)
	{
		if (CURRENT_CONFIGURATION(disable_openmp))
		{
			fprintf(stderr, "Ignoring OpenMP directive at %s:%d\n",
					scanning_now.current_filename,
					scanning_now.line_number);
			matched = 1;
			BEGIN(unknown_pragma);
			yymore();
		}
		else
		{
			BEGIN(openmp_directive);
			parse_token_text();
			return OMP_PRAGMA;
		}
	}

	// Ignore gcc
	if (!matched && strcasecmp(yytext, "gcc") == 0)
	{
        // All the whole line will be handled as an unknown pragma
		BEGIN(unknown_pragma);
        // current yytext must be included in the "unknown pragma"
		yymore();
		matched = 1;
	}

	if (!matched)
	{
		for (i = 0; i < CURRENT_CONFIGURATION(num_pragma_custom_prefix); i++)
		{
			if (strcmp(yytext, CURRENT_CONFIGURATION(pragma_custom_prefix[i])) == 0)
            {
                found = 1;
                break;
            }
		}

		if (!found)
		{
			fprintf(stderr, "Warning, unknown '#pragma %s' at %s:%d\n", 
					yytext,
					scanning_now.current_filename,
					scanning_now.line_number);
			BEGIN(unknown_pragma);
			// Ignore this prefix as an unknown pragma
			yymore();
			matched = 1;
		}
		else
		{
			BEGIN(pragma_custom_directive);
			parse_token_text();
            current_pragma_prefix = CURRENT_CONFIGURATION(pragma_custom_prefix[i]);
			return PRAGMA_CUSTOM;
		}
	}
}

<unknown_pragma>{
  /* This will also catch #pragma gcc */

[^\n]* {
	parse_token_text();
	BEGIN(preprocess);
	return UNKNOWN_PRAGMA;
}

}

<openmp_directive>{

[ \t] { }

parallel {
	BEGIN(openmp_clause);
	return OMP_PARALLEL;
}

for {
	BEGIN(openmp_clause);
	return OMP_FOR;
}

sections {
	BEGIN(openmp_clause);
	return OMP_SECTIONS;
}

section {
	BEGIN(openmp_clause);
	return OMP_SECTION;
}

parallel[ ]+for {
	BEGIN(openmp_clause);
	return OMP_PARALLEL_FOR;
}

parallel[ ]+sections {
	BEGIN(openmp_clause);
	return OMP_PARALLEL_SECTIONS;
}

parallel[ ]+single {
	BEGIN(openmp_clause);
	return OMP_PARALLEL_SINGLE;
}

single {
	BEGIN(openmp_clause);
	return OMP_SINGLE;
}

master {
	BEGIN(openmp_clause);
	return OMP_MASTER;
}

atomic {
	BEGIN(openmp_clause);
	return OMP_ATOMIC;
}

flush {
	BEGIN(openmp_clause);
	return OMP_FLUSH;
}

barrier {
	BEGIN(openmp_clause);
	return OMP_BARRIER;
}

ordered {
	BEGIN(openmp_clause);
	return OMP_ORDERED;
}

critical {
	BEGIN(openmp_clause);
	return OMP_CRITICAL;
}

threadprivate {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_THREADPRIVATE;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom constructs/directives
	BEGIN(openmp_clause);
    pragma_directive_kind_t directive_kind = lookup_pragma_directive("omp", yytext);

	parse_token_text(); 

    int token = 0;
    switch (directive_kind)
    {
        case PDK_DIRECTIVE : 
            {
                token = OMP_CUSTOM_DIRECTIVE;
                break;
            }
        case PDK_CONSTRUCT :
            {
                token = OMP_CUSTOM_CONSTRUCT;
                break;
            }
        case PDK_NONE :
            {
                running_error("Unknown OpenMP directive '%s'"
                        " at %s:%d. Maybe you forgot to register it?", 
                        yytext,
                        scanning_now.current_filename, 
                        scanning_now.line_number);
            }
        default:
            internal_error("Invalid pragma directive kind kind=%d", directive_kind);
    }

    return token;
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
                        " at %s:%d.", 
                        yytext,
                        current_pragma_prefix,
                        scanning_now.current_filename, 
                        scanning_now.line_number);
            }
        default:
            internal_error("Invalid pragma directive kind kind=%d", directive_kind);
    }

    current_pragma_prefix = NULL;
    return token;
}

[ \t] {
	// This blank must be eaten and should be mandatory
}

}

<pragma_custom_clause_first>{

\n {
    scanning_now.line_number++;
	BEGIN(INITIAL);
	return PRAGMA_CUSTOM_NEWLINE;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text(); 
	return PRAGMA_CUSTOM_CLAUSE; 
}

[ \t] {
	// This blank must be eaten
}

[(] {
        // Parameter
	pragma_custom_var_list_parentheses = 1;
    BEGIN(pragma_custom_var_list);
    return '(';
}

}

<pragma_custom_clause>{

\n {
    scanning_now.line_number++;
	BEGIN(INITIAL);
	return PRAGMA_CUSTOM_NEWLINE;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text(); 
	return PRAGMA_CUSTOM_CLAUSE; 
}

[ \t] {
	// This blank must be eaten
}

}

<pragma_custom_var_list>{

[(] {
	parse_token_text();
	pragma_custom_var_list_parentheses++;
	return '(';
}

[)] {
	parse_token_text();
	pragma_custom_var_list_parentheses--;
	if (pragma_custom_var_list_parentheses == 0)
	{
		BEGIN(pragma_custom_clause);
	}
	return ')';
}

[^,({[\]})\n ]+ {
    parse_token_text();
    return PRAGMA_CLAUSE_ARG_TEXT;
}

[,][ \t]* {
    parse_token_text();
    return ',';
}

[ \t] { 
    parse_token_text();
    return PRAGMA_CLAUSE_ARG_TEXT;
}

}

<openmp_clause>{

\n {
    scanning_now.line_number++;
    BEGIN(INITIAL);

    return OMP_NEWLINE;
}

[(] {
   /* 
	   This is a special one, this is a clause without name
	   and must be lexed as if it was a var-list, but a '(' has
	   already been seen thus "omp_var_list_parentheses == 1"
   */
   omp_var_list_parentheses = 1;

   BEGIN(openmp_var_list);
   return '(';
}

[,] {
	return ',';
}

[ \t] { }

schedule {
	omp_schedule_parentheses = 0;
	omp_seen_schedule_kind = 0;
	BEGIN(openmp_schedule);
	return OMP_SCHEDULE;
}

nowait {
	return OMP_NOWAIT;
}

ordered {
	return OMP_ORDERED;
}

private {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_PRIVATE;
}

copyprivate {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_COPYPRIVATE;
}

firstprivate {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_FIRSTPRIVATE;
}

lastprivate {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_LASTPRIVATE;
}

shared {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_SHARED;
}

copyin {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_COPYIN;
}

reduction {
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_REDUCTION;
}

default {
	BEGIN(openmp_default);
	return OMP_DEFAULT;
}

if {
	// Lexing is the same as a var-list
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_IF;
}

num_threads {
	// Lexing is the same as a var-list
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	return OMP_NUM_THREADS;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom clauses 
	omp_var_list_parentheses = 0;
	BEGIN(openmp_var_list);
	parse_token_text(); 
	return OMP_CUSTOM_CLAUSE; 
}

}

<openmp_default>{

[(] {
		return '(';
}

[)] {
	BEGIN(openmp_clause);
	return ')';
}

shared {
	return OMP_SHARED;
}

none {
	return OMP_NONE;
}

 /* Special case for default custom */
{idnondigit}({idnondigit}|{digit})* {
    parse_token_text();
    return OMP_DEFAULT_CUSTOM;
}

[ \t] { }

}

<openmp_var_list>{

[(] {
	parse_token_text();
	omp_var_list_parentheses++;
	return '(';
}

[)] {
    parse_token_text();
    omp_var_list_parentheses--;
    if (omp_var_list_parentheses == 0)
    {
        BEGIN(openmp_clause);
    }
    return ')';
}

[\n] {
    // This happens only in custom clauses with no parameters at the end of the line
    scanning_now.line_number++;
    BEGIN(INITIAL);
    return OMP_NEWLINE;
}

{idnondigit}({idnondigit}|{digit})* {
	// Special case for custom clauses 
	if (omp_var_list_parentheses == 0)
	{
        // We want to tokenize this one again
		BEGIN(openmp_clause);
        // Note that we are not doing REJECT because we do not want it to be
        // matched below but against under openmp_clause start condition
        yyless(0);
	}
	else
	{
        // Not handled here but below
		REJECT;
	}
}

}

<openmp_schedule>{

[(] {
	parse_token_text();
	omp_schedule_parentheses++;
	return '(';
}

[)] {
	parse_token_text();
	omp_schedule_parentheses--;

	if (omp_schedule_parentheses == 0)
	{
		BEGIN(openmp_clause);
	}

	return ')';
}

static {
	// static is a reserved word in C++
	// We do not need to take additional care
	parse_token_text();
	omp_seen_schedule_kind = 1;
	return OMP_STATIC;
}

dynamic {
	// dynamic is only a schedule-kind after "schedule("
	parse_token_text();
	if (!omp_seen_schedule_kind)
	{
		omp_seen_schedule_kind = 1;
		return OMP_DYNAMIC;
	}
	else
	{
		return IDENTIFIER;
	}
}

guided {
	// guided is only a schedule-kind after "schedule("
	parse_token_text();
	if (!omp_seen_schedule_kind)
	{
		omp_seen_schedule_kind = 1;
		return OMP_GUIDED;
	}
	else
	{
		return IDENTIFIER;
	}
}

runtime {
	// runtime is only a schedule-kind after "schedule("
	parse_token_text();
	if (!omp_seen_schedule_kind)
	{
		omp_seen_schedule_kind = 1;
		return OMP_RUNTIME;
	}
	else
	{
		return IDENTIFIER;
	}
}

{idnondigit}({idnondigit}|{digit})* {
    parse_token_text();
    if (!omp_seen_schedule_kind)
    {
        omp_seen_schedule_kind = 1;
        return OMP_SCHEDULE_CUSTOM;
    }
    else
    {
        return IDENTIFIER;
    }
}

[,] {
	// After the comma, no schedule-kind may appear
	omp_seen_schedule_kind = 1;
	return ',';
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
		*p = *directive;
		p++;
		directive++;
	}

	// Now we have the new number line and the new file name

    // Check include flags from GNU cpp and other preprocessors directive is on
    // the doublequote
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

        char *name_without_path = basename(path);
        strncpy(filename, name_without_path, 255);
    }

    if (start_of_new_file 
            || system_header_file)
    {
        if (include_counter == 0)
        {
            top_level_include_t *new_top_level_include = calloc(1, sizeof(*new_top_level_include));

            seen_filename(filename);
            new_top_level_include->included_file = reference_to_seen_filename(filename);

            if (system_header_file)
            {
                new_top_level_include->system_include = 1;
            }

            P_LIST_ADD(CURRENT_COMPILED_FILE(top_level_include_list),
                    CURRENT_COMPILED_FILE(num_top_level_includes),
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

	// One less line because of the \n
	scanning_now.line_number = (line_num - 1);
	seen_filename(filename);
	scanning_now.current_filename = reference_to_seen_filename(filename);
}

<preprocess>.       { /* ignore line */  }
<preprocess>\n  { scanning_now.line_number++; BEGIN(INITIAL); }

 /* C comments ( 6.4.9 ) */

 /* // comments. // can be splitted across different lines */

"/"(\\\n)*"/"       { 
    scanning_now.line_number += num_new_lines(yytext); 
    BEGIN(linecomment); 
}

 /* escaped new line does not end comment */
<linecomment>\\\n   { scanning_now.line_number++; }
<linecomment>.      { /* ignore comment */  }
<linecomment>\n     { scanning_now.line_number++; BEGIN(INITIAL); }

 /* long C comments. tags can be splitted across diferent lines */

"/"(\\\n)*"*"               { 
    scanning_now.line_number += num_new_lines(yytext);
    BEGIN(longcomment); 
}
<longcomment>\n             { scanning_now.line_number++; }
<longcomment>.              { /* ignore comment */ }
<longcomment>"*"(\\\n)*"/"  { 
    scanning_now.line_number += num_new_lines(yytext);
    BEGIN(INITIAL); 
}


 /* A.2.11 */
"char"          { parse_token_text(); return CHAR; }
 /*!if CPLUSPLUS*/
"wchar_t"       { parse_token_text(); return WCHAR_T; }
"bool"          { parse_token_text(); return BOOL; }
 /*!endif*/
 /*!if C99*/
"_Bool"          { parse_token_text(); return BOOL; }
 /*!endif*/
"short"         { parse_token_text(); return SHORT; }
"int"           { parse_token_text(); return INT; }
"long"          { parse_token_text(); return LONG; }
"signed"        { parse_token_text(); return SIGNED; }
"__signed"        { parse_token_text(); return SIGNED; }
"__signed__"        { parse_token_text(); return SIGNED; }
"unsigned"      { parse_token_text(); return UNSIGNED; }
"float"         { parse_token_text(); return FLOAT; }
"double"        { parse_token_text(); return DOUBLE; }
"void"          { parse_token_text(); return VOID; }

"typedef"       { parse_token_text(); return TYPEDEF; }
"enum"          { parse_token_text(); return ENUM; } 
"struct"        { parse_token_text(); return STRUCT; } 
"auto"          { parse_token_text(); return AUTO; } 
"register"      { parse_token_text(); return REGISTER; } 
"static"        { parse_token_text(); return STATIC; } 
"extern"        { parse_token_text(); return EXTERN; } 
"inline"        { parse_token_text(); return INLINE; } 
"const"         { parse_token_text(); return CONST; } 
"volatile"      { parse_token_text(); return VOLATILE; } 
"union"         { parse_token_text(); return UNION; } 
"asm"           { parse_token_text(); return ASM; } 

 /*!if CPLUSPLUS*/
"explicit"      { parse_token_text(); return EXPLICIT; } 
"private"       { parse_token_text(); return PRIVATE; } 
"protected"     { parse_token_text(); return PROTECTED; } 
"public"        { parse_token_text(); return PUBLIC; } 
"throw"         { parse_token_text(); return THROW; } 
"operator"      { parse_token_text(); return OPERATOR; } 
"new"           { parse_token_text(); return TOKEN_NEW; } 
"delete"        { parse_token_text(); return DELETE; }
"typename"      { parse_token_text(); return TYPENAME; } 
"export"        { parse_token_text(); return EXPORT; } 
"class"         { parse_token_text(); return CLASS; }
"template"      { parse_token_text(); return TEMPLATE; } 
"friend"        { parse_token_text(); return FRIEND; } 
"mutable"       { parse_token_text(); return MUTABLE; } 
"virtual"       { parse_token_text(); return VIRTUAL; } 
"using"         { parse_token_text(); return USING; } 
"namespace"     { parse_token_text(); return NAMESPACE; } 
"this"          { parse_token_text(); return THIS; }
"dynamic_cast"      { parse_token_text(); return DYNAMIC_CAST; }
"static_cast"       { parse_token_text(); return STATIC_CAST; }
"reinterpret_cast"      { parse_token_text(); return REINTERPRET_CAST; }
"const_cast"        { parse_token_text(); return CONST_CAST; }
"typeid"        { parse_token_text(); return TYPEID; }
"try"           { parse_token_text(); return TRY; }
"catch"         { parse_token_text(); return CATCH; }
 /*!endif*/

"sizeof"        { parse_token_text(); return SIZEOF; }
"case"          { parse_token_text(); return CASE; }
"default"       { parse_token_text(); return DEFAULT; }
"if"            { parse_token_text(); return IF; }
"else"          { parse_token_text(); return ELSE; }
"switch"        { parse_token_text(); return SWITCH; }
"while"         { parse_token_text(); return WHILE; }
"do"            { parse_token_text(); return DO; }
"for"           { parse_token_text(); return FOR; }
"break"         { parse_token_text(); return BREAK; }
"continue"      { parse_token_text(); return CONTINUE; }
"goto"          { parse_token_text(); return GOTO; }
"return"        { parse_token_text(); return RETURN; }

 /*!if CPLUSPLUS*/
 /* A.2.41 */
"false"         { parse_token_text(); return BOOLEAN_LITERAL; }
"true"          { parse_token_text(); return BOOLEAN_LITERAL; }
 /*!endif*/

 /*!if CPLUSPLUS*/
 /* Alternative tokens */
"<%" { parse_token_text(); return '{'; }
"%>" { parse_token_text(); return '}'; }
"<:" { parse_token_text(); return '['; }
":>" { parse_token_text(); return ']'; }
"and" { parse_token_text(); return ANDAND; }
"bitor" { parse_token_text(); return '|'; }
"or" { parse_token_text(); return OROR; }
"xor" { parse_token_text(); return '^'; }
"compl" { parse_token_text(); return '~'; }
"bitand" { parse_token_text(); return '&'; }
"and_eq" { parse_token_text(); return AND_ASSIGN; }
"or_eq" { parse_token_text(); return OR_ASSIGN; }
"xor_eq" { parse_token_text(); return XOR_ASSIGN; }
"not" { parse_token_text(); return '!'; }
"not_eq" { parse_token_text(); return NOT_EQUAL; }
 /*!endif*/

 /* GNU Extensions */
"__builtin_va_arg" { parse_token_text(); return BUILTIN_VA_ARG; }
"__extension__" { parse_token_text(); return EXTENSION; }
"__alignof" { parse_token_text(); return ALIGNOF; }
"__alignof__" { parse_token_text(); return ALIGNOF; }
"__ALIGNOF__" { parse_token_text(); return ALIGNOF; }
"__real" { parse_token_text(); return REAL; }
"__real__" { parse_token_text(); return REAL; }
"__imag__" { parse_token_text(); return IMAG; }
"__label__" { parse_token_text(); return LABEL; }
"__complex" { parse_token_text(); return COMPLEX; }
"__complex__" { parse_token_text(); return COMPLEX; }
"_Complex" { parse_token_text(); return COMPLEX; }
 /*!if C99*/
"_Imaginary" { parse_token_text(); return IMAGINARY; }
 /*!endif*/
"typeof" { parse_token_text(); return TYPEOF; }
"__typeof" { parse_token_text(); return TYPEOF; }
"__typeof__" { parse_token_text(); return TYPEOF; }
"restrict" { parse_token_text(); return RESTRICT; /* This should be only for C99 but we will be lax here */ }
"__restrict" { parse_token_text(); return RESTRICT; }
"__restrict__" { parse_token_text(); return RESTRICT; }
"__const" { parse_token_text(); return CONST; }
"__const__" { parse_token_text(); return CONST; }
"__attribute" { parse_token_text(); return ATTRIBUTE; }
"__attribute__" { parse_token_text(); return ATTRIBUTE; }
"__thread" { parse_token_text(); return THREAD; } 
"__inline"        { parse_token_text(); return INLINE; } 
"__inline__"        { parse_token_text(); return INLINE; } 
"__volatile"        { parse_token_text(); return VOLATILE; } 
"__volatile__"        { parse_token_text(); return VOLATILE; } 
"__asm" { parse_token_text(); return ASM; }
"__asm__" { parse_token_text(); return ASM; }

"<?" { parse_token_text(); return MIN_OPERATOR; }
">?" { parse_token_text(); return MAX_OPERATOR; }
"<?=" { parse_token_text(); return MIN_OPERATOR_ASSIGN; }
">?=" { parse_token_text(); return MAX_OPERATOR_ASSIGN; }

 /* mcxx extensions */
"__construct__" { parse_token_text(); return CONSTRUCT; }

 /* Special tokens for subparsing */
@EXPRESSION@ { parse_token_text(); return SUBPARSE_EXPRESSION; }
@STATEMENT@ { parse_token_text(); return SUBPARSE_STATEMENT; }
@DECLARATION@ { parse_token_text(); return SUBPARSE_DECLARATION; }
@TYPE@ { parse_token_text(); return SUBPARSE_TYPE; }
 /*!if CPLUSPLUS*/
@MEMBER@ { parse_token_text(); return SUBPARSE_MEMBER; }
 /*!endif*/
 /* Special tokens for prettyprinted comments and preprocessor elements */
@-C-@[^@]*@-CC-@ { parse_token_text(); return PP_COMMENT; }
@-P-@[^@]*@-PP-@ { parse_token_text(); return PP_TOKEN; }
 /* Special tokens for parsing superscalar related things */
@SUPERSCALAR_DECLARATOR@ { parse_token_text(); return SUBPARSE_SUPERSCALAR_DECLARATOR; }

 /* A plain identifier */
{idnondigit}({idnondigit}|{digit})* {parse_token_text(); return IDENTIFIER; }

 /* A.2.17 */
{nonzero}({digit})*{integersuffix}?     { parse_token_text(); return DECIMAL_LITERAL; }

 /* A.2.18 */
0*{octaldigit}*{integersuffix}?         { parse_token_text(); return OCTAL_LITERAL; }

 /* A.2.19 */
{hexprefix}{hexdigit}+{integersuffix}?  { parse_token_text(); return HEXADECIMAL_LITERAL; }

 /* A.2.26 */
L?'{cchar}*'        { parse_token_text(); return CHARACTER_LITERAL; }

 /* A.2.33 */
{fractconst}{exponent}?{floatingsuffix}?        { parse_token_text();
return FLOATING_LITERAL; }

 /* C99 additional */
{hexprefix}{hexadecimal_fractional_constant}{binary_exponent_part}({floatingsuffix})? { 
    parse_token_text(); 
    return HEXADECIMAL_FLOAT; 
}

{hexprefix}{hexdigit}+{binary_exponent_part}({floatingsuffix})? { 
    parse_token_text(); 
    return HEXADECIMAL_FLOAT; 
}

 /* A.2.39 */
L?\"{schar}*\"      { parse_token_text(); return STRING_LITERAL; }


"["         { parse_token_text(); return '['; }
"]"         { parse_token_text(); return ']'; }
")"         { parse_token_text(); return ')'; }
"("         { parse_token_text(); return '('; }
"->"        { parse_token_text(); return PTR_OP; }
 /*!if CPLUSPLUS*/
"->*"       { parse_token_text(); return PTR_OP_MUL; }
 /*!endif*/
"++"        { parse_token_text(); return PLUSPLUS; }
"--"        { parse_token_text(); return MINUSMINUS; }
"+"         { parse_token_text(); return '+'; }
"-"         { parse_token_text(); return '-'; }
"~"         { parse_token_text(); return '~'; }
"!"         { parse_token_text(); return '!'; }
"/"         { parse_token_text(); return '/'; }
"%"         { parse_token_text(); return '%'; }
"<<"        { parse_token_text(); return LEFT; }
">>"        { parse_token_text(); return RIGHT; }
"<="        { parse_token_text(); return LESS_OR_EQUAL; }
">="        { parse_token_text(); return GREATER_OR_EQUAL; }
"=="        { parse_token_text(); return EQUAL; }
"!="        { parse_token_text(); return NOT_EQUAL; }
"^"         { parse_token_text(); return '^'; }
"|"         { parse_token_text(); return '|'; }
"&&"        { parse_token_text(); return ANDAND; }
"||"        { parse_token_text(); return OROR; }
"="         { parse_token_text(); return '='; }
"*="        { parse_token_text(); return MUL_ASSIGN; }
"/="        { parse_token_text(); return DIV_ASSIGN; }
"%="        { parse_token_text(); return MOD_ASSIGN; }
"+="        { parse_token_text(); return ADD_ASSIGN; }
"-="        { parse_token_text(); return SUB_ASSIGN; }
"<<="       { parse_token_text(); return LEFT_ASSIGN; }
">>="       { parse_token_text(); return RIGHT_ASSIGN; }
"&="        { parse_token_text(); return AND_ASSIGN; }
"^="        { parse_token_text(); return XOR_ASSIGN; }
"|="        { parse_token_text(); return OR_ASSIGN; }



";"         { parse_token_text(); return ';'; } 
":"         { parse_token_text(); return ':'; }
"."         { parse_token_text(); return '.'; }
","         { parse_token_text(); return ','; }
"?"         { parse_token_text(); return '?'; }
"{"         { parse_token_text(); return '{'; }
"}"         { parse_token_text(); return '}'; } 
"<"         { parse_token_text(); return '<'; }
">"         { parse_token_text(); return '>'; } 
"&"         { parse_token_text(); return '&'; }
"*"         { parse_token_text(); return '*'; }
"\""        { parse_token_text(); return '"'; }
 /*!if CPLUSPLUS*/
"::"        { parse_token_text(); return TWO_COLONS; }
 /*!endif*/
"..."       { parse_token_text(); return ELLIPSIS; }

[\n]        { scanning_now.line_number++; }
[ \t\v\f]   {  }
<*>.           { 
    /* ignore bad characters */ 
    if (isgraph(*yytext))
    {
        fprintf(stderr, "Unknown character '%c' (%#x) at %s:%d. Ignoring it.\n", *yytext, *yytext,
				scanning_now.current_filename, scanning_now.line_number);
    }
    else
    {
        fprintf(stderr, "Unknown character %#x at %s:%d. Ignoring it.\n", *yytext,
				scanning_now.current_filename, scanning_now.line_number);
    }
}


%%

static int yywrap(void)
{
    return 1;
}

/*!if CPLUSPLUS*/
#define FLEX_LVAL mcxxlval
/*!endif*/
/*!if C99*/
#define FLEX_LVAL mc99lval
/*!endif*/

static void parse_token_text(void)
{
    FLEX_LVAL.token_atrib.token_line = scanning_now.line_number;
    FLEX_LVAL.token_atrib.token_text = strdup(yytext);
}

/*!if CPLUSPLUS*/
#define OPEN_FILE_FOR_SCANNING mcxx_open_file_for_scanning
#define PREPARE_STRING_FOR_SCANNING mcxx_prepare_string_for_scanning
#define TL_SOURCE_STRING mcxx_tl_source_string
/*!endif*/
/*!if C99*/
#define OPEN_FILE_FOR_SCANNING mc99_open_file_for_scanning
#define PREPARE_STRING_FOR_SCANNING mc99_prepare_string_for_scanning
#define TL_SOURCE_STRING mc99_tl_source_string
/*!endif*/

char* TL_SOURCE_STRING = "(tl-source-string)";

int PREPARE_STRING_FOR_SCANNING(const char* str)
{
    static int num_string = 0;
	DEBUG_CODE()
	{
		fprintf(stderr, "Going to parse string '%s'\n", str);
	}
	memset(&scanning_now, 0, sizeof(scanning_now));
	scanning_now.line_number = 1;

	const char* current_filename = CURRENT_COMPILED_FILE(input_filename);

	scanning_now.filename = calloc(strlen(TL_SOURCE_STRING) 
			+ strlen(current_filename) + 10, sizeof(char));
    sprintf(scanning_now.filename, "%s%s-%d", TL_SOURCE_STRING, current_filename, num_string);

    scanning_now.current_filename = scanning_now.filename;

    num_string++;

	scanning_now.scanning_buffer = yy_scan_string(str);

	// yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_switch_to_buffer(scanning_now.scanning_buffer);

	return 0;
}

int OPEN_FILE_FOR_SCANNING(char* scanned_filename, char* input_filename)
{
	FILE* file;

	file = fopen(scanned_filename, "r");

	if (file == NULL)
	{
		running_error("Cannot open file '%s' (%s)", scanned_filename, strerror(errno));
	}

	memset(&scanning_now, 0, sizeof(scanning_now));
	scanning_now.filename = strdup(scanned_filename);
	scanning_now.file_descriptor = file;
	scanning_now.line_number = 1;

	seen_filename(input_filename);
	main_input_filename = reference_to_seen_filename(input_filename);
    scanning_now.current_filename = main_input_filename;

	scanning_now.scanning_buffer = yy_create_buffer(file, YY_BUF_SIZE);

	// yy_flush_buffer(YY_CURRENT_BUFFER);
	yy_switch_to_buffer(scanning_now.scanning_buffer);

	return 0;
}
