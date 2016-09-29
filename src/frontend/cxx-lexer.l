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
#include "cxx-diagnostic.h"
#include "cxx-ast.h"
#include "cxx-exprtype.h"
/*!if C99*/
#include "c99-parser-internal.h"
/*!endif*/
/*!if CPLUSPLUS*/
#include "cxx-parser-internal.h"
/*!endif*/

#include "mem.h"

typedef 
struct lexer_keyword_tag
{
    const char* keyword;
    int token;
} lexer_keyword_t;

/*!if CPLUSPLUS*/
/* g++ 4.3 extensions */
static lexer_keyword_t gxx_43_keywords[] =
{
    { "__has_nothrow_assign", GXX_HAS_NOTHROW_ASSIGN },
    { "__has_nothrow_constructor", GXX_HAS_NOTHROW_CONSTRUCTOR },
    { "__has_nothrow_copy", GXX_HAS_NOTHROW_COPY },
    { "__has_trivial_assign", GXX_HAS_TRIVIAL_ASSIGN },
    { "__has_trivial_constructor", GXX_HAS_TRIVIAL_CONSTRUCTOR },
    { "__has_trivial_copy", GXX_HAS_TRIVIAL_COPY },
    { "__has_trivial_destructor", GXX_HAS_TRIVIAL_DESTRUCTOR },
    { "__has_virtual_destructor", GXX_HAS_VIRTUAL_DESTRUCTOR },
    { "__is_abstract", GXX_IS_ABSTRACT },
    { "__is_base_of", GXX_IS_BASE_OF },
    { "__is_class", GXX_IS_CLASS },
    { "__is_convertible_to", GXX_IS_CONVERTIBLE_TO },
    { "__is_empty", GXX_IS_EMPTY },
    { "__is_enum", GXX_IS_ENUM },
    { "__is_literal_type", GXX_IS_LITERAL_TYPE },
    { "__is_pod", GXX_IS_POD },
    { "__is_polymorphic", GXX_IS_POLYMORPHIC },
    { "__is_standard_layout", GXX_IS_STANDARD_LAYOUT },
    { "__is_trivial", GXX_IS_TRIVIAL },
    { "__is_union", GXX_IS_UNION },
    { "__is_final", GXX_IS_FINAL },
    { "__underlying_type", GXX_UNDERLYING_TYPE },
    { "__is_trivially_assignable", GXX_IS_TRIVIALLY_ASSIGNABLE },
    { "__is_trivially_constructible", GXX_IS_TRIVIALLY_CONSTRUCTIBLE },
    { "__is_trivially_copyable", GXX_IS_TRIVIALLY_COPYABLE },
    // Sentinel
    { NULL, 0 },
};
/*!endif*/

/*!if C99*/
static lexer_keyword_t upc_keywords[] =
{
    // It is easier to sign these in as normal builtins
    // { "MYTHREAD", UPC_MYTHREAD },
    // { "THREADS", UPC_THREADS },
    // { "UPC_MAX_BLOCKSIZE", UPC_MAX_BLOCKSIZE },
    { "relaxed", UPC_RELAXED },
    { "shared", UPC_SHARED },
    { "strict", UPC_STRICT },
    { "upc_barrier", UPC_BARRIER },
    { "upc_blocksizeof", UPC_BLOCKSIZEOF },
    { "upc_elemsizeof", UPC_ELEMSIZEOF },
    { "upc_fence", UPC_FENCE },
    { "upc_forall", UPC_FORALL },
    { "upc_localsizeof", UPC_LOCALSIZEOF },
    { "upc_notify", UPC_NOTIFY },
    { "upc_wait", UPC_WAIT },
    // Sentinel
    { NULL, 0 }
};
/*!endif*/

static lexer_keyword_t cuda_keywords[] =
{
    {"__device__" , CUDA_DEVICE},
    {"__global__", CUDA_GLOBAL},
    {"__host__", CUDA_HOST},
    {"__constant__", CUDA_CONSTANT},
    {"__shared__", CUDA_SHARED},
    // Sentinel
    { NULL, 0 }
};

static lexer_keyword_t opencl_keywords[] =
{
    {"__kernel" , OPENCL_KERNEL},
    {"__global", OPENCL_GLOBAL},
    {"__constant", OPENCL_CONSTANT},
    {"__local", OPENCL_LOCAL},
    // Sentinel
    { NULL, 0 }
};

static lexer_keyword_t ms_builtin_types[] =
{
    {"__int8" ,  MS_INT8},
    {"__int16" , MS_INT16},
    {"__int32" , MS_INT32},
    {"__int64" , MS_INT64},
    // Sentinel
    { NULL, 0 }
};

static lexer_keyword_t intel_builtins_syntax[] =
{
    {"__assume", INTEL_ASSUME},
    {"__assume_aligned", INTEL_ASSUME_ALIGNED},
    // Sentinel
    { NULL, 0 }
};

static int yywrap(void);

static void update_location_str(const char*);
static void update_location();

static void parse_token_text_str(const char*);
static void parse_token_text(void);

static int lookup_keyword_in_table(lexer_keyword_t *keyword_table, const char* keyword, char predicate);

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
};

static struct scan_file_descriptor scanning_now;

static const char* main_input_filename = NULL;

static int pragma_custom_var_list_parentheses = 0;

static const char *current_pragma_prefix = NULL;

static int include_counter = 0;

// static int verbatim_buffer_size = 0;
// static const char *verbatim_buffer = NULL;

static void verbatim_buffer_append(const char* c);
static void verbatim_buffer_free(void);
static const char* verbatim_buffer_get(void);

static const char* return_pragma_prefix_longest_match(const char* prefix, 
        const char* lexed_directive,
        pragma_directive_kind_t* kind,
        char* original_directive);

static char* inline_pragma_str = NULL;

 /*!if CPLUSPLUS*/
static int std_attribute_parenthesis_nesting = 0;

static int raw_string_buffer_capacity = 0;
static int raw_string_buffer_position = 0;
static char* raw_string_buffer = NULL;
static const char* raw_string_delimiter = NULL;
static int raw_string_delimiter_length = 0;
static int raw_string_delimiter_index = 0;

#define RAW_STRING_APPEND_CHAR(_x) \
{ \
    if (raw_string_buffer_position == raw_string_buffer_capacity) \
    { \
        if (raw_string_buffer_capacity == 0) \
          raw_string_buffer_capacity = 4; \
        else \
          raw_string_buffer_capacity *= 2; \
        raw_string_buffer = NEW_REALLOC(char, raw_string_buffer, raw_string_buffer_capacity); \
    } \
    raw_string_buffer[raw_string_buffer_position] = (_x); \
    raw_string_buffer_position++; \
}

 /*!endif*/

static void unput_string(const char* c);

%}

%option noyyalloc noyyrealloc noyyfree noinput

 /* Lexical conventions A.2 */
 /* A.2.1 */
hexquad         {hexdigit}{hexdigit}{hexdigit}{hexdigit}
 /* A.2.2 */
uchar           (\\u{hexquad}|\\U{hexquad}{hexquad})
 /* A.2.12 */
nondigit        [_a-zA-Z]
 /* A.2.13 */
digit_0     [0-9]
 /*!if CPLUSPLUS*/
digit       (({digit_0})|({digit_0}[']{digit_0}))
 /*!endif*/
 /*!if C99*/
digit       {digit_0}
 /*!endif*/
 /* A.2.17 */
 /*!if CPLUSPLUS*/
decimal_literal ({nonzero}({digit})*)|({nonzero}[']({digit})*)
 /*!endif*/
 /*!if C99*/
decimal_literal ({nonzero}({digit})*)
 /*!endif*/
 /* A.2.18 */
 /*!if CPLUSPLUS */
octal_literal (0{octaldigit}*)|(0[']{octaldigit}*)
 /*!endif*/
 /*!if C99 */
octal_literal (0{octaldigit}*)
 /*!endif*/
 /* A.2.19 */
hexadecimal_literal ({hexprefix}{hexdigit}+)
 /* For C and C++ < C++14, this is a GCC extension of the language
    For C++ >= C++14, this is a feature of the language */
binary_literal ({binprefix}{bindigit}+)
 /* A.2.20 */
nonzero         [1-9]
 /* A.2.21 */
octaldigit_0      [0-7]
 /*!if CPLUSPLUS*/
octaldigit     ({octaldigit_0})|({octaldigit_0}[']{octaldigit_0})
 /*!endif*/
 /*!if C99*/
octaldigit     ({octaldigit_0})
 /*!endif*/
 /* A.2.22 */
hexdigit_0        [a-fA-F0-9]
 /*!if CPLUSPLUS*/
hexdigit     ({hexdigit_0})|({hexdigit_0}[']{hexdigit_0})
 /*!endif*/
 /*!if C99*/
hexdigit     ({hexdigit_0})
 /*!endif*/
bindigit_0   [01]
 /*!if C99*/
bindigit     {bindigit_0}
 /*!endif*/
 /*!if CPLUSPLUS*/
bindigit     ({bindigit_0})|({bindigit_0}[']{bindigit_0})
 /*!endif*/

 /* A.2.23 includes GNU extension of long long */
int_suffix_size ({longsuffix}|{longlongsuffix})
int_suffix_sign ({unsignedsuffix})

 /* GCC is very flexible with suffixes */
integersuffix_1   ({int_suffix_size}(({int_suffix_sign}{complexsuffix}?)|({complexsuffix}{int_suffix_sign}?))?)
integersuffix_2   ({int_suffix_sign}(({int_suffix_size}{complexsuffix}?)|({complexsuffix}{int_suffix_size}?))?)
integersuffix_3   ({complexsuffix}(({int_suffix_size}{int_suffix_sign}?)|({int_suffix_sign}{int_suffix_size}?))?)

integersuffix ({integersuffix_1}|{integersuffix_2}|{integersuffix_3})

 /* A.2.24 */
unsignedsuffix  [uU]
 /* A.2.25 */
longsuffix  [lL]

 /* GNU Extension */
longlongsuffix [lL][lL]

 /* Complex GNU Extension */
complexsuffix [ji]

 /* A.2.28 */
cchar       (([^'\\\n])|(\\['])|(\\\\)|(\\[^'\\\n]+))

 /* universal4 (\\u[0-9a-fA-F]{4}) */
 /* universal8 (\\U[0-9a-fA-F]{8}) */

 /* universal ({universal4}|{universal8}) */

 /* A.2.29 */
 /* \e is a GNU extension */
 /* escape_seq      ((\\["'?eabfnrtv\\])|(\\{octaldigit}{1,3})|((\\x)+{hexdigit}+)|{universal}) */
 /* A.2.34 */
fractconst  (({digit}*\.{digit}+)|({digit}+(\.)?))
 /* A.2.35 */
exponent    ([eE][+-]?{digit}+)
 /* A.2.38 */
floatingsuffix_std  [flFL]
 /* Q is a GCC extension for float128 */
floatingsuffix_nonstd  [qQ]

floatingsuffix_all ({floatingsuffix_std}|{floatingsuffix_nonstd})

 /* GNU extension for complex floating suffix */
floatingsuffix  ({floatingsuffix_all}|{floatingsuffix_all}{complexsuffix}|{complexsuffix}{floatingsuffix_all}|{complexsuffix})
 /* A.2.40 */
schar       (([^"\\\n])|(\\["])|(\\\\)|(\\[^"\\\n]+))

dchar [^ ()\n\v\t\r]
rchar [^)]

idnondigit  ({nondigit}|{uchar})
hexprefix   0[xX]
binprefix   0[bB]

newline ([\n]|[\r]|([\r][\n]))
notnewline ([^\n\r])

blanksinline [ \t\v\f]

whitespace  (({newline})|({blanksinline}))

 /* C99 additional */
hexadecimal_fractional_constant (({hexdigit}*\.{hexdigit}+)|({hexdigit}\.))
binary_exponent_part ([pP]([+-])?{digit}+)

 /* Identifier */
identifier {idnondigit}({idnondigit}|{digit})* 

 /* states */
%x longcomment
%x linecomment
%x preprocess
%x pragma_line
%x inline_pragma
%x inline_pragma_after_string
%x unknown_pragma
%x gcc_pragma
%x pragma_custom_directive
%x pragma_custom_clause
%x pragma_custom_clause_first
%x pragma_custom_var_list

%x verbatim_text
%x verbatim_clauses
%x verbatim_clauses_args

%x std_attribute
%x std_attribute_clause

%x raw_string_body
%x raw_string_potential_suffix

%%
 /* preprocess */

^[ \t]*# { 
    update_location();
    BEGIN(preprocess); 
}

<preprocess>[ ]*pragma[ ]+ {
    update_location();
	BEGIN(pragma_line);
}

<pragma_line>mcc[ ]+enable_debug[ ]*{newline} {
    // This is only for debugging Mercurium itself
    extern void _enable_debug(void);
    _enable_debug();
    update_location();
    // Do not return anything
    BEGIN(INITIAL);
}

<pragma_line>mcc[ ]+disable_debug[ ]*{newline} {
    // This is only for debugging Mercurium itself
    extern void _disable_debug(void);
    _disable_debug();
    update_location();
    // Do not return anything
    BEGIN(INITIAL);
}

<pragma_line>((mcxx)|(mcc))[ ]+verbatim[ ]+start[ ]* {
    BEGIN(verbatim_clauses);
    parse_token_text();
    update_location();
    return VERBATIM_PRAGMA;
}

<verbatim_clauses>{
    
type {
    parse_token_text();
    update_location();
    BEGIN(verbatim_clauses_args);
    return VERBATIM_TYPE;
}

[ \t] {
    update_location();
}

{newline} {
    BEGIN(verbatim_text);
    update_location();
}

}

<verbatim_clauses_args>{

[(] {
    parse_token_text();
    update_location();
    return '(';
}

[)] {
    parse_token_text();
    update_location();
    BEGIN(verbatim_clauses);
    return ')';
}

[ \t] { }

{identifier} { 
    parse_token_text();
    update_location();
    return IDENTIFIER;
}

{newline} {
    BEGIN(verbatim_text);
    update_location();
}

}

<verbatim_text>{

{notnewline} {
    verbatim_buffer_append(yytext);
    update_location();
}

{newline} {
    verbatim_buffer_append(yytext);
    update_location();
}

#pragma[ ]+((mcxx)|(mcc))[ ]+verbatim[ ]+end[ ]*{newline} {
    BEGIN(INITIAL);

    parse_token_text_str(verbatim_buffer_get());
    verbatim_buffer_free();
    
    update_location();
    return VERBATIM_TEXT;
}

}

<pragma_line>{identifier} {
	// Custom pragmas
	int i;
	char found = 0;

	char matched = 0;

	// Ignore gcc and ident
    if (strcasecmp(yytext, "gcc") == 0)
    {
        // All the whole line may be handled as an unknown pragma
		BEGIN(gcc_pragma);
        // current yytext must be included in the "unknown pragma"
		unput_string(yytext);
		matched = 1;
    }
    else if (strcasecmp(yytext, "ident") == 0)
	{
        // All the whole line will be handled as an unknown pragma
		BEGIN(unknown_pragma);
        // current yytext must be included in the "unknown pragma"
		unput_string(yytext);
		matched = 1;
	}

	if (!matched)
	{
		for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
		{
			if (strcmp(yytext, CURRENT_CONFIGURATION->pragma_custom_prefix[i]) == 0)
            {
                found = 1;
                break;
            }
		}

		if (!found)
		{
			warn_printf_at(make_locus(
					scanning_now.current_filename,
					scanning_now.line_number,
					scanning_now.column_number),
                    "unknown '#pragma %s'\n", 
					yytext);
			BEGIN(unknown_pragma);
			// Ignore this prefix as an unknown pragma
			unput_string(yytext);
		}
		else
		{
			BEGIN(pragma_custom_directive);
			parse_token_text();
            update_location();
            current_pragma_prefix = CURRENT_CONFIGURATION->pragma_custom_prefix[i];
			return PRAGMA_CUSTOM;
		}
	}
}

<gcc_pragma>{
  /* This will catch #pragma gcc */

{notnewline}* {

    char ignore_gcc_header = 0;

    const char* p = yytext;
    // Skip blanks
    while (*p == ' ' || *p == '\t')
        p++;

    // Now skip gcc|GCC
    if (strncasecmp(p, "gcc", 3) == 0)
    {
        p += strlen("gcc");

        // Skip blanks again
        while (*p == ' ' || *p == '\t')
            p++;

        const char* system_header_str = "system_header";
        if (strncmp(p, system_header_str, strlen(system_header_str)) == 0)
        {
            p += strlen(system_header_str);
            // Whitespace
            if (*p == '\0'
                    || *p == ' '
                    || *p == '\t')
            {
                // This is a #pragma GCC system_header
                ignore_gcc_header = 1;
            }
        }
    }

    if (!ignore_gcc_header)
    {
        parse_token_text();
        update_location();
        BEGIN(preprocess);
        return UNKNOWN_PRAGMA;
    }
    else
    {
        update_location();
    }
}

{newline} {

    // Degenerate case. This either '#pragma gcc' probably due to wrong input or a 
    // skipped "#pragma GCC system_header"

    // Do not lose track of the line
    unput_string(yytext);

	BEGIN(INITIAL);
}

}

<unknown_pragma>{
  /* Besides unknown pragmas this will also catch #pragma ident */

{notnewline}* {
	parse_token_text();
    update_location();
	BEGIN(preprocess);
	return UNKNOWN_PRAGMA;
}

{newline} {
	parse_token_text_str("");
    unput_string(yytext);

	BEGIN(INITIAL);
	return UNKNOWN_PRAGMA;
}

}

<pragma_custom_directive>{

{identifier}([ \t]+{identifier})* {
	BEGIN(pragma_custom_clause_first);

    pragma_directive_kind_t directive_kind = PDK_NONE; 

    char original_directive[strlen(yytext)];
    memset(original_directive, 0, sizeof(original_directive));

    // This call will unput appropriately what was lexed too much
    /* const char* longest_match = */ return_pragma_prefix_longest_match(
            current_pragma_prefix, yytext, &directive_kind, original_directive);

    int line_number = scanning_now.line_number;
    int column_number = scanning_now.column_number;
    if (directive_kind == PDK_NONE)
    {
        // Give a chance to the empty "directive" since some ill-designed
        // pragmas might need this
        directive_kind = lookup_pragma_directive(current_pragma_prefix, "");
        parse_token_text();
        update_location();
    }
    else
    {
        parse_token_text_str(original_directive);
        update_location_str(original_directive);
    }


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
                fatal_printf_at(
                        make_locus(
                            scanning_now.current_filename,
                            line_number,
                            column_number),
                        "unknown directive '%s' for pragma '%s'. Maybe you forgot to register it?",
                        yytext,
                        current_pragma_prefix);
            }
        default:
            internal_error("Invalid pragma directive kind kind=%d", directive_kind);
    }

    current_pragma_prefix = NULL;
    return token;
}

{newline} {
    // This will cause a fail in the parser, but better this way than a silent \n mislexed
    update_location();
	BEGIN(INITIAL);
	return PRAGMA_CUSTOM_NEWLINE;
}

[ \t] {
	// This blank must be eaten and should be mandatory
    update_location();
}

}

<pragma_custom_clause_first>{

{newline} {
	BEGIN(INITIAL);
    update_location();
	return PRAGMA_CUSTOM_NEWLINE;
}

{identifier} {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text(); 
    update_location();
	return PRAGMA_CUSTOM_CLAUSE; 
}

[ \t] {
	// This blank must be eaten
    update_location();
}

[(] {
        // Parameter
	pragma_custom_var_list_parentheses = 1;
    BEGIN(pragma_custom_var_list);
    update_location();
    return '(';
}

}

<pragma_custom_clause>{

{newline} {
	BEGIN(INITIAL);
    update_location();
	return PRAGMA_CUSTOM_NEWLINE;
}

{identifier} {
	// Special case for custom clauses
	BEGIN(pragma_custom_var_list);
	parse_token_text(); 
    update_location();
	return PRAGMA_CUSTOM_CLAUSE; 
}

[ \t] {
	// This blank must be eaten
    update_location();
}

}

<pragma_custom_var_list>{

[(] {
	parse_token_text();
    update_location();
	if (pragma_custom_var_list_parentheses == 0)
    {
        pragma_custom_var_list_parentheses++;
        return '(';
    }
    else
    {
        pragma_custom_var_list_parentheses++;
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
}

[)] {
	parse_token_text();
    update_location();
	pragma_custom_var_list_parentheses--;
	if (pragma_custom_var_list_parentheses == 0)
	{
		BEGIN(pragma_custom_clause);
        return ')';
	}
    else
    {
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
}

[ \t]+ {
    // Ignore the blank if we are not yet within any parentheses
	if (pragma_custom_var_list_parentheses > 0)
    {
        parse_token_text();
        update_location();
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
    update_location();
}

{identifier} {
    if (pragma_custom_var_list_parentheses > 0)
    {
        parse_token_text();
        update_location();
        return PRAGMA_CLAUSE_ARG_TEXT;
    }
    else
    {
        // We are in .
        // #pragma foo bar . doe
        // and 'doe' must be lexed as another custom clause
        BEGIN(pragma_custom_var_list);
        parse_token_text(); 
        update_location();
        return PRAGMA_CUSTOM_CLAUSE; 
    }
}

{newline} {
    // We found a newline
    if (pragma_custom_var_list_parentheses != 0)
    {
        // Well, we found the newline but parentheses were not 
        // properly nested
        warn_printf_at(make_locus(
                scanning_now.current_filename,
                scanning_now.line_number,
                scanning_now.column_number),
                "unended custom clause. Skipping.\n");
    }
    // Head back to initial state and finish this pragma
    BEGIN(INITIAL);
    update_location();
    return PRAGMA_CUSTOM_NEWLINE;
}

  /* Catch all rule */
[^\n] {
    parse_token_text();
    update_location();
    return PRAGMA_CLAUSE_ARG_TEXT;
}

}

 /* _Pragma */
 /* Note: we do not update location when lexing here
    because we will reintroduce a #pragma */
"_Pragma"[ \t]*"("[ \t]* {
        BEGIN(inline_pragma);
        DELETE(inline_pragma_str);
        inline_pragma_str = NULL;
}

<inline_pragma>{

\"{schar}*\" {
    inline_pragma_str = interpret_schar(yytext,
            make_locus(
                uniquestr(scanning_now.current_filename),
                scanning_now.line_number, 0));

    BEGIN(inline_pragma_after_string);
}

}

<inline_pragma_after_string>{
[\t ]*")" {
    if (inline_pragma_str != NULL)
    {
        char whole_pragma_line[MCXX_MAX_FILENAME];
        int s = snprintf(whole_pragma_line,
                MCXX_MAX_FILENAME - 1,
                "%s\n# %d \"%s\"\n",
                inline_pragma_str,
                scanning_now.line_number,
                scanning_now.current_filename);
        if (s == MCXX_MAX_FILENAME - 1)
        {
            internal_error("Inline pragma is too long (> %d) at %s:%d:%d",
                    MCXX_MAX_FILENAME,
                    scanning_now.current_filename,
                    scanning_now.line_number,
                    scanning_now.column_number);
        }
        whole_pragma_line[MCXX_MAX_FILENAME - 1] = '\0';

        unput_string(whole_pragma_line);
        BEGIN(pragma_line);
    }
    else
    {
        BEGIN(INITIAL);
    }
}
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
 unsigned int line_num = 0;
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
 // There are no zero lines, so if for some reason we end with a 0 line, fix it to 1
 line_num = line_num == 0 ? 1 : line_num;

   // Update the line number, note that it is line_num - 1 
   // because \n is not handled here, but in another rule
 scanning_now.line_number = (line_num - 1);
 scanning_now.column_number = 1;

 // Jump blanks (if any)
 while (*p == ' ')
   p++;

 // If there is a filename, parse it
 if (*p == '"')
 {
     // Now we are on the quote "
     p++;
 
     char filename[MCXX_MAX_FILENAME];
 
     char *f = filename;
 
     while (*p != '"')
     {
         if (f < &(filename[MCXX_MAX_FILENAME - 1]))
         {
             (*f) = *p;
             f++;
         }
         p++;
     }
     *f = '\0';

     // Update the file 
	scanning_now.current_filename = uniquestr(filename);
 }
}

<preprocess>[ ]{digit}+[ ]+["][^"]+["][^\n]* {
	char* directive = yytext;

	// Jump the blank
	directive++;

	unsigned int line_num = 0;
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

	char filename[MCXX_MAX_FILENAME];

	char* p = filename;

	while (*directive != '"')
	{
        if (p < &(filename[MCXX_MAX_FILENAME-1]))
        {
            *p = *directive;
            p++;
        }
		directive++;
	}
    *p = '\0';

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
    char extern_c_block UNUSED_PARAMETER = 1;
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

#if 0
    CXX_LANGUAGE()
    {
        if (extern_c_block)
        {
            warn_printf(
                    make_locus(
                    scanning_now.current_filename,
                    scanning_now.line_number,
                    scanning_now.column_number),
                    "preprocessor line marking an implicit 'extern C' block. This is not fully supported\n");
        }
    }
#endif

    if (system_header_file)
    {
        char path[MCXX_MAX_FILENAME];
        strncpy(path, filename, MCXX_MAX_FILENAME-1);

        char *name_without_path = basename(path);
        strncpy(filename, name_without_path, MCXX_MAX_FILENAME);
    }

    if (start_of_new_file 
            || system_header_file)
    {
        if (include_counter == 0)
        {
            top_level_include_t *new_top_level_include = NEW0(top_level_include_t);

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
	scanning_now.line_number = (line_num - 1);
	scanning_now.column_number = 1;
    // Update file
	scanning_now.current_filename = uniquestr(filename);
}

<preprocess>.       { update_location(); /* ignore line */  }
<preprocess>\n  { update_location(); BEGIN(INITIAL); }

 /* C comments ( 6.4.9 ) */

 /* // comments. // can be splitted across different lines */

"/"(\\\n)*"/"       { 
    update_location();
    BEGIN(linecomment); 
}

 /* escaped new line does not end comment */
<linecomment>\\\n   { update_location(); }
<linecomment>(([^\])|([\][^\n]))+      { update_location(); }
<linecomment>\n     { update_location(); BEGIN(INITIAL); }

 /* long C comments. tags can be splitted across diferent lines */

"/"(\\\n)*"*"               { 
    update_location();
    BEGIN(longcomment); 
}
<longcomment>\n             { update_location(); }
<longcomment>(([^*])|("*"(\\\n)*[^/]))+  { update_location(); }
<longcomment>"*"(\\\n)*"/"  { 
    update_location();
    BEGIN(INITIAL); 
}


 /* A.2.11 */
"char"          { parse_token_text(); update_location(); return TOKEN_CHAR; }
 /*!if CPLUSPLUS*/
"wchar_t"       { parse_token_text(); update_location(); return TOKEN_WCHAR_T; }
"bool"          { parse_token_text(); update_location(); return TOKEN_BOOL; }
"char16_t"      { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_CHAR16_T; else return IDENTIFIER; }
"char32_t"      { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_CHAR32_T; else return IDENTIFIER; }
 /*!endif*/
 /*!if C99*/
"_Bool"          { parse_token_text(); update_location(); return TOKEN_BOOL; }
 /*!endif*/
"short"         { parse_token_text(); update_location(); return TOKEN_SHORT; }
"int"           { parse_token_text(); update_location(); return TOKEN_INT; }
"long"          { parse_token_text(); update_location(); return TOKEN_LONG; }
"signed"        { parse_token_text(); update_location(); return TOKEN_SIGNED; }
"__signed"        { parse_token_text(); update_location(); return TOKEN_SIGNED; }
"__signed__"        { parse_token_text(); update_location(); return TOKEN_SIGNED; }
"unsigned"      { parse_token_text(); update_location(); return TOKEN_UNSIGNED; }
"float"         { parse_token_text(); update_location(); return TOKEN_FLOAT; }
"double"        { parse_token_text(); update_location(); return TOKEN_DOUBLE; }
"void"          { parse_token_text(); update_location(); return TOKEN_VOID; }

"typedef"       { parse_token_text(); update_location(); return TYPEDEF; }
"enum"          { parse_token_text(); update_location(); return ENUM; } 
"struct"        { parse_token_text(); update_location(); return STRUCT; } 
 /*!if C99*/
"auto"          { parse_token_text(); update_location(); return TOKEN_AUTO_STORAGE; }
 /*!endif*/
 /*!if CPLUSPLUS*/
"auto"          { parse_token_text();
                  update_location();
                  if (IS_CXX11_LANGUAGE)
                      return TOKEN_AUTO_TYPE;
                  else
                      return TOKEN_AUTO_STORAGE;
                } 
 /*!endif*/
"register"      { parse_token_text(); update_location(); return REGISTER; } 
"static"        { parse_token_text(); update_location(); return STATIC; } 
"extern"        { parse_token_text(); update_location(); return EXTERN; } 
"inline"        { parse_token_text(); update_location(); return INLINE; } 
"const"         { parse_token_text(); update_location(); return TOKEN_CONST; } 
"volatile"      { parse_token_text(); update_location(); return TOKEN_VOLATILE; } 
"union"         { parse_token_text(); update_location(); return UNION; } 
"asm"           { parse_token_text(); update_location(); return ASM; } 

 /*!if CPLUSPLUS*/
"decltype"      { parse_token_text(); update_location(); return DECLTYPE; }
"__decltype"    { parse_token_text(); update_location(); return DECLTYPE; }
"explicit"      { parse_token_text(); update_location(); return EXPLICIT; } 
"private"       { parse_token_text(); update_location(); return PRIVATE; } 
"protected"     { parse_token_text(); update_location(); return PROTECTED; } 
"public"        { parse_token_text(); update_location(); return PUBLIC; } 
"throw"         { parse_token_text(); update_location(); return THROW; } 
"operator"      { parse_token_text(); update_location(); return OPERATOR; } 
"operator"[ \t\n]*"\"\""  { parse_token_text(); update_location(); return OPERATOR_LITERAL; }
"new"           { parse_token_text(); update_location(); return TOKEN_NEW; } 
"new"[ \t\n]*"["[ \t\n]*"]" { parse_token_text(); update_location(); return TOKEN_NEW_ARRAY; }
"delete"        { parse_token_text(); update_location(); return TOKEN_DELETE; }
"delete"[ \t\n]*"["[ \t\n]*"]" { parse_token_text(); update_location(); return TOKEN_DELETE_ARRAY; }
"typename"      { parse_token_text(); update_location(); return TYPENAME; } 
"export"        { parse_token_text(); update_location(); return EXPORT; } 
"class"         { parse_token_text(); update_location(); return CLASS; }
"template"      { parse_token_text(); update_location(); return TEMPLATE; } 
"friend"        { parse_token_text(); update_location(); return FRIEND; } 
"mutable"       { parse_token_text(); update_location(); return MUTABLE; } 
"virtual"       { parse_token_text(); update_location(); return VIRTUAL; } 
"using"         { parse_token_text(); update_location(); return USING; } 
"namespace"     { parse_token_text(); update_location(); return NAMESPACE; } 
"this"          { parse_token_text(); update_location(); return TOKEN_THIS; }
"dynamic_cast"      { parse_token_text(); update_location(); return DYNAMIC_CAST; }
"static_cast"       { parse_token_text(); update_location(); return STATIC_CAST; }
"reinterpret_cast"      { parse_token_text(); update_location(); return REINTERPRET_CAST; }
"const_cast"        { parse_token_text(); update_location(); return CONST_CAST; }
"typeid"        { parse_token_text(); update_location(); return TYPEID; }
"try"           { parse_token_text(); update_location(); return TRY; }
"catch"         { parse_token_text(); update_location(); return CATCH; }

"constexpr"     { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_CONSTEXPR; else return IDENTIFIER; }
"thread_local"  { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_THREAD_LOCAL; else return IDENTIFIER; }
"noexcept"/[ \t\n]*[(] {
                  parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_NOEXCEPT; else return IDENTIFIER; }
"noexcept"      { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_NOEXCEPT_ALONE; else return IDENTIFIER; }
"nullptr"       { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_NULLPTR; else return IDENTIFIER; }
 /*!endif*/

"sizeof"        { parse_token_text(); update_location(); return SIZEOF; }
"case"          { parse_token_text(); update_location(); return CASE; }
"default"       { parse_token_text(); update_location(); return DEFAULT; }
"if"            { parse_token_text(); update_location(); return IF; }
"else"          { parse_token_text(); update_location(); return ELSE; }
"switch"        { parse_token_text(); update_location(); return SWITCH; }
"while"         { parse_token_text(); update_location(); return WHILE; }
"do"            { parse_token_text(); update_location(); return DO; }
"for"           { parse_token_text(); update_location(); return FOR; }
"break"         { parse_token_text(); update_location(); return BREAK; }
"continue"      { parse_token_text(); update_location(); return CONTINUE; }
"goto"          { parse_token_text(); update_location(); return GOTO; }
"return"        { parse_token_text(); update_location(); return RETURN; }

 /*!if CPLUSPLUS*/
 /* A.2.41 */
"false"         { parse_token_text(); update_location(); return BOOLEAN_LITERAL; }
"true"          { parse_token_text(); update_location(); return BOOLEAN_LITERAL; }
 /*!endif*/

 /*!if CPLUSPLUS*/
 /* C++2011 keywords that are not reserved words */
"final"         { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOK_FINAL; else return IDENTIFIER; }
"override"      { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOK_OVERRIDE; else return IDENTIFIER; }
 /*!endif*/

 /*!if CPLUSPLUS*/
 /* Alternative tokens */
"<%" { parse_token_text(); update_location(); return '{'; }
"%>" { parse_token_text(); update_location(); return '}'; }
"<:" { parse_token_text(); update_location(); return '['; }
":>" { parse_token_text(); update_location(); return ']'; }
"and" { parse_token_text(); update_location(); return ANDAND; }
"bitor" { parse_token_text(); update_location(); return '|'; }
"or" { parse_token_text(); update_location(); return OROR; }
"xor" { parse_token_text(); update_location(); return '^'; }
"compl" { parse_token_text(); update_location(); return '~'; }
"bitand" { parse_token_text(); update_location(); return '&'; }
"and_eq" { parse_token_text(); update_location(); return AND_ASSIGN; }
"or_eq" { parse_token_text(); update_location(); return OR_ASSIGN; }
"xor_eq" { parse_token_text(); update_location(); return XOR_ASSIGN; }
"not" { parse_token_text(); update_location(); return '!'; }
"not_eq" { parse_token_text(); update_location(); return NOT_EQUAL; }
 /*!endif*/

 /* GNU Extensions */
"__builtin_va_arg" { parse_token_text(); update_location(); return BUILTIN_VA_ARG; }
"__builtin_offsetof" { parse_token_text(); update_location(); return BUILTIN_OFFSETOF; }
 /*!if C99*/
"__builtin_choose_expr" { parse_token_text(); update_location(); return BUILTIN_CHOOSE_EXPR; }
"__builtin_types_compatible_p" { parse_token_text(); update_location(); return BUILTIN_TYPES_COMPATIBLE_P; }
 /*!endif*/
"__extension__" { parse_token_text(); update_location(); return EXTENSION; }
"__alignof" { parse_token_text(); update_location(); return TOKEN_GCC_ALIGNOF; }
"__alignof__" { parse_token_text(); update_location(); return TOKEN_GCC_ALIGNOF; }
"__ALIGNOF__" { parse_token_text(); update_location(); return TOKEN_GCC_ALIGNOF; }
 /*!if CPLUSPLUS*/
"alignof" { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_ALIGNOF; else return IDENTIFIER; }
 /*!endif*/
 /*!if CPLUSPLUS*/
"alignas" { parse_token_text(); update_location(); if (IS_CXX11_LANGUAGE) return TOKEN_ALIGNAS; else return IDENTIFIER; }
 /*!endif*/
"__real" { parse_token_text(); update_location(); return REAL; }
"__real__" { parse_token_text(); update_location(); return REAL; }
"__imag__" { parse_token_text(); update_location(); return IMAG; }
"__label__" { parse_token_text(); update_location(); return LABEL; }
"__complex" { parse_token_text(); update_location(); return COMPLEX; }
"__complex__" { parse_token_text(); update_location(); return COMPLEX; }
"_Complex" { parse_token_text(); update_location(); return COMPLEX; }
 /*!if C99*/
"_Imaginary" { parse_token_text(); update_location(); return IMAGINARY; }
 /*!endif*/
"typeof" { parse_token_text(); update_location(); return TYPEOF; }
"__typeof" { parse_token_text(); update_location(); return TYPEOF; }
"__typeof__" { parse_token_text(); update_location(); return TYPEOF; }
"restrict" { parse_token_text(); update_location(); return RESTRICT; /* This should be only for C99 but we will be lax here */ }
"__restrict" { parse_token_text(); update_location(); return RESTRICT; }
"__restrict__" { parse_token_text(); update_location(); return RESTRICT; }
"__const" { parse_token_text(); update_location(); return TOKEN_CONST; }
"__const__" { parse_token_text(); update_location(); return TOKEN_CONST; }
"__declspec" { parse_token_text(); update_location(); return TOKEN_DECLSPEC; }
"__attribute" { parse_token_text(); update_location(); return TOKEN_GCC_ATTRIBUTE; }
"__attribute__" { parse_token_text(); update_location(); return TOKEN_GCC_ATTRIBUTE; }
"__thread" { parse_token_text(); update_location(); return THREAD; } 
"__inline"        { parse_token_text(); update_location(); return INLINE; } 
"__inline__"        { parse_token_text(); update_location(); return INLINE; } 
"__volatile"        { parse_token_text(); update_location(); return TOKEN_VOLATILE; } 
"__volatile__"        { parse_token_text(); update_location(); return TOKEN_VOLATILE; } 
 /*!if C99*/
"_Atomic" { parse_token_text(); update_location(); if (IS_C11_LANGUAGE) return TOKEN_ATOMIC; else return IDENTIFIER; }
"_Generic" { parse_token_text(); update_location(); if (IS_C11_LANGUAGE) return TOKEN_GENERIC; else return IDENTIFIER; }
"_Noreturn" { parse_token_text(); update_location(); if (IS_C11_LANGUAGE) return TOKEN_NORETURN; else return IDENTIFIER; }
"_Static_assert" { parse_token_text(); update_location(); if (IS_C11_LANGUAGE) return STATIC_ASSERT; else return IDENTIFIER; }
"_Thread_local" { parse_token_text(); update_location(); if (IS_C11_LANGUAGE) return TOKEN_THREAD_LOCAL; else return IDENTIFIER; }
 /*!endif*/
"__asm" { parse_token_text(); update_location(); return ASM; }
"__asm__" { parse_token_text(); update_location(); return ASM; }
 /* XL Compiler extensions */
 "_Builtin" { parse_token_text(); update_location(); return XL_BUILTIN_SPEC; }
 /*!if CPLUSPLUS*/
"static_assert" { parse_token_text(); update_location(); return STATIC_ASSERT; }
 /* g++ 4.3 extensions */
"__has_nothrow_assign" |
"__has_nothrow_constructor" |
"__has_nothrow_copy" |
"__has_trivial_assign" |
"__has_trivial_constructor" |
"__has_trivial_copy" |
"__has_trivial_destructor" |
"__has_virtual_destructor" |
"__is_abstract" |
"__is_base_of" |
"__is_class" |
"__is_convertible_to" |
"__is_empty" |
"__is_enum" |
"__is_literal_type" |
"__is_pod" |
"__is_polymorphic" |
"__is_standard_layout" |
"__is_trivial" |
"__is_union" |
"__is_final" |
"__underlying_type" |
"__is_trivially_assignable" |
"__is_trivially_constructible" |
"__is_trivially_copyable" {
    return lookup_keyword_in_table(gxx_43_keywords, yytext, !CURRENT_CONFIGURATION->disable_gxx_type_traits);
}
 /*!endif*/

 /*!if C99*/
 /* UPC Extensions */
"relaxed" |
"shared" |
"strict" |
"upc_barrier" |
"upc_blocksizeof" |
"upc_elemsizeof" |
"upc_fence" |
"upc_forall" |
"upc_localsizeof" |
"upc_notify" |
"upc_wait" {
    return lookup_keyword_in_table(upc_keywords, yytext, CURRENT_CONFIGURATION->enable_upc);
}
 /*!endif */

 /* CUDA keywords */
"__device__" |
"__global__" |
"__host__" |
"__constant__" |
"__shared__" {
    return lookup_keyword_in_table(cuda_keywords, yytext, CURRENT_CONFIGURATION->enable_cuda);
}

 /* OpenCL keywords */
"__kernel" |
"__local" |
"__global" |
"__constant" {
    return lookup_keyword_in_table(opencl_keywords, yytext, CURRENT_CONFIGURATION->enable_opencl);
}

"__int8" |
"__int16" |
"__int32" |
"__int64" {
    return lookup_keyword_in_table(ms_builtin_types, yytext, CURRENT_CONFIGURATION->enable_ms_builtin_types);
}

"__assume" |
"__assume_aligned" {
    return lookup_keyword_in_table(intel_builtins_syntax, yytext, CURRENT_CONFIGURATION->enable_intel_builtins_syntax);
}

"__int128" {
    parse_token_text();
    update_location();
#if HAVE_INT128
    return TOKEN_INT128;
#else
    return IDENTIFIER;
#endif
}

"__float128" {
    parse_token_text();
    update_location();
#ifdef HAVE_QUADMATH_H
    return TOKEN_FLOAT128;
#else
    return IDENTIFIER;
#endif
}

 /* Special tokens for subparsing */
@EXPRESSION@ { parse_token_text(); update_location(); return SUBPARSE_EXPRESSION; }
@EXPRESSION-LIST@ { parse_token_text(); update_location(); return SUBPARSE_EXPRESSION_LIST; }
@STATEMENT@ { parse_token_text(); update_location(); return SUBPARSE_STATEMENT; }
@DECLARATION@ { parse_token_text(); update_location(); return SUBPARSE_DECLARATION; }
@TYPE@ { parse_token_text(); update_location(); return SUBPARSE_TYPE; }
@TYPE-LIST@ { parse_token_text(); update_location(); return SUBPARSE_TYPE_LIST; }
@ID_EXPRESSION@ { parse_token_text(); update_location(); return SUBPARSE_ID_EXPRESSION; }
@OMP-DECLARE-REDUCTION@ { parse_token_text(); update_location(); return SUBPARSE_OPENMP_DECLARE_REDUCTION; }
@OMP-DEPEND-ITEM@ { parse_token_text(); update_location(); return SUBPARSE_OPENMP_DEPEND_ITEM; }
@NODECL-LITERAL-EXPR@ { parse_token_text(); update_location(); return NODECL_LITERAL_EXPR; }
@NODECL-LITERAL-STMT@ { parse_token_text(); update_location(); return NODECL_LITERAL_STMT; }
@SYMBOL-LITERAL-REF@ { parse_token_text(); update_location(); return SYMBOL_LITERAL_REF; }
@TYPE-LITERAL-REF@ { parse_token_text(); update_location(); return TYPE_LITERAL_REF; }
@OMPSS-DEPENDENCY-EXPR@ { parse_token_text(); update_location(); return SUBPARSE_OMPSS_DEPENDENCY_EXPRESSION; }
 /*!if CPLUSPLUS*/
@MEMBER@ { parse_token_text(); update_location(); return SUBPARSE_MEMBER; }
 /*!endif*/
 /* Special tokens for prettyprinted comments and preprocessor elements */
@-C-@[^@]*@-CC-@ { 

    char* t = xstrdup(yytext);
    t[strlen(t) - strlen("@-CC-@")] = '\0';

    char *p = t;
    p += strlen("@-C-@");

    parse_token_text_str(p); 
    DELETE(t);

    update_location();

    return PP_COMMENT; 
}
@-P-@[^@]*@-PP-@ { 

    char* t = xstrdup(yytext);
    t[strlen(t) - strlen("@-PP-@")] = '\0';

    char *p = t;
    p += strlen("@-P-@");

    parse_token_text_str(p); 
    DELETE(t);

    update_location();

    return PP_TOKEN; 
}
 /* Special tokens for placeholders */
@STATEMENT-PH::0[xX]{hexdigit}+@ { parse_token_text(); update_location(); return STATEMENT_PLACEHOLDER; }
 /* Special tokens for parsing superscalar related things */
@SUPERSCALAR_DECLARATOR@ { parse_token_text(); update_location(); return SUBPARSE_SUPERSCALAR_DECLARATOR; }
@SUPERSCALAR_DECLARATOR_LIST@ { parse_token_text(); update_location(); return SUBPARSE_SUPERSCALAR_DECLARATOR_LIST; }
@SUPERSCALAR_EXPRESSION@ { parse_token_text(); update_location(); return SUBPARSE_SUPERSCALAR_EXPRESSION; }

@FORTRAN_ALLOCATE@  { parse_token_text(); update_location(); return C_FORTRAN_ALLOCATE; }

 /*!if C99*/
@ref@ { parse_token_text(); update_location(); return MCC_REFERENCE; }
 /*!endif*/
 /*!if CPLUSPLUS*/
@ref@ { parse_token_text(); update_location(); return '&'; }
 /*!endif*/

@mcc_byte@ { parse_token_text(); update_location(); return MCC_BYTE; }
@byte@     { /* Deprecated */
             parse_token_text(); update_location(); return MCC_BYTE; }
@mcc_bool@ { parse_token_text(); update_location(); return MCC_BOOL; }
@mcc_mask@ { parse_token_text(); update_location(); return MCC_MASK; }

@reb-ref@ { parse_token_text(); update_location(); return MCC_REBINDABLE_REFERENCE; }

@array-subscript-check@ { parse_token_text(); update_location(); return MCC_ARRAY_SUBSCRIPT_CHECK; }
@const-value-check@ { parse_token_text(); update_location(); return MCC_CONST_VALUE_CHECK; }

 /* A plain identifier */
{identifier} {parse_token_text(); update_location(); return IDENTIFIER; }

 /* A.2.16 - decimals */
{decimal_literal}{integersuffix}?      { parse_token_text(); update_location(); return DECIMAL_LITERAL; }

 /* A.2.16 - octals */
{octal_literal}{integersuffix}?        { parse_token_text(); update_location(); return OCTAL_LITERAL; }

 /* A.2.16 - hexas */
{hexadecimal_literal}{integersuffix}?  { parse_token_text(); update_location(); return HEXADECIMAL_LITERAL; }

{binary_literal}{integersuffix}?       { parse_token_text(); update_location(); return BINARY_LITERAL; }

 /* A.2.26 */
 /*!if CPLUSPLUS*/
[LUu]?'{cchar}'        { parse_token_text(); update_location(); return CHARACTER_LITERAL; }
 /*!endif*/
 /*!if C99*/
L?'{cchar}'        { parse_token_text(); update_location(); return CHARACTER_LITERAL; }
 /*!endif*/

 /* A.2.33 */
{fractconst}{exponent}?{floatingsuffix}?        { parse_token_text(); update_location();
return FLOATING_LITERAL; }

 /* C99 additional */
{hexprefix}{hexadecimal_fractional_constant}{binary_exponent_part}({floatingsuffix})? { 
    parse_token_text(); update_location(); 
    return HEXADECIMAL_FLOAT; 
}

{hexprefix}{hexdigit}+{binary_exponent_part}({floatingsuffix})? { 
    parse_token_text(); update_location(); 
    return HEXADECIMAL_FLOAT; 
}

 /* A.2.39 */
 /*!if C99*/
L?\"{schar}*\"      { parse_token_text(); update_location(); return STRING_LITERAL; }
 /*!endif*/
 /*!if CPLUSPLUS*/
((u8)|[uUL])?\"{schar}*\"      { parse_token_text(); update_location(); return STRING_LITERAL; }
 /*!endif*/
 /*!if CPLUSPLUS*/
((u8)|[uUL])?R["]{dchar}*[(]      {
    raw_string_buffer_capacity = 0;
    raw_string_buffer_position = 0;
    raw_string_buffer = NULL;

    int i;
    for (i = 0; yytext[i] != '"'; i++)
    {
        RAW_STRING_APPEND_CHAR(yytext[i]);
    }
    // Initial quote
    ERROR_CONDITION(yytext[i] != '"', "Invalid token", 0);
    RAW_STRING_APPEND_CHAR('"'); i++;

    int length = strlen(yytext);
    char c[length + 1];
    int j;
    for (j = 0; yytext[i] != '('; i++, j++)
    {
       c[j] = yytext[i];
    }
    // Make the delimiter be DELIM" because this eases matching it later
    c[j] = '"'; j++;
    c[j] = '\0';
    raw_string_delimiter = uniquestr(c);
    raw_string_delimiter_length = strlen(raw_string_delimiter);

    update_location();
    BEGIN(raw_string_body);
}

<raw_string_body>{

{rchar} {
   // Note, this does not include the right parenthesis
   RAW_STRING_APPEND_CHAR(yytext[0]);
   update_location();
 }

 ")" {
    BEGIN(raw_string_potential_suffix);
    raw_string_delimiter_index = 0;
    update_location();
}

}

<raw_string_potential_suffix>{

{dchar} {
         if (yytext[0] == raw_string_delimiter[raw_string_delimiter_index])
         {
             raw_string_delimiter_index++;

             if (raw_string_delimiter_index == raw_string_delimiter_length)
             {
                 // We have matched a full delimiter (including the final quote)
                 // Add the final quote
                 RAW_STRING_APPEND_CHAR('"');
                 RAW_STRING_APPEND_CHAR('\0');
                 parse_token_text_str(raw_string_buffer);

                 DELETE(raw_string_buffer);
                 raw_string_buffer = NULL;

                 update_location();

                 BEGIN(0);

                 return STRING_LITERAL;
             }
         }
         else
         {
             // This is not a delimiter, add all the characters seen so far
             RAW_STRING_APPEND_CHAR(')');
             int i;
             for (i = 0; i < raw_string_delimiter_index; i++)
             {
                 RAW_STRING_APPEND_CHAR(raw_string_delimiter[i]);
             }
             // And the mismatch one
             RAW_STRING_APPEND_CHAR(yytext[0]);
             update_location();
             BEGIN(raw_string_body);
         }
}

(.)|([\n\r\v\t]) {
    // This is not a delimiter, add all the characters seen so far
    RAW_STRING_APPEND_CHAR(')');
    int i;
    for (i = 0; i < raw_string_delimiter_index; i++)
    {
        RAW_STRING_APPEND_CHAR(raw_string_delimiter[i]);
    }

    if (yytext[0] != ')')
    {
        // And the mismatch one
        RAW_STRING_APPEND_CHAR(yytext[0]);
        BEGIN(raw_string_body);
    }
    else
    {
        raw_string_delimiter_index = 0;
        // Start again, this is potentially another suffix
    }
    update_location();
}

}




 /*!endif*/

 /*!if CPLUSPLUS*/

"["[ \t]*"[" {
     if (IS_CXX11_LANGUAGE)
     {
         parse_token_text();
         update_location();
         BEGIN(std_attribute);
         return STD_ATTRIBUTE_START;
     }
     else
     {
         // In the unlikely case we find this sequence of tokens, reject it
         REJECT;
     }
}

<std_attribute>{

{identifier} { 
      parse_token_text();
      update_location();
      BEGIN(std_attribute_clause);
      std_attribute_parenthesis_nesting = 0;
      return IDENTIFIER; 
}

{identifier}[ \t]*::[ \t]*{identifier} {

    // Remove blanks
    int len = strlen(yytext);
    char tok[len + 1];
    int i, j;
    for (i = 0, j = 0; i < len; i++)
    {
        if (yytext[i] == ' ' || yytext[i] == '\t')
            continue;
        tok[j] = yytext[i];
        j++;
    }
    tok[j] = '\0';

    parse_token_text_str(tok);
    BEGIN(std_attribute_clause);
    std_attribute_parenthesis_nesting = 0;
    update_location();
    return IDENTIFIER;
}

"]"[ \t]*"]" {
    parse_token_text();
    update_location();
    BEGIN(0);
    return STD_ATTRIBUTE_END; 
}

"," { parse_token_text();
    update_location();
    return ','; }

[ \t] { 
    update_location();
    /* skip blanks */ }

}

<std_attribute_clause>{

"]"[ \t]*"]" {
    if (std_attribute_parenthesis_nesting != 0)
    {
      REJECT;
    }
    else
    {
         parse_token_text();
         update_location();
         BEGIN(0);
         return STD_ATTRIBUTE_END; 
    }
}

"," { parse_token_text(); update_location(); return ','; }
"[" { parse_token_text(); update_location(); return '['; }
"]" { parse_token_text(); update_location(); return ']'; }
"{" { parse_token_text(); update_location(); return '{'; }
"}" { parse_token_text(); update_location(); return '}'; }
"(" { parse_token_text(); update_location();
      std_attribute_parenthesis_nesting++;
      return '(';
    }
")" { parse_token_text(); update_location();
      std_attribute_parenthesis_nesting--;
      if (std_attribute_parenthesis_nesting == 0)
      {
          BEGIN(std_attribute);
      }
      return ')'; 
    }

[^\[\]{}()\n]* { parse_token_text(); update_location(); return STD_ATTRIBUTE_TEXT; }

}
 /*!endif*/

"["         { parse_token_text(); update_location(); return '['; }
"]"         { parse_token_text(); update_location(); return ']'; }
")"         { parse_token_text(); update_location(); return ')'; }
"("         { parse_token_text(); update_location(); return '('; }
"->"        { parse_token_text(); update_location(); return PTR_OP; }
 /*!if CPLUSPLUS*/
"->*"       { parse_token_text(); update_location(); return PTR_OP_MUL; }
 /*!endif*/
"++"        { parse_token_text(); update_location(); return PLUSPLUS; }
"--"        { parse_token_text(); update_location(); return MINUSMINUS; }
"+"         { parse_token_text(); update_location(); return '+'; }
"-"         { parse_token_text(); update_location(); return '-'; }
"~"         { parse_token_text(); update_location(); return '~'; }
"!"         { parse_token_text(); update_location(); return '!'; }
"/"         { parse_token_text(); update_location(); return '/'; }
"%"         { parse_token_text(); update_location(); return '%'; }
"<<"        { parse_token_text(); update_location(); return LEFT; }
"<<<"       { 
     if (CURRENT_CONFIGURATION->enable_cuda)
     {
         parse_token_text();
         update_location();
         return CUDA_KERNEL_LEFT;
     }
     REJECT;
}
 /* There is no '>>' token in C++ but AB1 followed by '>', see below */
 /*!if C99*/
">>>"       { 
    if (CURRENT_CONFIGURATION->enable_cuda)
    {
        parse_token_text();
         update_location();
        return CUDA_KERNEL_RIGHT;
    }
    REJECT;
}
">>"        { parse_token_text(); update_location(); return RIGHT; }
 /*!endif*/
"<="        { parse_token_text(); update_location(); return LESS_OR_EQUAL; }
">="        { parse_token_text(); update_location(); return GREATER_OR_EQUAL; }
"=="        { parse_token_text(); update_location(); return EQUAL; }
"!="        { parse_token_text(); update_location(); return NOT_EQUAL; }
"^"         { parse_token_text(); update_location(); return '^'; }
"|"         { parse_token_text(); update_location(); return '|'; }
"&&"        { parse_token_text(); update_location(); return ANDAND; }
"||"        { parse_token_text(); update_location(); return OROR; }
"="         { parse_token_text(); update_location(); return '='; }
"*="        { parse_token_text(); update_location(); return MUL_ASSIGN; }
"/="        { parse_token_text(); update_location(); return DIV_ASSIGN; }
"%="        { parse_token_text(); update_location(); return MOD_ASSIGN; }
"+="        { parse_token_text(); update_location(); return ADD_ASSIGN; }
"-="        { parse_token_text(); update_location(); return SUB_ASSIGN; }
"<<="       { parse_token_text(); update_location(); return LEFT_ASSIGN; }
">>="       { parse_token_text(); update_location(); return RIGHT_ASSIGN; }
"&="        { parse_token_text(); update_location(); return AND_ASSIGN; }
"^="        { parse_token_text(); update_location(); return XOR_ASSIGN; }
"|="        { parse_token_text(); update_location(); return OR_ASSIGN; }



";"         { parse_token_text(); update_location(); return ';'; } 
":"         { parse_token_text(); update_location(); return ':'; }
"."         { parse_token_text(); update_location(); return '.'; }
","         { parse_token_text(); update_location(); return ','; }
"?"         { parse_token_text(); update_location(); return '?'; }
"{"         { parse_token_text(); update_location(); return '{'; }
"}"         { parse_token_text(); update_location(); return '}'; } 
"<"         { parse_token_text(); update_location(); return '<'; }
 /*!if CPLUSPLUS*/
 /* In C++2003 these three tokens behave like in C99 
    REJECT is used to let the input be matched by later rules
    since flex always prioritize the longer matches
  */
>>>         {
              if (CURRENT_CONFIGURATION->enable_cuda
                   && IS_CXX03_LANGUAGE)
              {
                  parse_token_text();
                  update_location();
                  return CUDA_KERNEL_RIGHT;
              }
              REJECT;
            }
>>=         { parse_token_text(); update_location(); return RIGHT_ASSIGN; }
 /* In C++2011 >> is decomposed first into AB1 '>' */
>/>[^=]     { if (!IS_CXX11_LANGUAGE)
              { REJECT; }
              else
              { parse_token_text(); update_location(); return AB1; } }
 /* Pathological case for EOF */
>/>         { if (!IS_CXX11_LANGUAGE)
              { REJECT; }
              else
              { parse_token_text(); update_location(); return AB1; } }
>>          { parse_token_text(); update_location(); return RIGHT; }
>=          { parse_token_text(); update_location(); return GREATER_OR_EQUAL; }
>           { parse_token_text(); update_location(); return '>'; }
 /*!endif*/
 /*!if C99*/
">"         { parse_token_text(); update_location(); return '>'; }
 /*!endif*/
"&"         { parse_token_text(); update_location(); return '&'; }
"*"         { parse_token_text(); update_location(); return '*'; }
 /*!if CPLUSPLUS*/
"::"        { parse_token_text(); update_location(); return TWO_COLONS; }
"::"{whitespace}*"*"  { parse_token_text(); update_location(); return TWO_COLONS_AND_STAR; }
 /*!endif*/
"..."       { parse_token_text(); update_location(); return ELLIPSIS; }

".."        { parse_token_text(); update_location(); return TWO_DOTS; }

{newline}+        { update_location(); }
{blanksinline}+   { update_location(); }
<*>.           { 
    /* ignore bad characters */ 
    if (isgraph(*yytext))
    {
        error_printf_at(
                make_locus(
                    scanning_now.current_filename,
                    scanning_now.line_number,
                    scanning_now.column_number),
                "invalid character '%c' (%#x)\n",
                *yytext, *yytext);
    }
    else
    {
        error_printf_at(
                make_locus(
                    scanning_now.current_filename,
                    scanning_now.line_number,
                    scanning_now.column_number),
                "invalid character %#x\n", 
                *yytext);
    }
    update_location();
}


%%

static void close_scanned_file(void);

static int yywrap(void)
{
    close_scanned_file();
    return 1;
}

/*!if CPLUSPLUS*/
#define FLEX_LVAL mcxxlval
#define FLEX_LLOC mcxxlloc
/*!endif*/
/*!if C99*/
#define FLEX_LVAL mc99lval
#define FLEX_LLOC mc99lloc
/*!endif*/

static void update_location_str(const char* c)
{
    while (*c != '\0')
    {
        if (*c == '\n'
                || *c == '\r')
        {
            scanning_now.line_number++;
            scanning_now.column_number = 1;

            if (*c == '\r')
            {
                c++;
                // DOS endings are \r\n, skip \n
                if (*c == '\n')
                {
                    c++;
                }
            }
            else
            {
                // *c == '\n'
                c++;
            }
        }
        else
        {
            scanning_now.column_number++;
            c++;
        }
    }
}

static void update_location(void)
{
    update_location_str(yytext);
}

static void parse_token_text_str(const char* c)
{
    FLEX_LVAL.token_atrib.token_text = uniquestr(c);

    FLEX_LLOC.first_filename = uniquestr(scanning_now.current_filename);
    FLEX_LLOC.first_line = scanning_now.line_number;
    FLEX_LLOC.first_column = scanning_now.column_number;
}

static void parse_token_text(void)
{
    parse_token_text_str(yytext);
}

/*!if CPLUSPLUS*/
#define OPEN_FILE_FOR_SCANNING mcxx_open_file_for_scanning
#define PREPARE_STRING_FOR_SCANNING mcxx_prepare_string_for_scanning
/*!endif*/
/*!if C99*/
#define OPEN_FILE_FOR_SCANNING mc99_open_file_for_scanning
#define PREPARE_STRING_FOR_SCANNING mc99_prepare_string_for_scanning
/*!endif*/

static const char* const TL_SOURCE_STRING = "MERCURIUM_INTERNAL_SOURCE";

int PREPARE_STRING_FOR_SCANNING(const char* str)
{
    static int num_string = 0;
	DEBUG_CODE()
	{
		fprintf(stderr, "* Going to parse string in C/C++\n");
        fprintf(stderr, "%s\n", str);
		fprintf(stderr, "* End of parsed string\n");
	}
	memset(&scanning_now, 0, sizeof(scanning_now));
	scanning_now.line_number = 1;
	scanning_now.column_number = 1;

	const char* current_filename = CURRENT_COMPILED_FILE->input_filename;

    char c[MCXX_MAX_FILENAME];
    snprintf(c, MCXX_MAX_FILENAME, "%s-%s-%d", TL_SOURCE_STRING, current_filename, num_string);
    c[MCXX_MAX_FILENAME-1] = '\0';
    scanning_now.filename = uniquestr(c);

    scanning_now.current_filename = scanning_now.filename;

    num_string++;

	scanning_now.scanning_buffer = yy_scan_string(str);

	yy_switch_to_buffer(scanning_now.scanning_buffer);
    yy_set_bol(1);

	return 0;
}

int OPEN_FILE_FOR_SCANNING(const char* scanned_filename, const char* input_filename)
{
	FILE* file;

	file = fopen(scanned_filename, "r");

	if (file == NULL)
	{
		fatal_error("error: cannot open file '%s' (%s)", scanned_filename, strerror(errno));
	}

	memset(&scanning_now, 0, sizeof(scanning_now));
	scanning_now.filename = uniquestr(scanned_filename);
	scanning_now.file_descriptor = file;
	scanning_now.line_number = 1;
	scanning_now.column_number = 1;

	main_input_filename = uniquestr(input_filename);
    scanning_now.current_filename = main_input_filename;

	scanning_now.scanning_buffer = yy_create_buffer(file, YY_BUF_SIZE);

	yy_switch_to_buffer(scanning_now.scanning_buffer);
    yy_set_bol(1);

	return 0;
}


static void close_scanned_file(void)
{
    if (scanning_now.file_descriptor != NULL)
    {
        fclose(scanning_now.file_descriptor);
        scanning_now.file_descriptor = NULL;
    }
}

/*!if C99*/
static pragma_directive_kind_t lookup_pragma_directive_inner(pragma_directive_set_t* pragma_directive_set, 
        const char *directive)
{
    int j;
    for (j = 0; j < pragma_directive_set->num_directives; j++)
    {
        if (strcmp(pragma_directive_set->directive_names[j], directive) == 0)
        {
            return pragma_directive_set->directive_kinds[j];
        }
    }

    return PDK_NONE;
}

pragma_directive_kind_t lookup_pragma_directive(const char* prefix, const char* directive)
{
    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcmp(CURRENT_CONFIGURATION->pragma_custom_prefix[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION->pragma_custom_prefix_info[i];
            return lookup_pragma_directive_inner(pragma_directive_set, directive);
        }
    }

    return PDK_NONE;
}

static void register_new_directive_inner(pragma_directive_set_t* pragma_directive_set,
        const char* directive, pragma_directive_kind_t kind)
{
    int num_directives = pragma_directive_set->num_directives;
    P_LIST_ADD(pragma_directive_set->directive_names,
            num_directives,
            uniquestr(directive));
    P_LIST_ADD(pragma_directive_set->directive_kinds,
            pragma_directive_set->num_directives,
            kind);
}

void register_new_directive(compilation_configuration_t* configuration, 
        const char* prefix, const char* directive, char is_construct, char bound_to_single_stmt)
{
    pragma_directive_kind_t kind = (is_construct ? PDK_CONSTRUCT : PDK_DIRECTIVE);

    if (kind == PDK_CONSTRUCT 
            && bound_to_single_stmt)
    {
        kind = PDK_CONSTRUCT_NOEND;
    }

    int i;
    for (i = 0; i < configuration->num_pragma_custom_prefix; i++)
    {
        if (strcmp(configuration->pragma_custom_prefix[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = configuration->pragma_custom_prefix_info[i];

            int j;
            for (j = 0; j < pragma_directive_set->num_directives; j++)
            {
                if (strcmp(pragma_directive_set->directive_names[j], directive) == 0)
                {
                    warn_printf_at(NULL, "Warning, directive or construct "
                            "'%s' already registered for pragma '%s'"
                            ", ignoring additional registrations\n",
                            directive, prefix);
                    return;
                }
            }

            register_new_directive_inner(pragma_directive_set, directive, kind);
        }
    }
}
/*!endif*/

static char* _verbatim_buffer = NULL;
static int _verbatim_buffer_length = 0;
static int _verbatim_buffer_capacity = 0;

#define BUFF_SIZE 512

static void verbatim_buffer_append(const char* c)
{
    const char* p = c;

    // This may be regarded a bit inefficient but it is safer this way
    while (*p != '\0')
    {
        ERROR_CONDITION((_verbatim_buffer_length > _verbatim_buffer_capacity), 
                "Verbatim buffer overflow", 0);

        if (_verbatim_buffer_length == _verbatim_buffer_capacity)
        {
            // Enlarge verbatim buffer
            _verbatim_buffer_capacity += BUFF_SIZE;
            _verbatim_buffer = NEW_REALLOC(char, _verbatim_buffer, _verbatim_buffer_capacity);
            int i;
            for (i = _verbatim_buffer_length; i < _verbatim_buffer_capacity; i++)
            {
                _verbatim_buffer[i] = '\0';
            }
        }

        ERROR_CONDITION(!(_verbatim_buffer_length < _verbatim_buffer_capacity),
                "Wrong logic in verbatim buffer handling", 0);

        _verbatim_buffer[_verbatim_buffer_length] = *p;
        _verbatim_buffer_length++;

        p++;
    }
}

static void verbatim_buffer_free(void)
{
    DELETE(_verbatim_buffer);
    _verbatim_buffer = NULL;
    _verbatim_buffer_length = 0;
    _verbatim_buffer_capacity = 0;
}

static const char* verbatim_buffer_get(void)
{
    return _verbatim_buffer;
}

// I hate that this is not C90
static char is_blank(char c)
{
    return (c == ' ') || (c == '\t');
}

static int compute_length_match(const char* lexed_directive,
        const char* available_directive,
        const char **discard_source)
{
    int num_matches = 0;
    {
    char* lexed = xstrdup(lexed_directive);
    char* available = xstrdup(available_directive);

    char *next_lexed_info = NULL;
    char *next_lexed = strtok_r(lexed, " \r", &next_lexed_info);
    char *next_available_info = NULL;
    char *next_available = strtok_r(available, "|", &next_available_info);

    while (next_lexed != NULL
            && next_available != NULL)
    {
        if (strcmp(next_lexed, next_available) == 0)
        {
            num_matches++;
            next_lexed = strtok_r(NULL, " \r", &next_lexed_info);
            next_available = strtok_r(NULL, "|", &next_available_info);
        }
        else
        {
            // Failing one match is enough to discard all of it so we favour
            // the longest match and discard all partial matches
            num_matches = 0;
            break;
        }
    }

    DELETE(available);
    DELETE(lexed);
    }

    // Compute the length of the discards
    const char* p = lexed_directive;

    *discard_source = NULL;
    if (num_matches != 0)
    {
        ERROR_CONDITION((*p == ' ') || (*p == '\t'), 
                "Invalid lexing, there should not be a leading blank", 0);

        // Skip first match
        while (*p != '\0' 
                && !is_blank(*p))
        {
            p++;
        }

        int n = num_matches - 1;
        while ((n > 0)
                && (*p != '\0'))
        {
            // Now advance blanks
            while ((*p != '\0')
                    && is_blank(*p))
            {
                p++;
            }
            // And now, non-blanks
            while ((*p != '\0')
                    && !is_blank(*p))
            {
                p++;
            }
            n--;
        }

        if (*p != '\0')
        {
            *discard_source = p;
        }
    }

    return num_matches;
}

static const char* return_pragma_prefix_longest_match_inner(pragma_directive_set_t* pragma_directive_set,
        const char* lexed_directive,
        const char **discard_source,
        pragma_directive_kind_t* directive_kind)
{
    const char* longest_match_so_far = NULL;
    int length_match = 0;
    char exact_match = 0;
    int size_lexed_directive = strlen(lexed_directive);

    int j;
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
        char* original_directive)
{
    const char* longest_match = NULL;
    const char* discard_source = NULL;

    int i;
    for (i = 0; i < CURRENT_CONFIGURATION->num_pragma_custom_prefix; i++)
    {
        if (strcmp(CURRENT_CONFIGURATION->pragma_custom_prefix[i], prefix) == 0)
        {
            pragma_directive_set_t* pragma_directive_set = CURRENT_CONFIGURATION->pragma_custom_prefix_info[i];
            longest_match = return_pragma_prefix_longest_match_inner(pragma_directive_set, lexed_directive, 
                    &discard_source, kind);
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

        // *end == '\0'
        end--;

        // the character pointed by discard_source must be unput as well
        while (end >= discard_source)
        {
            unput(*end);
            end--;
        }
    }

    return longest_match;
}

static int lookup_keyword_in_table(lexer_keyword_t *keyword_table, const char* keyword, char predicate)
{
    if (!predicate)
    {
        parse_token_text();
        update_location();
        return IDENTIFIER;
    }

    int i = 0;
    while (keyword_table[i].keyword != NULL)
    {
        if (strcmp(keyword_table[i].keyword, keyword) == 0)
        {
            parse_token_text();
            update_location();
            return keyword_table[i].token;
        }
        i++;
    }

    internal_error("%s:%d:%d: error: unhandled lookup token '%s'", 
            scanning_now.filename,
            scanning_now.line_number,
            scanning_now.column_number,
            keyword);
}

static void unput_string(const char* c)
{
    if (c == NULL)
        return;

    int N = strlen(c);
    int i;
    for (i = N-1; i >= 0; i--)
    {
        unput(c[i]);
    }
}

/*!if C99*/
void * mc99alloc (size_t bytes)
{
    return xmalloc(bytes);
}
void * mc99realloc (void * ptr, size_t bytes)
{
    return xrealloc(ptr, bytes);
}
void   mc99free (void * ptr)
{
    DELETE(ptr);
}
/*!endif*/

/*!if CPLUSPLUS*/
void * mcxxalloc (size_t bytes)
{
    return xmalloc(bytes);
}
void * mcxxrealloc (void * ptr, size_t bytes)
{
    return xrealloc(ptr, bytes);
}
void   mcxxfree (void * ptr)
{
    DELETE(ptr);
}
/*!endif*/
