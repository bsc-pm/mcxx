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

#include <ctype.h>
#include <errno.h>
#include <string.h>
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include "cxx-utils.h"
#include "uniquestr.h"
#include "filename.h"
#include "mem.h"

char configfile_lexer_free_text = 0;

static int yywrap();

static void handle_token(const char *);

typedef
struct location_tag
{
    FILE* file;
    const char *filename;
    int line;
    int column;
} location_t;

location_t current_location = { NULL, NULL, 1, 1};

%}

%option noyyalloc noyyrealloc noyyfree noinput nounput

%x linecomment
%x maybe_comes_option_line
%x option_line
%x option_value
%x profile_name
%x flaglist

%%

<linecomment>{

[\n] { BEGIN(INITIAL); }

[^\n]* {
    current_location.line++;
    /* Ignore comment */
}

}


<profile_name>{

[ \t]+ { /* Ignore whitespace */ }

"]" { handle_token(yytext); return ']'; }
":" { handle_token(yytext); return ':'; }
">" { handle_token(yytext); return '>'; }

[-_+.[:alpha:]][-_+.[:alnum:]]* {
    handle_token(yytext);
    return CONFIGFILE_NAME;
}

[\n] {
    current_location.line++;
    BEGIN(INITIAL);
    return EOL; 
}

}

<flaglist>{

[ \t]+ { /* Ignore whitespace */ }
"," { handle_token(yytext); return ','; }
"}" { handle_token(yytext); BEGIN(maybe_comes_option_line); return '}'; }
"!" { handle_token(yytext); return '!'; }
"?" { handle_token(yytext); return '?'; }
"&" { handle_token(yytext); return '&'; }
"|" { handle_token(yytext); return '|'; }
"(" { handle_token(yytext); return '('; }
")" { handle_token(yytext); return ')'; }

[-_+.[:alpha:]][-_+.[:alnum:]]* {
    handle_token(yytext);
    return CONFIGFILE_NAME;
}

":true:" {
    handle_token(yytext);
    return CONFIGFILE_FLAG_TRUE;
}

":false:" {
    handle_token(yytext);
    return CONFIGFILE_FLAG_FALSE;
}

}

<maybe_comes_option_line>{

[ \t]+ { /* Ignore whitespace */ }

[-_+.[:alpha:]][-_+.[:alnum:]]* {
    handle_token(yytext);
    BEGIN(option_line);
    return CONFIGFILE_NAME;
}

[\n] { 
    current_location.line++;
    BEGIN(INITIAL);
    return EOL; 
}

}

<option_line>{

[ \t]+ { /* Ignore whitespace */ }

=[ \t]* { handle_token(yytext); BEGIN(option_value); return '='; }

"[" { handle_token(yytext); return '['; }
"]" { handle_token(yytext); return ']'; }

[-_+.[:alpha:]][-_+.[:alnum:]]* {
    handle_token(yytext);
    return CONFIGFILE_NAME;
}

}

<option_value>{ 

[^#\n]* { 
    handle_token(yytext); 
    return CONFIGFILE_OPTION_VALUE; 
}

"#" { /* Start of comment */ BEGIN(linecomment); return EOL; }

[\n] {
    current_location.line++;
    BEGIN(INITIAL);
    return EOL; 
}

<<EOF>> {
    BEGIN(INITIAL);
    return EOL;
}

}

"#" { /* Start of comment */ BEGIN(linecomment); }
"{" { handle_token(yytext); BEGIN(flaglist); return '{'; }
"[" { handle_token(yytext); BEGIN(profile_name); return '['; }

[-_+.[:alpha:]][-_+.[:alnum:]]* {
    handle_token(yytext);
    BEGIN(option_line);
    return CONFIGFILE_NAME;
}

[ \t]+ { /* Ignore whitespace at top level */ }
[\n] {
    // This is like whitespace
    current_location.line++;
}


<*>. {
    unsigned char c = *yytext;
    // Update location info
    handle_token(yytext);
    if (!isprint(c))
    {
        fprintf(stderr, "%s:%d: warning: ignoring invalid character '\\0x%03x' found in configuration file\n", 
                give_basename(current_location.filename), 
                current_location.line, 
                c);
    }
    else
    {
        fprintf(stderr, "%s:%d: warning: ignoring invalid character '%c' found in configuration file\n", 
                give_basename(current_location.filename), 
                current_location.line, 
                c);
    }
}


%%

static int yywrap()
{
    return 1;
}


static void handle_token(const char * text)
{
    configfilelval.str = uniquestr(text);
    // No token spans more than one line
    configfilelloc.filename = current_location.filename;
    configfilelloc.first_line = configfilelloc.last_line = current_location.line;
    configfilelloc.first_column = current_location.column;
    configfilelloc.last_column = configfilelloc.first_column + strlen(text);
    current_location.column = configfilelloc.last_column;

#if 0
    fprintf(stderr, "TOKEN: |%s| at %s:%d\n",
            configfilelval.str,
            configfilelloc.filename,
            configfilelloc.first_line);
#endif
}

extern int configfiledebug;

int open_configuration_file_for_scan(const char* scanned_filename)
{
	FILE* file;

	file = fopen(scanned_filename, "r");

	if (file == NULL)
	{
		fprintf(stderr, "warning: cannot open configuration file '%s' (%s)", scanned_filename, strerror(errno));
        return 0;
	}

    memset(&current_location, 0, sizeof(current_location));
    current_location.file = file;
    current_location.filename = uniquestr(scanned_filename);
	current_location.line = 1;
	current_location.column = 1;

    configfile_flex_debug = 0;
    configfiledebug = 0;

    struct yy_buffer_state* scanning_buffer = yy_create_buffer(file, YY_BUF_SIZE);

	yy_switch_to_buffer(scanning_buffer);

    BEGIN(INITIAL);

    return 1;
}

void close_configuration_file_for_scan(void)
{
    if (current_location.file != NULL)
    {
        fclose(current_location.file);
    }
}

void * configfilealloc (size_t bytes)
{
    return xmalloc(bytes);
}
void * configfilerealloc (void * ptr, size_t bytes)
{
    return xrealloc(ptr, bytes);
}
void   configfilefree (void * ptr)
{
    DELETE(ptr);
}
