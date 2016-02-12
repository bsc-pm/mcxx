/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

#include <string.h>
#include "cxx-driver.h"
#include "cxx-profile.h"
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include "cxx-utils.h"
#include "cxx-typeenviron.h"
#include "cxx-typeutils.h"
#include "fortran03-typeenviron.h"
#include "fortran03-mangling.h"
#include "mem.h"

static void new_option_list(option_list_t* list);
static void add_to_option_list(option_list_t* list, p_compilation_configuration_line);

static p_compilation_configuration_line process_option_line(
        flag_expr_t* flag_expr,
        profile_option_name_t* name, 
        const char* option_value,
        const char* filename,
        int line);

struct flag_expr_tag
{
    enum flag_op kind;
    struct flag_expr_tag* op[2];
    const char* text;
};

static flag_expr_t* flag_name(const char* name);
static flag_expr_t* flag_true(void);
static flag_expr_t* flag_false(void);
static flag_expr_t* flag_is_defined(const char* name);
static flag_expr_t* flag_not(flag_expr_t* op);
static flag_expr_t* flag_and(flag_expr_t* op1, flag_expr_t* op2);
static flag_expr_t* flag_or(flag_expr_t* op1, flag_expr_t* op2);

static void register_implicit_names(flag_expr_t* flag_expr);

#define YYMALLOC xmalloc
#define YYFREE DELETE
#define YYREALLOC xrealloc

%}

%union {
    profile_header_t profile_header;
    p_compilation_configuration_line profile_option;
    profile_option_name_t profile_option_name;
    struct flag_expr_tag* flag;
    const char* str;
    option_list_t option_list;
}

%token<str> '!'
%token<str> '['
%token<str> ']'
%token<str> ':'
%token<str> '>'
%token<str> ','
%token<str> '('
%token<str> ')'
%token<str> '&'
%token<str> '|'
%token<str> CONFIGFILE_NAME "identifier"
%token<str> CONFIGFILE_FLAG_TRUE ":true:"
%token<str> CONFIGFILE_FLAG_FALSE ":false:"
%token<str> CONFIGFILE_OPTION_VALUE "option-value"
%token<str> EOL "end-of-line"

%type<profile_header> profile_header

%type<profile_option> option_line

%type<profile_option_name> option_name

%type<flag> flag_spec
%type<flag> flag_expr
%type<flag> flag_not
%type<flag> flag_and
%type<flag> flag_or
%type<flag> flag_atom

%type<option_list> profile_body
%type<option_list> option_line_seq
%type<str> option_value

%start config_file

%locations

%%


config_file : profile_seq
{
}
| /* Empty. This is fine */
{
}
;

profile_seq : profile_seq profile
{
}
| profile
{
}
;

profile: profile_header profile_body
{
    compilation_configuration_t* base_config = NULL;
    if ($1.base_profile_name != NULL)
    {
        base_config = get_compilation_configuration($1.base_profile_name);

        if (base_config == NULL)
        {
            fprintf(stderr, "%s:%d: warning: base configuration '%s' does not exist. Ignoring\n",
                    @1.filename, @1.first_line, $1.base_profile_name);
        }
    }

    // fprintf(stderr, "--> PROCESSING SECTION ['%s' : '%s']\n", 
    //         $1.profile_name, 
    //         $1.base_profile_name == NULL ? "" : $1.base_profile_name);

    compilation_configuration_t *new_configuration = new_compilation_configuration($1.profile_name, base_config);

    new_configuration->num_configuration_lines = $2.num_options;
    new_configuration->configuration_lines = $2.options;

    new_configuration->type_environment = default_environment;
    new_configuration->fortran_array_descriptor = default_fortran_array_descriptor;
    new_configuration->fortran_name_mangling = default_fortran_name_mangling;
    new_configuration->print_vector_type = print_gnu_vector_type;

    if (get_compilation_configuration($1.profile_name) != NULL)
    {
        fprintf(stderr, "%s:%d: warning: configuration profile '%s' already exists. First one defined will be used!\n",
                @1.filename, @1.first_line,
                $1.profile_name);
    }

    P_LIST_ADD(compilation_process.configuration_set, 
            compilation_process.num_configurations, 
            new_configuration);
}
;

profile_header : '[' CONFIGFILE_NAME ']' EOL
{
    $$.profile_name = $2;
    $$.base_profile_name = NULL;
}
| '[' CONFIGFILE_NAME ':' CONFIGFILE_NAME ']' EOL
{
    $$.profile_name = $2;
    $$.base_profile_name = $4;
}
| '[' CONFIGFILE_NAME '>' CONFIGFILE_NAME ']' EOL
{
    $$.profile_name = $2;
    $$.base_profile_name = $4;
}
;

profile_body : option_line_seq
{
    $$ = $1;
}
// We allow it to be empty
|
{
    new_option_list(&$$);
}
;

option_line_seq : option_line_seq option_line
{
    $$ = $1;
    if ($2 != NULL)
    {
        add_to_option_list(&$$, $2);
    }
}
| option_line
{
    new_option_list(&$$);
    if ($1 != NULL)
    {
        add_to_option_list(&$$, $1);
    }
}
;

// We need a lexical tie in since we allow free text after the '=' 
option_line : flag_spec option_name '=' option_value EOL
{
    $$ = process_option_line($1, &$2, $4, @2.filename, @2.first_line);
}
// Degenerated cases. Do nothing with them
| flag_spec EOL
{
    $$ = NULL;
}
;

option_value : CONFIGFILE_OPTION_VALUE
{
    $$ = $1;
}
|
{
    $$ = "";
}
;

option_name : CONFIGFILE_NAME
{
    $$.option_name = $1;
    $$.option_index = NULL;
}
| CONFIGFILE_NAME '[' CONFIGFILE_NAME ']'
{
    $$.option_name = $1;
    $$.option_index = $3;
}
;

flag_spec : '{' flag_expr '}'
{
    $$ = $2;
}
// This may be useful sometimes
| '{' '}'
{
    $$ = NULL;
}
// It can be empty
| 
{
    $$ = NULL;
}
;

flag_expr : flag_or
{
    $$ = $1;
}
;

flag_or : flag_or '|' flag_and
{
    $$ = flag_or($1, $3);
}
| flag_and
{
    $$ = $1;
}
;

flag_and : flag_and ',' flag_not
{
    $$ = flag_and($1, $3);
}
| flag_and '&' flag_not
{
    $$ = flag_and($1, $3);
}
| flag_not
{
    $$ = $1;
}
;

flag_not : '!' flag_atom
{
    $$ = flag_not($2);
}
| flag_atom
{
    $$ = $1;
}
;

flag_atom : CONFIGFILE_NAME
{
    $$ = flag_name($1);
}
| '?' CONFIGFILE_NAME
{
    $$ = flag_is_defined($2);
}
| '(' flag_expr ')'
{
    $$ = $2;
}
| CONFIGFILE_FLAG_TRUE
{
    $$ = flag_true();
}
| CONFIGFILE_FLAG_FALSE
{
    $$ = flag_false();
}
;

%%

static void new_option_list(option_list_t* list)
{
    memset(list, 0, sizeof(*list));
}

static void add_to_option_list(option_list_t* list, p_compilation_configuration_line p_line)
{
    P_LIST_ADD(list->options, list->num_options, p_line);
}

void yyerror(const char *c)
{
  fprintf(
      stderr,
      "%s:%d:%d: warning: error encountered when parsing configuration file: %s\n",
      yylloc.filename, yylloc.first_line, yylloc.first_column, c);
}

static void register_implicit_names(flag_expr_t* flag_expr)
{
    if (flag_expr != NULL)
    {
        if (flag_expr->kind == FLAG_OP_NAME
            || flag_expr->kind == FLAG_OP_IS_DEFINED)
        {
            parameter_flags_t *new_parameter_flag = NEW0(parameter_flags_t);

            new_parameter_flag->name = flag_expr->text;
            new_parameter_flag->value = PFV_UNDEFINED;

            P_LIST_ADD(compilation_process.parameter_flags, 
                    compilation_process.num_parameter_flags,
                    new_parameter_flag);
        }
        else 
        {
            register_implicit_names(flag_expr->op[0]);
            register_implicit_names(flag_expr->op[1]);
        }
    }
}

static flag_expr_t* new_flag(void)
{
    flag_expr_t* result = NEW0(flag_expr_t);
    return result;
}

static flag_expr_t* flag_true(void)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_TRUE;

    return result;
}

static flag_expr_t* flag_false(void)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_FALSE;

    return result;
}

static flag_expr_t* flag_name(const char* name)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_NAME;
    result->text = name;

    return result;
}

static flag_expr_t* flag_is_defined(const char* name)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_IS_DEFINED;
    result->text = name;

    return result;
}

static flag_expr_t* flag_not(flag_expr_t* op)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_NOT;
    result->op[0] = op;

    return result;
}

static flag_expr_t* flag_and(flag_expr_t* op1, flag_expr_t* op2)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_AND;
    result->op[0] = op1;
    result->op[1] = op2;

    return result;
}

static flag_expr_t* flag_or(flag_expr_t* op1, flag_expr_t* op2)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_OR;
    result->op[0] = op1;
    result->op[1] = op2;

    return result;
}

static p_compilation_configuration_line process_option_line(
        flag_expr_t* flag_expr,
        profile_option_name_t* name,
        const char* option_value,
        const char* filename,
        int line)
{
    p_compilation_configuration_line result;

    char* option_value_tmp = xstrdup(option_value);
    {
        // Trim the option value
        char *p = &option_value_tmp[strlen(option_value_tmp) - 1];
        while (p >= option_value_tmp
                && (*p == ' ' || *p == '\t'))
        {
            *p = '\0';
            p--;
        }
    }

    result = NEW0(compilation_configuration_line_t);

    result->name = uniquestr(name->option_name);
    result->index = uniquestr(name->option_index);
    result->value = uniquestr(option_value_tmp);

    DELETE(option_value_tmp);

    result->flag_expr = flag_expr;
    result->filename = filename;
    result->line = line;

#if 0
    fprintf(stderr, "LINE: |%s| at %s:%d\n", result->name, result->filename, result->line);
#endif

    register_implicit_names(flag_expr);

    return result;
}

char flag_expr_eval(flag_expr_t* flag_expr)
{
    switch (flag_expr->kind)
    {
        case FLAG_OP_NAME:
        case FLAG_OP_IS_DEFINED:
            {
                // Ugly and inefficient lookup
                char found = 0;
                parameter_flag_value_t value_of_flag = PFV_UNDEFINED;
                int q;
                for (q = 0; !found && q < compilation_process.num_parameter_flags; q++)
                {
                    parameter_flags_t *parameter_flag = compilation_process.parameter_flags[q];
                    found = (strcmp(parameter_flag->name, flag_expr->text) == 0);
                    value_of_flag = parameter_flag->value;
                }

                if (flag_expr->kind == FLAG_OP_NAME)
                {
                    return (value_of_flag == PFV_TRUE);
                }
                else if (flag_expr->kind == FLAG_OP_IS_DEFINED)
                {
                    return (value_of_flag != PFV_UNDEFINED);
                }
                else
                {
                    internal_error("Code unreachable", 0);
                }
                break;
            }
        case FLAG_OP_NOT:
            {
                return !flag_expr_eval(flag_expr->op[0]);
            }
        case FLAG_OP_OR:
            {
                return flag_expr_eval(flag_expr->op[0]) || flag_expr_eval(flag_expr->op[1]);
            }
        case FLAG_OP_AND:
            {
                return flag_expr_eval(flag_expr->op[0]) && flag_expr_eval(flag_expr->op[1]);
            }
        case FLAG_OP_TRUE:
            {
                return 1;
            }
        case FLAG_OP_FALSE:
            {
                return 0;
            }
        default:
            {
                internal_error("Invalid flag expr", 0);
                return 0;
            }
    }

    return 0;
}
