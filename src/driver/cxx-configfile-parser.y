%{

#include <string.h>
#include "cxx-driver.h"
#include "cxx-profile.h"
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include "cxx-utils.h"
#include "cxx-typeenviron.h"

static void new_option_list(option_list_t* list);
static void add_to_option_list(option_list_t* list, p_compilation_configuration_line);

static p_compilation_configuration_line process_option_line(
        flag_expr_t* flag_expr,
        profile_option_name_t* name, 
        const char* option_value);

struct flag_expr_tag
{
    enum flag_op kind;
    struct flag_expr_tag* op[2];
    const char* text;
};

static flag_expr_t* flag_name(const char* name);
static flag_expr_t* flag_not(flag_expr_t* op);
static flag_expr_t* flag_and(flag_expr_t* op1, flag_expr_t* op2);
static flag_expr_t* flag_or(flag_expr_t* op1, flag_expr_t* op2);

static void register_implicit_names(flag_expr_t* flag_expr);

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
    $$ = process_option_line($1, &$2, $4);
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
| '(' flag_expr ')'
{
    $$ = $2;
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
    fprintf(stderr, "warning: error encountered when parsing configuration file: %s\n", c);
}

static void register_implicit_names(flag_expr_t* flag_expr)
{
    if (flag_expr != NULL)
    {
        if (flag_expr->kind == FLAG_OP_NAME)
        {
            struct parameter_flags_tag *new_parameter_flag = calloc(1, sizeof(*new_parameter_flag));

            new_parameter_flag->name = flag_expr->text;
            // This is redundant because of calloc, but make it explicit here anyway
            new_parameter_flag->value = 0;

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
    flag_expr_t* result = calloc(1, sizeof(*result));
    return result;
}

static flag_expr_t* flag_name(const char* name)
{
    flag_expr_t* result = new_flag();

    result->kind = FLAG_OP_NAME;
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
        const char* option_value)
{
    p_compilation_configuration_line result;

    // fprintf(stderr, "--> PROCESSING OPTION LINE -> '%s' = '%s'\n", 
    //         name->option_name, option_value);

    char* option_value_tmp = strdup(option_value);
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

    result = calloc(1, sizeof(*result));

    result->name = uniquestr(name->option_name);
    result->index = uniquestr(name->option_index);
    result->value = uniquestr(option_value_tmp);

    free(option_value_tmp);

    result->flag_expr = flag_expr;

    register_implicit_names(flag_expr);

    return result;
}

char flag_expr_eval(flag_expr_t* flag_expr)
{
    switch (flag_expr->kind)
    {
        case FLAG_OP_NAME:
            {
                // Ugly and inefficient lookup
                char found = 0;
                char value_of_flag = 0;
                int q;
                for (q = 0; !found && q < compilation_process.num_parameter_flags; q++)
                {
                    struct parameter_flags_tag *parameter_flag = compilation_process.parameter_flags[q];
                    found = (strcmp(parameter_flag->name, flag_expr->text) == 0);
                    value_of_flag = parameter_flag->value;
                }

                if (found)
                    return !!value_of_flag;
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
        default:
            {
                internal_error("Invalid flag expr", 0);
                return 0;
            }
    }

    return 0;
}
