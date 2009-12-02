%{

#include <string.h>
#include "cxx-driver.h"
#include "cxx-profile.h"
#include "cxx-configfile-lexer.h"
#include "cxx-configfile-parser.h"
#include "cxx-utils.h"
#include "cxx-typeenviron.h"

static void new_flag_list(profile_flaglist_t* flaglist);
static void add_to_flag_list(profile_flaglist_t*, const char* flag);
static void new_option_list(option_list_t* list);
static void add_to_option_list(option_list_t* list, p_compilation_configuration_line);

static p_compilation_configuration_line process_option_line(
        profile_flaglist_t* flaglist,
        profile_option_name_t* name, 
        const char* option_value);

%}

%union {
    profile_header_t profile_header;
    p_compilation_configuration_line profile_option;
    profile_option_name_t profile_option_name;
    profile_flaglist_t profile_flaglist;
    const char* str;
    option_list_t option_list;
}

%token<str> '!'
%token<str> '['
%token<str> ']'
%token<str> ':'
%token<str> '>'
%token<str> ','
%token<str> CONFIGFILE_NAME "identifier"
%token<str> CONFIGFILE_OPTION_VALUE "option-value"
%token<str> EOL "end-of-line"

%type<profile_header> profile_header

%type<profile_option> option_line

%type<profile_option_name> option_name

%type<profile_flaglist> flaglist_opt
%type<profile_flaglist> flaglist

%type<option_list> profile_body
%type<option_list> option_line_seq
%type<str> option_value

%type<str> flag

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
option_line : flaglist_opt option_name '=' option_value EOL
{
    $$ = process_option_line(&$1, &$2, $4);
}
// Degenerated cases. Do nothing with them
| '{' flaglist '}' EOL
{
    $$ = NULL;
}
| '{' '}' EOL
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

flaglist_opt : '{' flaglist '}'
{
    $$ = $2;
}
// This may be useful sometimes
| '{' '}'
{
    new_flag_list(&$$);
}
// It can be empty
| 
{
    new_flag_list(&$$);
}
;

flaglist : flaglist ',' flag
{
    $$ = $1;
    add_to_flag_list(&$$, $3);
}
| flag
{
    new_flag_list(&$$);
    add_to_flag_list(&$$, $1);
}
;

flag : CONFIGFILE_NAME
{
    $$ = $1;
}
| '!' CONFIGFILE_NAME
{
    $$ = strappend("!", $2);
}
;

%%

static void new_flag_list(profile_flaglist_t* flaglist)
{
    memset(flaglist, 0, sizeof(*flaglist));
}

static void add_to_flag_list(profile_flaglist_t* flaglist, const char* flag)
{
    if (flaglist->num_flags == MAX_FLAGS)
    {
        running_error("Too many flags (%d) in option line\n", MAX_FLAGS);
    }
    flaglist->flags[flaglist->num_flags] = flag;
    flaglist->num_flags++;
}

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

static p_compilation_configuration_line process_option_line(
        profile_flaglist_t* flaglist,
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
    result->value = uniquestr(option_value_tmp);

    free(option_value_tmp);

    result->num_flags = flaglist->num_flags;
    result->flags = calloc(result->num_flags, sizeof(*result->flags)); 

    int i;
    for (i = 0; i < flaglist->num_flags; i++)
    {
        const char *current_flag = NULL;
        char is_negative = 0;

        // fprintf(stderr, "-->   Flag: '%s'\n", flaglist->flags[i]);

        // If the flag is '!flag'
        if (flaglist->flags[i][0] == '!')
        {
            // Do not copy '!'
            current_flag = uniquestr(&(flaglist->flags[i][1]));
            // And state it is negative
            is_negative = 1;
        }
        else
        {
            // Otherwise just keep the flag
            current_flag = uniquestr(flaglist->flags[i]);
        }

        result->flags[i].flag = current_flag;
        result->flags[i].value = !is_negative;

        {
            // Now register in compilation process as valid flag
            char found = 0;
            int j;
            for (j = 0; !found && (j < compilation_process.num_parameter_flags); j++)
            {
                found |= (strcmp(current_flag, compilation_process.parameter_flags[j]->name) == 0);
            }

            if (!found)
            {
                struct parameter_flags_tag *new_parameter_flag = calloc(1, sizeof(*new_parameter_flag));

                new_parameter_flag->name = current_flag;
                // This is redundant because of calloc, but make it explicit here anyway
                new_parameter_flag->value = 0;

                P_LIST_ADD(compilation_process.parameter_flags, 
                        compilation_process.num_parameter_flags,
                        new_parameter_flag);
            }
        }

    }

    return result;
}
