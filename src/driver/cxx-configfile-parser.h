#ifndef CXX_CONFIGFILE_PARSER_H
#define CXX_CONFIGFILE_PARSER_H

typedef struct profile_header_tag
{
    const char *filename;
    int line;
    const char *profile_name;
    const char *base_profile_name;
} profile_header_t;

typedef struct profile_option_name_tag
{
    const char* option_name;
    const char* option_index;
} profile_option_name_t;

#define MAX_FLAGS 16

typedef struct profile_flaglist_tag
{
    int num_flags;
    const char* flags[MAX_FLAGS];
} profile_flaglist_t;

typedef struct compilation_configuration_line* p_compilation_configuration_line;


typedef struct option_list_tag
{
    int num_options;
    p_compilation_configuration_line* options;
} option_list_t;

typedef struct YYLTYPE
{
    const char* filename;
    int first_line;
    int first_column;
    int last_line;
    int last_column;
} YYLTYPE;

// Do not let bison redefine its own
#define YYLTYPE_IS_DECLARED

# define YYLLOC_DEFAULT(Current, Rhs, N) \
    do \
      if (N) \
        { \
           (Current).filename   = YYRHSLOC(Rhs, 1).filename; \
           (Current).first_line   = YYRHSLOC(Rhs, 1).first_line; \
           (Current).first_column = YYRHSLOC(Rhs, 1).first_column; \
           (Current).last_line    = YYRHSLOC(Rhs, N).last_line; \
           (Current).last_column = YYRHSLOC(Rhs, N).last_column; \
        } \
      else \
        { \
           (Current).filename   = YYRHSLOC(Rhs, 0).filename; \
           (Current).first_line   = (Current).last_line   = \
             YYRHSLOC(Rhs, 0).last_line; \
           (Current).first_column = (Current).last_column = \
             YYRHSLOC(Rhs, 0).last_column; \
        } \
    while (0)


#include "cxx-configfile-parser-internal.h"

int configfileparse(void);
void configfileerror(const char *c);


#endif // CXX_CONFIGFILE_PARSER_H
