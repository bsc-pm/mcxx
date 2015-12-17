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

enum flag_op
{
    FLAG_OP_INVALID = 0,
    FLAG_OP_OR,
    FLAG_OP_AND,
    FLAG_OP_NOT,
    FLAG_OP_NAME,
    FLAG_OP_IS_DEFINED,
    FLAG_OP_TRUE,
    FLAG_OP_FALSE,
};



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
