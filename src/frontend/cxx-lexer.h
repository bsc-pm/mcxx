/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

#ifndef CXX_LEXER_H
#define CXX_LEXER_H

#include <stdio.h>
#include "libmcxx-common.h"
#include "cxx-driver-decls.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef 
struct token_atrib_tag 
{
    const char* token_text;
    const char* token_file;
    int token_line;
} token_atrib_t;

struct scan_file_descriptor 
{
    char in_include_file;
    const char* filename;

    // Current filename due to include lines
    const char* current_filename;
    int line_number;
    int joined_lines;
    FILE* file_descriptor;
    struct yy_buffer_state* scanning_buffer;
};

LIBMCXX_EXTERN struct scan_file_descriptor scanning_now;

LIBMCXX_EXTERN int mcxx_open_file_for_scanning(const char* scanned_filename, const char* input_filename);
LIBMCXX_EXTERN int mc99_open_file_for_scanning(const char* scanned_filename, const char* input_filename);

LIBMCXX_EXTERN int mcxx_prepare_string_for_scanning(const char* str);
LIBMCXX_EXTERN int mc99_prepare_string_for_scanning(const char* str);

LIBMCXX_EXTERN void register_new_directive(const char* prefix, const char* directive, char is_construct);

LIBMCXX_EXTERN pragma_directive_kind_t lookup_pragma_directive(const char* prefix, const char* directive);

LIBMCXX_EXTERN int mc99_flex_debug;
LIBMCXX_EXTERN int mcxx_flex_debug;

LIBMCXX_EXTERN int mcxxdebug;
LIBMCXX_EXTERN int mc99debug;

LIBMCXX_EXTERN void close_scanned_file(void); 

MCXX_END_DECLS

#endif // CXX_LEXER_H
