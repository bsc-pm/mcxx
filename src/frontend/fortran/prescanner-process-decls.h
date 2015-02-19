/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#ifndef PRESCANNER_PROCESS_DECLS_H
#define PRESCANNER_PROCESS_DECLS_H

#include <stdio.h>

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef
struct prescanner_tag {
    FILE* output_file;
    FILE* input_file;
    const char* output_filename;
    const char* input_filename;
    int width;
    char append;
    char pad_strings;
    char openmp_processing;
    char quiet;
    char line_marks;

    int num_include_directories;
    const char** include_directories;

    const char* output_include_directory;
} prescanner_t;

MCXX_END_DECLS

#endif // PRESCANNER_PROCESS_DECLS_H
