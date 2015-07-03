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



#include "fortran03-utils.h"
#include "cxx-utils.h"
#include "filename.h"

// Fix this for Windows one day
#include <unistd.h>
#include <string.h>

const char* fortran_find_file_in_directories(int given_num_dirs, 
        const char** given_directories, 
        const char* filename,
        const char* origin_of_include)
{
    DEBUG_CODE()
    {
        fprintf(stderr, "INCLUDE: Finding file '%s' origin is '%s'\n", filename, origin_of_include);
    }
    const char * origin_dir = give_dirname(origin_of_include);

    // Extend the given directories with the directory of the origin file
    const char* directories[given_num_dirs + 1];
    memset(directories, 0, sizeof(directories));

    int i;
    for (i = 0; i < given_num_dirs; i++)
    {
        directories[i] = given_directories[i];
    }
    directories[i] = origin_dir;

    int num_dirs = given_num_dirs + 1;
    for (i = 0; i < num_dirs; i++)
    {
        const char* current_dir = directories[i];
        int length = strlen(current_dir) + 1 + strlen(filename) + 1;
        char c[length];
        memset(c, 0, sizeof(c));

        snprintf(c, length, "%s/%s", current_dir, filename);
        c[length-1] = '\0';

        DEBUG_CODE()
        {
            fprintf(stderr, "INCLUDE: Trying file '%s'\n", c);
        }

        if (access(c, F_OK) == 0)
        {
            DEBUG_CODE()
            {
                fprintf(stderr, "INCLUDE: File '%s' found\n", c);
            }
            return uniquestr(c);
        }
    }

    DEBUG_CODE()
    {
        fprintf(stderr, "INCLUDE: File '%s' NOT found in any of the directories\n", filename);
    }

    return NULL;
}
