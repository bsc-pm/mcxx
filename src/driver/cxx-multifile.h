/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef CXX_MULTIFILE_H
#define CXX_MULTIFILE_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

#define MULTIFILE_DIRECTORY "./.mercurium"
#define MULTIFILE_SECTION ".mercurium"
#define MULTIFILE_TAR_FILE "multifile.tar"

// Directory handling
char multifile_dir_exists(void);
void multifile_remove_dir(void);
void multifile_init_dir(void);

// Extended info handling
char multifile_object_has_extended_info(const char* filename);
void multifile_extract_extended_info(const char* filename);

void multifile_get_extracted_profiles(const char*** multifile_profiles, int *num_multifile_profiles);

void multifile_get_profile_file_list(const char* profile_name,
        const char*** multifile_file_list,
        int *num_multifile_files);

MCXX_END_DECLS

#endif // CXX_MULTIFILE_H
