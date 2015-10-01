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




#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include "cxx-multifile.h"
#include "cxx-driver-utils.h"
#include "cxx-utils.h"
#include "cxx-driver.h"
#include "cxx-embed.h"

#include "filename.h"

char embed_to_file(const char* dest_filename, int num_embed_files, 
        embedded_file_t* embed_files)
{
    // Create the temporal directory
    temporal_file_t temp_dir = new_temporal_dir();

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Embedding files into '%s'\n", dest_filename);
    }

    // For each translation unit create the profile directory if needed
    int j;
    for (j = 0; j < num_embed_files; j++)
    {
        embedded_file_t *current_embed_file = &embed_files[j];
        char dir_path[1024];
        snprintf(dir_path, 1023, "%s%s%s", 
                temp_dir->name, 
                DIR_SEPARATOR, 
                current_embed_file->profile_name);
        dir_path[1023] = '\0';

        struct stat buf;
        int res = stat(dir_path, &buf);

        if (res != 0)
        {
            if (errno == ENOENT)
            {
                // Create the directory if it does not exist
                if (mkdir(dir_path, 0700) != 0)
                {
                    fatal_error("When creating multifile archive, cannot create directory '%s': %s\n",
                            dir_path,
                            strerror(errno));
                }
            }
            else
            {
                fatal_error("Stat failed on '%s': %s\n",
                        dir_path,
                        strerror(errno));
            }
        }
        else
        {
            if (!S_ISDIR(buf.st_mode))
            {
                fatal_error("When creating multifile archive, path '%s' is not a directory\n",
                        dir_path);
            }
        }

        // Now move the secondary file

        char dest_path[1024];
        snprintf(dest_path, 1023, "%s%s%s", 
                dir_path, 
                DIR_SEPARATOR, 
                give_basename(current_embed_file->filename));

        if (move_file(current_embed_file->filename, dest_path) != 0)
        {
            fatal_error("When creating multifile archive, file '%s' could not be moved to '%s'\n",
                    current_embed_file->filename,
                    dest_path);
        }

        // Now all files have been moved into the temporal directory, run the tar there
        temporal_file_t new_tar_file = new_temporal_file_extension(".tar");
        const char* tar_args[] =
        {
            "cf",
            new_tar_file->name,
            "-C", temp_dir->name,
            ".",
            NULL
        };

        if (execute_program("tar", tar_args) != 0)
        {
            fatal_error("When creating multifile archive, 'tar' failed\n");
        }

        // Now we have tar that we are going to embed into the .o file

        // objcopy --add-section .mercurium=architectures.tar --set-section-flags .mercurium=alloc,readonly prova.o

        char multifile_section_and_file[1024], multifile_section_and_flags[1024];

        snprintf(multifile_section_and_file, 1023, "%s=%s",
                MULTIFILE_SECTION, new_tar_file->name);
        multifile_section_and_file[1023] = '\0';

        snprintf(multifile_section_and_flags, 1023, "%s=alloc,readonly",
                MULTIFILE_SECTION);
        multifile_section_and_flags[1023] = '\0';

        const char* objcopy_args[] =
        {
            "--add-section", multifile_section_and_file, 
            "--set-section-flags", multifile_section_and_flags,
            dest_filename,
            NULL,
        };

        if (execute_program(CURRENT_CONFIGURATION->target_objcopy, objcopy_args) != 0)
        {
            fatal_error("When creating multifile archive, 'objcopy' failed\n");
        }

        if (CURRENT_CONFIGURATION->verbose)
        {
            fprintf(stderr, "Secondary files successfully embedded into '%s' file\n", 
                    dest_filename);
        }
    }

    return 1;
}
