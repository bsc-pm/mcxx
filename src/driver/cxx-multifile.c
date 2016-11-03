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




#include "cxx-multifile.h"
#include "cxx-utils.h"
#include "cxx-driver-utils.h"
#include "filename.h"

#include <sys/types.h>
#include <dirent.h>

#include <string.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>

#include <unistd.h>

#ifdef WIN32_BUILD
  #include <windows.h>
  #include <shellapi.h>
#endif

// int stat(const char *restrict path, struct stat *restrict buf);

static const char* get_multifile_dir(void)
{
    if (CURRENT_CONFIGURATION->multifile_dir == NULL)
    {
        temporal_file_t temporal_dir = new_temporal_dir();
        CURRENT_CONFIGURATION->multifile_dir = temporal_dir->name;
    }
    return CURRENT_CONFIGURATION->multifile_dir;
}

void multifile_init_dir(void)
{
    // This ensures it is initialized at the next get_multifile_dir
    CURRENT_CONFIGURATION->multifile_dir = NULL;
}


static void multifile_extract_extended_info_single_object(const char* filename)
{
    char only_section[256] = { 0 };
    snprintf(only_section, 255, "--only-section=%s", MULTIFILE_SECTION);
    only_section[255] = '\0';

    char output_filename[1024] = { 0 };
    snprintf(output_filename, 1023, "%s%s%s", get_multifile_dir(), DIR_SEPARATOR, MULTIFILE_TAR_FILE);
    output_filename[1023] = '\0';

    const char* arguments_objcopy[] = {
        "-Obinary",
        only_section,
        filename,
        output_filename,
        NULL
    };

    if (execute_program(CURRENT_CONFIGURATION->target_objcopy, arguments_objcopy) != 0)
    {
        fatal_error("Error when extracting the object file data");
    }

    // Now extract the tar

    const char* arguments_tar[] = {
        "xf",
        output_filename,
        "-C",
        get_multifile_dir(),
        ".",
        NULL
    };

    if (execute_program("tar", arguments_tar) != 0)
    {
        fatal_error("Error when extracting the object file tar");
    }

    // Now remove the file
    if (remove(output_filename) < 0)
    {
        fatal_error("Error when removing temporal file '%s'. %s\n", 
                output_filename,
                strerror(errno));
    }
}

void multifile_extract_extended_info(const char* filename)
{
    // Maybe we should detect the file instead of relying on the extension?
    const char* extension = get_extension_filename(filename);
    if (extension != NULL
            && strcmp(extension, ".a") == 0)
    {
        // Note we rely on the POSIX 2008 behaviour, maybe we need a check in
        // configure?
        char *full_path = realpath(filename, NULL);

        char current_directory[1024] = { 0 };
        getcwd(current_directory, 1023);
        current_directory[1023] = '\0';

        // Change to temporal directory
        temporal_file_t temporal_dir = new_temporal_dir();
        int r = chdir(temporal_dir->name);
        if (r < 0)
        {
            fatal_error("Error during chdir to '%s'. %s\n",
                    temporal_dir->name,
                    strerror(errno));
        }

        const char* list_arguments[] = {
            "x",
            full_path,
            NULL,
        };

        if (execute_program(CURRENT_CONFIGURATION->target_ar, list_arguments) != 0)
        {
            fatal_error("Error while extracting members of archive");
        }
        DELETE(full_path);

        // Go back to previous directory
        r = chdir(current_directory);
        if (r < 0)
        {
            fatal_error("Error during chdir to '%s'. %s\n",
                    temporal_dir->name,
                    strerror(errno));
        }

        DIR* archive_dir = opendir(temporal_dir->name);
        // Now scan the temporary directory where we unpacked the archive
        struct dirent *dir_entry;

        dir_entry = readdir(archive_dir);
        while (dir_entry != NULL)
        {
            struct stat buf;
            memset(&buf, 0, sizeof(buf));

            char full_path[1024] = { 0 };

            snprintf(full_path, 1023, "%s%s%s", temporal_dir->name, DIR_SEPARATOR, dir_entry->d_name);
            full_path[1023] = '\0';

            // Recursively extract the info from this object (or maybe nested .a file, is this possible?)
            if (stat(full_path, &buf) == 0)
            {
                if (!S_ISDIR(buf.st_mode))
                {
                    if (multifile_object_has_extended_info(full_path))
                    {
                        multifile_extract_extended_info(full_path);
                    }
                }
            }
            dir_entry = readdir(archive_dir);
        }

        closedir(archive_dir);
    }
    else
    {
        multifile_extract_extended_info_single_object(filename);
    }
}

// This routine works both for .a and for .o thanks to objdump
char multifile_object_has_extended_info(const char* filename)
{
    // If the file cannot be accessed by some reason, ignore it
    // and let the linker fail later
    if (access(filename, R_OK) != 0)
        return 0;

    temporal_file_t temp = new_temporal_file();

    const char* arguments[] =
    {
        "-w",
        "-h",
        filename,
        NULL
    };

    if (execute_program_flags(CURRENT_CONFIGURATION->target_objdump,
                arguments, /* stdout_f */ temp->name, /* stderr_f */ NULL) != 0)
    {
        fatal_error("Error when identifying object file '%s'", filename);
    }

    FILE* stdout_file = fopen(temp->name, "r");

    if (stdout_file == NULL)
    {
        fatal_error("Error when examining output of 'objdump' of file '%s'", filename);
    }

    char line[256];

    char result = 0;

    while (fgets(line, 255, stdout_file) != NULL)
    {
        if ((strlen(line) > 1))
        {
            const char *q = line;

            // First blank part
            while (*q != '\0'
                    && (*q == ' ' || *q == '\t'))
                q++;

            if (*q == '\0')
                continue;

            // Index of the section
            while (*q != '\0'
                    && *q != ' '
                    && isdigit(*q))
                q++;

            if (*q == '\0' 
                    || *q != ' ')
                continue;

            // Second blank part
            while (*q != '\0'
                    && (*q == ' ' || *q == '\t'))
                q++;

            if (*q == '\0')
                continue;

            // Section name
            const char* p = q;

            while (*p != '\0'
                    && *p != ' ')
                p++;

            if (*p == '\0')
                continue;

            int num_chars = p - q;
            char name[num_chars + 1];
            strncpy(name, q, num_chars);
            name[num_chars] = '\0';

            if (strcmp(name, MULTIFILE_SECTION) == 0)
            {
                result = 1;
                break;
            }

        }
    }

    fclose(stdout_file);

    return result;
}


void multifile_get_extracted_profiles(
        multifile_extracted_profile_t** multifile_extracted_profile,
        int *num_multifile_profiles)
{
    // Profiles are stored in get_multifile_dir()/<directory>, each directory being a profile
    DIR* multifile_dir = opendir(get_multifile_dir());
    if (multifile_dir == NULL)
    {
        if (errno != ENOENT)
        {
            // Only give an error if it does exist
            fatal_error("Cannot open multifile directory '%s'", get_multifile_dir());
        }
    }
    else
    {
        struct dirent *dir_entry;

        dir_entry = readdir(multifile_dir);
        while (dir_entry != NULL)
        {
            struct stat buf;
            memset(&buf, 0, sizeof(buf));

            char full_path[1024] = { 0 };

            snprintf(full_path, 1023, "%s%s%s", get_multifile_dir(), DIR_SEPARATOR, dir_entry->d_name);
            full_path[1023] = '\0';

            if (stat(full_path, &buf) == 0)
            {
                if (S_ISDIR(buf.st_mode)
                        && dir_entry->d_name[0] != '.')
                {
                    multifile_extracted_profile_t new_extracted_profile;
                    memset(&new_extracted_profile, 0, sizeof(new_extracted_profile));

                    if (strncmp(dir_entry->d_name, "tag.", strlen("tag.")) == 0)
                    {
                        // Now extract the tag number and then the profile name
                        const char* p = dir_entry->d_name + strlen("tag.");
                        int tag = 0;
                        while (*p >= '0'
                                && *p <= '9')
                        {
                            tag += 10*tag + (*p - '0');
                            p++;
                        }
                        // We only create tagged directories for nonzero tags
                        ERROR_CONDITION (tag == 0,
                                "Invalid tag extracted from multifile directory '%s'\n",
                                dir_entry->d_name);

                        // We expect a dot here
                        ERROR_CONDITION(*p != '.',
                                "Malformed multifile directory '%s'\n",
                                dir_entry->d_name);
                        p++;

                        new_extracted_profile.name = uniquestr(p);
                        new_extracted_profile.tag = tag;
                    }
                    else
                    {
                        new_extracted_profile.name = uniquestr(dir_entry->d_name);
                    }
                    P_LIST_ADD(*multifile_extracted_profile,
                            *num_multifile_profiles,
                            new_extracted_profile);
                }
            }

            dir_entry = readdir(multifile_dir);
        }

        closedir(multifile_dir);
    }
}

void multifile_get_profile_file_list(
        const multifile_extracted_profile_t* multifile_extracted_profile,
        const char*** multifile_file_list,
        int *num_multifile_files)
{
    char profile_dir[1024];

    if (multifile_extracted_profile->tag == 0)
    {
        snprintf(profile_dir, 1023,
                "%s%s%s",
                get_multifile_dir(),
                DIR_SEPARATOR,
                multifile_extracted_profile->name);
    }
    else
    {
        snprintf(profile_dir, 1023,
                "%s%stag.%d.%s",
                get_multifile_dir(),
                DIR_SEPARATOR,
                multifile_extracted_profile->tag,
                multifile_extracted_profile->name);
    }
    profile_dir[1023] = '\0';

    DIR* multifile_dir = opendir(profile_dir);
    if (multifile_dir == NULL)
    {
        if (errno != ENOENT)
        {
            // Only give an error if it does exist
            fatal_error("Cannot open multifile profile directory '%s'", profile_dir);
        }
    }
    else
    {
        struct dirent *dir_entry;

        dir_entry = readdir(multifile_dir);
        while (dir_entry != NULL)
        {
            struct stat buf;
            memset(&buf, 0, sizeof(buf));

            char full_path[1024] = { 0 };

            snprintf(full_path, 1023, "%s%s%s", profile_dir, DIR_SEPARATOR, dir_entry->d_name);
            full_path[1023] = '\0';

            if (stat(full_path, &buf) == 0)
            {
                if (dir_entry->d_name[0] != '.')
                {
                    const char* profile_name = strappend(profile_dir, 
                            strappend(DIR_SEPARATOR, dir_entry->d_name));
                    P_LIST_ADD(*multifile_file_list, *num_multifile_files, profile_name);
                }
            }

            dir_entry = readdir(multifile_dir);
        }

        closedir(multifile_dir);
    }
}

typedef
struct embed_bfd_data_tag
{
    temporal_file_t temp_dir;
} embed_bfd_data_t;

void multifile_embed_bfd_single(void** data, compilation_file_process_t* secondary_compilation_file)
{
    embed_bfd_data_t* embed_data = NULL;
    if (*data == NULL)
    {
        (*data) = NEW0(embed_bfd_data_t);
         embed_data = (embed_bfd_data_t*)(*data);
        
        // Create the temporal directory
        embed_data->temp_dir = new_temporal_dir();
    }
    else
    {
        embed_data = (embed_bfd_data_t*)(*data);
    }


    translation_unit_t* current_secondary = secondary_compilation_file->translation_unit;
    compilation_configuration_t* secondary_configuration = secondary_compilation_file->compilation_configuration;

    int tag = secondary_compilation_file->tag;

    char dir_path[1024];
    if (tag == 0)
    {
        snprintf(dir_path, 1023, "%s%s%s",
                embed_data->temp_dir->name,
                DIR_SEPARATOR,
                secondary_configuration->configuration_name);
    }
    else
    {
        snprintf(dir_path, 1023, "%s%stag.%d.%s",
                embed_data->temp_dir->name,
                DIR_SEPARATOR,
                tag,
                secondary_configuration->configuration_name);
    }
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
            give_basename(current_secondary->output_filename));

    if (move_file(current_secondary->output_filename, dest_path) != 0)
    {
        fatal_error("When creating multifile archive, file '%s' could not be moved to '%s'\n",
                current_secondary->output_filename,
                dest_path);
    }
}


void multifile_embed_partial_linking_single(void** data, compilation_file_process_t* secondary_compilation_file, const char* output_filename)
{
    // We move the primary object file to a temporary file
    temporal_file_t new_obj_file = new_temporal_file_extension(".o");
    const char* tmp_output_filename = new_obj_file->name;
    if (move_file(output_filename, tmp_output_filename) != 0)
    {
        fatal_error("When partial linking, file '%s' could not be moved to '%s'\n",
                output_filename,
                tmp_output_filename);
    }

    // Once the primary object file has been moved, we can link the new
    // temporal object file with a secondary object file using the relocation flag
    const char* linker_args[] = {
        "-r",
        tmp_output_filename,
        secondary_compilation_file->translation_unit->output_filename,
        "-o",
        output_filename,
        NULL
    };

    if (execute_program(CURRENT_CONFIGURATION->target_ld, linker_args) != 0)
    {
        fatal_error("When partial linking with '%s', relocation failed\n",
                CURRENT_CONFIGURATION->target_ld);
    }
}

void multifile_embed_bfd_collective(void **data, const char* output_filename)
{
    ERROR_CONDITION((*data == NULL), "This cannot be NULL", 0);
    embed_bfd_data_t* embed_data  = (embed_bfd_data_t*)(*data);

    // Now all files have been moved into the temporal directory, run the tar there
    temporal_file_t new_tar_file = new_temporal_file_extension(".tar");
    const char* tar_args[] =
    {
        "cf",
        new_tar_file->name,
        "-C", embed_data->temp_dir->name,
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
        output_filename,
        NULL,
    };

    if (execute_program(CURRENT_CONFIGURATION->target_objcopy, objcopy_args) != 0)
    {
        fatal_error("When creating multifile archive, 'objcopy' failed, if compiling for MIC, set MIC_TOOLS configure flag correctly\n");
    }

    if (CURRENT_CONFIGURATION->verbose)
    {
        fprintf(stderr, "Secondary files successfully embedded into '%s' file\n", 
                output_filename);
    }
}
