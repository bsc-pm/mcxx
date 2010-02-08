#include "cxx-multifile.h"
#include "cxx-utils.h"
#include "cxx-driver-utils.h"

#include <sys/types.h>
#include <dirent.h>

#include <string.h>
#include <sys/stat.h>

#include <ctype.h>
#include <errno.h>

// int stat(const char *restrict path, struct stat *restrict buf);

char multifile_dir_exists(void)
{
    struct stat buf;
    memset(&buf, 0, sizeof(buf));

    if (stat(MULTIFILE_DIRECTORY, &buf) != 0)
    {
        return false;
    }
    else
    {
        return S_ISDIR(buf.st_mode);
    }
}

void multifile_wipe_dir(void)
{
    char c[256];

    // This is a bit lame but it is easier than using nftw
#ifndef WIN32_BUILD
    snprintf(c, 255, "rm -fr %s", MULTIFILE_DIRECTORY);

    if (system(c) != 0)
    {
        running_error("There was a problem when removing multifile temporal directory", 0);
    }
#else
  #error Uninmplemented function yet
#endif
}

void multifile_extract_extended_info(const char* filename)
{
    char only_section[256] = { 0 };
    snprintf(only_section, 255, "--only-section=%s", MULTIFILE_SECTION);
    only_section[255] = '\0';

    char output_filename[256] = { 0 };
    snprintf(output_filename, 255, "%s/%s", MULTIFILE_DIRECTORY, MULTIFILE_TAR_FILE);
    only_section[255] = '\0';

    const char* arguments_objcopy[] = {
        "-Obinary",
        only_section,
        filename,
        output_filename,
        NULL
    };

    if (execute_program("objcopy", arguments_objcopy) != 0)
    {
        running_error("Error when extracting the object file data", 0);
    }

    // Now extract the tar

    const char* arguments_tar[] = {
        "xvf",
        output_filename,
        "-C",
        MULTIFILE_DIRECTORY
    };

    if (execute_program("tar", arguments_tar) != 0)
    {
        running_error("Error when extracting the object file tar", 0);
    }

    // Now remove the file
    if (remove(output_filename) < 0)
    {
        running_error("Error when removing temporal file '%s'. %s\n", 
                output_filename,
                strerror(errno));
    }
}

char multifile_object_has_extended_info(const char* filename)
{
    temporal_file_t temp = new_temporal_file();

    const char* arguments[] =
    {
        "-w", 
        "-h",
        filename,
        NULL
    };

    // Should this 'objdump' be configurable?
    if (execute_program_flags("objdump", arguments, /* stdout_f */ temp->name, /* stderr_f */ NULL) != 0)
    {
        running_error("Error when identifying object file", 0);
    }

    FILE* stdout_file = fopen(temp->name, "r");

    if (stdout_file == NULL)
    {
        running_error("Error when examining output of 'objdump'", 0);
    }

    char line[256];

    char result = 0;

    while (fgets(line, 255, stdout_file) != NULL)
    {
        if ((strlen(line) > 1)
                && line[0] == ' ')
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


void multifile_get_extracted_profiles(const char*** multifile_profiles, int *num_multifile_profiles)
{
    // Profiles are stored in MULTIFILE_DIRECTORY/<directory>, each directory being a profile
    DIR* multifile_dir = opendir(MULTIFILE_DIRECTORY);
    if (multifile_dir == NULL)
    {
        if (errno != ENOENT)
        {
            // Only give an error if it does exist
            running_error("Cannot open multifile directory '%s'", MULTIFILE_DIRECTORY);
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

            snprintf(full_path, 1023, "%s%s%s", MULTIFILE_DIRECTORY, DIR_SEPARATOR, dir_entry->d_name);
            full_path[1023] = '\0';

            if (stat(full_path, &buf) != 0)
            {
                if (S_ISDIR(buf.st_mode)
                        && dir_entry->d_name[0] != '.')
                {
                    const char* profile_name = uniquestr(dir_entry->d_name);
                    P_LIST_ADD(*multifile_profiles, *num_multifile_profiles, profile_name);
                }
            }

            dir_entry = readdir(multifile_dir);
        }

        closedir(multifile_dir);
    }
}

void multifile_get_profile_file_list(const char* profile_name,
        const char*** multifile_file_list,
        int *num_multifile_files)
{
    char profile_dir[1024];

    snprintf(profile_dir, 1023, "%s%s%s", MULTIFILE_DIRECTORY, DIR_SEPARATOR, profile_name);
    profile_dir[1023] = '\0';

    DIR* multifile_dir = opendir(profile_dir);
    if (multifile_dir == NULL)
    {
        if (errno != ENOENT)
        {
            // Only give an error if it does exist
            running_error("Cannot open multifile profile directory '%s'", profile_dir);
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

            if (stat(full_path, &buf) != 0)
            {
                if (dir_entry->d_name[0] != '.')
                {
                    const char* profile_name = uniquestr(dir_entry->d_name);
                    P_LIST_ADD(*multifile_file_list, *num_multifile_files, profile_name);
                }
            }

            dir_entry = readdir(multifile_dir);
        }

        closedir(multifile_dir);
    }
}
