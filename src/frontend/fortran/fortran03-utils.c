#include "fortran03-utils.h"
#include "cxx-utils.h"
#include "filename.h"

// Fix this for Windows one day
#include <unistd.h>
#include <string.h>

const char* find_file_in_directories(int given_num_dirs, 
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
