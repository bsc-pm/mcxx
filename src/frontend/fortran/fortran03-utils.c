#include "fortran03-utils.h"
#include "cxx-utils.h"

const char* find_file_in_directories(int num_dirs UNUSED_PARAMETER, 
        const char** directories UNUSED_PARAMETER, 
        const char* filename UNUSED_PARAMETER, 
        const char* mode UNUSED_PARAMETER)
{
    internal_error("Not yet implemented\n", 0);
}
