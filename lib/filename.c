#include "filename.h"
#include "uniquestr.h"

#include <libgen.h>
#include <string.h>
#include <stdlib.h>

const char* give_basename(const char* c)
{
    char *tmp = strdup(c);
    char *basename_tmp = basename(tmp);

    const char* result = uniquestr(basename_tmp);
    free(tmp);

    return result;
}

const char* give_dirname(const char* c)
{
    char *tmp = strdup(c);
    char *dirname_tmp = dirname(tmp);

    const char* result = uniquestr(dirname_tmp);
    free(tmp);

    return result;
}

