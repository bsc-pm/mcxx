#include "gcstring.h"
#include <string.h>
#include <gc.h>

char* GC_STRDUP(const char* str)
{
    char* result = GC_CALLOC(strlen(str) + 1, sizeof(char));

    strcpy(result, str);

    return result;
}
