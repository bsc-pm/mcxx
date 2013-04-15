#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <string.h>

#include "mem.h"

#undef malloc
#undef calloc
#undef free
#undef realloc
#undef strdup

#define OUT_OF_MEM(_size) \
    do { \
        fprintf(stderr, "%s: allocation failure of %zd bytes\n", __FUNCTION__, (_size)); \
        raise(SIGABRT); \
        return NULL; \
    } \
    while (0)

void *xmalloc(size_t size)
{
    if (size == 0)
        return NULL;

    void* ptr = malloc(size);
    if (ptr == NULL)
    {
        OUT_OF_MEM(size);
    }
    else
    {
        return ptr;
    }
}

void xfree(void *ptr)
{
    if (ptr != NULL)
        free(ptr);
}

void *xcalloc(size_t nmemb, size_t size)
{
    if (nmemb == 0
            || size == 0)
        return NULL;

    void* ptr = calloc(nmemb, size);
    if (ptr == NULL)
    {
        OUT_OF_MEM(nmemb * size);
    }
    else
    {
        return ptr;
    }
}

void *xrealloc(void *ptr, size_t size)
{
    if (size == 0)
    {
        xfree(ptr);
        return NULL;
    }
    else
    {
        void *res = realloc(ptr, size);
        if (res == NULL)
        {
            OUT_OF_MEM(size);
        }
        else
        {
            return res;
        }
    }
}

char *xstrdup(const char *s)
{
    char* result = strdup(s);

    if (result == NULL)
    {
        OUT_OF_MEM(strlen(s) + 1);
    }

    return result;
}

