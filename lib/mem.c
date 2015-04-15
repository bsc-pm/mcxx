/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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

// This one is guaranteed to call free
// DO NOT CHANGE IT
void c_free(void *ptr)
{
    free(ptr);
}

