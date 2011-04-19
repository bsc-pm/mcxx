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


#include "string_utils.h"
#include "uniquestr.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

const char* strappend(const char* orig, const char* appended)
{
    int total = strlen(orig) + strlen(appended) + 1;

    char append_tmp[total];
    append_tmp[0] = '\0';

    strcat(append_tmp, orig);
    strcat(append_tmp, appended);

    return uniquestr(append_tmp);
}

const char* strprepend(const char* orig, const char* prepended)
{
    return strappend(prepended, orig);
}

const char* get_unique_name(void)
{
    static int num_var = 100;
    char result[15];

    snprintf(result, 14, "$.anon%05d", num_var);

    return uniquestr(result);
}

const char** comma_separate_values(const char* value, int *num_elems)
{
    const char** result = NULL;
    *num_elems = 0;

    if (value != NULL)
    {
        char* comma_string = strdup(value);
        char* current = strtok(comma_string, ",");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, uniquestr(current));
            current = strtok(NULL, ",");
        }

        free(comma_string);
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}

const char** blank_separate_values(const char* value, int *num_elems)
{
    const char** result = NULL;
    *num_elems = 0;

    if (value != NULL)
    {
        char* comma_string = strdup(value);
        char* current = strtok(comma_string, " \t");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, strdup(current));
            current = strtok(NULL, " \t");
        }

        free(comma_string);
    }

    P_LIST_ADD(result, *num_elems, NULL);
    (*num_elems)--;

    return result;
}


static char is_blank(char c)
{
    return (c == ' ' || c == '\t');
}

char is_blank_string(const char* c)
{
    char result = 1;

    while (result && (c != '\0'))
    {
        result &= is_blank(*c);
        c++;
    }

    return result;
}

const char* strtoupper(const char* c)
{
    if (c != NULL)
    {
        int n = strlen(c);
        char result[n + 1];
        int i;
        for (i = 0; i < n; i++)
        {
            result[i] = toupper(c[i]);
        }
        result[n] = '\0';
        return uniquestr(result);
    }
    return NULL;
}

const char* strtolower(const char* c)
{
    if (c != NULL)
    {
        int n = strlen(c);
        char result[n + 1];
        int i;
        for (i = 0; i < n; i++)
        {
            result[i] = tolower(c[i]);
        }
        result[n] = '\0';
        return uniquestr(result);
    }
    return NULL;
}
