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



#include "string_utils.h"
#include "uniquestr.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "mem.h"

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

const char *strconcat_n(unsigned int n, const char** c)
{
    if (n == 0)
        return NULL;
    if (n == 1)
        return c[0];

    if (n == 2)
        return strappend(c[0], c[1]);

    unsigned int size = 0;
    unsigned int i;
    for (i = 0; i < n; i++)
    {
        if (c[i] != NULL)
            size += strlen(c[i]);
    }

    if (size == 0)
        return NULL;

    char result[size + 1];
    result[0] = '\0';

    for (i = 0; i < n; i++)
    {
        if (c[i] != NULL)
        {
            strcat(result, c[i]);
        }
    }
    result[size - 1] = '\0';

    return uniquestr(result);
}

struct strbuilder_tag
{
    char* str;
    unsigned int capacity;
    unsigned int position;
};

strbuilder_t* strbuilder_new(void)
{
    strbuilder_t* strb = NEW0(strbuilder_t);

    strb->capacity = 8;
    strb->str = NEW_VEC(char, strb->capacity);
    strb->str[0] = '\0';

    return strb;
}

const char* strbuilder_str(strbuilder_t* strb)
{
    return strb->str;
}

void strbuilder_append(strbuilder_t* strb, const char* str)
{
    unsigned int len = strlen(str);
    while ((strb->position + len) >= strb->capacity)
    {
        strb->capacity *= 2;
        strb->str = NEW_REALLOC(char, strb->str, strb->capacity);
    }

    strcat(strb->str, str);
    strb->position += len;
}

void strbuilder_free(strbuilder_t* strb)
{
    xfree(strb->str);
    xfree(strb);
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
        char* comma_string = xstrdup(value);
        char* current = strtok(comma_string, ",");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, uniquestr(current));
            current = strtok(NULL, ",");
        }

        xfree(comma_string);
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
        char* comma_string = xstrdup(value);
        char* current = strtok(comma_string, " \t");

        while (current != NULL)
        {
            P_LIST_ADD(result, *num_elems, xstrdup(current));
            current = strtok(NULL, " \t");
        }

        xfree(comma_string);
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

    while (result && (*c != '\0'))
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

const char* has_prefix(const char* prefix, const char* str)
{
    if (strncmp(prefix, str, strlen(prefix)) == 0)
    {
        return str + strlen(prefix);
    }
    else
    {
        return NULL;
    }
}

unsigned char contain_prefix_number(const char* c) 
{
    if (strlen(c) == 0) return 0; 
    return ('0' <= c[0] &&  c[0] <= '9');
}

// merge sort functions 
static void private_fusion(const char **list, int ind_lower, int ind_upper, unsigned char ascending_order)
{
    const char ** aux_list = NULL;
    int i, j, k, middle, min, res, num_str;
    num_str = 0;

    middle = ind_lower + (ind_upper - ind_lower)/2;
    for(i = ind_lower; i <= middle; ++i) 
    {
        P_LIST_ADD(aux_list, num_str, list[i]);
    }

    for(j = middle + 1; j <= ind_upper; ++j)
    {
        P_LIST_ADD(aux_list, num_str, list[ind_upper - j + middle + 1]);
    }

    i = 0;
    j = ind_upper - ind_lower;
    const char * str;
    if(ascending_order)
    {
        for(k = ind_lower; k <= ind_upper; ++k) 
        {
            //calculating the minimum lenght beetwen two strings
            if (strlen(aux_list[i]) < strlen(aux_list[j])) min = strlen(aux_list[i]);
            else min = strlen(aux_list[j]);
            
            if ( (res = strncmp(aux_list[i], aux_list[j], min)) > 0 || 
               (res == 0 && (strlen(aux_list[j]) < strlen(aux_list[i]))))
            {
                str = aux_list[j];
                j--;
            }
            else
            {
                str = aux_list[i];
                i++;
            }
            list[k] = str;
        }
    }
    else
    {
        for(k = ind_lower; k <= ind_upper; ++k) 
        {
            //calculating the minimum lenght beetwen two strings
            if (strlen(aux_list[i]) < strlen(aux_list[j])) min = strlen(aux_list[i]);
            else min = strlen(aux_list[j]);
            
            if ( (res = strncmp(aux_list[i], aux_list[j], min)) < 0 || 
               (res == 0 && (strlen(aux_list[j]) > strlen(aux_list[i]))))
            {
                str = aux_list[j];
                j--;
            }
            else
            {
                str = aux_list[i];
                i++;
            }
            list[k] = str;
        }
    }

    xfree(aux_list);
}

static void private_merge_sort_str(const char** list, int ind_lower, int ind_upper, unsigned char ascending_order)
{
    if(ind_upper - ind_lower > 0)
    {
        private_merge_sort_str(list,ind_lower,ind_lower + (ind_upper - ind_lower)/2, ascending_order);
        private_merge_sort_str(list,(ind_lower + (ind_upper - ind_lower)/2)+1, ind_upper, ascending_order);
        private_fusion(list, ind_lower, ind_upper, ascending_order);
    }
}

void  merge_sort_list_str(const char** list, int size,unsigned char ascending_order) 
{
   private_merge_sort_str(list, 0, size-1, ascending_order);
}

int uniquestr_vsprintf(const char** out_str, const char* format, va_list args)
{
    int result;
    int size = 512;
    char* c = NEW_VEC(char, size);
    va_list va;

    va_copy(va, args);
    result = vsnprintf(c, size, format, va);
    va_end(va);

    while (result < 0 || result >= size)
    {
        va_copy(va, args);
        size *= 2;
        c = NEW_REALLOC(char, c, size);
        result = vsnprintf(c, size, format, va);
        va_end(va);
    }

    *out_str = uniquestr(c); 

    xfree(c);
    return result;
}

int uniquestr_sprintf(const char** out_str, const char* format, ...)
{
    int result = 0;
    va_list args;

    va_start(args, format);
    result = uniquestr_vsprintf(out_str, format, args);
    va_end(args);

    return result;
}

unsigned int simple_hash_str(const char *str)
{
    const int MULTIPLIER = 33;
    unsigned int h;
    unsigned const char *p;

    h = 0;
    for (p = (unsigned const char*)str; *p != '\0'; p++)
        h = MULTIPLIER * h + *p;

    h += (h >> 5);

    return h; // or, h % ARRAY_SIZE;
}

// Packed pointers in strings
//
#define PACK_POINTER_FORMAT_WRITE "\"%s:%p\""
const char* pack_pointer(const char* prefix, void* pointer_addr)
{
    const char* result = NULL;
    uniquestr_sprintf(&result, PACK_POINTER_FORMAT_WRITE, prefix, pointer_addr);
    return result;
}

#define PACK_POINTER_FORMAT_READ "\"%255[^:]:%p\""
void unpack_pointer(const char* text, 
        // out
        const char** prefix, void** pointer_addr)
{
    if (text == NULL)
    {
        *prefix = NULL;
        *pointer_addr = NULL;
        return;
    }

    char tmp[256] = { 0 };

    int matched = sscanf(text, PACK_POINTER_FORMAT_READ, tmp, pointer_addr);
    if (matched != 2)
    {
        *prefix = NULL;
        *pointer_addr = NULL;
        return;
    }

    *prefix = uniquestr(tmp);
}
