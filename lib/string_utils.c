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


unsigned char contain_prefix_number(const char* c) 
{
    if (strlen(c) == 0) return 0; 
    return ('0' <= c[0] &&  c[0] <= '9');
}

// merge sort functions 
void private_fusion(char **list, int ind_lower, int ind_upper, unsigned char ascending_order)
{
    char ** aux_list = NULL;
    char * str;
    int i, j, k, middle, min, res, num_str;
    num_str = 0;

    middle = ind_lower + (ind_upper - ind_lower)/2;
    for(i = ind_lower; i <= middle; ++i) 
    {
        char * str = (char *) calloc(strlen(list[i]), sizeof(char));
        strncpy(str, list[i], strlen(list[i]));
        P_LIST_ADD(aux_list, num_str, str);
    }

    for(j = middle + 1; j <= ind_upper; ++j)
    {
        char * str = (char *) calloc(strlen(list[ind_upper - j + middle + 1]), sizeof(char));
        strncpy(str, list[ind_upper - j + middle + 1], strlen(list[ind_upper - j + middle + 1]));
        P_LIST_ADD(aux_list, num_str, str);
    }
    
    i = 0;
    j = ind_upper - ind_lower;
    if(ascending_order)
    {
        for(k = ind_lower; k <= ind_upper; ++k) 
        {
            //deallocating the actual string of the list
            free(list[k]);

            //calculating the minimum lenght beetwen two strings
            if (strlen(aux_list[i]) < strlen(aux_list[j])) min = strlen(aux_list[i]);
            else min = strlen(aux_list[j]);
            
            
            if ( (res = strncmp(aux_list[i], aux_list[j], min)) > 0 || 
               (res == 0 && (strlen(aux_list[j]) < strlen(aux_list[i]))))
            {
                str = (char *) calloc(strlen(aux_list[j]), sizeof(char));
                strncpy(str, aux_list[j], strlen(aux_list[j]));
                j--;
            }
            else
            {
                str = (char *) calloc(strlen(aux_list[i]), sizeof(char));
                strncpy(str, aux_list[i], strlen(aux_list[i]));
                i++;
            }
            list[k] = str;
        }
    }
    else
    {
        for(k = ind_lower; k <= ind_upper; ++k) 
        {
            //deallocating the actual string of the list
            free(list[k]);

            //calculating the minimum lenght beetwen two strings
            if (strlen(aux_list[i]) < strlen(aux_list[j])) min = strlen(aux_list[i]);
            else min = strlen(aux_list[j]);
            
            
            if ( (res = strncmp(aux_list[i], aux_list[j], min)) < 0 || 
               (res == 0 && (strlen(aux_list[j]) > strlen(aux_list[i]))))
            {
                str = (char *) calloc(strlen(aux_list[j]), sizeof(char));
                strncpy(str, aux_list[j], strlen(aux_list[j]));
                j--;
            }
            else
            {
                str = (char *) calloc(strlen(aux_list[i]), sizeof(char));
                strncpy(str, aux_list[i], strlen(aux_list[i]));
                i++;
            }
            list[k] = str;
        }
    }
}



void private_merge_sort_str(char** list, int ind_lower, int ind_upper, unsigned char ascending_order)
{
    if(ind_upper - ind_lower > 0)
    {
        private_merge_sort_str(list,ind_lower,ind_lower + (ind_upper - ind_lower)/2, ascending_order);
        private_merge_sort_str(list,(ind_lower + (ind_upper - ind_lower)/2)+1, ind_upper, ascending_order);
        private_fusion(list, ind_lower, ind_upper, ascending_order);
    }
}

void  merge_sort_list_str(char** list, int size,unsigned char ascending_order) 
{
   private_merge_sort_str(list, 0, size-1, ascending_order);
}
