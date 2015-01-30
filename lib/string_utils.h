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




#ifndef STRING_UTILS_H
#define STRING_UTILS_H

#include "libutils-common.h"
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

LIBUTILS_EXTERN const char* strappend(const char* orig, const char* appended);
LIBUTILS_EXTERN const char* strprepend(const char* orig, const char* prepended);

// This is more efficient than a sequence of strappend
LIBUTILS_EXTERN const char *strconcat_n(unsigned int n, const char** c);

// For scenarios where a sequence of strappend is inefficient
typedef struct strbuilder_tag strbuilder_t;
LIBUTILS_EXTERN strbuilder_t* strbuilder_new(void);
LIBUTILS_EXTERN const char* strbuilder_str(strbuilder_t* strb);
LIBUTILS_EXTERN void strbuilder_append(strbuilder_t*, const char*);
LIBUTILS_EXTERN void strbuilder_free(strbuilder_t*);

// States whether the string is blank
LIBUTILS_EXTERN char is_blank_string(const char* c);

// Separate values
LIBUTILS_EXTERN const char** blank_separate_values(const char* value, int *num_elems);
LIBUTILS_EXTERN const char** comma_separate_values(const char* value, int *num_elems);

// Gives a unique name for the identifier
LIBUTILS_EXTERN const char* get_unique_name(void);

LIBUTILS_EXTERN const char* strtoupper(const char*);
LIBUTILS_EXTERN const char* strtolower(const char*);

LIBUTILS_EXTERN const char* has_prefix(const char* prefix, const char* str);

// Returns 1 if the string contains a prefix with one or more numbers. Otherwise, returns 0.
LIBUTILS_EXTERN unsigned char contain_prefix_number(const char*);

// Merge sort algorithm 
LIBUTILS_EXTERN void  merge_sort_list_str(const char** list, int size,unsigned char ascending_order);


#ifdef __GNUC__
  #if (__GNUC__ == 4 && __GNUC_MINOR__ >= 4) || (__GNUC__ > 4)
     #define CHECK_PRINTF_STR __attribute__ ((format (gnu_printf, 2, 3)))
  #else
     #define CHECK_PRINTF_STR __attribute__ ((format (printf, 2, 3)))
  #endif
#else
  #define CHECK_PRINTF_STR
#endif

// Like asprintf but returning a uniquestr
LIBUTILS_EXTERN CHECK_PRINTF_STR int uniquestr_sprintf(const char** out_str, const char* format, ...);
LIBUTILS_EXTERN int uniquestr_vsprintf(const char** out_str, const char* format, va_list args);

LIBUTILS_EXTERN unsigned int simple_hash_str(const char *str);

// Packed pointers
LIBUTILS_EXTERN const char* pack_pointer(const char* prefix, void* pointer_addr);
LIBUTILS_EXTERN void unpack_pointer(const char* text, const char** prefix, void** pointer_addr);

// Routine to ease adding pointers to a pointer list
//   list is a T**
//   size is an int
//   elem is a T*
#define P_LIST_ADD(list, size, elem)  \
do { \
    (size)++; \
    (list) = (__typeof__(list))xrealloc((list), sizeof(*(list))*(size)); \
    (list)[((size)-1)] = (elem); \
} while(0)

#define P_LIST_ADD_PREPEND(list, size, elem) \
do {  \
    (size)++; \
    (list) = (__typeof__(list))xrealloc((list), sizeof(*(list))*(size)); \
    int _i; \
    for (_i = (size) - 1; _i > 0; _i--) \
    { \
        (list)[_i] = (list)[_i - 1]; \
    } \
    (list)[0] = elem; \
} while(0)

// This is a bit inefficient. Should not be used for large lists
#define P_LIST_ADD_ONCE(list, size, elem) \
do { \
    int _i; \
    char _found = 0; \
    for (_i = 0; (_i < (size)) && !_found; _i++) \
    { \
         _found = ((list)[_i] == (elem)); \
    } \
    if (!_found) \
    { \
        P_LIST_ADD((list), (size), (elem)); \
    } \
} while (0)

// This is a bit inefficient. Should not be used for large lists
// Like P_LIST_ADD_ONCE but using fun as an equality function
#define P_LIST_ADD_ONCE_FUN(list, size, elem, fun) \
do { \
    int _i; \
    char _found = 0; \
    for (_i = 0; (_i < (size)) && !_found; _i++) \
    { \
         _found = (fun)((list)[_i], (elem)); \
    } \
    if (!_found) \
    { \
        P_LIST_ADD((list), (size), (elem)); \
    } \
} while (0)

#define P_LIST_REMOVE(list, size, elem) \
do { \
    int _i; \
    char _found = 0; \
    for (_i = 0; (_i < (size)) && !_found; _i++) \
    { \
        _found = ((list)[_i] == (elem)); \
    } \
    if (_found) \
    {\
        for (; (_i < (size)); _i++) \
        { \
            ((list)[_i -1]) = ((list)[_i]); \
        } \
        (size)--; \
        (list) = (__typeof__(list))xrealloc((list), sizeof(*(list))*(size)); \
    }\
} while (0)

#define P_LIST_REMOVE_FUN(list, size, elem, fun) \
do { \
    int _i; \
    char _found = 0; \
    for (_i = 0; (_i < (size)) && !_found; _i++) \
    { \
        _found = (fun)((list)[_i], (elem)); \
    } \
    if (_found) \
    {\
        for (; (_i < (size)); _i++) \
        { \
            ((list)[_i -1]) = ((list)[_i]); \
        } \
        (size)--; \
        (list) = (__typeof__(list))xrealloc((list), sizeof(*(list))*(size)); \
    }\
} while (0)

#ifdef __cplusplus
}
#endif

#endif
