/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef CXX_PROCESS_H
#define CXX_PROCESS_H

#include "cxx-driver-decls.h"
#include "cxx-macros.h"
#include <stdlib.h>

#ifdef WIN32_BUILD
  #ifdef LIBPROCESS_DLL_EXPORT
     #define LIBPROCESS_EXTERN extern __declspec(dllexport)
  #else
     #define LIBPROCESS_EXTERN extern __declspec(dllimport)
  #endif
#else
  #define LIBPROCESS_EXTERN extern
#endif

MCXX_BEGIN_DECLS

LIBPROCESS_EXTERN compilation_process_t compilation_process;

LIBPROCESS_EXTERN void add_new_file_to_compilation_process(const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration);

LIBPROCESS_EXTERN unsigned long long int _bytes_dynamic_lists;
LIBPROCESS_EXTERN unsigned long long dynamic_lists_used_memory(void);

// Routine to ease adding pointers to a pointer list
//   list is a T**
//   size is an int
//   elem is a T*
#define P_LIST_ADD(list, size, elem)  \
do { \
    (size)++; \
    (list) = realloc((list), sizeof(*(list))*(size)); \
    _bytes_dynamic_lists += sizeof(*(list)); \
    (list)[((size)-1)] = (elem); \
} while(0)

#define P_LIST_ADD_PREPEND(list, size, elem) \
do {  \
    (size)++; \
    (list) = realloc((list), sizeof(*(list))*(size)); \
    _bytes_dynamic_lists += sizeof(*(list)); \
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

MCXX_END_DECLS

#endif // CXX_PROCESS_H
