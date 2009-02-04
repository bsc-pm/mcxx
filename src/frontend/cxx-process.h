#ifndef CXX_PROCESS_H
#define CXX_PROCESS_H

#include "cxx-driver-decls.h"
#include "cxx-macros.h"
#include <stdlib.h>

#ifdef _WIN32
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
// Like P_LIST_ADD_ONCE_FUN but using fun as an equality function
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
