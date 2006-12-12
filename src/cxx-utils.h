#ifndef CXX_UTILS_H
#define CXX_UTILS_H

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sys/time.h>
#include <time.h>

#include "cxx-driver.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

void running_error(char* message, ...) NORETURN;

#define internal_error(message, ...) \
{ \
    debug_message(message, "Internal compiler error. Please report bug:\n", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__ ); \
    if (compilation_options.debug_options.abort_on_ice) \
            raise(SIGABRT); \
    exit(EXIT_FAILURE); \
}

void debug_message(const char* message, const char* kind, const char* source_file, int line, const char* function_name, ...);

#define WARNING_MESSAGE(message, ...) \
{ \
    debug_message(message, "Warning: ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}

#define ASSERT_MESSAGE(cond, message, ...) \
{ if (!(cond)) \
    { \
        debug_message((message), "Assertion failed (" #cond ")\n\t", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (compilation_options.debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define ERROR_CONDITION(cond, message, ...) \
{ if ((cond)) \
    { \
        debug_message((message), "Error condition (" #cond ")\n\t", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (compilation_options.debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define DEBUG_MESSAGE(message, ...) \
{ \
    debug_message(message, "Debug : ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}

#define HASH_SIZE 23
int prime_hash(char* key, int hash_size);

char* strappend(char* orig, char* appended);
char* strprepend(char* orig, char* prepended);

// Routine to ease adding pointers to a pointer list
//   list is a T**
//   size is an int
//   elem is a T*
#define P_LIST_ADD(list, size, elem)  \
do { \
    (size)++; \
    (list) = realloc((list), sizeof(*(list))*(size)); \
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

#define DEBUG_CODE() if (compilation_options.debug_options.enable_debug_code)
#define NOT_DEBUG_CODE() if (!compilation_options.debug_options.enable_debug_code)

#define CXX_LANGUAGE() if (compilation_options.source_language == SOURCE_LANGUAGE_CXX)
#define C_LANGUAGE() if (compilation_options.source_language == SOURCE_LANGUAGE_C)

// Gives a unique name for the identifier
char* get_unique_name(void);


// Temporal handling routines
typedef struct 
{
    FILE* file;
    char* name;
}* temporal_file_t;

// Gives you a new temporal file that will be removed when
// finishing the program
temporal_file_t new_temporal_file();

// Routine that does the cleanup. Can be atexit-registered
// or used discretionally inside the program. Every temporal
// file is closed and erased.
void temporal_files_cleanup(void);

char* get_extension_filename(char* filename);

int execute_program(char* program_name, char** arguments);

// char** routines
char** comma_separate_values(char* value, int* num_elems);
int count_null_ended_array(void** v);

// Table of seen filenames
void seen_filename(char* filename);
char* reference_to_seen_filename(char* filename);

typedef struct
{
  struct timeval start;
  struct timeval end;
  double elapsed_time;
} timing_t;

void timing_start(timing_t* t);
void timing_end(timing_t* t);
int timing_seconds(const timing_t* t);
int timing_microseconds(const timing_t* t);
double timing_elapsed(const timing_t* t);

char is_blank_string(const char* c);

char* give_dirname(const char* c);
char* give_basename(const char* c);

MCXX_END_DECLS

#endif // CXX_UTILS_H
