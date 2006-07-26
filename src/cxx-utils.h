#ifndef CXX_UTILS_H
#define CXX_UTILS_H

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>

#include <gc.h>

// Some useful macros
#ifndef __GNUC__
    #define __attribute__(x)
#endif

#ifdef __GNUC__
  #if __GNUC__ >= 3 
     #define NORETURN __attribute__((noreturn))

     #if __GNUC_MINOR__ >= 4
         #define WARN_UNUSED __attribute__((warn_unused_result))
     #else
         #define WARN_UNUSED
     #endif
  #elif __GNUC__ == 2
     #error "This code will not compile with GCC 2"
  #endif
#endif

void running_error(char* message, ...) NORETURN;

#define internal_error(message, ...) \
{ \
	debug_message(message, "Internal compiler error. Please report bug:\n", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__ ); \
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
		raise(SIGABRT); \
	} \
}

#define ERROR_CONDITION(cond, message, ...) \
{ if ((cond)) \
	{ \
		debug_message((message), "Error condition (" #cond ")\n\t", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
		raise(SIGABRT); \
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
	(list) = GC_REALLOC((list), sizeof(*(list))*(size)); \
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

char* GC_STRDUP(const char* str);

#define BITMAP_TEST(x, b) (((x) & (b)) == (b))

#define DEBUG_CODE() if (compilation_options.debug_level)

// Gives a unique name for the identifier
char* get_unique_name(void);

char** comma_separate_values(char* value, int* num_elems);

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

#endif // CXX_UTILS_H
