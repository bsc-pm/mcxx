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
#ifndef CXX_UTILS_H
#define CXX_UTILS_H

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "cxx-driver.h"
#include "cxx-macros.h"
#include "uniquestr.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void running_error(const char* message, ...) NORETURN;

#define BUG_URL "\nPlease report a bug at\n" \
                "http://nanos.ac.upc.edu/projects/mcxx/newticket\n" \
                "with preprocessed source if possible\n"

#define internal_error(message, ...) \
{ \
    debug_message(message, "Internal compiler error."BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__ ); \
    if (CURRENT_CONFIGURATION->debug_options.abort_on_ice) \
            raise(SIGABRT); \
    exit(EXIT_FAILURE); \
}

LIBMCXX_EXTERN void debug_message(const char* message, const char* kind, const char* source_file, int line, const char* function_name, ...);

#define WARNING_MESSAGE(message, ...) \
{ \
    debug_message(message, "Warning: ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}

#define ASSERT_MESSAGE(cond, message, ...) \
{ if (!(cond)) \
    { \
        debug_message((message), "Assertion failed (" #cond ")"BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (CURRENT_CONFIGURATION->debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define ERROR_CONDITION(cond, message, ...) \
{ if ((cond)) \
    { \
        debug_message((message), "Error condition (" #cond ")"BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (CURRENT_CONFIGURATION->debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define DEBUG_MESSAGE(message, ...) \
{ \
    debug_message(message, "Debug : ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}


LIBMCXX_EXTERN const char* strappend(const char* orig, const char* appended);
LIBMCXX_EXTERN const char* strprepend(const char* orig, const char* prepended);



#define DEBUG_CODE() if (CURRENT_CONFIGURATION->debug_options.enable_debug_code)
#define NOT_DEBUG_CODE() if (!CURRENT_CONFIGURATION->debug_options.enable_debug_code)

#define IS_CXX_LANGUAGE (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_CXX)
#define IS_C_LANGUAGE (CURRENT_CONFIGURATION->source_language == SOURCE_LANGUAGE_C)

#define CXX_LANGUAGE() if (IS_CXX_LANGUAGE)
#define C_LANGUAGE() if (IS_C_LANGUAGE)

#define STATIC_ARRAY_LENGTH(_v) (sizeof(_v)/sizeof(_v[0]))

// Gives a unique name for the identifier
LIBMCXX_EXTERN const char* get_unique_name(void);

// States whether the string is blank
LIBMCXX_EXTERN char is_blank_string(const char* c);

// Special calloc that counts
LIBMCXX_EXTERN void *counted_calloc(size_t nmemb, size_t size, unsigned long long *counter);

// Separate values
LIBMCXX_EXTERN const char** blank_separate_values(const char* value, int *num_elems);
LIBMCXX_EXTERN const char** comma_separate_values(const char* value, int *num_elems);

// Convenience routines
LIBMCXX_EXTERN const char* give_dirname(const char* c);
LIBMCXX_EXTERN const char* give_basename(const char* c);

MCXX_END_DECLS

#endif // CXX_UTILS_H
