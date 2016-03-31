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




#ifndef CXX_PROCESS_H
#define CXX_PROCESS_H

#include "cxx-driver-decls.h"
#include "cxx-macros.h"
#include <stdlib.h>
#include <signal.h>

#ifdef WIN32_BUILD
  #ifdef LIBMCXXPROCESS_DLL_EXPORT
     #define LIBMCXXPROCESS_EXTERN extern __declspec(dllexport)
  #else
     #define LIBMCXXPROCESS_EXTERN extern __declspec(dllimport)
  #endif
#else
  #define LIBMCXXPROCESS_EXTERN extern
#endif

MCXX_BEGIN_DECLS

LIBMCXXPROCESS_EXTERN compilation_process_t compilation_process;

LIBMCXXPROCESS_EXTERN translation_unit_t* add_new_file_to_compilation_process(
        compilation_file_process_t* current_file_process,
        const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration,
        int tag);

LIBMCXXPROCESS_EXTERN void fatal_error(const char* message, ...) NORETURN CHECK_PRINTF(1, 2);
LIBMCXXPROCESS_EXTERN void fatal_vprintf(const char* message, va_list ap) NORETURN;

#define BUG_URL "\nPlease report a bug at " \
                "pm-tools@bsc.es " \
                "with preprocessed source if possible\n"

#define internal_error(message, ...) \
{ \
    debug_message(message, "Internal compiler error." BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__ ); \
    if (debug_options.abort_on_ice) \
            raise(SIGABRT); \
    exit(EXIT_FAILURE); \
}

LIBMCXXPROCESS_EXTERN void debug_message(
        const char* message,
        const char* kind,
        const char* source_file, unsigned int line,
        const char* function_name, ...);

#define WARNING_MESSAGE(message, ...) \
{ \
    debug_message(message, "Warning: ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}

#define ASSERT_MESSAGE(cond, message, ...) \
{ if (__builtin_expect(!(cond), 0)) \
    { \
        debug_message((message), "Assertion failed (" #cond ")" BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define ERROR_CONDITION(cond, message, ...) \
{ if (__builtin_expect(!!(cond), 0)) \
    { \
        debug_message((message), "Error condition (" #cond ")" BUG_URL, __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
        if (debug_options.abort_on_ice) \
            raise(SIGABRT); \
        exit(EXIT_FAILURE); \
    } \
}

#define DEBUG_MESSAGE(message, ...) \
{ \
    debug_message(message, "Debug : ", __FILE__, __LINE__, __FUNCTION__, __VA_ARGS__); \
}

MCXX_END_DECLS

#endif // CXX_PROCESS_H
