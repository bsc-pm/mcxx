/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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

LIBPROCESS_EXTERN void add_new_file_to_compilation_process(
        compilation_file_process_t* current_file_process,
        const char* file_path, const char* output_file, 
        compilation_configuration_t* configuration);

LIBMCXX_EXTERN void running_error(const char* message, ...) NORETURN;

#define BUG_URL "\nPlease report a bug at " \
                "http://nanos.ac.upc.edu/projects/mcxx/newticket " \
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

MCXX_END_DECLS

#endif // CXX_PROCESS_H
