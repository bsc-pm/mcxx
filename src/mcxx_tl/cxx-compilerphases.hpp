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




#ifndef CXX_COMPILERPHASES
#define CXX_COMPILERPHASES

#include "cxx-driver-decls.h"
#include <stdio.h>

#ifdef __cplusplus
extern "C"
{
#endif

#ifdef _WIN32
   #ifdef LIBMCXXTL_DLL_EXPORT
       #define LIBMCXXTL_EXTERN extern __declspec(dllexport)
   #else
       #define LIBMCXXTL_EXTERN extern __declspec(dllimport)
   #endif
#else
   #define LIBMCXXTL_EXTERN extern
#endif

LIBMCXXTL_EXTERN void load_compiler_phases_cxx(compilation_configuration_t* config);
LIBMCXXTL_EXTERN void start_compiler_phase_pre_execution(compilation_configuration_t* config, translation_unit_t* translation_unit);
LIBMCXXTL_EXTERN void start_compiler_phase_execution(compilation_configuration_t* config, translation_unit_t* translation_unit);
LIBMCXXTL_EXTERN void phases_help(compilation_configuration_t* config);
LIBMCXXTL_EXTERN void unload_compiler_phases(void);

LIBMCXXTL_EXTERN void compiler_regular_phase_loader(compilation_configuration_t* config, const char* data);
LIBMCXXTL_EXTERN void compiler_special_phase_set_dto(compilation_configuration_t* config, const char* data);
LIBMCXXTL_EXTERN void compiler_special_phase_set_codegen(compilation_configuration_t* config, const char* data);

LIBMCXXTL_EXTERN void run_codegen_phase(FILE *out_file,
        translation_unit_t* translation_unit,
        const char* output_filename);

LIBMCXXTL_EXTERN void initialize_dto(translation_unit_t* translation_unit);

// This creates a dependence of fronted with mcxx_tl and tl, which is the worst thing it can happen
LIBMCXXTL_EXTERN const char* codegen_to_str(nodecl_t node, const decl_context_t* decl_context);

LIBMCXXTL_EXTERN void codegen_set_parameter(int n, void* data);


#ifdef __cplusplus
}
#endif

#endif // CXX_COMPILERPHASES
