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
#ifndef CXX_COMPILERPHASES
#define CXX_COMPILERPHASES

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

#ifdef __cplusplus
}
#endif

#endif // CXX_COMPILERPHASES
