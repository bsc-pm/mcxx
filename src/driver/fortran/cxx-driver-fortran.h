/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#ifndef CXX_DRIVER_FORTRAN_H
#define CXX_DRIVER_FORTRAN_H


// This function states to the driver that we are going to use the module 'module_name'
// it returns the path of the mf03 specific module. If such module was wrapped, it returns
// too the wrap file itself
void driver_fortran_retrieve_module(const char* module_name, const char **mf03_filename, const char **wrap_filename);

// This function states to the driver that we are going to create the module 'module_name'
// it returns the path where the mf03 specific module will be created
// The driver will ensure that the native module is properly wrapped along with mf03_filename
void driver_fortran_register_module(const char* module_name, 
        const char **mf03_filename,
        char is_intrinsic);

// This function is called by the driver after the native compilation of a Fortran file
void driver_fortran_wrap_all_modules(void);

// This function is called by the driver if native compilation is not actually performed
void driver_fortran_discard_all_modules(void);

// This function hides all wrap modules, lest they were found by the native compiler
void driver_fortran_hide_mercurium_modules(void);

// This function restores all wrap modules, for subsequent uses
void driver_fortran_restore_mercurium_modules(void);

#endif // CXX_DRIVER_FORTRAN_H
