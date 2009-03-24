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
#ifndef TL_COMMON_HPP
#define TL_COMMON_HPP

#ifdef _WIN32
  #ifdef LIBTL_DLL_EXPORT
    #define LIBTL_EXTERN extern __declspec(dllexport)
    #define LIBTL_CLASS __declspec(dllexport)
  #else
    #define LIBTL_EXTERN extern __declspec(dllimport)
    #define LIBTL_CLASS __declspec(dllimport)
  #endif
  #define LIBTL_ALWAYS_EXPORT __declspec(dllexport)
#else
  #define LIBTL_EXTERN extern
  #define LIBTL_CLASS
  #define LIBTL_ALWAYS_EXPORT 
#endif

// In C++0x maybe this will make some sense
#ifndef FINAL
  #define FINAL
#endif

#endif // TL_COMMON_HPP
