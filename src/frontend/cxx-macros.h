/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_MACROS_H
#define CXX_MACROS_H

// Some useful macros
#ifndef __GNUC__
    #define __attribute__(x)
#endif

// Some gcc-isms
#ifdef __GNUC__
  #if __GNUC__ == 3 
     #define NORETURN __attribute__((noreturn))

     #if __GNUC_MINOR__ >= 1
        #define DEPRECATED __attribute__((deprecated))
        #define UNUSED_PARAMETER __attribute__((unused))
     #else
        #define DEPRECATED
        #define UNUSED_PARAMETER
     #endif
     #if __GNUC_MINOR__ >= 4
         #define WARN_UNUSED __attribute__((warn_unused_result))
     #else
         #define WARN_UNUSED
     #endif
  #elif __GNUC__ == 4
     #define NORETURN __attribute__((noreturn))
     #define WARN_UNUSED __attribute__((warn_unused_result))
     #define DEPRECATED __attribute__((deprecated))
     #define UNUSED_PARAMETER __attribute__((unused))
  #elif __GNUC__ == 2
     #error "This code will not compile with GCC 2"
  #else
     #error "Unsupported version of GCC"
  #endif
#else
  #define NORETURN
  #define WARN_UNUSED
  #define UNUSED_PARAMETER
  #define DEPRECATED
#endif

#ifdef __cplusplus
  #define MCXX_BEGIN_DECLS extern "C" { 
  #define MCXX_END_DECLS }
#else
  #define MCXX_BEGIN_DECLS
  #define MCXX_END_DECLS
#endif

#define BITMAP_TEST(x, b) ((((int)(x)) & ((int)(b))) == (b))

#endif // CXX_MACROS_H
