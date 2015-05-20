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




#ifndef CXX_MACROS_H
#define CXX_MACROS_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// Some useful macros
#ifndef __GNUC__
    #define __attribute__(x)
#endif

// Some gcc-isms
#if defined(__GNUC__) && !defined(__clang__)
  #if __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
     // Supported in >=4.3
     #define NORETURN __attribute__((noreturn))
     #define WARN_UNUSED __attribute__((warn_unused_result))
     #define DEPRECATED __attribute__((deprecated))
     #define UNUSED_PARAMETER __attribute__((unused))
     #define WARN_FUNCTION(x) __attribute__((warning(x)))
     #define MALLOC_RETURN __attribute__((malloc))
     #define ALWAYS_INLINE __attribute__((always_inline))
     // Supported in >=4.4
     #if (__GNUC__ > 4 || __GNUC_MINOR__ >= 4)
        #define CHECK_PRINTF(x,y) __attribute__ ((format (gnu_printf, x, y)))
     #else
        #define CHECK_PRINTF(x,y) __attribute__ ((format (printf, x, y)))
     #endif
     // Supported in >=4.5
     #if (__GNUC__ > 4 || __GNUC_MINOR__ >= 5)
        #define DEPRECATED_REASON(r) __attribute__((deprecated(r)))
     #else
        #define DEPRECATED_REASON(r) __attribute__((deprecated))
     #endif
  #else
     #error "Unsupported version of GCC. It must be 4.3 or better"
  #endif
#elif defined(__clang__)
  #define NORETURN __attribute__((noreturn))
  #define WARN_UNUSED __attribute__((warn_unused_result))
  #define DEPRECATED __attribute__((deprecated))
  #define UNUSED_PARAMETER __attribute__((unused))
  #define WARN_FUNCTION(x)
  #define MALLOC_RETURN __attribute__((malloc))
  #define ALWAYS_INLINE __attribute__((always_inline))
  #define CHECK_PRINTF(x,y) __attribute__ ((format (printf, x, y)))
  #define DEPRECATED_REASON(r) __attribute__((deprecated))
#else
  #define NORETURN
  #define WARN_UNUSED
  #define UNUSED_PARAMETER
  #define DEPRECATED
  #define DEPRECATED_REASON(r)
  #define WARN_FUNCTION(x)
  #define ALWAYS_INLINE
#endif

#if defined(__cplusplus) && defined(HAVE_CXX11)
  #define OVERRIDE override
  #define FINAL final
#else
  #define OVERRIDE
  #define FINAL
#endif

#define UNUSED_FUNCTION UNUSED_PARAMETER

#ifdef __cplusplus
  #define MCXX_BEGIN_DECLS extern "C" { 
  #define MCXX_END_DECLS }
#else
  #define MCXX_BEGIN_DECLS
  #define MCXX_END_DECLS
#endif

#define BITMAP_TEST(x, b) ((((int)(x)) & ((int)(b))) == (b))

#endif // CXX_MACROS_H
