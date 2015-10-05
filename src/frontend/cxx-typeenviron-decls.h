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




#ifndef CXX_TYPEENVIRON_DECLS_H
#define CXX_TYPEENVIRON_DECLS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

#include <stdlib.h>
typedef size_t _size_t;

#include "cxx-type-fwd.h"
#include "cxx-scope-fwd.h"

/*
 * Typing environment
 */
// We assume IEEE 754 binary floats
struct floating_type_info_tag
{
    // What sizeof will return for these variables
    _size_t size_of;
    // What alignof will return for these variables
    _size_t align_of;

    // Bits in memory
    _size_t bits;

    // Real model
    _size_t base;
    _size_t p;
    _size_t emin;
    _size_t emax;
};

typedef struct floating_type_info_tag floating_type_info_t;

#define MAX_DIFFERENT_FLOATS 8

typedef
enum endianness_tag
{
    ENV_BIG_ENDIAN,
    ENV_LITTLE_ENDIAN,
} endianness_t;

struct type_environment_tag
{
    // This is a short name (without blanks) used to identify it in the config file
    const char* environ_id;
    // A more descriptive name
    const char* environ_name;

    // Endianness
    endianness_t endianness;

    // bool
    _size_t sizeof_bool;
    _size_t alignof_bool;
    
    // wchar_t
    type_t* (*int_type_of_wchar_t)(void); // This is only for C99
    _size_t sizeof_wchar_t;
    _size_t alignof_wchar_t;

    // short
    _size_t sizeof_unsigned_short;
    _size_t alignof_unsigned_short;

    _size_t sizeof_signed_short;
    _size_t alignof_signed_short;

    // int
    _size_t sizeof_signed_int;
    _size_t alignof_signed_int;

    _size_t sizeof_unsigned_int;
    _size_t alignof_unsigned_int;
    
    // long
    _size_t sizeof_signed_long;
    _size_t alignof_signed_long;

    _size_t sizeof_unsigned_long;
    _size_t alignof_unsigned_long;
    
    // long long
    _size_t sizeof_signed_long_long;
    _size_t alignof_signed_long_long;

    _size_t sizeof_unsigned_long_long;
    _size_t alignof_unsigned_long_long;

    int num_float_types;
    floating_type_info_t* all_floats[MAX_DIFFERENT_FLOATS];
    
    // float
    floating_type_info_t* float_info;
    
    // double
    floating_type_info_t* double_info;

    // long double
    floating_type_info_t* long_double_info;

    // half float
    floating_type_info_t* float16_info;

    // float128
    floating_type_info_t* float128_info;

    // pointer (to data)
    _size_t sizeof_pointer;
    _size_t alignof_pointer;

    // pointer (to function)
    // this one exists because a pointer to function
    // does not have to be compatible with a regular
    // pointer to data
    _size_t sizeof_function_pointer;
    _size_t alignof_function_pointer;

    // pointer to data member
    _size_t sizeof_pointer_to_data_member;
    _size_t alignof_pointer_to_data_member;

    // pointer to member function
    _size_t sizeof_pointer_to_member_function;
    _size_t alignof_pointer_to_member_function;

    // function that computes the size of a class type
    // this typically will follow some underlying ABI
    void (*compute_sizeof)(type_t*);

    // The type that matches the one of sizeof
    type_t* (*type_of_sizeof)(void);
    type_t* (*type_of_ptrdiff_t)(void);

    // The exact 'char' type (depending on the environment it is 'signed' or
    // 'unsigned')
    type_t* (*char_type)(void);

    // Special type for GCC compatibility
    _size_t sizeof_builtin_va_list;
    _size_t alignof_builtin_va_list;
    type_t* (*builtin_va_list_type)(void); // overrides __builtin_va_list if not NULL

    // Target-specific GCC builtins
    void (*gcc_target_specific_builtins)(const decl_context_t* global_context);
};

typedef struct type_environment_tag type_environment_t;

MCXX_END_DECLS

#endif // CXX_TYPEENVIRON_DECLS_H
