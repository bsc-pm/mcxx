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

#ifndef CXX_TYPEENVIRON_DECLS_H
#define CXX_TYPEENVIRON_DECLS_H

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "cxx-type-decls.h"

MCXX_BEGIN_DECLS

#include <stdlib.h>
typedef size_t _size_t;

/*
 * Typing environment
 */
struct type_environment_tag
{
    // This is a short name (without blanks) used to identify it in the config file
    const char* environ_id;
    // A more descriptive name
    const char* environ_name;

    // bool
    _size_t sizeof_bool;
    _size_t alignof_bool;

    // wchar_t
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

    // Let it empty if the environment does not support it
    // half (float)
    _size_t sizeof_half;
    _size_t alignof_half;
    
    // float
    _size_t sizeof_float;
    _size_t alignof_float;
    
    // double
    _size_t sizeof_double;
    _size_t alignof_double;

    // long double
    _size_t sizeof_long_double;
    _size_t alignof_long_double;

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

    // The exact 'char' type (depending on the environment it is 'signed' or
    // 'unsigned')
    type_t* (*char_type)(void);

    // Special type for GCC compatibility
    _size_t sizeof_builtin_va_list;
    _size_t alignof_builtin_va_list;
};

typedef struct type_environment_tag type_environment_t;

MCXX_END_DECLS

#endif // CXX_TYPEENVIRON_DECLS_H
