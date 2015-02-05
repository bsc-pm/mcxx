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



#ifndef CXX_OVERLOAD_DECLS_H
#define CXX_OVERLOAD_DECLS_H

#include "cxx-scope-decls.h"
#include "cxx-type-decls.h"

typedef
struct candidate_tag
{
    // Called function
    scope_entry_t* entry;

    // Number of types of arguments
    int num_args;
    type_t** args;

    // Link to next
    struct candidate_tag *next;
} candidate_t;

enum initialization_kind {
    IK_INVALID = 0,
    IK_COPY_INITIALIZATION   =   1 << 1,
    IK_DIRECT_INITIALIZATION =   1 << 2,
    // Modifiers for copy and direct initialization
    IK_BY_CONSTRUCTOR                   =   1 << 10,
    IK_BY_USER_DEFINED_CONVERSION       =   1 << 11,
    IK_BY_DIRECT_REFERENCE_BINDING      =   1 << 12,

    // Flag to avoid more than one user-defined conversion
    IK_NO_MORE_USER_DEFINED_CONVERSIONS      =   1 << 13,

    // These are mainly for debugging, although it is fine to use them
    IK_DIRECT_INITIALIZATION_BY_CONSTRUCTOR = IK_DIRECT_INITIALIZATION | IK_BY_CONSTRUCTOR,
    IK_DIRECT_INITIALIZATION_BY_USER_DEFINED_CONVERSION = IK_DIRECT_INITIALIZATION | IK_BY_USER_DEFINED_CONVERSION,
    IK_COPY_INITIALIZATION_BY_CONSTRUCTOR = IK_COPY_INITIALIZATION | IK_BY_CONSTRUCTOR,
    IK_COPY_INITIALIZATION_BY_USER_DEFINED_CONVERSION = IK_COPY_INITIALIZATION | IK_BY_USER_DEFINED_CONVERSION,
};

#endif // CXX_OVERLOAD_DECLS_H
