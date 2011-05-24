/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef CXX_TYPE_DECLS_H
#define CXX_TYPE_DECLS_H

#include "cxx-type-fwd.h"

#include "libmcxx-common.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

// Information of a parameter feeded to get_new_function_type
struct parameter_info_tag
{
    // This parameter is '...'
    char is_ellipsis;
    // Otherwise it has the type here
    struct type_tag* type_info;
    // Nonadjusted type
    struct type_tag* nonadjusted_type_info;
};

// Standard conversions info
typedef
enum standard_conversion_item_tag
{
    SCI_INVALID = 0,
    SCI_NO_CONVERSION = 1,
    // Special value for identities
    SCI_IDENTITY,
    // Zero or one of these
    SCI_LVALUE_TO_RVALUE,
    SCI_ARRAY_TO_POINTER,
    SCI_FUNCTION_TO_POINTER,
    // Zero or one of these
    SCI_INTEGRAL_PROMOTION,
    SCI_VECTOR_INTEGRAL_PROMOTION,
    SCI_FLOATING_PROMOTION,
    SCI_INTEGRAL_CONVERSION,
    SCI_FLOATING_CONVERSION,
    SCI_FLOATING_INTEGRAL_CONVERSION,
    SCI_POINTER_CONVERSION,
    SCI_POINTER_TO_MEMBER_CONVERSION,
    SCI_BOOLEAN_CONVERSION,
    // Zero or one of this
    SCI_QUALIFICATION_CONVERSION,
} standard_conversion_item_t;

// Class kind
enum class_kind_t {
    CK_INVALID = 0,
    CK_STRUCT, // struct
    CK_CLASS, // class
    CK_UNION // union 
};

// Standard conversion info
struct standard_conversion_tag
{
    struct type_tag* orig;
    struct type_tag* dest;
    standard_conversion_item_t conv[3];
};

// Dependent typenames
struct dependent_name_part_tag
{
    const char* name;
    struct template_argument_list_tag* template_arguments;
    struct type_tag* related_type;
    struct dependent_name_part_tag* next;
};

LIBMCXX_EXTERN const standard_conversion_t no_scs_conversion;

MCXX_END_DECLS

#endif // CXX_TYPE_DECLS_H
