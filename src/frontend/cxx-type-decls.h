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




#ifndef CXX_TYPE_DECLS_H
#define CXX_TYPE_DECLS_H

#include "cxx-ast-fwd.h"
#include "cxx-scope-fwd.h"
#include "cxx-type-fwd.h"
#include "cxx-cexpr-fwd.h"

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-cexpr-decls.h"
#include "cxx-nodecl-decls.h"

MCXX_BEGIN_DECLS

typedef scope_entry_t* (*computed_function_type_t)(scope_entry_t* symbol, 
        type_t** argument_types, 
        nodecl_t *argument_expressions,
        int num_arguments,
        const_value_t** const_value);

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

typedef
enum ref_qualifier_tag
{
    REF_QUALIFIER_NONE = 0,
    // These are only meaningful in C++2011
    REF_QUALIFIER_LVALUE = 1, // &
    REF_QUALIFIER_RVALUE = 2  // &&
} ref_qualifier_t;

typedef
struct member_declaration_info_tag
{
    scope_entry_t* entry;
    const decl_context_t* decl_context;
    char is_definition:1;
} member_declaration_info_t;

#define SCI_LIST \
    SCI_CONVERSION_ID(SCI_INVALID) \
    /* Means that conv[x] does not apply any conversion */ \
    SCI_CONVERSION_ID(SCI_NO_CONVERSION) \
    /* Special tag in conv[0]. conv[1] and conv[2] are unused */ \
    SCI_CONVERSION_ID(SCI_IDENTITY) /* Special value for identities */ \
    SCI_CONVERSION_ID(SCI_DERIVED_TO_BASE) /* Not a real conversion, used only in overloads */ \
    /* One or more of these. Found in conv[0] */ \
    SCI_CONVERSION_ID(SCI_LVALUE_TO_RVALUE) \
    SCI_CONVERSION_ID(SCI_ARRAY_TO_POINTER)  \
    SCI_CONVERSION_ID(SCI_FUNCTION_TO_POINTER) \
    /* Zero or one of these. Found in conv[1] */ \
    SCI_CONVERSION_ID(SCI_INTEGRAL_PROMOTION) \
    SCI_CONVERSION_ID(SCI_VECTOR_INTEGRAL_PROMOTION) \
    SCI_CONVERSION_ID(SCI_FLOATING_PROMOTION) \
    SCI_CONVERSION_ID(SCI_INTEGRAL_CONVERSION) \
    SCI_CONVERSION_ID(SCI_FLOATING_CONVERSION) \
    SCI_CONVERSION_ID(SCI_FLOATING_INTEGRAL_CONVERSION) \
    SCI_CONVERSION_ID(SCI_INTEGRAL_FLOATING_CONVERSION) \
    /*     (C++ pointer conversions) */ \
    SCI_CONVERSION_ID(SCI_ZERO_TO_POINTER_CONVERSION) \
    SCI_CONVERSION_ID(SCI_ZERO_TO_NULLPTR) \
    SCI_CONVERSION_ID(SCI_NULLPTR_TO_POINTER_CONVERSION) \
    SCI_CONVERSION_ID(SCI_CLASS_POINTER_DERIVED_TO_BASE_CONVERSION) \
    SCI_CONVERSION_ID(SCI_POINTER_TO_MEMBER_BASE_TO_DERIVED_CONVERSION) \
    SCI_CONVERSION_ID(SCI_POINTER_TO_VOID_CONVERSION) \
    SCI_CONVERSION_ID(SCI_VOID_TO_POINTER_CONVERSION) \
    SCI_CONVERSION_ID(SCI_INTEGRAL_TO_POINTER_CONVERSION) \
    SCI_CONVERSION_ID(SCI_POINTER_TO_INTEGRAL_CONVERSION) \
    /*     (C++ end of pointer conversions) */ \
    SCI_CONVERSION_ID(SCI_POINTER_TO_POINTER_CONVERSION) \
    SCI_CONVERSION_ID(SCI_BOOLEAN_CONVERSION) \
    SCI_CONVERSION_ID(SCI_INTEGRAL_TO_COMPLEX_CONVERSION) \
    SCI_CONVERSION_ID(SCI_FLOAT_TO_COMPLEX_PROMOTION) \
    SCI_CONVERSION_ID(SCI_FLOAT_TO_COMPLEX_CONVERSION) \
    SCI_CONVERSION_ID(SCI_COMPLEX_FLOATING_INTEGRAL_CONVERSION) \
    SCI_CONVERSION_ID(SCI_COMPLEX_CONVERSION) \
    SCI_CONVERSION_ID(SCI_COMPLEX_PROMOTION) \
    SCI_CONVERSION_ID(SCI_COMPLEX_TO_INTEGRAL_CONVERSION) \
    SCI_CONVERSION_ID(SCI_COMPLEX_TO_FLOAT_CONVERSION) \
    SCI_CONVERSION_ID(SCI_SCALAR_TO_VECTOR_CONVERSION) \
    /* Zero or one of this. Found in conv[2] */ \
    SCI_CONVERSION_ID(SCI_QUALIFICATION_CONVERSION)

// Standard conversions info
typedef
enum standard_conversion_item_tag
{
#define SCI_CONVERSION_ID(X) X,
    SCI_LIST
#undef SCI_CONVERSION_ID
} standard_conversion_item_t;

// type tag
enum type_tag_t 
{
    TT_TYPENAME = 0, //typename
    TT_STRUCT, // struct
    TT_CLASS, // class
    TT_UNION, // union
    TT_ENUM,  // enum
    TT_INVALID 
};

// Standard conversion info
struct standard_conversion_tag
{
    struct type_tag* orig;
    struct type_tag* dest;
    standard_conversion_item_t conv[3];
};


LIBMCXX_EXTERN const standard_conversion_t no_scs_conversion;

typedef const char* (*print_type_callback_t)(type_t*, const decl_context_t*, void*);

MCXX_END_DECLS

#endif // CXX_TYPE_DECLS_H
