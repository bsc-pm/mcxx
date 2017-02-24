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




#ifndef CXX_CEXPR_H
#define CXX_CEXPR_H

#include <stdint.h>

#include "libmcxx-common.h"
#include "cxx-ast-decls.h"
#include "cxx-cexpr-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-macros.h"
#include "cxx-nodecl-decls.h"
#include "cxx-typeutils.h"

#include <stdint.h>
#ifdef HAVE_QUADMATH_H
// Somebody forgot to protect this
MCXX_BEGIN_DECLS
#include <quadmath.h>
MCXX_END_DECLS
#endif

MCXX_BEGIN_DECLS

#ifdef HAVE_INT128
typedef unsigned __int128 cvalue_uint_t;
typedef signed __int128   cvalue_int_t;
#else
typedef uint64_t cvalue_uint_t;
typedef int64_t  cvalue_int_t;
#endif

LIBMCXX_EXTERN const_value_t* const_value_get_integer(cvalue_uint_t value, int num_bytes, char sign);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_int(cvalue_uint_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_int(cvalue_uint_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_long_int(cvalue_uint_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_long_int(cvalue_uint_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_long_long_int(cvalue_uint_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_long_long_int(cvalue_uint_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_short_int(cvalue_uint_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_short_int(cvalue_uint_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_zero(int num_bytes, char sign);
LIBMCXX_EXTERN const_value_t* const_value_get_one(int num_bytes, char sign);
LIBMCXX_EXTERN const_value_t* const_value_get_minus_one(int num_bytes, char sign);
LIBMCXX_EXTERN char const_value_is_nonzero(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_zero(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_one(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_minus_one(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_signed(const_value_t* val);
LIBMCXX_EXTERN char const_value_is_positive(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_negative(const_value_t* v);

LIBMCXX_EXTERN const_value_t* const_value_get_float(float f);
LIBMCXX_EXTERN const_value_t* const_value_get_double(double d);
LIBMCXX_EXTERN const_value_t* const_value_get_long_double(long double ld);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN const_value_t* const_value_get_float128(__float128 ld);
#endif

#ifdef HAVE_INT128
LIBMCXX_EXTERN unsigned __int128 const_value_cast_to_16(const_value_t* val);
#endif
LIBMCXX_EXTERN uint64_t const_value_cast_to_8(const_value_t* val);
LIBMCXX_EXTERN uint32_t const_value_cast_to_4(const_value_t* val);
LIBMCXX_EXTERN uint16_t const_value_cast_to_2(const_value_t* val);
LIBMCXX_EXTERN uint8_t const_value_cast_to_1(const_value_t* val);

LIBMCXX_EXTERN const_value_t* const_value_cast_as_another(const_value_t* val, const_value_t* mold);

LIBMCXX_EXTERN int const_value_cast_to_signed_int(const_value_t* val);
LIBMCXX_EXTERN unsigned int const_value_cast_to_unsigned_int(const_value_t* val);

LIBMCXX_EXTERN long int const_value_cast_to_signed_long_int(const_value_t* val);
LIBMCXX_EXTERN unsigned long int const_value_cast_to_unsigned_long_int(const_value_t* val);

LIBMCXX_EXTERN long long int const_value_cast_to_signed_long_long_int(const_value_t* val);
LIBMCXX_EXTERN unsigned long long int const_value_cast_to_unsigned_long_long_int(const_value_t* val);

LIBMCXX_EXTERN const_value_t* const_value_cast_to_bytes(const_value_t* val, int bytes, char sign);

LIBMCXX_EXTERN const_value_t* const_value_cast_to_signed_int_value(const_value_t* value);

LIBMCXX_EXTERN cvalue_int_t const_value_cast_to_cvalue_int(const_value_t* value);
LIBMCXX_EXTERN cvalue_uint_t const_value_cast_to_cvalue_uint(const_value_t* value);

LIBMCXX_EXTERN nodecl_t const_value_to_nodecl(const_value_t* v);
LIBMCXX_EXTERN nodecl_t const_value_to_nodecl_cached(const_value_t* v);

// This function uses the basic type for elemental types (both for integer or floating)
LIBMCXX_EXTERN nodecl_t const_value_to_nodecl_with_basic_type(const_value_t* v, 
        type_t* basic_type);
LIBMCXX_EXTERN nodecl_t const_value_to_nodecl_with_basic_type_cached(const_value_t* v, 
        type_t* basic_type);

LIBMCXX_EXTERN type_t* const_value_get_minimal_integer_type(const_value_t* val);
LIBMCXX_EXTERN type_t* const_value_get_minimal_integer_for_value_at_least_signed_int(const_value_t* val);
LIBMCXX_EXTERN type_t* const_value_get_minimal_integer_type_from_list_of_types(
        cvalue_uint_t value,
        int num_types,
        type_t** types);

LIBMCXX_EXTERN const_value_t* integer_type_get_maximum(type_t* t);
LIBMCXX_EXTERN const_value_t* integer_type_get_minimum(type_t* t);
LIBMCXX_EXTERN const_value_t* floating_type_get_maximum(type_t* t);
LIBMCXX_EXTERN const_value_t* floating_type_get_minimum(type_t* t);

LIBMCXX_EXTERN int const_value_get_bytes(const_value_t* val);

LIBMCXX_EXTERN char const_value_is_integer(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_floating(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_float(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_double(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_long_double(const_value_t* v);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN char const_value_is_float128(const_value_t* v);
#endif
LIBMCXX_EXTERN char const_value_is_complex(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_structured(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_array(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_vector(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_string(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_range(const_value_t* v);

LIBMCXX_EXTERN float const_value_cast_to_float(const_value_t* val);
LIBMCXX_EXTERN double const_value_cast_to_double(const_value_t* val);
LIBMCXX_EXTERN long double const_value_cast_to_long_double(const_value_t* val);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN __float128 const_value_cast_to_float128(const_value_t* val);
#endif

LIBMCXX_EXTERN _Complex float const_value_cast_to_complex_float(const_value_t* val);
LIBMCXX_EXTERN _Complex double const_value_cast_to_complex_double(const_value_t* val);
LIBMCXX_EXTERN _Complex long double const_value_cast_to_complex_long_double(const_value_t* val);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN __complex128 const_value_cast_to_complex_float128(const_value_t* val);
#endif

LIBMCXX_EXTERN const_value_t* const_value_cast_to_float_value(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_cast_to_double_value(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_cast_to_long_double_value(const_value_t* val);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN const_value_t* const_value_cast_to_float128_value(const_value_t* val);
#endif

LIBMCXX_EXTERN const_value_t* const_value_cast_to_floating_type_value(const_value_t* val, type_t* floating_type);

LIBMCXX_EXTERN const_value_t* const_value_get_complex_float(_Complex float f);
LIBMCXX_EXTERN const_value_t* const_value_get_complex_double(_Complex double d);
LIBMCXX_EXTERN const_value_t* const_value_get_complex_long_double(_Complex long double ld);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN const_value_t* const_value_get_complex_float128(__complex128 ld);
#endif

LIBMCXX_EXTERN const_value_t* const_value_make_array(int num_elements, const_value_t **elements);
LIBMCXX_EXTERN const_value_t* const_value_make_vector(int num_elements, const_value_t **elements);
LIBMCXX_EXTERN const_value_t* const_value_make_struct(int num_elements, const_value_t **elements, type_t* struct_type);
LIBMCXX_EXTERN const_value_t* const_value_make_complex(const_value_t* real_part, const_value_t* imag_part);
LIBMCXX_EXTERN const_value_t* const_value_make_range(const_value_t* lower, const_value_t* upper, const_value_t* stride);

LIBMCXX_EXTERN const_value_t* const_value_make_vector_from_scalar(int num_elements, const_value_t* value);
LIBMCXX_EXTERN const_value_t* const_value_make_array_from_scalar(int num_elements, const_value_t* value);

// If you want to create a null ended string with this one you will have to
// explicitly pass the null value as the last element (i.e. num_elements > 1)
LIBMCXX_EXTERN const_value_t* const_value_make_string_from_values(int num_elements, const_value_t **elements);

// This constants are not null-ended (these are for Fortran mainly)
LIBMCXX_EXTERN const_value_t* const_value_make_string(const char* literal, int num_elems);
LIBMCXX_EXTERN const_value_t* const_value_make_wstring(int * literal, int num_elems);

// This constants are null-ended (these are for C/C++)
// Do not count the null in num_elems (i.e. an empty string literal will have num_elems == 0)
LIBMCXX_EXTERN const_value_t* const_value_make_string_null_ended(const char* literal, int num_elems);
LIBMCXX_EXTERN const_value_t* const_value_make_wstring_null_ended(int * literal, int num_elems);

// These functions never return the trailing null, instead they set
// is_null_ended if the last value is a zero.
LIBMCXX_EXTERN void const_value_string_unpack_to_int(const_value_t* v, int**, int*, char* is_null_ended);
LIBMCXX_EXTERN const char *const_value_string_unpack_to_string(const_value_t* v, char* is_null_ended);

// Note that this function will NOT ignore any null at the end of v1 and v2
LIBMCXX_EXTERN const_value_t* const_value_string_concat(const_value_t* v1, const_value_t* v2);

LIBMCXX_EXTERN const_value_t* const_value_complex_get_real_part(const_value_t* value);
LIBMCXX_EXTERN const_value_t* const_value_complex_get_imag_part(const_value_t* value);

LIBMCXX_EXTERN int const_value_get_num_elements(const_value_t* value);
LIBMCXX_EXTERN const_value_t* const_value_get_element_num(const_value_t* value, int num);

LIBMCXX_EXTERN const_value_t* const_value_convert_to_type(const_value_t* const_value, type_t* dst_type);
LIBMCXX_EXTERN const_value_t* const_value_convert_to_vector(const_value_t* value, int num_elements);
LIBMCXX_EXTERN const_value_t* const_value_convert_to_array(const_value_t* value, int num_elements);

LIBMCXX_EXTERN const_value_t* const_value_real_to_complex(const_value_t* value);

LIBMCXX_EXTERN const_value_t* const_value_round_to_zero(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_round_to_zero_bytes(const_value_t* val, int num_bytes);

LIBMCXX_EXTERN const_value_t* const_value_round_to_nearest(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_round_to_nearest_bytes(const_value_t* val, int num_bytes);

LIBMCXX_EXTERN const_value_t* const_value_round(const_value_t* val, int num_bytes, int rounding_mode);

LIBMCXX_EXTERN type_t* const_value_get_struct_type(const_value_t* v);

LIBMCXX_EXTERN const_value_t* const_value_get_unknown(void);
LIBMCXX_EXTERN char const_value_is_unknown(const_value_t* v);

#define BINOP_DECL(_opname, _binop) \
LIBMCXX_EXTERN const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2); \

BINOP_DECL(add, +)
BINOP_DECL(sub, -)
BINOP_DECL(mul, *)
BINOP_DECL(div, /)
BINOP_DECL(mod, %)
BINOP_DECL(shr, >>)
BINOP_DECL(bitshl, <<)
BINOP_DECL(bitand, &)
BINOP_DECL(bitor, |)
BINOP_DECL(bitxor, ^)
BINOP_DECL(and, &&)
BINOP_DECL(or, ||)
BINOP_DECL(lt, <)
BINOP_DECL(lte, <=)
BINOP_DECL(gt, >)
BINOP_DECL(gte, >=)
BINOP_DECL(eq, ==)
BINOP_DECL(neq, !=)
BINOP_DECL(pow, **)

#define UNOP_DECL(_opname, _unop) \
LIBMCXX_EXTERN const_value_t* const_value_##_opname(const_value_t* v1);

UNOP_DECL(plus, +)
UNOP_DECL(neg, -)
UNOP_DECL(bitnot, ~)
UNOP_DECL(not, !)

LIBMCXX_EXTERN const_value_t* const_value_square(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_sqrt(const_value_t* val);

// Fortran module support
LIBMCXX_EXTERN size_t const_value_get_raw_data_size(void);
LIBMCXX_EXTERN const_value_t* const_value_build_from_raw_data(const char*);

// Debugging
LIBMCXX_EXTERN const char* const_value_to_str(const_value_t*);

// EXPERIMENTAL
LIBMCXX_EXTERN const_value_t* const_value_make_address(const_value_t* val);
LIBMCXX_EXTERN char const_value_is_address(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_address_dereference(const_value_t* val);

typedef
enum subobject_accessor_kind {
    SUBOBJ_INVALID = 0,
    SUBOBJ_ELEMENT,
    SUBOBJ_MEMBER,
} subobject_accessor_kind_t;

typedef
struct subobject_accessors_tag
{
    subobject_accessor_kind_t kind;
    const_value_t* index;
} subobject_accessor_t;

LIBMCXX_EXTERN const_value_t* const_value_make_object(scope_entry_t* object,
        int num_subobject_accesors,
        subobject_accessor_t* accessors);
LIBMCXX_EXTERN char const_value_is_object(const_value_t*);
LIBMCXX_EXTERN scope_entry_t* const_value_object_get_base(const_value_t*);
LIBMCXX_EXTERN int const_value_object_get_num_accessors(const_value_t*);
LIBMCXX_EXTERN subobject_accessor_t const_value_object_get_accessor_num(const_value_t*, int i);
LIBMCXX_EXTERN void const_value_object_get_all_accessors(const_value_t*, subobject_accessor_t* out);

MCXX_END_DECLS

#endif // CXX_CEXPR_H
