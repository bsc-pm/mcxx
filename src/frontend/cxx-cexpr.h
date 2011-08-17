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
  #include <quadmath.h>
#endif

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN const_value_t* const_value_get_integer(uint64_t value, int num_bytes, char sign);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_int(uint64_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_int(uint64_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_long_int(uint64_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_long_int(uint64_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_signed_long_long_int(uint64_t value);
LIBMCXX_EXTERN const_value_t* const_value_get_unsigned_long_long_int(uint64_t value);

LIBMCXX_EXTERN const_value_t* const_value_get_zero(int num_bytes, char sign);
LIBMCXX_EXTERN const_value_t* const_value_get_one(int num_bytes, char sign);
LIBMCXX_EXTERN char const_value_is_nonzero(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_zero(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_signed(const_value_t* val);

LIBMCXX_EXTERN const_value_t* const_value_get_float(float f);
LIBMCXX_EXTERN const_value_t* const_value_get_double(double d);
LIBMCXX_EXTERN const_value_t* const_value_get_long_double(long double ld);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN const_value_t* const_value_get_float128(__float128 ld);
#endif

LIBMCXX_EXTERN uint64_t const_value_cast_to_8(const_value_t* val);
LIBMCXX_EXTERN uint32_t const_value_cast_to_4(const_value_t* val);
LIBMCXX_EXTERN uint16_t const_value_cast_to_2(const_value_t* val);
LIBMCXX_EXTERN uint8_t const_value_cast_to_1(const_value_t* val);

LIBMCXX_EXTERN const_value_t* const_value_cast_to_bytes(const_value_t* val, int bytes, char sign);

LIBMCXX_EXTERN const_value_t* const_value_cast_to_signed_int_value(const_value_t* value);

LIBMCXX_EXTERN nodecl_t const_value_to_nodecl(const_value_t* v);

LIBMCXX_EXTERN type_t* const_value_get_minimal_integer_type(const_value_t* val);

LIBMCXX_EXTERN const_value_t* integer_type_get_maximum(type_t* t);
LIBMCXX_EXTERN const_value_t* integer_type_get_minimum(type_t* t);

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

LIBMCXX_EXTERN float const_value_cast_to_float(const_value_t* val);
LIBMCXX_EXTERN double const_value_cast_to_double(const_value_t* val);
LIBMCXX_EXTERN long double const_value_cast_to_long_double(const_value_t* val);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN __float128 const_value_cast_to_float128(const_value_t* val);
#endif

LIBMCXX_EXTERN const_value_t* const_value_cast_to_float_value(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_cast_to_double_value(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_cast_to_long_double_value(const_value_t* val);
#ifdef HAVE_QUADMATH_H
LIBMCXX_EXTERN const_value_t* const_value_cast_to_float128_value(const_value_t* val);
#endif

LIBMCXX_EXTERN const_value_t* const_value_make_array(int num_elements, const_value_t **elements);
LIBMCXX_EXTERN const_value_t* const_value_make_vector(int num_elements, const_value_t **elements);
LIBMCXX_EXTERN const_value_t* const_value_make_struct(int num_elements, const_value_t **elements);
LIBMCXX_EXTERN const_value_t* const_value_make_complex(const_value_t* real_part, const_value_t* imag_part);

LIBMCXX_EXTERN const_value_t* const_value_make_string(const char* literal, int num_elems);
LIBMCXX_EXTERN const_value_t* const_value_make_wstring(int * literal, int num_elems);
LIBMCXX_EXTERN const_value_t* const_value_make_string_from_values(int num_elements, const_value_t **elements);

LIBMCXX_EXTERN void const_value_string_unpack(const_value_t* v, int**, int*);

LIBMCXX_EXTERN const_value_t* const_value_string_concat(const_value_t* v1, const_value_t* v2);

LIBMCXX_EXTERN const_value_t* const_value_complex_get_real_part(const_value_t* value);
LIBMCXX_EXTERN const_value_t* const_value_complex_get_imag_part(const_value_t* value);

LIBMCXX_EXTERN int const_value_get_num_elements(const_value_t* value);
LIBMCXX_EXTERN const_value_t* const_value_get_element_num(const_value_t* value, int num);

LIBMCXX_EXTERN const_value_t* const_value_convert_to_vector(const_value_t* value, int num_elements);
LIBMCXX_EXTERN const_value_t* const_value_convert_to_array(const_value_t* value, int num_elements);

LIBMCXX_EXTERN const_value_t* const_value_real_to_complex(const_value_t* value);

LIBMCXX_EXTERN const_value_t* const_value_round_to_zero(const_value_t* val);
LIBMCXX_EXTERN const_value_t* const_value_round_to_zero_bytes(const_value_t* val, int num_bytes);

#define BINOP_DECL(_opname, _binop) \
LIBMCXX_EXTERN const_value_t* const_value_##_opname(const_value_t* v1, const_value_t* v2); \

BINOP_DECL(add, +)
BINOP_DECL(sub, -)
BINOP_DECL(mul, *)
BINOP_DECL(div, /)
BINOP_DECL(mod, %)
BINOP_DECL(shr, >>)
BINOP_DECL(shl, <<)
BINOP_DECL(bitand, &)
BINOP_DECL(bitor, |)
BINOP_DECL(bitxor, ^)
BINOP_DECL(and, &&)
BINOP_DECL(or, ||)
BINOP_DECL(lt, <)
BINOP_DECL(lte, <=)
BINOP_DECL(gt, >)
BINOP_DECL(gte, >)
BINOP_DECL(eq, ==)
BINOP_DECL(neq, !=)
BINOP_DECL(pow, **)

#define UNOP_DECL(_opname, _unop) \
LIBMCXX_EXTERN const_value_t* const_value_##_opname(const_value_t* v1);

UNOP_DECL(plus, +)
UNOP_DECL(neg, -)
UNOP_DECL(bitnot, ~)
UNOP_DECL(not, !)

MCXX_END_DECLS

#endif // CXX_CEXPR_H
