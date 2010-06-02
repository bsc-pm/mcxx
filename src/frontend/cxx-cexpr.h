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

#ifndef CXX_CEXPR_H
#define CXX_CEXPR_H

#include "libmcxx-common.h"
#include "cxx-ast-decls.h"
#include "cxx-cexpr-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN const_value_t* const_value_get(uint64_t value, int num_bytes, char sign);
LIBMCXX_EXTERN const_value_t* const_value_get_zero(int num_bytes, char sign);
LIBMCXX_EXTERN const_value_t* const_value_get_one(int num_bytes, char sign);
LIBMCXX_EXTERN char const_value_is_nonzero(const_value_t* v);
LIBMCXX_EXTERN char const_value_is_zero(const_value_t* v);

LIBMCXX_EXTERN uint64_t const_value_cast_to_8(const_value_t* val);
LIBMCXX_EXTERN uint32_t const_value_cast_to_4(const_value_t* val);
LIBMCXX_EXTERN uint16_t const_value_cast_to_2(const_value_t* val);
LIBMCXX_EXTERN uint8_t const_value_cast_to_1(const_value_t* val);

LIBMCXX_EXTERN AST const_value_to_tree(const_value_t* v);

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

#define UNOP_DECL(_opname, _unop) \
LIBMCXX_EXTERN const_value_t* const_value_##_opname(const_value_t* v1);

UNOP_DECL(plus, +)
UNOP_DECL(neg, -)
UNOP_DECL(bitnot, ~)
UNOP_DECL(not, !)

MCXX_END_DECLS

#endif // CXX_CEXPR_H
