/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#ifndef CXX_GCCBUILTINS_H
#define CXX_GCCBUILTINS_H

#include "libmcxx-common.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void gcc_sign_in_builtins(decl_context_t global_context);

LIBMCXX_EXTERN type_t* get_m128_struct_type(void);
LIBMCXX_EXTERN type_t* get_m128d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m128i_struct_type(void);

LIBMCXX_EXTERN type_t* get_m256_struct_type(void);
LIBMCXX_EXTERN type_t* get_m256d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m256i_struct_type(void);

LIBMCXX_EXTERN type_t* get_m512_struct_type(void);
LIBMCXX_EXTERN type_t* get_m512d_struct_type(void);
LIBMCXX_EXTERN type_t* get_m512i_struct_type(void);

LIBMCXX_EXTERN scope_entry_t* get_m128_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m128d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m128i_typedef(void);

LIBMCXX_EXTERN scope_entry_t* get_m256_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m256d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m256i_typedef(void);

LIBMCXX_EXTERN scope_entry_t* get_m512_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m512d_typedef(void);
LIBMCXX_EXTERN scope_entry_t* get_m512i_typedef(void);

LIBMCXX_EXTERN char is_intel_vector_struct_type(type_t* t, int *size);

LIBMCXX_EXTERN char vector_type_to_intel_vector_struct_type(type_t* orig, type_t* dest);
LIBMCXX_EXTERN char vector_type_to_intel_vector_struct_reinterpret_type(type_t* orig, type_t* dest);

LIBMCXX_EXTERN type_t* vector_type_get_intel_vector_struct_type(type_t* vector_type);
LIBMCXX_EXTERN type_t* intel_vector_struct_type_get_vector_type(type_t* vector_type);

LIBMCXX_EXTERN void prepend_intel_vector_typedefs(nodecl_t* nodecl_output);

MCXX_END_DECLS

#endif // CXX_GCCBUILTINS_H
