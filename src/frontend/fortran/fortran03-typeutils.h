/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef FORTRAN03_TYPEUTILS_H
#define FORTRAN03_TYPEUTILS_H

#include "libmf03-common.h"
#include "cxx-typeutils.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN const char* fortran_print_type_str(type_t*);

LIBMF03_EXTERN char is_fortran_intrinsic_type(type_t* t);

LIBMF03_EXTERN char is_pointer_to_array_type(type_t*);
LIBMF03_EXTERN int get_rank_of_type(type_t* t);
LIBMF03_EXTERN type_t* get_rank0_type(type_t* t);
LIBMF03_EXTERN type_t* get_n_ranked_type(type_t* scalar_type, int rank, decl_context_t decl_context);

LIBMF03_EXTERN char equivalent_tk_types(type_t* t1, type_t* t2);
LIBMF03_EXTERN char equivalent_tkr_types(type_t* t1, type_t* t2);

// States that this is an array of character type
LIBMF03_EXTERN char is_fortran_character_type(type_t*);
// A pointer to is_fortran_character_type
LIBMF03_EXTERN char is_pointer_to_fortran_character_type(type_t* t);
LIBMF03_EXTERN char is_fortran_character_type_or_pointer_to(type_t* t);

// States that this is an array of non-character type
LIBMF03_EXTERN char is_fortran_array_type(type_t* t);

// States that this is an scalar type
LIBMF03_EXTERN char fortran_is_scalar_type(type_t* t);

// A pointer to is_fortran_array_type
LIBMF03_EXTERN char is_pointer_to_fortran_array_type(type_t* t);
LIBMF03_EXTERN char is_fortran_array_type_or_pointer_to(type_t* t);



LIBMF03_EXTERN type_t* replace_return_type_of_function_type(type_t* function_type, 
        type_t* new_return_type);

LIBMF03_EXTERN type_t* update_basic_type_with_type(type_t* type_info, type_t* basic_type);

LIBMF03_EXTERN char basic_type_is_implicit_none(type_t* t);

LIBMF03_EXTERN type_t* rebuild_array_type(type_t* rank0_type, type_t* array_type);

LIBMF03_EXTERN char are_conformable_types(type_t* t1, type_t* t2);

type_t* fortran_get_default_integer_type(void);
type_t* fortran_get_default_real_type(void);
type_t* fortran_get_default_logical_type(void);
type_t* fortran_get_default_character_type(void);

int fortran_get_default_integer_type_kind(void);
int fortran_get_default_real_type_kind(void);
int fortran_get_default_logical_type_kind(void);
int fortran_get_default_character_type_kind(void);

MCXX_END_DECLS

#endif // FORTRAN03_TYPEUTILS_H
