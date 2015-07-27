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

#ifndef FORTRAN03_TYPEUTILS_H
#define FORTRAN03_TYPEUTILS_H

#include "libmf03-common.h"
#include "cxx-typeutils.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN const char* fortran_print_type_str(type_t*);

LIBMF03_EXTERN char fortran_is_intrinsic_type(type_t* t);

LIBMF03_EXTERN int fortran_get_rank_of_type(type_t* t);
LIBMF03_EXTERN type_t* fortran_get_rank0_type(type_t* t);
LIBMF03_EXTERN type_t* fortran_get_n_ranked_type(type_t* scalar_type, int rank, const decl_context_t* decl_context);
LIBMF03_EXTERN type_t* fortran_get_n_ranked_type_with_descriptor(type_t* scalar_type, int rank, const decl_context_t* decl_context);

LIBMF03_EXTERN char fortran_equivalent_tk_types(type_t* t1, type_t* t2);
LIBMF03_EXTERN char fortran_equivalent_tkr_types(type_t* t1, type_t* t2);

// States that this is an array of character type
LIBMF03_EXTERN char fortran_is_character_type(type_t*);
// A pointer to fortran_is_character_type
LIBMF03_EXTERN char fortran_is_pointer_to_character_type(type_t* t);
LIBMF03_EXTERN char fortran_is_character_type_or_pointer_to(type_t* t);

// States that this is an array of non-character type
LIBMF03_EXTERN char fortran_is_array_type(type_t* t);

// States that this is an scalar type
LIBMF03_EXTERN char fortran_is_scalar_type(type_t* t);

// A pointer to fortran_is_array_type
LIBMF03_EXTERN char fortran_is_pointer_to_array_type(type_t* t);
LIBMF03_EXTERN char fortran_is_array_type_or_pointer_to(type_t* t);

LIBMF03_EXTERN type_t* fortran_get_basic_type(type_t* type_info);
LIBMF03_EXTERN type_t* fortran_replace_return_type_of_function_type(type_t* function_type, 
        type_t* new_return_type);

LIBMF03_EXTERN type_t* fortran_update_basic_type_with_type(type_t* type_info, type_t* basic_type);

LIBMF03_EXTERN char fortran_basic_type_is_implicit_none(type_t* t);

LIBMF03_EXTERN type_t* fortran_rebuild_array_type(type_t* rank0_type, type_t* array_type);

LIBMF03_EXTERN int fortran_array_type_get_total_number_of_elements(type_t* t);

LIBMF03_EXTERN char fortran_array_has_zero_size(type_t* t);

LIBMF03_EXTERN char fortran_type_is_conformable_to(type_t* t1, type_t* t2);

LIBMF03_EXTERN type_t* fortran_get_default_integer_type(void);
LIBMF03_EXTERN type_t* fortran_get_default_real_type(void);
LIBMF03_EXTERN type_t* fortran_get_doubleprecision_type(void);
LIBMF03_EXTERN type_t* fortran_get_default_logical_type(void);
LIBMF03_EXTERN type_t* fortran_get_default_character_type(void);

LIBMF03_EXTERN int fortran_get_default_integer_type_kind(void);
LIBMF03_EXTERN int fortran_get_default_real_type_kind(void);
LIBMF03_EXTERN int fortran_get_doubleprecision_type_kind(void);
LIBMF03_EXTERN int fortran_get_default_logical_type_kind(void);
LIBMF03_EXTERN int fortran_get_default_character_type_kind(void);

LIBMF03_EXTERN void fortran_init_kinds(void);

LIBMF03_EXTERN type_t* fortran_choose_int_type_from_kind(int kind_size);
LIBMF03_EXTERN type_t* fortran_choose_float_type_from_kind(int kind_size);
LIBMF03_EXTERN type_t* fortran_choose_logical_type_from_kind(int kind_size);
LIBMF03_EXTERN type_t* fortran_choose_character_type_from_kind(int kind_size);

MCXX_END_DECLS

#endif // FORTRAN03_TYPEUTILS_H
