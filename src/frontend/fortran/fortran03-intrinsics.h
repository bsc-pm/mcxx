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

#ifndef FORTRAN03_INTRINSICS_H
#define FORTRAN03_INTRINSICS_H

#include "libmf03-common.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN void copy_intrinsic_function_info(scope_entry_t* dest, scope_entry_t* intrinsic);

void fortran_init_intrinsics(const decl_context_t* decl_context);

scope_entry_t* fortran_solve_generic_intrinsic_call(scope_entry_t* symbol, 
        nodecl_t* nodecl_actual_arguments,
        int num_actual_arguments,
        char is_call);

void fortran_simplify_specific_intrinsic_call(scope_entry_t* symbol,
        nodecl_t* nodecl_actual_arguments,
        int num_actual_arguments,
        nodecl_t* nodecl_simplified,
        const locus_t* locus);

const decl_context_t* fortran_get_context_of_intrinsics(const decl_context_t* decl_context);

// These functions are for serialization purposes only
int fortran_intrinsic_get_id(computed_function_type_t t);
computed_function_type_t fortran_intrinsic_get_ptr(int id);
int fortran_simplify_function_get_id(simplify_function_t fun);
simplify_function_t fortran_simplify_function_get_ptr(int id);

MCXX_END_DECLS

#endif // FORTRAN03_INTRINSICS_H
