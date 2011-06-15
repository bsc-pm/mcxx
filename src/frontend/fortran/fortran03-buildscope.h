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


#ifndef FORTRAN03_BUILDSCOPE_H
#define FORTRAN03_BUILDSCOPE_H

#include "cxx-macros.h"
#include "libmf03-common.h"

#include "cxx-driver-decls.h"
#include "cxx-type-decls.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN void fortran_initialize_translation_unit_scope(translation_unit_t* translation_unit);

LIBMF03_EXTERN void build_scope_fortran_translation_unit(translation_unit_t* translation_unit);

LIBMF03_EXTERN void fortran_build_scope_statement(AST statement, decl_context_t decl_context, nodecl_t* nodecl_output);

LIBMF03_EXTERN type_t* choose_int_type_from_kind(AST expr, int kind_size);
LIBMF03_EXTERN type_t* choose_float_type_from_kind(AST expr, int kind_size);
LIBMF03_EXTERN type_t* choose_logical_type_from_kind(AST expr, int kind_size);

LIBMF03_EXTERN void build_scope_program_unit(AST program_unit, 
        decl_context_t decl_context,
        decl_context_t (*new_context)(decl_context_t),
        scope_entry_t** program_unit_symbol,
        nodecl_t* nodecl_output);

LIBMF03_EXTERN scope_entry_t* function_get_result_symbol(scope_entry_t* entry);

LIBMF03_EXTERN scope_entry_t* get_data_symbol_info(decl_context_t decl_context_t);
LIBMF03_EXTERN scope_entry_t* get_equivalence_symbol_info(decl_context_t decl_context);

MCXX_END_DECLS

#endif // FORTRAN03_BUILDSCOPE_H
