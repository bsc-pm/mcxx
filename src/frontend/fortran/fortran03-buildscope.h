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



#ifndef FORTRAN03_BUILDSCOPE_H
#define FORTRAN03_BUILDSCOPE_H

#include "cxx-macros.h"
#include "libmf03-common.h"

#include "cxx-driver-decls.h"
#include "cxx-type-decls.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN void fortran_initialize_translation_unit_scope(translation_unit_t* translation_unit);

LIBMF03_EXTERN nodecl_t build_scope_fortran_translation_unit(translation_unit_t* translation_unit);

LIBMF03_EXTERN void fortran_build_scope_statement(AST statement, const decl_context_t* decl_context, nodecl_t* nodecl_output);

LIBMF03_EXTERN type_t* choose_int_type_from_kind(nodecl_t expr, int kind_size);
LIBMF03_EXTERN type_t* choose_float_type_from_kind(nodecl_t expr, int kind_size);
LIBMF03_EXTERN type_t* choose_logical_type_from_kind(nodecl_t expr, int kind_size);
LIBMF03_EXTERN type_t* choose_character_type_from_kind(nodecl_t expr, int kind_size);

LIBMF03_EXTERN type_t* fortran_gather_type_from_declaration_type_spec(AST a,
        const decl_context_t* decl_context,
        AST* length);

LIBMF03_EXTERN void build_scope_program_unit_seq(AST program_unit_seq, 
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

LIBMF03_EXTERN scope_entry_t* function_get_result_symbol(scope_entry_t* entry);

LIBMF03_EXTERN scope_entry_t* fortran_get_data_symbol_info(const decl_context_t* decl_context);
LIBMF03_EXTERN scope_entry_t* fortran_get_equivalence_symbol_info(const decl_context_t* decl_context);

LIBMF03_EXTERN scope_entry_t* get_or_create_used_modules_symbol_info(const decl_context_t* decl_context);

LIBMF03_EXTERN scope_entry_t* fortran_query_label(AST label, 
        const decl_context_t* decl_context, 
        char is_definition);

LIBMF03_EXTERN void add_untyped_symbol(const decl_context_t* decl_context, scope_entry_t* entry);
LIBMF03_EXTERN void remove_untyped_symbol(const decl_context_t* decl_context, scope_entry_t* entry);

LIBMF03_EXTERN void add_unknown_kind_symbol(const decl_context_t* decl_context, scope_entry_t* entry);
LIBMF03_EXTERN void remove_unknown_kind_symbol(const decl_context_t* decl_context, scope_entry_t* entry);

LIBMF03_EXTERN scope_entry_t* query_common_name(const decl_context_t* decl_context, const char* common_name,
        const locus_t* locus);

LIBMF03_EXTERN scope_entry_t* fortran_load_module(const char* module_name_str, char must_be_intrinsic_module,
        const locus_t* locus);

LIBMF03_EXTERN scope_entry_t* insert_symbol_from_module(scope_entry_t* entry, 
        const decl_context_t* decl_context, 
        const char* local_name, 
        scope_entry_t* module_symbol,
        const locus_t* locus);

MCXX_END_DECLS

#endif // FORTRAN03_BUILDSCOPE_H
