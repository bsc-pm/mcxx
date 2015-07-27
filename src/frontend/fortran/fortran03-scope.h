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



#ifndef FORTRAN03_SCOPE_H
#define FORTRAN03_SCOPE_H

#include "cxx-macros.h"
#include "libmf03-common.h"
#include "cxx-scope-decls.h"
#include "cxx-typeutils.h"
#include "fortran03-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN const decl_context_t* new_program_unit_context(const decl_context_t*);
LIBMF03_EXTERN const decl_context_t* new_internal_program_unit_context(const decl_context_t*);
LIBMF03_EXTERN const decl_context_t* fortran_new_block_context(const decl_context_t*);

// The first function avoids implicit symbols. The second query
// only shows implicit symbols.
LIBMF03_EXTERN scope_entry_t* fortran_query_name_str(const decl_context_t* decl_context, 
        const char* unqualified_name,
        const locus_t* locus);
LIBMF03_EXTERN scope_entry_list_t* fortran_query_name_str_for_function(const decl_context_t* decl_context, 
        const char* unqualified_name,
        const locus_t* locus);
LIBMF03_EXTERN scope_entry_t* fortran_query_intrinsic_name_str(const decl_context_t* decl_context, const char* unqualified_name);

// Creates a new fortran symbol. Use this function instead of new_symbol as
// this one takes care of case. It always uses current_scope.
LIBMF03_EXTERN scope_entry_t* new_fortran_symbol(const decl_context_t*, const char* name);

// Like new_fortran_symbol but does not sign the symbol in the unknown type set
LIBMF03_EXTERN scope_entry_t* new_fortran_symbol_not_unknown(const decl_context_t*, const char* name);

// Creates a new fortran implicit type appearing in an expression
LIBMF03_EXTERN scope_entry_t* new_fortran_implicit_symbol(const decl_context_t* decl_context, AST locus, const char* name);

// Use this one for typechecking of expressions only. This function uses
// IMPLIICT info and creates SK_VARIABLEs as needed using the locus
// information. locus may be NULL
LIBMF03_EXTERN scope_entry_t* fortran_get_variable_with_locus(const decl_context_t*, AST locus, const char* name);

// Query of derived type. 
// class_context should have a current_scope->kind ==  CLASS_SCOPE
LIBMF03_EXTERN scope_entry_t* query_name_in_class(const decl_context_t* class_context, const char* name, const locus_t* locus);

// IMPLICIT info functions
LIBMF03_EXTERN void set_implicit_info(const decl_context_t* decl_context, char from_letter, char to_letter, type_t* type);
LIBMF03_EXTERN void set_implicit_none(const decl_context_t* decl_context);
LIBMF03_EXTERN char is_implicit_none(const decl_context_t* decl_context);
LIBMF03_EXTERN char implicit_has_been_set(const decl_context_t* decl_context);

LIBMF03_EXTERN type_t* get_implicit_type_for_symbol(const decl_context_t* decl_context, const char* name);

LIBMF03_EXTERN scope_entry_list_t* fortran_query_module_for_name(scope_entry_t* module_symbol, const char* name);

// Gets the ultimate symbol of a symbol coming from a module
LIBMF03_EXTERN scope_entry_t* fortran_get_ultimate_symbol(scope_entry_t* entry);

MCXX_END_DECLS

#endif // FORTRAN03_SCOPE_H
