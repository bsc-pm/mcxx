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


#ifndef FORTRAN03_SCOPE_H
#define FORTRAN03_SCOPE_H

#include "cxx-macros.h"
#include "libmf03-common.h"
#include "cxx-scope-decls.h"
#include "cxx-typeutils.h"
#include "fortran03-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN decl_context_t new_program_unit_context(decl_context_t);
LIBMF03_EXTERN decl_context_t new_internal_program_unit_context(decl_context_t);
LIBMF03_EXTERN decl_context_t fortran_new_block_context(decl_context_t);

LIBMF03_EXTERN scope_entry_t* new_fortran_symbol(decl_context_t, const char* name);
LIBMF03_EXTERN scope_entry_t* query_name_no_implicit_or_builtin(decl_context_t, const char* name);
LIBMF03_EXTERN scope_entry_t* query_name_no_implicit(decl_context_t, const char* name);
LIBMF03_EXTERN scope_entry_t* query_name_with_locus(decl_context_t, AST locus, const char* name);

LIBMF03_EXTERN scope_entry_t* query_name_in_class(decl_context_t class_context, const char* name);

LIBMF03_EXTERN void set_implicit_info(decl_context_t decl_context, char from_letter, char to_letter, type_t* type);
LIBMF03_EXTERN void set_implicit_none(decl_context_t decl_context);
LIBMF03_EXTERN char is_implicit_none(decl_context_t decl_context);
LIBMF03_EXTERN char implicit_has_been_set(decl_context_t decl_context);

LIBMF03_EXTERN type_t* get_implicit_type_for_symbol(decl_context_t decl_context, const char* name);

MCXX_END_DECLS

#endif // FORTRAN03_SCOPE_H
