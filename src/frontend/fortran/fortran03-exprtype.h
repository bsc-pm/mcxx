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



#ifndef FORTRAN_EXPRTYPE_H
#define FORTRAN_EXPRTYPE_H

#include "cxx-macros.h"
#include "libmf03-common.h"

#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-typeutils.h"
#include "libmf03-common.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN char fortran_check_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

LIBMF03_EXTERN char fortran_check_array_bounds_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

LIBMF03_EXTERN void fortran_check_initialization(
        scope_entry_t* entry,
        AST expr, 
        const decl_context_t* decl_context, 
        char is_pointer_initialization,
        nodecl_t* nodecl_output);

LIBMF03_EXTERN void fortran_cast_initialization(scope_entry_t* entry,
        nodecl_t *nodecl_init);

LIBMF03_EXTERN type_t* common_type_of_binary_operation(type_t* t1, type_t* t2);
LIBMF03_EXTERN type_t* common_type_of_equality_operation(type_t* t1, type_t* t2);

LIBMF03_EXTERN scope_entry_t* fortran_data_ref_get_symbol(nodecl_t n);

LIBMF03_EXTERN nodecl_t fortran_expression_as_value(nodecl_t expr);
LIBMF03_EXTERN nodecl_t fortran_expression_as_variable(nodecl_t expr);

MCXX_END_DECLS

#endif // FORTRAN_EXPRTYPE_H
