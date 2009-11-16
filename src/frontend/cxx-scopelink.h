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

#ifndef CXX_SCOPELINK_H
#define CXX_SCOPELINK_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-scopelink-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN scope_link_t* scope_link_new(decl_context_t global_decl_context);

LIBMCXX_EXTERN void scope_link_set(scope_link_t* sl, AST a, decl_context_t decl_context);

LIBMCXX_EXTERN void scope_link_unset(scope_link_t* sl, AST a);

LIBMCXX_EXTERN decl_context_t scope_link_get_global_decl_context(scope_link_t* sl);

LIBMCXX_EXTERN decl_context_t scope_link_get_decl_context(scope_link_t* sl, AST a);

LIBMCXX_EXTERN char scope_link_direct_get_scope(scope_link_t* sl, AST a, 
        decl_context_t *decl_result);

LIBMCXX_EXTERN unsigned long long scopelink_used_memory(void);

MCXX_END_DECLS

#endif // CXX_SCOPELINK_H
