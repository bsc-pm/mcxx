/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef CXX_SCOPELINK_H
#define CXX_SCOPELINK_H

#include "cxx-macros.h"
#include "cxx-scopelink-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

scope_link_t* scope_link_new(decl_context_t global_decl_context);

void scope_link_set(scope_link_t* sl, AST a, decl_context_t decl_context);

decl_context_t scope_link_get_global_decl_context(scope_link_t* sl);

decl_context_t scope_link_get_decl_context(scope_link_t* sl, AST a);

char scope_link_direct_get_scope(scope_link_t* sl, AST a, 
        decl_context_t *decl_result);

unsigned long long scopelink_used_memory(void);

MCXX_END_DECLS

#endif // CXX_SCOPELINK_H
