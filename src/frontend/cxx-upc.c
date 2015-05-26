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




#include "cxx-upc.h"

#include "cxx-scope.h"
#include "cxx-typeutils.h"
#include "cxx-driver.h"
#include "cxx-buildscope.h"
#include "cxx-exprtype.h"
#include "uniquestr.h"

void upc_sign_in_builtins(const decl_context_t* decl_context)
{
    // THREADS
    scope_entry_t* upc_THREADS;

    upc_THREADS = new_symbol(decl_context, decl_context->global_scope, UNIQUESTR_LITERAL("THREADS"));
    upc_THREADS->kind = SK_VARIABLE;
    upc_THREADS->type_information = get_const_qualified_type(get_signed_int_type());
    upc_THREADS->defined = 1;
    upc_THREADS->do_not_print = 1;
    upc_THREADS->locus = make_locus("(global scope)", 0, 0);
    symbol_entity_specs_set_is_builtin(upc_THREADS, 1);
    if (CURRENT_CONFIGURATION->upc_threads != NULL)
    {
        upc_THREADS->value = internal_expression_parse(CURRENT_CONFIGURATION->upc_threads, decl_context);
    }

    // MYTHREAD
    scope_entry_t* upc_MYTHREAD;

    upc_MYTHREAD = new_symbol(decl_context, decl_context->global_scope, UNIQUESTR_LITERAL("MYTHREAD"));
    upc_MYTHREAD->kind = SK_VARIABLE;
    upc_MYTHREAD->type_information = get_const_qualified_type(get_signed_int_type());
    upc_MYTHREAD->defined = 1;
    upc_MYTHREAD->do_not_print = 1;
    upc_MYTHREAD->locus = make_locus("(global scope)", 0, 0);
    symbol_entity_specs_set_is_builtin(upc_MYTHREAD, 1);
    
    // UPC_MAX_BLOCK_SIZE
    scope_entry_t* upc_UPC_MAX_BLOCK_SIZE;

    upc_UPC_MAX_BLOCK_SIZE = new_symbol(decl_context, decl_context->global_scope, UNIQUESTR_LITERAL("UPC_MAX_BLOCK_SIZE"));
    upc_UPC_MAX_BLOCK_SIZE->kind = SK_VARIABLE;
    upc_UPC_MAX_BLOCK_SIZE->type_information = get_const_qualified_type(get_signed_int_type());
    upc_UPC_MAX_BLOCK_SIZE->defined = 1;
    upc_UPC_MAX_BLOCK_SIZE->do_not_print = 1;
    upc_UPC_MAX_BLOCK_SIZE->locus = make_locus("(global scope)", 0, 0);
    symbol_entity_specs_set_is_builtin(upc_UPC_MAX_BLOCK_SIZE, 1);

    // upc_lock_t
    scope_entry_t* upc_lock_t;

    upc_lock_t = new_symbol(decl_context, decl_context->global_scope, UNIQUESTR_LITERAL("upc_lock_t"));
    upc_lock_t->kind = SK_TYPEDEF;
    upc_lock_t->defined = 1;
    upc_lock_t->type_information = get_void_type();
    upc_lock_t->do_not_print = 1;
    upc_lock_t->locus = make_locus("(global scope)", 0, 0);
    symbol_entity_specs_set_is_builtin(upc_lock_t, 1);
}

