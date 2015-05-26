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




#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-instantiation-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-nodecl-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void instantiate_template_class_if_needed(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus);
LIBMCXX_EXTERN char instantiate_template_class_if_possible(scope_entry_t* entry, const decl_context_t* decl_context, const locus_t* locus);

LIBMCXX_EXTERN void instantiate_nontemplate_member_class_if_needed(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus);
LIBMCXX_EXTERN char instantiate_nontemplate_member_class_if_possible(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN AST instantiate_tree(AST orig_tree, const decl_context_t* context_of_being_instantiated);

LIBMCXX_EXTERN void instantiation_init(void);

LIBMCXX_EXTERN void instantiation_instantiate_pending_functions(nodecl_t* nodecl_output);

LIBMCXX_EXTERN void instantiation_add_symbol_to_instantiate(scope_entry_t* entry,
        const locus_t* locus);

LIBMCXX_EXTERN char function_may_be_instantiated(scope_entry_t* entry);
LIBMCXX_EXTERN void instantiate_template_function(scope_entry_t* entry, const locus_t* locus);

LIBMCXX_EXTERN void instantiate_template_function_and_integrate_in_translation_unit(scope_entry_t* entry,
        const locus_t* locus);

// Instantiation map
LIBMCXX_EXTERN scope_entry_t* instantiation_symbol_do_map(instantiation_symbol_map_t* map, scope_entry_t* orig);
LIBMCXX_EXTERN scope_entry_t* instantiation_symbol_try_to_map(instantiation_symbol_map_t* map, scope_entry_t* orig);

LIBMCXX_EXTERN void instantiation_symbol_map_add(instantiation_symbol_map_t* map, scope_entry_t* orig, scope_entry_t* new_sym);
LIBMCXX_EXTERN instantiation_symbol_map_t* instantiation_symbol_map_push(instantiation_symbol_map_t* parent);
LIBMCXX_EXTERN instantiation_symbol_map_t* instantiation_symbol_map_pop(instantiation_symbol_map_t* map);

MCXX_END_DECLS

#endif // CXX_INSTANTIATION_H
