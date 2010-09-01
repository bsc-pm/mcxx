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

#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "libmcxx-common.h"
#include "cxx-overload-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN candidate_t* add_to_candidate_set(candidate_t* candidate_set,
        scope_entry_t* entry,
        int num_args,
        type_t** args);

LIBMCXX_EXTERN struct scope_entry_tag* solve_overload(candidate_t* candidate_set,
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversor_per_argument);

LIBMCXX_EXTERN char type_can_be_implicitly_converted_to(struct type_tag* orig, struct type_tag* dest, decl_context_t decl_context, 
        char *ambiguous_conversion, scope_entry_t** conversor);

LIBMCXX_EXTERN struct scope_entry_tag* address_of_overloaded_function(struct scope_entry_list_tag* overload_set, 
        template_argument_list_t* explicit_template_arguments,
        struct type_tag* target_type,
        decl_context_t decl_context,
        const char *filename,
        int line);

LIBMCXX_EXTERN scope_entry_t* solve_constructor(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors,
        scope_entry_list_t** candidates);

LIBMCXX_EXTERN scope_entry_t* solve_init_list_constructor(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors,
        scope_entry_list_t** candidates);

LIBMCXX_EXTERN unsigned long long overload_used_memory(void);

MCXX_END_DECLS

#endif // CXX_OVERLOAD_H
