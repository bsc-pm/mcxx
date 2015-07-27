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




#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "libmcxx-common.h"
#include "cxx-overload-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN candidate_t* candidate_set_add(candidate_t* candidate_set,
        scope_entry_t* entry,
        int num_args,
        type_t** args);

LIBMCXX_EXTERN void candidate_set_free(candidate_t** candidate_set);

LIBMCXX_EXTERN scope_entry_t* solve_overload(candidate_t* candidate_set,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN char solve_initialization_of_nonclass_type(
        type_t* orig,
        type_t* dest,
        const decl_context_t* decl_context, 
        enum initialization_kind initialization_kind,
        scope_entry_t** conversor,
        scope_entry_list_t** candidates,
        const locus_t* locus);

LIBMCXX_EXTERN scope_entry_t* address_of_overloaded_function(scope_entry_list_t* overload_set,
        template_parameter_list_t* explicit_template_arguments,
        struct type_tag* target_type,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN char solve_initialization_of_class_type(
        type_t* class_type, 
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** constructor,
        scope_entry_list_t** candidates);

LIBMCXX_EXTERN char solve_list_initialization_of_class_type(
        type_t* class_type,
        type_t** argument_types, 
        int num_arguments,
        enum initialization_kind initialization_kind,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** constructor,
        scope_entry_list_t** candidates);

MCXX_END_DECLS

#endif // CXX_OVERLOAD_H
