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




#ifndef CXX_TYPEORDER_H
#define CXX_TYPEORDER_H

#include "libmcxx-common.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS


LIBMCXX_EXTERN char is_more_specialized_template_function(
        scope_entry_t* template_1,
        scope_entry_t* template_2,
        template_parameter_list_t* explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        char is_overload, int num_actual_arguments,
        char is_conversion,
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_requiring_exact_match,
        // out
        template_parameter_list_t** deduced_template_arguments);

LIBMCXX_EXTERN char is_more_specialized_template_class(
        type_t* c1, type_t* c2,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN char class_template_specialization_matches(
        type_t* c1, type_t* c2,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t** deduced_template_arguments);

LIBMCXX_EXTERN char is_more_specialized_template_function_in_overload(
        scope_entry_t* f1,
        scope_entry_t* f2,
        const decl_context_t* decl_context,
        template_parameter_list_t* explicit_template_arguments,
        const locus_t* locus,
        // Flags
        int num_actual_arguments,
        char is_conversion);

LIBMCXX_EXTERN char is_more_specialized_template_function_in_function_address(
        scope_entry_t* f1,
        scope_entry_t* f2,
        const decl_context_t* decl_context,
        template_parameter_list_t* explicit_template_arguments,
        const locus_t* locus,
        // Flags
        char is_conversion);

MCXX_END_DECLS

#endif // CXX_TYPEORDER_H
