/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_OVERLOAD_H
#define CXX_OVERLOAD_H

#include "libmcxx-common.h"
#include "cxx-type-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN struct scope_entry_tag* solve_overload(struct scope_entry_list_tag* candidate_functions, 
        struct type_tag **argument_types, int num_arguments,
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

LIBMCXX_EXTERN scope_entry_t* solve_constructor(type_t* class_type, 
        type_t** arguments, 
        int num_arguments,
        char is_explicit, 
        decl_context_t decl_context,
        const char* filename, int line,
        scope_entry_t** conversors);

LIBMCXX_EXTERN unsigned long long overload_used_memory(void);

MCXX_END_DECLS

#endif // CXX_OVERLOAD_H
