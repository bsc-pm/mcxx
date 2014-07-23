/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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




#ifndef CXX_TYPEDEDUC_H
#define CXX_TYPEDEDUC_H

#include "libmcxx-common.h"
#include "cxx-typeunif.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN char deduce_template_arguments_common(
        // These are the template parameters of this function specialization
        template_parameter_list_t* template_parameters,
        // These are the template parameters of template-type
        // We need these because of default template arguments for template
        // functions (they are not kept in each specialization)
        template_parameter_list_t* type_template_parameters,
        type_t** arguments, int num_arguments,
        type_t** parameters, int num_parameters,
        type_t** original_parameters,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus,
        template_parameter_list_t* explicit_template_parameters,
        char is_function_call);

LIBMCXX_EXTERN char deduce_arguments_from_call_to_specific_template_function(struct type_tag** call_argument_types,
        int num_arguments, struct type_tag* specialized_named_type, 
        template_parameter_list_t* template_parameters, 
        template_parameter_list_t* type_template_parameters, 
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus,
        template_parameter_list_t* explicit_template_parameters);

LIBMCXX_EXTERN char deduce_arguments_of_conversion(
        type_t* destination_type,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* type_template_parameters,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const locus_t* locus);

LIBMCXX_EXTERN char deduce_arguments_of_auto_initialization(
        type_t* destination_type,
        type_t* initializer_type,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        char is_braced_array,
        const locus_t* locus);

LIBMCXX_EXTERN unsigned long long int typededuc_used_memory(void);

LIBMCXX_EXTERN void deduction_set_merge(deduction_set_t* dest, deduction_set_t* source,
        char combine_sequence);
LIBMCXX_EXTERN deduction_t* deduction_set_get_unification_item(
        deduction_set_t* deduction_set,
        int position, int nesting,
        enum template_parameter_kind kind,
        const char* name);
LIBMCXX_EXTERN deduction_t* deduction_set_get_unification_item_for_template_parameter(deduction_set_t* deduction_set,
        scope_entry_t* s1);
LIBMCXX_EXTERN char deduction_set_add_nontype_parameter_deduction(deduction_t* deduction,
        deduced_parameter_t* current_deduced_parameter);
LIBMCXX_EXTERN char deduction_set_add_type_parameter_deduction(deduction_t* deduction,
        deduced_parameter_t* current_deduced_parameter);

MCXX_END_DECLS

#endif // CXX_TYPEDEDUC_H
