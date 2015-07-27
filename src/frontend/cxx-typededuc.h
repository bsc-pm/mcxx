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




#ifndef CXX_TYPEDEDUC_H
#define CXX_TYPEDEDUC_H

#include "libmcxx-common.h"
#include "cxx-typededuc-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN deduction_result_t deduce_template_arguments_from_a_type(
        type_t* parameter,
        type_t* argument,
        template_parameter_list_t* explicit_template_argument_list,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // flags
        int pack_index,
        int pack_length,
        char is_computing_address_of_function,
        char is_deducing_arguments_from_function_declaration,
        char is_partial_ordering,
        char ignore_cv_qualifiers,
        // out
        deduction_set_t* deduction_result);

LIBMCXX_EXTERN deduction_result_t deduce_template_arguments_from_function_declaration(
        type_t* potential_match, /* P */
        type_t* function_type_from_declaration, /* A */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments);

LIBMCXX_EXTERN deduction_result_t deduce_template_arguments_from_function_call(
        type_t** call_argument_types,
        int num_arguments,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments);

LIBMCXX_EXTERN deduction_result_t deduce_template_arguments_for_conversion_function(
        scope_entry_t* conversion_function,
        type_t* required_type, /* A */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments);

LIBMCXX_EXTERN char deduce_arguments_of_auto_initialization(
        type_t* destination_type,
        type_t* initializer_type,
        const decl_context_t* decl_context,
        template_parameter_list_t** deduced_template_arguments,
        char is_braced_array,
        const locus_t* locus);

LIBMCXX_EXTERN deduction_result_t deduce_template_arguments_from_address_of_a_function_template(
        type_t* specified_type, /* A */
        type_t* specialized_named_type, /* P */
        template_parameter_list_t* template_parameters,         // those of the primary
        template_parameter_list_t* type_template_parameters,    // those of the template-type
        template_parameter_list_t* raw_explicit_template_arguments, // explicit by the user
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t **out_deduced_template_arguments);

// Used in cxx-typeorder
deduction_result_t deduction_combine_to_second(
        deduction_set_t* current_deduction,
        // out
        deduction_set_t* previous_deduction);
void deduction_set_free(deduction_set_t* deduction_set);
deduction_result_t finish_deduced_template_arguments(
        template_parameter_list_t* type_template_parameters,
        deduction_set_t* deduction_result,
        const decl_context_t* decl_context,
        const locus_t* locus,
        /* inout */ template_parameter_list_t* deduced_template_arguments);
deduction_result_t handle_explicit_template_arguments(
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* raw_explicit_template_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        // out
        template_parameter_list_t** explicit_template_arguments);
template_parameter_list_t* build_template_parameter_list_from_deduction_set(
        template_parameter_list_t* template_parameters,
        deduction_set_t* deduction_set);

MCXX_END_DECLS

#endif // CXX_TYPEDEDUC_H
