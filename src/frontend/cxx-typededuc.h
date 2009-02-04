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
#ifndef CXX_TYPEDEDUC_H
#define CXX_TYPEDEDUC_H

#include "libmcxx-common.h"
#include "cxx-typeunif.h"

LIBMCXX_EXTERN char deduce_template_arguments_common(
        template_parameter_list_t* template_parameters,
        struct type_tag** arguments, int num_arguments,
        struct type_tag** parameters,
        decl_context_t decl_context,
        deduction_set_t **deduced_arguments,
        const char *filename, int line,
        template_argument_list_t* explicit_template_arguments);

LIBMCXX_EXTERN char deduce_arguments_from_call_to_specific_template_function(struct type_tag** call_argument_types,
        int num_arguments, struct type_tag* specialized_named_type, 
        template_parameter_list_t* template_parameters, 
        decl_context_t decl_context,
        deduction_set_t **deduction_result, 
        const char* filename, int line,
        template_argument_list_t* explicit_template_arguments);

LIBMCXX_EXTERN char deduce_arguments_of_conversion(
        struct type_tag* destination_type,
        struct type_tag* specialized_named_type,
        template_parameter_list_t* template_parameters,
        decl_context_t decl_context_t,
        deduction_set_t **deduction_result,
        const char *filename, int line);

LIBMCXX_EXTERN template_argument_list_t* build_template_argument_list_from_deduction_set(
        deduction_set_t* deduction_set);

LIBMCXX_EXTERN unsigned long long int typededuc_used_memory(void);

#endif // CXX_TYPEDEDUC_H
