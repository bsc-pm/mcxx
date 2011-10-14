/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

LIBMCXX_EXTERN char deduce_template_parameters_common(
        // These are the template parameters of this function specialization
        template_parameter_list_t* template_parameters,
        // These are the template parameters of template-type
        // We need these because of default template arguments for template
        // functions (they are not kept in each specialization)
        template_parameter_list_t* type_template_parameters,
        type_t** arguments, int num_arguments,
        type_t** parameters,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const char *filename, int line,
        template_parameter_list_t* explicit_template_parameters,
        deduction_flags_t flags);

LIBMCXX_EXTERN char deduce_arguments_from_call_to_specific_template_function(struct type_tag** call_argument_types,
        int num_arguments, struct type_tag* specialized_named_type, 
        template_parameter_list_t* template_parameters, 
        template_parameter_list_t* type_template_parameters, 
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const char* filename, int line,
        template_parameter_list_t* explicit_template_parameters);

LIBMCXX_EXTERN char deduce_arguments_of_conversion(
        type_t* destination_type,
        type_t* specialized_named_type,
        template_parameter_list_t* template_parameters,
        template_parameter_list_t* type_template_parameters,
        decl_context_t decl_context,
        template_parameter_list_t** deduced_template_arguments,
        const char *filename, int line);

LIBMCXX_EXTERN unsigned long long int typededuc_used_memory(void);

MCXX_END_DECLS

#endif // CXX_TYPEDEDUC_H
