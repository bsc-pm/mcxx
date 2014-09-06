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


#ifndef CXX_TYPEDEDUC_DECLS_H
#define CXX_TYPEDEDUC_DECLS_H

#include "cxx-macros.h"

#include "cxx-type-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

typedef
enum deduction_result_tag
{
    DEDUCTION_FAILURE = 0,
    DEDUCTION_OK = 1,
} deduction_result_t;

typedef
struct deduced_argument_tag
{
    type_t* type;

    // This tree is owned by this structure
    nodecl_t value;
} deduced_argument_t;

// Deprecated name
typedef deduced_argument_t deduced_parameter_t;

typedef 
struct deduction_tag
{
    enum template_parameter_kind kind;
    int parameter_position;
    int parameter_nesting;
    const char* parameter_name;
    
    // FIXME - Change field name: num_deduced_parameters -> num_deduced_arguments
    // num_deduced_parameters is >1 only for template-packs
    int num_deduced_parameters;
    // FIXME - Change field name: deduced_parameters -> deduced_arguments
    deduced_argument_t** deduced_parameters;
} deduction_t;

typedef struct deduction_set_tag
{
    int num_deductions;
    deduction_t** deduction_list;
} deduction_set_t;


MCXX_END_DECLS

#endif
