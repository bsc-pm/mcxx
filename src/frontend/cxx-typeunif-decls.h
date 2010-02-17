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

#ifndef CXX_TYPEUNIF_DECLS_H
#define CXX_TYPEUNIF_DECLS_H

#include "cxx-macros.h"

#include "cxx-type-decls.h"
#include "cxx-ast-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

typedef
struct deduced_parameter_tag
{
    struct type_tag* type;
    struct AST_tag* expression;
    decl_context_t decl_context;
} deduced_parameter_t;

typedef 
struct deduction_tag
{
    enum template_parameter_kind kind;
    int parameter_position;
    int parameter_nesting;
    const char* parameter_name;
    
    int num_deduced_parameters;
    deduced_parameter_t** deduced_parameters;
} deduction_t;

typedef struct deduction_set_tag
{
    int num_deductions;
    deduction_t** deduction_list;
} deduction_set_t;

MCXX_END_DECLS

#endif // CXX_TYPEUNIF_DECLS_H
