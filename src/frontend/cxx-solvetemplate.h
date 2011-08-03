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



#ifndef CXX_SOLVETEMPLATE_H
#define CXX_SOLVETEMPLATE_H

#include "libmcxx-common.h"
#include "cxx-macros.h"

#include "cxx-typeunif.h"
#include "cxx-buildscope-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN struct type_tag* solve_class_template(struct type_tag* template_type,
        struct type_tag* specialized_type,
        deduction_set_t** deduction_set,
        const char *filename,
        int line);

LIBMCXX_EXTERN scope_entry_t* solve_template_function(scope_entry_list_t* template_set,
        template_parameter_list_t* explicit_template_parameters,
        struct type_tag* function_type, 
        const char *filename, int line);

MCXX_END_DECLS

#endif // CXX_SOLVETEMPLATE_H
