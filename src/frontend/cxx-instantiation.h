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

#ifndef CXX_INSTANTIATION_H
#define CXX_INSTANTIATION_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void instantiate_template_class(struct scope_entry_tag* entry, decl_context_t decl_context, 
        const char *filename, int line);
LIBMCXX_EXTERN void instantiate_template_function(scope_entry_t* entry, 
        decl_context_t decl_context, const char* filename, int line);

LIBMCXX_EXTERN decl_context_t get_instantiation_context(scope_entry_t* entry, 
        template_parameter_list_t* template_parameters);

MCXX_END_DECLS

#endif // CXX_INSTANTIATION_H
