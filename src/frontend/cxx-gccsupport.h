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



#ifndef CXX_GCCSUPPORT_H
#define CXX_GCCSUPPORT_H

#include "libmcxx-common.h"
#include "cxx-ast-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void gather_gcc_attribute(AST attribute, 
        gather_decl_spec_t* gather_info, 
        decl_context_t decl_context);

LIBMCXX_EXTERN void gather_gcc_attribute_list(AST attribute_list, 
        gather_decl_spec_t *gather_info, 
        decl_context_t decl_context);

LIBMCXX_EXTERN char check_for_gxx_type_traits(AST expression, 
        decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_GCCSUPPORT_H
