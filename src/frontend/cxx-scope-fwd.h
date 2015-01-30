/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#ifndef CXX_SCOPE_FWD_H
#define CXX_SCOPE_FWD_H

#include "cxx-macros.h"

MCXX_BEGIN_DECLS

struct scope_entry_tag;
typedef struct scope_entry_tag scope_entry_t;

struct decl_context_tag;
typedef struct decl_context_tag decl_context_t;

struct template_parameter_tag;
typedef struct template_parameter_tag template_parameter_t;

struct template_parameter_value_tag;
typedef struct template_parameter_value_tag template_parameter_value_t;

struct template_parameter_list_tag;
typedef struct template_parameter_list_tag template_parameter_list_t;

struct default_argument_info_tag;
typedef struct default_argument_info_tag default_argument_info_t;

struct function_parameter_info_tag;
typedef struct function_parameter_info_tag function_parameter_info_t;

struct scope_tag;
typedef struct scope_tag scope_t;

MCXX_END_DECLS

#endif // CXX_SCOPE_FWD_H
