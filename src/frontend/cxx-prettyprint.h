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




#ifndef CXX_PRETTYPRINT_H
#define CXX_PRETTYPRINT_H

#include <stdio.h>
#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"

MCXX_BEGIN_DECLS

typedef const char* (*prettyprint_callback_t)(AST a, void *data);

LIBMCXX_EXTERN void prettyprint(FILE* f, AST a);
LIBMCXX_EXTERN const char* prettyprint_in_buffer(AST a);
LIBMCXX_EXTERN const char* prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data);
LIBMCXX_EXTERN const char* list_handler_in_buffer(AST a);

LIBMCXX_EXTERN void prettyprint_set_not_internal_output(void);
LIBMCXX_EXTERN void prettyprint_set_internal_output(void);

LIBMCXX_EXTERN void cxx_prettyprint(FILE* f, AST a);
LIBMCXX_EXTERN const char* cxx_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data);
LIBMCXX_EXTERN const char* cxx_list_handler_in_buffer(AST a);

MCXX_END_DECLS

#endif // CXX_PRETTYPRINT_H
