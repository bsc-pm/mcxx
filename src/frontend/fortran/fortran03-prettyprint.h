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



#ifndef FORTRAN03_PRETTYPRINT_H
#define FORTRAN03_PRETTYPRINT_H

#include "cxx-macros.h"
#include "libmf03-common.h"
#include "cxx-prettyprint.h"
#include "cxx-ast.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN void fortran_prettyprint(FILE* f, AST a);
LIBMF03_EXTERN const char* fortran_prettyprint_in_buffer(AST a);
LIBMF03_EXTERN const char* fortran_prettyprint_in_buffer_callback(AST a, prettyprint_callback_t callback, void *data);
LIBMCXX_EXTERN const char* fortran_list_handler_in_buffer(AST a);

MCXX_END_DECLS

#endif // FORTRAN03_PRETTYPRINT_H
