/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_PRETTYPRINT_H
#define CXX_PRETTYPRINT_H

#include <stdio.h>
#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"

MCXX_BEGIN_DECLS

typedef const char* (*prettyprint_callback_t)(AST a);

LIBMCXX_EXTERN void prettyprint_set_main_filename(char* filename);
LIBMCXX_EXTERN void prettyprint(FILE* f, AST a);
LIBMCXX_EXTERN char* prettyprint_in_buffer(AST a);
LIBMCXX_EXTERN char* prettyprint_in_buffer_cb(AST a);
LIBMCXX_EXTERN char* list_handler_in_buffer(AST a);
LIBMCXX_EXTERN void prettyprint_set_not_internal_output(void);
LIBMCXX_EXTERN void prettyprint_set_internal_output(void);

MCXX_END_DECLS

#endif // CXX_PRETTYPRINT_H
