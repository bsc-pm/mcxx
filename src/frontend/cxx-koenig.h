/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef CXX_KOENIG_H
#define CXX_KOENIG_H

#include "libmcxx-common.h"
#include "cxx-scope.h"
#include "cxx-type-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN struct scope_entry_list_tag* koenig_lookup(
        int num_arguments,
        struct type_tag** argument_type_list,
        decl_context_t normal_decl_context,
        AST id_expression);

MCXX_END_DECLS

#endif // CXX_KOENIG_H
