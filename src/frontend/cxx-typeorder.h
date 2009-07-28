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
#ifndef CXX_TYPEORDER_H
#define CXX_TYPEORDER_H

#include "libmcxx-common.h"
#include "cxx-scope-decls.h"
#include "cxx-typeunif.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN char is_less_or_equal_specialized_template_class(struct type_tag* c1, struct type_tag* c2, 
        decl_context_t decl_context, deduction_set_t** deduction_set, 
        const char *filename, int line);

LIBMCXX_EXTERN char is_less_or_equal_specialized_template_function(struct type_tag* f1, struct type_tag* f2,
        decl_context_t decl_context, deduction_set_t** deduction_set,
        template_argument_list_t* explicit_template_arguments,
        const char *filename, int line, char is_conversion);

LIBMCXX_EXTERN char is_sound_type(struct type_tag* t, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_TYPEORDER_H
