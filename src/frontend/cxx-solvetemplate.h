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
#ifndef CXX_SOLVETEMPLATE_H
#define CXX_SOLVETEMPLATE_H

#include "libmcxx-common.h"
#include "cxx-macros.h"

#include "cxx-typeunif.h"
#include "cxx-buildscope-decls.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN struct type_tag* solve_class_template(decl_context_t decl_context,
        struct type_tag* template_type,
        struct type_tag* specialized_type,
        deduction_set_t** deduction_set,
        const char *filename,
        int line);

LIBMCXX_EXTERN struct scope_entry_tag* solve_template_function(struct scope_entry_list_tag* template_set,
        template_argument_list_t* explicit_template_arguments,
        struct type_tag* function_type, decl_context_t decl_context,
        const char *filename, int line);

MCXX_END_DECLS

#endif // CXX_SOLVETEMPLATE_H
