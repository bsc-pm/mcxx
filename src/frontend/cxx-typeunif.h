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
#ifndef CXX_TYPEUNIF_H
#define CXX_TYPEUNIF_H

#include "libmcxx-common.h"
#include "cxx-macros.h"

#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-typeunif-decls.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN void unificate_two_types(struct type_tag* t1, struct type_tag* t2, deduction_set_t** unif_set, decl_context_t decl_context, 
        const char* filename, int line);
LIBMCXX_EXTERN void unificate_two_expressions(deduction_set_t **unif_set, 
        struct AST_tag* left_tree, decl_context_t left_decl_context, 
        struct AST_tag* right_tree, decl_context_t right_decl_context);
LIBMCXX_EXTERN char equivalent_dependent_expressions(struct AST_tag* left_tree, decl_context_t left_decl_context, struct AST_tag*
        right_tree, decl_context_t right_decl_context, deduction_set_t** unif_set);
LIBMCXX_EXTERN char same_functional_expression(struct AST_tag* left_tree, decl_context_t left_decl_context, struct AST_tag* right_tree, 
        decl_context_t right_decl_context);

LIBMCXX_EXTERN deduction_t* get_unification_item_template_parameter(deduction_set_t** deduction_set, 
        struct scope_entry_tag* s1);

LIBMCXX_EXTERN long long int typeunif_used_memory(void);

LIBMCXX_EXTERN void unificate_two_id_expressions(AST left_id_expr, AST right_id_expr,
        decl_context_t left_decl_context, 
        decl_context_t right_decl_context,
        deduction_set_t** deduction_set);

MCXX_END_DECLS

#endif
