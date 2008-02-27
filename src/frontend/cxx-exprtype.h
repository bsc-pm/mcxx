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
#ifndef CXX_EXPRTYPE_H
#define CXX_EXPRTYPE_H

#include "cxx-ast-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

/*
 * Computes the type of an expression
 *
 * Only implemented for C
 *
 */
struct type_tag* decimal_literal_type(AST expr);
struct type_tag* character_literal_type(AST expr);
struct type_tag* floating_literal_type(AST expr);
struct type_tag* string_literal_type(AST expr);

struct type_tag *compute_expression_type(AST expr, decl_context_t decl_context, 
        char *is_lvalue) __attribute__((deprecated));

char check_for_initialization(AST initializer, decl_context_t decl_context);

AST advance_expression_nest(AST expr);

char can_be_called_with_number_of_arguments(scope_entry_t *entry, int num_arguments);

char check_for_expression(AST a, decl_context_t decl_context);

unsigned long long exprtype_used_memory(void);

MCXX_END_DECLS

#endif
