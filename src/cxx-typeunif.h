/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

typedef 
struct unification_item_tag
{
    // parameter type <- value
    int parameter_num;
    int parameter_nesting;
    char* parameter_name;
    
    type_t* value;
    AST expression;
} unification_item_t;

typedef struct 
{
    int num_elems;
    unification_item_t** unif_list;
} unification_set_t;

char unificate_two_types(type_t* t1, type_t* t2, scope_t* st, 
        unification_set_t** unif_set, decl_context_t decl_context);
char unificate_two_expressions(unification_set_t **unif_set, 
        AST left_tree, scope_t* left_scope, 
        AST right_tree, scope_t* right_scope, decl_context_t decl_context);

MCXX_END_DECLS

#endif
