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
#ifndef CXX_AMBIGUITY
#define CXX_AMBIGUITY

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

// Non contextual
void solve_parameter_declaration_vs_type_parameter_class(AST a, decl_context_t decl_context);

// Contextual
void solve_ambiguous_declaration(AST a, decl_context_t decl_context);
void solve_ambiguous_declarator(AST a, decl_context_t decl_context);
void solve_ambiguous_statement(AST a, decl_context_t decl_context);
void solve_ambiguous_init_declarator(AST a, decl_context_t decl_context);
void solve_ambiguous_type_specifier_seq(AST type_spec_seq, decl_context_t decl_context); // one alias to decl_specifier_seq
void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, decl_context_t decl_context);
void solve_ambiguous_for_init_statement(AST for_init_statement, decl_context_t decl_context);
void solve_ambiguous_parameter_decl(AST parameter_declaration, decl_context_t decl_context);
void solve_ambiguous_exception_decl(AST exception_decl, decl_context_t decl_context);
void solve_ambiguous_type_specifier(AST type_specifier, decl_context_t decl_context);
void solve_possibly_ambiguous_expression(AST a, decl_context_t decl_context);

// These two are misleading, should be 'check_for_' instead of 'solve_'
char solve_ambiguous_template_argument(AST ambig_template_argument, decl_context_t decl_context);
char solve_possibly_ambiguous_template_id(AST type_name, decl_context_t decl_context);

char check_for_expression(AST a, decl_context_t decl_context);
char check_for_initialization(AST initializer, decl_context_t decl_context);
char check_nested_name_spec(AST nested_name_spec, decl_context_t decl_context);

void solve_ambiguous_expression_list(AST expression_list, decl_context_t decl_context);

MCXX_END_DECLS

#endif // CXX_AMBIGUITY
