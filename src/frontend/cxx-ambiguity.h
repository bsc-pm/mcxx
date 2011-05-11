/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#ifndef CXX_AMBIGUITY
#define CXX_AMBIGUITY

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-asttype.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

// Non contextual
LIBMCXX_EXTERN void solve_parameter_declaration_vs_type_parameter_class(AST a, decl_context_t decl_context);

// Contextual
LIBMCXX_EXTERN void solve_ambiguous_declaration(AST a, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_declarator(AST a, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_statement(AST a, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_init_declarator(AST a, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_type_specifier_seq(AST type_spec_seq, decl_context_t decl_context); // one alias to decl_specifier_seq
LIBMCXX_EXTERN void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_for_init_statement(AST for_init_statement, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_parameter_decl(AST parameter_declaration, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_exception_decl(AST exception_decl, decl_context_t decl_context);
LIBMCXX_EXTERN void solve_ambiguous_type_specifier(AST type_specifier, decl_context_t decl_context);

// These two are misleading, should be 'check_' instead of 'solve_'
LIBMCXX_EXTERN char solve_ambiguous_template_argument(AST ambig_template_argument, decl_context_t decl_context);
LIBMCXX_EXTERN char solve_possibly_ambiguous_template_id(AST type_name, decl_context_t decl_context);

LIBMCXX_EXTERN char check_nested_name_spec(AST nested_name_spec, decl_context_t decl_context);
LIBMCXX_EXTERN char check_type_id_tree(AST type_id, decl_context_t decl_context);
LIBMCXX_EXTERN char check_simple_type_spec(AST type_spec, decl_context_t decl_context, struct type_tag** computed_type);

LIBMCXX_EXTERN void solve_ambiguous_nested_name_specifier(AST a, decl_context_t decl_context);

LIBMCXX_EXTERN void solve_ambiguous_expression_list(AST expression_list, decl_context_t decl_context);
LIBMCXX_EXTERN char solve_ambiguous_expression(AST ambig_expression, decl_context_t decl_context);

LIBMCXX_EXTERN void solve_condition_ambiguity(AST a, decl_context_t decl_context);

LIBMCXX_EXTERN int either_type(AST t1, AST t2, node_t n1, node_t n2);

// States if we are checking ambiguities
LIBMCXX_EXTERN char checking_ambiguity(void);
LIBMCXX_EXTERN void enter_test_expression(void);
LIBMCXX_EXTERN void leave_test_expression(void);

// This is used when instantiating
LIBMCXX_EXTERN char get_test_expression_status(void);
LIBMCXX_EXTERN void set_test_expression_status(char c);

MCXX_END_DECLS

#endif // CXX_AMBIGUITY
