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



#ifndef CXX_EXPRTYPE_H
#define CXX_EXPRTYPE_H

#include "libmcxx-common.h"
#include "cxx-ast-decls.h"
#include "cxx-exprtype-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

/*
 * Computes the type of an expression
 */

LIBMCXX_EXTERN AST advance_expression_nest(AST expr);
LIBMCXX_EXTERN AST advance_expression_nest_flags(AST expr, char advance_parentheses);

LIBMCXX_EXTERN char can_be_called_with_number_of_arguments(scope_entry_t *entry, int num_arguments);

LIBMCXX_EXTERN char check_expression(AST a, decl_context_t decl_context);

LIBMCXX_EXTERN char check_expression_list(AST expression_list, decl_context_t decl_context);

LIBMCXX_EXTERN char check_initialization(AST initializer, decl_context_t decl_context, type_t* declared_type);

// Used in some TL phases, do not remove
LIBMCXX_EXTERN char check_initializer_clause(AST initializer, decl_context_t decl_context, type_t* declared_type);

LIBMCXX_EXTERN char check_zero_args_constructor(type_t* class_type, decl_context_t decl_context, AST declarator);

LIBMCXX_EXTERN unsigned long long exprtype_used_memory(void);

LIBMCXX_EXTERN scope_entry_list_t* unfold_and_mix_candidate_functions(
        scope_entry_list_t* result_from_lookup,
        scope_entry_list_t* builtin_list,
        type_t** argument_types,
        int num_arguments,
        decl_context_t decl_context,
        const char *filename,
        int line,
        template_argument_list_t *explicit_template_arguments
        );

LIBMCXX_EXTERN type_t* expression_get_type(AST expr);
LIBMCXX_EXTERN void expression_set_type(AST expr, type_t* t);



LIBMCXX_EXTERN void expression_set_error(AST expr);
LIBMCXX_EXTERN char expression_is_error(AST expr);

LIBMCXX_EXTERN char expression_is_constant(AST expr);
LIBMCXX_EXTERN void expression_set_non_constant(AST expr);
LIBMCXX_EXTERN void expression_set_constant(AST expr, const_value_t* const_val);
LIBMCXX_EXTERN const_value_t* expression_get_constant(AST expr);

LIBMCXX_EXTERN void expression_set_is_lvalue(AST expr, char is_lvalue);
LIBMCXX_EXTERN char expression_is_lvalue(AST expr);

LIBMCXX_EXTERN char expression_is_value_dependent(AST expr);
LIBMCXX_EXTERN void expression_set_is_value_dependent(AST expr, char value_dependent);

LIBMCXX_EXTERN char expression_has_symbol(AST expr);
LIBMCXX_EXTERN void expression_set_symbol(AST expr, scope_entry_t* entry);
LIBMCXX_EXTERN scope_entry_t* expression_get_symbol(AST expr);

LIBMCXX_EXTERN void expression_clear_computed_info(AST expr);

LIBMCXX_EXTERN unsigned long long expression_info_sizeof(void);

LIBMCXX_EXTERN type_t* compute_type_for_type_id_tree(AST type_id, decl_context_t decl_context);

LIBMCXX_EXTERN scope_entry_t* get_std_initializer_list_template(decl_context_t decl_context, AST expr, char mandatory);

LIBMCXX_EXTERN nodecl_t expression_get_nodecl(AST expr);
LIBMCXX_EXTERN void expression_set_nodecl(AST expr, nodecl_t nodecl_output);

// Internal function for the frontend only
char _check_functional_expression(AST whole_function_call, AST called_expression, 
        AST arguments, decl_context_t decl_context, char might_require_koenig);

MCXX_END_DECLS

#endif
