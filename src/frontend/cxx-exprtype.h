/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "cxx-instantiation-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-typeutils.h"
#include "cxx-cexpr.h"
#include "cxx-nodecl-output.h"
#include "cxx-overload-decls.h"

MCXX_BEGIN_DECLS

/*
 * Computes the type of an expression
 */

LIBMCXX_EXTERN AST advance_expression_nest(AST expr);
LIBMCXX_EXTERN AST advance_expression_nest_flags(AST expr, char advance_parentheses);

LIBMCXX_EXTERN char can_be_called_with_number_of_arguments(scope_entry_t *entry, int num_arguments);

LIBMCXX_EXTERN char check_expression(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
LIBMCXX_EXTERN char check_expression_must_be_constant(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
LIBMCXX_EXTERN char check_expression_non_executable(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);
LIBMCXX_EXTERN char check_expression_non_executable_must_be_constant(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output);

LIBMCXX_EXTERN char check_list_of_expressions(AST expression_list, const decl_context_t* decl_context, nodecl_t* nodecl_output);

LIBMCXX_EXTERN nodecl_t nodecl_expression_make_rvalue(nodecl_t nodecl_expr, const decl_context_t* decl_context);

LIBMCXX_EXTERN char check_list_of_initializer_clauses(AST initializer_clause_list,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN char check_initialization(AST initializer, 
        const decl_context_t* decl_context, 
        scope_entry_t* initialized_entry, // May have its type_information updated
        type_t* declared_type,
        nodecl_t* nodecl_output,
        char is_auto_type,
        char is_decltype_auto);

LIBMCXX_EXTERN void check_nodecl_initialization(
        nodecl_t nodecl_initializer,
        const decl_context_t* decl_context,
        scope_entry_t* initialized_entry, // May have its type_information updated
        type_t* declared_type,
        nodecl_t* nodecl_output,
        char is_auto_type,
        char is_decltype_auto);

LIBMCXX_EXTERN void compute_nodecl_initialization(AST initializer,
        const decl_context_t* decl_context,
        char preserve_top_level_parentheses,
        nodecl_t* nodecl_output);

// Used in some TL phases, do not remove
LIBMCXX_EXTERN void check_initializer_clause(AST initializer,
        const decl_context_t* decl_context,
        type_t* declared_type,
        char is_decltype_auto,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN char check_default_initialization(scope_entry_t* entry, const decl_context_t* decl_context, 
        const locus_t* locus,
        scope_entry_t** constructor);

LIBMCXX_EXTERN char check_default_initialization_and_destruction_declarator(scope_entry_t* entry, const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN char check_default_initialization_of_type(
        type_t* t,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** constructor);

LIBMCXX_EXTERN char check_copy_constructor(scope_entry_t* entry,
        const decl_context_t* decl_context,
        char has_const,
        const locus_t* locus,
        scope_entry_t** constructor);

LIBMCXX_EXTERN char check_copy_assignment_operator(scope_entry_t* entry,
        const decl_context_t* decl_context,
        char has_const,
        const locus_t* locus,
        scope_entry_t** constructor);

LIBMCXX_EXTERN char check_move_assignment_operator(scope_entry_t* entry,
        const decl_context_t* decl_context,
        char has_const,
        const locus_t* locus,
        scope_entry_t** constructor);

LIBMCXX_EXTERN char check_assignment_expression_for_class_type(
        type_t* lhs_type,
        type_t* rhs_type,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** function);

LIBMCXX_EXTERN char check_construction_expression_for_class_type(
        type_t* lhs_type,
        type_t* seq_of_types,
        const decl_context_t *decl_context,
        const locus_t* locus,
        scope_entry_t** function);

LIBMCXX_EXTERN char check_copy_construction_expression_for_class_type(
        type_t* lhs_type,
        const decl_context_t* decl_context,
        const locus_t* locus,
        scope_entry_t** function);

LIBMCXX_EXTERN scope_entry_list_t* unfold_and_mix_candidate_functions(
        scope_entry_list_t* result_from_lookup,
        scope_entry_list_t* builtin_list,
        type_t** argument_types,
        int num_arguments,
        const decl_context_t* decl_context,
        const locus_t* locus,
        template_parameter_list_t *explicit_template_arguments
        );

LIBMCXX_EXTERN type_t* compute_type_for_type_id_tree(AST type_id,
        const decl_context_t* decl_context,
        // Out
        type_t** out_simple_type,
        gather_decl_spec_t *out_gather_info);

LIBMCXX_EXTERN scope_entry_t* get_std_initializer_list_template(
        const decl_context_t* decl_context,
        const locus_t* locus,
        char mandatory);

LIBMCXX_EXTERN void diagnostic_candidates(scope_entry_list_t* entry_list, const char**, const locus_t* locus);

LIBMCXX_EXTERN void ensure_function_is_emitted(scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN char check_nontype_template_argument_type(type_t* t);
LIBMCXX_EXTERN char check_nontype_template_argument_expression(AST expression, const decl_context_t* decl_context, nodecl_t*);
LIBMCXX_EXTERN char check_nodecl_nontype_template_argument_expression(nodecl_t nodecl, const decl_context_t* decl_context, nodecl_t*);
LIBMCXX_EXTERN char check_nodecl_template_argument_can_be_converted_to_parameter_type(
        nodecl_t nodecl_argument,
        type_t* parameter_type,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_out);

LIBMCXX_EXTERN void check_nodecl_expr_initializer(nodecl_t expr, 
        const decl_context_t* decl_context, 
        type_t* declared_type, 
        char disallow_narrowing,
        enum initialization_kind initialization_kind,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void check_nodecl_function_argument_initialization(
        nodecl_t nodecl_expr,
        const decl_context_t* decl_context, 
        type_t* declared_type, 
        char disallow_narrowing,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void check_nodecl_braced_initializer(nodecl_t braced_initializer, 
        const decl_context_t* decl_context, 
        type_t* declared_type, 
        char is_explicit_type_cast,
        char allow_excess_of_initializers,
        enum initialization_kind initialization_kind,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void check_template_id_expr(AST expr, const decl_context_t* decl_context, nodecl_t* nodecl_output);

// Like nodecl_make_function_call but takes care of virtual function calls
LIBMCXX_EXTERN nodecl_t cxx_nodecl_make_function_call(
        nodecl_t called,
        nodecl_t called_name,
        nodecl_t arg_list,
        nodecl_t function_form,
        type_t* t,
        const decl_context_t*,
        const locus_t* locus);
LIBMCXX_EXTERN nodecl_t cxx_nodecl_make_conversion(nodecl_t expr, type_t* dest_type,
        const decl_context_t* decl_context,
        const locus_t* locus);
LIBMCXX_EXTERN nodecl_t cxx_nodecl_make_conversion_to_logical(nodecl_t expr, type_t* dest_type,
        const decl_context_t* decl_context,
        const locus_t* locus);
LIBMCXX_EXTERN nodecl_t cxx_nodecl_wrap_in_parentheses(nodecl_t n);

LIBMCXX_EXTERN scope_entry_t* resolve_symbol_this(const decl_context_t* decl_context);
 
// Given a base NODECL_SYMBOL it integrates it in an accessor that can be a NODECL_SYMBOL or a NODECL_CLASS_MEMBER_ACCESS
LIBMCXX_EXTERN nodecl_t cxx_integrate_field_accesses(nodecl_t base, nodecl_t accessor);

// Not meant to be LIBMCXX_EXTERN (used by cxx-cuda.c and cxx-buildscope.c)
void check_function_arguments(AST arguments, const decl_context_t* decl_context, nodecl_t* nodecl_output);
void check_nodecl_function_call(nodecl_t nodecl_called, nodecl_t nodecl_argument_list,
        const decl_context_t* decl_context, nodecl_t* nodecl_output);

void get_packs_in_expression(nodecl_t nodecl,
        scope_entry_t*** packs_to_expand,
        int *num_packs_to_expand);

void call_destructor_for_data_layout_members(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus);

// Used in overload
char builtin_needs_contextual_conversion(scope_entry_t* candidate,
        int num_arg, type_t* parameter_type);

void check_contextual_conversion(nodecl_t expression,
        type_t* dest_type,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

// Instantiation of expressions
nodecl_t instantiate_expression(nodecl_t nodecl_expr, const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index);

nodecl_t instantiate_expression_non_executable(nodecl_t nodecl_expr, const decl_context_t* decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index);

nodecl_t update_cxx_dep_qualified_name(nodecl_t cxx_dep_name,
        const decl_context_t* new_decl_context,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index);

scope_entry_t* expand_template_function_given_template_arguments(
        scope_entry_t* entry,
        const decl_context_t* decl_context,
        const locus_t* locus,
        template_parameter_list_t* explicit_template_arguments);

LIBMCXX_EXTERN char same_functional_expression(
        nodecl_t n1,
        nodecl_t n2);

LIBMCXX_EXTERN type_t* clear_special_expr_type_variants(type_t* t);

// Used by the lexer
char* interpret_schar(const char* schar, const locus_t* locus);

MCXX_END_DECLS

#endif
