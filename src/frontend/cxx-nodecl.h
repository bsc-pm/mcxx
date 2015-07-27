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

#ifndef CXX_NODECL_H
#define CXX_NODECL_H

#include "cxx-macros.h"
#include "cxx-cexpr-fwd.h"
#include "cxx-scope-decls.h"
#include "cxx-nodecl-decls.h"
#include "cxx-type-decls.h"
#include "cxx-locus.h"
#include "cxx-nodecl-deep-copy-fwd.h"

#include <stddef.h>

MCXX_BEGIN_DECLS

#define NODECL_STATIC_NULL { NULL }

static inline nodecl_t nodecl_null(void);
static inline char nodecl_is_null(nodecl_t t);

// Returns the internal AST
static inline AST nodecl_get_ast(nodecl_t t);

// Replicates the tree but does not change symbols or other indirectly mentioned symbols
nodecl_t nodecl_shallow_copy(nodecl_t t);

// Duplicates the node like nodecl_shallow_copy but does not copy children. Creates a leaf node
static inline nodecl_t nodecl_duplicate(nodecl_t t);

// Replicates the tree and duplicates bound variables. Free variables may be replaced using the given map
nodecl_t nodecl_deep_copy(nodecl_t, const decl_context_t* new_decl_context, symbol_map_t* symbol_map);

// Parent
static inline nodecl_t nodecl_get_parent(nodecl_t t);
static inline void nodecl_set_parent(nodecl_t node, nodecl_t parent);

// Children
static inline nodecl_t nodecl_get_child(nodecl_t n, int i);

// Constant values
static inline char nodecl_is_constant(nodecl_t t);
static inline void nodecl_set_constant(nodecl_t t, const_value_t* cval);
static inline const_value_t* nodecl_get_constant(nodecl_t t);

// Text
static inline void nodecl_set_text(nodecl_t t, const char*);
static inline const char* nodecl_get_text(nodecl_t);

// Type
static inline type_t* nodecl_get_type(nodecl_t);
static inline void nodecl_set_type(nodecl_t, type_t*);

// Symbol
static inline scope_entry_t* nodecl_get_symbol(nodecl_t);
static inline void nodecl_set_symbol(nodecl_t, scope_entry_t*);

// Template parameters (C++ only)
static inline void nodecl_set_template_parameters(nodecl_t n, template_parameter_list_t* template_parameters);
static inline template_parameter_list_t* nodecl_get_template_parameters(nodecl_t n);

// Location
static inline const locus_t* nodecl_get_locus(nodecl_t);

static inline const char* nodecl_get_filename(nodecl_t);
static inline unsigned int nodecl_get_line(nodecl_t);
static inline unsigned int nodecl_get_column(nodecl_t);
static inline const char* nodecl_locus_to_str(nodecl_t);

static inline void nodecl_set_locus(nodecl_t n, const locus_t* locus);

static inline void nodecl_set_locus_as(nodecl_t n, nodecl_t loc);

// Kind of node
static inline node_t nodecl_get_kind(nodecl_t);

// 'list' parameter can be a 'nodecl_null()'
static inline nodecl_t nodecl_append_to_list(nodecl_t list, nodecl_t element);

// Returns the head of the list
static inline nodecl_t nodecl_list_head(nodecl_t list);

// Either list1 or list2 can be 'nodecl_null()'
static inline nodecl_t nodecl_concat_lists(nodecl_t list1, nodecl_t list2);

// States that this nonnull nodecl is a list
static inline char nodecl_is_list(nodecl_t);

// States that this nodecl is a list
static inline char nodecl_is_list_or_null(nodecl_t);

// Unpack a list. Do not forget to free the returned pointer
static inline nodecl_t* nodecl_unpack_list(nodecl_t n, int *num_items);

// Length of a list
static inline int nodecl_list_length(nodecl_t list);

// Wrap (use sparingly)
static inline nodecl_t _nodecl_wrap(AST);

// Generic routines (meant for generic processing)
static inline nodecl_t nodecl_generic_make(node_t, const locus_t* location);
static inline void nodecl_set_child(nodecl_t, int, nodecl_t);

// Free a temporally allocated nodecl
static inline void nodecl_free(nodecl_t);

static inline char nodecl_expr_is_value_dependent(nodecl_t);
static inline void nodecl_expr_set_is_value_dependent(nodecl_t, char);

static inline char nodecl_expr_is_type_dependent(nodecl_t);
static inline void nodecl_expr_set_is_type_dependent(nodecl_t, char);

static inline char nodecl_is_err_expr(nodecl_t);
static inline char nodecl_is_err_stmt(nodecl_t);

// Retrieve the context from a node (or the currently compiled file global
// context if not context is found)
static inline const decl_context_t* nodecl_retrieve_context(nodecl_t);

// These only work on NODECL_CONTEXT or NODECL_PRAGMA_CONTEXT
static inline const decl_context_t* nodecl_get_decl_context(nodecl_t n);
static inline void nodecl_set_decl_context(nodecl_t, const decl_context_t*);

// Exchange
DEPRECATED void nodecl_exchange(nodecl_t old_nod, nodecl_t new_node);

// Replace
void nodecl_replace(nodecl_t old_node, nodecl_t new_node);

// Hash table
size_t nodecl_hash_table(nodecl_t key);

// Sourceify
const char* nodecl_stmt_to_source(nodecl_t n);
const char* nodecl_expr_to_source(nodecl_t n);

// Build from AST_NODECL_LITERAL
nodecl_t nodecl_make_from_ast_nodecl_literal(AST);

// Placeholders
AST* nodecl_get_placeholder(nodecl_t n);
void nodecl_set_placeholder(nodecl_t n, AST* p);

MCXX_END_DECLS

#include "cxx-nodecl-inline.h"

#endif // CXX_NODECL_H
