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
#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "cxx-ast.h"
#include "hash.h"
#include "cxx-macros.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

decl_context_t new_global_context(void);
decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, char* qualification_name);
decl_context_t new_prototype_context(decl_context_t enclosing_decl_context);
decl_context_t new_block_context(decl_context_t enclosing_decl_context);
decl_context_t new_function_context(decl_context_t enclosing_decl_context);
decl_context_t new_class_context(decl_context_t enclosing_decl_context, char* qualification_name);
decl_context_t new_template_context(decl_context_t enclosing_decl_context);

// Functions to handle scopes
scope_entry_t* new_symbol(decl_context_t decl_context, scope_t* st, char* name);
void remove_entry(scope_t* st, scope_entry_t* entry);
void insert_entry(scope_t* st, scope_entry_t* entry);

// Given a list of symbols, purge all those that are not of symbol_kind kind
scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*));

// Query functions
scope_entry_list_t* query_unqualified_name_str_flags(decl_context_t decl_context,
        char* unqualified_name, decl_flags_t decl_flags);
#define query_unqualified_name_str(_decl_context, _unqualified_name) \
    query_unqualified_name_str_flags(_decl_context, _unqualified_name, DF_NONE)

// There is no query_unqualified_name as it is the same as query_nested_name with global_op == NULL
// and nested_name == NULL
scope_entry_list_t* query_nested_name_flags(decl_context_t decl_context, 
        AST global_op, 
        AST nested_name, 
        AST unqualified_name,
        decl_flags_t decl_flags);
#define query_nested_name(_decl_context, _global_op, _nested_name, _unqualified_name) \
    query_nested_name_flags(_decl_context, _global_op, _nested_name, _unqualified_name, DF_NONE)

// Only in the current scope
scope_entry_list_t* query_in_scope_str_flags(decl_context_t decl_context,
        char *name, decl_flags_t decl_flags);
#define query_in_scope_str(_decl_context, _name) \
    query_in_scope_str_flags(_decl_context, _name, DF_NONE)

scope_entry_list_t* query_in_scope_flags(decl_context_t decl_context,
        AST unqualified_name, decl_flags_t decl_flags);
#define query_in_scope(_decl_context, _unqualified_name) \
    query_in_scope_flags(_decl_context, _unqualified_name, DF_NONE)

// Convenience function
scope_entry_list_t* query_id_expression_flags(decl_context_t decl_context, AST id_expression, decl_flags_t decl_flags);
#define query_id_expression(_decl_context, _id_expression) \
    query_id_expression_flags(_decl_context, _id_expression, DF_NONE)

// Manipulators
scope_entry_list_t* create_list_from_entry(scope_entry_t* entry);

// Get the fully qualified symbol name in the scope of the ocurrence
char* get_fully_qualified_symbol_name(scope_entry_t* entry, decl_context_t decl_context, char* is_dependent, int* max_qualif_level);
char* get_unqualified_template_symbol_name(scope_entry_t* entry, decl_context_t decl_context);
char same_scope(scope_t* stA, scope_t* stB);

MCXX_END_DECLS

#endif // CXX_SCOPE_H
