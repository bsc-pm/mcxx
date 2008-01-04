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
#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "hash.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-type-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"

MCXX_BEGIN_DECLS

decl_context_t new_global_context(void);
decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, const char* qualification_name);
decl_context_t new_prototype_context(decl_context_t enclosing_decl_context);
decl_context_t new_block_context(decl_context_t enclosing_decl_context);
decl_context_t new_function_context(decl_context_t enclosing_decl_context);
decl_context_t new_class_context(decl_context_t enclosing_decl_context, 
        const char* qualification_name, struct type_tag* class_type);
decl_context_t new_template_context(decl_context_t enclosing_decl_context);

// Functions to handle scopes
struct scope_entry_tag* new_symbol(decl_context_t decl_context, 
        struct scope_tag* st, const char* name);
void remove_entry(struct scope_tag* st, struct scope_entry_tag* entry);
void insert_entry(struct scope_tag* st, struct scope_entry_tag* entry);

// Given a list of symbols, purge all those that are not of symbol_kind kind
struct scope_entry_list_tag* filter_symbol_kind(struct scope_entry_list_tag* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
struct scope_entry_list_tag* filter_symbol_kind_set(struct scope_entry_list_tag* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
struct scope_entry_list_tag* filter_symbol_non_kind(struct scope_entry_list_tag* entry_list, enum cxx_symbol_kind symbol_kind);
struct scope_entry_list_tag* filter_symbol_non_kind_set(struct scope_entry_list_tag* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

struct scope_entry_list_tag* filter_symbol_using_predicate(struct scope_entry_list_tag* entry_list, char (*f)(struct scope_entry_tag*));

// Query functions
struct scope_entry_list_tag* query_unqualified_name_str_flags(decl_context_t decl_context,
        const char* unqualified_name, decl_flags_t decl_flags);
#define query_unqualified_name_str(_decl_context, _unqualified_name) \
    query_unqualified_name_str_flags(_decl_context, _unqualified_name, DF_NONE)

// There is no query_unqualified_name as it is the same as query_nested_name with global_op == NULL
// and nested_name == NULL
struct scope_entry_list_tag* query_nested_name_flags(decl_context_t decl_context, 
        struct AST_tag* global_op, 
        struct AST_tag* nested_name, 
        struct AST_tag* unqualified_name,
        decl_flags_t decl_flags);
#define query_nested_name(_decl_context, _global_op, _nested_name, _unqualified_name) \
    query_nested_name_flags(_decl_context, _global_op, _nested_name, _unqualified_name, DF_NONE)

// Only in the current scope
struct scope_entry_list_tag* query_in_scope_str_flags(decl_context_t decl_context,
        const char *name, decl_flags_t decl_flags);
#define query_in_scope_str(_decl_context, _name) \
    query_in_scope_str_flags(_decl_context, _name, DF_NONE)

struct scope_entry_list_tag* query_in_scope_flags(decl_context_t decl_context,
        struct AST_tag* unqualified_name, decl_flags_t decl_flags);
#define query_in_scope(_decl_context, _unqualified_name) \
    query_in_scope_flags(_decl_context, _unqualified_name, DF_NONE)

// Convenience function
struct scope_entry_list_tag* query_id_expression_flags(decl_context_t decl_context, struct AST_tag* id_expression, decl_flags_t decl_flags);
#define query_id_expression(_decl_context, _id_expression) \
    query_id_expression_flags(_decl_context, _id_expression, DF_NONE)

// Manipulators
struct scope_entry_list_tag* create_list_from_entry(struct scope_entry_tag* entry);

// Get the fully qualified symbol name in the scope of the ocurrence
const char* get_fully_qualified_symbol_name(struct scope_entry_tag* entry, decl_context_t decl_context, char* is_dependent, int* max_qualif_level);
const char* get_unqualified_template_symbol_name(struct scope_entry_tag* entry, decl_context_t decl_context);

// Class scopes
struct scope_entry_list_tag* class_context_lookup(decl_context_t decl_context, const char* name);

template_argument_list_t *get_template_arguments_of_template_id(
        struct AST_tag* template_id,
        struct type_tag* template_type,
        decl_context_t template_arguments_context,
        char *valid);
template_argument_list_t* get_template_arguments_from_syntax(
        struct AST_tag* template_arguments_list,
        decl_context_t template_arguments_context,
        int nesting_level);

// Template things, should be moved to typeutils
template_argument_t* update_template_argument(template_argument_list_t* given_template_args,
        template_argument_t* current_template_arg,
        decl_context_t template_arguments_context,
        const char *filename, int line);
struct type_tag* update_type(template_argument_list_t* given_template_args,
        struct type_tag* orig_type, 
        decl_context_t template_arguments_context,
        const char* filename, int line);

MCXX_END_DECLS

#endif // CXX_SCOPE_H
