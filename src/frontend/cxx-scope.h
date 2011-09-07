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



#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-type-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

LIBMCXX_EXTERN decl_context_t new_global_context(void);
LIBMCXX_EXTERN decl_context_t new_namespace_context(decl_context_t enclosing_decl_context, scope_entry_t* namespace_symbol);
LIBMCXX_EXTERN decl_context_t new_prototype_context(decl_context_t enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t new_block_context(decl_context_t enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t new_function_context(decl_context_t enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t new_class_context(decl_context_t enclosing_decl_context, 
        scope_entry_t* class_entry);

// Used only in TL
LIBMCXX_EXTERN decl_context_t decl_context_empty();

// Functions to handle scopes
LIBMCXX_EXTERN scope_entry_t* new_symbol(decl_context_t decl_context, 
        struct scope_tag* st, const char* name);
LIBMCXX_EXTERN void remove_entry(struct scope_tag* st, scope_entry_t* entry);
LIBMCXX_EXTERN void insert_entry(struct scope_tag* st, scope_entry_t* entry);
LIBMCXX_EXTERN void insert_alias(struct scope_tag* st, scope_entry_t* entry, const char* alias_name);

// Given a list of symbols, purge all those that are not of symbol_kind kind
LIBMCXX_EXTERN scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
LIBMCXX_EXTERN scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
LIBMCXX_EXTERN scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
LIBMCXX_EXTERN scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

LIBMCXX_EXTERN scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*, void*), void* p);

// Query functions
LIBMCXX_EXTERN scope_entry_list_t* query_name_str_flags(decl_context_t decl_context,
        const char* unqualified_name, decl_flags_t decl_flags);
#define query_name_str(_decl_context, _unqualified_name) \
    query_name_str_flags(_decl_context, _unqualified_name, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_nodecl_name_flags(decl_context_t decl_context,
        nodecl_t nodecl_name, decl_flags_t decl_flags);
#define query_nodecl_name(_decl_context, _nodecl_simple_name) \
    query_nodecl_name_flags(_decl_context, _nodecl_simple_name, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_nodecl_name_in_class_flags(scope_entry_t* class_symbol,
        nodecl_t nodecl_name, decl_flags_t decl_flags);

#define query_nodecl_name_in_class(_decl_context, _nodecl_simple_name) \
    query_nodecl_name_in_class_flags(_decl_context, _nodecl_simple_name, DF_NONE)

// There is no query_unqualified_name as it is the same as query_nested_name with global_op == NULL
// and nested_name == NULL
LIBMCXX_EXTERN scope_entry_list_t* query_nested_name_flags(decl_context_t decl_context, 
        struct AST_tag* global_op, 
        struct AST_tag* nested_name, 
        struct AST_tag* unqualified_name,
        decl_flags_t decl_flags);
#define query_nested_name(_decl_context, _global_op, _nested_name, _unqualified_name) \
    query_nested_name_flags(_decl_context, _global_op, _nested_name, _unqualified_name, DF_NONE)

// Only in the current scope
LIBMCXX_EXTERN scope_entry_list_t* query_in_scope_str_flags(decl_context_t decl_context,
        const char *name, decl_flags_t decl_flags);
#define query_in_scope_str(_decl_context, _name) \
    query_in_scope_str_flags(_decl_context, _name, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_in_scope_flags(decl_context_t decl_context,
        struct AST_tag* unqualified_name, decl_flags_t decl_flags);
#define query_in_scope(_decl_context, _unqualified_name) \
    query_in_scope_flags(_decl_context, _unqualified_name, DF_NONE)

// Convenience function
LIBMCXX_EXTERN scope_entry_list_t* query_id_expression_flags(decl_context_t decl_context, struct AST_tag* id_expression, decl_flags_t decl_flags);
#define query_id_expression(_decl_context, _id_expression) \
    query_id_expression_flags(_decl_context, _id_expression, DF_NONE)

// Get the fully qualified symbol name in the scope of the ocurrence
LIBMCXX_EXTERN const char* get_fully_qualified_symbol_name(struct
        scope_entry_tag* entry, decl_context_t decl_context, char*
        is_dependent, int* max_qualif_level);

LIBMCXX_EXTERN const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int*
        max_qualif_level);

LIBMCXX_EXTERN const char* get_class_qualification_of_symbol(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level);

LIBMCXX_EXTERN const char* get_class_qualification_of_symbol_without_template(scope_entry_t* entry,
        decl_context_t decl_context, char* is_dependent, int* max_qualif_level);

// A simpler version of get_fully_qualified_symbol_name
LIBMCXX_EXTERN const char* get_qualified_symbol_name(scope_entry_t* entry, decl_context_t decl_context);

// Template things, should be moved to typeutils
LIBMCXX_EXTERN type_t* update_type(type_t* orig_type, 
        decl_context_t template_parameters_context,
        const char* filename, int line);

LIBMCXX_EXTERN type_t* update_type_for_instantiation(type_t* orig_type,
        decl_context_t context_of_being_instantiated,
        const char* filename, int line);

// Other stuff
LIBMCXX_EXTERN scope_entry_list_t* cascade_lookup(decl_context_t decl_context, 
        const char* name, 
        decl_flags_t decl_flags,
        const char* filename, int line);

LIBMCXX_EXTERN unsigned long long scope_used_memory(void);
LIBMCXX_EXTERN unsigned long long symbols_used_memory(void);

// Template parameter names
LIBMCXX_EXTERN scope_entry_t* lookup_of_template_parameter(decl_context_t context, 
        int template_parameter_nesting, int template_parameter_position);

LIBMCXX_EXTERN char is_unqualified_id_expression(AST a);

LIBMCXX_EXTERN char is_inline_namespace_of(decl_context_t inner_namespace_ctx, 
        decl_context_t outer_namespace_ctx);

LIBMCXX_EXTERN int get_template_nesting_of_context(decl_context_t);
LIBMCXX_EXTERN int get_template_nesting_of_template_parameters(template_parameter_list_t*);

LIBMCXX_EXTERN template_parameter_list_t* get_template_parameters_from_syntax(
        AST template_parameters_list_tree,
        decl_context_t template_parameters_context);

LIBMCXX_EXTERN template_parameter_list_t* duplicate_template_argument_list(template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN const char* get_template_arguments_str(scope_entry_t* entry, 
        decl_context_t decl_context);

LIBMCXX_EXTERN const char* template_arguments_to_str(template_parameter_list_t* template_parameters,
        decl_context_t decl_context);

LIBMCXX_EXTERN template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        decl_context_t decl_context,
        const char* filename, int line);

// Friend support
LIBMCXX_EXTERN char is_friend_declared(scope_entry_t* entry);
LIBMCXX_EXTERN scope_entry_list_t* filter_friend_declared(scope_entry_list_t* entry_list);

// Iteration in scopes
LIBMCXX_EXTERN void scope_for_each_entity(scope_t* sc, void *data, void (fun)(scope_entry_list_t*, void*));

// Internal use only
LIBMCXX_EXTERN scope_t* _new_scope(void);

// Fake symbol representing a scope
LIBMCXX_EXTERN scope_entry_t* new_scope_symbol(decl_context_t decl_context);

// Debug functions
LIBMCXX_EXTERN const char* symbol_kind_name(scope_entry_t* entry);

// Utility
LIBMCXX_EXTERN const char* unmangle_symbol_name(scope_entry_t* entry);

// Debug
LIBMCXX_EXTERN void print_template_parameter_list(template_parameter_list_t* template_parameters);

// Dependent names
LIBMCXX_EXTERN void compute_nodecl_name_from_nested_name(AST nested_part, 
        AST unqualified_part, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void compute_nodecl_name_from_id_expression(AST id_expression, decl_context_t decl_context,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void compute_nodecl_name_from_qualified_name(AST global_op, 
        AST nested_name_spec, AST unqualified_id, 
        decl_context_t decl_context,
        nodecl_t* nodecl_output);

// Used by Fortran
LIBMCXX_EXTERN scope_entry_list_t* class_context_lookup(decl_context_t decl_context, 
        decl_flags_t decl_flags, const char* name);

// C++ names
LIBMCXX_EXTERN nodecl_t nodecl_name_get_last_part(nodecl_t nodecl_name);
LIBMCXX_EXTERN char nodecl_name_ends_in_template_id(nodecl_t nodecl_name);
LIBMCXX_EXTERN template_parameter_list_t* nodecl_name_name_last_template_arguments(nodecl_t nodecl_name);

LIBMCXX_EXTERN template_parameter_list_t* update_template_argument_list_in_dependent_typename(
        decl_context_t decl_context,
        template_parameter_list_t* dependent_type_template_arguments,
        const char* filename, 
        int line);

MCXX_END_DECLS

#endif // CXX_SCOPE_H
