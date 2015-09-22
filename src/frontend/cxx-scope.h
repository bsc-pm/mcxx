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




#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-ast-decls.h"
#include "cxx-type-decls.h"
#include "cxx-scope-decls.h"
#include "cxx-buildscope-decls.h"
#include "cxx-instantiation-decls.h"
#include "cxx-nodecl-output.h"

MCXX_BEGIN_DECLS

#include "cxx-entity-specs-ops.h"

LIBMCXX_EXTERN decl_context_t* new_global_context(void);
LIBMCXX_EXTERN decl_context_t* new_namespace_context(const decl_context_t* enclosing_decl_context, scope_entry_t* namespace_symbol);
LIBMCXX_EXTERN decl_context_t* new_prototype_context(const decl_context_t* enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t* new_block_context(const decl_context_t* enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t* new_function_context(const decl_context_t* enclosing_decl_context);
LIBMCXX_EXTERN decl_context_t* new_class_context(const decl_context_t* enclosing_decl_context, 
        scope_entry_t* class_entry);

// Used only in TL
LIBMCXX_EXTERN decl_context_t* decl_context_empty();
LIBMCXX_EXTERN decl_context_t* decl_context_clone(const decl_context_t* t);

// Functions to handle scopes
LIBMCXX_EXTERN scope_entry_t* new_symbol(const decl_context_t* decl_context, 
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
LIBMCXX_EXTERN scope_entry_list_t* query_name_str_flags(const decl_context_t* decl_context,
        const char* unqualified_name, field_path_t* field_path, decl_flags_t decl_flags);
#define query_name_str(_decl_context, _unqualified_name, _field_path) \
    query_name_str_flags(_decl_context, _unqualified_name, _field_path, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_nodecl_name_flags(const decl_context_t* decl_context,
        nodecl_t nodecl_name, field_path_t *field_path, decl_flags_t decl_flags);
#define query_nodecl_name(_decl_context, _nodecl_simple_name, _field_path) \
    query_nodecl_name_flags(_decl_context, _nodecl_simple_name, _field_path, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_nodecl_name_in_class_flags(
        const decl_context_t* decl_context,
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        field_path_t *field_path,
        decl_flags_t decl_flags);
#define query_nodecl_name_in_class(_decl_context, _class_symbol, _nodecl_simple_name, _field_path) \
    query_nodecl_name_in_class_flags(_decl_context, _class_symbol, _nodecl_simple_name, _field_path, DF_NONE)

// There is no query_unqualified_name as it is the same as query_nested_name with global_op == NULL
// and nested_name == NULL
LIBMCXX_EXTERN scope_entry_list_t* query_nested_name_flags(const decl_context_t* decl_context, 
        AST global_op, 
        AST nested_name, 
        AST unqualified_name,
        field_path_t* field_path,
        decl_flags_t decl_flags);
#define query_nested_name(_decl_context, _global_op, _nested_name, _unqualified_name, _field_path) \
    query_nested_name_flags(_decl_context, _global_op, _nested_name, _unqualified_name, _field_path, DF_NONE)

// Only in the current scope
LIBMCXX_EXTERN scope_entry_list_t* query_in_scope_str_flags(const decl_context_t* decl_context,
        const char *name, field_path_t* field_path, decl_flags_t decl_flags);
#define query_in_scope_str(_decl_context, _name, _field_path) \
    query_in_scope_str_flags(_decl_context, _name, _field_path, DF_NONE)

LIBMCXX_EXTERN scope_entry_list_t* query_in_scope_flags(const decl_context_t* decl_context,
        AST unqualified_name, field_path_t* field_path, decl_flags_t decl_flags);
#define query_in_scope(_decl_context, _unqualified_name, _field_path) \
    query_in_scope_flags(_decl_context, _unqualified_name, _field_path, DF_NONE)

// Convenience function
LIBMCXX_EXTERN scope_entry_list_t* query_id_expression_flags(const decl_context_t* decl_context,
        AST id_expression,
        field_path_t* field_path,
        decl_flags_t decl_flags);
#define query_id_expression(_decl_context, _id_expression, _field_path) \
    query_id_expression_flags(_decl_context, _id_expression, _field_path, DF_NONE)

// Get the fully qualified symbol name in the scope of the ocurrence
LIBMCXX_EXTERN const char* get_fully_qualified_symbol_name(struct
        scope_entry_tag* entry, const decl_context_t* decl_context, char*
        is_dependent, int* max_qualif_level);

LIBMCXX_EXTERN const char* get_fully_qualified_symbol_name_ex(scope_entry_t* entry,
        const decl_context_t* decl_context,
        char* is_dependent, int* max_qualif_level,
        char no_templates,
        char only_classes,
        char do_not_emit_template_keywords,
        print_type_callback_t print_type_fun,
        void *print_type_data
        );

LIBMCXX_EXTERN const char* get_fully_qualified_symbol_name_without_template(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int*
        max_qualif_level);

LIBMCXX_EXTERN const char* get_class_qualification_of_symbol(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level);

LIBMCXX_EXTERN const char* get_class_qualification_of_symbol_without_template(scope_entry_t* entry,
        const decl_context_t* decl_context, char* is_dependent, int* max_qualif_level);

// A simpler version of get_fully_qualified_symbol_name
LIBMCXX_EXTERN const char* get_qualified_symbol_name(scope_entry_t* entry, const decl_context_t* decl_context);

// Specific query for C++ conversion functions like 'operator T'
LIBMCXX_EXTERN scope_entry_list_t* query_conversion_function_info(const decl_context_t* decl_context,
        type_t* t,
        const locus_t* locus);

// Template things, should be moved to typeutils
LIBMCXX_EXTERN type_t* update_type(type_t* orig_type,
        const decl_context_t* template_parameters_context,
        const locus_t* locus);

LIBMCXX_EXTERN type_t* update_type_with_pack_index(type_t* orig_type,
        const decl_context_t* template_parameters_context,
        const locus_t* locus,
        int pack_index);

LIBMCXX_EXTERN type_t* update_type_for_auto(type_t* orig_type,
        type_t* template_parameter);

LIBMCXX_EXTERN type_t* update_type_for_instantiation(type_t* orig_type,
        const decl_context_t* context_of_being_instantiated,
        const locus_t* locus,
        instantiation_symbol_map_t* instantiation_symbol_map,
        int pack_index);

LIBMCXX_EXTERN template_parameter_list_t* update_template_argument_list(
        const decl_context_t* decl_context,
        template_parameter_list_t* dependent_type_template_arguments,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus,
        int pack_index);

// Template parameter names
LIBMCXX_EXTERN scope_entry_t* lookup_of_template_parameter(const decl_context_t* context, 
        int template_parameter_nesting, int template_parameter_position);

LIBMCXX_EXTERN char is_id_expression(AST a);
LIBMCXX_EXTERN char is_qualified_id_expression(AST a);
LIBMCXX_EXTERN char is_unqualified_id_expression(AST a);

LIBMCXX_EXTERN char is_inline_namespace_of(const decl_context_t* inner_namespace_ctx, 
        const decl_context_t* outer_namespace_ctx);

LIBMCXX_EXTERN int get_template_nesting_of_context(const decl_context_t*);
LIBMCXX_EXTERN int get_template_nesting_of_template_parameters(template_parameter_list_t*);

LIBMCXX_EXTERN template_parameter_list_t* get_template_arguments_from_syntax(
        AST template_parameters_list_tree,
        const decl_context_t* template_parameters_context);

LIBMCXX_EXTERN template_parameter_list_t* duplicate_template_argument_list(template_parameter_list_t* template_parameters);
LIBMCXX_EXTERN void free_template_parameter_list(template_parameter_list_t* template_parameters);

LIBMCXX_EXTERN const char* get_template_arguments_str(scope_entry_t* entry, 
        const decl_context_t* decl_context);

LIBMCXX_EXTERN const char* template_arguments_to_str(template_parameter_list_t* template_parameters,
        int first_argument_to_be_printed,
        char print_first_level_bracket,
        const decl_context_t* decl_context);

LIBMCXX_EXTERN template_parameter_value_t* update_template_parameter_value(
        template_parameter_value_t* v,
        const decl_context_t* decl_context,
        instantiation_symbol_map_t *instantiation_symbol_map,
        const locus_t* locus,
        int pack_index);

// Friend support
LIBMCXX_EXTERN char is_friend_declared(scope_entry_t* entry);

// Iteration in scopes
LIBMCXX_EXTERN void scope_for_each_entity(scope_t* sc, void *data, void (fun)(scope_entry_list_t*, void*));

// Internal use only
LIBMCXX_EXTERN scope_t* _new_scope(void);

// Return a descriptive name of the symbol kind, used during diagnostics
const char* symbol_kind_descriptive_name(enum cxx_symbol_kind symbol_kind);

// Debug functions
LIBMCXX_EXTERN const char* symbol_kind_name(scope_entry_t* entry);

// Utility
LIBMCXX_EXTERN const char* unmangle_symbol_name(scope_entry_t* entry);

// Used during serialization of modules
LIBMCXX_EXTERN const char* symbol_kind_to_str(enum cxx_symbol_kind symbol_kind);
LIBMCXX_EXTERN enum cxx_symbol_kind symbol_str_to_kind(const char* str);

// Debug
LIBMCXX_EXTERN void print_template_parameter_list(template_parameter_list_t* template_parameters);

// Dependent names
LIBMCXX_EXTERN void compute_nodecl_name_from_nested_name(AST nested_part, 
        AST unqualified_part, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void compute_nodecl_name_from_nested_part(AST nested_part,
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

LIBMCXX_EXTERN void compute_nodecl_name_from_id_expression(AST id_expression, const decl_context_t* decl_context,
        nodecl_t* nodecl_output);
LIBMCXX_EXTERN void compute_nodecl_name_from_qualified_name(AST global_op, 
        AST nested_name_spec, AST unqualified_id, 
        const decl_context_t* decl_context,
        nodecl_t* nodecl_output);

// Used by Fortran
LIBMCXX_EXTERN scope_entry_list_t* class_context_lookup(const decl_context_t* decl_context, 
        field_path_t* field_path,
        decl_flags_t decl_flags,
        const char* name,
        const locus_t* locus);

// C++ names
LIBMCXX_EXTERN nodecl_t nodecl_name_get_last_part(nodecl_t nodecl_name);
LIBMCXX_EXTERN char nodecl_name_ends_in_template_id(nodecl_t nodecl_name);
LIBMCXX_EXTERN template_parameter_list_t* nodecl_name_get_last_template_arguments(nodecl_t nodecl_name);

LIBMCXX_EXTERN type_t* build_dependent_typename_for_entry(
        scope_entry_t* class_symbol,
        nodecl_t nodecl_name,
        const locus_t* locus);

LIBMCXX_EXTERN scope_entry_list_t* query_dependent_entity_in_context(
        const decl_context_t* decl_context,
        scope_entry_t* dependent_entity,
        int pack_index,
        field_path_t* field_path,
        instantiation_symbol_map_t* instantiation_symbol_map,
        const locus_t* locus);

LIBMCXX_EXTERN char compute_type_of_dependent_conversion_type_id(
        nodecl_t nodecl_name,
        const decl_context_t* decl_context);

// Utils
LIBMCXX_EXTERN char scope_is_enclosed_by(const scope_t *const scope, const scope_t *const potential_enclosing);
LIBMCXX_EXTERN char class_is_in_lexical_scope(const decl_context_t* decl_context, 
        scope_entry_t* class_symbol);
LIBMCXX_EXTERN char symbol_is_member_of_dependent_class(scope_entry_t* entry);
LIBMCXX_EXTERN char symbol_is_local_of_dependent_function(scope_entry_t* entry);
LIBMCXX_EXTERN scope_entry_t* get_function_or_class_where_symbol_depends(scope_entry_t* entry);

LIBMCXX_EXTERN const char* symbol_to_source(scope_entry_t* entry);
LIBMCXX_EXTERN char is_dependent_function(scope_entry_t* entry);

LIBMCXX_EXTERN void field_path_init(field_path_t* field_path);

LIBMCXX_EXTERN void symbol_clear_indirect_types(scope_entry_t* entry);

// Symbol helping routines
LIBMCXX_EXTERN void symbol_set_as_parameter_of_function(scope_entry_t* entry, scope_entry_t* function,
        int nesting, int position);
LIBMCXX_EXTERN char symbol_is_parameter_of_function(scope_entry_t* entry, scope_entry_t* function);
LIBMCXX_EXTERN int symbol_get_parameter_position_in_function(scope_entry_t* entry, scope_entry_t* function);
LIBMCXX_EXTERN int symbol_get_parameter_nesting_in_function(scope_entry_t* entry, scope_entry_t* function);

LIBMCXX_EXTERN char template_parameter_kind_is_pack(enum template_parameter_kind k);
LIBMCXX_EXTERN enum template_parameter_kind template_parameter_kind_get_base_kind(enum template_parameter_kind kind);
LIBMCXX_EXTERN char template_argument_is_pack(template_parameter_value_t* value);

LIBMCXX_EXTERN int get_length_of_pack_expansion_from_expression(nodecl_t expr,
        const decl_context_t* decl_context,
        const locus_t* locus);
LIBMCXX_EXTERN int get_length_of_pack_expansion_from_type(type_t* pack_type,
        const decl_context_t* decl_context,
        const locus_t* locus);

LIBMCXX_EXTERN nodecl_t symbol_get_aligned_attribute(scope_entry_t* entry);
LIBMCXX_EXTERN char symbol_has_gcc_attribute(scope_entry_t* entry, const char* name, gcc_attribute_t* gcc_attr);
LIBMCXX_EXTERN void symbol_update_gcc_attribute(scope_entry_t* entry, const char* name, gcc_attribute_t gcc_attr);

LIBMCXX_EXTERN scope_entry_t* class_symbol_get_canonical_symbol(scope_entry_t* class_symbol);

MCXX_END_DECLS

#endif // CXX_SCOPE_H
