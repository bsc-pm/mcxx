#ifndef CXX_SCOPE_H
#define CXX_SCOPE_H

#include "cxx-ast.h"
#include "hash.h"
#include "cxx-macros.h"
#include "cxx-scope-decls.h"

MCXX_BEGIN_DECLS

scope_t* new_namespace_scope(scope_t* enclosing_scope, char* qualification_name);
scope_t* new_prototype_scope(scope_t* enclosing_scope);
scope_t* new_block_scope(scope_t* enclosing_scope, scope_t* prototype_scope, scope_t* function_scope);
scope_t* new_function_scope(scope_t* enclosing_scope, scope_t* prototype_scope);
scope_t* new_class_scope(scope_t* enclosing_scope, char* qualification_name);
scope_t* new_template_scope(scope_t* enclosing_scope);

// Functions to handle scope
scope_entry_t* new_symbol(scope_t* st, char* name);
void remove_entry(scope_t* st, scope_entry_t* entry);
void insert_entry(scope_t* st, scope_entry_t* entry);


// Higher level functions when dealing with the scope
scope_entry_t* filter_simple_type_specifier(scope_entry_list_t* entry_list);

// Given a list of symbols, purge all those that are not of symbol_kind kind
scope_entry_list_t* filter_symbol_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
// Similar but can be used to filter based on a kind set
scope_entry_list_t* filter_symbol_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

// Opposite filtering
scope_entry_list_t* filter_symbol_non_kind(scope_entry_list_t* entry_list, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* filter_symbol_non_kind_set(scope_entry_list_t* entry_list, int num_kinds, enum cxx_symbol_kind* symbol_kind_set);

scope_entry_list_t* filter_entry_from_list(scope_entry_list_t* entry_list, scope_entry_t* entry);

scope_entry_list_t* filter_symbol_using_predicate(scope_entry_list_t* entry_list, char (*f)(scope_entry_t*));

// Everything built by an id_expression can be queried with this function
scope_entry_list_t* query_id_expression(scope_t* st, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, struct decl_context_tag decl_context);

scope_entry_list_t* query_id_expression_flags(scope_t* st, AST id_expr, 
        unqualified_lookup_behaviour_t unqualified_lookup, 
        lookup_flags_t lookup_flags, struct decl_context_tag decl_context);

// Performs a full unqualified lookup
scope_entry_list_t* query_unqualified_name(scope_t* st, char* unqualified_name);
scope_entry_list_t* query_unqualified_name_flags(scope_t* st, char* unqualified_name, 
        lookup_flags_t lookup_flags);

// Nested names
//    This one should be enough for most cases
scope_entry_list_t* query_nested_name(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup, struct decl_context_tag decl_context);

scope_entry_list_t* query_nested_name_flags(scope_t* sc, AST global_op, AST nested_name, AST name, 
        unqualified_lookup_behaviour_t unqualified_lookup, lookup_flags_t lookup_flags,
        struct decl_context_tag decl_context);
//    These are here for the purpose of flexibility but should be rarely needed
scope_t* query_nested_name_spec(scope_t* sc, AST global_op, AST nested_name, scope_entry_list_t** result_entry_list, 
        char* is_dependent, struct decl_context_tag decl_context);
scope_t* query_nested_name_spec_flags(scope_t* sc, AST global_op, AST nested_name, scope_entry_list_t** result_entry_list,
        char* is_dependent, lookup_flags_t lookup_flags, struct decl_context_tag decl_context);
// char incompatible_symbol_exists(scope_t* st, AST id_expr, enum cxx_symbol_kind symbol_kind);
scope_entry_list_t* query_template_id(AST nested_name_spec, scope_t* st, scope_t* lookup_scope,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_template_id_flags(AST nested_name_spec, scope_t* st, scope_t* lookup_scope,
        lookup_flags_t lookup_flags, struct decl_context_tag decl_context);
scope_entry_list_t* query_unqualified_template_id(AST template_id, scope_t* sc, scope_t* lookup_scope,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_unqualified_template_id_flags(AST template_id, scope_t* sc, scope_t* lookup_scope, 
        lookup_flags_t lookup_flags,
        struct decl_context_tag decl_context);
scope_entry_list_t* query_in_symbols_of_scope(scope_t* sc, char* name);

// Manipulators
scope_entry_list_t* create_list_from_entry(scope_entry_t* entry);
scope_entry_list_t* append_scope_entry_lists(scope_entry_list_t* a, scope_entry_list_t* b);

// Looking for scopes
scope_t* enclosing_namespace_scope(scope_t* st);

// Copy scope
scope_t* copy_scope(scope_t* st);

// Get the fully qualified symbol name in the scope of the ocurrence
char* get_fully_qualified_symbol_name(scope_entry_t* entry, scope_t* st);
char* get_unqualified_template_symbol_name(scope_entry_t* entry, scope_t* st);
char same_scope(scope_t* stA, scope_t* stB);

MCXX_END_DECLS

#endif // CXX_SCOPE_H
