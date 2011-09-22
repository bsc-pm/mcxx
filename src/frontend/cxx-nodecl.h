#ifndef CXX_NODECL_H
#define CXX_NODECL_H

#include "cxx-macros.h"
#include "cxx-cexpr-fwd.h"
#include "cxx-scope-fwd.h"
#include "cxx-nodecl-decls.h"

MCXX_BEGIN_DECLS

nodecl_t nodecl_null(void);
char nodecl_is_null(nodecl_t t);

AST nodecl_get_ast(nodecl_t t);

nodecl_t nodecl_copy(nodecl_t t);

// Children
nodecl_t nodecl_get_child(nodecl_t n, int i);

// Constant values
char nodecl_is_constant(nodecl_t t);
void nodecl_set_constant(nodecl_t t, const_value_t* cval);
const_value_t* nodecl_get_constant(nodecl_t t);

// Text
void nodecl_set_text(nodecl_t t, const char*);
const char* nodecl_get_text(nodecl_t);

// Type
type_t* nodecl_get_type(nodecl_t);
void nodecl_set_type(nodecl_t, type_t*);

// Symbol
scope_entry_t* nodecl_get_symbol(nodecl_t);
void nodecl_set_symbol(nodecl_t, scope_entry_t*);

// Template parameters (C++ only)
void nodecl_set_template_parameters(nodecl_t n, template_parameter_list_t* template_parameters);
template_parameter_list_t* nodecl_get_template_parameters(nodecl_t n);

// Location
const char* nodecl_get_filename(nodecl_t);
int nodecl_get_line(nodecl_t);
const char* nodecl_get_locus(nodecl_t);

// Kind of node
node_t nodecl_get_kind(nodecl_t);

// 'list' parameter can be a 'nodecl_null()'
nodecl_t nodecl_append_to_list(nodecl_t list, nodecl_t element);

// Returns the head of the list
nodecl_t nodecl_list_head(nodecl_t list);

// Either list1 or list2 can be 'nodecl_null()'
nodecl_t nodecl_concat_lists(nodecl_t list1, nodecl_t list2);

// States that this nodecl is a list
char nodecl_is_list(nodecl_t);

// Unpack a list. Do not forget to free the returned pointer
nodecl_t* nodecl_unpack_list(nodecl_t n, int *num_items);

// Length of a list
int nodecl_list_length(nodecl_t list);

// Wrap (use sparingly)
nodecl_t _nodecl_wrap(AST);

// Generic routines (meant for generic processing)
nodecl_t nodecl_generic_make(node_t, const char* filename, int line);
void nodecl_set_child(nodecl_t, int, nodecl_t);

// Free a temporally allocated nodecl
void nodecl_free(nodecl_t);

// Expression specific stuff
char nodecl_expr_is_lvalue(nodecl_t);
void nodecl_expr_set_is_lvalue(nodecl_t, char);

char nodecl_expr_is_value_dependent(nodecl_t);
void nodecl_expr_set_is_value_dependent(nodecl_t, char);

char nodecl_expr_is_type_dependent(nodecl_t);
void nodecl_expr_set_is_type_dependent(nodecl_t, char);

char nodecl_is_err_expr(nodecl_t);

// Retrieve the context from a node (or the currently compiled file global
// context if not context is found)
decl_context_t nodecl_retrieve_context(nodecl_t);

// These only work on NODECL_CONTEXT
decl_context_t nodecl_get_decl_context(nodecl_t n);
void nodecl_set_decl_context(nodecl_t, decl_context_t);

MCXX_END_DECLS

#endif // CXX_NODECL_H
