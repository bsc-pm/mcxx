#ifndef CXX_AMBIGUITY
#define CXX_AMBIGUITY

#include "cxx-ast.h"
#include "cxx-scope.h"

// Non contextual
void solve_parameter_declaration_vs_type_parameter_class(AST a);

// Contextual
void solve_ambiguous_declaration(AST a, scope_t* st);
void solve_ambiguous_declarator(AST a, scope_t* st);
void solve_ambiguous_statement(AST a, scope_t* st);
void solve_ambiguous_init_declarator(AST a, scope_t* st);
void solve_ambiguous_type_specifier_seq(AST type_spec_seq, scope_t* st); // one alias to decl_specifier_seq
void solve_ambiguous_decl_specifier_seq(AST type_spec_seq, scope_t* st);
void solve_ambiguous_for_init_statement(AST for_init_statement, scope_t* st);
void solve_ambiguous_parameter_decl(AST parameter_declaration, scope_t* st);
void solve_ambiguous_type_specifier(AST type_specifier, scope_t* st);
void solve_ambiguous_template_argument(AST ambig_template_argument, scope_t* st);
void solve_possibly_ambiguous_expression(AST a, scope_t* st);
void solve_possibly_ambiguous_template_id(AST type_name, scope_t* st);

char check_for_expression(AST a, scope_t* st);

char check_for_initialization(AST initializer, scope_t* st);

void solve_ambiguous_expression_list(AST expression_list, scope_t* st);

#endif // CXX_AMBIGUITY
