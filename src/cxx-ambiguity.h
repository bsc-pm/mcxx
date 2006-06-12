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
void solve_ambiguous_type_spec_seq(AST type_spec_seq, scope_t* st);
void solve_ambiguous_for_init_statement(AST for_init_statement, scope_t* st);

void solve_possibly_ambiguous_expression(AST a, scope_t* st);

void choose_option(AST a, int n);

#endif // CXX_AMBIGUITY
