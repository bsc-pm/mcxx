#ifndef CXX_AMBIGUITY
#define CXX_AMBIGUITY

#include "cxx-ast.h"
#include "cxx-scope.h"

void solve_parameter_declaration_vs_type_parameter_class(AST a);
void solve_ambiguous_declaration(AST a, scope_t* st);
void solve_ambiguous_declarator(AST a, scope_t* st);

#endif // CXX_AMBIGUITY
