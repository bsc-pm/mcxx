#ifndef CXX_GRAPHVIZ_H
#define CXX_GRAPHVIZ_H

#include "cxx-ast.h"
#include "cxx-macros.h"

MCXX_BEGIN_DECLS

// Debug
void ast_dump_graphviz(AST a, FILE* f);

MCXX_END_DECLS

#endif
