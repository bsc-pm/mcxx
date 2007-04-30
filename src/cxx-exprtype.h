#ifndef CXX_EXPRTYPE_H
#define CXX_EXPRTYPE_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

/*
 * Computes the type of an expression
 *
 * Only implemented for C
 *
 */

type_t *compute_expression_type(AST expr, scope_t *sc, decl_context_t decl_context);

#endif
