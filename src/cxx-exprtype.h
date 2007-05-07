#ifndef CXX_EXPRTYPE_H
#define CXX_EXPRTYPE_H

#include "cxx-ast.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

MCXX_BEGIN_DECLS

/*
 * Computes the type of an expression
 *
 * Only implemented for C
 *
 */
type_t *integer_type(void);
type_t *unsigned_integer_type(void);

type_t *compute_expression_type(AST expr, scope_t *sc, decl_context_t decl_context);

MCXX_END_DECLS

#endif
