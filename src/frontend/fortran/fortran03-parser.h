#ifndef FORTRAN03_PARSER_H
#define FORTRAN03_PARSER_H

#include "libmf03-common.h"
#include "cxx-ast.h"
#include "cxx-utils.h"

MCXX_BEGIN_DECLS

LIBMF03_EXTERN int mf03parse(AST* parsed_tree);

MCXX_END_DECLS

#endif // FORTRAN03_PARSER_H
