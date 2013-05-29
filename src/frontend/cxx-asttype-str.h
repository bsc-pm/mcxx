#ifndef CXX_ASTTYPE_NAMES_H
#define CXX_ASTTYPE_NAMES_H

#include "cxx-ast.h"

typedef struct node_str_t
{
    const char* name;
    node_t kind;
} node_str_t;

LIBMCXX_EXTERN node_t ast_node_name_to_kind(const char* name);

#endif // CXX_ASTTYPE_NAMES_H
