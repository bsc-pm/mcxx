#ifndef CXX_AST_TYPES_H
#define CXX_AST_TYPES_H

#include "cxx-macros.h"
#include "cxx-asttype.h"
#include "extstruct.h"

MCXX_BEGIN_DECLS

#define MAX_AST_CHILDREN (4)

struct node_ast
{
    // Node stuff
    node_t type; // Node type
    int num_children; // Number of children
    struct node_ast* parent; // Parent node
    struct node_ast* children[MAX_AST_CHILDREN]; // The children
    int line; // Code line
    char* text; // Associated text of the node, normally the symbol or the literal
    char* filename;

    int num_ambig;
    struct node_ast** ambig;

    // Extension node
    extensible_struct_t* extended_data;
};

typedef struct node_ast* AST;

MCXX_END_DECLS

#endif // CXX_AST_TYPES_H
