#ifndef CXX_AST_H
#define CXX_AST_H

/*
 * Abstract syntax tree
 */

#define MAX_AST_CHILDREN (4)

#include "cxx-asttype.h"

struct node_ast
{
	// Node stuff
	node_t type; // Node type
	int num_children; // Number of children
	struct node_ast* parent; // Parent node
	struct node_ast* children[MAX_AST_CHILDREN]; // The children
	int line; // Code line
	char* text; // Associated text of the node, normally the symbol or the literal
};

typedef struct node_ast* AST;

// Mandatory macros
#define ASTType(a) ((a)->type)
#define ASTParent(a) ((a)->parent)
// ASTLine hardened to avoid problems (not a lvalue)
#define ASTLine(a) (((a) != NULL) ? ((a)->line) : 0)
#define ASTText(a) ((a)->text)
#define ASTSon0(a) ASTChild(a, 0)
#define ASTSon1(a) ASTChild(a, 1)
#define ASTSon2(a) ASTChild(a, 2)
#define ASTSon3(a) ASTChild(a, 3)

#define ASTLeaf(node, line, text) ASTMake(node, 0,  NULL, NULL, NULL, NULL, line, text)
#define ASTMake1(node, son0, line, text) ASTMake(node, 1, son0, NULL, NULL, NULL, line, text)
#define ASTMake2(node, son0, son1, line, text) ASTMake(node, 2, son0, son1, NULL, NULL, line, text)
#define ASTMake3(node, son0, son1, son2, line, text) ASTMake(node, 3, son0, son1, son2, NULL, line, text)
#define ASTMake4(node, son0, son1, son2, son3, line, text) ASTMake(node, 4, son0, son1, son2, son3, line, text)

// Convenience macros
#define ASTChild0(a) ASTChild(a, 0)
#define ASTChild1(a) ASTChild(a, 1)
#define ASTChild2(a) ASTChild(a, 2)
#define ASTChild3(a) ASTChild(a, 3)
#define ASTChild(a, n) ((a)->children[n])

#define ASTNumChildren(a) ((a)->num_children)

#define ASTListLeaf(a) ASTMake2(AST_NODE_LIST, NULL, a, 0, NULL)
#define ASTList(list,element) ASTMake2(AST_NODE_LIST, list, element, 0, NULL)

#define ASTAmbiguous(a, b) ASTMake2(AST_AMBIGUITY, a, b, 0, NULL)

// Routine to create a node
AST ASTMake(node_t type, int num_children, const AST son0, const AST son1, const AST son2, const AST son3, int line, const char *text);

char ASTCheck(AST node);
void ASTFree(AST node);

AST duplicate_ast(AST a);

char* ast_print_node_type(node_t n);

#endif // CXX_AST_H
