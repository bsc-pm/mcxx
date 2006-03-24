#include "cxx-ast.h"
/** 
  Abstract Syntax Tree
 */
#include "cxx-ast.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define out_of_memory() out_of_memory_(__FILE__, __LINE__)
static void out_of_memory_(char* fitxer, int linia);

/**
  Create an AST node

  The text associated with the node must not be a copy, we will copy it
*/
AST ASTMake(node_t type, int num_children, const AST child0, const AST child1, const AST child2, const AST child3, int line, const char *text)
{
	AST result = calloc(1, sizeof(*result));
	if (result == NULL) // unlikely
	{
		out_of_memory();
	}

	ASTParent(result) = NULL;

	ASTType(result) = type;
	result->num_children = num_children;

#define ADD_SON(n) \
	ASTChild##n(result) = child##n; \
	if (child##n != NULL) \
	{ \
		ASTParent(child##n) = result; \
	}

	ADD_SON(0);
	ADD_SON(1);
	ADD_SON(2);
	ADD_SON(3);
#undef ADD_SON

	result->line = line;

	ASTText(result) = NULL;
	if (text != NULL)
	{
		ASTText(result) = strdup(text);

		if (ASTText(result) == NULL) // unlikely
		{
			free(result);
			out_of_memory();
		}
	}

	return result;
}

/**
  Recursively free a tree
 */
void ASTFree(AST node)
{
	if (node != NULL)
	{
		int i;
		for (i = 0; i < MAX_AST_CHILDREN; i++)
		{
			ASTFree(ASTChild(node, i));
		}
		free(node);
	}
}

/**
  Checks that double-linked nodes are
 */
char ASTCheck(AST node)
{
	char check = 1;
	if (node != NULL)
	{
		int i;
		for (i = 0; i < ASTNumChildren(node); i++)
		{
			if (ASTChild(node, i) != NULL)
			{
				if (ASTParent(ASTChild(node, i)) != node)
				{
					check = 0;
				}
				else
				{
					check &= ASTCheck(ASTChild(node, i));
				}
			}
		}
	}
	return check;
}

AST duplicate_ast(AST a)
{
	if (a == NULL)
		return NULL;

	AST result = calloc(1, sizeof(*result));

	// Copy everything by value
	*result = *a;

	int i;
	for (i = 0; i < ASTNumChildren(result); i++)
	{
		ASTChild(result, i) = duplicate_ast(ASTChild(a, i));
		if (ASTChild(result, i) != NULL)
		{
			ASTParent(ASTChild(result, i)) = result;
		}
	}

	if (ASTText(a) != NULL)
	{
		ASTText(result) = strdup(ASTText(a));
	}
	ASTParent(result) = NULL;

	return result;
}

static void out_of_memory_(char* fitxer, int linia)
{
	// running_error("Out of memory at %s:%d", fitxer, linia);
}

char* ast_print_node_type(node_t n)
{
	return ast_node_names[n];
}
