#include "cxx-ast.h"
/** 
  Abstract Syntax Tree
 */
#include "cxx-ast.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <gc.h>

#include "cxx-lexer.h"
#include "cxx-utils.h"

#define out_of_memory() out_of_memory_(__FILE__, __LINE__)
static void out_of_memory_(char* fitxer, int linia);

/**
  Create an AST node

  The text associated with the node must not be a copy, we will copy it
*/
AST ASTMake(node_t type, int num_children, const AST child0, const AST child1, const AST child2, const AST child3, int line, const char *text)
{
	AST result = GC_CALLOC(1, sizeof(*result));
	if (result == NULL) // unlikely
	{
		out_of_memory();
	}

	ASTParent(result) = NULL;

	ASTType(result) = type;
	result->num_children = num_children;

	result->filename = scanning_now.current_filename;

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

	ASTText(result) = NULL;
	if (text != NULL)
	{
		ASTText(result) = GC_STRDUP(text);

		if (ASTText(result) == NULL) // unlikely
		{
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
	// Does nothing
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

		if (ASTType(node) == AST_AMBIGUITY)
		{
			for (i = 0; i < node->num_ambig; i++)
			{
				check &= ASTCheck(node->ambig[i]);
			}
		}
	}
	return check;
}

AST duplicate_ast(AST a)
{
	if (a == NULL)
		return NULL;

	AST result = GC_CALLOC(1, sizeof(*result));

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
		ASTText(result) = GC_STRDUP(ASTText(a));
	}
	ASTParent(result) = NULL;

	return result;
}

/*
AST ASTListLeaf(AST element)
{
	AST result = ASTLeaf(AST_NODE_LIST, 0, NULL);
	result->num_list = 1;
	result->list = (AST*) GC_CALLOC(sizeof(*result->list), result->num_list);
	result->list[result->num_list-1] = element;

	return result;
}

AST ASTList(AST list, AST element)
{
	list->num_list++;
	list->list = (AST*) GC_REALLOC(list->list, sizeof(*list->list)*list->num_list);

	list->list[list->num_list-1] = element;

	return list;
}
*/

static void out_of_memory_(char* fitxer, int linia)
{
	// running_error("Out of memory at %s:%d", fitxer, linia);
}

char* ast_print_node_type(node_t n)
{
	return ast_node_names[n];
}

int get_children_num(AST parent, AST children)
{
	int i;
	for (i = 0; i < 4; i++)
	{
		if (ASTChild(parent, i) == children)
			return i;
	}

	return -1;
}

char ast_equal_node (AST ast1, AST ast2)
{
	if (ast1 == ast2)
		return 1;
	if (!ast1 || !ast2)
		return 0;

	if (ASTType(ast1) != ASTType(ast2))
		return 0;
	if (ASTNumChildren(ast1) != ASTNumChildren(ast2))
		return 0;

	if (ASTText(ast1) != ASTText(ast2))
	{
		if (!ASTText(ast1) || !ASTText(ast2))
			return 0;
		if (strcmp (ASTText(ast1), ASTText(ast2)))
			return 0;
	}

	return 1;
}

char ast_equal (AST ast1, AST ast2)
{
	int i;

	if (!ast_equal_node (ast1, ast2))
		return 0;

	if (ast1 == NULL)
		return 1;

	for (i = 0; i < ASTNumChildren(ast1); i++)
	{
		if (!ast_equal(ASTChild(ast1, i), ASTChild(ast2, i)))
			return 0;
	}
	return 1;
}

char* node_information(AST a)
{
	if (a == NULL)
		return "";

	char* result = GC_CALLOC(256, sizeof(char));

	if (ASTFileName(a) == NULL)
	{
		snprintf(result, 255, "<unknown file>:%d", ASTLine(a));
	}
	else
	{
		snprintf(result, 255, "%s:%d", ASTFileName(a), ASTLine(a));
	}

	result[255] = '\0';

	return result;
}
