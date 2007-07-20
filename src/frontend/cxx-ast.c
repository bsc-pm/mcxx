/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "cxx-ast.h"
/** 
  Abstract Syntax Tree
 */
#include "cxx-ast.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-lexer.h"
#include "cxx-utils.h"

#define out_of_memory() out_of_memory_(__FILE__, __LINE__)
static void out_of_memory_(char* fitxer, int linia);

extensible_schema_t ast_extensible_schema;

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

    result->line = line;
    result->filename = scanning_now.current_filename;

    result->extended_data = calloc(1, sizeof(*(result->extended_data)));
    extensible_struct_init(result->extended_data, &ast_extensible_schema);

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
        ASTText(result) = strdup(text);

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
                    AST wrong_parent = ASTParent(ASTChild(node, i));
                    fprintf(stderr, "Child %d of %s (%s, %p) does not correctly relink. Instead it points to %s (%s, %p)\n",
                            i, node_information(node), ast_print_node_type(ASTType(node)), node,
                            wrong_parent == NULL ? "(null)" : node_information(wrong_parent),
                            wrong_parent == NULL ? "null" : ast_print_node_type(ASTType(wrong_parent)),
                            wrong_parent);
                            
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

    AST result = calloc(1, sizeof(*result));

    // extensible_struct_t orig_extended_data = result->extended_data;

    // Copy everything by value
    *result = *a;

    result->extended_data = calloc(1, sizeof(*(result->extended_data)));
    extensible_struct_init(result->extended_data, &ast_extensible_schema);

    int i;
    for (i = 0; i < ASTNumChildren(result); i++)
    {
        ASTChild(result, i) = duplicate_ast(ASTChild(a, i));
        if (ASTChild(result, i) != NULL)
        {
            ASTParent(ASTChild(result, i)) = result;
        }
    }

    // Duplicate ambiguous trees too
    if (ASTType(a) == AST_AMBIGUITY && 
            (a->num_ambig > 0))
    {
        result->num_ambig = a->num_ambig;
        result->ambig = calloc(a->num_ambig, sizeof(*(result->ambig)));
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = duplicate_ast(a->ambig[i]);
        }
    }

    if (ASTText(a) != NULL)
    {
        ASTText(result) = strdup(ASTText(a));
    }
    ASTParent(result) = NULL;

    return result;
}

/*
AST ASTListLeaf(AST element)
{
    AST result = ASTLeaf(AST_NODE_LIST, 0, NULL);
    result->num_list = 1;
    result->list = (AST*) calloc(sizeof(*result->list), result->num_list);
    result->list[result->num_list-1] = element;

    return result;
}

AST ASTList(AST list, AST element)
{
    list->num_list++;
    list->list = (AST*) realloc(list->list, sizeof(*list->list)*list->num_list);

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

    char* result = calloc(256, sizeof(char));

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
