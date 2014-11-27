/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




/** 
  Abstract Syntax Tree
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "cxx-ast.h"
#include "cxx-locus.h"
#include "cxx-limits.h"

#include "cxx-lexer.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"

#include "cxx-nodecl-decls.h"

/**
  Checks that double-linked nodes are
 */
char ast_check(const_AST node)
{
    char check = 1;
    if (node == NULL)
        return check;

    // Already visited. See below
    if (__builtin_expect(((((intptr_t)node->parent) & 0x1) == 0x1), 0))
    {
        check = 0;
        fprintf(stderr, "ERROR: Cycle detected in node '%s'\n", ast_location(node));
        return check;
    }

    int i;
    if (ast_get_kind(node) != AST_AMBIGUITY)
    {
        for (i = 0; i < MCXX_MAX_AST_CHILDREN && check; i++)
        {
            if (ast_get_child(node, i) != NULL)
            {
                if (ast_get_parent(ast_get_child(node, i)) != node)
                {
                    check = 0;
                    AST wrong_parent = ast_get_parent(ast_get_child(node, i));
                    fprintf(stderr, "Child %d of %s (%s, %p) does not correctly relink. Instead it points to %s (%s, %p)\n",
                            i, ast_location(node), ast_print_node_type(ast_get_kind(node)), node,
                            wrong_parent == NULL ? "(null)" : ast_location(wrong_parent),
                            wrong_parent == NULL ? "null" : ast_print_node_type(ast_get_kind(wrong_parent)),
                            wrong_parent);
                }
                else
                {
                    // Tag this node as visited to avoid infinite recursion under the presence
                    // of cycles (note that this works as long as AST pointers are at least
                    // aligned to two bytes)
                    ((AST)node)->parent = (struct AST_tag*)(((intptr_t)node->parent) | 0x1);

                    check = check && ast_check(ast_get_child(node, i));

                    // Remove tag
                    ((AST)node)->parent = (struct AST_tag*)(((intptr_t)node->parent) & ~0x1);
                }
            }
        }
    }
    else
    {
        ((AST)node)->parent = (struct AST_tag*)(((intptr_t)node->parent) | 0x1);
        for (i = 0; i < node->num_ambig && check; i++)
        {
            check = check && ast_check(node->ambig[i]);
        }
        ((AST)node)->parent = (struct AST_tag*)(((intptr_t)node->parent) & ~0x1);
    }
    return check;
}

static void ast_copy_one_node(AST dest, AST orig)
{
    *dest = *orig;
    dest->bitmap_sons = 0;
    dest->children = 0;
}

AST ast_duplicate_one_node(AST orig)
{
    AST dest = ASTLeaf(AST_INVALID_NODE, make_locus(NULL, 0, 0), NULL);
    ast_copy_one_node(dest, orig);

    return dest;
}

AST ast_copy(const_AST a)
{
    if (a == NULL)
        return NULL;

    AST result = xcalloc(1, sizeof(*result));

    ast_copy_one_node(result, (AST)a);

    int i;
    if (a->node_type == AST_AMBIGUITY
            && a->num_ambig > 0)
    {
        result->num_ambig = a->num_ambig;
        result->ambig = xcalloc(a->num_ambig, sizeof(*(result->ambig)));
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy(a->ambig[i]);
        }
    }
    else
    {
        result->bitmap_sons = a->bitmap_sons;
        int num_children = ast_count_bitmap(result->bitmap_sons);

        result->children = xcalloc(num_children, sizeof(*(result->children)));

        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            AST c = ast_copy(ast_get_child(a, i));
            if (c != NULL)
                ast_set_child(result, i, c);
        }
    }

    if (a->text != NULL)
    {
        result->text = a->text;
    }

    result->parent = NULL;

    return result;
}

AST ast_copy_for_instantiation(const_AST a)
{
    return ast_copy(a);
}

void ast_list_split_head_tail(AST list, AST *head, AST* tail)
{
    if (list == NULL)
    {
        *head = NULL;
        *tail = NULL;
        return;
    }

    AST first = ast_list_head(list);
    if (first == list)
    {
        *tail = NULL;
    }
    else
    {
        AST p = ASTParent(first);
        ast_set_child(p, 0, NULL);
        ast_set_parent(first, NULL);

        *tail = list;
    }
    *head = first;
}

char ast_equal_node (const_AST ast1, const_AST ast2)
{
    if (ast1 == ast2)
        return 1;
    if (!ast1 || !ast2)
        return 0;

    if (ast_get_kind(ast1) != ast_get_kind(ast2))
        return 0;
    if (ast_num_children(ast1) != ast_num_children(ast2))
        return 0;

    if (ast_get_text(ast1) != ast_get_text(ast2))
    {
        if (!ast_get_text(ast1) || !ast_get_text(ast2))
            return 0;
        if (strcmp (ast_get_text(ast1), ast_get_text(ast2)))
            return 0;
    }

    return 1;
}

char ast_equal (const_AST ast1, const_AST ast2)
{
    int i;

    if (!ast_equal_node (ast1, ast2))
        return 0;

    if (ast1 == NULL)
        return 1;

    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (!ast_equal(ast_get_child(ast1, i), ast_get_child(ast2, i)))
            return 0;
    }
    return 1;
}

