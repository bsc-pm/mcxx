/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
  Checks that nodes are really doubly-linked.

  We used to have the usual recursive traversal here
  but some big trees feature very deep recursion
  which caused stack overflows.
 */
char ast_check(const_AST root)
{
    int stack_capacity = 1024;
    int stack_length = 1;
    const_AST *stack = NEW_VEC(const_AST, stack_capacity);

    stack[0] = root;

#define PUSH_BACK(child) \
{ \
    if (stack_length == stack_capacity) \
    { \
        stack_capacity *= 2; \
        stack = NEW_REALLOC(const_AST, stack, stack_capacity); \
    } \
    stack_length++; \
    stack[stack_length - 1] = (child); \
}

    char ok = 1;
    while (stack_length > 0 && ok)
    {
        const_AST node = stack[stack_length - 1];
        stack_length--;

        int i;
        if (ast_get_kind(node) != AST_AMBIGUITY)
        {
            for (i = 0; i < MCXX_MAX_AST_CHILDREN && ok; i++)
            {
                AST c = ast_get_child(node, i);
                if (c != NULL)
                {
                    if (ast_get_parent(c) != node)
                    {
                        AST wrong_parent = ast_get_parent(c);
                        fprintf(stderr, "Child %d of %s (%s, %p) does not correctly relink. Instead it points to %s (%s, %p)\n",
                                i, ast_location(node), ast_print_node_type(ast_get_kind(node)), node,
                                wrong_parent == NULL ? "(null)" : ast_location(wrong_parent),
                                wrong_parent == NULL ? "null" : ast_print_node_type(ast_get_kind(wrong_parent)),
                                wrong_parent);
                        ok = 0;
                    }
                    else
                    {
                        PUSH_BACK(c);
                    }
                }
            }

            // Extra check for lists (only top level lists are verified)
            if (ast_get_kind(node) == AST_NODE_LIST
                    && (ast_get_parent(node) == NULL
                        || (ast_get_kind(ast_get_parent(node)) != AST_NODE_LIST))
                    && !ast_check_list_tree(node))
            {
                fprintf(stderr, "List rooted %p (%s) is incorrectly built\n",
                        node,
                        ast_location(node));
                ok = 0;
            }
        }
        else
        {
            for (i = 0; i < node->num_ambig; i++)
            {
                AST c = node->ambig[i];
                PUSH_BACK(c);
            }
        }
    }

    DELETE(stack);

    return ok;
}

char ast_check_list_tree(const_AST root)
{
    int stack_capacity = 1024;
    int stack_length = 1;
    const_AST *stack = NEW_VEC(const_AST, stack_capacity);

    stack[0] = root;

#define PUSH_BACK(child) \
    { \
        if (stack_length == stack_capacity) \
        { \
            stack_capacity *= 2; \
            stack = NEW_REALLOC(const_AST, stack, stack_capacity); \
        } \
        stack_length++; \
        stack[stack_length - 1] = (child); \
    }

    char ok = 1;
    while (stack_length > 0)
    {
        const_AST a = stack[stack_length - 1];
        stack_length--;

        if ((a == NULL)
                || (ASTKind(a) != AST_NODE_LIST)
                || (ASTSon1(a) == NULL)
                || (ASTKind(ASTSon1(a)) == AST_NODE_LIST))
        {
            ok = 0;
            break;
        }

        if (ASTSon0(a) == NULL)
            break;

        if ((ASTKind(ASTSon0(a)) != AST_NODE_LIST)
                || (ASTSon0(a) != NULL && ASTParent(ASTSon0(a)) != a)
                || (ASTParent(ASTSon1(a)) != a))
        {
            ok = 0;
            break;
        }

        PUSH_BACK(ASTSon0(a));
    }

    DELETE(stack);

    return ok;
}

#if 0
char ast_check_list_tree(const_AST a)
{
    if (a == NULL)
        return 0;

    if (ASTKind(a) != AST_NODE_LIST)
        return 0;

    if (ASTSon1(a) == NULL)
        return 0;

    if (ASTKind(ASTSon1(a)) == AST_NODE_LIST)
        return 0;

    if (ASTSon0(a) == NULL)
        return 1;

    if (ASTKind(ASTSon0(a)) != AST_NODE_LIST)
        return 0;

    if (ASTSon0(a) != NULL && ASTParent(ASTSon0(a)) != a)
        return 0;

    if (ASTParent(ASTSon1(a)) != a)
        return 0;

    return ast_check_list_tree(ASTSon0(a));
}
#endif

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

    AST result = NEW0(AST_node_t);

    ast_copy_one_node(result, (AST)a);

    int i;
    if (a->node_type == AST_AMBIGUITY
            && a->num_ambig > 0)
    {
        result->num_ambig = a->num_ambig;
        result->ambig = NEW_VEC(AST, a->num_ambig);
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy(a->ambig[i]);
        }
    }
    else
    {
        result->bitmap_sons = a->bitmap_sons;
        int num_children = ast_count_bitmap(result->bitmap_sons);

        result->children = NEW_VEC(AST, num_children);

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

