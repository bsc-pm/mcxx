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

// We need this to fix nodes
#include "cxx-tltype.h"

#include "cxx-nodecl-decls.h"

// Definition of the type
struct AST_tag
{
    // Node type (1024 different node types)
    node_t node_type:10;

    // This is a bitmap for the sons
    unsigned int bitmap_sons:MCXX_MAX_AST_CHILDREN;

    // Number of ambiguities of this node
    int num_ambig;

    // Parent node
    struct AST_tag* parent;

    // Node locus
    const locus_t* locus;

    // Textual information linked to the node
    // normally the symbol or the literal
    const char* text;

    union
    {
        // The children of this tree (except for AST_AMBIGUITY)
        struct AST_tag** children;
        // When type == AST_AMBIGUITY, all intepretations are here
        struct AST_tag** ambig;
    };

    // This is used by nodecl trees
    struct nodecl_expr_info_tag* expr_info;
};

static inline int count_bitmap(unsigned int bitmap)
{
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
    return __builtin_popcount(bitmap);
#else
    int i;
    int s = 0;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        s += bitmap & 1;
        bitmap = bitmap >> 1;
    }

    return s;
#endif
}

static inline int bitmap_to_index(unsigned int bitmap, int num)
{
    ERROR_CONDITION(((1 << num) & bitmap) == 0,
            "Invalid bitmap!", 0);
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
    bitmap = bitmap & (~(~0U << num));
    return __builtin_popcount(bitmap);
#else
    int i;
    int s = 0;
    for (i = 0; i < num; i++)
    {
        s += bitmap & 1;
        bitmap = bitmap >> 1;
    }

    return s;
#endif
}


static inline int ast_son_num_to_son_index(const_AST a, int num_son)
{
    return bitmap_to_index(a->bitmap_sons, num_son);
}

static inline char ast_has_son(const_AST a, int son)
{
    return (((1 << son) & a->bitmap_sons) != 0);
}

static long long unsigned int _bytes_due_to_astmake = 0;
static long long unsigned int _bytes_due_to_instantiation = 0;

long long unsigned int ast_astmake_used_memory(void)
{
    return _bytes_due_to_astmake;
}

long long unsigned int ast_instantiation_used_memory(void)
{
    return _bytes_due_to_instantiation;
}

AST ast_make(node_t type, int __num_children UNUSED_PARAMETER, 
        AST child0, AST child1, AST child2, AST child3, 
        const locus_t* location, const char *text)
{
    AST result = counted_xmalloc(1, sizeof(*result), &_bytes_due_to_astmake);
    result->node_type = type;

    int num_children = 0;
    unsigned int bitmap_sons = 0;
#define COUNT_SON(n) \
    if (child##n != NULL) \
    { \
        num_children++; \
        bitmap_sons |= (1 << n); \
    }

    result->bitmap_sons = bitmap_sons;
    result->parent = NULL;
    result->locus = location;

    if (text != NULL)
    {
        result->text = uniquestr(text);
    }
    else
    {
        result->text = NULL;
    }

    COUNT_SON(0);
    COUNT_SON(1);
    COUNT_SON(2);
    COUNT_SON(3);
#undef COUNT_SON

    result->bitmap_sons = bitmap_sons;
    result->children = counted_xmalloc(
            sizeof(*result->children), 
            num_children,
            &_bytes_due_to_astmake);

    int index = 0;
#define ADD_SON(n) \
    if (child##n != NULL) \
    { \
        result->children[index] = child##n; \
        child##n->parent = result; \
        index++; \
    }

    ADD_SON(0);
    ADD_SON(1);
    ADD_SON(2);
    ADD_SON(3);
#undef ADD_SON

    result->expr_info = NULL;

    return result;
}

AST ast_get_parent(const_AST a)
{
    return a->parent;
}

unsigned int ast_get_line(const_AST a)
{
    return locus_get_line(a->locus);
}

const char* ast_get_text(const_AST a)
{
    return a->text;
}

node_t ast_get_type(const_AST a)
{
    return a->node_type;
}

void ast_set_text(AST a, const char* str)
{
    a->text = uniquestr(str);
}

void ast_set_type(AST a, node_t node_type)
{
    a->node_type = node_type;
}

AST ast_get_child(const_AST a, int num_child)
{
    if (ast_has_son(a, num_child))
    {
        return a->children[ast_son_num_to_son_index(a, num_child)];
    }
    else
    {
        return NULL;
    }
}

void ast_set_parent(AST a, AST parent)
{
    a->parent = parent;
}

// This function works both for shrinking or widening
static void ast_reallocate_children(AST a, int num_child, AST new_child)
{
    // Reallocate childrens
    // Save the old children array
    AST* old_children = a->children;
    // And the old children bitmap
    unsigned int old_bitmap = a->bitmap_sons;

    // Enable or disable this new son depending on it being null
    if (new_child != NULL)
    {
        a->bitmap_sons = (a->bitmap_sons | (1 << num_child));
    }
    else
    {
        a->bitmap_sons = (a->bitmap_sons & (~(1 << num_child)));
    }

    a->children = counted_xmalloc(sizeof(*a->children), 
            (count_bitmap(a->bitmap_sons)), 
            &_bytes_due_to_astmake);

    // Now for every old son, update the new children
    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        // Note that when shrinking the node ast_has_son
        // will always return false for num_child.
        if (ast_has_son(a, i))
        {
            if (i == num_child)
            {
                // The new son
                a->children[ast_son_num_to_son_index(a, i)] = new_child;
            }
            else
            {
                // Old sons that are updated their position
                a->children[ast_son_num_to_son_index(a, i)] = 
                    old_children[bitmap_to_index(old_bitmap, i)];
            }
        }
    }

    // Now xfree the old children (if any)
    if (old_children != NULL)
        xfree(old_children);

    // Count this free
    _bytes_due_to_astmake -= (count_bitmap(old_bitmap) * sizeof(AST));
}

void ast_set_child_but_parent(AST a, int num_child, AST new_child)
{
    if (new_child == NULL)
    {
        if (ast_has_son(a, num_child))
        {
            // Shrink the node if we have it
            ast_reallocate_children(a, num_child, new_child);
        }
    }
    else
    {
        if (ast_has_son(a, num_child))
        {
            a->children[ast_son_num_to_son_index(a, num_child)] = new_child;
        }
        else
        {
            // This will widen the node
            ast_reallocate_children(a, num_child, new_child);
        }
    }
}

void ast_set_child(AST a, int num_child, AST new_child)
{
    ast_set_child_but_parent(a, num_child, new_child);
    if (new_child != NULL)
    {
        new_child->parent = a;
    }
}

int ast_num_children(const_AST a)
{
    return count_bitmap(a->bitmap_sons);
}

/**
  Checks that double-linked nodes are
 */
char ast_check(const_AST node)
{
    char check = 1;
    if (node != NULL)
    {
        int i;
        if (ast_get_type(node) != AST_AMBIGUITY)
        {
            for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
            {
                if (ast_get_child(node, i) != NULL)
                {
                    if (ast_get_parent(ast_get_child(node, i)) != node)
                    {
                        check = 0;
                        AST wrong_parent = ast_get_parent(ast_get_child(node, i));
                        fprintf(stderr, "Child %d of %s (%s, %p) does not correctly relink. Instead it points to %s (%s, %p)\n",
                                i, ast_location(node), ast_print_node_type(ast_get_type(node)), node,
                                wrong_parent == NULL ? "(null)" : ast_location(wrong_parent),
                                wrong_parent == NULL ? "null" : ast_print_node_type(ast_get_type(wrong_parent)),
                                wrong_parent);

                    }
                    else
                    {
                        check &= ast_check(ast_get_child(node, i));
                    }
                }
            }
        }
        else 
        {
            for (i = 0; i < node->num_ambig; i++)
            {
                check &= ast_check(node->ambig[i]);
            }
        }
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

    AST result = counted_xcalloc(1, sizeof(*result), &_bytes_due_to_astmake);

    ast_copy_one_node(result, (AST)a);

    int i;
    if (a->node_type == AST_AMBIGUITY
            && a->num_ambig > 0)
    {
        result->num_ambig = a->num_ambig;
        result->ambig = counted_xcalloc(a->num_ambig, sizeof(*(result->ambig)), &_bytes_due_to_astmake);
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy(a->ambig[i]);
        }
    }
    else
    {
        result->bitmap_sons = a->bitmap_sons;
        int num_children = count_bitmap(result->bitmap_sons);

        result->children = counted_xcalloc(num_children, sizeof(*(result->children)), &_bytes_due_to_astmake);

        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            AST c = ast_copy(ast_get_child(a, i));
            if (c != NULL)
                ast_set_child(result, i, c);
        }
    }

    if (a->text != NULL)
    {
        result->text = uniquestr(a->text);
    }

    result->parent = NULL;

    return result;
}

AST ast_copy_for_instantiation(const_AST a)
{
    return ast_copy(a);
}

AST ast_list_leaf(AST a)
{
    ERROR_CONDITION(a == NULL, "Invalid tree", 0);
    AST result = ast_make(AST_NODE_LIST, 2, NULL, a, NULL, NULL, 
            ast_get_locus(a), ast_get_text(a));

    return result;
}

AST ast_list(AST list, AST last_elem)
{
    ERROR_CONDITION(last_elem == NULL, "Invalid tree", 0);
    const char* filename = NULL;
    if (list != NULL)
    {
        filename = ast_get_filename(list);
    }
    else
    {
        filename = ast_get_filename(last_elem);
    }

    AST a = ast_make(AST_NODE_LIST, 2, list, last_elem, NULL, NULL, 
            make_locus(filename, ast_get_line(last_elem), 0), ast_get_text(last_elem));

    return a;
}

AST ast_list_head(AST list)
{
    if (list == NULL)
        return NULL;

    if (ASTType(list) != AST_NODE_LIST)
        return NULL;

    AST iter;
    for_each_element(list, iter)
    {
        return iter;
    }

    return NULL;
}

AST ast_list_concat(AST before, AST after)
{
    if (before == NULL)
        return after;
    if (after == NULL)
        return before;

    if (ASTType(before) != AST_NODE_LIST
            || ASTType(after) != AST_NODE_LIST)
        return NULL;

    AST head_after = ast_list_head(after);

    ast_set_child(head_after, 0, before);
    return after;
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

const char* ast_node_type_name(node_t n)
{
    return ast_node_names[n];
}

int ast_num_of_given_child(const_AST parent, const_AST child)
{
    int i;
    for (i = 0; i < 4; i++)
    {
        if (ast_get_child(parent, i) == child)
            return i;
    }

    return -1;
}

char ast_equal_node (const_AST ast1, const_AST ast2)
{
    if (ast1 == ast2)
        return 1;
    if (!ast1 || !ast2)
        return 0;

    if (ast_get_type(ast1) != ast_get_type(ast2))
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

const char* ast_location(const_AST a)
{
    return locus_to_str(a->locus);
}

#if 0
// Note that we use expr_info to mark visited nodes since AST involved
// in ambiguities do not have expr_info
static void ast_mark_visit(AST a, char visited)
{
    if (a == NULL)
        return;

    if (ast_get_type(a) == AST_AMBIGUITY)
    {
        int i;
        for (i = 0; i < ast_get_num_ambiguities(a); i++)
        {
            AST current = ast_get_ambiguity(a, i);
            current->expr_info = (struct nodecl_expr_info_tag*)(intptr_t)visited;

            ast_mark_visit(current, visited);
        }
    }
    else
    {
        int i;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            AST current = ast_get_child(a, i);
            if (current != NULL)
            {
                current->expr_info = (struct nodecl_expr_info_tag*)(intptr_t)visited;
                ast_mark_visit(current, visited);
            }
        }
    }
}

static char ast_has_been_visited(AST a)
{
    // We will piggy back the expr_info to keep the visited mark
    return a->expr_info != NULL;
}
#endif

int ast_get_num_ambiguities(const_AST a)
{
    return a->num_ambig;
}

AST ast_get_ambiguity(const_AST a, int num)
{
    return a->ambig[num];
}

#if 0
static void ast_set_ambiguity(AST a, int num, AST child)
{
    a->ambig[num] = child;
}

static AST unshare_nodes_rec(AST a)
{
    if (a == NULL)
        return NULL;

    if (node_has_been_visited(a))
    {
        return ast_copy(a);
    }
    else
    {
        if (ast_get_type(a) == AST_AMBIGUITY)
        {
            int i;
            for (i = 0; i < ast_get_num_ambiguities(a); i++)
            {
                AST current = ast_get_ambiguity(a, i);

                ast_set_ambiguity(a, i, unshare_nodes_rec(current));
            }
        }
        else
        {
            int i;
            for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
            {
                AST current = ast_get_child(a, i);

                ast_set_child(a, i, unshare_nodes_rec(current));
            }
        }

        return a;
    }
}
#endif

void ast_fix_parents_inside_intepretation(AST node)
{
    if (node == NULL)
        return;

    if (ast_get_type(node) == AST_AMBIGUITY)
        return;

    int i;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (ast_get_child(node, i) != NULL)
        {
            ast_set_parent(ast_get_child(node, i), node);

            ast_fix_parents_inside_intepretation(ast_get_child(node, i));
        }
    }
}

// Be careful when handling ambiguity nodes, the set of interpretations may not
// be an actual tree but a DAG
AST ast_make_ambiguous(AST son0, AST son1)
{
    if (ASTType(son0) == AST_AMBIGUITY)
    {
        if (ASTType(son1) == AST_AMBIGUITY)
        {
            int original_son0 = son0->num_ambig;

            son0->num_ambig += son1->num_ambig;
            son0->ambig = (AST*) xrealloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);

            int i;
            for (i = 0; i < son1->num_ambig; i++)
            {
                son0->ambig[original_son0 + i] = son1->ambig[i];
            }

            return son0;
        }
        else
        {
            son0->num_ambig++;
            son0->ambig = (AST*) xrealloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);
            son0->ambig[son0->num_ambig-1] = son1;

            return son0;
        }
    }
    else if (ASTType(son1) == AST_AMBIGUITY)
    {
        son1->num_ambig++;
        son1->ambig = (AST*) xrealloc(son1->ambig, sizeof(*(son1->ambig)) * son1->num_ambig);
        son1->ambig[son1->num_ambig-1] = son0;

        return son1;
    }
    else
    {
        AST result = ASTLeaf(AST_AMBIGUITY, make_locus("", 0, 0), NULL);

        result->num_ambig = 2;
        result->ambig = counted_xmalloc(sizeof(*(result->ambig)), result->num_ambig, &_bytes_due_to_astmake);
        result->ambig[0] = son0;
        result->ambig[1] = son1;
        result->locus = son0->locus;

        return result;
    }
}


void ast_replace(AST dest, const_AST src)
{
    *dest = *src;
}

void ast_free(AST a)
{
    if (a != NULL)
    {
        if (ast_get_type(a) == AST_AMBIGUITY)
        {
            int i;
            for (i = 0; i < ast_get_num_ambiguities(a); i++)
            {
                ast_free(ast_get_ambiguity(a, i));
            }
        }
        else
        {
            int i;
            for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
            {
                ast_free(ast_get_child(a, i));
            }
        }

        // This will uncover dangling references
        xfree(a->expr_info);
        xfree(a->children);
        _bytes_due_to_astmake -= sizeof(*(a->children)) * count_bitmap(a->bitmap_sons);
        memset(a, 0, sizeof(*a));
        xfree(a);

        _bytes_due_to_astmake -= sizeof(*a);
    }
}

void ast_replace_with_ambiguity(AST a, int n)
{
    if (n >= ast_get_num_ambiguities(a))
    {
        internal_error("There is no such option (%d) in this ambiguous node (options = %d)", n, ast_get_num_ambiguities(a));
    }
    else if (n < 0)
    {
        internal_error("Invalid node number (%d)", n);
    }

    AST parent = ASTParent(a);

    DEBUG_CODE()
    {
        fprintf(stderr, "*** Choosing '%s' in the ambiguity tree %p (%s) using %p\n", 
                ast_print_node_type(ASTType(ast_get_ambiguity(a, n))), a, ast_location(ast_get_ambiguity(a, n)), 
                ast_get_ambiguity(a, n));
    }

    AST chosen_ambig = ast_get_ambiguity(a, n);
    ast_replace(a, chosen_ambig);

    // Correctly relink to the parent
    ast_set_parent(a, parent);
    ast_fix_parents_inside_intepretation(a);

    // if (!ASTCheck(a))
    // {
    //     internal_error("*** INCONSISTENT TREE DETECTED IN DISAMBIGUATED TREE %p ***\n", a);
    // }
}

const locus_t* ast_get_locus(const_AST a)
{
    return a->locus;
}

void ast_set_locus(AST a, const locus_t* locus)
{
    a->locus = locus;
}

const char *ast_get_filename(const_AST a)
{
    return locus_get_filename(a->locus);
}

int ast_node_size(void)
{
    return sizeof(struct AST_tag);
}

struct nodecl_expr_info_tag* ast_get_expr_info(const_AST a)
{
    return a->expr_info;
}

void ast_set_expr_info(AST a, struct nodecl_expr_info_tag* expr_info)
{
    a->expr_info = expr_info;
}
