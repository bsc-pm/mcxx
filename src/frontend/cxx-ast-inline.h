/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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


#ifndef CXX_AST_INLINE_H
#define CXX_AST_INLINE_H

#include "mem.h"
#include "cxx-process.h"
#include <stdint.h>

MCXX_BEGIN_DECLS

// Definition of the type
typedef
struct AST_tag
{
    // Node type (1024 different node types)
    node_t node_type:11;

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
} AST_node_t;


static inline node_t ast_get_kind(const_AST a)
{
    return a->node_type;
}

static inline AST ast_get_parent(const_AST a)
{
    return a->parent;
}

static inline unsigned int ast_get_line(const_AST a)
{
    return locus_get_line(ast_get_locus(a));
}

static inline unsigned int ast_get_column(const_AST a)
{
    return locus_get_column(ast_get_locus(a));
}

static inline const char* ast_get_text(const_AST a)
{
    return a->text;
}

static inline void ast_set_text(AST a, const char* str)
{
    a->text = str;
}

static inline void ast_set_kind(AST a, node_t node_type)
{
    a->node_type = node_type;
}

static inline int ast_bitmap_to_index(unsigned int bitmap, int num)
{
    ERROR_CONDITION(((1 << num) & bitmap) == 0,
            "Invalid bitmap!", 0);
#if HAVE__BUILTIN_POPCOUNT
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

ALWAYS_INLINE static inline int ast_son_num_to_son_index(const_AST a, int num_son)
{
    return ast_bitmap_to_index(a->bitmap_sons, num_son);
}

ALWAYS_INLINE static inline char ast_has_son(const_AST a, int son)
{
    return (((1 << son) & a->bitmap_sons) != 0);
}

ALWAYS_INLINE static inline AST ast_get_child(const_AST a, int num_child)
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

static inline void ast_set_parent(AST a, AST parent)
{
    a->parent = parent;
}

static inline int ast_count_bitmap(unsigned int bitmap)
{
#if HAVE__BUILTIN_POPCOUNT
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

static inline AST ast_make(node_t type, int __num_children UNUSED_PARAMETER, 
        AST child0, AST child1, AST child2, AST child3, 
        const locus_t* location, const char *text)
{
    AST result = NEW(AST_node_t);
    // ERROR_CONDITION(result & 0x1 != 0, "Invalid pointer for AST", 0);

    result->node_type = type;

    int num_children = 0;
    unsigned int bitmap_sons = 0;

    result->bitmap_sons = bitmap_sons;
    result->parent = NULL;
    result->locus = location;

    result->text = text;

    bitmap_sons =
        (!!child0)
        | (!!child1 << 1)
        | (!!child2 << 2)
        | (!!child3 << 3);
    num_children = ast_count_bitmap(bitmap_sons);

    result->bitmap_sons = bitmap_sons;
    result->children = NEW_VEC(AST, num_children);

    int idx = 0;
#define ADD_SON(n) \
    if (child##n != NULL) \
    { \
        result->children[idx] = child##n; \
        child##n->parent = result; \
        idx++; \
    }

    ADD_SON(0);
    ADD_SON(1);
    ADD_SON(2);
    ADD_SON(3);
#undef ADD_SON

    result->expr_info = NULL;

    return result;
}

// This function works both for shrinking or widening
static inline void ast_reallocate_children(AST a, int num_child, AST new_child)
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

    a->children = NEW_VEC(AST, ast_count_bitmap(a->bitmap_sons));

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
                    old_children[ast_bitmap_to_index(old_bitmap, i)];
            }
        }
    }

    // Now DELETE the old children (if any)
    if (old_children != NULL)
        DELETE(old_children);
}

static inline void ast_set_child_but_parent(AST a, int num_child, AST new_child)
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

static inline void ast_set_child(AST a, int num_child, AST new_child)
{
    ast_set_child_but_parent(a, num_child, new_child);
    if (new_child != NULL)
    {
        new_child->parent = a;
    }
}

static inline int ast_num_children(const_AST a)
{
    return ast_count_bitmap(a->bitmap_sons);
}


static inline AST ast_list(AST list, AST last_elem)
{
    ERROR_CONDITION(last_elem == NULL, "Invalid tree", 0);

    AST a = ast_make(AST_NODE_LIST, 2, list, last_elem, NULL, NULL, 
            NULL, /* lists have a calculated locus not a physical one */
            ast_get_text(last_elem));
    return a;
}

static inline AST ast_list_leaf(AST a)
{
    return ast_list(NULL, a);
}

static inline AST ast_list_head(const_AST list)
{
    if (list == NULL)
        return NULL;

    if (ASTKind(list) != AST_NODE_LIST)
        return NULL;

    const_AST iter;
    for_each_element(list, iter)
    {
        return (AST)iter;
    }

    return NULL;
}

static inline AST ast_list_concat(AST before, AST after)
{
    if (before == NULL)
        return after;
    if (after == NULL)
        return before;

    if (ASTKind(before) != AST_NODE_LIST
            || ASTKind(after) != AST_NODE_LIST)
        return NULL;

    AST head_after = ast_list_head(after);

    ast_set_child(head_after, 0, before);
    return after;
}


static inline const char* ast_location(const_AST a)
{
    return locus_to_str(ast_get_locus(a));
}

static inline int ast_get_num_ambiguities(const_AST a)
{
    return a->num_ambig;
}

static inline AST ast_get_ambiguity(const_AST a, int num)
{
    return a->ambig[num];
}

static inline void ast_fix_parents_inside_intepretation(AST node)
{
    if (node == NULL)
        return;

    if (ast_get_kind(node) == AST_AMBIGUITY)
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

// Be careful when handling ambiguity nodes, the set of interpretations may not
// be an actual tree but a DAG
static inline AST ast_make_ambiguous(AST son0, AST son1)
{
    if (ASTKind(son0) == AST_AMBIGUITY)
    {
        if (ASTKind(son1) == AST_AMBIGUITY)
        {
            int original_son0 = son0->num_ambig;

            son0->num_ambig += son1->num_ambig;
            son0->ambig = NEW_REALLOC(AST, son0->ambig, son0->num_ambig);

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
            son0->ambig = NEW_REALLOC(AST, son0->ambig, son0->num_ambig);
            son0->ambig[son0->num_ambig-1] = son1;

            return son0;
        }
    }
    else if (ASTKind(son1) == AST_AMBIGUITY)
    {
        son1->num_ambig++;
        son1->ambig = NEW_REALLOC(AST, son1->ambig, son1->num_ambig);
        son1->ambig[son1->num_ambig-1] = son0;

        return son1;
    }
    else
    {
        AST result = ASTLeaf(AST_AMBIGUITY, make_locus("", 0, 0), NULL);

        result->num_ambig = 2;
        result->ambig = NEW_VEC(AST, result->num_ambig);
        result->ambig[0] = son0;
        result->ambig[1] = son1;
        result->locus = son0->locus;

        return result;
    }
}

static inline void ast_replace(AST dest, const_AST src)
{
    *dest = *src;
}

static inline void ast_free(AST a)
{
    if (a == NULL)
        return;

    // Already visited. See below
    if (__builtin_expect(((((intptr_t)a->parent) & 0x1) == 0x1), 0))
        return;

    // Tag this node as visited to avoid infinite recursion under the presence
    // of cycles (note that this works as long as AST pointers are at least
    // aligned to two bytes)
    a->parent = (struct AST_tag*)(((intptr_t)a->parent) | 0x1);

    if (ast_get_kind(a) == AST_AMBIGUITY)
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

    DELETE(a->expr_info);
    DELETE(a->children);
    // Clear the node for safety
    // __builtin_memset(a, 0, sizeof(*a));
    DELETE(a);
}

static inline void ast_replace_with_ambiguity(AST a, int n)
{
    ERROR_CONDITION(n >= ast_get_num_ambiguities(a),
        "There is no such option (%d) in this ambiguous node (options = %d)", n, ast_get_num_ambiguities(a));
    ERROR_CONDITION(n < 0, "Invalid node number (%d)", n);

    AST parent = ASTParent(a);

    // DEBUG_CODE()
    // {
    //     fprintf(stderr, "*** Choosing '%s' in the ambiguity tree %p (%s) using %p\n", 
    //             ast_print_node_type(ASTKind(ast_get_ambiguity(a, n))), a, ast_location(ast_get_ambiguity(a, n)), 
    //             ast_get_ambiguity(a, n));
    // }

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

static inline AST ast_list_head(const_AST list);
static inline const locus_t* ast_get_locus(const_AST a)
{
    if (a == NULL)
        return NULL;
    else if (ASTKind(a) != AST_NODE_LIST)
        return a->locus;
    else
        return ast_get_locus(
                ASTSon1(ast_list_head(a))
                );
}

static inline void ast_set_locus(AST a, const locus_t* locus)
{
    ERROR_CONDITION(ASTKind(a) == AST_NODE_LIST,
            "list nodes do not have locus", 0);
    a->locus = locus;
}

static inline const char *ast_get_filename(const_AST a)
{
    return locus_get_filename(ast_get_locus(a));
}

static inline int ast_node_size(void)
{
    return sizeof(struct AST_tag);
}

static inline struct nodecl_expr_info_tag* ast_get_expr_info(const_AST a)
{
    return a->expr_info;
}

static inline void ast_set_expr_info(AST a, struct nodecl_expr_info_tag* expr_info)
{
    a->expr_info = expr_info;
}

static inline const char* ast_node_type_name(node_t n)
{
    return ast_node_names[n];
}


MCXX_END_DECLS

#endif
