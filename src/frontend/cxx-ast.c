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
/** 
  Abstract Syntax Tree
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-ast.h"

#include "cxx-lexer.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scopelink.h"

// Definition of the type
struct AST_tag
{
    // Node type
    node_t node_type; 

    // Number of children
    int num_children; 

    // Parent node
    struct AST_tag* parent; 

    // The children of this tree (except for AST_AMBIGUITY)
    struct AST_tag* children[MAX_AST_CHILDREN];

    // Node locus
    int line; 
    const char* filename;

    // Textual information linked to the node
    // normally the symbol or the literal
    const char* text; 

    // When type == AST_AMBIGUITY, all intepretations are here
    int num_ambig;
    struct AST_tag** ambig;

    // For nodes holding some kind of expression
    // this should be the related type
    struct type_tag* expr_type;
    // Only meaningful if expr_type != NULL
    char expr_is_lvalue;

    // Extensible information
    extensible_struct_t* extended_data;
};

// Define the extensible schema of AST's
extensible_schema_t ast_extensible_schema;


AST ast_make(node_t type, int num_children, 
        AST child0, AST child1, AST child2, AST child3, 
        int line, const char *text)
{
    AST result = calloc(1, sizeof(*result));

    result->parent = NULL;

    result->node_type = type;
    result->num_children = num_children;

    result->line = line;
    result->filename = scanning_now.current_filename;

    result->extended_data = calloc(1, sizeof(*(result->extended_data)));
    extensible_struct_init(result->extended_data, &ast_extensible_schema);

#define ADD_SON(n) \
    ast_set_child(result, n, child##n);

    ADD_SON(0);
    ADD_SON(1);
    ADD_SON(2);
    ADD_SON(3);
#undef ADD_SON

    result->text = NULL;
    if (text != NULL)
    {
        result->text = strdup(text);
    }

    return result;
}

node_t ast_get_type(const_AST a)
{
    return a->node_type;
}

AST ast_get_parent(const_AST a)
{
    return a->parent;
}

int ast_get_line(const_AST a)
{
    return a->line;
}

const char* ast_get_text(const_AST a)
{
    return a->text;
}

void ast_set_text(AST a, const char* str)
{
    a->text = strdup(str);
}

AST ast_get_child(const_AST a, int num_child)
{
    return a->children[num_child];
}

void ast_set_parent(AST a, AST parent)
{
    a->parent = parent;
}

void ast_set_child(AST a, int num_child, AST new_children)
{
    a->children[num_child] = new_children;
    if (new_children != NULL)
    {
        new_children->parent = a;
    }
}

AST *ast_child_ref(AST a, int num_child)
{
    return &(a->children[num_child]);
}

AST *ast_parent_ref(AST a)
{
    return &(a->parent);
}

int ast_num_children(const_AST a)
{
    return a->num_children;
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
        for (i = 0; i < MAX_AST_CHILDREN; i++)
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

        if (ast_get_type(node) == AST_AMBIGUITY)
        {
            for (i = 0; i < node->num_ambig; i++)
            {
                check &= ast_check(node->ambig[i]);
            }
        }
    }
    return check;
}

AST ast_copy(const_AST a)
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
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        ast_set_child(result, i, ast_copy(ast_get_child(a, i)));
    }

    // Duplicate ambiguous trees too
    if (a->node_type == AST_AMBIGUITY && 
            (a->num_ambig > 0))
    {
        result->num_ambig = a->num_ambig;
        result->ambig = calloc(a->num_ambig, sizeof(*(result->ambig)));
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy(a->ambig[i]);
        }
    }

    if (a->text != NULL)
    {
        result->text = strdup(a->text);
    }

    result->parent = NULL;

    return result;
}

AST ast_copy_for_instantiation(const_AST a)
{
    if (a == NULL)
        return NULL;

    AST result = calloc(1, sizeof(*result));

    // Copy everything by value
    *result = *a;

    // Remove dependent type information
    if (result->expr_type != NULL
            && is_dependent_expr_type(result->expr_type))
    {
        result->expr_type = NULL;
        result->expr_is_lvalue = 0;
    }

    result->extended_data = calloc(1, sizeof(*(result->extended_data)));
    extensible_struct_init(result->extended_data, &ast_extensible_schema);

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        ast_set_child(result, i, ast_copy_for_instantiation(ast_get_child(a, i)));
    }

    // Duplicate ambiguous trees too
    if (ASTType(a) == AST_AMBIGUITY && 
            (a->num_ambig > 0))
    {
        result->num_ambig = a->num_ambig;
        result->ambig = calloc(a->num_ambig, sizeof(*(result->ambig)));
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy_for_instantiation(a->ambig[i]);
        }
    }

    if (a->text != NULL)
    {
        result->text = strdup(a->text);
    }

    result->parent = NULL;

    return result;
}

AST ast_list_leaf(AST a)
{
    AST result = ast_make(AST_NODE_LIST, 2, NULL, a, NULL, NULL, 
            ast_get_line(a), ast_get_text(a));

    return result;
}

AST ast_list(AST list, AST last_elem)
{
    AST a = ast_make(AST_NODE_LIST, 2, list, last_elem, NULL, NULL, 
            ast_get_line(last_elem), ast_get_text(last_elem));

    return a;
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

    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        if (!ast_equal(ast_get_child(ast1, i), ast_get_child(ast2, i)))
            return 0;
    }
    return 1;
}

char* ast_location(const_AST a)
{
    if (a == NULL)
        return "";

    char* result = calloc(256, sizeof(char));

    if (a->filename == NULL)
    {
        snprintf(result, 255, "<unknown file>:%d", a->line);
    }
    else
    {
        snprintf(result, 255, "%s:%d", a->filename, a->line);
    }

    result[255] = '\0';

    return result;
}

// Trees are copied
AST ast_make_ambiguous(AST son0, AST son1)
{
    if (ASTType(son0) == AST_AMBIGUITY)
    {
        if (ASTType(son1) == AST_AMBIGUITY)
        {
            int original_son0 = son0->num_ambig;

            son0->num_ambig += son1->num_ambig;
            son0->ambig = (AST*) realloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);

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
            son0->ambig = (AST*) realloc(son0->ambig, sizeof(*(son0->ambig)) * son0->num_ambig);
            son0->ambig[son0->num_ambig-1] = ast_copy(son1);

            return son0;
        }
    }
    else if (ASTType(son1) == AST_AMBIGUITY)
    {
        son1->num_ambig++;
        son1->ambig = (AST*) realloc(son1->ambig, sizeof(*(son1->ambig)) * son1->num_ambig);
        son1->ambig[son1->num_ambig-1] = ast_copy(son0);

        return son1;
    }
    else
    {
        AST result = ASTLeaf(AST_AMBIGUITY, 0, NULL);

        result->num_ambig = 2;
        result->ambig = calloc(sizeof(*(result->ambig)), result->num_ambig);
        result->ambig[0] = ast_copy(son0);
        result->ambig[1] = ast_copy(son1);
        result->line = son0->line;
        result->filename = son0->filename;

        return result;
    }
}

int ast_get_num_ambiguities(const_AST a)
{
    return a->num_ambig;
}

AST ast_get_ambiguity(const_AST a, int num)
{
    return a->ambig[num];
}

void ast_replace(AST dest, const_AST src)
{
    *dest = *src;
}

static void ast_free(AST a)
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
            for (i = 0; i < MAX_AST_CHILDREN; i++)
            {
                ast_free(ast_get_child(a, i));
            }
        }

        // This will uncover dangling references
        memset(a, 0, sizeof(*a));

        free(a);
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
    // int num_children = get_children_num(parent, a);

    // if (num_children < 0)
    // {
    //  internal_error("Children not found in the parent!\n", 0);
    // }

    DEBUG_CODE()
    {
        fprintf(stderr, "*** Choosing '%s' in the ambiguity tree %p (%s) using %p\n", 
                ast_print_node_type(ASTType(ast_get_ambiguity(a, n))), a, ast_location(ast_get_ambiguity(a, n)), 
                ast_get_ambiguity(a, n));
    }

    if (!ASTCheck(ast_get_ambiguity(a, n)))
    {
        internal_error("*** INCONSISTENT TREE DETECTED IN AMBIGUITY TREE %d *** %p\n", n, ast_get_ambiguity(a, n));
    }

    int i;
    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        if (i != n)
        {
            ast_free(ast_get_ambiguity(a, i));
        }
    }
    
    // This will work, trust me :)
    ast_replace(a, ast_get_ambiguity(a, n));

    // Correctly relink to the parent
    ast_set_parent(a, parent);

    for (i = 0; i < ASTNumChildren(a); i++)
    {
        if (ASTChild(a, i) != NULL)
        {
            AST child = ASTChild(a, i);

            ast_set_parent(child, a);
        }
    }

    if (!ASTCheck(a))
    {
        internal_error("*** INCONSISTENT TREE DETECTED IN DISAMBIGUATED TREE %p ***\n", a);
    }
}

type_t** ast_expression_type_ref(AST a)
{
    return &(a->expr_type);
}

char *ast_expression_is_lvalue_ref(AST a)
{
    return &(a->expr_is_lvalue);
}

const char *ast_get_filename(const_AST a)
{
    return a->filename;
}

void ast_set_filename(AST a, const char* str)
{
    a->filename = str;
}

char ast_get_expression_is_lvalue(const_AST a)
{
    return a->expr_is_lvalue;
}

void ast_set_expression_is_lvalue(AST a, char c)
{
    a->expr_is_lvalue = c;
}


struct type_tag* ast_get_expression_type(const_AST a)
{
    return a->expr_type;
}

void ast_set_expression_type(AST a, struct type_tag* type)
{
    a->expr_type = type;
}

extensible_struct_t* ast_get_extensible_struct(const_AST a)
{
    return a->extended_data;
}

/*
 * This functions give an additional support to scopelink
 *
 * This function assumes that no ambiguities are found
 */
static AST ast_copy_with_scope_link_rec(AST a, scope_link_t* orig, scope_link_t* new_sl)
{
    if (a == NULL)
        return NULL;

    AST result = calloc(1, sizeof(*result));

    // extensible_struct_t orig_extended_data = result->extended_data;

    // Update the scope_link
    decl_context_t decl_context;
    if (scope_link_direct_get_scope(orig, a, &decl_context))
    {
        scope_link_set(new_sl, result, decl_context);
    }

    // Copy everything by value
    *result = *a;

    int i;
    for (i = 0; i < ASTNumChildren(result); i++)
    {
        ast_set_child(result, i, ast_copy_with_scope_link_rec(ASTChild(a, i), orig, new_sl));
    }

    if (ASTText(a) != NULL)
    {
        ast_set_text(result, strdup(ASTText(a)));
    }
    ast_set_parent(result, NULL);

    return result;
}


AST ast_copy_with_scope_link(AST a, scope_link_t* orig, scope_link_t* new_sl)
{
    // This scope must be always available
    AST result = ast_copy_with_scope_link_rec(a, orig, new_sl);

    decl_context_t decl_context = scope_link_get_decl_context(orig, a);
    scope_link_set(new_sl, result, decl_context);

    return result;
}
