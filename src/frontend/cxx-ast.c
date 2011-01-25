/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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
#include <string.h>

#include "cxx-ast.h"

#include "cxx-lexer.h"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scopelink.h"

// We need this to fix nodes
#include "cxx-tltype.h"
#include "cxx-attrnames.h"

// Definition of the type
struct AST_tag
{
    // Node type (1024 different node types)
    node_t node_type:10; 

    // This is a bitmap for the sons
    unsigned int bitmap_sons:MAX_AST_CHILDREN;

    // Number of ambiguities of this node
    int num_ambig;

    // Parent node
    struct AST_tag* parent; 

    // Node locus
    unsigned int line; 
    const char* filename;

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

    // Extensible information (created lazily)
    extensible_struct_t* extended_data;
};

// Define the extensible schema of AST's
extensible_schema_t ast_extensible_schema = EMPTY_EXTENSIBLE_SCHEMA;

static int count_bitmap(unsigned int bitmap)
{
    int i;
    int s = 0;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        s += bitmap & 1;
        bitmap = bitmap >> 1;
    }

    return s;
}

static int bitmap_to_index(unsigned int bitmap, int num)
{
    ERROR_CONDITION(((1 << num) & bitmap) == 0,
            "Invalid bitmap!", 0);

    int i;
    int s = 0;
    for (i = 0; i < num; i++)
    {
        s += bitmap & 1;
        bitmap = bitmap >> 1;
    }

    return s;
}


static int ast_son_num_to_son_index(const_AST a, int num_son)
{
    return bitmap_to_index(a->bitmap_sons, num_son);
}

static char ast_has_son(const_AST a, int son)
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

AST ast_make(node_t type, int num_children UNUSED_PARAMETER, 
        AST child0, AST child1, AST child2, AST child3, 
        const char* file, unsigned int line, const char *text)
{
    AST result = counted_calloc(1, sizeof(*result), &_bytes_due_to_astmake);

    result->parent = NULL;

    result->node_type = type;

    result->line = line;
    result->filename = file;

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
        result->text = uniquestr(text);
    }

    return result;
}

AST ast_get_parent(const_AST a)
{
    return a->parent;
}

unsigned int ast_get_line(const_AST a)
{
    return a->line;
}

void ast_set_line(AST a, int line)
{
    a->line = line;
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

    a->children = counted_calloc(sizeof(*a->children), 
            (count_bitmap(a->bitmap_sons)), 
            &_bytes_due_to_astmake);

    // Now for every old son, update the new children
    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
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

    // Now free the old children (if any)
    if (old_children != NULL)
        free(old_children);

    // Count this free
    _bytes_due_to_astmake -= (count_bitmap(old_bitmap) * sizeof(AST));
}

void ast_set_child(AST a, int num_child, AST new_child)
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

        new_child->parent = a;
    }
}

int ast_num_children(const_AST a)
{
    // We return the maximum
    int max = 0;
    int i;
    unsigned int bitmap = a->bitmap_sons;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        if ((1 << i) & bitmap)
        {
            max = i + 1;
        }
    }

    return max;
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

static void ast_copy_one_node(AST dest, AST orig)
{
    *dest = *orig;
    dest->bitmap_sons = 0;
    dest->children = 0;
}

static char ast_is_parent_rec(const_AST parent, const_AST current)
{
    if (current == NULL)
        return 0;

    if (parent == current)
        return 1;
    else
    {
        return ast_is_parent_rec(parent, ASTParent(current));
    }
}

static char ast_is_parent(const_AST parent, const_AST a)
{
    return ast_is_parent_rec(parent, a);
}

// This function returns the number of son of current tree, -1 if node does not have parent
static int ast_get_number_of_son(const_AST a)
{
    if (ASTParent(a) == NULL)
        return -1;

    const_AST parent = ASTParent(a);

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        if (ASTChild(parent, i) == a)
            return i;
    }
    internal_error("invalid chaining of trees", 0);
}

static void ast_fix_one_ast_field_rec(
        AST new, 
        const_AST orig, 
        const char *field_name, 
        // AST pointed_tree,
        /* Recursion info */
        const_AST current,
        int path_length,
        int *path)
{

    if (current == orig)
    {
        // Now retrace all the path using 'new' as the source
        int i;
        AST iter = new;
        // Note that since we built the list backwards, we have to traverse it
        // backwards
        for (i = path_length - 1; i >= 0; i--)
        {
            iter = ASTChild(iter, path[i]);
            if (iter == NULL)
                internal_error("unreachable code", 0);
        }

        // Now update node 'new' with 'iter'
        ASTAttrSetValueType(new, field_name, tl_type_t, tl_ast(iter));
    }
    else if (current == NULL)
    {
        internal_error("unreachable code", 0);
    }
    else
    {
        // New path length
        int new_path_length = path_length + 1;

        // Compute new path (note, path is backwards built)
        int new_path[new_path_length];

        // !!! We rely on memcpy not doing anything when size copied is 0
        memcpy(new_path, path, path_length * sizeof(new_path[0]));

        new_path[new_path_length - 1] = ast_get_number_of_son(current);

        // New current node
        AST new_current = ASTParent(current);

        ast_fix_one_ast_field_rec(
                new,
                orig,
                field_name,
                // Recursion info
                new_current,
                new_path_length,
                new_path);
    }
}

static void ast_fix_one_ast_field(
        AST new, 
        const_AST orig, 
        const char *field_name, 
        const_AST pointed_tree)
{
    ast_fix_one_ast_field_rec(
            new,
            orig,
            field_name,
            pointed_tree,
            0,
            NULL);
}

// Note that fixing must be performed later
static void ast_copy_extended_data(AST new, const_AST orig)
{
    if (orig->extended_data == NULL)
    {
        new->extended_data = NULL;
        return;
    }

    new->extended_data = counted_calloc(1, sizeof(*(new->extended_data)), &_bytes_due_to_astmake);
    extensible_struct_init(new->extended_data, &ast_extensible_schema);

    int num_fields = extensible_struct_get_num_fields(&ast_extensible_schema,
            orig->extended_data);
    int i;
    for (i = 0; i < num_fields; i++)
    {
        const char* field_name = extensible_struct_get_field_num(&ast_extensible_schema,
                orig->extended_data, i);

        char is_found = 0;
        void* data = extensible_struct_get_field_pointer_lazy(&ast_extensible_schema,
                orig->extended_data, field_name, &is_found);
        if (data != NULL)
        {
            tl_type_t* tl_data = (tl_type_t*)data;
            ASTAttrSetValueType(new, field_name, tl_type_t, (*tl_data));
        }
    }
}

// Use this to fix extended data pointing to other ASTs
static void ast_fix_extended_data(AST new, const_AST orig)
{
    if (orig->extended_data == NULL)
        return;

    // First get all TL_AST in 'orig' that point to its childrens
    int num_fields = extensible_struct_get_num_fields(&ast_extensible_schema,
            orig->extended_data);

    typedef
    struct tl_data_index_tag
    {
        const char* field_name;
        AST ast;
    } tl_data_index_t;

    tl_data_index_t tl_data_index[num_fields];
    memset(tl_data_index, 0, sizeof(tl_data_index));
    int num_ast_fields = 0;

    int i;
    for (i = 0; i < num_fields; i++)
    {
        const char* field_name = extensible_struct_get_field_num(&ast_extensible_schema,
                orig->extended_data, i);

        char is_found = 0;
        void* data = extensible_struct_get_field_pointer_lazy(&ast_extensible_schema,
                orig->extended_data, field_name, &is_found);
        tl_type_t* tl_data = (tl_type_t*)data;

        if (data != NULL
                && tl_data->kind == TL_AST
                && tl_data->data._ast != NULL)
        {
            if (ast_is_parent(/* potential parent */ orig, /* node */ tl_data->data._ast))
            {
                tl_data_index[num_ast_fields].field_name = field_name;
                tl_data_index[num_ast_fields].ast = tl_data->data._ast;
                num_ast_fields++;

            }
        }
    }

    // num_ast_fields contains the number of fixable trees
    for (i = 0; i < num_ast_fields; i++)
    {
        ast_fix_one_ast_field(new, orig, tl_data_index[i].field_name, tl_data_index[i].ast);
    }
}

AST ast_copy(const_AST a)
{
    if (a == NULL)
        return NULL;

    AST result = counted_calloc(1, sizeof(*result), &_bytes_due_to_astmake);

    ast_copy_one_node(result, (AST)a);

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
        result->ambig = counted_calloc(a->num_ambig, sizeof(*(result->ambig)), &_bytes_due_to_astmake);
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy(a->ambig[i]);
        }
    }

    if (a->text != NULL)
    {
        result->text = uniquestr(a->text);
    }

    result->parent = NULL;

    // Adjust TL_AST extended fields stored in a with the proper son
    ast_copy_extended_data(result, a);
    ast_fix_extended_data(result, a);

    return result;
}

void ast_clear_extended_data(AST a)
{
    if (a != NULL)
    {
        a->extended_data = NULL;
        int i;
        for (i = 0; i < MAX_AST_CHILDREN; i++)
        {
            ast_clear_extended_data(ast_get_child(a, i));
        }
    }
}

AST ast_copy_clearing_extended_data(const_AST a)
{
    AST result = ast_copy(a);
    ast_clear_extended_data(result);

    return result;
}

AST ast_copy_for_instantiation(const_AST a)
{
    if (a == NULL)
        return NULL;

    AST result = counted_calloc(1, sizeof(*result), &_bytes_due_to_instantiation);

    ast_copy_one_node(result, (AST)a);

    // Children are special and will be properly copied by ast_set_child
    result->children = NULL;

    // Clear extended data since we will want to recheck this code
    result->extended_data = NULL;

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
        result->ambig = counted_calloc(a->num_ambig, sizeof(*(result->ambig)), &_bytes_due_to_instantiation);
        for (i = 0; i < a->num_ambig; i++)
        {
            result->ambig[i] = ast_copy_for_instantiation(a->ambig[i]);
        }
    }

    if (a->text != NULL)
    {
        result->text = uniquestr(a->text);
    }

    result->parent = NULL;

    ast_fix_extended_data(result, a);

    // Copy template parameter info (it is vital for proper resolution)
    if (is_template_parameter_name((AST)a))
    {
         tl_type_t* tl_data = ASTAttrValue((AST)a, LANG_TEMPLATE_PARAMETER_NAME_SYMBOL);

         ASTAttrSetValueType(result, LANG_IS_TEMPLATE_PARAMETER_NAME, tl_type_t, tl_bool(1));
         ASTAttrSetValueType(result, LANG_TEMPLATE_PARAMETER_NAME_SYMBOL, tl_type_t, *tl_data);
    }

    return result;
}

AST ast_list_leaf(AST a)
{
    AST result = ast_make(AST_NODE_LIST, 2, NULL, a, NULL, NULL, 
            ast_get_filename(a), ast_get_line(a), ast_get_text(a));

    return result;
}

AST ast_list(AST list, AST last_elem)
{
    AST a = ast_make(AST_NODE_LIST, 2, list, last_elem, NULL, NULL, 
            ast_get_filename(list), ast_get_line(last_elem), ast_get_text(last_elem));

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

const char* ast_location(const_AST a)
{
    if (a == NULL)
        return "";

    char result[256];

    if (a->filename == NULL)
    {
        snprintf(result, 255, "<unknown file>:%u", a->line);
    }
    else
    {
        snprintf(result, 255, "%s:%u", a->filename, a->line);
    }

    result[255] = '\0';

    return uniquestr(result);
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
        AST result = ASTLeaf(AST_AMBIGUITY, NULL, 0, NULL);

        result->num_ambig = 2;
        result->ambig = counted_calloc(sizeof(*(result->ambig)), result->num_ambig, &_bytes_due_to_astmake);
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

static void ast_set_ambiguity(AST a, int num, AST child)
{
    a->ambig[num] = child;
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
            for (i = 0; i < MAX_AST_CHILDREN; i++)
            {
                ast_free(ast_get_child(a, i));
            }
        }

        // This will uncover dangling references
        free(a->children);
        _bytes_due_to_astmake -= sizeof(*(a->children)) * count_bitmap(a->bitmap_sons);
        memset(a, 0, sizeof(*a));
        free(a);

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

    int i;
    for (i = 0; i < ast_get_num_ambiguities(a); i++)
    {
        if (i != n)
        {
            ast_free(ast_get_ambiguity(a, i));
            ast_set_ambiguity(a, i, NULL);
        }
    }
    
    if (!ASTCheck(a))
    {
        internal_error("*** INCONSISTENT TREE DETECTED IN ABOUT TO BE DISAMBIGUATED TREE %p ***\n", a);
    }

    // This will work, trust me :)
    ast_replace(a, ast_get_ambiguity(a, n));

    // Correctly relink to the parent
    ast_set_parent(a, parent);

    for (i = 0; i < MAX_AST_CHILDREN; i++)
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

const char *ast_get_filename(const_AST a)
{
    return a->filename;
}

void ast_set_filename(AST a, const char* str)
{
    a->filename = str;
}

extensible_struct_t* ast_get_extensible_struct(AST a)
{
    if (a->extended_data == NULL)
    {
        a->extended_data = counted_calloc(1, sizeof(*(a->extended_data)), &_bytes_due_to_astmake);
        extensible_struct_init(a->extended_data, &ast_extensible_schema);
    }
    return a->extended_data;
}

/*
 * This functions give an additional support to scopelink
 *
 * This function assumes that no ambiguities are found
 */
static AST ast_copy_with_scope_link_rec(AST a, scope_link_t* sl)
{
    if (a == NULL)
        return NULL;

    AST result = counted_calloc(1, sizeof(*result), &_bytes_due_to_astmake);

    ast_copy_one_node(result, a);

    // Update the scope_link
    decl_context_t decl_context;
    if (scope_link_direct_get_scope(sl, a, &decl_context))
    {
        scope_link_set(sl, result, decl_context);
    }

    int i;
    for (i = 0; i < MAX_AST_CHILDREN; i++)
    {
        ast_set_child(result, i, ast_copy_with_scope_link_rec(ASTChild(a, i), sl));
    }

    if (ASTText(a) != NULL)
    {
        ast_set_text(result, uniquestr(ASTText(a)));
    }
    ast_set_parent(result, NULL);

    ast_copy_extended_data(result, a);
    ast_fix_extended_data(result, a);

    return result;
}


AST ast_copy_with_scope_link(AST a, scope_link_t* sl)
{
    // This scope must be always available
    AST result = ast_copy_with_scope_link_rec(a, sl);

    decl_context_t decl_context = scope_link_get_decl_context(sl, a);
    scope_link_set(sl, result, decl_context);

    return result;
}

int ast_node_size(void)
{
    return sizeof(struct AST_tag);
}

