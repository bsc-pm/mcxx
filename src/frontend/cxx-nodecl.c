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

#ifdef HAVE_CONFIG_H
  #include "config.h"
#endif
#ifdef HAVE_OPEN_MEMSTREAM
  // Needed, otherwise open_memstream is not declared
  #define _GNU_SOURCE
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cxx-nodecl.h"
#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include "cxx-codegen.h"

// nodecl_t nodecl_shallow_copy(nodecl_t t)
// {
//    Implemented in cxx-nodecl-shallow-copy.c
// }

#if 0
char nodecl_is_in_list(nodecl_t n)
{
    return !nodecl_is_null(n) && nodecl_is_list(nodecl_get_parent(n));
}
#endif


void nodecl_replace(nodecl_t old_node, nodecl_t new_node)
{
    ERROR_CONDITION(nodecl_is_null(old_node), "Old node cannot be null", 0);
    ERROR_CONDITION(nodecl_is_null(new_node), "New node cannot be null", 0);

    ERROR_CONDITION(!nodecl_is_list(old_node) && nodecl_is_list(new_node),
            "Replacing a non-list nodecl with a list nodecl is not allowed. ", 0);

    if (nodecl_is_list(old_node)
            && !nodecl_is_list(new_node))
    {
        new_node = nodecl_make_list_1(new_node);
    }

    ast_replace(old_node.tree, new_node.tree);

    // If the new node is a placeholder, transfer it to point to old_node
    AST *p = nodecl_get_placeholder(new_node);
    if (p != NULL)
    {
        *p = old_node.tree;
    }
}

#if 0
void nodecl_append_after_node(nodecl_t node, nodecl_t items)
{
    ERROR_CONDITION(!nodecl_is_in_list(node), "Node is not inside a list", 0);
    ERROR_CONDITION(nodecl_is_null(items), "Items cannot be NULL", 0);

    // If not a list create a singleton
    if (!nodecl_is_list(items))
    {
        items = nodecl_make_list_1(items);
    }

    // The parent list
    nodecl_t parent_list = nodecl_get_parent(node);
    // Previous items to 'node'
    nodecl_t previous_items = nodecl_get_child(node, 0);

    // Create a new list with all the previous items up to 'node'
    nodecl_t new_node_list = nodecl_append_to_list(previous_items, node);

    // Get the beginning of 'items'
    nodecl_t head_of_list = items;
    while (!nodecl_is_null(nodecl_get_child(head_of_list, 0)))
    {
        head_of_list = nodecl_get_child(head_of_list, 0);
    }

    // Add the new list before 'items'
    nodecl_set_child(head_of_list, 0, new_node_list);

    // Now replace the contents of the parent list with items list
    nodecl_replace(parent_list, items);
}

void nodecl_prepend_before_node(nodecl_t node, nodecl_t items)
{
    ERROR_CONDITION(!nodecl_is_in_list(node), "Node is not inside a list", 0);
    ERROR_CONDITION(nodecl_is_null(items), "Items cannot be NULL", 0);

    // If not a list create a singleton
    if (!nodecl_is_list(items))
    {
        items = nodecl_make_list_1(items);
    }

    // The parent list
    nodecl_t parent_list = nodecl_get_parent(node);
    nodecl_t previous = nodecl_get_child(parent_list, 0);

    // Get the beginning of 'items'
    nodecl_t head_of_list = items;
    while (!nodecl_is_null(nodecl_get_child(head_of_list, 0)))
    {
        head_of_list = nodecl_get_child(head_of_list, 0);
    }

    // Add the new list before 'items'
    nodecl_set_child(head_of_list, 0, previous);

    // Update the previous elements to items
    nodecl_set_child(parent_list, 0, items);
}
#endif

// REMOVE THIS FUNCTION
void nodecl_exchange(nodecl_t old_node, nodecl_t new_node)
{
    ERROR_CONDITION(nodecl_is_null(old_node), "Old node cannot be null", 0);
    ERROR_CONDITION(nodecl_is_null(new_node), "New node cannot be null", 0);

    // Do nothing
    if (nodecl_get_ast(old_node) == nodecl_get_ast(new_node))
        return;

    nodecl_t parent_of_old = nodecl_get_parent(old_node);

    ERROR_CONDITION(nodecl_is_null(parent_of_old), "Without parent, exchange is not possible", 0);

    int i = 0;
    for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
    {
        if (nodecl_get_ast(nodecl_get_child(parent_of_old, i)) == nodecl_get_ast(old_node))
        {
            nodecl_set_child(parent_of_old, i, new_node);
            nodecl_set_parent(old_node, nodecl_null());
            return;
        }
    }

    internal_error("Old node was not properly chained to its parent", 0);
}

static size_t hash_string(const char* str)
{
    size_t str_hash = 0;
    int c;

    if (str != NULL)
    {
        while ( (c = *str++) )   
            str_hash = c + (str_hash << 6) + (str_hash << 16) - str_hash;
    }

    return str_hash;
}

size_t nodecl_hash_table(nodecl_t key)
{
    size_t hash = 0;
    
    if (!nodecl_is_null(key))
    {
        // Actual hash
        if (!nodecl_is_list(key))
        {
            const char* kind = ast_print_node_type(nodecl_get_kind(key));
            scope_entry_t* s = nodecl_get_symbol(key);
            const char* sym = NULL; if (s != NULL) sym = s->symbol_name;
            const char* text = nodecl_get_text(key);
            const char* type = print_type_str(nodecl_get_type(key), CURRENT_COMPILED_FILE->global_decl_context);
            
            hash = hash_string(kind) + hash_string(sym) + hash_string(text) + hash_string(type);
        }
        
        // Hash for all children
        int i = 0;
        size_t child_hash;
        for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
        {
            child_hash = nodecl_hash_table(nodecl_get_child(key, i));
            hash += child_hash << (i * 2);
        }    
    }
    
    return hash;
}

// Placeholder
void nodecl_set_placeholder(nodecl_t n, AST* p)
{
    nodecl_expr_set_placeholder(n.tree, p);
}

AST* nodecl_get_placeholder(nodecl_t n)
{
    return nodecl_expr_get_placeholder(n.tree);
}

// Sourceify
static const char* nodecl_to_source(nodecl_t n);

const char* nodecl_stmt_to_source(nodecl_t n)
{
#define PRE "@NODECL-LITERAL-STMT@("
#define POST ")"

    const char* c = nodecl_to_source(n);
    int ITEMS = strlen(c) + strlen(PRE) + strlen(POST);
    char result[2 + ITEMS];

    snprintf(result, ITEMS + 1, "%s%s%s", PRE, c, POST);

#undef PRE
#undef POST

    return uniquestr(result);
}

const char* nodecl_expr_to_source(nodecl_t n)
{
#define PRE "@NODECL-LITERAL-EXPR@("
#define POST ")"

    const char* c = nodecl_to_source(n);
    int ITEMS = strlen(c) + strlen(PRE) + strlen(POST);
    char result[2 + ITEMS];

    snprintf(result, ITEMS + 1, "%s%s%s", PRE, c, POST);

#undef PRE
#undef POST

    return uniquestr(result);
}

static const char* nodecl_to_source(nodecl_t n)
{
    char *buff = NULL;
    size_t size = 0;
    FILE* string_stream = open_memstream(&buff, &size);

    if (!nodecl_is_null(n))
    {
        fprintf(string_stream, "%s", pack_pointer("ast", nodecl_get_ast(n)));
    }

    fclose(string_stream);

    const char* result = uniquestr(buff);
    DELETE(buff);

    return result;
}

// Build from AST_NODECL_LITERAL
nodecl_t nodecl_make_from_ast_nodecl_literal(AST a)
{
    if (a == NULL)
    {
        return nodecl_null();
    }

    ERROR_CONDITION(ASTKind(a) != AST_NODECL_LITERAL, "Invalid node", 0);

    AST string_literal_list = ASTSon0(a);
    AST it;

    nodecl_t result = nodecl_null();

    for_each_element(string_literal_list, it)
    {
        const char* attr_name = NULL;
        void *p = NULL;
        AST current_string_literal = ASTSon1(it);

        unpack_pointer(ASTText(current_string_literal), &attr_name, &p);

        if (strcmp(attr_name, "ast") == 0)
        {
            result = _nodecl_wrap((AST)p);
        }
        else
        {
            internal_error("Malformed nodecl literal. Unknown property '%s'\n", attr_name);
        }
    }

    return result;
}
