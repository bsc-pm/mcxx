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

typedef
struct nodecl_expr_info_tag
{
    char is_value_dependent:1;
    char is_type_dependent_expression:1;

    type_t* type_info;

    const_value_t* const_val;
    scope_entry_t* symbol;

    template_parameter_list_t* template_parameters;

    AST* placeholder;

    decl_context_t* decl_context;
} nodecl_expr_info_t;

// Nodecl expression routines. 
// These are implementation only
static nodecl_expr_info_t* nodecl_expr_get_expression_info_noalloc(AST expr)
{
    if (expr == NULL)
        return NULL;

    nodecl_expr_info_t* p = ast_get_expr_info(expr);
    return p;
}

#define NODECL_EXPR_GET_PTR(return_type, what, field_name) \
static return_type* nodecl_expr_get_##what(AST expr) \
{ \
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr); \
    return expr_info == NULL ? NULL : expr_info->field_name; \
}

// static type_t* nodecl_expr_get_type(AST expr)
NODECL_EXPR_GET_PTR(type_t, type, type_info)

// static scope_entry_t* nodecl_expr_get_symbol(AST expr)
NODECL_EXPR_GET_PTR(scope_entry_t, symbol, symbol)

// static const_value_t* nodecl_expr_get_constant(AST expr)
NODECL_EXPR_GET_PTR(const_value_t, constant, const_val)

// static template_parameter_list_t* nodecl_expr_get_template_parameters(AST expr)
NODECL_EXPR_GET_PTR(template_parameter_list_t, template_parameters, template_parameters)

// static AST* nodecl_expr_get_placeholder(AST expr)
NODECL_EXPR_GET_PTR(AST, placeholder, placeholder);

char nodecl_expr_is_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? 0 : (expr_info->const_val != NULL);
}

static nodecl_expr_info_t* nodecl_expr_get_expression_info(AST expr)
{
    nodecl_expr_info_t* p = ast_get_expr_info(expr);
    if (p == NULL)
    {
        p = xmalloc(sizeof(*p));
        p->is_value_dependent = 0;
        p->is_type_dependent_expression = 0;
        p->type_info = NULL;
        p->const_val = NULL;
        p->symbol = NULL;
        p->template_parameters = NULL;
        p->placeholder = NULL;
        p->decl_context = NULL;
        ast_set_expr_info(expr, p);
    }
    return p;
}

#define NODECL_EXPR_SET_PTR(type, what, field_name) \
static void nodecl_expr_set_##what(AST expr, type * datum) \
{ \
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(expr); \
    expr_info->field_name = datum; \
}

// static void nodecl_expr_set_symbol(AST expr, scope_entry_t* entry)
NODECL_EXPR_SET_PTR(scope_entry_t, symbol, symbol)

// static void nodecl_expr_set_type(AST expr, type_t* t)
NODECL_EXPR_SET_PTR(type_t, type, type_info)

// static void nodecl_expr_set_constant(AST expr, const_value_t* const_val)
NODECL_EXPR_SET_PTR(const_value_t, constant, const_val)
    
// static void nodecl_expr_set_template_parameters(AST expr, template_parameter_list_t* template_params)
NODECL_EXPR_SET_PTR(template_parameter_list_t, template_parameters, template_parameters)

// static void nodecl_expr_set_placeholder(AST expr, AST* template_params)
NODECL_EXPR_SET_PTR(AST, placeholder, placeholder);

// Public routines
nodecl_t nodecl_null(void)
{
    nodecl_t result = { NULL };
    return result;
}

char nodecl_is_null(nodecl_t t)
{
    return t.tree == NULL;
}

AST nodecl_get_ast(nodecl_t t)
{
    return t.tree;
}

const char* nodecl_get_text(nodecl_t n)
{
    return ASTText(n.tree);
}

void nodecl_set_text(nodecl_t n, const char *c)
{
    ast_set_text(n.tree, c);
}

type_t* nodecl_get_type(nodecl_t t)
{
    return nodecl_expr_get_type(t.tree);
}

void nodecl_set_type(nodecl_t t, type_t* type)
{
    nodecl_expr_set_type(t.tree, type);
}

// nodecl_t nodecl_shallow_copy(nodecl_t t)
// {
//    Implemented in cxx-nodecl-shallow-copy.c
// }

nodecl_t nodecl_duplicate(nodecl_t t)
{
    return _nodecl_wrap(ast_duplicate_one_node(nodecl_get_ast(t)));
}

char nodecl_is_constant(nodecl_t t)
{
    return nodecl_expr_is_constant(t.tree);
}

const_value_t* nodecl_get_constant(nodecl_t t)
{
    return nodecl_expr_get_constant(t.tree);
}

void nodecl_set_constant(nodecl_t t, const_value_t* cval)
{
    nodecl_expr_set_constant(t.tree, cval);
}

template_parameter_list_t* nodecl_get_template_parameters(nodecl_t n)
{
    return nodecl_expr_get_template_parameters(n.tree);
}

void nodecl_set_template_parameters(nodecl_t n, template_parameter_list_t* template_parameters)
{
    nodecl_expr_set_template_parameters(n.tree, template_parameters);
}

const locus_t* nodecl_get_locus(nodecl_t t)
{
    return ast_get_locus(t.tree);
}

const char* nodecl_get_filename(nodecl_t t)
{
    return ASTFileName(t.tree);
}

int nodecl_get_line(nodecl_t t)
{
    return ASTLine(t.tree);
}

const char* nodecl_locus_to_str(nodecl_t t)
{
    const char* result;

    uniquestr_sprintf(&result, "%s:%d", nodecl_get_filename(t), nodecl_get_line(t));

    return result;
}

nodecl_t nodecl_concat_lists(nodecl_t list1, nodecl_t list2)
{
    if (list1.tree == NULL)
        return list2;

    if (list2.tree == NULL)
        return list1;

    if (ASTType(list1.tree) == AST_NODE_LIST
            && ASTType(list2.tree) == AST_NODE_LIST)
    {
        nodecl_t result;
        result.tree = ast_list_concat(list1.tree, list2.tree);
        return result;
    }

    internal_error("Invalid trees when appending two nodecl_t", 0);
    nodecl_t result = { NULL };
    return result;
}
nodecl_t nodecl_append_to_list(nodecl_t list, nodecl_t element)
{
    if (element.tree == NULL)
    {
        return list;
    }
    if (list.tree == NULL)
    {
        nodecl_t result = { ASTListLeaf(element.tree) };
        return result;
    }
    else
    {
        nodecl_t result = { ASTList(list.tree, element.tree) };
        return result;
    }
}

nodecl_t nodecl_make_list_1(nodecl_t element0)
{
    return nodecl_append_to_list(nodecl_null(), element0);
}

scope_entry_t* nodecl_get_symbol(nodecl_t node)
{
    return nodecl_expr_get_symbol(node.tree);
}

void nodecl_set_symbol(nodecl_t node, scope_entry_t* entry)
{
    nodecl_expr_set_symbol(node.tree, entry);
}

nodecl_t _nodecl_wrap(AST a)
{
    nodecl_t n = { a };
    return n;
}

nodecl_t nodecl_get_child(nodecl_t n, int i)
{
    return _nodecl_wrap(ast_get_child(n.tree, i));
}

nodecl_t* nodecl_unpack_list(nodecl_t n, int *num_items)
{
    if (nodecl_is_null(n))
    {
        *num_items = 0;
        return NULL;
    }

    AST list = nodecl_get_ast(n);
    ERROR_CONDITION(ASTType(list) != AST_NODE_LIST, "Cannot unpack non-list node", 0);
    AST it;
    int num_elements = 0;
    for_each_element(list, it)
    {
        num_elements++;
    }

    nodecl_t* output = xmalloc(num_elements*sizeof(*output));

    num_elements = 0;
    for_each_element(list, it)
    {
        output[num_elements] = _nodecl_wrap(ASTSon1(it));
        num_elements++;
    }

    *num_items = num_elements;
    return output;
}

int nodecl_list_length(nodecl_t list)
{
    if (nodecl_is_null(list))
        return 0;

    ERROR_CONDITION(!nodecl_is_list(list), "Invalid list", 0);
    AST a = nodecl_get_ast(list);

    int n = 0;
    AST it = NULL;
    for_each_element(a, it)
    {
        n++;
    }

    return n;
}

nodecl_t nodecl_list_head(nodecl_t list)
{
    ERROR_CONDITION(nodecl_is_null(list), "Invalid list", 0);
    AST a = nodecl_get_ast(list);
    ERROR_CONDITION(ASTType(a) != AST_NODE_LIST, "Cannot get head of non list", 0);
    AST it;
    for_each_element(a, it)
    {
        AST head = ASTSon1(it);
        return _nodecl_wrap(head);
    }
    return nodecl_null();
}

node_t nodecl_get_kind(nodecl_t n)
{
    return ASTType(nodecl_get_ast(n));
}

char nodecl_is_list(nodecl_t n)
{
    return !nodecl_is_null(n) && nodecl_get_kind(n) == AST_NODE_LIST;
}

#if 0
char nodecl_is_in_list(nodecl_t n)
{
    return !nodecl_is_null(n) && nodecl_is_list(nodecl_get_parent(n));
}
#endif

void nodecl_free(nodecl_t n)
{
    ast_free(nodecl_get_ast(n));
}

char nodecl_expr_is_value_dependent(nodecl_t node)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    return expr_info == NULL ? 0 : expr_info->is_value_dependent;
}

void nodecl_expr_set_is_value_dependent(nodecl_t node, char is_value_dependent)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    if (expr_info == NULL)
    {
        if (is_value_dependent)
        {
            expr_info = nodecl_expr_get_expression_info(node.tree);
            expr_info->is_value_dependent = 1;
        }
    }
    else
    {
        expr_info->is_value_dependent = is_value_dependent;
    }
}

char nodecl_expr_is_type_dependent(nodecl_t node)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    return expr_info == NULL ? 0 : expr_info->is_type_dependent_expression;
}

void nodecl_expr_set_is_type_dependent(nodecl_t node, char is_type_dependent_expression)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    if (expr_info == NULL)
    {
        if (is_type_dependent_expression)
        {
            expr_info = nodecl_expr_get_expression_info(node.tree);
            expr_info->is_type_dependent_expression = 1;
        }
    }
    else
    {
        expr_info->is_type_dependent_expression = is_type_dependent_expression;
    }
}

char nodecl_is_err_expr(nodecl_t n)
{
    return nodecl_get_kind(n) == NODECL_ERR_EXPR;
}

nodecl_t nodecl_generic_make(node_t kind, const locus_t* location)
{
    return _nodecl_wrap(ASTLeaf(kind, location, NULL));
}

void nodecl_set_child(nodecl_t n, int nc, nodecl_t c)
{
    ast_set_child(nodecl_get_ast(n), nc, nodecl_get_ast(c));
}

void nodecl_set_locus(nodecl_t n, const locus_t* locus)
{
    ast_set_locus(n.tree, locus);
}

void nodecl_set_locus_as(nodecl_t n, nodecl_t loc)
{
    ast_set_locus(n.tree, nodecl_get_locus(loc));
}

decl_context_t nodecl_get_decl_context(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);

    nodecl_expr_info_t* p = ast_get_expr_info(nodecl_get_ast(n));

    ERROR_CONDITION(p == NULL || p->decl_context == NULL, "Invalid context", 0);

    return *(p->decl_context);
}

void nodecl_set_decl_context(nodecl_t n, decl_context_t decl_context)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);

    nodecl_expr_info_t* p = nodecl_expr_get_expression_info(n.tree);

    if (p->decl_context == NULL)
        p->decl_context = xmalloc(sizeof(*(p->decl_context)));

    *(p->decl_context) = decl_context;
}

nodecl_t nodecl_get_parent(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n), "Invalid node", 0);

    return _nodecl_wrap(ASTParent(nodecl_get_ast(n)));
}

static decl_context_t nodecl_retrieve_context_rec(nodecl_t n)
{
    if (nodecl_is_null(n))
    {
        return CURRENT_COMPILED_FILE->global_decl_context;
    }
    else if (nodecl_get_kind(n) == NODECL_CONTEXT
            || nodecl_get_kind(n) == NODECL_PRAGMA_CONTEXT)
    {
        return nodecl_get_decl_context(n);
    }
    else 
    {
        return nodecl_retrieve_context_rec(nodecl_get_parent(n));
    }
}

decl_context_t nodecl_retrieve_context(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n), "Invalid node", 0);

    return nodecl_retrieve_context_rec(n);
}

void nodecl_set_parent(nodecl_t node, nodecl_t parent)
{
    ast_set_parent(nodecl_get_ast(node), nodecl_get_ast(parent));
}

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
    xfree(buff);

    return result;
}

// Build from AST_NODECL_LITERAL
nodecl_t nodecl_make_from_ast_nodecl_literal(AST a)
{
    if (a == NULL)
    {
        return nodecl_null();
    }

    ERROR_CONDITION(ASTType(a) != AST_NODECL_LITERAL, "Invalid node", 0);

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
