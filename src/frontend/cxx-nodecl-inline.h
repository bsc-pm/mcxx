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



#ifndef CXX_NODECL_INLINE_H
#define CXX_NODECL_INLINE_H

#include "cxx-ast.h"
#include "string_utils.h"

MCXX_BEGIN_DECLS

struct nodecl_expr_info_tag
{
    char is_value_dependent:1;
    char is_type_dependent_expression:1;

    type_t* type_info;

    const_value_t* const_val;
    scope_entry_t* symbol;

    template_parameter_list_t* template_parameters;

    AST* placeholder;

    const decl_context_t* decl_context;
};

// Nodecl expression routines. 
static inline nodecl_expr_info_t* nodecl_expr_get_expression_info_noalloc(AST expr)
{
    if (expr == NULL)
        return NULL;

    nodecl_expr_info_t* p = ast_get_expr_info(expr);
    return p;
}

#define NODECL_EXPR_GET_PTR(return_type, what, field_name) \
static inline return_type* nodecl_expr_get_##what(AST expr) \
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

static inline char nodecl_expr_is_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? 0 : (expr_info->const_val != NULL);
}

static inline nodecl_expr_info_t* nodecl_expr_get_expression_info(AST expr)
{
    nodecl_expr_info_t* p = ast_get_expr_info(expr);
    if (p == NULL)
    {
        p = NEW(nodecl_expr_info_t);
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
static inline void nodecl_expr_set_##what(AST expr, type * datum) \
{ \
    nodecl_expr_info_t* expr_info; \
    if (datum == NULL) \
    { \
       expr_info = nodecl_expr_get_expression_info_noalloc(expr); \
       if (expr_info == NULL) \
         return; \
    } \
    else \
    { \
     expr_info = nodecl_expr_get_expression_info(expr); \
    } \
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
static inline nodecl_t nodecl_null(void)
{
    nodecl_t result = { NULL };
    return result;
}

static inline char nodecl_is_null(nodecl_t t)
{
    return t.tree == NULL;
}

static inline AST nodecl_get_ast(nodecl_t t)
{
    return t.tree;
}

static inline const char* nodecl_get_text(nodecl_t n)
{
    return ASTText(n.tree);
}

static inline void nodecl_set_text(nodecl_t n, const char *c)
{
    ast_set_text(n.tree, c);
}

static inline type_t* nodecl_get_type(nodecl_t t)
{
    return nodecl_expr_get_type(t.tree);
}

static inline void nodecl_set_type(nodecl_t t, type_t* type)
{
    nodecl_expr_set_type(t.tree, type);
}

static inline char nodecl_is_constant(nodecl_t t)
{
    return nodecl_expr_is_constant(t.tree);
}

static inline const_value_t* nodecl_get_constant(nodecl_t t)
{
    return nodecl_expr_get_constant(t.tree);
}

static inline void nodecl_set_constant(nodecl_t t, const_value_t* cval)
{
    nodecl_expr_set_constant(t.tree, cval);
}

static inline const locus_t* nodecl_get_locus(nodecl_t t)
{
    return ast_get_locus(t.tree);
}

static inline const char* nodecl_get_filename(nodecl_t t)
{
    return ast_get_filename(t.tree);
}

static inline unsigned int nodecl_get_line(nodecl_t t)
{
    return ast_get_line(t.tree);
}

static inline unsigned int nodecl_get_column(nodecl_t t)
{
    return ast_get_column(t.tree);
}

static inline const char* nodecl_locus_to_str(nodecl_t t)
{
    return locus_to_str(ast_get_locus(t.tree));
}

static inline template_parameter_list_t* nodecl_get_template_parameters(nodecl_t n)
{
    return nodecl_expr_get_template_parameters(n.tree);
}

static inline void nodecl_set_template_parameters(nodecl_t n, template_parameter_list_t* template_parameters)
{
    nodecl_expr_set_template_parameters(n.tree, template_parameters);
}

static inline nodecl_t nodecl_concat_lists(nodecl_t list1, nodecl_t list2)
{
    if (list1.tree == NULL)
        return list2;

    if (list2.tree == NULL)
        return list1;

    if (ASTKind(list1.tree) == AST_NODE_LIST
            && ASTKind(list2.tree) == AST_NODE_LIST)
    {
        nodecl_t result;
        result.tree = ast_list_concat(list1.tree, list2.tree);
        return result;
    }

    internal_error("Invalid trees when appending two nodecl_t", 0);
    nodecl_t result = { NULL };
    return result;
}

static inline nodecl_t nodecl_append_to_list(nodecl_t list, nodecl_t element)
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

static inline nodecl_t nodecl_make_list_1(nodecl_t element0)
{
    return nodecl_append_to_list(nodecl_null(), element0);
}

static inline scope_entry_t* nodecl_get_symbol(nodecl_t node)
{
    return nodecl_expr_get_symbol(node.tree);
}

static inline void nodecl_set_symbol(nodecl_t node, scope_entry_t* entry)
{
    nodecl_expr_set_symbol(node.tree, entry);
}

static inline nodecl_t nodecl_duplicate(nodecl_t t)
{
    return _nodecl_wrap(ast_duplicate_one_node(nodecl_get_ast(t)));
}

ALWAYS_INLINE static inline nodecl_t _nodecl_wrap(AST a)
{
    nodecl_t n = { a };
    return n;
}

ALWAYS_INLINE static inline nodecl_t nodecl_get_child(nodecl_t n, int i)
{
    return _nodecl_wrap(ast_get_child(n.tree, i));
}

static inline nodecl_t* nodecl_unpack_list(nodecl_t n, int *num_items)
{
    if (nodecl_is_null(n))
    {
        *num_items = 0;
        return NULL;
    }

    AST list = nodecl_get_ast(n);
    ERROR_CONDITION(ASTKind(list) != AST_NODE_LIST, "Cannot unpack non-list node", 0);
    AST it;
    int num_elements = 0;
    for_each_element(list, it)
    {
        num_elements++;
    }

    nodecl_t* output = NEW_VEC(nodecl_t, num_elements);

    num_elements = 0;
    for_each_element(list, it)
    {
        output[num_elements] = _nodecl_wrap(ASTSon1(it));
        num_elements++;
    }

    *num_items = num_elements;
    return output;
}

static inline int nodecl_list_length(nodecl_t list)
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

static inline nodecl_t nodecl_list_head(nodecl_t list)
{
    ERROR_CONDITION(nodecl_is_null(list), "Invalid list", 0);
    AST a = nodecl_get_ast(list);
    ERROR_CONDITION(ASTKind(a) != AST_NODE_LIST, "Cannot get head of non list", 0);
    AST it;
    for_each_element(a, it)
    {
        AST head = ASTSon1(it);
        return _nodecl_wrap(head);
    }
    return nodecl_null();
}

static inline node_t nodecl_get_kind(nodecl_t n)
{
    return ASTKind(nodecl_get_ast(n));
}

static inline char nodecl_is_list(nodecl_t n)
{
    return !nodecl_is_null(n) && nodecl_get_kind(n) == AST_NODE_LIST;
}

static inline char nodecl_is_list_or_null(nodecl_t n)
{
    return nodecl_is_null(n) || nodecl_get_kind(n) == AST_NODE_LIST;
}

static inline void nodecl_free(nodecl_t n)
{
    ast_free(nodecl_get_ast(n));
}

static inline char nodecl_expr_is_value_dependent(nodecl_t node)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    return expr_info == NULL ? 0 : expr_info->is_value_dependent;
}

static inline void nodecl_expr_set_is_value_dependent(nodecl_t node, char is_value_dependent)
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

static inline char nodecl_expr_is_type_dependent(nodecl_t node)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    return expr_info == NULL ? 0 : expr_info->is_type_dependent_expression;
}

static inline void nodecl_expr_set_is_type_dependent(nodecl_t node, char is_type_dependent_expression)
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

static inline char nodecl_is_err_expr(nodecl_t n)
{
    return nodecl_get_kind(n) == NODECL_ERR_EXPR;
}

static inline char nodecl_is_err_stmt(nodecl_t n)
{
    return nodecl_get_kind(n) == NODECL_ERR_STATEMENT;
}

static inline nodecl_t nodecl_generic_make(node_t kind, const locus_t* location)
{
    return _nodecl_wrap(ASTLeaf(kind, location, NULL));
}

static inline void nodecl_set_child(nodecl_t n, int nc, nodecl_t c)
{
    ast_set_child(nodecl_get_ast(n), nc, nodecl_get_ast(c));
}

static inline void nodecl_set_locus(nodecl_t n, const locus_t* locus)
{
    ast_set_locus(n.tree, locus);
}

static inline void nodecl_set_locus_as(nodecl_t n, nodecl_t loc)
{
    ast_set_locus(n.tree, nodecl_get_locus(loc));
}

static inline const decl_context_t* nodecl_get_decl_context(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);

    nodecl_expr_info_t* p = ast_get_expr_info(nodecl_get_ast(n));

    ERROR_CONDITION(p == NULL || p->decl_context == NULL, "Invalid context", 0);

    return p->decl_context;
}

static inline void nodecl_set_decl_context(nodecl_t n, const decl_context_t* decl_context)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);

    nodecl_expr_info_t* p = nodecl_expr_get_expression_info(n.tree);

    p->decl_context = decl_context;
}

static inline nodecl_t nodecl_get_parent(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n), "Invalid node", 0);

    return _nodecl_wrap(ASTParent(nodecl_get_ast(n)));
}

static inline const decl_context_t* nodecl_retrieve_context_rec(nodecl_t n)
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

static inline const decl_context_t* nodecl_retrieve_context(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n), "Invalid node", 0);

    return nodecl_retrieve_context_rec(n);
}

static inline void nodecl_set_parent(nodecl_t node, nodecl_t parent)
{
    ast_set_parent(nodecl_get_ast(node), nodecl_get_ast(parent));
}

MCXX_END_DECLS

#endif
