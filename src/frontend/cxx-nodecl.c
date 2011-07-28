#include "cxx-nodecl.h"
#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include <stdlib.h>

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

const char* nodecl_get_text(nodecl_t t)
{
    return ASTText(t.tree);
}

type_t* nodecl_get_type(nodecl_t t)
{
    type_t* type = expression_get_type(t.tree);

    // Very usual case
    if (type == NULL
            && nodecl_get_kind(t) == NODECL_SYMBOL)
    {
        type = nodecl_get_symbol(t)->type_information;
    }

    return type;
}

void nodecl_set_type(nodecl_t t, type_t* type)
{
    return expression_set_type(t.tree, type);
}

nodecl_t nodecl_copy(nodecl_t t)
{
    nodecl_t result = { ast_copy(t.tree) };
    return result;
}

char nodecl_is_constant(nodecl_t t)
{
    return expression_is_constant(t.tree);
}

const_value_t* nodecl_get_constant(nodecl_t t)
{
    return expression_get_constant(t.tree);
}

void nodecl_set_constant(nodecl_t t, const_value_t* cval)
{
    expression_set_constant(t.tree, cval);
}

const char* nodecl_get_filename(nodecl_t t)
{
    return ASTFileName(t.tree);
}

int nodecl_get_line(nodecl_t t)
{
    return ASTLine(t.tree);
}

const char* nodecl_get_locus(nodecl_t t)
{
    char c[256];
    snprintf(c, 255, "%s:%d", nodecl_get_filename(t), nodecl_get_line(t));
    c[255] = '\0';
    return uniquestr(c);
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
    return expression_get_symbol(node.tree);
}

void nodecl_set_symbol(nodecl_t node, scope_entry_t* entry)
{
    return expression_set_symbol(node.tree, entry);
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

    nodecl_t* output = calloc(num_elements, sizeof(*output));

    num_elements = 0;
    for_each_element(list, it)
    {
        output[num_elements] = _nodecl_wrap(ASTSon1(it));
        num_elements++;
    }

    *num_items = num_elements;
    return output;
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

char nodecl_is_cxx_dependent_expr(nodecl_t n)
{
    return !nodecl_is_null(n) && (nodecl_get_kind(n) == NODECL_CXX_DEPENDENT_EXPR);
}

nodecl_t nodecl_wrap_cxx_dependent_expr(AST expression)
{
    // Create a raw tree
    nodecl_t nodecl_raw = nodecl_make_cxx_dependent_expr(ASTFileName(expression), ASTLine(expression));
    // Note that we do not want this nodecl_raw to become the parent of expression
    ast_set_child_but_parent(nodecl_get_ast(nodecl_raw), 0, expression);

    return nodecl_raw;
}

AST nodecl_unwrap_cxx_dependent_expr(nodecl_t n)
{
    ERROR_CONDITION(nodecl_get_kind(n) != NODECL_CXX_DEPENDENT_EXPR, "Invalid nodecl of kind", ast_print_node_type(nodecl_get_kind(n)));

    return nodecl_get_ast(nodecl_get_child(n, 0));
}
