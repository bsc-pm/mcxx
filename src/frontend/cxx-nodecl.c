#include "cxx-nodecl.h"
#include "cxx-exprtype.h"
#include "cxx-utils.h"

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
    return expression_get_type(t.tree);
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

char nodecl_is_value_dependent(nodecl_t t)
{
    return expression_is_value_dependent(t.tree);
}

void nodecl_set_is_value_dependent(nodecl_t t, char is_value_dependent)
{
    return expression_set_is_value_dependent(t.tree, is_value_dependent);
}

const char* nodecl_get_filename(nodecl_t t)
{
    return ASTFileName(t.tree);
}

int nodecl_get_line(nodecl_t t)
{
    return ASTLine(t.tree);
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

nodecl_t _nodecl_wrap(AST a)
{
    nodecl_t n = { a };
    return n;
}

nodecl_t nodecl_get_children(nodecl_t n, int i)
{
    return _nodecl_wrap(ast_get_child(n.tree, i));
}
