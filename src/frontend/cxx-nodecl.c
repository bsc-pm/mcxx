#include "cxx-nodecl.h"
#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include "cxx-attrnames.h"
#include <stdlib.h>
#include <string.h>

typedef
struct nodecl_expr_info_tag
{
    char is_lvalue:1;
    char is_value_dependent:1;
    int _reserved0;

    type_t* type_info;

    const_value_t* const_val;
    scope_entry_t* symbol;
} nodecl_expr_info_t;

// Nodecl expression routines. 
// These are implementation only
static nodecl_expr_info_t* nodecl_expr_get_expression_info_noalloc(AST expr)
{
    if (expr == NULL)
        return NULL;

    nodecl_expr_info_t* p = ASTAttrValueType(expr, LANG_EXPRESSION_INFO, nodecl_expr_info_t);
    return p;
}

static type_t* nodecl_expr_get_type(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? NULL : expr_info->type_info;
}

static scope_entry_t* nodecl_expr_get_symbol(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? NULL : expr_info->symbol;
}

char nodecl_expr_is_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? 0 : (expr_info->const_val != NULL);
}

static const_value_t* nodecl_expr_get_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? NULL : expr_info->const_val;
}

static char nodecl_expr_is_value_dependent(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? 0 : expr_info->is_value_dependent;
}


static nodecl_expr_info_t* nodecl_expr_get_expression_info(AST expr)
{
    nodecl_expr_info_t* p = ASTAttrValueType(expr, LANG_EXPRESSION_INFO, nodecl_expr_info_t);
    if (p == NULL)
    {
        p = calloc(1, sizeof(*p));
        ast_set_field(expr, LANG_EXPRESSION_INFO, p);
    }
    return p;
}

static void nodecl_expr_set_symbol(AST expr, scope_entry_t* entry)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(expr);
    expr_info->symbol = entry;
}

static void nodecl_expr_set_type(AST expr, type_t* t)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(expr);
    expr_info->type_info = t;
}

static void nodecl_expr_set_non_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(expr);
    expr_info->const_val = NULL;
}

static void nodecl_expr_set_constant(AST expr, const_value_t* const_val)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(expr);
    expr_info->const_val = const_val;
}

static void nodecl_expr_clear_computed_info(AST t)
{
    if (t == NULL)
        return;

    nodecl_expr_info_t* info = nodecl_expr_get_expression_info_noalloc(t);
    if (info != NULL)
    {
        memset(info, 0, sizeof(*info));
    }
    int i;
    for (i = 0; i < ASTNumChildren(t); i++)
    {
        nodecl_expr_clear_computed_info(ASTChild(t, i));
    }
}

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

const char* nodecl_get_text(nodecl_t t)
{
    return ASTText(t.tree);
}

type_t* nodecl_get_type(nodecl_t t)
{
    type_t* type = nodecl_expr_get_type(t.tree);

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
    return nodecl_expr_set_type(t.tree, type);
}

nodecl_t nodecl_copy(nodecl_t t)
{
    nodecl_t result = { ast_copy(t.tree) };
    return result;
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
    return nodecl_expr_get_symbol(node.tree);
}

void nodecl_set_symbol(nodecl_t node, scope_entry_t* entry)
{
    return nodecl_expr_set_symbol(node.tree, entry);
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

char nodecl_is_cxx_dependent_expr(nodecl_t n)
{
    return !nodecl_is_null(n) && (nodecl_get_kind(n) == NODECL_CXX_DEPENDENT_EXPR);
}

nodecl_t nodecl_wrap_cxx_dependent_expr(AST expression, decl_context_t decl_context)
{
    // Create a raw tree
    nodecl_t nodecl_raw = nodecl_make_cxx_dependent_expr(
            new_scope_symbol(decl_context),
            ASTFileName(expression), ASTLine(expression));
    // Note that we do not want this nodecl_raw to become the parent of expression
    // FIXME - This may create lots of copies
    ast_set_child(nodecl_get_ast(nodecl_raw), 0, ast_copy(expression));

    return nodecl_raw;
}

AST nodecl_unwrap_cxx_dependent_expr(nodecl_t n, decl_context_t* decl_context)
{
    ERROR_CONDITION(nodecl_get_kind(n) != NODECL_CXX_DEPENDENT_EXPR, "Invalid nodecl of kind", ast_print_node_type(nodecl_get_kind(n)));

    nodecl_t wrap = nodecl_get_child(n, 0);
    AST tree = nodecl_get_ast(wrap);

    scope_entry_t* entry = nodecl_get_symbol(n);
    ERROR_CONDITION(entry == NULL, "Invalid cxx dependent expr", 0);
    *decl_context = entry->decl_context;

    return tree;
}

void nodecl_free(nodecl_t n)
{
    ast_free(nodecl_get_ast(n));
}

char nodecl_expr_is_lvalue(nodecl_t expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr.tree);
    return expr_info == NULL ? 0 : expr_info->is_lvalue;
}

void nodecl_expr_set_is_lvalue(nodecl_t node, char is_lvalue)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info(node.tree);
    expr_info->is_lvalue = is_lvalue;
}

char nodecl_is_err_expr(nodecl_t n)
{
    return nodecl_get_kind(n) == NODECL_ERR_EXPR;
}
