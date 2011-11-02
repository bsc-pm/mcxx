#include "cxx-nodecl.h"
#include "cxx-exprtype.h"
#include "cxx-utils.h"
#include "cxx-codegen.h"
#include <stdlib.h>
#include <string.h>

typedef
struct nodecl_expr_info_tag
{
    char is_lvalue:1;
    char is_value_dependent:1;
    char is_type_dependent_expression:1;
    int _reserved0;

    type_t* type_info;

    const_value_t* const_val;
    scope_entry_t* symbol;

    template_parameter_list_t* template_parameters;
} nodecl_expr_info_t;

#define LANG_EXPRESSION_INFO "lang.expression_info"

// Nodecl expression routines. 
// These are implementation only
static nodecl_expr_info_t* nodecl_expr_get_expression_info_noalloc(AST expr)
{
    if (expr == NULL)
        return NULL;

    nodecl_expr_info_t* p = (nodecl_expr_info_t*)ast_get_field(expr, LANG_EXPRESSION_INFO);
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

char nodecl_expr_is_constant(AST expr)
{
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(expr);
    return expr_info == NULL ? 0 : (expr_info->const_val != NULL);
}

static nodecl_expr_info_t* nodecl_expr_get_expression_info(AST expr)
{
    nodecl_expr_info_t* p = (nodecl_expr_info_t*)ast_get_field(expr, LANG_EXPRESSION_INFO);
    if (p == NULL)
    {
        p = calloc(1, sizeof(*p));
        ast_set_field(expr, LANG_EXPRESSION_INFO, p);
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

template_parameter_list_t* nodecl_get_template_parameters(nodecl_t n)
{
    return nodecl_expr_get_template_parameters(n.tree);
}

void nodecl_set_template_parameters(nodecl_t n, template_parameter_list_t* template_parameters)
{
    nodecl_expr_set_template_parameters(n.tree, template_parameters);
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
    nodecl_expr_info_t* expr_info = nodecl_expr_get_expression_info_noalloc(node.tree);
    if (expr_info == NULL)
    {
        if (is_lvalue)
        {
            expr_info = nodecl_expr_get_expression_info(node.tree);
            expr_info->is_lvalue = 1;
        }
    }
    else
    {
        expr_info->is_lvalue = is_lvalue;
    }
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

nodecl_t nodecl_generic_make(node_t kind, const char* filename, int line)
{
    return _nodecl_wrap(ASTLeaf(kind, filename, line, NULL));
}

void nodecl_set_child(nodecl_t n, int nc, nodecl_t c)
{
    ast_set_child(nodecl_get_ast(n), nc, nodecl_get_ast(c));
}

#define LANG_DECL_CONTEXT "lang.decl_context_t"

decl_context_t nodecl_get_decl_context(nodecl_t n)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);
    decl_context_t* p = (decl_context_t*)ast_get_field(nodecl_get_ast(n), LANG_DECL_CONTEXT);

    ERROR_CONDITION(p == NULL, "Invalid context", 0);

    return *p;
}

void nodecl_set_decl_context(nodecl_t n, decl_context_t decl_context)
{
    ERROR_CONDITION(nodecl_is_null(n) || 
            (nodecl_get_kind(n) != NODECL_CONTEXT
            && nodecl_get_kind(n) != NODECL_PRAGMA_CONTEXT),
            "This is not a context node", 0);

    decl_context_t* p = (decl_context_t*)ast_get_field(nodecl_get_ast(n), LANG_DECL_CONTEXT);

    if (p == NULL)
    {
        p = calloc(1, sizeof(*p));

        *p = decl_context;

        ast_set_field(nodecl_get_ast(n), LANG_DECL_CONTEXT, p);
    }
    else
    {
        *p = decl_context;
    }
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

static void nodecl_set_parent(nodecl_t node, nodecl_t parent)
{
    ast_set_parent(nodecl_get_ast(node), nodecl_get_ast(parent));
}

void nodecl_exchange(nodecl_t old_node, nodecl_t new_node)
{
    ERROR_CONDITION(nodecl_is_null(old_node), "Old node cannot be null", 0);
    ERROR_CONDITION(nodecl_is_null(new_node), "Old node cannot be null", 0);

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
//             char* key_s = codegen_to_str(key);
//             printf("kind = '%s',\t sym = '%s',\t text = '%s',\t type = '%s',\t hash <'%s' , '%d'>\n", kind, sym, text, type, key_s, hash);         
  
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
