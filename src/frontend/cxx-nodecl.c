#include "cxx-nodecl.h"
#include "cxx-nodecl-checker.h"
#include "cxx-utils.h"
#include "cxx-buildscope.h"
#include "cxx-attrnames.h"
#include "cxx-tltype.h"
#include "cxx-exprtype.h"

static void c_simplify_tree_decl(AST a, AST *out);
static void c_simplify_tree_stmt(AST a, AST *out);
static void c_simplify_tree_expr(AST a, AST *out);

static void c_simplify_tree_init_decl(AST a, AST *out)
{
    if (a == NULL)
    {
        *out = NULL;
        return;
    }

    switch (ASTType(a))
    {
        case AST_EQUAL_INITIALIZER:
            {
                c_simplify_tree_expr(ASTSon0(a), out);
                break;
            }
        default:
            internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(a)));
    }
}

// This function returns a list (or NULL)
static void c_simplify_tree_simple_decl(AST a, AST *out)
{
    AST decl_specifier_seq = ASTSon0(a);
    AST init_declarator_list = ASTSon1(a);

    // AST nontype_seq_0 = ASTSon0(decl_specifier_seq);
    AST type_specifier = ASTSon1(decl_specifier_seq);
    // AST nontype_seq_1 = ASTSon2(decl_specifier_seq);

    AST class_member_decl_list = NULL;
    if (ASTType(type_specifier) == AST_CLASS_SPECIFIER)
    {
        AST member_spec_list = ASTSon1(type_specifier);
        AST it;
        for_each_element(member_spec_list, it)
        {
            AST member_spec = ASTSon1(it);
            AST new_member_spec = NULL;

            c_simplify_tree_decl(member_spec, &new_member_spec);

            if (new_member_spec != NULL)
            {
                member_spec_list = ASTList(member_spec_list, new_member_spec);
            }
        }
    }

    AST new_init_declarator_list = NULL;
    if (init_declarator_list != NULL)
    {
        AST it;
        for_each_element(init_declarator_list, it)
        {
            AST init_declarator = ASTSon1(it);

            AST declarator = ASTSon0(init_declarator);
            AST initializer = ASTSon1(init_declarator);

            AST declarator_name = get_declarator_name(declarator,
                    scope_link_get_decl_context(CURRENT_COMPILED_FILE->scope_link, declarator));

            ERROR_CONDITION((declarator_name == NULL), "Invalid declarator name\n", 0);

            tl_type_t* tl_data = ASTAttrValueType(declarator_name, LANG_DECLARED_SYMBOL, tl_type_t);

            ERROR_CONDITION (tl_data == NULL 
                    || tl_data->kind != TL_SYMBOL
                    || tl_data->data._entry == NULL, 
                    "Invalid declarator", 0);

            scope_entry_t* entry = tl_data->data._entry;
            if (entry->kind != SK_VARIABLE)
                continue;

            AST new_initializer = NULL;
            c_simplify_tree_init_decl(initializer, &new_initializer);

            AST new_decl_name = 
                    ast_copy_with_scope_link(declarator_name, CURRENT_COMPILED_FILE->scope_link);
            expression_set_symbol(new_decl_name, entry);
            AST new_init_declarator = ASTMake2(AST_OBJECT_INIT, 
                    new_decl_name,
                    new_initializer, 
                    ASTFileName(init_declarator), ASTLine(init_declarator), NULL);

            if (entry->expression_value != NULL)
            {
                AST new_out = NULL;
                c_simplify_tree_expr(entry->expression_value, &new_out);
                entry->expression_value = new_out;
            }

            new_init_declarator_list = ASTList(new_init_declarator_list, new_init_declarator);
        }
    }

    *out = ast_list_concat(class_member_decl_list, new_init_declarator_list);
}

static void c_simplify_tree_namespace_def(AST a, AST *out)
{
    AST decl_seq = ASTSon1(a);
    c_simplify_tree_decl(decl_seq, out);
}

static void c_simplify_tree_function_def(AST a, AST *out)
{
    AST function_declarator = ASTSon1(a);

    AST declarator_name = get_declarator_name(function_declarator,
            scope_link_get_decl_context(CURRENT_COMPILED_FILE->scope_link, function_declarator));

    tl_type_t* tl_data = ASTAttrValueType(declarator_name, LANG_DECLARED_SYMBOL, tl_type_t);

    ERROR_CONDITION (tl_data == NULL 
            || tl_data->kind != TL_SYMBOL
            || tl_data->data._entry == NULL, 
            "Invalid declarator", 0);

    scope_entry_t* entry = tl_data->data._entry;
    expression_set_symbol(declarator_name, entry);

    ERROR_CONDITION(declarator_name == NULL, "Invalid tree", 0);

    AST function_body = ASTSon3(a);

    AST statement = ASTSon0(function_body);

    AST new_stmt = NULL;
    c_simplify_tree_stmt(statement, &new_stmt);

    *out = ASTListLeaf(
            ASTMake3(AST_FUNCTION_CODE, 
                declarator_name, 
                new_stmt != NULL ? ASTListLeaf(new_stmt) : NULL, 
                NULL,
                ASTFileName(a), ASTLine(a), NULL)
            );
}

static void c_simplify_tree_expr(AST a, AST *out)
{
    switch (ASTType(a))
    {
        case AST_EXPRESSION :
        case AST_CONSTANT_EXPRESSION :
        case AST_PARENTHESIZED_EXPRESSION :
        case AST_EQUAL_INITIALIZER:
            // GCC extensions
        case AST_GCC_EXTENSION_EXPR : 
            {
                c_simplify_tree_expr(ASTSon0(a), out);
                break;
            }
        case AST_POINTER_CLASS_MEMBER_ACCESS:
            {
                AST lhs = NULL;
                c_simplify_tree_expr(ASTSon0(a), &lhs);

                type_t * lhs_type = expression_get_type(lhs);
                lhs = ASTMake1(AST_DERREFERENCE, lhs, ASTFileName(lhs), ASTLine(lhs), NULL);
                if (is_pointer_type(lhs_type))
                {
                    expression_set_type(lhs, pointer_type_get_pointee_type(lhs_type));
                }
                else
                {
                    internal_error("Unexpected type %s", print_declarator(lhs_type));
                }

                AST rhs = NULL;
                c_simplify_tree_expr(ASTSon0(a), &rhs);

                *out = ASTMake2(AST_CLASS_MEMBER_ACCESS, lhs, rhs, ASTFileName(lhs), ASTLine(lhs), NULL);
                expression_set_type(*out, expression_get_type(rhs));
                break;
            }
        default:
            {
                AST children[MCXX_MAX_AST_CHILDREN] = { 0 };
                int i;
                for (i = 0; i < MCXX_MAX_AST_CHILDREN; i++)
                {
                    if (ast_get_child(a, i) != NULL)
                    {
                        c_simplify_tree_expr(ast_get_child(a, i), &children[i]);
                    }
                }

                *out = ASTMake4(ast_get_type(a), children[0], children[1], children[2], children[3], 
                        ASTFileName(a),
                        ASTLine(a),
                        ASTText(a));

                expression_set_type(*out, expression_get_type(a));
                expression_set_symbol(*out, expression_get_symbol(a));
                break;
            }
    }
}

static void c_simplify_tree_condition(AST a UNUSED_PARAMETER, AST *out UNUSED_PARAMETER)
{
    internal_error("Not yet implemented", 0);
}

static void c_simplify_tree_loop_control(AST a UNUSED_PARAMETER, AST *out UNUSED_PARAMETER)
{
    internal_error("Not yet implemented", 0);
}

static void c_simplify_tree_stmt(AST a, AST *out)
{
    switch (ASTType(a))
    {
        case AST_EXPRESSION_STATEMENT:
            {
                AST new_expr = NULL;
                c_simplify_tree_expr(ASTSon0(a), &new_expr);
                *out = ASTMake1(AST_EXPRESSION_STATEMENT, new_expr, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_DECLARATION_STATEMENT : 
            {
                c_simplify_tree_decl(ASTSon0(a), out);
                break;
            }
        case AST_COMPOUND_STATEMENT : 
            {
                AST new_inner_seq = NULL;
                AST statement_seq = ASTSon0(a);
                AST it;
                if (statement_seq != NULL)
                {
                    for_each_element(statement_seq, it)
                    {
                        AST stmt = ASTSon1(it);

                        AST new_stmt = NULL;
                        c_simplify_tree_stmt(stmt, &new_stmt);

                        if (new_stmt != NULL)
                        {
                            if (ASTType(new_stmt) != AST_NODE_LIST)
                            {
                            new_inner_seq = ASTList(new_inner_seq, new_stmt);
                            }
                            else
                            {
                                new_inner_seq = ast_list_concat(new_inner_seq, new_stmt);
                            }
                        }
                    }
                }

                *out = ASTMake1(AST_COMPOUND_STATEMENT, new_inner_seq, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_DO_STATEMENT : 
            {
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon0(a), &new_stmt);
                AST new_expr = NULL;
                c_simplify_tree_expr(ASTSon1(a), &new_expr);
                *out = ASTMake2(AST_DO_STATEMENT, new_stmt, new_expr, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_WHILE_STATEMENT : 
            {
                AST new_cond = NULL;
                c_simplify_tree_condition(ASTSon0(a), &new_cond);
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake2(AST_WHILE_STATEMENT, new_cond, new_stmt, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_FOR_STATEMENT : 
            {
                AST new_loop_control = NULL;
                c_simplify_tree_loop_control(ASTSon0(a), &new_loop_control);
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake3(AST_FOR_STATEMENT, new_loop_control, new_stmt, NULL, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_IF_ELSE_STATEMENT : 
            {
                AST new_cond = NULL;
                c_simplify_tree_condition(ASTSon0(a), &new_cond);
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                AST new_else_stmt = NULL;
                if (ASTSon2(a) != NULL)
                {
                    c_simplify_tree_stmt(ASTSon2(a), &new_else_stmt);
                }

                *out = ASTMake4(AST_IF_ELSE_STATEMENT, 
                        new_cond, new_stmt, new_else_stmt, NULL, 
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_LABELED_STATEMENT : 
            {
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake2(AST_LABELED_STATEMENT, 
                        ast_copy_with_scope_link(ASTSon0(a), CURRENT_COMPILED_FILE->scope_link), 
                        new_stmt,
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_DEFAULT_STATEMENT : 
            {
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon0(a), &new_stmt);

                *out = ASTMake1(AST_DEFAULT_STATEMENT, new_stmt, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_CASE_STATEMENT : 
            {
                AST new_expr = NULL;
                c_simplify_tree_expr(ASTSon0(a), &new_expr);
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake2(AST_CASE_STATEMENT, new_expr, new_stmt, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_RETURN_STATEMENT : 
            {
                if (ASTSon0(a) == NULL)
                {
                    *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                }
                else
                {
                    // FIXME this expression might be a braced-init-list
                    AST new_expr = NULL;
                    c_simplify_tree_expr(ASTSon0(a), &new_expr);

                    *out = ASTMake1(AST_RETURN_STATEMENT, new_expr, ASTFileName(a), ASTLine(a), NULL);
                }
                break;
            }
        case AST_TRY_BLOCK : 
            {
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon0(a), &new_stmt);

                AST new_handler_seq = NULL;
                AST handler_seq = ASTSon1(a);
                AST it;
                for_each_element(handler_seq, it)
                {
                    AST handler = ASTSon1(it);

                    AST new_handler_stmt = NULL;
                    c_simplify_tree_stmt(ASTSon1(handler), &new_handler_stmt);

                    AST new_handler = ASTMake2(AST_CATCH_HANDLER, 
                            ast_copy_with_scope_link(ASTSon0(handler), CURRENT_COMPILED_FILE->scope_link),
                            new_handler_stmt,
                            ASTFileName(handler), ASTLine(handler), NULL);
                    new_handler_seq = ASTList(new_handler_seq, new_handler);
                }

                *out = ASTMake2(AST_TRY_BLOCK, new_stmt, new_handler_seq, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_SWITCH_STATEMENT : 
            {
                AST new_cond = NULL;
                c_simplify_tree_condition(ASTSon0(a), &new_cond);
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake3(AST_SWITCH_STATEMENT, new_cond, new_stmt, NULL, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_EMPTY_STATEMENT : 
            {
                // Do nothing
                *out = NULL;
                break;
            }
        case AST_BREAK_STATEMENT : 
            {
                *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                break;
            }
        case AST_CONTINUE_STATEMENT : 
            {
                *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                break;
            }
        case AST_GOTO_STATEMENT : 
            {
                *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT : 
            {
                AST new_stmt = NULL;
                c_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, 
                        ast_copy_with_scope_link(ASTSon0(a), CURRENT_COMPILED_FILE->scope_link),
                        new_stmt,
                        ASTFileName(a),
                        ASTLine(a),
                        NULL);
                break;
            }
        case AST_PRAGMA_CUSTOM_DIRECTIVE : 
            {
                *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                break;
            }
        case AST_CUSTOM_CONSTRUCT_STATEMENT : 
        case AST_UPC_NOTIFY : 
        case AST_UPC_WAIT : 
        case AST_UPC_BARRIER : 
        case AST_UPC_FENCE : 
        case AST_UPC_FORALL : 
            {
                internal_error("Unsupported trees\n", 0);
            }
        default:
            internal_error("Invalid tree '%s'\n", ast_print_node_type(ASTType(a)));
    }
}

// This function should return a list (or NULL)
static void c_simplify_tree_decl(AST a, AST *out)
{
    switch (ASTType(a))
    {
        case AST_SIMPLE_DECLARATION:
            {
                c_simplify_tree_simple_decl(a, out);
                break;
            }
        case AST_NAMESPACE_DEFINITION :
            {
                c_simplify_tree_namespace_def(a, out);
                break;
            }
        case AST_FUNCTION_DEFINITION :
            {
                c_simplify_tree_function_def(a, out);
                break;
            }
        case AST_MEMBER_DECLARATION :
            {
                break;
            }
        case AST_LINKAGE_SPEC :
        case AST_LINKAGE_SPEC_DECL :
        case AST_NAMESPACE_ALIAS :
        case AST_DELETED_FUNCTION_DEFINITION :
        case AST_DEFAULTED_FUNCTION_DEFINITION :
        case AST_EXPORT_TEMPLATE_DECLARATION :
        case AST_TEMPLATE_DECLARATION :
        case AST_EXPLICIT_INSTANTIATION :
        case AST_EXPLICIT_SPECIALIZATION :
        case AST_USING_NAMESPACE_DIRECTIVE:
        case AST_USING_DECLARATION :
        case AST_USING_DECLARATION_TYPENAME :
        case AST_STATIC_ASSERT:
        case AST_EMPTY_DECL :
        case AST_ASM_DEFINITION :
        case AST_UNKNOWN_PRAGMA :
        case AST_PRAGMA_CUSTOM_DIRECTIVE :
        case AST_PRAGMA_CUSTOM_CONSTRUCT: 
        case AST_GCC_EXTENSION :
        case AST_GCC_ASM_DEFINITION :
        case AST_GCC_USING_NAMESPACE_DIRECTIVE:
        case AST_GCC_NAMESPACE_DEFINITION :
        case AST_PP_COMMENT :
        case AST_PP_TOKEN :
        case AST_VERBATIM :
            {
                internal_error("not yet implemented %s\n", ast_print_node_type(ASTType(a)));
            }
        case AST_MEMBER_ACCESS_SPEC:
            {
                // Do nothing
                break;
            }
        default:
            internal_error("Unexpected node '%s'\n", ast_print_node_type(ASTType(a)));
    }
}

void c_simplify_tree_translation_unit(AST a, AST *out)
{
    ERROR_CONDITION(ASTType(a) != AST_TRANSLATION_UNIT, "Invalid tree", 0);

    AST decl_sequence_list = ASTSon0(a);

    AST result = NULL;

    if (decl_sequence_list != NULL)
    {
        AST it;
        for_each_element(decl_sequence_list, it)
        {
            AST new_decl = NULL;
            c_simplify_tree_decl(ASTSon1(it), &new_decl);

            if (new_decl != NULL)
            {
                result = ast_list_concat(result, new_decl);
            }
        }
    }
    
    result = ASTMake1(AST_NODECL_TOP_LEVEL, result, 
            result != NULL ? ASTFileName(result) : NULL, 
            result != NULL ? ASTLine(result) : 0,
            NULL);

    nodecl_check_tree(result);

    *out = result;
}
