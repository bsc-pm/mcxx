#include "fortran03-nodecl.h"
#include "cxx-utils.h"

static void fortran_simplify_tree_program_unit(AST a, AST *out);

#if 0
static void fortran_simplify_tree_expression(AST a, AST* out)
{
    // Nothing to simplify in Fortran
    *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
}
#endif

static void fortran_simplify_tree_stmt(AST a, AST *out)
{
    // FIXME: Think ways of improving this
    switch (ASTType(a))
    {
        case AST_ALLOCATE_STATEMENT:
        case AST_ARITHMETIC_IF_STATEMENT:
        case AST_ASSIGNED_GOTO_STATEMENT:
        case AST_BREAK_STATEMENT:
        case AST_CLOSE_STATEMENT:
        case AST_COMPUTED_GOTO_STATEMENT:
        case AST_CONTINUE_STATEMENT:
        case AST_DEALLOCATE_STATEMENT:
        case AST_EMPTY_STATEMENT:
        case AST_EXPRESSION_STATEMENT:
        case AST_GOTO_STATEMENT:
        case AST_IO_STATEMENT:
        case AST_LABEL_ASSIGN_STATEMENT:
        case AST_NULLIFY_STATEMENT:
        case AST_OPEN_STATEMENT:
        case AST_PRINT_STATEMENT:
        case AST_READ_STATEMENT:
        case AST_RETURN_STATEMENT:
        case AST_STOP_STATEMENT:
        case AST_PAUSE_STATEMENT:
        case AST_WRITE_STATEMENT:
        case AST_PRAGMA_CUSTOM_DIRECTIVE:
            {
                // Simple copy
                *out = ast_copy_with_scope_link(a, CURRENT_COMPILED_FILE->scope_link);
                break;
            }
        case AST_BLOCK_CONSTRUCT:
            {
                AST block = ASTSon1(a);
                AST new_block = NULL;
                AST it;
                for_each_element(block, it)
                {
                    AST stmt = ASTSon1(it);
                    AST new_stmt = NULL;

                    fortran_simplify_tree_stmt(stmt, &new_stmt);

                    if (new_stmt != NULL)
                    {
                        new_block = ASTList(new_block, new_stmt);
                    }
                }
                *out = ASTMake1(AST_COMPOUND_STATEMENT, new_block, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_COMPOUND_STATEMENT:
            {
                AST list = ASTSon0(a);
                AST new_list = NULL;
                AST it;
                for_each_element(list, it)
                {
                    AST stmt = ASTSon1(it);
                    AST new_stmt = NULL;

                    fortran_simplify_tree_stmt(stmt, &new_stmt);

                    if (new_stmt != NULL)
                    {
                        new_list = ASTList(new_list, new_stmt);
                    }
                }
                *out = ASTMake1(AST_COMPOUND_STATEMENT, new_list, ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_LABELED_STATEMENT:
            {
                AST label = ASTSon0(a);
                AST stmt = ASTSon1(a);

                AST new_stmt = NULL;
                fortran_simplify_tree_stmt(stmt, &new_stmt);

                if (new_stmt != NULL)
                {
                    *out = ASTMake2(AST_LABELED_STATEMENT, 
                            ast_copy_with_scope_link(label, CURRENT_COMPILED_FILE->scope_link),
                            new_stmt,
                            ASTFileName(a), ASTLine(a), NULL);
                }
                break;
            }
        case AST_SWITCH_STATEMENT:
            {
                AST block = ASTSon1(a);
                AST new_block = NULL;

                fortran_simplify_tree_stmt(block, &new_block);

                *out = ASTMake2(AST_SWITCH_STATEMENT,
                        ast_copy_with_scope_link(ASTSon0(a), CURRENT_COMPILED_FILE->scope_link),
                        new_block, 
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_CASE_STATEMENT:
            {
                AST expr = ASTSon0(a);

                AST stmt = ASTSon1(a);
                AST new_stmt = NULL;
                fortran_simplify_tree_stmt(stmt, &new_stmt);

                *out = ASTMake2(AST_CASE_STATEMENT, 
                        ast_copy_with_scope_link(expr, CURRENT_COMPILED_FILE->scope_link),
                        new_stmt,
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_DEFAULT_STATEMENT:
            {
                AST stmt = ASTSon0(a);
                AST new_stmt = NULL;
                fortran_simplify_tree_stmt(stmt, &new_stmt);

                *out = ASTMake1(AST_DEFAULT_STATEMENT,
                        new_stmt,
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_FOR_STATEMENT:
            {
                AST loop_control = ASTSon0(a);
                AST block = ASTSon1(a);
                AST new_block = NULL;

                fortran_simplify_tree_stmt(block, &new_block);

                *out = ASTMake2(AST_FOR_STATEMENT, 
                        loop_control,
                        new_block,
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_IF_ELSE_STATEMENT:
            {
                AST expr = ASTSon0(a);
                AST stmt = ASTSon1(a);
                AST new_stmt = NULL;
                fortran_simplify_tree_stmt(stmt, &new_stmt);

                AST else_stmt = ASTSon2(a);
                AST new_else_stmt = NULL;
                if (else_stmt != NULL)
                {
                    fortran_simplify_tree_stmt(else_stmt, &new_else_stmt);
                }

                *out = ASTMake4(AST_IF_ELSE_STATEMENT,
                        ast_copy_with_scope_link(expr, CURRENT_COMPILED_FILE->scope_link),
                        new_stmt, new_else_stmt, NULL, 
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_WHILE_STATEMENT:
            {
                AST expr = ASTSon0(a);
                AST block = ASTSon1(a);

                AST new_block = NULL;

                fortran_simplify_tree_stmt(block, &new_block);

                *out = ASTMake2(AST_WHILE_STATEMENT,
                        ast_copy_with_scope_link(expr, CURRENT_COMPILED_FILE->scope_link),
                        new_block,
                        ASTFileName(a), ASTLine(a), NULL);
                break;
            }
        case AST_PRAGMA_CUSTOM_CONSTRUCT:
            {
                AST new_stmt = NULL;
                fortran_simplify_tree_stmt(ASTSon1(a), &new_stmt);

                *out = ASTMake2(AST_PRAGMA_CUSTOM_CONSTRUCT, 
                        ast_copy_with_scope_link(ASTSon0(a), CURRENT_COMPILED_FILE->scope_link),
                        new_stmt,
                        ASTFileName(a),
                        ASTLine(a),
                        NULL);
                break;
            }
        case AST_ACCESS_STATEMENT:
        case AST_ALLOCATABLE_STATEMENT:
        case AST_BIND_STATEMENT:
        case AST_CODIMENSION_STATEMENT:
        case AST_COMMON_STATEMENT:
        case AST_DATA_STATEMENT:
        case AST_DECLARATION_STATEMENT:
        case AST_DERIVED_TYPE_DEF:
        case AST_DIMENSION_STATEMENT:
        case AST_EQUIVALENCE_STATEMENT:
        case AST_EXTERNAL_STATEMENT:
        case AST_FORMAT_STATEMENT:
        case AST_IMPLICIT_STATEMENT:
        case AST_INTENT_STATEMENT:
        case AST_INTERFACE_BLOCK:
        case AST_INTRINSIC_STATEMENT:
        case AST_NAMELIST_STATEMENT:
        case AST_OPTIONAL_STATEMENT:
        case AST_PARAMETER_STATEMENT:
        case AST_POINTER_STATEMENT:
        case AST_PROCEDURE_DECL_STATEMENT:
        case AST_PROTECTED_STATEMENT:
        case AST_SAVE_STATEMENT:
        case AST_STATEMENT_FUNCTION_STATEMENT:
        case AST_TARGET_STATEMENT:
        case AST_UNKNOWN_PRAGMA:
        case AST_USE_STATEMENT:
        case AST_VALUE_STATEMENT:
        case AST_VOLATILE_STATEMENT:
            {
                // Do nothing
                break;
            }
        case AST_LOCK_STATEMENT:
        case AST_ALL_STOP_STATEMENT:
        case AST_ASSOCIATE_CONSTRUCT:
        case AST_ASYNCHRONOUS_STATEMENT:
        case AST_CRITICAL_CONSTRUCT:
        case AST_ENTRY_STATEMENT:
        case AST_ENUM_DEF:
        case AST_FORALL_CONSTRUCT:
        case AST_FORALL_STATEMENT:
        case AST_IMPORT_STATEMENT:
        case AST_SELECT_TYPE_CONSTRUCT:
        case AST_UNLOCK_STATEMENT:
        case AST_WAIT_STATEMENT:
        case AST_SYNC_ALL_STATEMENT:
        case AST_SYNC_IMAGES_STATEMENT:
        case AST_SYNC_MEMORY_STATEMENT:
        case AST_WHERE_CONSTRUCT:
        case AST_WHERE_STATEMENT:
            {
                internal_error("Not supported", 0);
                break;
            }
        default:
            {
                internal_error("Unexpected tre '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

static void fortran_simplify_tree_body_program_unit(AST program_body, 
        AST program_unit_name,
        AST *out)
{
    ERROR_CONDITION(ASTType(program_body) != AST_BODY_PROGRAM_UNIT, 
            "Invalid program unit body", 0);

    AST top_level_block = ASTSon0(program_body);
    AST internal_subprograms = ASTSon1(program_body);
    AST new_body = NULL;

    // There are subtle differences here that we have to handle beforehand
    if (ASTType(top_level_block) == AST_NODE_LIST)
    {
        AST it;
        for_each_element(top_level_block, it)
        {
            AST stmt = ASTSon1(it);
            AST new_stmt = NULL;

            fortran_simplify_tree_stmt(stmt, &new_stmt);
            if (new_stmt != NULL)
            {
                new_body = ASTList(new_body, new_stmt);
            }
        }
    }
    else if (ASTType(top_level_block) == AST_COMPOUND_STATEMENT)
    {
        fortran_simplify_tree_stmt(top_level_block, &new_body);
    }
    else
    {
        internal_error("Invalid tree '%s'\n", ast_print_node_type(ASTType(top_level_block)));
    }

    AST new_internal_subprograms = NULL;
    if (internal_subprograms != NULL)
    {
        AST it;
        for_each_element(internal_subprograms, it)
        {
            AST subprogram = ASTSon1(it);

            AST new_subprogram = NULL;
            fortran_simplify_tree_program_unit(subprogram, &new_subprogram);

            new_internal_subprograms = ASTList(new_internal_subprograms, new_subprogram);
        }
    }

    *out = ASTMake3(AST_FUNCTION_CODE, program_unit_name, new_body, new_internal_subprograms, 
            ASTFileName(program_unit_name), ASTLine(program_unit_name), NULL);
}

// This function returns a list
static void fortran_simplify_tree_program_unit(AST a, AST *out)
{
    switch (ASTType(a))
    {
        case AST_MAIN_PROGRAM_UNIT:
            {
                AST program_stmt = ASTSon0(a);
                AST name = NULL; 
                if (program_stmt != NULL)
                {
                    name = ASTSon1(program_stmt);
                }
                if (name == NULL)
                {
                    name = ASTLeaf(AST_SYMBOL, ASTFileName(a), ASTLine(a), uniquestr("__MAIN__"));
                }

                AST program_body = ASTSon1(a);

                fortran_simplify_tree_body_program_unit(program_body, name, out);
                break;
            }
        case AST_FUNCTION_PROGRAM_UNIT:
            {
                AST function_stmt = ASTSon0(a);
                AST name = ASTSon2(function_stmt);
                AST program_body = ASTSon1(a);

                fortran_simplify_tree_body_program_unit(program_body, name, out);
                break;
            }
        case AST_SUBROUTINE_PROGRAM_UNIT:
            {
                AST subroutine_stmt = ASTSon0(a);
                AST name = ASTSon1(subroutine_stmt);
                AST program_body = ASTSon1(a);

                fortran_simplify_tree_body_program_unit(program_body, name, out);
                break;
            }
        case AST_MODULE_PROGRAM_UNIT:
            {
                internal_error("Not supported yet", 0);
                break;
            }
        case AST_BLOCK_DATA_PROGRAM_UNIT:
            {
                internal_error("Not supported yet", 0);
                break;
            }
        default:
            {
                internal_error("Invalid tree of type '%s'\n", ast_print_node_type(ASTType(a)));
            }
    }
}

void fortran_simplify_tree_translation_unit(AST a, AST *out)
{
    ERROR_CONDITION(ASTType(a) != AST_TRANSLATION_UNIT, "Invalid node", 0);

    AST program_unit_seq = ASTSon0(a);
    AST new_program_unit_seq = NULL;

    if (program_unit_seq != NULL)
    {
        AST it;
        for_each_element(program_unit_seq, it)
        {
            AST program_unit = ASTSon1(it);
            AST new_program_unit = NULL;

            fortran_simplify_tree_program_unit(program_unit, &new_program_unit);

            new_program_unit_seq = ASTList(new_program_unit_seq, new_program_unit);
        }
    }

    *out = new_program_unit_seq;
}
