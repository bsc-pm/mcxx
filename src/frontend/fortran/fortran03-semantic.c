#include "cxx-scope.h"
#include "cxx-utils.h"
#include "fortran03-semantic.h"
#include <string.h>

static void fortran_semantic_statement(AST statement, decl_context_t decl_context)
{
    statement = 0;
    decl_context.current_scope = 0;
}

static void fortran_semantic_body_program_unit(AST program_body, decl_context_t program_unit_context)
{
    AST specification_part = ASTSon0(program_body);
    AST execution_part = ASTSon1(program_body);
    AST internal_subprogram_part = ASTSon1(program_body);

    if (specification_part != NULL)
    {
        AST it;
        for_each_element(specification_part, it)
        {
            AST statement = ASTSon1(it);
            fortran_semantic_statement(statement, program_unit_context);
        }
    }
    if (execution_part != NULL)
    {
        AST it;
        for_each_element(execution_part, it)
        {
            AST statement = ASTSon1(it);
            fortran_semantic_statement(statement, program_unit_context);
        }
    }

    if (internal_subprogram_part != NULL)
    {
        AST it;
        for_each_element(internal_subprogram_part, it)
        {
            // AST internal_subprogram = ASTSon1(it);
        }
    }
}

static void fortran_semantic_main_program_unit(AST main_program_unit)
{
    AST program_stmt = ASTSon0(main_program_unit);
    AST program_body = ASTSon1(main_program_unit);
    AST end_program_stmt = ASTSon2(main_program_unit);

    AST program_name = NULL;
    if (program_stmt != NULL)
    {
        program_name = ASTSon1(program_stmt);
    }

    if (program_body != NULL)
    {
        decl_context_t program_unit_context = new_program_unit_context();

        if (program_name != NULL)
        {
            scope_entry_t* program_symbol = new_symbol(program_unit_context, 
                    program_unit_context.current_scope,
                    ASTText(program_name));
            program_symbol->kind = SK_PROGRAM;
            program_symbol->file = ASTFileName(program_name);
            program_symbol->line = ASTLine(program_name);
        }

        fortran_semantic_body_program_unit(program_body, program_unit_context);
    }

    AST end_program_name = ASTSon1(end_program_stmt);
    if (end_program_name != NULL)
    {
        if (program_name == NULL
                || (strcasecmp(ASTText(program_name), ASTText(end_program_name)) != 0))
        {
            running_error("%s: error: END PROGRAM statement name does not match PROGRAM statement\n",
                    ast_location(end_program_name));
        }
    }
}

static void fortran_semantic_program_unit_seq(AST program_unit_seq)
{
    AST list = program_unit_seq, it;

    for_each_element(list, it)
    {
        AST program_unit = ASTSon1(it);

        switch (ASTType(program_unit))
        {
            case AST_MAIN_PROGRAM_UNIT:
                {
                    fortran_semantic_main_program_unit(program_unit);
                    break;
                }
            case AST_FUNCTION_PROGRAM_UNIT:
                {
                    break;
                }
            case AST_SUBROUTINE_PROGRAM_UNIT:
                {
                    break;
                }
            case AST_MODULE_PROGRAM_UNIT:
                {
                    break;
                }
            case AST_SUBMODULE_PROGRAM_UNIT:
                {
                    break;
                }
            case AST_BLOCK_DATA_PROGRAM_UNIT:
                {
                    break;
                }
            default:
                {
                    internal_error("Invalid program unit '%s'\n", ast_print_node_type(ASTType(program_unit)));
                }
        }
    }
}

void fortran_semantic_translation_unit(translation_unit_t* translation_unit)
{
    AST a = translation_unit->parsed_tree;
    AST list = ASTSon0(a);
    if (list != NULL)
    {
        // Refactor this and "build_scope_translation_unit_tree_with_global_scope" one day
        fortran_semantic_program_unit_seq(list);
    }
}

