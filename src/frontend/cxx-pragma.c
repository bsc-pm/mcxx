#include "cxx-pragma.h"
#include "cxx-nodecl-output.h"
#include "cxx-ast.h"
#include "cxx-tltype.h"
#include "cxx-scope.h"

static void common_build_scope_pragma_custom_clause_argument(AST a, 
        decl_context_t decl_context UNUSED_PARAMETER,
        nodecl_t *nodecl_output)
{
    *nodecl_output = nodecl_make_pragma_clause_arg(ASTText(a), ASTFileName(a), ASTLine(a));
}

static void common_build_scope_pragma_custom_clause(AST a, decl_context_t decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_argument = nodecl_null();
    if (ASTSon0(a) != NULL)
    {
        common_build_scope_pragma_custom_clause_argument(ASTSon0(a), decl_context, &nodecl_argument);
        // This is a list because it may be extended in later phases
        nodecl_argument = nodecl_make_list_1(nodecl_argument);
    }

    *nodecl_output = nodecl_make_pragma_custom_clause(nodecl_argument, ASTText(a), ASTFileName(a), ASTLine(a));
}

void common_build_scope_pragma_custom_line(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_clauses = nodecl_null();
    if (ASTSon0(a) != NULL)
    {
        AST list, iter;
        list = ASTSon0(a);

        for_each_element(list, iter)
        {
            AST pragma_clause = ASTSon1(iter);

            nodecl_t nodecl_clause = nodecl_null();
            common_build_scope_pragma_custom_clause(pragma_clause, decl_context, &nodecl_clause);

            nodecl_clauses = nodecl_append_to_list(nodecl_clauses, nodecl_clause);
        }
    }

    nodecl_t nodecl_parameter = nodecl_null();
    if (ASTSon1(a) != NULL)
    {
        common_build_scope_pragma_custom_clause_argument(ASTSon1(a), decl_context, &nodecl_parameter);
        
        nodecl_parameter = nodecl_make_list_1(nodecl_parameter);

    }

    *nodecl_output = nodecl_make_pragma_custom_line(nodecl_parameter, nodecl_clauses, ASTText(a), ASTFileName(a), ASTLine(a));
}

void common_build_scope_pragma_custom_declaration(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_pragma_line,
        nodecl_t* nodecl_nested_decl,
        void (*function_for_child)(AST, decl_context_t decl_context, nodecl_t*, void* info),
        void* info)
{
    common_build_scope_pragma_custom_line(ASTSon0(a), decl_context, nodecl_pragma_line);
    function_for_child(ASTSon1(a), decl_context, nodecl_nested_decl, info);
}

void common_build_scope_pragma_custom_statement(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output,
        nodecl_t* nodecl_pragma_line,
        void (*function_for_child)(AST, decl_context_t decl_context, nodecl_t*, void* info),
        void* info)
{
    common_build_scope_pragma_custom_line(ASTSon0(a), decl_context, nodecl_pragma_line);

    nodecl_t nodecl_child = nodecl_null();
    function_for_child(ASTSon1(a), new_block_context(decl_context), &nodecl_child, info);

    *nodecl_output = nodecl_make_pragma_custom_statement(*nodecl_pragma_line, nodecl_child, ASTText(a), ASTFileName(a), ASTLine(a));
}

void common_build_scope_pragma_custom_directive(AST a, 
        decl_context_t decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(ASTSon0(a), decl_context, &nodecl_pragma_line);


    *nodecl_output = nodecl_make_pragma_custom_directive(nodecl_pragma_line, ASTText(a), ASTFileName(a), ASTLine(a));
}
