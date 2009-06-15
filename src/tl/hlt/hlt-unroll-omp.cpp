#include "hlt-unroll.hpp"
#include "hlt-unroll-omp.hpp"
#include "tl-omp.hpp"

using namespace TL::HLT;
using namespace TL::OpenMP;

void LoopUnroll::omp_replication(int factor, Source &replicated_body, 
        IdExpression induction_var, Statement loop_body,
        Source &before, Source &after)
{
    if (_omp_bundling)
    {
        omp_replication_by_task_bundling(factor, replicated_body, 
                induction_var, loop_body, before, after);
    }
    else
    {
        // Plain task aggregation will be used instead
        omp_replication_by_task_aggregation(factor, replicated_body, induction_var, loop_body);
    }
}

void LoopUnroll::omp_replication_by_task_bundling(int factor, Source& replicated_body,
        IdExpression induction_var, Statement loop_body,
        Source& before, Source &after)
{
    Source replication;
    Source aggregation;
    aggregation
        << "{"
        << replication
        << "}"
        ;

    Symbol induction_sym = induction_var.get_symbol();
    for (unsigned int i = 0; i < factor; i++)
    {
        replication
            << flatten_compound(loop_body, i, induction_sym)
            ;
    }
    
    // Implement task aggregation
    AST_t tree = aggregation.parse_statement(loop_body.get_ast(),
            loop_body.get_scope_link());

    ASTIterator iterator = tree.get_list_iterator();
    Statement stmt(iterator.item(), loop_body.get_scope_link());

    TaskAggregation task_aggregation(stmt);
    task_aggregation.set_aggregation_method(TaskAggregation::BUNDLING)
        .set_global_bundling_source(before)
        .set_finish_bundling_source(after)
        .set_enclosing_function_tree(_for_stmt.get_ast().get_enclosing_function_definition());

    replicated_body = task_aggregation;
}

void LoopUnroll::omp_replication_by_task_aggregation(int factor, Source& replicated_body,
        IdExpression induction_var, Statement loop_body)
{
    Source replication;
    Source aggregation;
    aggregation
        << "{"
        << replication
        << "}"
        ;

    Symbol induction_sym = induction_var.get_symbol();
    for (unsigned int i = 0; i < factor; i++)
    {
        replication
            << flatten_compound(loop_body, i, induction_sym)
            ;
    }
    
    // Implement task aggregation
    AST_t tree = aggregation.parse_statement(loop_body.get_ast(),
            loop_body.get_scope_link());

    ASTIterator iterator = tree.get_list_iterator();
    Statement stmt(iterator.item(), loop_body.get_scope_link());

    TaskAggregation task_aggregation(stmt);
    task_aggregation
        .set_enclosing_function_tree(_for_stmt.get_ast().get_enclosing_function_definition());

    replicated_body = task_aggregation;
}

TL::Source LoopUnroll::flatten_compound(Statement stmt, int num, Symbol sym)
{
    if (!stmt.is_compound_statement())
    {
        return stmt.prettyprint();
    }

    Source result;

    ReplaceSrcIdExpression replacements(stmt.get_scope_link());

    // Induction var
    Source induction_var_rpl;
    induction_var_rpl
        << "(" << sym.get_name() << "+" << num << ")"
        ;
    replacements.add_replacement(sym, induction_var_rpl);

    ObjectList<Statement> stmt_list = stmt.get_inner_statements();
    for (ObjectList<Statement>::iterator it_current_stmt = stmt_list.begin();
            it_current_stmt != stmt_list.end();
            it_current_stmt++)
    {
        Statement &current_stmt(*it_current_stmt);

        if (current_stmt.is_simple_declaration())
        {
            Declaration decl = current_stmt.get_simple_declaration();

            ObjectList<DeclaredEntity> declaration_list = decl.get_declared_entities();

            for (ObjectList<DeclaredEntity>::iterator it_entity = declaration_list.begin();
                    it_entity != declaration_list.end();
                    it_entity++)
            {
                DeclaredEntity &entity(*it_entity);
                Symbol sym = entity.get_declared_symbol();

                Source repl_src;
                repl_src
                    << "_" << sym.get_name() << "_" << num
                    ;

                replacements.add_replacement(sym, repl_src);
            }
        }

        result
            << replacements.replace(current_stmt)
            ;
    }

    return result;
}

bool TL::HLT::there_is_declaration(TL::Statement st)
{
    if (st.is_compound_statement())
    {
        TL::ObjectList<TL::Statement> list = st.get_inner_statements();
        for (TL::ObjectList<TL::Statement>::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if (it->is_declaration())
                return true;
        }
    }
    
    return false;
}

