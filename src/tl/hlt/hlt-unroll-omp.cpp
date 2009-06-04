#include "hlt-unroll.hpp"
#include "hlt-unroll-omp.hpp"
#include "tl-omp.hpp"

using namespace TL::HLT;
using namespace TL::OpenMP;

bool LoopUnroll::contains_relevant_openmp(Statement stmt)
{
    if (TaskConstruct::predicate(stmt.get_ast()))
        return true;
    else if (stmt.is_compound_statement())
    {
        ObjectList<Statement> stmt_list = stmt.get_inner_statements();
        for (ObjectList<Statement>::iterator it = stmt_list.begin();
                it != stmt_list.end();
                it++)
        {
            if (contains_relevant_openmp(*it))
                return true;

        }
    }

    return false;
}

void LoopUnroll::get_task_parts_aux(ObjectList<TaskPart>& result, ObjectList<Statement> &current_prologue, Statement stmt)
{
    if (TaskConstruct::predicate(stmt.get_ast()))
    {
        TaskConstruct task_construct(stmt.get_ast(), stmt.get_scope_link());
        TaskPart new_task_part(current_prologue, task_construct);
        result.append(new_task_part);
        current_prologue.clear();
    }
    else if (stmt.is_compound_statement())
    {
        ObjectList<Statement> stmt_list = stmt.get_inner_statements();
        for (ObjectList<Statement>::iterator it = stmt_list.begin();
                it != stmt_list.end();
                it++)
        {
            get_task_parts_aux(result, current_prologue, *it);
        }
    }
    else
    {
        current_prologue.append(stmt);
    }
}

TL::ObjectList<TaskPart> LoopUnroll::get_task_parts(Statement stmt)
{
    ObjectList<TaskPart> result;
    ObjectList<Statement> prologue;

    get_task_parts_aux(result, prologue, stmt);

    if (!prologue.empty())
    {
        TaskPart last_part(prologue);
        result.append(last_part);
    }

    return result;
}

void LoopUnroll::omp_replication(int factor, Source &replicated_body, 
        IdExpression induction_var, Statement loop_body)
{
    // FIXME: We need to know the omp data sharing of symbols in order to
    // FIXME: implement this properly.
    // FIXME: For the time being, we will rely on the firstprivate clause
    Source task_header, task_contents;

    ObjectList<TaskPart> task_parts = get_task_parts(loop_body);

    unsigned int j = 0;
    for (ObjectList<TaskPart>::iterator it = task_parts.begin();
            it != task_parts.end();
            it++, j++)
    {
        TaskConstruct task_construct = it->get_task();
        Directive task_directive = task_construct.directive();
        Clause firstprivate_clause = task_directive.firstprivate_clause();
        ObjectList<IdExpression> firstprivate_ids;
        ObjectList<Statement> prolog = it->get_prolog();

        if (firstprivate_clause.is_defined())
        {
            firstprivate_ids = firstprivate_clause.id_expressions();
        }

        for (unsigned int i = 0; i < factor; i++)
        {
            for (ObjectList<Statement>::iterator stmt = prolog.begin();
                    stmt != prolog.end();
                    stmt++)
            {
                replicated_body
                    << (*stmt)
                    ;
            }

            for (ObjectList<IdExpression>::iterator current_id_expr = firstprivate_ids.begin();
                    current_id_expr != firstprivate_ids.end();
                    current_id_expr++)
            {
                Source name;
                name << (*current_id_expr) << "_" << j << "_" << i;

                replicated_body
                    << current_id_expr->get_symbol().get_type().get_declaration(current_id_expr->get_symbol().get_scope(),
                            name.get_source()) << " = " << (*current_id_expr) << ";";
            }
        }
        replicated_body
            << task_header
            << "{"
            << task_contents
            << "}"
            ;
        for (unsigned int i = 0; i < factor; i++)
        {
            ReplaceSrcIdExpression replacements(task_construct.get_scope_link());
            for (ObjectList<IdExpression>::iterator current_id_expr = firstprivate_ids.begin();
                    current_id_expr != firstprivate_ids.end();
                    current_id_expr++)
            {
                Source name;
                name << (*current_id_expr) << "_" << j << "_" << i;

                replacements.add_replacement(current_id_expr->get_symbol(), name.get_source());
            }

            if (!task_construct.body().is_compound_statement()
                    || there_is_declaration(task_construct.body()))
            {
                task_contents
                    << replacements.replace(task_construct.body())
                    ;
            }
            else
            {
                ObjectList<Statement> list = task_construct.body().get_inner_statements();
                for (ObjectList<Statement>::iterator each_stmt = list.begin();
                        each_stmt != list.end();
                        each_stmt++)
                {
                    task_contents
                        << replacements.replace(*each_stmt);
                }
            }
        }

        Source firstprivate_clause_src, firstprivate_list;
        task_header
            << "#pragma omp task"
            << firstprivate_clause_src
            << "\n"
            ;

        for (unsigned int i = 0; i < factor; i++)
        {
            for (ObjectList<IdExpression>::iterator current_id_expr = firstprivate_ids.begin();
                    current_id_expr != firstprivate_ids.end();
                    current_id_expr++)
            {
                Source name;
                name << (*current_id_expr) << "_" << j << "_" << i;

                firstprivate_list.append_with_separator(
                        name.get_source(), ",");
            }
        }

        if (!firstprivate_list.empty())
        {
            firstprivate_clause_src << " firstprivate(" << firstprivate_list << ")"
                ;
        }
    }
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
            if (TL::Declaration::predicate(it->get_ast()))
                return true;
        }
    }
    
    return false;
}
