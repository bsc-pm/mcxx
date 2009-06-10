#include "hlt-task-aggregation.hpp"
#include "tl-omp.hpp"

#include "hlt-unroll-omp.hpp"

using namespace TL::HLT;
using namespace TL::OpenMP;

TaskAggregation::TaskAggregation(Statement stmt)
    : _stmt(stmt)
{
}

TL::Source TaskAggregation::get_source()
{
    if (!contains_relevant_openmp(_stmt))
    {
        return _stmt.prettyprint();
    }
    else
    {
        return do_aggregation();
    }
}

TL::Source TaskAggregation::do_aggregation()
{
    Source result;
    ObjectList<TaskPart> task_part_list = get_task_parts(_stmt);

    // FIXME - Still relying on explicit firstprivates

    Source prologue, big_task, firstprivate_clause_src, firstprivate_arg;

    result
        << prologue
        << "#pragma omp task"
        << firstprivate_clause_src
        << "\n"
        << "{"
        << big_task
        << "}"
        ;


    int i = 0;
    for (ObjectList<TaskPart>::iterator it_task_part = task_part_list.begin();
            it_task_part != task_part_list.end();
            it_task_part++, i++)
    {
        TaskPart &task_part(*it_task_part);
        ObjectList<Statement> current_prolog = task_part.get_prolog();

        for (ObjectList<Statement>::iterator it = current_prolog.begin();
                it != current_prolog.end();
                it++)
        {
            prologue << *it
                ;
        }

        if (task_part.has_task())
        {
            TaskConstruct task_construct = task_part.get_task();
            Directive task_directive = task_construct.directive();
            Clause firstprivate_clause = task_directive.firstprivate_clause();
            ObjectList<IdExpression> firstprivate_ids;
            if (firstprivate_clause.is_defined())
            {
                firstprivate_ids = firstprivate_clause.id_expressions();
            }

            ReplaceSrcIdExpression replacements(_stmt.get_scope_link());

            for (ObjectList<IdExpression>::iterator current_id_expr = firstprivate_ids.begin();
                    current_id_expr != firstprivate_ids.end();
                    current_id_expr++)
            {
                Source name;
                name << (*current_id_expr) << "_" << i;

                prologue
                    << current_id_expr->get_symbol().get_type().get_declaration(current_id_expr->get_symbol().get_scope(),
                            name.get_source()) << " = " << (*current_id_expr) << ";";

                replacements.add_replacement(current_id_expr->get_symbol(), name.get_source());

                firstprivate_arg.append_with_separator(name.get_source(), ",");
            }

            if (!task_construct.body().is_compound_statement()
                    || there_is_declaration(task_construct.body()))
            {
                big_task
                    << replacements.replace(task_construct.body())
                    ;
            }
            else
            {
                ObjectList<Statement> inner = task_construct.body().get_inner_statements();
                for (ObjectList<Statement>::iterator it = inner.begin();
                        it != inner.end();
                        it++)
                {
                    big_task
                        << replacements.replace(*it)
                        ;
                }
            }
        }

    }

    if (!firstprivate_arg.empty())
    {
        firstprivate_clause_src
            << " firstprivate(" << firstprivate_arg << ")"
            ;
    }

    return result;
}

bool TaskAggregation::contains_relevant_openmp(Statement stmt)
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
    else if (IfStatement::predicate(stmt.get_ast()))
    {
        IfStatement if_statement(stmt.get_ast(), stmt.get_scope_link());

        return contains_relevant_openmp(if_statement.get_then_body())
            || (if_statement.has_else() && contains_relevant_openmp(if_statement.get_else_body()));
    }

    return false;
}

void TaskAggregation::get_task_parts_aux(ObjectList<TaskPart>& result, ObjectList<Statement> &current_prologue, Statement stmt)
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

TL::ObjectList<TaskPart> TaskAggregation::get_task_parts(Statement stmt)
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
