#include "hlt-task-aggregation.hpp"
#include "tl-omp.hpp"

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
