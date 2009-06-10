#include "hlt-task-aggregation.hpp"
#include "tl-omp.hpp"

#include "hlt-unroll-omp.hpp"

/*
   FIXME - Still relying on explicit firstprivates :(
 */

using namespace TL;
using namespace HLT;
using namespace OpenMP;

TaskAggregation::TaskAggregation(Statement stmt, AggregationMethod method)
    : _stmt(stmt), _method(method)
{
}

TaskAggregation& TaskAggregation::set_aggregation_method(AggregationMethod method)
{
    _method = method;
    return *this;
}

Source TaskAggregation::get_source()
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

struct GuardedTask
{
    public:
        typedef std::pair<std::string, Symbol> additional_var;
    private:
        TaskConstruct _task_construct;
        ObjectList<additional_var> _additional_vars;
        std::string _predicate_name;
    public:
        GuardedTask(TaskConstruct task_construct,
                ObjectList<additional_var> additional_vars,
                std::string predicate_name)
            : _task_construct(task_construct),
            _additional_vars(additional_vars),
            _predicate_name(predicate_name)
        {
        }

        TaskConstruct get_task() const
        {
            return _task_construct;
        }

        ObjectList<additional_var> get_additional_vars() const
        {
            return _additional_vars;
        }

        std::string get_predicate_name() const
        {
            return _predicate_name;
        }
};

struct GuardTaskInfo
{
    public:
    private:
        int _num_tasks;
        ObjectList<GuardedTask> _guarded_task_list;
    public:
        GuardTaskInfo()
            : _num_tasks(0)
        {
        }

        int get_num_tasks() const
        {
            return _num_tasks;
        }

        ObjectList<GuardedTask> get_guarded_tasks()
        {
            return _guarded_task_list;
        }

        friend struct GuardTaskGenerator;
};

struct GuardTaskGenerator : Functor<TL::AST_t::callback_result, TL::AST_t>
{
    private:
        ScopeLink _sl;
        GuardTaskInfo& _info;
    public:
        GuardTaskGenerator(ScopeLink sl, GuardTaskInfo& info)
            : _sl(sl), _info(info)
        {
        }

        virtual AST_t::callback_result do_(TL::AST_t& a) const
        {
            if (TaskConstruct::predicate(a))
            {
                Source result;
                Source predicate_name;

                predicate_name
                    << "_task_guard_" << _info._num_tasks
                    ;

                result
                    << predicate_name << "=" << _info._num_tasks << ";"
                    ;

                // Capture current firstprivate values
                // FIXME -> We are relying on explicit firstprivates
                TaskConstruct task_construct(a, _sl);
                Directive directive = task_construct.directive();
                Clause firstprivate_clause = directive.firstprivate_clause();

                ObjectList<GuardedTask::additional_var> additional_vars;

                if (firstprivate_clause.is_defined())
                {
                    ObjectList<TL::IdExpression> vars = firstprivate_clause.id_expressions();

                    for (ObjectList<TL::IdExpression>::iterator it = vars.begin();
                            it != vars.end();
                            it++)
                    {
                        Symbol sym = it->get_symbol();
                        Source new_name;
                        new_name
                            << "_" << sym.get_name() << "_" << _info._num_tasks;
                        GuardedTask::additional_var new_var(new_name, sym);

                        additional_vars.append(new_var);

                        result 
                            << new_name << " = " << sym.get_name() << ";"
                            ;
                    }
                }
                
                GuardedTask new_guarded_task(task_construct, additional_vars, predicate_name);
                _info._guarded_task_list.append(new_guarded_task);

                _info._num_tasks++;

                return AST_t::callback_result(true, result);
            }
            else 
                return AST_t::callback_result(false, "");
        }
};

Source TaskAggregation::do_aggregation()
{
    switch ((int)_method)
    {
        case PREDICATION:
            {
                return do_predicated_aggregation();
            }
        case BUNDLING:
            {
                return do_bundled_aggregation();
            }
        default:
            return Source("");
    }
}

Source TaskAggregation::do_bundled_aggregation()
{
    return Source("");
}

Source TaskAggregation::do_predicated_aggregation()
{
    GuardTaskInfo guard_task_info;
    GuardTaskGenerator guard_task_generator(_stmt.get_scope_link(), guard_task_info);

    Source result, guard_declarations, temporal_values_declarations, guarded_tasks, predicated_task;

    result
        << "{"
        << guard_declarations
        << temporal_values_declarations
        << guarded_tasks
        << predicated_task
        << "}"
        ;


    guarded_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    ObjectList<GuardedTask> guarded_task_list = guard_task_info.get_guarded_tasks();

    Source firstprivate_clause, firstprivate_args, predicated_body;
    predicated_task
        << "#pragma omp task" << firstprivate_clause << "\n"
        << "{"
        << predicated_body
        << "}"
        ;

    for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
            it != guarded_task_list.end();
            it++)
    {
        TaskConstruct task = it->get_task();

        Source replaced_body;

        Statement body = task.body();
        predicated_body
            << "if (" << it->get_predicate_name() << ")"
            << "{"
            << replaced_body
            << "}"
            ;

        guard_declarations
            << "char " << it->get_predicate_name() << " = 0;"
            ;

        firstprivate_args.append_with_separator(it->get_predicate_name(), ",");

        ReplaceSrcIdExpression replacements(_stmt.get_scope_link());
        ObjectList<GuardedTask::additional_var> additional_vars = it->get_additional_vars();
        for (ObjectList<GuardedTask::additional_var>::iterator it_additional_var = additional_vars.begin();
                it_additional_var != additional_vars.end();
                it_additional_var++)
        {
            GuardedTask::additional_var& additional_var(*it_additional_var);

            replacements.add_replacement(additional_var.second, additional_var.first);

            replaced_body << replacements.replace(body);

            firstprivate_args.append_with_separator(additional_var.first, ",");

            Symbol &sym(additional_var.second);
            temporal_values_declarations
                << sym.get_type().get_declaration(sym.get_scope(), additional_var.first) << ";";

        }
    }

    if (!firstprivate_args.empty())
    {
        firstprivate_clause << " firstprivate(" << firstprivate_args << ")"
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

ObjectList<TaskPart> TaskAggregation::get_task_parts(Statement stmt)
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
