#include "hlt-task-aggregation.hpp"
#include "hlt-task-aggregation-common.hpp"
#include "tl-omp.hpp"
#include "tl-counters.hpp"

#include "hlt-unroll-omp.hpp"

/*
   FIXME - Still relying on explicit firstprivates :(
 */

using namespace TL;
using namespace HLT;
using namespace OpenMP;

const std::string TASK_PREDICATION_COUNTER("hlt.task_predication");

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

                result
                    << "{"
                    ;

                predicate_name
                    << "_task_guard_" << _info._num_tasks
                    ;

                result
                    << _info.get_guard_struct_var_name() << "." << predicate_name << "= 1;"
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

                result
                    << "}"
                    ;

                return AST_t::callback_result(true, result);
            }
            else 
                return AST_t::callback_result(false, "");
        }
};

Source TaskAggregation::do_predicated_aggregation()
{
    CounterManager::get_counter(TASK_PREDICATION_COUNTER)++;

    GuardTaskInfo guard_task_info;
    GuardTaskGenerator guard_task_generator(_stmt.get_scope_link(), guard_task_info);

    Source result, temporal_values_declarations, guarded_tasks, predicated_task;
    Source guard_struct_src, guard_struct_fields, guard_struct_name, guard_struct_var_decl;

    result
        << "{"
        << guard_struct_var_decl
        << temporal_values_declarations
        << guarded_tasks
        << predicated_task
        << "}"
        ;

    guard_struct_src
        << "struct " << guard_struct_name
        << "{"
        << guard_struct_fields
        << "};"
        ;

    // Number of guard structs created so far
    static int num_guard_structs = 0;
    num_guard_structs++;

    if (!_enclosing_function_def_tree.is_valid())
    {
        _enclosing_function_def_tree = _stmt.get_ast().get_enclosing_function_definition();
    }
    FunctionDefinition enclosing_function_def(_enclosing_function_def_tree, _stmt.get_scope_link());

    guard_struct_name 
        << "_guard_" << CounterManager::get_counter(TASK_PREDICATION_COUNTER) << "_" << num_guard_structs
        ;

    Source guard_struct_var_name = guard_task_info.get_guard_struct_var_name();
    guard_struct_var_decl << "struct " << guard_struct_name << " " << guard_struct_var_name << " = { 0 };"
        ;

    // This fills all the guard task info, before it won't contain any guarded task
    guarded_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    ObjectList<GuardedTask> guarded_task_list = guard_task_info.get_guarded_tasks();

    Source firstprivate_clause, firstprivate_args, predicated_body, guard_task_list;
	Source task_construct;
    predicated_task
		<< "if (" << guard_task_list << ")"
		<< "{"
		<< task_construct
		<< "}"
        ;

	if (!_do_not_create_tasks)
	{
		task_construct
			<<    "#pragma omp task" << firstprivate_clause << "\n"
			<<    "{"
			<<       predicated_body
			<<    "}"
			;
	}

    for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
            it != guarded_task_list.end();
            it++)
    {
        TaskConstruct task = it->get_task();

        Source replaced_body;

        Statement body = task.body();
        predicated_body
            << "if (" << guard_task_info.get_guard_struct_var_name() << "." << it->get_predicate_name() << ")"
            << "{"
            << replaced_body
            << "}"
            ;

		guard_task_list.append_with_separator(
				guard_task_info.get_guard_struct_var_name() + "." + it->get_predicate_name(),
				"||");

        guard_struct_fields
            << "char " << it->get_predicate_name() << ":1;"
            ;

        ReplaceSrcIdExpression replacements(_stmt.get_scope_link());
        ObjectList<GuardedTask::additional_var> additional_vars = it->get_additional_vars();
        for (ObjectList<GuardedTask::additional_var>::iterator it_additional_var = additional_vars.begin();
                it_additional_var != additional_vars.end();
                it_additional_var++)
        {
            GuardedTask::additional_var& additional_var(*it_additional_var);

            replacements.add_replacement(additional_var.second, additional_var.first);

            Symbol &sym(additional_var.second);
            temporal_values_declarations
                << sym.get_type().get_declaration(sym.get_scope(), additional_var.first) << ";";

            firstprivate_args.append_with_separator(additional_var.first, ",");
        }
		replaced_body << replacements.replace(body);
    }

    if (!firstprivate_args.empty())
    {
        firstprivate_args.append_with_separator(guard_struct_var_name, ",");
        firstprivate_clause << " firstprivate(" << firstprivate_args << ")"
            ;
    }

    // Now parse the guard struct and prepend it
    AST_t guard_struct_tree = guard_struct_src.parse_declaration(enclosing_function_def.get_ast(),
            _stmt.get_scope_link());

    _enclosing_function_def_tree.prepend(guard_struct_tree);

    return result;
}
