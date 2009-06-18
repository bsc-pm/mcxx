#include "hlt-task-aggregation.hpp"
#include "hlt-task-aggregation-common.hpp"
#include "tl-omp.hpp"

#include "hlt-unroll-omp.hpp"

/*
   FIXME - Still relying on explicit firstprivates :(
 */

using namespace TL;
using namespace HLT;
using namespace OpenMP;

struct BundleGenerator
{
    private:
        const GuardTaskInfo &_info;
        ScopeLink _sl;
    public:
        BundleGenerator(const GuardTaskInfo& info, ScopeLink sl)
            : _info(info), _sl(sl)
        {
        }

        Source generate_bundle(Source &clear_indexes)
        {
            Source try_to_run_every_task;
            Source firstprivate_clause_src, firstprivate_args, code_of_all_tasks;
            try_to_run_every_task
                << "#pragma omp task" << firstprivate_clause_src << "\n"
                << "{"
                <<   "int _current_index = 0;"
                //   FIXME - Unroll this
                <<   "for (_current_index = 0; "
                <<         "_current_index < _global_task_index; "
                <<         "_current_index++)"
                <<   "{"
                <<       "switch (_task_log[_current_index])"
                <<       "{"
                <<           code_of_all_tasks
                <<           "default: break;"
                <<       "}"
                <<   "}"
                << "}"
                ;

            ObjectList<Symbol> firstprivated_symbols;
            ObjectList<GuardedTask> guarded_task_list = _info.get_guarded_tasks();
            for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
                    it != guarded_task_list.end();
                    it++)
            {
                TaskConstruct current_task = it->get_task();
                Directive current_directive = current_task.directive();
                Clause current_firstprivate_clause = current_directive.firstprivate_clause();
                ReplaceSrcIdExpression replacements(_sl);

                if (current_firstprivate_clause.is_defined())
                {
                    ObjectList<TL::IdExpression> vars = current_firstprivate_clause.id_expressions();

                    for (ObjectList<TL::IdExpression>::iterator it_var = vars.begin();
                            it_var != vars.end();
                            it_var++)
                    {
                        Symbol sym(it_var->get_symbol());
                        if (!firstprivated_symbols.contains(sym))
                        {
                            firstprivate_args.append_with_separator(
                                    "_tmp_" + sym.get_name(),
                                    ",");
                            firstprivated_symbols.insert(sym);
                        }

                        Source replace_src;
                        replace_src
                            << "(_tmp_" << sym.get_name() << "[_current_index])"
                            ;
                        replacements.add_replacement(sym, replace_src);
                    }
                }

                Source extended_task_body;

                code_of_all_tasks
                    << "case " << it->get_id() << ":"
                    << "{"
                    <<    extended_task_body
                    <<    "break;"
                    << "}"
                    ;

                extended_task_body << replacements.replace(current_task.body());
            }

            // Task log is always passed
            firstprivate_args.append_with_separator(
                    "_task_log",
                    ",");
            // Global index is always passed
            firstprivate_args.append_with_separator(
                    "_global_task_index",
                    ",");
            if (!firstprivate_args.empty())
            {
                firstprivate_clause_src << " firstprivate(" << firstprivate_args << ")"
                    ;
            }

            return try_to_run_every_task;
        }
};

struct GuardTaskGeneratorBundled : Functor<TL::AST_t::callback_result, TL::AST_t>
{
    private:
        ScopeLink _sl;
        const GuardTaskInfo& _info;
        int _bundling_amount;
    public:
        GuardTaskGeneratorBundled(ScopeLink sl, const GuardTaskInfo& info, int bundling_amount = 4)
            : _sl(sl), 
            _info(info), 
            _bundling_amount(bundling_amount)
        {
        }

        virtual AST_t::callback_result do_(TL::AST_t& a) const
        {
            if (TaskConstruct::predicate(a))
            {
                Source result;
                // Capture current firstprivate values
                // FIXME -> We are relying on explicit firstprivates
                TaskConstruct task_construct(a, _sl);
                int task_id = _info.get_task_id(task_construct);
                Directive directive = task_construct.directive();
                Clause firstprivate_clause = directive.firstprivate_clause();

                ObjectList<GuardedTask::additional_var> additional_vars;

                Source task_index_name;
                task_index_name << "_global_task_index"
                    ;

                result
                    << "_task_log[" << task_index_name << "] = " << task_id << ";"
                    ;

                if (firstprivate_clause.is_defined())
                {
                    ObjectList<TL::IdExpression> vars = firstprivate_clause.id_expressions();

                    for (ObjectList<TL::IdExpression>::iterator it = vars.begin();
                            it != vars.end();
                            it++)
                    {
                        Symbol sym(it->get_symbol());
                        result
                            << "_tmp_" << sym.get_name() << "[" << task_index_name << "] = " << it->prettyprint() << ";"
                            ;
                    }
                }

                Source try_to_run_every_task, clear_indexes;

                result
                    << "_global_task_index++;"
                    << "if (_global_task_index == " << _bundling_amount << ")"
                    << "{"
                    <<     try_to_run_every_task
                    <<     "_global_task_index = 0;"
                    <<     clear_indexes
                    << "}"
                    ;

                BundleGenerator bundle_gen(_info, _sl);

                try_to_run_every_task
                    << bundle_gen.generate_bundle(clear_indexes)
                    ;


                return AST_t::callback_result(true, result);
            }
            else 
                return AST_t::callback_result(false, "");
        }
};

Source TaskAggregation::do_bundled_aggregation()
{
    GuardTaskInfo guard_task_info;
    guard_task_info.fill_guard_tasks_basic(_stmt);

    Source result,
           inline_task_counters,
           inline_finish_task,
           bundled_tasks;

    result
        << "{"
        << inline_task_counters
        << bundled_tasks
        << inline_finish_task
        << "}"
        ;

    Source &task_counters = (_global_bundling_src != NULL ? *_global_bundling_src : inline_task_counters);
    Source &bundle_remainder = (_finish_bundling_src != NULL ? *_finish_bundling_src : inline_finish_task);

    Source extended_declarations;
    task_counters
        << "int _task_log[" << _bundling_amount << "] = {0};"
        << extended_declarations;

    ObjectList<GuardedTask> guarded_tasks = guard_task_info.get_guarded_tasks();

    ObjectList<Symbol> firstprivate_syms;

    for (ObjectList<GuardedTask>::iterator it_guarded_task = guarded_tasks.begin();
            it_guarded_task != guarded_tasks.end();
            it_guarded_task++)
    {
        GuardedTask &guarded_task(*it_guarded_task);

        TaskConstruct task_construct = guarded_task.get_task();
        Directive task_directive = task_construct.directive();
        Clause firstprivate_clause = task_directive.firstprivate_clause();

        if (firstprivate_clause.is_defined())
        {
            ObjectList<IdExpression> id_expressions = firstprivate_clause.id_expressions();
            firstprivate_syms.insert(id_expressions.map(functor(&IdExpression::get_symbol)));
        }
    }

    {
        Source bundle_expr_src;
        bundle_expr_src << _bundling_amount;
        AST_t bundle_expr_tree = bundle_expr_src.parse_expression(_stmt.get_ast(),
                _stmt.get_scope_link());
        for (ObjectList<Symbol>::iterator it_sym = firstprivate_syms.begin();
                it_sym != firstprivate_syms.end();
                it_sym++)
        {
            Symbol& sym(*it_sym);
            Type array_type = sym.get_type().get_array_to(bundle_expr_tree, sym.get_scope());

            extended_declarations
                << array_type.get_declaration(sym.get_scope(), "_tmp_" + sym.get_name()) << ";"
                ;
        }
    }

    task_counters
        << "int _global_task_index = 0;" 
        ;

    GuardTaskGeneratorBundled guard_task_generator(_stmt.get_scope_link(), guard_task_info, _bundling_amount);
    bundled_tasks << _stmt.get_ast().prettyprint_with_callback(guard_task_generator);

    BundleGenerator bundle_gen(guard_task_info, _stmt.get_scope_link());
    Source clear_indexes;
    bundle_remainder
        << "if (_global_task_index != 0)"
        << "{"
        << bundle_gen.generate_bundle(clear_indexes)
        // << clear_indexes
        << "}"
        ;

    return result;
}
