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
: _stmt(stmt),
    _method(method),
    _bundling_amount(4),
    _global_bundling_src(NULL),
    _finish_bundling_src(NULL)
{
}

TaskAggregation& TaskAggregation::set_aggregation_method(AggregationMethod method)
{
    _method = method;
    return *this;
}

TaskAggregation& TaskAggregation::set_bundling_amount(int amount)
{
    _bundling_amount = amount;
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
        static int _global_task_id;
    private:
        TaskConstruct _task_construct;
        ObjectList<additional_var> _additional_vars;
        std::string _predicate_name;
        int _task_id;
    public:
        GuardedTask(TaskConstruct task_construct,
                ObjectList<additional_var> additional_vars = ObjectList<additional_var>(),
                std::string predicate_name = std::string(""))
            : _task_construct(task_construct),
            _additional_vars(additional_vars),
            _predicate_name(predicate_name),
            _task_id(_global_task_id++)
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

        int get_id() const
        {
            return _task_id;
        }
};

int GuardedTask::_global_task_id = 0;

struct GuardTaskInfo
{
    public:
    private:
        static int _guard_task_num;
        int _num_tasks;
        ObjectList<GuardedTask> _guarded_task_list;
    public:
        GuardTaskInfo()
            : _num_tasks(0)
        {
            _guard_task_num++;
        }

        int get_num_tasks() const
        {
            return _num_tasks;
        }

        ObjectList<GuardedTask> get_guarded_tasks() const
        {
            return _guarded_task_list;
        }

        std::string get_guard_struct_var_name() const
        {
            Source src;
            src << "_g_" << _guard_task_num;
            return src.get_source();
        }

        void fill_guard_tasks_basic(Statement stmt) 
        {
            ObjectList<AST_t> tasks = stmt.get_ast().depth_subtrees(TaskConstruct::predicate);

            for (ObjectList<AST_t>::iterator it = tasks.begin();
                    it != tasks.end();
                    it++)
            {
                TaskConstruct task_construct(*it, stmt.get_scope_link());

                GuardedTask guarded_task(task_construct);
                _guarded_task_list.append(guarded_task);
                _num_tasks++;
            }
        }

        int get_task_id(TaskConstruct task_construct) const
        {
            ObjectList<GuardedTask> found_tasks 
                = _guarded_task_list.find(functor(get_ast_from_guarded_task),
                        task_construct.get_ast());

            if (found_tasks.empty())
                return -1;
            return found_tasks[0].get_id();
        }

    private:
        static AST_t get_ast_from_guarded_task(GuardedTask gt)
        {
            return gt.get_task().get_ast();
        }

        friend struct GuardTaskGenerator;
};

int GuardTaskInfo::_guard_task_num;

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
                    << _info.get_guard_struct_var_name() << "." << predicate_name << "=" << _info._num_tasks << ";"
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
                << "int _current_index = 0;"
                << code_of_all_tasks
                << "}"
                ;

            ObjectList<GuardedTask> guarded_task_list = _info.get_guarded_tasks();
            for (ObjectList<GuardedTask>::iterator it = guarded_task_list.begin();
                    it != guarded_task_list.end();
                    it++)
            {
                clear_indexes
                    << "_task_index_" << it->get_id() << "= 0;"
                    ;

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
                        firstprivate_args.append_with_separator(
                                "_tmp_" + sym.get_name(),
                                ",");

                        Source replace_src;
                        replace_src
                            << "(_tmp_" << sym.get_name() << "[_current_index])"
                            ;
                        replacements.add_replacement(sym, replace_src);
                    }
                }

                Source extended_task_body;

                code_of_all_tasks
                    << "while (_task_index_" << it->get_id() << " > 0)"
                    << "{"
                    <<    extended_task_body
                    <<    "_task_index_" << it->get_id() << "--;"
                    <<    "_current_index++;"
                    << "}"
                    ;

                extended_task_body << replacements.replace(current_task.body());
            }

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
                    << "}"
                    << "_global_task_index = 0;"
                    << clear_indexes
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
    GuardTaskInfo guard_task_info;
    guard_task_info.fill_guard_tasks_basic(_stmt);

    Source result,
           inline_task_counters,
           inline_finish_task,
           extended_declarations,
           bundled_tasks;

    result
        << "{"
        << inline_task_counters
        << extended_declarations
        << bundled_tasks
        << inline_finish_task
        << "}"
        ;

    Source &task_counters = (_global_bundling_src != NULL ? *_global_bundling_src : inline_task_counters);
    Source &bundle_remainder = (_finish_bundling_src != NULL ? *_finish_bundling_src : inline_finish_task);

    ObjectList<GuardedTask> guarded_tasks = guard_task_info.get_guarded_tasks();

    ObjectList<Symbol> firstprivate_syms;

    for (ObjectList<GuardedTask>::iterator it_guarded_task = guarded_tasks.begin();
            it_guarded_task != guarded_tasks.end();
            it_guarded_task++)
    {
        GuardedTask &guarded_task(*it_guarded_task);

        task_counters
            << "int _task_index_" << guarded_task.get_id() << " = 0;"
            ;

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

Source TaskAggregation::do_predicated_aggregation()
{
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

    AST_t enclosing_function_def_tree = _stmt.get_ast().get_enclosing_function_definition();
    FunctionDefinition enclosing_function_def(enclosing_function_def_tree, _stmt.get_scope_link());

    guard_struct_name << "_guard_" << enclosing_function_def.get_function_symbol().get_name() << "_" << num_guard_structs
        ;

    Source guard_struct_var_name = guard_task_info.get_guard_struct_var_name();
    guard_struct_var_decl << "struct " << guard_struct_name << " " << guard_struct_var_name << ";"
        ;

    // This fills all the guard task info, before it won't contain any guarded task
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
            << "if (" << guard_task_info.get_guard_struct_var_name() << "." << it->get_predicate_name() << ")"
            << "{"
            << replaced_body
            << "}"
            ;

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

            replaced_body << replacements.replace(body);


            Symbol &sym(additional_var.second);
            temporal_values_declarations
                << sym.get_type().get_declaration(sym.get_scope(), additional_var.first) << ";";

            firstprivate_args.append_with_separator(additional_var.first, ",");
        }
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

    enclosing_function_def_tree.prepend(guard_struct_tree);

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

TaskAggregation& TaskAggregation::set_global_bundling_source(Source& src)
{
    _global_bundling_src = &src;
    return *this;
}

TaskAggregation& TaskAggregation::set_finish_bundling_source(Source& src)
{
    _finish_bundling_src = &src;
    return *this;
}
