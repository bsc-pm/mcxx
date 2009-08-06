// This is an internal header for the implementation of task aggregation
#ifndef HLT_TASK_AGGREGATION_COMMON_HPP
#define HLT_TASK_AGGREGATION_COMMON_HPP

#include "tl-symbol.hpp"
#include "tl-omp.hpp"
#include "tl-counters.hpp"

using namespace TL;
using namespace TL::OpenMP;

struct GuardedTask
{
    public:
        typedef std::pair<std::string, Symbol> additional_var;
    private:
        PragmaCustomConstruct _task_construct;
        ObjectList<additional_var> _additional_vars;
        std::string _predicate_name;
        int _task_id;
    public:
        GuardedTask(PragmaCustomConstruct task_construct,
                ObjectList<additional_var> additional_vars = ObjectList<additional_var>(),
                std::string predicate_name = std::string(""))
            : _task_construct(task_construct),
            _additional_vars(additional_vars),
            _predicate_name(predicate_name),
            _task_id(CounterManager::get_counter("hlt.guarded_task")++)
        {
        }

        PragmaCustomConstruct get_task() const
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

struct GuardTaskInfo
{
    public:
    private:
        int _task_counter;
        int _num_tasks;
        ObjectList<GuardedTask> _guarded_task_list;
    public:
        GuardTaskInfo()
            : _num_tasks(0), _task_counter(CounterManager::get_counter("hlt.guard_task_info")++)
        {
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
            src << "_g_" << _task_counter;
            return src.get_source();
        }

        void fill_guard_tasks_basic(Statement stmt) 
        {
            ObjectList<AST_t> tasks = stmt.get_ast().depth_subtrees(PragmaCustomConstruct::predicate);

            for (ObjectList<AST_t>::iterator it = tasks.begin();
                    it != tasks.end();
                    it++)
            {
                PragmaCustomConstruct task_construct(*it, stmt.get_scope_link());

                GuardedTask guarded_task(task_construct);
                _guarded_task_list.append(guarded_task);
                _num_tasks++;
            }
        }

        int get_task_id(PragmaCustomConstruct task_construct) const
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

#endif // HLT_TASK_AGGREGATION_COMMON_HPP
