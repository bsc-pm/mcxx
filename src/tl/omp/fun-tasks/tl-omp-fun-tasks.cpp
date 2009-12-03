#include "tl-omp-fun-tasks.hpp"

namespace TL
{
namespace OpenMP
{
    FunctionTasks::FunctionTasks()
    {
        set_phase_name("OpenMP Task Functions");
        set_phase_description("This phase transforms calls to functions that "
                "have been declared with an OpenMP task construct into tasks "
                "themselves");

        // on_construct_post["task"].connect(functor(&FunctionTasks::on_task_post, *this));
    }

    void FunctionTasks::run(DTO& dto)
    {
        if (!dto.get_keys().contains("openmp_task_info"))
        {
            std::cerr << "OpenMP Task Info was not found in the pipeline" << std::endl;
            set_phase_status(PHASE_STATUS_ERROR);
            return;
        }

        _function_task_set = RefPtr<FunctionTaskSet>::cast_static(dto["openmp_task_info"]);

        // Run the parent phase which will simply remove the tasks
        OpenMPPhase::run(dto);

        // Convert calls
        // convert_calls(dto);
    }

#if 0
    void FunctionTasks::on_task_post(PragmaCustomConstruct construct)
    {
        // We do not deal with this kind of tasks
        if (!construct.get_declaration().is_valid())
        {
            return;
        }

        // Remove the task pragma
        construct.get_ast().replace(construct.get_declaration());
    }

    void FunctionTasks::convert_calls(DTO& dto)
    {
        AST_t translation_unit = dto["translation_unit"];
        ScopeLink scope_link = dto["scope_link"];

        struct IsFunctionCall : Predicate<AST_t>
        {
            ScopeLink _sl;
            RefPtr<FunctionTasksSet> _function_task_set;
            IsFunctionCall(ScopeLink sl, RefPtr<FunctionTasksSet> function_task_set)
                : _sl(sl), _function_task_set(function_task_set) { }

            bool do_(AST_t& a) const
            {
                if (Expression::predicate(a))
                {
                    bool result = false;
                    Expression expr(a, _sl);
                    // We want a function call
                    if (expr.is_function_call()
                        // To an entity
                        && expr.get_called_expression().is_id_expression())
                    {
                        Symbol sym = expr.get_called_expression().get_id_expression().get_computed_symbol();
                        if (sym.is_valid()
                                && sym.is_function()
                                && _function_task_set->is_function_task(sym))
                        {
                            result = true;
                        }
                    }
                    return result
                }
            }
        };

        ObjectList<AST_t> eligible_function_calls 
            = translation_unit.depth_subtrees(IsFunctionCall(scope_link));

        // This list will contain only calls to function tasks
        for (ObjectList<AST_t>::iterator it = eligible_function_calls.begin();
                it != eligible_function_calls.end();
                it++)
        {
            Expression expr(*it, scope_link);
            Symbol sym = expr.get_called_expression().get_id_expression().get_computed_symbol();

            FunctionTaskInfo& task_info = _function_task_set->get_function_task(sym);

            // Only void functions can be flagged as function tasks so we now
            // they will never be in an expression
        }
    }
#endif

} }
