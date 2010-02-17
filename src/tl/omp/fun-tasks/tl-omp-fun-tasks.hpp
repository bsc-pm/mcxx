#ifndef TL_OMP_FUN_TASKS_HPP
#define TL_OMP_FUN_TASKS_HPP

#include "tl-omp.hpp"
#include "tl-omp-tasks.hpp"

namespace TL
{
    namespace OpenMP
    {
        class FunctionTasks : public OpenMPPhase
        {
            private:
                RefPtr<OpenMP::FunctionTaskSet> _function_task_set;

                void convert_calls(DTO& dto);

                void on_task_post(PragmaCustomConstruct construct);
            public:
                FunctionTasks();

                virtual void pre_run(DTO& dto);
                virtual void run(DTO& dto);
        };
    }
}

EXPORT_PHASE(TL::OpenMP::FunctionTasks);

#endif // TL_OMP_FUN_TASKS_HPP
