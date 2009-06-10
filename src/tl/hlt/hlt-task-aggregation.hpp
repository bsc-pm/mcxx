#ifndef HLT_TASK_AGGREGATION_HPP
#define HLT_TASK_AGGREGATION_HPP

#include "tl-langconstruct.hpp"
#include "tl-omp.hpp"
#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        class TaskPart
        {
            private:
                ObjectList<Statement> _prolog;
                TL::OpenMP::TaskConstruct *_task;

            public:
                TaskPart(ObjectList<Statement> prolog, TL::OpenMP::TaskConstruct task)
                    : _prolog(prolog), _task(new TL::OpenMP::TaskConstruct(task)) { }

                TaskPart(TL::OpenMP::TaskConstruct task)
                    : _task(new TL::OpenMP::TaskConstruct(task)) { }

                TaskPart(ObjectList<Statement> prolog)
                    : _prolog(prolog), _task(NULL) { }

                TaskPart(const TaskPart& t)
                    : _prolog(t._prolog), _task(new TL::OpenMP::TaskConstruct(*t._task))
                {
                }

                ~TaskPart() 
                {
                    delete _task;
                }

                ObjectList<Statement> get_prolog()
                {
                    return _prolog;
                }

                bool has_task()
                {
                    return (_task != NULL);
                }

                TL::OpenMP::TaskConstruct get_task()
                {
                    return *_task;
                }
        };

        class TaskAggregation : public BaseTransform
        {
            public:
                enum AggregationMethod
                {
                    PREDICATION,
                    BUNDLING
                };
            private:
                Statement _stmt;
                AggregationMethod _method;

                static void get_task_parts_aux(ObjectList<TaskPart>& result, 
                        ObjectList<Statement> &current_prologue, Statement stmt);

                Source do_aggregation();
                Source do_predicated_aggregation();
                Source do_bundled_aggregation();
            protected:
                virtual Source get_source();
            public:
                static bool contains_relevant_openmp(Statement stmt);
                static ObjectList<TaskPart> get_task_parts(Statement stmt);
                TaskAggregation(Statement stmt, AggregationMethod = PREDICATION);

                TaskAggregation& set_aggregation_method(AggregationMethod);
        };
    }
}

#endif // HLT_TASK_AGGREGATION_HPP
