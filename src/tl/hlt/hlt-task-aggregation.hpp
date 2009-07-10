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
                int _bundling_amount;

				bool _do_not_create_tasks;
				bool _timing;

                Source *_global_bundling_src;
                Source *_finish_bundling_src;

                AST_t _enclosing_function_def_tree;

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

                TaskAggregation& set_bundling_amount(int amount);

                TaskAggregation& set_global_bundling_source(Source& src);
                TaskAggregation& set_finish_bundling_source(Source& src);

				TaskAggregation& set_do_not_create_tasks(bool b);

                TaskAggregation& set_enclosing_function_tree(AST_t enclosing_tree);

				TaskAggregation& set_timing(bool b);
        };
    }
}

#endif // HLT_TASK_AGGREGATION_HPP
