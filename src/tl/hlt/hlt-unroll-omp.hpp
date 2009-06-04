#ifndef HLT_UNROLL_OMP_HPP
#define HLT_UNROLL_OMP_HPP

#include "tl-langconstruct.hpp"
#include "tl-omp.hpp"

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

        // Private, do not use it elsewhere!!!
        bool there_is_declaration(TL::Statement st);
    }
}

#endif // HLT_UNROLL_OMP_HPP
