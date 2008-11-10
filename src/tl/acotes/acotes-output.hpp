#ifndef ACOTES_OUTPUT_HPP
#define ACOTES_OUTPUT_HPP

#include "tl-pragmasupport.hpp"
#include "tl-langconstruct.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"

#include "acotes-outputtasks.hpp"

namespace TL
{
    class AcotesOutputPhase : public CompilerPhase
    {
        private:
            RefPtr<OutputTasks> _output_tasks;
        public:
            virtual void run(DTO& dto);
            virtual void pre_run(DTO& dto);
    };
}

#endif // ACOTES_OUTPUT_HPP
