#ifndef TL_EXAMPLE_VISITOR_HPP
#define TL_EXAMPLE_VISITOR_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
    class VisitorExamplePhase : public TL::CompilerPhase
    {
        private:
        public:
            VisitorExamplePhase();
            ~VisitorExamplePhase();
            virtual void run(TL::DTO& dto);
    };
}

#endif // TL_EXAMPLE_VISITOR_HPP
