#ifndef TL_BASIC_TEST_HPP
#define TL_BASIC_TEST_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
    class BasicTestPhase : public TL::CompilerPhase
    {
        private:
        public:
            BasicTestPhase();
            virtual void run(TL::DTO& dto);
            virtual void pre_run(TL::DTO& dto);
    };
}

#endif // TL_BASIC_TEST_HPP
