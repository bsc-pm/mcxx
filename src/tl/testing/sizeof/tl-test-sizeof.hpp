#ifndef TL_TEST_SIZEOF_HPP
#define TL_TEST_SIZEOF_HPP

#include "tl-compilerphase.hpp"

namespace TL
{
    class SizeofTest : public CompilerPhase
    {
        public:
            virtual void run(DTO& dto);
    };
}

#endif // TL_TEST_SIZEOF_HPP

