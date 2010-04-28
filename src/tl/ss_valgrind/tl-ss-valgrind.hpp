#ifndef TL_SS_VALGRIND_HPP
#define TL_SS_VALGRIND_HPP

#include "tl-pragmasupport.hpp"

namespace TL
{
    class SSValgrind : public PragmaCustomCompilerPhase
    {
        public:
            SSValgrind();
            virtual void run(DTO& dto);

        private:
            void pragma_start(PragmaCustomConstruct ctr);
            void pragma_finish(PragmaCustomConstruct ctr);
            void pragma_barrier(PragmaCustomConstruct ctr);
    };
}

#endif // TL_SS_VALGRIND_HPP
