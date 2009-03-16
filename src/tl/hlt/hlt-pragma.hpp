#ifndef HLT_PRAGMA_HPP
#define HLT_PRAGMA_HPP

#include "tl-pragmasupport.hpp"

namespace TL
{
    namespace HLT
    {
        class HLTPragmaPhase : public PragmaCustomCompilerPhase
        {
            public:
                HLTPragmaPhase();
                virtual void run(TL::DTO& dto);
            private:
                void unroll_loop(PragmaCustomConstruct construct);
        };
    }

}

#endif // HLT_PRAGMA_HPP
