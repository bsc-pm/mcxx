#ifndef TL_NANOX_NODECL_HPP
#define TL_NANOX_NODECL_HPP

#include "tl-compilerphase.hpp"

namespace TL { namespace Nanox {

    class Lowering : public TL::CompilerPhase
    {
        public:
            Lowering();
            virtual void run(DTO& dto);
            virtual void pre_run(DTO& dto) { }
    };

} } 

#endif // TL_NANOX_NODECL_HPP
