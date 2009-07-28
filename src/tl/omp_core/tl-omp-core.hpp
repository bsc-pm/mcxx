#ifndef TL_OMP_CORE_HPP
#define TL_OMP_CORE_HPP

#include "tl-compilerphase.hpp"
#include "tl-pragmasupport.hpp"

namespace TL
{
    class OmpCore : public TL::PragmaCustomCompilerPhase
    {
        private:
            virtual void register_omp_constructs();

            // Handler functions
#define DIRECTIVE_INFO(_name) \
            void _name##_handler(PragmaCustomConstruct);
#define CONSTRUCT_INFO(_name) DIRECTIVE_INFO(_name)
#include "tl-omp-core.def"
#undef DIRECTIVE_INFO
#undef CONSTRUCT_INFO
        public:
            OmpCore();

            virtual void run(TL::DTO& dto);
            virtual void pre_run(TL::DTO& dto);

            virtual ~OmpCore() { }
    };
}

EXPORT_PHASE(TL::OmpCore)

#endif // TL_OMP_CORE_HPP
