#ifndef TL_OMP_NANOX_HPP
#define TL_OMP_NANOX_HPP

#include "tl-omp.hpp"

namespace TL
{
namespace Nanox
{
    class OMPTransform : public OpenMP::OpenMPPhase
    {
        public:
            OMPTransform();

        private:
            void parallel_postorder(PragmaCustomConstruct ctr);
            void task_postorder(PragmaCustomConstruct ctr);
            void taskwait_postorder(PragmaCustomConstruct ctr);
            void single_postorder(PragmaCustomConstruct ctr);
    };

    const std::string NANOX_OUTLINE_COUNTER("nanox_outline_counter");
}
}

#endif // TL_OMP_NANOX_HPP
