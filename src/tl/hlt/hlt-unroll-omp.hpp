#ifndef HLT_UNROLL_OMP_HPP
#define HLT_UNROLL_OMP_HPP

#include "tl-langconstruct.hpp"
#include "tl-omp.hpp"
#include "hlt-task-aggregation.hpp"

namespace TL
{
    namespace HLT
    {
        // Private, do not use it elsewhere!!!
        bool there_is_declaration(TL::Statement st);
    }
}

#endif // HLT_UNROLL_OMP_HPP
