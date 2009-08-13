#ifndef TL_OUTLINE_HPP
#define TL_OUTLINE_HPP

#include "tl-omp-nanox.hpp"

namespace TL
{
    namespace Nanox
    {
        Source create_outline(
                FunctionDefinition enclosing_function,
                Source outline_name,
                Source parameter_list,
                Source body);
    }
}

#endif // TL_OUTLINE_HPP
