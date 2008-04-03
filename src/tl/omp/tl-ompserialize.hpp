#ifndef TL_OMPSZERIALIZE_HPP
#define TL_OMPSZERIALIZE_HPP

#include "tl-objectlist.hpp"
#include "tl-symbol.hpp"

namespace TL
{
    namespace Nanos4
    {
        struct SerializedFunctionsInfo : public Object
        {
            public:
                ObjectList<Symbol> serialized_functions;
        };
    }

    const char * const SERIALIZED_FUNCTIONS_INFO = "omp_serialized_functions_info";
}


#endif // TL_OMPSZERIALIZE_HPP
