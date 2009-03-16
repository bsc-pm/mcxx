#ifndef HLT_TRANSFORM_HPP
#define HLT_TRANSFORM_HPP

#include "tl-source.hpp"
#include <string>

namespace TL
{
    namespace HLT
    {
        // Base for all transformations
        struct BaseTransform
        {
            protected:
                // Everybody should implement this one
                virtual Source get_source() = 0;
            public:
                 operator Source();
                 operator std::string();
        };
    }
}

#endif // HLT_TRANSFORM_HPP
