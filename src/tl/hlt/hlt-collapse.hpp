#ifndef HLT_COLLAPSE_HPP
#define HLT_COLLAPSE_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        struct LoopCollapse : public BaseTransform
        {
            private:
                ForNestInfo _for_nest_info;
            protected:
                Source get_source();
            public:
                LoopCollapse(ForStatement for_nest);
        };
    }
}

#endif // HLT_COLLAPSE_HPP
