#ifndef HLT_UNROLL_HPP
#define HLT_UNROLL_HPP

#include "tl-langconstruct.hpp"
#include "hlt-transform.hpp"

namespace TL
{
    namespace HLT
    {
        class LoopUnroll : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                ForStatement _for_stmt;
                unsigned int _factor;
                bool _regular;
                bool _with_epilog;

                Source do_unroll();
            public:
                LoopUnroll(ForStatement for_stmt, unsigned int factor);
        };

        LoopUnroll unroll_loop(ForStatement for_stmt, unsigned int factor);
    }
}

#endif // HLT_UNROLL_HPP
