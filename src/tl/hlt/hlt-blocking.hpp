#ifndef HLT_BLOCKING_HPP
#define HLT_BLOCKING_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        class LoopBlocking : public BaseTransform
        {
            protected:
                virtual Source get_source();
            private:
                TL::ForStatement _for_stmt;
                unsigned int _nesting;
                ObjectList<TL::Expression> _nest_factors;
                ObjectList<TL::ForStatement> _nest_loops;

                Source do_blocking();

                Source do_nothing();

                bool check_nesting();
                void discover_for_nest_rec(TL::AST_t tree);
                unsigned int discover_for_nest();
            public:
                LoopBlocking(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);
        };

        LoopBlocking block_loop(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors);
    }
}

#endif // HLT_BLOCKING_HPP
