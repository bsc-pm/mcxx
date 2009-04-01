#ifndef HLT_INTERCHANGE_HPP
#define HLT_INTERCHANGE_HPP

#include "hlt-transform.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        class LoopInterchange : public BaseTransform
        {
            protected:
                Source do_interchange();
                virtual Source get_source();
                bool is_valid_permutation(ObjectList<int> permutation, bool &identity);
                ForNestInfo _for_nest;
                ObjectList<int> _permutation;
                bool _is_identity;
            public:
                LoopInterchange(ForStatement for_stmt, ObjectList<int> permutation);
        };
    }
}

#endif // HLT_INTERCHANGE_HPP
