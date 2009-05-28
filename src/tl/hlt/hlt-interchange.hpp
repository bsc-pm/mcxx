#ifndef HLT_INTERCHANGE_HPP
#define HLT_INTERCHANGE_HPP

#include "hlt-transform.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! This class implements loops interchange
        /*!
          Loop interchange is a loop transformation where a perfect loop nest
          has its loops permuted.
         */
        class LIBHLT_CLASS LoopInterchange : public BaseTransform
        {
            protected:
                Source do_interchange();
                virtual Source get_source();
                bool is_valid_permutation(ObjectList<int> permutation, bool &identity);
                ForNestInfo _for_nest;
                ObjectList<int> _permutation;
                bool _is_identity;
            public:
                //! Constructs a LoopInterchange object
                /*!
                  \param for_stmt Perfect loop nest
                  \param permutation A permutation of integers ranging from 1 to the depth of \a for_stmt loop nest
                 */
                LoopInterchange(ForStatement for_stmt, ObjectList<int> permutation);
        };

        //! Constructs a LoopInterchange object
        /*!
          \param for_stmt Perfect loop nest
          \param permutation A permutation of integers ranging from 1 to the depth of \a for_stmt loop nest
         */
        LIBHLT_EXTERN LoopInterchange loop_interchange(ForStatement for_stmt, ObjectList<int> permutation);
        //! @}
    }
}

#endif // HLT_INTERCHANGE_HPP
