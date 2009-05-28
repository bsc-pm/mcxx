#ifndef HLT_COLLAPSE_HPP
#define HLT_COLLAPSE_HPP

#include "hlt-transform.hpp"
#include "tl-langconstruct.hpp"
#include "tl-for-nest.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{

        //! Class that implements loop collapsing
        /*!
          This class collapses a perfect loop nest into a single
          loop that performs the same iterations as the original
          loop nest.
         */
        class LIBHLT_CLASS LoopCollapse : public BaseTransform
        {
            private:
                ForNestInfo _for_nest_info;
            protected:
                virtual Source get_source();
            public:
                //! Constructs a LoopCollapse object
                /*!
                  \param for_nest Perfect loop nest
                  */
                LoopCollapse(ForStatement for_nest);
        };

        //! Constructs a LoopCollapse object
        /*!
          \param for_stmt Perfect loop nest
         */
        LIBHLT_EXTERN LoopCollapse loop_collapse(ForStatement for_stmt);

        //! @}
    }
}

#endif // HLT_COLLAPSE_HPP
