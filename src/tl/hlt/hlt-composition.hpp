#ifndef HLT_COMPOSITION
#define HLT_COMPOSITION

#include "hlt-common.hpp"
#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        //! \addtogroup HLT High Level Transformations
        //! @{
        /*!
          \internal 
          */
        LIBHLT_EXTERN ObjectList<ForStatement> get_all_sibling_for_statements(Statement st);
        //! @}
    }
}

#endif // HLT_COMPOSITION
