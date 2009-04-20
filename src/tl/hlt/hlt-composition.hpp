#ifndef HLT_COMPOSITION
#define HLT_COMPOSITION

#include "tl-langconstruct.hpp"

namespace TL
{
    namespace HLT
    {
        ObjectList<ForStatement> get_all_sibling_for_statements(Statement st);
    }
}

#endif // HLT_COMPOSITION
