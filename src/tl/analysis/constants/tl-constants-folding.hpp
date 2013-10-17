#ifndef TL_CONSTANTS_FOLDING_HPP
#define TL_CONSTANTS_FOLDING_HPP

#include "tl-nodecl.hpp"

namespace TL { namespace Analysis {

    Nodecl::NodeclBase fold_constants(Nodecl::NodeclBase expression);

} }

#endif // TL_CONSTANTS_FOLDING_HPP
