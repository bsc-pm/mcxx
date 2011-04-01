#include "tl-nanos.hpp"
#include "tl-omp-nanox.hpp"

namespace TL { namespace Nanox {

    Source OMPTransform::get_wait_completion(Source arg, bool do_flush)
    {
        Source src;
        if (Nanos::Version::interface_is_at_least("master", 5006))
        {
            src << "nanos_wg_wait_completion(" << arg << "," << (do_flush ? "1" : "0") << ");"
                ;
        }
        else
        {
            src << "nanos_wg_wait_completion(" << arg << ");"
                ;
        }

        return src;
    }

} }
