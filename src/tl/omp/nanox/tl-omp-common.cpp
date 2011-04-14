#include "tl-nanos.hpp"
#include "tl-omp-nanox.hpp"

namespace TL { namespace Nanox {

    // Taskwait
    Source OMPTransform::get_wait_completion(Source arg, bool avoid_flush, AST_t ref_tree)
    {
        Source src;
        if (Nanos::Version::interface_is_at_least("master", 5006))
        {
            src << "nanos_wg_wait_completion(" << arg << "," << (avoid_flush ? "1" : "0") << ");"
                ;
        }
        else
        {
            if (avoid_flush)
            {
                std::cerr << ref_tree.get_locus() << ": warning: avoiding flush in wait is not supported in this runtime interface" << std::endl;
            }
            src << "nanos_wg_wait_completion(" << arg << ");"
                ;
        }

        return src;
    }

    // TODO : Barrier

} }
