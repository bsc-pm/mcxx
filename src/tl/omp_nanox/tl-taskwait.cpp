#include "tl-omp-nanox.hpp"

namespace TL
{
    namespace Nanox
    {
        void OMPTransform::taskwait_postorder(PragmaCustomConstruct ctr)
        {
            Source src;
            src 
                << "nanos_wg_wait_completation(nanos_current_wd());"
                ;

            AST_t tree = src.parse_statement(ctr.get_ast(), ctr.get_scope_link());
            ctr.get_ast().replace(tree);
        }
    }
}
