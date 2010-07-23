#include "tl-omp-nanox.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::target_postorder(PragmaCustomConstruct ctr)
{
    // We allow this to appear both for statements and declarations
    Statement stmt = ctr.get_statement();
    if (stmt.get_ast().is_valid())
    {
        ctr.get_ast().replace(stmt.get_ast());
    }
    else
    {
        ctr.get_ast().replace(ctr.get_declaration());
    }
}
