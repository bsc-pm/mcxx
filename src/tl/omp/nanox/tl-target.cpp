#include "tl-omp-nanox.hpp"
#include "tl-devices.hpp"

using namespace TL;
using namespace TL::Nanox;

void OMPTransform::target_postorder(PragmaCustomConstruct ctr)
{
    ctr.get_ast().replace(ctr.get_statement().get_ast());
}
