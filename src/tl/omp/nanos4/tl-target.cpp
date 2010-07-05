#include "tl-omptransform.hpp"

using namespace TL;
using namespace TL::Nanos4;

void OpenMPTransform::target_postorder(PragmaCustomConstruct construct)
{
    running_error("%s: error: '#pragma omp target' is not supported in Nanos 4",
            construct.get_ast().get_locus().c_str());
}

void OpenMPTransform::target_preorder(PragmaCustomConstruct construct)
{
}
