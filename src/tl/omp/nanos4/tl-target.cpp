#include "tl-omptransform.hpp"

using namespace TL;
using namespace TL::Nanos4;

void OpenMPTransform::target_postorder(PragmaCustomConstruct construct)
{
    std::cerr << construct.get_ast().get_locus() << ": warning: '#pragma omp target' is not supported in Nanos 4'" << std::endl;
    std::cerr << construct.get_ast().get_locus() << ": warning: no processing will be done for this pragma" << std::endl;
}

void OpenMPTransform::target_preorder(PragmaCustomConstruct construct)
{
    construct.get_ast().replace(construct.get_declaration());
}
