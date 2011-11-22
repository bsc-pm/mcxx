#include "tl-lowering-visitor.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::Async& construct)
{
    std::cerr << "FOUND ASYNC AT " << construct.get_locus() << std::endl;
}

} }
