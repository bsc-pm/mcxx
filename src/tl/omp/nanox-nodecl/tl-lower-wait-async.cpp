#include "tl-lowering-visitor.hpp"

namespace TL { namespace Nanox {

void LoweringVisitor::visit(const Nodecl::Parallel::WaitAsyncsShallow& construct)
{
    std::cerr << "FOUND WAIT ASYNC AT " << construct.get_locus() << std::endl;
}

} }
