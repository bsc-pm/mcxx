#include "tl-nanox-nodecl.hpp"
#include "tl-lowering-visitor.hpp"

namespace TL { namespace Nanox {

    Lowering::Lowering()
    {
        set_phase_name("Nanos++ lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR into real code involving Nanos++ runtime interface");
    }

    void Lowering::run(DTO& dto)
    {
        std::cerr << "Nanos++ phase" << std::endl;

        Nodecl::NodeclBase n = dto["nodecl"];

        LoweringVisitor lowering_visitor;
        lowering_visitor.walk(n);
    }

} }


EXPORT_PHASE(TL::Nanox::Lowering);
