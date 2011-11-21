#include "tl-nanox-nodecl.hpp"

namespace TL { namespace Nanox {

    Lowering::Lowering()
    {
        set_phase_name("Nanos++ lowering");
        set_phase_description("This phase lowers from Mercurium parallel IR into real code involving Nanos++ runtime interface");
    }

    void Lowering::run(DTO& dto)
    {
        std::cerr << "Nanos++ phase" << std::endl;
    }

} }


EXPORT_PHASE(TL::Nanox::Lowering);
