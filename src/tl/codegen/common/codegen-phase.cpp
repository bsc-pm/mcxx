#include "codegen-phase.hpp"
#include "tl-builtin.hpp"

namespace Codegen
{
    void CodegenPhase::run(TL::DTO& dto)
    {
        TL::File output_file = dto["output_file"];
        FILE* f = output_file.get_file();

        Nodecl::NodeclBase n = dto["nodecl"];

        this->codegen_top_level(n, f);
    }

}

Codegen::CodegenPhase& Codegen::get_current()
{
    CodegenPhase* result = reinterpret_cast<CodegenPhase*>(CURRENT_CONFIGURATION->codegen_phase);

    return *result;
}
