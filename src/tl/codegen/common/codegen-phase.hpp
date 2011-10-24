#ifndef CODEGEN_PHASE_HPP
#define CODEGEN_PHASE_HPP

#include "tl-compilerphase.hpp"
#include "codegen-common.hpp"

namespace Codegen
{
    class CodegenPhase : public TL::CompilerPhase, public CodegenVisitor
    {
        virtual void run(TL::DTO& dto);
    };
}

#endif // CODEGEN_PHASE_HPP
