#ifndef CODEGEN_CXX_HPP
#define CODEGEN_CXX_HPP

#include "codegen-phase.hpp"
#include "tl-scope.hpp"
#include "tl-symbol.hpp"
#include <sstream>
#include <map>
#include <set>

namespace Codegen
{
    class CxxLegacy : public CodegenPhase
    {
        protected:
            virtual std::string codegen(const Nodecl::NodeclBase&);
    };
}

#endif // CODEGEN_CXX_HPP
