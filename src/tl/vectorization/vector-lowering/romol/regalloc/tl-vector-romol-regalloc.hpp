#ifndef TL_VECTOR_ROMOL_REGALLOC_HPP
#define TL_VECTOR_ROMOL_REGALLOC_HPP

#include "tl-nodecl-visitor.hpp"

namespace TL { namespace Vectorization {

    class RomolVectorRegAlloc : public Nodecl::NodeclVisitor<void>
    {
        public:
            virtual void visit(const Nodecl::TopLevel& node);
            virtual void visit(const Nodecl::FunctionCode& node);
    };

} }

#endif // TL_VECTOR_ROMOL_REGALLOC_HPP
