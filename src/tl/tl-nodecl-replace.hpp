#ifndef TL_NODECL_REPLACE_HPP
#define TL_NODECL_REPLACE_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace Nodecl
{
    class BaseReplaceSymbols : public NodeclVisitor<void>
    {
        public:
            virtual void visit(const Nodecl::Symbol& nodecl) = 0;
    };

    class ReplaceSymbols : public BaseReplaceSymbols
    {
    };
}

#endif // TL_NODECL_REPLACE_HPP
