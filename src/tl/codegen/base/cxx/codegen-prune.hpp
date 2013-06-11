#ifndef CODEGEN_PRUNE_HPP
#define CODEGEN_PRUNE_HPP

#include "tl-nodecl-visitor.hpp"

#include <set>

namespace Codegen
{
    class PruneVLAVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        private:
            std::set<TL::Symbol> _used_symbols;
            std::set<TL::Type> _visited_types;
            TL::ObjectList<Nodecl::NodeclBase> _saved_expressions;
        public:

            virtual void visit(const Nodecl::Symbol& sym);
            virtual void visit(const Nodecl::ObjectInit& n);
            virtual void visit(const Nodecl::Cast& n);
            virtual void visit(const Nodecl::FunctionCode& function_code);

            void walk_type(TL::Type t);
    };
}

#endif // CODEGEN_PRUNE_HPP
