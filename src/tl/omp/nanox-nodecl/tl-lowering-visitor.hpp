#include "tl-nodecl-visitor.hpp"

namespace TL { namespace Nanox {

class LoweringVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        LoweringVisitor();
        void visit(const Nodecl::Parallel::Async& construct);
        void visit(const Nodecl::Parallel::WaitAsyncsShallow& construct);

    private:
        std::string declare_argument_structure(const Nodecl::NodeclBase &environment);

        void emit_outline(Nodecl::NodeclBase environment, 
                Nodecl::NodeclBase body,
                const std::string& outline_name,
                const std::string& structure_name);
};

} }
