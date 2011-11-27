#include "tl-nodecl-visitor.hpp"
#include "tl-outline-info.hpp"

namespace TL { namespace Nanox {

class LoweringVisitor : public Nodecl::ExhaustiveVisitor<void>
{
    public:
        LoweringVisitor();
        void visit(const Nodecl::Parallel::Async& construct);
        void visit(const Nodecl::Parallel::WaitAsyncsShallow& construct);

    private:
        std::string declare_argument_structure(OutlineInfo& outline_info, Nodecl::NodeclBase construct);

        void emit_outline(OutlineInfo& outline_info,
                Nodecl::NodeclBase body,
                const std::string& outline_name,
                const std::string& structure_name);
};

} }
