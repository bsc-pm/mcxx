#include "tl-example-visitor.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {

    VisitorExamplePhase::VisitorExamplePhase()
    {
    }

    VisitorExamplePhase::~VisitorExamplePhase()
    {
    }

    class SimpleExhaustiveVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
        public:

            virtual void visit_pre(const Nodecl::IfElseStatement &node)
            {
                std::cerr << "Entering the if-statement at " << node.get_locus_str() << std::endl;
            }

            virtual void visit_post(const Nodecl::IfElseStatement &node)
            {
                std::cerr << "Leaving the if-statement at " << node.get_locus_str() << std::endl;
            }
    };

    void VisitorExamplePhase::run(TL::DTO& dto)
    {
        Nodecl::NodeclBase top_level = dto["nodecl"];

        SimpleExhaustiveVisitor simple_exhaustive_visitor;
        simple_exhaustive_visitor.walk(top_level);
    }
}

EXPORT_PHASE(TL::VisitorExamplePhase);
