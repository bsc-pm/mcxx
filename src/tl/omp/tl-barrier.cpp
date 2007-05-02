#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::barrier_postorder(OpenMP::BarrierDirective barrier_directive)
    {
        Source barrier_source;

        Source instrumentation_code_before, instrumentation_code_after;

        if (instrumentation_requested())
        {
            instrumentation_code_before
                << "int __previous_state = mintaka_get_state();"
                << "mintaka_state_synch();"
                ;

            instrumentation_code_after
                << "mintaka_set_state(__previous_state);"
                ;
        }

        barrier_source
            << "{"
            //                    <<    "extern void in__tone_barrier_();"
            <<    instrumentation_code_before
            <<    "in__tone_barrier_();"
            <<    instrumentation_code_after
            << "}"
            ;

        AST_t barrier_tree = barrier_source.parse_statement(barrier_directive.get_ast(),
                barrier_directive.get_scope_link());

        barrier_directive.get_ast().replace(barrier_tree);
    }
}

