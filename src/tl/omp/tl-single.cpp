#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::single_postorder(OpenMP::SingleConstruct single_construct)
    {
        Source single_source;
        Source barrier_code;

        Statement body_construct = single_construct.body();
        OpenMP::Directive directive = single_construct.directive();

        Source instrumentation_code_before, instrumentation_code_after;

        single_source
            << "{"
            <<   instrumentation_code_before
            <<   "int nth_low;"
            <<   "int nth_upper;"
            <<   "int nth_step;"
            <<   "int nth_chunk;"
            <<   "int nth_schedule;"
            <<   "int nth_dummy1;"
            <<   "int nth_dummy2;"
            <<   "int nth_dummy3;"
            <<   "int nth_barrier; "

            <<   "nth_low = 0;"
            <<   "nth_upper = 0;"
            <<   "nth_step = 1;"
            <<   "nth_schedule = 2;" // Dynamic
            <<   "nth_chunk = 1;"

            //                    <<   "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
            //                    <<   "extern int in__tone_next_iters_(int*, int*, int*);"
            //                    <<   "extern void in__tone_end_for_(int*);"

            <<   "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
            <<   "while (in__tone_next_iters_ (&nth_dummy1, &nth_dummy2, &nth_dummy3) != 0)"
            <<   "{"
            <<       instrumentation_code_after
            <<       body_construct.prettyprint()
            <<       instrumentation_code_before
            <<   "}"
            <<   barrier_code
            <<   instrumentation_code_after
            << "}"
            ;

        if (instrumentation_requested())
        {
            instrumentation_code_before
                << "mintaka_state_synch();"
                ;
            instrumentation_code_after
                << "mintaka_state_run();"
                ;
        }

        OpenMP::Clause nowait_clause = directive.nowait_clause();
        barrier_code = get_loop_finalization(!(nowait_clause.is_defined()));

        AST_t single_tree = single_source.parse_statement(single_construct.get_ast(), 
                single_construct.get_scope_link());

        single_construct.get_ast().replace(single_tree);
    }
}
