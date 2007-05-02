#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::section_postorder(OpenMP::SectionConstruct section_construct)
    {
        int &num_sections = num_sections_stack.top();

        Source section_source, instrumentation_before, instrumentation_after;
        Statement construct_body = section_construct.body();

        section_source
            << "case " << num_sections << ":"
            << "{"
            <<    instrumentation_before
            <<    construct_body.prettyprint()
            <<    instrumentation_after
            <<    "break;"
            << "}"
            ;

        if (instrumentation_requested())
        {
            instrumentation_before
                << "mintaka_state_run();"
                ;

            instrumentation_before
                << "mintaka_state_synch();"
                ;
        }

        AST_t section_tree = section_source.parse_statement(section_construct.get_ast(),
                section_construct.get_scope_link());

        // One more section
        num_sections++;

        section_construct.get_ast().replace(section_tree);
    }
}
