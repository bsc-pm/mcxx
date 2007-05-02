#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::flush_postorder(OpenMP::FlushDirective flush_directive)
    {
        Source flush_source;

        flush_source
            << "{"
            //                    <<    "extern void synchronize();"
            <<    "synchronize();"
            << "}"
            ;

        AST_t flush_tree = flush_source.parse_statement(flush_directive.get_ast(),
                flush_directive.get_scope_link());

        flush_directive.get_ast().replace(flush_tree);
    }
}
