#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::master_postorder(OpenMP::MasterConstruct master_construct)
    {
        Source master_source;

        Statement statement = master_construct.body();

        master_source
            << "if (in__tone_is_master_())"
            << "{"
            <<    statement.prettyprint()
            << "}"
            ;

        AST_t master_tree = master_source.parse_statement(master_construct.get_ast(),
                master_construct.get_scope_link());

        master_construct.get_ast().replace(master_tree);
    }
}
