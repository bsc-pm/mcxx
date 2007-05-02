#include "tl-omptransform.hpp"

namespace TL
{
    void OpenMPTransform::ordered_postorder(OpenMP::OrderedConstruct ordered_construct)
    {
        IdExpression induction_var = induction_var_stack.top();

        Statement construct_body = ordered_construct.body();
        Source ordered_source;

        ordered_source
            << "{"
            <<   "in__tone_enter_ordered_ (& "<< induction_var.prettyprint() << ");"
            <<   construct_body.prettyprint()
            <<   "in__tone_leave_ordered_ (&" << induction_var.prettyprint() << ");"
            << "}"
            ;

        AST_t ordered_code = ordered_source.parse_statement(ordered_construct.get_ast(),
                ordered_construct.get_scope_link());

        ordered_construct.get_ast().replace(ordered_code);
    }
}

