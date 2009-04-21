#include "hlt-composition.hpp"

using namespace TL::HLT;

TL::ObjectList<TL::ForStatement> TL::HLT::get_all_sibling_for_statements(TL::Statement st)
{
    ObjectList<ForStatement> result;
    if (ForStatement::predicate(st.get_ast()))
    {
        result.append(ForStatement(st.get_ast(), st.get_scope_link()));
    }
    else if (st.is_compound_statement())
    {
        ObjectList<Statement> inner_stmt = st.get_inner_statements();
        for (ObjectList<Statement>::iterator it = inner_stmt.begin();
                it != inner_stmt.end();
                it++)
        {
            result.append(get_all_sibling_for_statements(*it));
        }
    }

    return result;
}
