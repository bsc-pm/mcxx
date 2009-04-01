#include "tl-for-nest.hpp"

#include <utility>

// This is a helper routine for the reduction used in is_all_regular
static bool for_statement_and(std::pair<TL::ForStatement, bool> p)
{
    return (p.second && p.first.is_regular_loop());
}

namespace TL
{
    ForNestInfo::ForNestInfo(ForStatement for_stmt)
        : _is_perfect(false), _for_nest(), _for_stmt(for_stmt)
    {
        gather_nest_info();
    }

    ObjectList<ForStatement> ForNestInfo::get_nest_list()
    {
        return _for_nest;
    }

    bool ForNestInfo::is_perfect()
    {
        return _is_perfect;
    }

    bool ForNestInfo::is_all_regular()
    {
        bool result = _for_nest.reduction(
                functor(for_statement_and), 
                /* neuter */ true);
        return result;
    }

    void ForNestInfo::gather_nest_info()
    {
        // The original for statement used to create a ForNestInfo is always in the nest
        _for_nest.append(_for_stmt);

        ForStatement current_for_stmt(_for_stmt);
        bool valid_nest = true;

        while (valid_nest)
        {
            // has_nested_for updates 'current_for_stmt' with the newly found for-statement if 
            // the nest is still valid
            valid_nest = has_nested_for(current_for_stmt);
            if (valid_nest)
            {
                _for_nest.append(current_for_stmt);
            }
        }
    }

    bool ForNestInfo::has_nested_for(ForStatement &for_stmt)
    {
        Statement stmt = for_stmt.get_loop_body();

        AST_t result(NULL);
        if (contains_a_for_statement(stmt, result))
        {
            for_stmt = ForStatement(result, for_stmt.get_scope_link());
            return true;
        }
        else if (stmt.is_compound_statement())
        {
            bool valid_sequence = true;
            ObjectList<Statement> inner_list = stmt.get_inner_statements();
            for (ObjectList<Statement>::iterator it = inner_list.begin();
                    it != inner_list.end() && valid_sequence;
                    it++)
            {
                if (contains_a_for_statement(*it, result))
                {
                    // We know that all statements till this one were not
                    // for-statements. Check that the remaining are not either

                    // This kind of checks are O(n^2), so expensive
                    for (ObjectList<Statement>::iterator it2 = it;
                            it2 != inner_list.end() && valid_sequence;
                            it2++)
                    {
                        if (contains_a_for_statement(*it2))
                        {
                            // This list of statements contains more than one
                            // for-statement, thus it is not a stacked loop
                            // nest (but a tree-alike one)
                            valid_sequence = false;
                        }
                    }

                    if (valid_sequence)
                    {
                        for_stmt = ForStatement(result, for_stmt.get_scope_link());
                        return true;
                    }
                }
            }
        }

        return false;
    }

    bool ForNestInfo::contains_a_for_statement(Statement stmt)
    {
        AST_t result(NULL);
        return contains_a_for_statement(stmt, result);
    }

    bool ForNestInfo::contains_a_for_statement(Statement stmt, AST_t &result)
    {
        if (ForStatement::predicate(stmt.get_ast()))
        {
            result = stmt.get_ast();
            return true;
        }
        else if (stmt.is_compound_statement())
        {
            ObjectList<Statement> inner_list = stmt.get_inner_statements();

            if (inner_list.size() == 1)
            {
                return contains_a_for_statement(inner_list[0]);
            }
        }
        return false;
    }
}
