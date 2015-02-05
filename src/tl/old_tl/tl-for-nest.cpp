/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




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
        : _is_perfect(true), _for_nest(), _for_stmt(for_stmt)
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
            bool perfect_nest = true;
            valid_nest = has_nested_for(current_for_stmt, perfect_nest);
            if (valid_nest)
            {
                _for_nest.append(current_for_stmt);

                _is_perfect = _is_perfect && perfect_nest;
            }
        }
    }

    bool ForNestInfo::has_nested_for(ForStatement &for_stmt, bool &perfect_nest)
    {
        Statement stmt = for_stmt.get_loop_body();

        AST_t result(NULL);
        if (contains_a_for_statement(stmt, result))
        {
            perfect_nest = true;
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
                    for (ObjectList<Statement>::iterator it2 = it+1;
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
                        perfect_nest = false;
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
                return contains_a_for_statement(inner_list[0], result);
            }
        }
        return false;
    }
}
