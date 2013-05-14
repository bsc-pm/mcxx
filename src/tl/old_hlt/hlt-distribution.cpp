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




#include "hlt-distribution.hpp"

using namespace TL::HLT;

LoopDistribution::LoopDistribution(TL::ForStatement for_stmt)
    : _for_stmt(for_stmt)
{
}

LoopDistribution::LoopDistribution(TL::ForStatement for_stmt, 
        TL::ObjectList<TL::Symbol> expanded)
: _for_stmt(for_stmt), _expand(expanded)
{
}

TL::Source LoopDistribution::do_distribution()
{
    TL::Statement loop_body = _for_stmt.get_loop_body();

    if (!loop_body.is_compound_statement())
    {
        return _for_stmt.prettyprint();
    }

    TL::Source result;

    TL::Source distributed_loops, expanded_scalars;
    result
        << "{"
        << expanded_scalars
        << distributed_loops
        << "}"
        ;

    TL::ReplaceIdExpression escalar_expansion;
    if (!_expand.empty())
    {
        if (!_for_stmt.is_regular_loop())
        {
            return _for_stmt.prettyprint();
        }
        expanded_scalars
            << "int __loop_trip = (" << _for_stmt.get_upper_bound() << ") - (" << _for_stmt.get_lower_bound() << " + 1);"
            // Absolute value (calculated this way allows for conditional move instructions)
            << "__loop_trip = (__loop_trip < 0) ? (-__loop_trip) : __loop_trip;"
            ;
        // We need this because of array nature
        TL::AST_t array_expr;
        {
            TL::AST_t array_expr_ref_tree;
            Source temporal_parse;
            temporal_parse
                << "{"
                << expanded_scalars
                << statement_placeholder(array_expr_ref_tree)
                << "}"
                ;
            temporal_parse.parse_statement(_for_stmt.get_ast(),
                    _for_stmt.get_scope_link());

            array_expr = Source("__loop_trip").parse_expression(array_expr_ref_tree,
                    _for_stmt.get_scope_link());
        }

        for (TL::ObjectList<TL::Symbol>::iterator it = _expand.begin();
                it != _expand.end();
                it++)
        {
            TL::Symbol &sym(*it);

            std::string expanded_scalar_name = "_" + sym.get_name();

            TL::Type type = sym.get_type();
            TL::Type array_type = type.get_array_to(array_expr,
                    _for_stmt.get_scope_link().get_scope(array_expr));

            expanded_scalars
                << array_type.get_declaration(it->get_scope(), expanded_scalar_name) << ";";

            escalar_expansion.add_replacement(sym, 
                    expanded_scalar_name + "[" + _for_stmt.get_induction_variable().prettyprint() + "]" );
        }
    }


    TL::ObjectList<TL::Statement> statement_list = loop_body.get_inner_statements();

    for (TL::ObjectList<TL::Statement>::iterator it = statement_list.begin();
            it != statement_list.end();
            it++)
    {
        Statement &stmt(*it);
        distributed_loops
            << "for ( " << _for_stmt.get_iterating_init().prettyprint() // This one already includes ';'
            << _for_stmt.get_iterating_condition() << ";"
            << _for_stmt.get_iterating_expression() << ")"
            << "{"
            << escalar_expansion.replace(stmt)
            << "}"
            ;
    }

    return result;
}

TL::Source LoopDistribution::get_source()
{
    return do_distribution();
}

LoopDistribution TL::HLT::distribute_loop(TL::ForStatement for_stmt, TL::ObjectList<TL::Symbol> expanded_scalars)
{
    LoopDistribution result(for_stmt, expanded_scalars);
    return result;
}
