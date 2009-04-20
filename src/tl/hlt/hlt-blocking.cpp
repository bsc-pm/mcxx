/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2009 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "hlt-blocking.hpp"
#include "hlt-exception.hpp"
#include <utility>

using namespace TL::HLT;

TL::Source LoopBlocking::get_source()
{
    return do_blocking();
}

LoopBlocking::LoopBlocking(ForStatement for_stmt, ObjectList<Expression> block_factors)
    : _for_stmt(for_stmt), 
    _nesting(block_factors.size()), 
    _nest_factors(block_factors), 
    _for_nest_info(for_stmt)
{
    if (!check_nesting())
    {
        _ostream << _for_stmt.get_ast().get_locus() << ": warning: blocking not performed" << std::endl;
        set_identity(_for_stmt.get_ast());
    }
}

TL::Source LoopBlocking::do_blocking()
{
    Source result, block_loops;

    result
        << block_loops
        ;

    ObjectList<ForStatement> nest_loops = _for_nest_info.get_nest_list();

    _nesting = std::min(_nest_factors.size(), nest_loops.size());

    TL::Source *current_innermost_part = &block_loops;
    // For every loop declare its block loop variable and the inter-block loop
    ObjectList<TL::Expression>::iterator current_factor = _nest_factors.begin();
    ObjectList<TL::ForStatement>::iterator current_for = nest_loops.begin();
    for (int current_nest = 0;
            current_nest < _nesting;
            current_nest++, current_for++, current_factor++)
    {
        TL::IdExpression induction_var = current_for->get_induction_variable();
        TL::Symbol sym = induction_var.get_symbol();
        TL::Type type = sym.get_type();

        std::string var = "_blk_" + sym.get_name();

        TL::Source *new_innermost_part = new TL::Source();
        (*current_innermost_part)
            << "for(" << type.get_declaration(sym.get_scope(), var) << " = " << current_for->get_lower_bound() << ";"
                      << var << current_for->get_bound_operator() << current_for->get_upper_bound() << ";"
                      << var << "+= ( " << current_for->get_step() << ") * " << current_factor->prettyprint() << ")" 
            << (*new_innermost_part)
            ;

        current_innermost_part = new_innermost_part;
    }

    // Now for every loop, declare the intra-loop
    current_factor = _nest_factors.begin();
    current_for = nest_loops.begin();
    for (int current_nest = 0;
            current_nest < _nesting;
            current_nest++, current_for++, current_factor++)
    {
        TL::IdExpression induction_var = current_for->get_induction_variable();
        TL::Symbol sym = induction_var.get_symbol();
        TL::Type type = sym.get_type();

        std::string var = induction_var.prettyprint();
        std::string init_var = var;
        // If the loop declares the iterator in the for statement
        // declare it again
        AST_t loop_init = current_for->get_iterating_init();
        if (Declaration::predicate(loop_init))
        {
            // Fix init_var to be a declaration
            init_var = type.get_declaration(sym.get_scope(), var);
        }

        std::string blk_var = "_blk_" + sym.get_name();

        TL::Source min_code;

        TL::Source *new_innermost_part = new TL::Source();
        (*current_innermost_part)
            << "for(" << init_var << " = " << blk_var << ";"
                      << var << current_for->get_bound_operator() << min_code  << ";"
                      << var << "+= ( " << current_for->get_step() << "))" 
            << (*new_innermost_part)
            ;

        TL::Source a, b;
        min_code
            << "((" << a << ") < (" << b << ") ? (" << a << ") : (" << b << "))"
            ;

        a << blk_var << " * (" << current_for->get_step() << ") * (" << current_factor->prettyprint() << " - 1 )";
        b << current_for->get_upper_bound();

        current_innermost_part = new_innermost_part;
    }

    // And now the innermost loop
    (*current_innermost_part)
        << nest_loops[_nesting - 1].get_loop_body()
        ;

    return result;
}

bool LoopBlocking::check_nesting()
{
    int found_nesting = 0;
    ObjectList<ForStatement> for_nest_list = _for_nest_info.get_nest_list();
    for (ObjectList<ForStatement>::iterator it = for_nest_list.begin();
            it != for_nest_list.end();
            it++)
    {
        if (it->is_regular_loop())
        {
            found_nesting++;
        }
        else
        {
            break;
        }
    }

    if (_nesting != 0 
            && _nesting > found_nesting)
    {
        _ostream << _for_stmt.get_ast().get_locus() << ": warning: given nest of " << _nesting 
            << " is bigger than the real nesting of " << found_nesting << std::endl;
        return false;
    }
    else if (_nesting == 0)
    {
        _ostream << _for_stmt.get_ast().get_locus() << ": notice: considering a nest of " << found_nesting << " when blocking" << std::endl;
    }
    _nesting = found_nesting;

    return true;
}

LoopBlocking TL::HLT::block_loop(TL::ForStatement for_stmt, ObjectList<TL::Expression> block_factors)
{
    LoopBlocking result(for_stmt, block_factors);
    return result;
}
