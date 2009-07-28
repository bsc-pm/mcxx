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
#include "hlt-unroll.hpp"
#include <sstream>

using namespace TL::HLT;

LoopUnroll TL::HLT::unroll_loop(TL::ForStatement for_stmt, unsigned int factor)
{
    return LoopUnroll(for_stmt, factor);
}


TL::Source LoopUnroll::get_source()
{
    // Nothing at the moment
    return do_unroll();
}

LoopUnroll::LoopUnroll(ForStatement for_stmt, unsigned int factor)
     : _for_stmt(for_stmt), _factor(factor), _with_epilog(false)
{
    if (!_for_stmt.regular_loop())
    {
        _ostream
            << _for_stmt.get_ast().get_locus() 
            << ": warning: is not a regular loop, unroll will not be applied" 
            << std::endl;
        set_identity(_for_stmt.get_ast());
    }
}

static bool there_is_declaration(TL::Statement st)
{
    if (st.is_compound_statement())
    {
        TL::ObjectList<TL::Statement> list = st.get_inner_statements();
        for (TL::ObjectList<TL::Statement>::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if (TL::Declaration::predicate(it->get_ast()))
                return true;
        }
    }
    
    return false;
}

TL::Source LoopUnroll::do_unroll()
{
    // Get parts of the loop
    IdExpression induction_var = _for_stmt.get_induction_variable();
    Expression lower_bound = _for_stmt.get_lower_bound();
    Expression upper_bound = _for_stmt.get_upper_bound();
    Expression step = _for_stmt.get_step();
    TL::Source operator_bound = _for_stmt.get_bound_operator();

    Statement loop_body = _for_stmt.get_loop_body();

    TL::Source result, epilogue, main, induction_var_decl;

    std::stringstream ss;
    ss << _factor;

    result
        << "{"
        << induction_var_decl
        << main
        << epilogue
        << "}"
        ;

    AST_t init = _for_stmt.get_iterating_init();
    if (Declaration::predicate(init))
    {
        TL::Symbol sym = induction_var.get_symbol();
        TL::Type type = sym.get_type();
        // Declare it since it will have local scope
        induction_var_decl
            << type.get_declaration(sym.get_scope(), sym.get_name()) << ";"
            ;
    }

    Source replicated_body;
    main
        << "for (" << induction_var << " = " << lower_bound << ";"
        << induction_var << operator_bound << "((" << upper_bound << ") - (" << _factor << " - 1)* (" << step << "));"
        << induction_var << "+= (" << step << ") * " << _factor << ")"
        << "{"
        << replicated_body
        << "}"
        ;

    // FIXME - It could help to initialize here another variable and make both loops independent
    epilogue
        << "for ( ; "  // No initialization, keep using the old induction var
                   << induction_var << operator_bound << upper_bound << ";"
                   << induction_var << "+= (" << step << "))"
                   << loop_body
        ;

    // Replicate the body
    for (unsigned int i = 0; i < _factor; i++)
    {
        ReplaceSrcIdExpression replacement(_for_stmt.get_scope_link());
        replacement.set_ignore_pragma(true);
        if (i > 0)
        {
            std::stringstream ss;
            ss << induction_var << " + " << i;
            replacement.add_replacement(induction_var.get_symbol(), ss.str());
        }

        if (!loop_body.is_compound_statement()
                || there_is_declaration(loop_body))
        {
            replicated_body
                << replacement.replace(loop_body)
                ;
        }
        else
        {
            ObjectList<Statement> list = loop_body.get_inner_statements();
            for (ObjectList<Statement>::iterator it = list.begin();
                    it != list.end();
                    it++)
            {
                replicated_body
                    << replacement.replace(*it)
                    ;
            }
        }
    }

    return result;
}
