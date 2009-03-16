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
#include "hlt-pragma.hpp"
#include "hlt-unroll.hpp"
#include "hlt-exception.hpp"

using namespace TL::HLT;

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase("hlt")
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implement several high level "
            "transformations available through the usage of #pragma hlt");

    // register_variable("hlt-quiet

    register_construct("unroll");
    on_directive_post["unroll"].connect(functor(&HLTPragmaPhase::unroll_loop, *this));
}

void HLTPragmaPhase::run(TL::DTO& dto)
{
    try
    {
        PragmaCustomCompilerPhase::run(dto);
    }
    catch (HLTException e)
    {
        std::cerr << e << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
    catch (...)
    {
        std::cerr << "(hlt-phase): error: unknown exception" << std::endl;
        set_phase_status(PHASE_STATUS_ERROR);
    }
}

void HLTPragmaPhase::unroll_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    AST_t statement_ast = statement.get_ast();
    if (!ForStatement::predicate(statement_ast))
    {
        throw HLTException(construct, "'#pragma hlt unroll' can only be used with for statements");
    }

    TL::PragmaCustomClause factor_clause = construct.get_clause("factor");

    int unroll_factor = 32;
    if (factor_clause.is_defined())
    {
        ObjectList<Expression> args = factor_clause.get_expression_list();

        if (args.size() != 1)
        {
            throw HLTException(construct, "factor clause only accepts one argument");
        }

        Expression& expr = args[0];

        if (!expr.is_constant())
        {
            throw HLTException(expr, "factor clause argument should be a constant expression");
        }

        bool valid = false;
        unroll_factor = expr.evaluate_constant_int_expression(valid);
        if (!valid)
        {
            throw HLTException(expr, "factor clause argument expression could not be evaluated");
        }
    }
    else
    {
        std::cerr << construct.get_ast().get_locus() << ": warning: no factor clause given for unrolling, assuming factor(32)" << std::endl;
    }

    ForStatement for_stmt(statement_ast, statement.get_scope_link());
    TL::Source unrolled_loop_src = HLT::unroll_loop(for_stmt,  unroll_factor);

    AST_t unrolled_loop_tree = unrolled_loop_src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(unrolled_loop_tree);
}

EXPORT_PHASE(TL::HLT::HLTPragmaPhase)
