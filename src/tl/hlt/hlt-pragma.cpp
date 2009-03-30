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
#include "hlt-blocking.hpp"
#include "hlt-distribution.hpp"
#include "hlt-fusion.hpp"
#include "hlt-exception.hpp"

using namespace TL::HLT;

HLTPragmaPhase::HLTPragmaPhase()
    : PragmaCustomCompilerPhase("hlt")
{
    set_phase_name("High Level Transformations");
    set_phase_description("This phase implement several high level "
            "transformations available through the usage of #pragma hlt");

    register_construct("unroll");
    on_directive_post["unroll"].connect(functor(&HLTPragmaPhase::unroll_loop, *this));

    register_construct("block");
    on_directive_post["block"].connect(functor(&HLTPragmaPhase::block_loop, *this));

    register_construct("distribute");
    on_directive_post["distribute"].connect(functor(&HLTPragmaPhase::distribute_loop, *this));

    register_construct("fusion");
    on_directive_post["fusion"].connect(functor(&HLTPragmaPhase::fuse_loops, *this));
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

    if (!ForStatement::predicate(statement.get_ast()))
    {
        throw HLTException(construct, "'#pragma hlt unroll' can only be used with for-statements");
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
        std::cerr << construct.get_ast().get_locus() << ": warning: no factor clause given for unrolling, assuming 'factor(32)'" << std::endl;
    }

    ForStatement for_stmt(statement.get_ast(), statement.get_scope_link());
    TL::Source unrolled_loop_src = HLT::unroll_loop(for_stmt,  unroll_factor);

    AST_t unrolled_loop_tree = unrolled_loop_src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(unrolled_loop_tree);
}

void HLTPragmaPhase::block_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    if (!ForStatement::predicate(statement.get_ast()))
    {
        throw HLTException(construct, "'#pragma hlt block' can only be used with for-statements");
    }

    TL::PragmaCustomClause factors_clause = construct.get_clause("factors");

    if (!factors_clause.is_defined())
    {
        throw HLTException(construct, "'#pragma hlt block' requires a clause 'factors' with a list of block factors");
    }

    TL::ObjectList<TL::Expression> factors_list = factors_clause.get_expression_list();

    ForStatement for_stmt(statement.get_ast(), statement.get_scope_link());
    TL::Source blocked_loop_src = HLT::block_loop(for_stmt, factors_list);

    AST_t blocked_loop_tree = blocked_loop_src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(blocked_loop_tree);
}

void HLTPragmaPhase::distribute_loop(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    if (!ForStatement::predicate(statement.get_ast()))
    {
        throw HLTException(construct, "'#pragma hlt distribute' can only be used with for-statements");
    }

    ForStatement for_stmt(statement.get_ast(), statement.get_scope_link());

    PragmaCustomClause expanded_scalars = construct.get_clause("expand");

    ObjectList<Symbol> expanded_syms;
    if (expanded_scalars.is_defined())
    {
        ObjectList<IdExpression> id_expression_list = expanded_scalars.id_expressions();

        if (!id_expression_list.empty())
        {
            if (!for_stmt.is_regular_loop())
            {
                throw HLTException(construct, 
                        "'#pragma hlt distribute' when scalar expansion is requested can "
                        "only be used with regular for-statements");
            }
        }
        expanded_syms.insert(id_expression_list.map(functor(&IdExpression::get_symbol)));
    }

    TL::Source distributed_loop_src = HLT::distribute_loop(for_stmt, expanded_syms);

    AST_t distributed_loop_tree = distributed_loop_src.parse_statement(construct.get_ast(),
            construct.get_scope_link());

    construct.get_ast().replace(distributed_loop_tree);
}

void HLTPragmaPhase::fuse_loops(PragmaCustomConstruct construct)
{
    Statement statement = construct.get_statement();

    if (!statement.is_compound_statement())
    {
        throw HLTException(construct, 
                "'#pragma hlt fusion' must be applied to a compound statement");
    }

    ObjectList<Statement> statement_list = statement.get_inner_statements();

    ObjectList<ForStatement> for_statement_list;

    for (ObjectList<Statement>::iterator it = statement_list.begin();
            it != statement_list.end();
            it++)
    {
        if (ForStatement::predicate(it->get_ast()))
        {
            for_statement_list.append(ForStatement(it->get_ast(), it->get_scope_link()));
        }
    }

    if (for_statement_list.empty())
    {
        throw HLTException(construct,
                "'#pragma hlt fusion' must be applied to a compound statement that contains one or more regular for-statements");
    }

    TL::Source fused_loops_src = HLT::loop_fusion(for_statement_list);

    TL::AST_t fused_loops_tree = fused_loops_src.parse_statement(
            for_statement_list[0].get_ast(),
            construct.get_scope_link());

    // Now remove all the for statements but the first which is replaced,
    // actually
    {
        ObjectList<ForStatement>::iterator it = for_statement_list.begin();

        it->get_ast().replace(fused_loops_tree);
        it++;

        for (; it != for_statement_list.end(); it++)
        {
            it->get_ast().remove_in_list();
        }
    }
}

EXPORT_PHASE(TL::HLT::HLTPragmaPhase)
