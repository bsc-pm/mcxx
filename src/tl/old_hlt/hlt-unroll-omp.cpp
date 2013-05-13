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




#include "hlt-unroll.hpp"
#include "hlt-unroll-omp.hpp"
#include "tl-omp.hpp"

using namespace TL::HLT;
using namespace TL::OpenMP;

struct IfZeroTaskGenerator : public TL::Functor<TL::AST_t::callback_result, TL::AST_t>
{
    private:
		TL::ScopeLink _sl;
    public:
        IfZeroTaskGenerator(TL::ScopeLink sl)
            : _sl(sl)
        {

        }

        virtual TL::AST_t::callback_result do_(IfZeroTaskGenerator::ArgType a) const
        {
			TL::AST_t::callback_result result;

            if (is_pragma_custom_construct("omp", "task", a, _sl))
            {
				TL::Source output, clauses;

				TL::PragmaCustomConstruct task_construct(a, _sl);

                output << "#pragma omp task if(0)" << clauses << "\n"
                    << task_construct.get_statement().get_ast().prettyprint_with_callback(*this)
                    ;

				TL::ObjectList<std::string> clause_names = task_construct.get_clause_names();

                for (TL::ObjectList<std::string>::iterator it = clause_names.begin();
                        it != clause_names.end();
                        it++)
                {
                    if (*it != "if")
                    {
						TL::PragmaCustomClause clause = task_construct.get_clause(*it);

                        clauses << " " << *it
                            ;

						TL::ObjectList<std::string> arguments = clause.get_arguments();

                        if (!arguments.empty())
                        {
                            clauses << "(" << TL::concat_strings(arguments, ",") << ")"
                                ;
                        }
                    }
                }

                result.first = true;
                result.second = output.get_source();
            }
            else 
            {
                result.first = false;
            }
            return result;
        }
};

void LoopUnroll::omp_replication(int factor, 
        Source &replicated_body, 
        Source &epilog_body,
        IdExpression induction_var, Statement loop_body,
        Source &before, Source &after)
{
	if (_omp_bundling)
    {
        omp_replication_by_task_bundling(factor, replicated_body, 
                induction_var, loop_body, before, after);
    }
    else
    {
        // Plain task predication will be used instead
        omp_replication_by_task_aggregation(factor, replicated_body, induction_var, loop_body);
    }

    if (_omp_aggregate_epilog)
    {
        // We have to aggregate the epilog, currently we simply add an if clause to be 0
        Source new_epilog;

        new_epilog = loop_body.get_ast().prettyprint_with_callback(
				IfZeroTaskGenerator(loop_body.get_scope_link())
				);

        epilog_body = new_epilog;
    }
}

void LoopUnroll::omp_replication_by_task_bundling(int factor, Source& replicated_body,
        IdExpression induction_var, Statement loop_body,
        Source& before, Source &after)
{
    Source replication;
    Source aggregation;
    aggregation
        << "{"
        << replication
        << "}"
        ;

    Symbol induction_sym = induction_var.get_symbol();
    for (unsigned int i = 0; i < (unsigned int)factor; i++)
    {
        replication
            << flatten_compound(loop_body, i, induction_sym)
            ;
    }
    
    // Implement task aggregation
    AST_t tree = aggregation.parse_statement(loop_body.get_ast(),
            loop_body.get_scope_link());

    ASTIterator iterator = tree.get_list_iterator();
    Statement stmt(iterator.item(), loop_body.get_scope_link());

    TaskAggregation task_aggregation(stmt);
    task_aggregation.set_aggregation_method(TaskAggregation::BUNDLING)
        .set_global_bundling_source(before)
        .set_finish_bundling_source(after)
		.set_do_not_create_tasks(_remove_tasks)
		.set_timing(_timing)
        .set_enclosing_function_tree(_for_stmt.get_ast().get_enclosing_function_definition());

	if (_omp_bundling_factor > 0)
	{
		task_aggregation.set_bundling_amount(_omp_bundling_factor);
	}
    else
    {
		task_aggregation.set_bundling_amount(factor);
    }

    replicated_body = task_aggregation;
}

void LoopUnroll::omp_replication_by_task_aggregation(int factor, Source& replicated_body,
        IdExpression induction_var, Statement loop_body)
{
    Source replication;
    Source aggregation;
    aggregation
        << "{"
        << replication
        << "}"
        ;

    Symbol induction_sym = induction_var.get_symbol();
    for (unsigned int i = 0; i < (unsigned int)factor; i++)
    {
        replication
            << flatten_compound(loop_body, i, induction_sym)
            ;
    }
    
    // Implement task aggregation
    AST_t tree = aggregation.parse_statement(loop_body.get_ast(),
            loop_body.get_scope_link());

    ASTIterator iterator = tree.get_list_iterator();
    Statement stmt(iterator.item(), loop_body.get_scope_link());

    TaskAggregation task_aggregation(stmt);
    task_aggregation
		.set_do_not_create_tasks(_remove_tasks)
        .set_enclosing_function_tree(_for_stmt.get_ast().get_enclosing_function_definition());

    replicated_body = task_aggregation;
}

TL::Source LoopUnroll::flatten_compound(Statement stmt, int num, Symbol sym)
{
    Source result;

    ReplaceSrcIdExpression replacements(stmt.get_scope_link());
    replacements.set_ignore_pragma(true);

    // Induction var
    Source induction_var_rpl;
    induction_var_rpl
        << "(" << sym.get_name() << "+" << num << ")"
        ;
    replacements.add_replacement(sym, induction_var_rpl);

    if (!stmt.is_compound_statement())
    {
        result
            << replacements.replace(stmt)
            ;
    }
    else
    {
        ObjectList<Statement> stmt_list = stmt.get_inner_statements();
        for (ObjectList<Statement>::iterator it_current_stmt = stmt_list.begin();
                it_current_stmt != stmt_list.end();
                it_current_stmt++)
        {
            Statement &current_stmt(*it_current_stmt);

            if (current_stmt.is_simple_declaration())
            {
                Declaration decl = current_stmt.get_simple_declaration();

                ObjectList<DeclaredEntity> declaration_list = decl.get_declared_entities();

                for (ObjectList<DeclaredEntity>::iterator it_entity = declaration_list.begin();
                        it_entity != declaration_list.end();
                        it_entity++)
                {
                    DeclaredEntity &entity(*it_entity);
                    Symbol sym = entity.get_declared_symbol();

                    Source repl_src;
                    repl_src
                        << "_" << sym.get_name() << "_" << num
                        ;

                    replacements.add_replacement(sym, repl_src);
                }
            }

            result
                << replacements.replace(current_stmt)
                ;
        }
    }

    return result;
}

bool TL::HLT::there_is_declaration(TL::Statement st)
{
    if (st.is_compound_statement())
    {
        TL::ObjectList<TL::Statement> list = st.get_inner_statements();
        for (TL::ObjectList<TL::Statement>::iterator it = list.begin();
                it != list.end();
                it++)
        {
            if (it->is_declaration())
                return true;
        }
    }
    
    return false;
}

