/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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



#include "tl-taskaggregation.hpp"

namespace TL
{
    TaskAggregationPhase::TaskAggregationPhase()
        : _num_aggregations(0)
    {
        set_phase_name("Task Aggregation Phase");
        set_phase_name("Implements '#pragma omp while' in terms of OpenMP 3.0");

        register_construct("while");
        on_custom_construct_pre["while"].connect(functor(&TaskAggregationPhase::while_preorder, *this));
        on_custom_construct_post["while"].connect(functor(&TaskAggregationPhase::while_postorder, *this));

        // This will cause a warning when OpenMP phase is also loaded :D
        register_construct("task");
    }

    void TaskAggregationPhase::while_preorder(OpenMP::CustomConstruct custom_construct)
    {
        std::cerr << custom_construct.get_ast().get_locus() << ": note: '#pragma omp while' found" << std::endl;

        bool &valid_while = custom_construct.get_data<bool>("valid_while");

        ObjectList<Statement> &list_of_tasks 
            = custom_construct.get_data<ObjectList<Statement> >("list_of_tasks");
        ObjectList<Statement> &sequentiation_code 
            = custom_construct.get_data<ObjectList<Statement> >("sequentiation_code");

        Statement st = custom_construct.body();

		int &nesting_value = custom_construct.get_data<int>("nest");
		nesting_value = 1;

        OpenMP::Directive directive = custom_construct.directive();
        // nest clause
        {
            OpenMP::CustomClause nest_clause = directive.custom_clause("nest");

            ObjectList<Expression> expression_list;
            if (nest_clause.is_defined())
            {
                expression_list = nest_clause.get_expression_list();
                if (expression_list.size() != 1)
                {
                    std::cerr << directive.get_ast().get_locus() 
                        << ": warning: 'nest' clause requires one constant-expression argument" 
                        << std::endl;
                }
            }

            if (!expression_list.empty())
            {
                Expression &expr = expression_list[0];

				bool valid_expr = false;
				int nest = expr.evaluate_constant_int_expression(valid_expr);
				if (valid_expr)
				{
					nesting_value = nest;

					// Let's avoid misunderstandings
					if (nesting_value != 1)
					{
						std::cerr << directive.get_ast().get_locus() 
							<< ": warning: 'nest' with a value higher than 1 is not supported yet" 
							<< std::endl;
					}
				}
				else
				{
					std::cerr << directive.get_ast().get_locus()
						<< ": warning: 'nest' clause requires a constant-expression" 
						<< std::endl;
				}
            }
        }

        check_construction(st, valid_while, list_of_tasks, sequentiation_code, nesting_value);

        if (valid_while)
        {
            check_aggregated_data_sharing_clauses(st, list_of_tasks, valid_while);
        }

        if (!valid_while)
        {
            std::cerr << custom_construct.get_ast().get_locus() 
                << ": warning: '#pragma omp while' ignored since it does not meet structural requirements" << std::endl;
            return;
        }
        else
        {
            _num_aggregations++;
        }

		// Schedule clause
        OpenMP::ScheduleClause schedule = directive.schedule_clause();

        AST_t &chunk = custom_construct.get_data<AST_t>("chunk");
        if (schedule.is_defined())
        {
            if (!schedule.is_dynamic())
            {
                std::cerr << custom_construct.get_ast().get_locus() 
                    << ": warning: '#pragma omp while' has an invalid schedule kind, only 'dynamic' is allowed. Assuming 'dynamic'" << std::endl;
            }

            chunk = schedule.get_chunk();
        }
        else
        {
            std::cerr << custom_construct.get_ast().get_locus() 
                << ": warning: '#pragma omp while' does not have any 'schedule(dynamic, N)'. Assuming N=32" << std::endl;
            Source src;

            // Stupid value
            src << "32";

            // We need a tree
            chunk = src.parse_expression(custom_construct.get_ast(), custom_construct.get_scope_link());
        }
    }

    void TaskAggregationPhase::while_postorder(OpenMP::CustomConstruct custom_construct)
    {
        bool &valid_while = custom_construct.get_data<bool>("valid_while");

        if (valid_while)
        {
            std::cerr << custom_construct.get_ast().get_locus() << ": note: '#pragma omp while' is valid. Transforming it" << std::endl;
            // Perform transformation
            Source aggregated_code = get_aggregated_code(custom_construct);

            AST_t parsed_tree = aggregated_code.parse_statement(custom_construct.get_ast(),
                    custom_construct.get_scope_link());

            custom_construct.get_ast().replace(parsed_tree);
        }
    }

    void TaskAggregationPhase::check_construction(Statement st, 
            bool &is_valid,
            ObjectList<Statement> &list_of_tasks,
            ObjectList<Statement> &sequentiation_code,
			int nesting_value)
    {
        is_valid = true;

        AST_t statement_ast = st.get_ast();

        if (!WhileStatement::predicate(statement_ast)
                && !ForStatement::predicate(statement_ast))
        {
            std::cerr << st.get_ast().get_locus()
                << ": warning: '#pragma omp while' requires a while-statement or a for-statement" << std::endl;
            is_valid = false;
            return;
        }

        if (WhileStatement::predicate(statement_ast))
		{
			// The code must follow this layout
			//
			// #pragma omp while scheduling(dynamic, N)
			// while (E1)
			// {
			//    #pragma omp task
			//    {
			//      Task1;
			//    }
			//    [#pragma omp task
			//    {
			//      Task2;
			//    }
			//    ...
			//    ]
			//    CODE; // Causing some side-effect to the evaluation of E1 only (we won't check this :)
			// }
			//
			WhileStatement while_statement(st.get_ast(), st.get_scope_link());
			Statement while_body = while_statement.get_body();

			check_task_aggregated_body(st, while_body, is_valid, list_of_tasks, sequentiation_code, /* is_for */ false);

			if (!is_valid)
				return;
		}
        else // if (ForStatement::predicate(statement_ast))
        {
            // The code must follow this layout
            //
            // #pragma omp while scheduling(dynamic, N)
            // for (E|D; E|D; E)
            // {
            //    #pragma omp task
            //    {
            //      Task1;
            //    }
            //    [#pragma omp task
            //    {
            //      Task1;
            //    }
            //    ...
            //    ]
            //    CODE; // Causing some side-effect to the evaluation of E1 only (we won't check this :)
            // }
            ForStatement for_statement(st.get_ast(), st.get_scope_link());

            Statement for_body = for_statement.get_loop_body();

            check_task_aggregated_body(st, for_body, is_valid, list_of_tasks, sequentiation_code, /* is_for */ true);

            if (!is_valid)
                return;
        }
    }

    void TaskAggregationPhase::check_task_aggregated_body(
            Statement st,
            Statement body,
            bool &is_valid,
            ObjectList<Statement> &list_of_tasks,
            ObjectList<Statement> &sequentiation_code,
            bool is_for)
    {
        OpenMP::CustomConstructPredicate task_predicate("task");

        ObjectList<Statement> body_list;
        if (body.is_compound_statement())
        {
            body_list = body.get_inner_statements();
        }
        else
        {
            // Add myself in the list
            body_list.append(body);
        }

        if (body_list.empty())
        {
            std::cerr << st.get_ast().get_locus()
                << ": warning: '#pragma omp while' requires a non empty compound-statement" << std::endl;
            is_valid = false;
            return;
        }

        bool found_something_not_task = false;
        for (ObjectList<Statement>::iterator it = body_list.begin();
                it != body_list.end();
                it++)
        {
            Statement &current_st(*it);

            AST_t current_statement_ast = current_st.get_ast();

            if (task_predicate(current_statement_ast))
            {
                if (found_something_not_task)
                {
                    std::cerr << current_st.get_ast().get_locus()
                        << ": warning: '#pragma omp while' requires a sequence of tasks not interleaved with other statements" << std::endl;
                    is_valid = false;
                    return;
                }

                list_of_tasks.append(current_st);
            }
            else
            {
                // This is plain code
                found_something_not_task = true;

                sequentiation_code.append(current_st);
            }
        }

        if (!is_for)
        {
            if (list_of_tasks.empty())
            {
                std::cerr << st.get_ast().get_locus()
                    << ": warning: '#pragma omp while' requires a nonempty sequence of tasks" << std::endl;
                is_valid = false;
                return;
            }
        }
        else
        {
            if (list_of_tasks.empty())
            {
                // Let's assume that the whole loop body is parallel
                // It must be a task ...
                Source src;
                src 
                    // Be nice to the user
                    << "#line " << st.get_ast().get_line() << " \"" << st.get_ast().get_file() << "\"\n"
                    << "#pragma omp task\n"
                    << body.prettyprint()
                    ;

                AST_t task = src.parse_statement(st.get_ast(), st.get_scope_link());
				Statement st(task, st.get_scope_link());
                list_of_tasks.append(st);
            }
        }
    }

    Source TaskAggregationPhase::get_aggregated_code(OpenMP::CustomConstruct custom_construct)
    {
        Statement st = custom_construct.body();

        ObjectList<Statement> &list_of_tasks 
            = custom_construct.get_data<ObjectList<Statement> >("list_of_tasks");
        ObjectList<Statement> &sequentiation_code 
            = custom_construct.get_data<ObjectList<Statement> >("sequentiation_code");

        AST_t &chunk = custom_construct.get_data<AST_t>("chunk");

        AST_t statement_ast = st.get_ast();

        if (WhileStatement::predicate(statement_ast))
        {
            return get_aggregated_code_while(st, list_of_tasks, sequentiation_code, chunk);
        }
        else // if (ForStatement::predicate(statement_ast))
        {
            return get_aggregated_code_for(st, list_of_tasks, sequentiation_code, chunk);
        }
    }

    Source TaskAggregationPhase::get_aggregated_code_while(Statement st, 
            ObjectList<Statement> &list_of_tasks,
            ObjectList<Statement> &sequentiation_code,
            AST_t chunk)
    {
        WhileStatement while_statement(st.get_ast(), st.get_scope_link());

        Source result;

        Source while_expr;
        while_expr << while_statement.get_condition().prettyprint();

        // FIXME - Get this from the schedule clause
        Source chunk_factor = chunk.prettyprint();

        Source chunk_factor_name;
        chunk_factor_name << "_chunk_factor_" << _num_aggregations;

        Source aggregated_code, advancing_code, sequentiation_code_src, task_bodies;

        result
            << "while ( " << while_expr << ")"
            << "{"
            <<    aggregated_code
            <<    advancing_code
            << "}"
            ;

        Source other_data_sharings;

        // Aggregated code is a while with all the bodies of tasks inside
        aggregated_code
            // FIXME - Fill other_data_sharings!
            << "#pragma omp task " << other_data_sharings << "\n"
            << "{"
            << "int " << chunk_factor_name << " = 0;"
            << "while ((" << while_expr << ") && (" << chunk_factor_name << " < (" << chunk_factor << ")))"
            << "{"
            <<    task_bodies
            <<    sequentiation_code_src
            <<    chunk_factor_name << "++;"
            << "}"
            << "}"
            ;

        advancing_code
            << "int " << chunk_factor_name << " = 0;"
            << "while ((" << while_expr << ") && (" << chunk_factor_name << " < (" << chunk_factor << ")))"
            << "{"
            <<    sequentiation_code_src
            <<    chunk_factor_name << "++;"
            << "}"
            ;

        ObjectList<OpenMP::Directive> directive_list;
        for (ObjectList<Statement>::iterator it = list_of_tasks.begin();
                it != list_of_tasks.end();
                it++)
        {
            OpenMP::CustomConstruct current_task(it->get_ast(), it->get_scope_link(),
                    // Not needed here
                    /* Construct */ NULL, /* DataScoping*/ NULL );

			OpenMP::Directive directive = current_task.directive();
            directive_list.append(directive);

            Statement task_body = current_task.body();

            task_bodies
                << task_body.prettyprint()
                ;
        }

        for (ObjectList<Statement>::iterator it = sequentiation_code.begin();
                it != sequentiation_code.end();
                it++)
        {
            sequentiation_code_src
                << it->prettyprint()
                ;
        }

        other_data_sharings = aggregate_data_sharing_clauses(directive_list);

        return result;
    }

    Source TaskAggregationPhase::get_aggregated_code_for(Statement st, 
            ObjectList<Statement> &list_of_tasks,
            ObjectList<Statement> &sequentiation_code,
            AST_t chunk)
    {
        ForStatement for_statement(st.get_ast(), st.get_scope_link());

        Source result;

        // FIXME - Get this from the schedule clause
        Source chunk_factor = chunk.prettyprint();

        Source chunk_factor_name;
        chunk_factor_name << "_chunk_factor_" << _num_aggregations;

        Source aggregated_code, advancing_code, sequentiation_code_src, task_bodies;

        result
            // FIXME - Regenerating a for header should be easier 
            << "for (" << for_statement.get_iterating_init().prettyprint() 
            <<            for_statement.get_iterating_condition().prettyprint() << ";"
            <<            for_statement.get_iterating_expression().prettyprint() << ")"
            << "{"
            <<    aggregated_code
            <<    advancing_code
            << "}"
            ;

        Source other_data_sharings;

        // Aggregated code is a for with all the bodies of tasks inside
        aggregated_code
            // FIXME - Fill other_data_sharings!
            << "#pragma omp task " << other_data_sharings << "\n"
            << "{"
            << "int " << chunk_factor_name << " = 0;"
            << "for (" << for_statement.get_iterating_init().prettyprint() 
            <<            "(" << for_statement.get_iterating_condition().prettyprint() << ") && (" << chunk_factor_name << " < (" << chunk_factor << "));"
            <<            for_statement.get_iterating_expression().prettyprint() << ")"
            << "{"
            <<    task_bodies
            <<    sequentiation_code_src
            <<    chunk_factor_name << "++;"
            << "}"
            << "}"
            ;

        advancing_code
            << "int " << chunk_factor_name << " = 0;"
            << "for (" << for_statement.get_iterating_init().prettyprint() 
            <<            "(" << for_statement.get_iterating_condition().prettyprint() << ") && (" << chunk_factor_name << " < (" << chunk_factor << "));"
            <<            for_statement.get_iterating_expression().prettyprint() << ")"
            << "{"
            // <<    sequentiation_code_src
            <<    chunk_factor_name << "++;"
            << "}"
            ;

        ObjectList<OpenMP::Directive> directive_list;
        for (ObjectList<Statement>::iterator it = list_of_tasks.begin();
                it != list_of_tasks.end();
                it++)
        {
            OpenMP::CustomConstruct current_task(it->get_ast(), it->get_scope_link(),
                    // Not needed here
                    /* Construct */ NULL, /* DataScoping*/ NULL );

			OpenMP::Directive directive = current_task.directive();
            directive_list.append(directive);

            Statement task_body = current_task.body();

            task_bodies
                << task_body.prettyprint()
                ;
        }

        other_data_sharings = aggregate_data_sharing_clauses(directive_list);

        for (ObjectList<Statement>::iterator it = sequentiation_code.begin();
                it != sequentiation_code.end();
                it++)
        {
            sequentiation_code_src
                << it->prettyprint()
                ;
        }

        return result;
    }

    Source TaskAggregationPhase::aggregate_data_sharing_clauses(
            ObjectList<OpenMP::Directive> directive_list)
    {
        ObjectList<IdExpression> private_list;
        ObjectList<IdExpression> firstprivate_list;
        ObjectList<IdExpression> shared_list;

        for (ObjectList<OpenMP::Directive>::iterator it = directive_list.begin();
                it != directive_list.end();
                it++)
        {
            OpenMP::Directive &directive(*it);

            {
                // Firstprivate
                OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
                ObjectList<IdExpression> id_expression_list = firstprivate_clause.id_expressions();
                firstprivate_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
            {
                // Private
                OpenMP::Clause private_clause = directive.private_clause();
                ObjectList<IdExpression> id_expression_list = private_clause.id_expressions();
                private_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
            {
                // Shared
                OpenMP::Clause shared_clause = directive.shared_clause();
                ObjectList<IdExpression> id_expression_list = shared_clause.id_expressions();
                shared_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
        }

        Source result;

        result
            << " "
            ;

        if (!firstprivate_list.empty())
        {
            Source list;
            result << "firstprivate("
                << list
                << ")";

            for (ObjectList<IdExpression>::iterator it = firstprivate_list.begin();
                    it != firstprivate_list.end();
                    it++)
            {
                list.append_with_separator(it->prettyprint(), ",");
            }
        }

        if (!private_list.empty())
        {
            Source list;
            result << "private("
                << list
                << ")";

            for (ObjectList<IdExpression>::iterator it = private_list.begin();
                    it != private_list.end();
                    it++)
            {
                list.append_with_separator(it->prettyprint(), ",");
            }
        }

        if (!private_list.empty())
        {
            Source list;
            result << "shared("
                << list
                << ")";

            for (ObjectList<IdExpression>::iterator it = shared_list.begin();
                    it != shared_list.end();
                    it++)
            {
                list.append_with_separator(it->prettyprint(), ",");
            }
        }

        return result;
    }

    void TaskAggregationPhase::check_aggregated_data_sharing_clauses(
            Statement st,
            ObjectList<Statement> &task_list, 
            bool &is_valid)
    {
        // assert(is_valid)
        ObjectList<IdExpression> private_list;
        ObjectList<IdExpression> firstprivate_list;
        ObjectList<IdExpression> shared_list;

        for (ObjectList<Statement>::iterator it_task = task_list.begin();
                it_task != task_list.end();
                it_task++)
        {
            OpenMP::CustomConstruct current_task(it_task->get_ast(), it_task->get_scope_link(),
                    // Not needed here
                    /* Construct */ NULL, /* DataScoping*/ NULL );

            ObjectList<IdExpression> private_list;
            ObjectList<IdExpression> firstprivate_list;
            ObjectList<IdExpression> shared_list;

            OpenMP::Directive directive = current_task.directive();

            {
                // Firstprivate
                OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
                ObjectList<IdExpression> id_expression_list = firstprivate_clause.id_expressions();
                firstprivate_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
            {
                // Private
                OpenMP::Clause private_clause = directive.private_clause();
                ObjectList<IdExpression> id_expression_list = private_clause.id_expressions();
                private_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
            {
                // Shared
                OpenMP::Clause shared_clause = directive.shared_clause();
                ObjectList<IdExpression> id_expression_list = shared_clause.id_expressions();
                shared_list.insert(id_expression_list, functor(&IdExpression::get_symbol));
            }
        }

        // Check problems
        for (ObjectList<IdExpression>::iterator it = firstprivate_list.begin();
                it != firstprivate_list.end() && is_valid;
                it++)
        {
            IdExpression& id_expression(*it);

            if (private_list.contains(id_expression, functor(&IdExpression::get_symbol))
                    || shared_list.contains(id_expression, functor(&IdExpression::get_symbol)))
            {
                is_valid = false;
                return;
            }
        }

        for (ObjectList<IdExpression>::iterator it = private_list.begin();
                it != private_list.end() && is_valid;
                it++)
        {
            IdExpression& id_expression(*it);

            if (firstprivate_list.contains(id_expression, functor(&IdExpression::get_symbol))
                    || shared_list.contains(id_expression, functor(&IdExpression::get_symbol)))
            {
                is_valid = false;
                return;
            }
        }

        for (ObjectList<IdExpression>::iterator it = shared_list.begin();
                it != shared_list.end() && is_valid;
                it++)
        {
            IdExpression& id_expression(*it);

            if (firstprivate_list.contains(id_expression, functor(&IdExpression::get_symbol))
                    || private_list.contains(id_expression, functor(&IdExpression::get_symbol)))
            {
                is_valid = false;
            }
        }

        if (!is_valid)
        {
            std::cerr << st.get_ast().get_locus() 
                << ": warning: two or more tasks in '#pragma omp while' define different data sharing attributes for the same entity" << std::endl;
        }
    }

}
