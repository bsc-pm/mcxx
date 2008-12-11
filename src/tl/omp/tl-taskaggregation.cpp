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

        check_construction(st, valid_while, list_of_tasks, sequentiation_code);

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

        // Get schedule clause
        OpenMP::Directive directive = custom_construct.directive();
        OpenMP::ScheduleClause schedule = directive.schedule_clause();

        if (!schedule.is_dynamic())
        {
            std::cerr << custom_construct.get_ast().get_locus() 
                << ": warning: '#pragma omp while' has an invalid schedule kind, only 'dynamic' is allowed. Assuming 'dynamic'" << std::endl;
        }

        AST_t &chunk = custom_construct.get_data<AST_t>("chunk");
        chunk = schedule.get_chunk();
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
            ObjectList<Statement> &sequentiation_code)
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
            //      Task1;
            //    }
            //    ...
            //    ]
            //    CODE; // Causing some side-effect to the evaluation of E1 only (we won't check this :)
            // }
            //
            WhileStatement while_statement(st.get_ast(), st.get_scope_link());
            Statement while_body = while_statement.get_body();

            if (!while_body.is_compound_statement())
            {
                std::cerr << st.get_ast().get_locus()
                    << ": warning: '#pragma omp while' requires a while-statement followed by a compound statement" << std::endl;
                is_valid = false;
                return;
            }

            ObjectList<Statement> while_body_list = while_body.get_inner_statements();

            check_task_aggregated_body(st, while_body_list, is_valid, list_of_tasks, sequentiation_code);

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

            if (!for_body.is_compound_statement())
            {
                std::cerr << st.get_ast().get_locus()
                    << ": warning: '#pragma omp while' requires a while-statement followed by a compound statement" << std::endl;
                is_valid = false;
                return;
            }

            ObjectList<Statement> for_body_list = for_body.get_inner_statements();

            check_task_aggregated_body(st, for_body_list, is_valid, list_of_tasks, sequentiation_code);

            if (!is_valid)
                return;
        }
    }

    void TaskAggregationPhase::check_task_aggregated_body(
            Statement st,
            ObjectList<Statement> body_list,
            bool &is_valid,
            ObjectList<Statement> &list_of_tasks,
            ObjectList<Statement> &sequentiation_code)
    {
        OpenMP::CustomConstructPredicate task_predicate("task");

        if (body_list.empty())
        {
            std::cerr << st.get_ast().get_locus()
                << ": warning: '#pragma omp while' requires a sequence of tasks not interleaved with other statements" << std::endl;
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

        if (list_of_tasks.empty())
        {
            std::cerr << st.get_ast().get_locus()
                << ": warning: '#pragma omp while' requires a nonempty sequence of tasks" << std::endl;
            is_valid = false;
            return;
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

        for (ObjectList<Statement>::iterator it = list_of_tasks.begin();
                it != list_of_tasks.end();
                it++)
        {
            OpenMP::CustomConstruct current_task(it->get_ast(), it->get_scope_link(),
                    // Not needed here
                    /* Construct */ NULL, /* DataScoping*/ NULL );

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

        for (ObjectList<Statement>::iterator it = list_of_tasks.begin();
                it != list_of_tasks.end();
                it++)
        {
            OpenMP::CustomConstruct current_task(it->get_ast(), it->get_scope_link(),
                    // Not needed here
                    /* Construct */ NULL, /* DataScoping*/ NULL );

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

        return result;
    }
}
