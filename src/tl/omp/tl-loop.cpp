/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-omptransform.hpp"
namespace TL
{
    Source OpenMPTransform::get_loop_distribution_in_sections(
            int num_sections,
            Statement construct_body,
            ReplaceIdExpression replace_references)
    {
        Source loop_distribution;

        // Replace references using set "replace_references" over construct body
        Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

        Source instrumentation_before, instrumentation_after;

        loop_distribution 
            << instrumentation_before
            << "int nth_low;"
            << "int nth_upper;"
            << "int nth_step;"
            << "int nth_chunk;"
            << "int nth_schedule;"
            << "int intone_start;"
            << "int intone_end;"
            << "int intone_last;"
            << "int nth_barrier;"
            << "int nth_i;"

            << "nth_low = 0;"
            << "nth_upper = " << num_sections << ";"
            << "nth_step = 1;"
            << "nth_schedule = 2;" // Dynamic
            << "nth_chunk = 1;"

            //                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
            //                    << "extern int in__tone_next_iters_(int*, int*, int*);"
            //                    << "extern void in__tone_end_for_(int*);"

            << "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
            << "while (in__tone_next_iters_ (&intone_start, &intone_end, &intone_last) != 0)"
            << "{"
            <<    "for (nth_i = intone_start; nth_i <= intone_end; nth_i += nth_step)"
            <<    "{"
            <<         "switch (nth_i)"
            <<         "{"
            <<            modified_parallel_body_stmt.prettyprint()
            <<            "default: break;" 
            <<         "}"
            <<    "}"
            << "}"
            << instrumentation_after
            ;

        if (instrumentation_requested())
        {
            instrumentation_before
                << "mintaka_state_synch();"
                ;
            instrumentation_after
                << "mintaka_state_run();"
                ;
        }

        return loop_distribution;
    }

    Source OpenMPTransform::get_loop_distribution_code(ForStatement for_statement,
            ReplaceIdExpression replace_references,
            FunctionDefinition function_definition,
            OpenMP::Directive directive)
    {
        Source parallel_for_body;

        Source loop_initialization;

        Source schedule_decisions;
        Source distributed_loop_body;
        Source loop_reductions;
        Source reduction_initialization;

        parallel_for_body
            << reduction_initialization
            << loop_initialization
            << schedule_decisions
            << distributed_loop_body
            << loop_reductions
            ;

        Statement loop_body = for_statement.get_loop_body();

        Source instrumentation_code_before, instrumentation_code_after;
        instrumentation_outline(instrumentation_code_before,
                instrumentation_code_after, 
                function_definition,
                loop_body);

        IdExpression induction_var = for_statement.get_induction_variable();
        Source induction_var_name;
        // Induction var name is handled specially
        induction_var_name << "p_" << induction_var.mangle_id_expression();

        Expression lower_bound = for_statement.get_lower_bound();
        Expression upper_bound = for_statement.get_upper_bound();
        Expression step = for_statement.get_step();

        lower_bound = replace_references.replace(lower_bound);
        upper_bound = replace_references.replace(upper_bound);
        step = replace_references.replace(step);

        // Define here the bounds of the loop
        loop_initialization 
//            << "int nth_low;"
//            << "int nth_upper;"
	    << induction_var.get_symbol().get_type().get_declaration(induction_var.get_scope(),"nth_low") << ";"
	    << induction_var.get_symbol().get_type().get_declaration(induction_var.get_scope(),"nth_upper") << ";"
            << "int nth_step;"
            << "int intone_start;"
            << "int intone_end;"
            << "int intone_last;"
            << "int nth_barrier;"

            << "nth_low = " << lower_bound.prettyprint() << ";"
            << "nth_upper = " << upper_bound.prettyprint() << ";"
            << "nth_step = " << step.prettyprint() << ";"
            ;

        // Schedule decisions
        Source schedule_const;
        Source schedule_chunk;
        schedule_decisions
            << "int nth_schedule;"
            << "int nth_chunk;"
            << "nth_schedule = " << schedule_const << ";"
            << "nth_chunk = " << schedule_chunk << ";"
            ;

        OpenMP::ScheduleClause schedule_clause = directive.schedule_clause();
        if (schedule_clause.is_defined())
        {
            schedule_const << schedule_clause.internal_code();

            AST_t schedule_chunk_tree = schedule_clause.get_chunk();

            if (schedule_chunk_tree.is_valid())
            {
                schedule_chunk << schedule_chunk_tree.prettyprint();
            }
            else
            {
                schedule_chunk << "0";
            }
        }
        else
        {
            schedule_const << "0";
            schedule_chunk << "0";
        }

        // #define INTONE_DEFAULT              0
        // #define INTONE_STATIC               1
        // #define INTONE_DYNAMIC              2
        // #define INTONE_GUIDED               4

        // Loop distribution
        Source modified_loop_body;
        distributed_loop_body
            //                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
            //                    << "extern int in__tone_next_iters_(int*, int*, int*);"
            //                    << "extern void in__tone_end_for_(int*);"

	    << "int nth_zero = 0;"
	    << "int nth_iters;"
	    << "if ( nth_step > 0 ) nth_iters =  nth_upper - nth_low + 1;"
	    << "else nth_iters = nth_low - nth_upper + 1;"


            << "in__tone_begin_for_(&nth_zero, &nth_iters, &nth_step, &nth_chunk, &nth_schedule);"

            // Get a slice of the iteration space
            << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
            << "{"
            << instrumentation_code_before
            // And do something with it
            << "   for (" << induction_var_name << " = nth_low+intone_start; "
            << "        nth_step >= 1 ? " << induction_var_name << " <= nth_low+intone_end : " << induction_var_name << ">= nth_low+intone_end;"
            << "        " << induction_var_name << " += nth_step)"
            << "   {"
            << "   " << modified_loop_body
            << "   }"
            << instrumentation_code_after
            << "}"
            ;

        // Replace references using set "replace_references" over loop body
        Statement modified_loop_body_stmt = replace_references.replace(loop_body);
        // and get the source of the modified tree
        modified_loop_body << modified_loop_body_stmt.prettyprint();

        return parallel_for_body;
    }

    Source OpenMPTransform::get_loop_finalization(bool do_barrier)
    {
        Source loop_finalization;

        if (do_barrier 
                && instrumentation_requested())
        {
            loop_finalization
                << "mintaka_state_synch();"
                << "nth_barrier = " << (int)(do_barrier) << ";"
                << "in__tone_end_for_(&nth_barrier);"
                << "mintaka_state_run();"
                ;
        }
        else
        {
            loop_finalization
                << "nth_barrier = " << (int)(do_barrier) << ";"
                << "in__tone_end_for_(&nth_barrier);"
                ;
        }

        return loop_finalization;
    }

}
