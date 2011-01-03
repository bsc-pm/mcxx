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



#include "tl-omptransform.hpp"
namespace TL
{
    namespace Nanos4
    {
        static std::string schedule_constant_name(const std::string& str);

        Source OpenMPTransform::get_loop_distribution_in_sections(
                int num_sections,
                Statement construct_body,
                ReplaceIdExpression replace_references)
        {
            Source loop_distribution;

            // Replace references using set "replace_references" over construct body
            Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

            // FIXME - 'int' might no be enough for some loops!
            loop_distribution 
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
                << "nth_schedule = INTONE_SCH_DYNAMIC;" // Dynamic
                << "nth_chunk = 1;"

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
                ;

            return loop_distribution;
        }

        Source OpenMPTransform::get_loop_distribution_code(
                ForStatement for_statement,
                PragmaCustomConstruct &for_construct,
                ReplaceIdExpression replace_references,
                FunctionDefinition function_definition)
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

            IdExpression induction_var = for_statement.get_induction_variable();
            Source induction_var_name;
            // Induction var name is handled specially. We check that only in 'for' it will be private, not in
            // any enclosing data environment

            OpenMP::DataSharingEnvironment& data_sharing = openmp_info->get_data_sharing(for_construct.get_ast());

            if ((data_sharing.get_data_sharing(induction_var.get_symbol()) & OpenMP::DS_PRIVATE) == OpenMP::DS_PRIVATE)
            {
                induction_var_name << "p_" << induction_var.mangle_id_expression();
            }
            else
            {
                induction_var_name << induction_var.mangle_id_expression();
            }

            Expression lower_bound = for_statement.get_lower_bound();
            Expression upper_bound = for_statement.get_upper_bound();
            Expression step = for_statement.get_step();

            lower_bound = replace_references.replace(lower_bound);
            upper_bound = replace_references.replace(upper_bound);
            step = replace_references.replace(step);

            // Define here the bounds of the loop
            loop_initialization 
                // Iterator type, normally an integer type
                // but it can be a random_access_iterator
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

            PragmaCustomClause schedule_clause = for_construct.get_clause("schedule");
            if (schedule_clause.is_defined())
            {
                ObjectList<std::string> args = schedule_clause.get_arguments(ExpressionTokenizer());
                schedule_const << schedule_constant_name(args[0]);

                if (args.size() > 1)
                {
                    schedule_chunk << args[1];
                }
                else
                {
                    schedule_chunk << "0";
                }
            }
            else
            {
                schedule_const << "INTONE_SCH_DEFAULT";
                schedule_chunk << "0";
            }

            // Loop distribution
            Source modified_loop_body;
            Source instrument_code_before,
                   instrument_code_after;
            distributed_loop_body
                //                    << "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
                //                    << "extern int in__tone_next_iters_(int*, int*, int*);"
                //                    << "extern void in__tone_end_for_(int*);"

                << "int nth_zero = 0;"
                << "int nth_iters;"
                << "if ( nth_step > 0 ) nth_iters =  (nth_upper - nth_low);"
                << "else {"
                <<    "nth_iters = 0;"
                <<    "nth_zero = (nth_low - nth_upper);"
                <<    "nth_low = nth_upper;"
                << "}"

                << "in__tone_begin_for_(&nth_zero, &nth_iters, &nth_step, &nth_chunk, &nth_schedule);"

                // Get a slice of the iteration space
                << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
                << "{"
                // And do something with it
                << instrument_code_before
                << "   for (" << induction_var_name << " = nth_low+intone_start; "
                << "        nth_step >= 1 ? " << induction_var_name << " <= nth_low+intone_end : " << induction_var_name << ">= nth_low+intone_end;"
                << "        " << induction_var_name << " += nth_step)"
                << "   {"
                <<        modified_loop_body
                << "   }"
                << instrument_code_after
                << "}"

                ;

            // Replace references using set "replace_references" over loop body
            Statement modified_loop_body_stmt = replace_references.replace(loop_body);
            // and get the source of the modified tree

            modified_loop_body 
                << modified_loop_body_stmt.prettyprint()
                ;

            return parallel_for_body;
        }

        Source OpenMPTransform::get_loop_finalization(bool do_barrier)
        {
            Source loop_finalization;
            loop_finalization
                << "nth_barrier = " << (int)(do_barrier) << ";"
                << "in__tone_end_for_(&nth_barrier);"
                ;
            return loop_finalization;
        }

        static std::string schedule_constant_name(const std::string& schedule_name)
        {
            if (schedule_name == "static")
            {
                return "INTONE_SCH_STATIC";
            }
            else if (schedule_name == "dynamic")
            {
                return "INTONE_SCH_DYNAMIC";
            }
            else if (schedule_name == "guided")
            {
                return "INTONE_SCH_GUIDED";
            }
            else if (schedule_name == "runtime")
            {
                return "INTONE_SCH_RUNTIME";
            }
            else 
            {
                // Quite stupid
                return "<not valid schedule clause>";
            }
        }
    }
}
