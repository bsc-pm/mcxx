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
        void OpenMPTransform::parallel_sections_preorder(PragmaCustomConstruct parallel_sections_construct)
        {
            ObjectList<OpenMP::ReductionSymbol> inner_reductions;
            inner_reductions_stack.push(inner_reductions);

            // Increase the parallel nesting value
            parallel_nesting++;

            // We push a new level of sections with zero "section" counted
            // so far
            num_sections_stack.push(0);

            common_parallel_data_sharing_code(parallel_sections_construct);
        }

        void OpenMPTransform::parallel_sections_postorder(PragmaCustomConstruct parallel_sections_construct)
        {
            // One more parallel seen
            num_parallels++;

            // Decrease the parallel nesting
            parallel_nesting--;

            // Get the enclosing function definition
            FunctionDefinition function_definition = parallel_sections_construct.get_enclosing_function();
            // its scope
            Scope function_scope = function_definition.get_scope();
            // and the id-expression of the function name
            IdExpression function_name = function_definition.get_function_name();

            // This was computed in the preorder
            ObjectList<Symbol>& shared_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                parallel_sections_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                parallel_sections_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

            // Get the construct_body of the statement
            Statement construct_body = parallel_sections_construct.get_statement();

            ObjectList<ParameterInfo> parameter_info_list;
            ReplaceIdExpression replace_references = 
                set_replacements(function_definition,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        inner_reductions_stack.top(),
                        copyin_references,
                        copyprivate_references,
                        parameter_info_list);

            // Get the outline function name
            Source outlined_function_name = get_outlined_function_name(function_name);

            // Create the outline for parallel sections using 
            // the privatized entities and pass by pointer
            // lists.
            // Additionally {first|last}private and reduction
            // entities are needed for proper initializations
            // and assignments.
            AST_t outline_code = get_outline_parallel_sections(
                    parallel_sections_construct,
                    function_definition,
                    outlined_function_name, 
                    construct_body,
                    replace_references,
                    parameter_info_list,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    copyprivate_references);

            // In the AST of the function definition, prepend outline_code
            // as a sibling (at the same level)
            function_definition.get_ast().prepend_sibling_function(outline_code);

            PragmaCustomClause if_clause = parallel_sections_construct.get_clause("if_clause");
            PragmaCustomClause num_threads = parallel_sections_construct.get_clause("num_threads");
            PragmaCustomClause groups_clause = parallel_sections_construct.get_clause("groups"); 

            // Now create the spawning code. Pass by pointer list and
            // reductions are needed for proper pass of data and reduction
            // vectors declaration
            Source instrument_code_before;
            Source instrument_code_after;

            AST_t spawn_code = get_parallel_spawn_code(
                    parallel_sections_construct.get_ast(),
                    function_definition,
                    parallel_sections_construct.get_scope(),
                    parallel_sections_construct.get_scope_link(),
                    parameter_info_list,
                    reduction_references,
                    if_clause,
                    num_threads,
                    groups_clause,
                    instrument_code_before,
                    instrument_code_after
                    );

            // One less level of sections
            num_sections_stack.pop();

            // Discard inner reductions information
            inner_reductions_stack.pop();

            // Now replace the whole construct with spawn_code
            parallel_sections_construct.get_ast().replace(spawn_code);
        }

        // Create outline for parallel sections
        AST_t OpenMPTransform::get_outline_parallel_sections(
                PragmaCustomConstruct &construct,
                FunctionDefinition function_definition,
                Source outlined_function_name, 
                Statement construct_body,
                ReplaceIdExpression replace_references,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<Symbol> private_references,
                ObjectList<Symbol> firstprivate_references,
                ObjectList<Symbol> lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                ObjectList<Symbol> copyin_references,
                ObjectList<Symbol> copyprivate_references)
        {
            ObjectList<IdExpression> pass_by_value;

            Source outline_parallel_sections;
            Source parallel_sections_body;

            // Get the source of the common parallel X outline
            outline_parallel_sections = get_outline_common(
                    function_definition,
                    parallel_sections_body,
                    outlined_function_name,
                    parameter_info_list,
                    construct,
                    /* team_parameter */ true);

            Source private_declarations = get_privatized_declarations(
                    construct,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    parameter_info_list
                    ); 

            Source loop_distribution;

            int num_sections = num_sections_stack.top();

            loop_distribution = get_loop_distribution_in_sections(num_sections,
                    construct_body,
                    replace_references
                    );

            Source lastprivate_code;

            if (!lastprivate_references.empty())
            {
                Source lastprivate_assignments = get_lastprivate_assignments(
                        firstprivate_references,
                        lastprivate_references, 
                        copyprivate_references,
                        ObjectList<ParameterInfo>()
                        );

                lastprivate_code
                    << "if (intone_last != 0)"
                    << "{"
                    <<    lastprivate_assignments
                    << "}"
                    ;
            }

            // Barrier is already done at parallel level
            Source loop_finalization = get_loop_finalization(/* do_barrier = */ false);

            Source reduction_update = get_reduction_update(reduction_references);

            Source code_before_entering_team,
                   code_after_leaving_team;
            Source enter_team,
                   leave_team;
            code_before_entering_team
                << "nth_player_t nth_player;"
                << "nth_init_player(&nth_player);"
                ;
            enter_team
                << "nth_enter_team(nth_current_team, &nth_player, 0);"
                ;
            leave_team
                << "nth_leave_team(1);"
                ;
            code_after_leaving_team
                << "nth_end_player(&nth_player);"
                ;

            Source destructor_calls;
            invoke_destructors(parameter_info_list, destructor_calls);

            parallel_sections_body 
                << private_declarations

                << comment("Entering team")
                << code_before_entering_team
                << enter_team

                << comment("Construct code")
                << loop_distribution

                << reduction_update
                << lastprivate_code
                << loop_finalization

                << destructor_calls

                << comment("Leaving team")
                << leave_team
                << code_after_leaving_team
                ;


            return finish_outline(function_definition, 
                    outline_parallel_sections, 
                    parameter_info_list,
                    /* team_parameter */ true);
        }
    }
}
