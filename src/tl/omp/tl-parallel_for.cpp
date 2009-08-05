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
#include "tl-omptransform.hpp"

namespace TL
{
    namespace Nanos4
    {
        void OpenMPTransform::parallel_for_preorder(PragmaCustomConstruct parallel_for_construct)
        {
            ObjectList<OpenMP::ReductionSymbol> inner_reductions;
            inner_reductions_stack.push(inner_reductions);

            // Increase the parallel nesting value
            parallel_nesting++;

            common_parallel_data_sharing_code(parallel_for_construct);
        }

        void OpenMPTransform::parallel_for_postorder(PragmaCustomConstruct parallel_for_construct)
        {
            // One more parallel seen
            num_parallels++;

            // Remove the induction var from the stack
            induction_var_stack.pop();

            // Decrease the parallel nesting level 
            parallel_nesting--;

            // Get the enclosing function definition
            FunctionDefinition function_definition = parallel_for_construct.get_enclosing_function();
            Scope function_scope = function_definition.get_scope();
            IdExpression function_name = function_definition.get_function_name();

            // This was computed in the preorder
            ObjectList<Symbol>& shared_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                parallel_for_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                parallel_for_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

            // Get the construct_body of the statement
            Statement construct_body = parallel_for_construct.get_statement();
            // The construct is in fact a ForStatement in a #pragma omp parallel do
            ForStatement for_statement(construct_body);
            Statement loop_body = for_statement.get_loop_body();

            // Create the replacement map and the pass_by_pointer set
            ObjectList<ParameterInfo> parameter_info_list;
            ReplaceIdExpression replace_references = 
                set_replacements(function_definition,
                        loop_body,
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

            // Create the outline for parallel for
            AST_t outline_code = get_outline_parallel_for(
                    parallel_for_construct,
                    function_definition,
                    outlined_function_name, 
                    for_statement,
                    loop_body,
                    replace_references,
                    parameter_info_list,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    copyprivate_references);

            // Now prepend the outline
            function_definition.get_ast().prepend_sibling_function(outline_code);

            PragmaCustomClause if_clause = parallel_for_construct.get_clause("if");
            PragmaCustomClause num_threads = parallel_for_construct.get_clause("num_threads");
            PragmaCustomClause groups_clause = parallel_for_construct.get_clause("groups");

            Source instrument_code_before;
            Source instrument_code_after;

            AST_t spawn_code = get_parallel_spawn_code(
                    parallel_for_construct.get_ast(),
                    function_definition,
                    parallel_for_construct.get_scope(),
                    parallel_for_construct.get_scope_link(),
                    parameter_info_list,
                    reduction_references,
                    if_clause,
                    num_threads,
                    groups_clause,
                    instrument_code_before,
                    instrument_code_after
                    );

            // Discard inner reduction information
            inner_reductions_stack.pop();

            // Replace all the whole construct with spawn_code
            parallel_for_construct.get_ast().replace(spawn_code);
        }

        // Create outline for parallel for
        AST_t OpenMPTransform::get_outline_parallel_for(
                PragmaCustomConstruct &construct,
                FunctionDefinition function_definition,
                Source outlined_function_name,
                ForStatement for_statement,
                Statement /* loop_body */,
                ReplaceIdExpression replace_references,
                ObjectList<ParameterInfo> parameter_info_list,
                ObjectList<Symbol> private_references,
                ObjectList<Symbol> firstprivate_references,
                ObjectList<Symbol> lastprivate_references,
                ObjectList<OpenMP::ReductionSymbol> reduction_references,
                ObjectList<Symbol> copyin_references,
                ObjectList<Symbol> copyprivate_references
                )
        {
            // empty
            ObjectList<IdExpression> pass_by_value;

            Source empty;
            Source outline_parallel_for;
            Source parallel_for_body;

            // Get the source of the common parallel X outline
            outline_parallel_for = get_outline_common(
                    function_definition,
                    parallel_for_body,
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

            Source loop_distribution = get_loop_distribution_code(
                    for_statement, 
                    construct,
                    replace_references, 
                    function_definition);

            Source lastprivate_code;

            if (!lastprivate_references.empty())
            {
                Source lastprivate_assignments = get_lastprivate_assignments(
                        firstprivate_references,
                        lastprivate_references, 
                        copyprivate_references,
                        parameter_info_list);

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

            parallel_for_body 
                << private_declarations

                << comment("Entering team")
                << code_before_entering_team
                << enter_team

                << comment("Construct code")
                << loop_distribution
                << loop_finalization

                << reduction_update
                << lastprivate_code

                << destructor_calls
                << comment("Leaving team")
                << leave_team
                << code_after_leaving_team
                ;

            return finish_outline(function_definition, 
                    outline_parallel_for, 
                    parameter_info_list,
                    /* team_parameter */ true);
        }
    }
}
