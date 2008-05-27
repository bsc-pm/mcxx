/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
        void OpenMPTransform::common_parallel_data_sharing_code(OpenMP::Construct &parallel_construct)
        {
            // They will hold the entities as they appear in the clauses
            ObjectList<Symbol>& shared_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                parallel_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

            // Get the directive
            OpenMP::Directive directive = parallel_construct.directive();

            // Get the construct_body of the statement
            Statement construct_body = parallel_construct.body();

            // Get the data attributes for every entity
            get_data_attributes(parallel_construct,
                    directive,
                    construct_body,
                    shared_references,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    copyprivate_references);
        }

        void OpenMPTransform::parallel_preorder(OpenMP::ParallelConstruct parallel_construct)
        {
            // Inner reductions stack
            ObjectList<OpenMP::ReductionSymbol> inner_reductions;
            inner_reductions_stack.push(inner_reductions);

            // Increase the parallel nesting value
            parallel_nesting++;

            common_parallel_data_sharing_code(parallel_construct);
        }

        void OpenMPTransform::parallel_postorder(OpenMP::ParallelConstruct parallel_construct)
        {
            // One more parallel seen
            num_parallels++;

            // Decrease the parallel nesting value
            parallel_nesting--;

            // Get the directive
            OpenMP::Directive directive = parallel_construct.directive();

            // Get the construct_body of the statement
            Statement construct_body = parallel_construct.body();

            // Get the enclosing function definition
            FunctionDefinition function_definition = parallel_construct.get_enclosing_function();
            // its scope
            Scope function_scope = function_definition.get_scope();
            // and the id-expression of the function name
            IdExpression function_name = function_definition.get_function_name();

            // This was computed in the preorder
            ObjectList<Symbol>& shared_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("shared_references");
            ObjectList<Symbol>& private_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("private_references");
            ObjectList<Symbol>& firstprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("firstprivate_references");
            ObjectList<Symbol>& lastprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("lastprivate_references");
            ObjectList<OpenMP::ReductionSymbol>& reduction_references =
                parallel_construct.get_data<ObjectList<OpenMP::ReductionSymbol> >("reduction_references");
            ObjectList<Symbol>& copyin_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("copyin_references");
            ObjectList<Symbol>& copyprivate_references = 
                parallel_construct.get_data<ObjectList<Symbol> >("copyprivate_references");

            ObjectList<ParameterInfo> parameter_info_list;

            // Compute the replacements
            ReplaceIdExpression replace_references = 
                set_replacements(function_definition,
                        directive,
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

            // Create the outline for parallel for using 
            // the privatized entities and pass by pointer
            // lists.
            // Additionally {first|last}private and reduction
            // entities are needed for proper initializations
            // and assignments.
            OpenMP::CustomClause noinstr_clause = directive.custom_clause("noinstr");

            AST_t outline_code  = get_outline_parallel(
                    parallel_construct,
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
                    copyprivate_references,
                    noinstr_clause.is_defined());

            // In the AST of the function definition, prepend outline_code
            // as a sibling (at the same level)
            function_definition.get_ast().prepend_sibling_function(outline_code);

            // Now create the spawning code. Pass by pointer list and
            // reductions are needed for proper pass of data and reduction
            // vectors declaration

            OpenMP::Clause if_clause = directive.if_clause();
            OpenMP::Clause num_threads = directive.num_threads_clause();
            OpenMP::CustomClause groups_clause = directive.custom_clause("groups");

            Source instrument_code_before;
            Source instrument_code_after;

            AST_t spawn_code = get_parallel_spawn_code(
                    parallel_construct.get_ast(),
                    function_definition,
                    parallel_construct.get_scope(),
                    parallel_construct.get_scope_link(),
                    parameter_info_list,
                    reduction_references,
                    if_clause,
                    num_threads,
                    groups_clause,
                    instrument_code_before,
                    instrument_code_after
                    );

            // Discard inner reductions information
            inner_reductions_stack.pop();

            // Now replace the whole construct with spawn_code
            parallel_construct.get_ast().replace(spawn_code);
        }

        AST_t OpenMPTransform::get_outline_parallel(
                OpenMP::Construct &construct,
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
                ObjectList<Symbol> /* copyprivate_references - ???*/,
                bool never_instrument /* = false */
                )
        {
            ObjectList<IdExpression> pass_by_value;

            Source outline_parallel;
            Source parallel_body;
            Source empty;

            outline_parallel = get_outline_common(
                    function_definition,
                    parallel_body, // The body of the outline
                    outlined_function_name,
                    parameter_info_list,
                    /* team_parameter */ true);

            // Replace references using set "replace_references" over construct body
            Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

            Source private_declarations = get_privatized_declarations(
                    construct,
                    private_references,
                    firstprivate_references,
                    lastprivate_references,
                    reduction_references,
                    copyin_references,
                    parameter_info_list
                    ); 

            Source reduction_update = get_reduction_update(reduction_references);

            // Debug information
            Source debug_comment = debug_parameter_info(
                    parameter_info_list);

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

            // Source destructor_calls;

            parallel_body 
                << debug_comment
                << private_declarations
                << comment("Entering team")
                << code_before_entering_team
                << enter_team

                << comment("Construct code")
                << modified_parallel_body_stmt.prettyprint()

                << reduction_update
                // << destructor_calls
                << comment("Leaving team")
                << leave_team
                << code_after_leaving_team
                ;

            // invoke_destructors(parameter_info_list, destructor_calls);

            return finish_outline(function_definition, 
                    outline_parallel, 
                    parameter_info_list,
                    /* team_parameter */ true);
        }

    }
}
