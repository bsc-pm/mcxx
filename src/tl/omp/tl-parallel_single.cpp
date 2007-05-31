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
    void OpenMPTransform::parallel_single_preorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
    {
        // Allocate a new element for inner reductions
        ObjectList<OpenMP::ReductionIdExpression> inner_reductions;
        inner_reductions_stack.push(inner_reductions);

        // Increase the parallel nesting value
        parallel_nesting++;
    }

    void OpenMPTransform::parallel_single_postorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
    {
        // One more parallel seen
        num_parallels++;

        // Decrease the parallel nesting
        parallel_nesting--;

        // Get the directive
        OpenMP::Directive directive = parallel_single_construct.directive();

        // Get the enclosing function definition
        FunctionDefinition function_definition = parallel_single_construct.get_enclosing_function();
        // its scope
        Scope function_scope = function_definition.get_scope();
        // and the id-expression of the function name
        IdExpression function_name = function_definition.get_function_name();

        // They will hold the entities as they appear in the clauses
        ObjectList<IdExpression> shared_references;
        ObjectList<IdExpression> private_references;
        ObjectList<IdExpression> firstprivate_references;
        ObjectList<IdExpression> lastprivate_references;
        ObjectList<OpenMP::ReductionIdExpression> reduction_references;
        ObjectList<IdExpression> copyin_references;
        ObjectList<IdExpression> copyprivate_references;

        // Get the construct_body of the statement
        Statement construct_body = parallel_single_construct.body();

        // Get the data attributes for every entity
        get_data_attributes(function_scope,
                directive,
                construct_body,
                shared_references,
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                copyprivate_references);

        // Create the replacement map and fill the parameter info list
        ObjectList<ParameterInfo> parameter_info_list;
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
        AST_t outline_code = get_outline_parallel_single(
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

        // Now create the spawning code. Pass by pointer list and
        // reductions are needed for proper pass of data and reduction
        // vectors declaration

        OpenMP::Clause num_threads = directive.num_threads_clause();
        OpenMP::CustomClause groups_clause = directive.custom_clause("groups");

        Source instrument_code_before;
        Source instrument_code_after;

        if (instrumentation_requested())
        {
            instrument_code_before
                << "const int EVENT_PARALLEL = 60000001;"
                << "const int VALUE_PARALLEL_SINGLE = 4;"
                << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_SINGLE);"
                << "mintaka_state_schedule();"
                ;
            instrument_code_after
                << "const int VALUE_PARALLEL_CLOSE = 0;"
                << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_CLOSE);"
                << "mintaka_state_run();"
                ;
        }

        AST_t spawn_code = get_parallel_spawn_code(
                parallel_single_construct.get_ast(),
                function_definition,
                parallel_single_construct.get_scope(),
                parallel_single_construct.get_scope_link(),
                parameter_info_list,
                reduction_references,
                num_threads,
                groups_clause,
                instrument_code_before,
                instrument_code_after
                );

        // Discard inner reductions information
        inner_reductions_stack.pop();

        // Now replace the whole construct with spawn_code
        parallel_single_construct.get_ast().replace(spawn_code);
    }

    AST_t OpenMPTransform::get_outline_parallel_single(
            FunctionDefinition function_definition,
            Source outlined_function_name,
            Statement construct_body,
            ReplaceIdExpression replace_references,
            ObjectList<ParameterInfo> parameter_info,
            ObjectList<IdExpression> private_references,
            ObjectList<IdExpression> firstprivate_references,
            ObjectList<IdExpression> lastprivate_references,
            ObjectList<OpenMP::ReductionIdExpression> reduction_references,
            ObjectList<IdExpression> copyin_references,
            ObjectList<IdExpression> copyprivate_references
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
                parameter_info);

        // Replace references using set "replace_references" over construct body
        Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

        Source private_declarations = get_privatized_declarations(
                private_references,
                firstprivate_references,
                lastprivate_references,
                reduction_references,
                copyin_references,
                parameter_info
                ); 

        Source reduction_update = get_reduction_update(reduction_references);

        Source single_source;

        Source barrier_code;

        Source instrumentation_code_before, instrumentation_code_after;
        single_source
            << "{"
            <<   "int nth_low;"
            <<   "int nth_upper;"
            <<   "int nth_step;"
            <<   "int nth_chunk;"
            <<   "int nth_schedule;"
            <<   "int nth_dummy1;"
            <<   "int nth_dummy2;"
            <<   "int nth_dummy3;"
            <<   "int nth_barrier; "

            <<   "nth_low = 0;"
            <<   "nth_upper = 0;"
            <<   "nth_step = 1;"
            <<   "nth_schedule = 2;" // Dynamic
            <<   "nth_chunk = 1;"

            //                    <<   "extern void in__tone_begin_for_(int*, int*, int*, int*, int*);"
            //                    <<   "extern int in__tone_next_iters_(int*, int*, int*);"
            //                    <<   "extern void in__tone_end_for_(int*);"

            <<   "in__tone_begin_for_ (&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"
            <<   "while (in__tone_next_iters_ (&nth_dummy1, &nth_dummy2, &nth_dummy3) != 0)"
            <<   "{"
            <<       instrumentation_code_before
            <<       modified_parallel_body_stmt.prettyprint()
            <<       instrumentation_code_after
            <<   "}"
            <<   barrier_code
            << "}"
            ;

        barrier_code << "nth_barrier = 1;";
        barrier_code << "in__tone_end_for_(&nth_barrier);";

        instrumentation_outline(instrumentation_code_before,
                instrumentation_code_after, 
                function_definition,
                construct_body);

        Source task_block_code;

        parallel_body 
            << private_declarations
            << single_source
            << task_block_code
            ;

        task_block_code = get_task_block_code();

        // std::cerr << "OUTLINE CODE" << std::endl;
        // std::cerr << outline_parallel.get_source(true) << std::endl;
        // std::cerr << "End OUTLINE CODE" << std::endl;

        AST_t result;

        result = outline_parallel.parse_global(function_definition.get_ast(), 
                function_definition.get_scope_link());

        return result;
    }
}
