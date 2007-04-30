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
#include "cxx-utils.h"
#include "tl-omp.hpp"
#include "tl-omptransform.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include "tl-externalvars.hpp"
#include <iostream>
#include <utility>
#include <stack>
#include <set>


namespace TL
{
    class ParameterInfo
    {
        public:
            typedef enum 
            {
                UNKNOWN = 0,
                BY_VALUE,
                BY_POINTER,
                BY_REFERENCE // Unused
            } parameter_kind_t;

            std::string parameter_name;
            std::string argument_name;
            Type type;
            parameter_kind_t kind;
            IdExpression id_expression;
            Symbol symbol;

            ParameterInfo(const std::string& _parameter_name, 
                    const std::string& _argument_name, 
                    IdExpression _id_expression, 
                    Type _type, 
                    parameter_kind_t _kind)
                : parameter_name(_parameter_name), 
                argument_name(_argument_name), 
                type(_type), 
                kind(_kind), 
                id_expression(_id_expression), 
                symbol(_id_expression.get_symbol())
            {
            }
    };

    /*
     * What is an IdExpression ?
     * -------------------------
     *
     * An id-expression (IdExpression) is a name in C/C++.
     *
     * In C it will be just a plain identifier (ah!, plain easy C :)
     *
     *    void f(void)
     *    {
     *      a = 3;     // a
     *    }
     *
     * In C++ it can be an unqualified name or a qualified one.
     *
     *    namespace A
     *    {
     *       int b;
     *    }
     *    void f()
     *    {
     *      A::b = 3;   // A::b
     *    }
     *
     * It can be "seasoned" with funny templates not very supported (yet) in this transformation phase
     *
     *    template <class T>
     *    struct A
     *    {
     *       static T a;
     *    };
     *
     *    void f()
     *    {
     *       A<int>::a = 3;  // A<int>::a
     *       A<float>::a = 3.4f; // A<float>::a
     *    }
     *
     * Or like this
     *
     *    template <class T>
     *    struct A
     *    {
     *       void f()
     *       {
     *          // This is an assignment
     *          T::template B<int>::a = 3; // T::template B<int>::a
     *       }
     *    };
     *
     * In fact, an IdExpression object catches an ocurrence of a symbol. Thus,
     * two different IdExpression can refer to the same Symbol. This is the
     * reason you will see around lots of "IdExpression::get_symbol" because we
     * do not want repeated symbols in many places.
     *
     */

    class OpenMPTransform : public OpenMP::OpenMPPhase
    {
        private:
            // Here we declare "persistent" variables

            // The number of parallel regions seen so far
            // (technically not, since it is updated in the postorder of these)
            int num_parallels;

            // The nesting for parallel, updated both in preorder and postorder
            int parallel_nesting;

            // A stack to save the number of "section" within a "sections".  If
            // just a scalar was used, we would only be able to handle one
            // level of sections
            std::stack<int> num_sections_stack;

            // Stores the innermost induction variable of a parallel for or for construct
            std::stack<IdExpression> induction_var_stack;

            // Stores the non orphaned reduction of the enclosing parallel (if any)
            std::stack<ObjectList<OpenMP::ReductionIdExpression> > inner_reductions_stack;

            // A set to save what critical names have been defined in
            // translation unit level
            std::set<std::string> criticals_defined;
        public:
            OpenMPTransform()
            {
            }

            bool instrumentation_requested()
            {
                return (ExternalVars::get("instrument", "0") == "1");
            }

            virtual ~OpenMPTransform()
            {
                // This is needed since "init" is a virtual method
            }

            virtual void init()
            {
                if (ExternalVars::get("nanos_new_interface", "0") != "1")
                {
                    std::cerr << "OpenMP: Using old interface 'nthf_create_1s_vp_' for parallel spawn." << std::endl;
                    std::cerr << "OpenMP: Use '--variable=nanos_new_interface:1' to enable the newer interface" << std::endl;
                }

                // This function is called in OpenMPPhase::run. The user
                // can register here the handlers that will be called for
                // every construction (in preorder and postorder)
                //
                // Register the handlers (callbacks) for every construction

                // #pragma omp parallel
                on_parallel_pre.connect(functor(&OpenMPTransform::parallel_preorder, *this));
                on_parallel_post.connect(functor(&OpenMPTransform::parallel_postorder, *this));

                // #pragma omp parallel for
                on_parallel_for_pre.connect(functor(&OpenMPTransform::parallel_for_preorder, *this));
                on_parallel_for_post.connect(functor(&OpenMPTransform::parallel_for_postorder, *this));

                // #pragma omp for
                on_for_pre.connect(functor(&OpenMPTransform::for_preorder, *this));
                on_for_post.connect(functor(&OpenMPTransform::for_postorder, *this));

                // #pragma omp parallel sections 
                on_parallel_sections_pre.connect(functor(&OpenMPTransform::parallel_sections_preorder, *this));
                on_parallel_sections_post.connect(functor(&OpenMPTransform::parallel_sections_postorder, *this));
                
                // #pragma omp sections
                on_sections_pre.connect(functor(&OpenMPTransform::sections_preorder, *this));
                on_sections_pre.connect(functor(&OpenMPTransform::sections_postorder, *this));
                
                // #pragma omp section
                on_section_post.connect(functor(&OpenMPTransform::section_postorder, *this));

                // #pragma omp barrier
                on_barrier_post.connect(functor(&OpenMPTransform::barrier_postorder, *this));
                
                // #pragma omp atomic
                on_atomic_post.connect(functor(&OpenMPTransform::atomic_postorder, *this));

                // #pragma omp ordered
                on_ordered_post.connect(functor(&OpenMPTransform::ordered_postorder, *this));

                // #pragma omp master
                on_master_post.connect(functor(&OpenMPTransform::master_postorder, *this));

                // #pragma omp single
                on_single_post.connect(functor(&OpenMPTransform::single_postorder, *this));
                
                // #pragma omp parallel single
                on_parallel_single_pre.connect(functor(&OpenMPTransform::parallel_single_preorder, *this));
                on_parallel_single_post.connect(functor(&OpenMPTransform::parallel_single_postorder, *this));
                
                // #pragma omp critical
                on_critical_post.connect(functor(&OpenMPTransform::critical_postorder, *this));
                
                // #pragma omp flush
                on_flush_post.connect(functor(&OpenMPTransform::flush_postorder, *this));

                // #pragma omp threadprivate
                on_threadprivate_post.connect(functor(&OpenMPTransform::threadprivate_postorder, *this));

                // #pragma omp task
                on_custom_construct_post["task"].connect(functor(&OpenMPTransform::task_postorder, *this));

                // #pragma omp directive taskwait
                on_custom_construct_post["taskwait"].connect(functor(&OpenMPTransform::taskwait_postorder, *this));
                
                // #pragma omp directive taskgroup
                on_custom_construct_post["taskgroup"].connect(functor(&OpenMPTransform::taskgroup_postorder, *this));

                // #pragma omp directive taskyield
                on_custom_construct_post["taskyield"].connect(functor(&OpenMPTransform::taskyield_postorder, *this));

                // #pragma omp construct transaction
                on_custom_construct_post["transaction"].connect(functor(&OpenMPTransform::transaction_postorder, *this));
            }
            
            // Parallel in preorder
            void parallel_preorder(OpenMP::ParallelConstruct parallel_construct)
            {
                ObjectList<OpenMP::ReductionIdExpression> inner_reductions;
                inner_reductions_stack.push(inner_reductions);
                
                // Increase the parallel nesting value
                parallel_nesting++;
            }

            // Parallel in postorder
            void parallel_postorder(OpenMP::ParallelConstruct parallel_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Decrease the parallel nesting value
                parallel_nesting--;

                // Get the directive
                OpenMP::Directive directive = parallel_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_construct.get_enclosing_function();
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
                Statement construct_body = parallel_construct.body();

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
                OpenMP::CustomClause noinstr_clause = directive.custom_clause("noinstr");
                
                AST_t outline_code  = get_outline_parallel(
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
                
                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");

                Source instrument_code_before;
                Source instrument_code_after;
                if (instrumentation_requested())
                {
                    instrument_code_before
                        << "const int EVENT_PARALLEL = 60000001;"
                        << "const int VALUE_PARALLEL_REGION = 3;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_REGION);"
                        << "mintaka_state_schedule();"
                        ;
                    instrument_code_after
                        << "const int VALUE_PARALLEL_CLOSE = 0;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_CLOSE);"
                        << "mintaka_state_run();"
                        ;
                }
                
                AST_t spawn_code = get_parallel_spawn_code(
                        parallel_construct.get_ast(),
                        function_definition,
                        parallel_construct.get_scope(),
                        parallel_construct.get_scope_link(),
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
                parallel_construct.get_ast().replace(spawn_code);
            }

            void parallel_for_preorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                ObjectList<OpenMP::ReductionIdExpression> inner_reductions;
                inner_reductions_stack.push(inner_reductions);
                
                // Increase the parallel nesting value
                parallel_nesting++;

                Statement construct_body = parallel_for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
                
                IdExpression induction_var = for_statement.get_induction_variable();

                // Save this induction var in the stack
                induction_var_stack.push(induction_var);
            }

            void parallel_for_postorder(OpenMP::ParallelForConstruct parallel_for_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Remove the induction var from the stack
                induction_var_stack.pop();

                // Decrease the parallel nesting level 
                parallel_nesting--;

                // Get the directive
                OpenMP::Directive directive = parallel_for_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_for_construct.get_enclosing_function();
                Scope function_scope = function_definition.get_scope();
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
                Statement construct_body = parallel_for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
                Statement loop_body = for_statement.get_loop_body();

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
                
                // The induction variable deserves special treatment
                IdExpression induction_var = for_statement.get_induction_variable();
                // If private_references does not contain an IdExpression whose
                // related symbol is the induction variable symbol, then
                // privatize here (FIXME: I've seen codes where the induction
                // variable appears in lastprivate)
                if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
                {
                    // Add the induction variale onto the private references
                    private_references.append(induction_var);
                    // And now remove, if it was already there, any reference
                    // to the induction variable symbol from the set of shared
                    // references
                    shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
                            induction_var.get_symbol());
                }

                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;

                // Create the replacement map and the pass_by_pointer set
                ObjectList<ParameterInfo> parameter_info_list;
                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
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
                        copyprivate_references,
                        directive);

                // Now prepend the outline
                function_definition.get_ast().prepend_sibling_function(outline_code);

                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");

                Source instrument_code_before;
                Source instrument_code_after;
                if (instrumentation_requested())
                {
                    instrument_code_before
                        << "const int EVENT_PARALLEL = 60000001;"
                        << "const int VALUE_PARALLEL_FOR = 1;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_FOR);"
                        << "mintaka_state_schedule();"
                        ;
                    instrument_code_after
                        << "const int VALUE_PARALLEL_CLOSE = 0;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_CLOSE);"
                        << "mintaka_state_run();"
                        ;
                }

                AST_t spawn_code = get_parallel_spawn_code(
                        parallel_for_construct.get_ast(),
                        function_definition,
                        parallel_for_construct.get_scope(),
                        parallel_for_construct.get_scope_link(),
                        parameter_info_list,
                        reduction_references,
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

            void for_preorder(OpenMP::ForConstruct for_construct)
            {
                Statement construct_body = for_construct.body();
                // The construct is in fact a ForStatement in a #pragma omp parallel do
                ForStatement for_statement(construct_body);
                
                IdExpression induction_var = for_statement.get_induction_variable();

                // Save this induction var in the stack
                induction_var_stack.push(induction_var);
            }
            
            void for_postorder(OpenMP::ForConstruct for_construct)
            {
                OpenMP::Directive directive = for_construct.directive();
                Statement construct_body = for_construct.body();
                ForStatement for_statement = construct_body;
                Statement loop_body = for_statement.get_loop_body();

                // Remove the induction var from the stack
                induction_var_stack.pop();

                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                ObjectList<IdExpression> copyin_references;
                ObjectList<IdExpression> copyprivate_references;
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = for_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();
                
                // Get the data attributes for every entity
                get_data_explicit_attributes(function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        copyprivate_references);
                
                // The induction variable deserves special treatment
                IdExpression induction_var = for_statement.get_induction_variable();
                // If private_references does not contain an IdExpression whose
                // related symbol is the induction variable symbol, then
                // privatize here (FIXME: I've seen codes where the induction
                // variable appears in lastprivate)
                if (!private_references.contains(functor(&IdExpression::get_symbol), induction_var.get_symbol()))
                {
                    // Add the induction variale onto the private references
                    private_references.append(induction_var);
                    // And now remove, if it was already there, any reference
                    // to the induction variable symbol from the set of shared
                    // references
                    shared_references = shared_references.not_find(functor(&IdExpression::get_symbol), 
                            induction_var.get_symbol());
                }

                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;

                // The lists of entities passed by pointer and entities
                // privatized in the outline
                ObjectList<ParameterInfo> parameter_info_list;
                // Create the replacement map and the pass_by_pointer set
                ReplaceIdExpression replace_references  = 
                    set_replacements(function_definition,
                            directive,
                            loop_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            reduction_empty,
                            copyin_references,
                            copyprivate_references,
                            parameter_info_list);

                Source parallel_for_body;

                parameter_info_list.clear();
                
                Source private_declarations = get_privatized_declarations(
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        parameter_info_list
                        ); 

                Source loop_distribution_code = get_loop_distribution_code(for_statement,
                        replace_references, function_definition, directive);

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(
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

                OpenMP::Clause nowait_clause = directive.nowait_clause();

                Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

                Source reduction_code;

                parallel_for_body
                    << "{"
                    <<    private_declarations
                    <<    loop_distribution_code
                    <<    lastprivate_code
                    <<    reduction_code
                    <<    loop_finalization
                    << "}"
                    ;

                bool orphaned = (parallel_nesting == 0);
                if (orphaned)
                {
                    reduction_code = get_critical_reduction_code(reduction_references);
                }
                else
                {
                    reduction_code = get_noncritical_inlined_reduction_code(reduction_references);
                }

                AST_t result;
                result = parallel_for_body.parse_statement(loop_body.get_ast(), 
                         loop_body.get_scope_link());

                for_construct.get_ast().replace(result);
            }

            void parallel_sections_preorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct)
            {
                ObjectList<OpenMP::ReductionIdExpression> inner_reductions;
                inner_reductions_stack.push(inner_reductions);
                
                // Increase the parallel nesting value
                parallel_nesting++;

                // We push a new level of sections with zero "section" counted
                // so far
                num_sections_stack.push(0);
            }

            void parallel_sections_postorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Decrease the parallel nesting
                parallel_nesting--;
                
                // Get the directive
                OpenMP::Directive directive = parallel_sections_construct.directive();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = parallel_sections_construct.get_enclosing_function();
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
                Statement construct_body = parallel_sections_construct.body();

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

                // Create the outline for parallel sections using 
                // the privatized entities and pass by pointer
                // lists.
                // Additionally {first|last}private and reduction
                // entities are needed for proper initializations
                // and assignments.
                AST_t outline_code = get_outline_parallel_sections(
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

                OpenMP::Clause num_threads = directive.num_threads_clause();
                OpenMP::CustomClause groups_clause = directive.custom_clause("groups");
                
                // Now create the spawning code. Pass by pointer list and
                // reductions are needed for proper pass of data and reduction
                // vectors declaration
                Source instrument_code_before;
                Source instrument_code_after;
                if (instrumentation_requested())
                {
                    instrument_code_before
                        << "const int EVENT_PARALLEL = 60000001;"
                        << "const int VALUE_PARALLEL_SECTIONS = 2;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_SECTIONS);"
                        << "mintaka_state_schedule();"
                        ;
                    instrument_code_after
                        << "const int VALUE_PARALLEL_CLOSE = 0;"
                        << "mintaka_event(EVENT_PARALLEL, VALUE_PARALLEL_CLOSE);"
                        << "mintaka_state_run();"
                        ;
                }

                AST_t spawn_code = get_parallel_spawn_code(
                        parallel_sections_construct.get_ast(),
                        function_definition,
                        parallel_sections_construct.get_scope(),
                        parallel_sections_construct.get_scope_link(),
                        parameter_info_list,
                        reduction_references,
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

            void sections_preorder(OpenMP::SectionsConstruct sections_construct)
            {
                // We push a new level of sections with zero "section" counted
                // so far
                num_sections_stack.push(0);
            }

            void sections_postorder(OpenMP::SectionsConstruct sections_construct)
            {
                // They will hold the entities as they appear in the clauses
                ObjectList<IdExpression> shared_references;
                ObjectList<IdExpression> private_references;
                ObjectList<IdExpression> firstprivate_references;
                ObjectList<IdExpression> lastprivate_references;
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;
                ObjectList<IdExpression> copyin_references;
                ObjectList<IdExpression> copyprivate_references;
                
                // Get the construct_body of the statement
                OpenMP::Directive directive = sections_construct.directive();
                Statement construct_body = sections_construct.body();
                
                // Get the enclosing function definition
                FunctionDefinition function_definition = sections_construct.get_enclosing_function();
                // its scope
                Scope function_scope = function_definition.get_scope();

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

                ObjectList<ParameterInfo> parameter_info_list;

                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;

                ReplaceIdExpression replace_references = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            shared_references,
                            private_references,
                            firstprivate_references,
                            lastprivate_references,
                            reduction_references,
                            reduction_empty,
                            copyin_references,
                            copyprivate_references,
                            parameter_info_list);

                int num_sections = num_sections_stack.top();

                Source loop_distribution_code;

                loop_distribution_code = get_loop_distribution_in_sections(num_sections,
                        construct_body,
                        replace_references);

                // In fact we are not passing anything by parameters since
                // there is no outline here
                parameter_info_list.clear();

                Source private_declarations = get_privatized_declarations(
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        parameter_info_list
                        ); 

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(
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

                OpenMP::Clause nowait_clause = directive.nowait_clause();
                Source loop_finalization = get_loop_finalization(/*do_barrier=*/!(nowait_clause.is_defined()));

                Source reduction_code;

                bool orphaned = (parallel_nesting == 0);
                if (orphaned)
                {
                    reduction_code = get_critical_reduction_code(reduction_references);
                }
                else
                {
                    reduction_code = get_noncritical_inlined_reduction_code(reduction_references);
                            
                }

                Source sections_source;
                sections_source 
                    << "{"
                    <<    private_declarations
                    <<    loop_distribution_code
                    <<    lastprivate_code
                    <<    reduction_code
                    <<    loop_finalization
                    << "}"
                    ;

                num_sections_stack.pop();

                AST_t sections_tree = sections_source.parse_statement(sections_construct.get_ast(),
                        sections_construct.get_scope_link());

                sections_construct.get_ast().replace(sections_tree);
            }

            void section_postorder(OpenMP::SectionConstruct section_construct)
            {
                int &num_sections = num_sections_stack.top();

                Source section_source, instrumentation_before, instrumentation_after;
                Statement construct_body = section_construct.body();

                section_source
                    << "case " << num_sections << ":"
                    << "{"
                    <<    instrumentation_before
                    <<    construct_body.prettyprint()
                    <<    instrumentation_after
                    <<    "break;"
                    << "}"
                    ;

                if (instrumentation_requested())
                {
                    instrumentation_before
                        << "mintaka_state_run();"
                        ;
                        
                    instrumentation_before
                        << "mintaka_state_synch();"
                        ;
                }

                AST_t section_tree = section_source.parse_statement(section_construct.get_ast(),
                        section_construct.get_scope_link());

                // One more section
                num_sections++;

                section_construct.get_ast().replace(section_tree);
            }

            void barrier_postorder(OpenMP::BarrierDirective barrier_directive)
            {
                Source barrier_source;

                Source instrumentation_code_before, instrumentation_code_after;

                if (instrumentation_requested())
                {
                    instrumentation_code_before
                        << "int __previous_state = mintaka_get_state();"
                        << "mintaka_state_synch();"
                        ;

                    instrumentation_code_after
                        << "mintaka_set_state(__previous_state);"
                        ;
                }

                barrier_source
                    << "{"
//                    <<    "extern void in__tone_barrier_();"
                    <<    instrumentation_code_before
                    <<    "in__tone_barrier_();"
                    <<    instrumentation_code_after
                    << "}"
                    ;

                AST_t barrier_tree = barrier_source.parse_statement(barrier_directive.get_ast(),
                        barrier_directive.get_scope_link());

                barrier_directive.get_ast().replace(barrier_tree);
            }

            void atomic_postorder(OpenMP::AtomicConstruct atomic_construct)
            {
                // TODO - An atomic can be implemented better
                Source critical_source;

                Statement critical_body = atomic_construct.body();

                critical_source
                    << "{"
                    <<   "static nth_word_t default_mutex_var;"
//                    <<   "extern void nthf_spin_lock_(void*);"
//                    <<   "extern void nthf_spin_unlock_(void*);"
                    <<   "nthf_spin_lock_(&default_mutex_var);"
                    <<   critical_body.prettyprint()
                    <<   "nthf_spin_unlock_(&default_mutex_var);"
                    << "}"
                    ;

                AST_t atomic_tree = critical_source.parse_statement(atomic_construct.get_ast(),
                        atomic_construct.get_scope_link());

                atomic_construct.get_ast().replace(atomic_tree);
            }

            void ordered_postorder(OpenMP::OrderedConstruct ordered_construct)
            {
                IdExpression induction_var = induction_var_stack.top();

                Statement construct_body = ordered_construct.body();
                Source ordered_source;

                ordered_source
                    << "{"
                    <<   "in__tone_enter_ordered_ (& "<< induction_var.prettyprint() << ");"
                    <<   construct_body.prettyprint()
                    <<   "in__tone_leave_ordered_ (&" << induction_var.prettyprint() << ");"
                    << "}"
                    ;

                AST_t ordered_code = ordered_source.parse_statement(ordered_construct.get_ast(),
                        ordered_construct.get_scope_link());

                ordered_construct.get_ast().replace(ordered_code);
            }

            void master_postorder(OpenMP::MasterConstruct master_construct)
            {
                Source master_source;

                Statement statement = master_construct.body();

                master_source
                    << "if (in__tone_is_master_())"
                    << "{"
                    <<    statement.prettyprint()
                    << "}"
                    ;

                AST_t master_tree = master_source.parse_statement(master_construct.get_ast(),
                        master_construct.get_scope_link());

                master_construct.get_ast().replace(master_tree);
            }

            void single_postorder(OpenMP::SingleConstruct single_construct)
            {
                Source single_source;
                Source barrier_code;

                Statement body_construct = single_construct.body();
                OpenMP::Directive directive = single_construct.directive();

                Source instrumentation_code_before, instrumentation_code_after;

                single_source
                    << "{"
                    <<   instrumentation_code_before
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
                    <<       instrumentation_code_after
                    <<       body_construct.prettyprint()
                    <<       instrumentation_code_before
                    <<   "}"
                    <<   barrier_code
                    <<   instrumentation_code_after
                    << "}"
                    ;

                if (instrumentation_requested())
                {
                    instrumentation_code_before
                        << "mintaka_state_synch();"
                        ;
                    instrumentation_code_after
                        << "mintaka_state_run();"
                        ;
                }

                OpenMP::Clause nowait_clause = directive.nowait_clause();
                barrier_code = get_loop_finalization(!(nowait_clause.is_defined()));

                AST_t single_tree = single_source.parse_statement(single_construct.get_ast(), 
                        single_construct.get_scope_link());

                single_construct.get_ast().replace(single_tree);
            }

            void parallel_single_preorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
            {
                // Allocate a new element for inner reductions
                ObjectList<OpenMP::ReductionIdExpression> inner_reductions;
                inner_reductions_stack.push(inner_reductions);
                
                // Increase the parallel nesting value
                parallel_nesting++;
            }

            void parallel_single_postorder(OpenMP::ParallelSingleConstruct parallel_single_construct)
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

            void critical_postorder(OpenMP::CriticalConstruct critical_construct)
            {
                Source critical_source;

                OpenMP::Directive directive = critical_construct.directive();
                Statement critical_body = critical_construct.body();
                ScopeLink scope_link = critical_construct.get_scope_link();
                
                OpenMP::Clause region_name = directive.parameter_clause();

                std::string mutex_variable;

                if (!region_name.is_defined())
                {
                    mutex_variable = "_nthf_unspecified_critical";
                }
                else
                {
                    ObjectList<IdExpression> id_expressions = region_name.id_expressions(TL::ALL_FOUND_SYMBOLS);
                    IdExpression head = id_expressions[0];

                    mutex_variable = "_nthf_"  + head.prettyprint();
                }

                critical_source
                    << "{"
//                    <<   "extern void nthf_spin_lock_(void*);"
//                    <<   "extern void nthf_spin_unlock_(void*);"
                    <<   "nthf_spin_lock_(&" << mutex_variable << ");"
                    <<   critical_body.prettyprint()
                    <<   "nthf_spin_unlock_(&" << mutex_variable << ");"
                    << "}"
                    ;

                define_global_mutex(mutex_variable, critical_construct.get_ast(),
                        critical_construct.get_scope_link());

                AST_t critical_tree = critical_source.parse_statement(critical_construct.get_ast(),
                        critical_construct.get_scope_link());

                critical_construct.get_ast().replace(critical_tree);
            }

            void define_global_mutex(std::string mutex_variable, AST_t ref_tree, ScopeLink sl)
            {
                if (criticals_defined.find(mutex_variable) == criticals_defined.end())
                {
                    // Now declare, if not done before
                    Source critical_mutex_def_src;

                    critical_mutex_def_src <<
                        "nth_word_t " << mutex_variable << ";"
                        ;

                    // AST_t translation_unit = critical_construct.get_ast().get_translation_unit();
                    // Scope scope_translation_unit = scope_link.get_scope(translation_unit);

                    AST_t critical_mutex_def_tree = critical_mutex_def_src.parse_global(ref_tree, sl);

                    ref_tree.prepend_sibling_function(critical_mutex_def_tree);

                    criticals_defined.insert(mutex_variable);
                }
            }

            void threadprivate_postorder(OpenMP::ThreadPrivateDirective threadprivate_directive)
            {
                // Given
                //
                //    int a, b, c;
                //    #pragma omp threadprivate(b)
                //
                // The compiler will create
                // 
                //    int a;
                //    int __thread b;
                //    int c;
                //

                // Get the threadprivate directive
                OpenMP::Directive directive = threadprivate_directive.directive();

                // And get its parameter clause (you can see the (...) as a
                // clause without name, we'll call it "parameter_clause")
                OpenMP::Clause clause = directive.parameter_clause();

                // Now get the list of symbols of this clause
                ObjectList<IdExpression> threadprivate_references = clause.id_expressions();

                // For every symbol in the clause
                for (ObjectList<IdExpression>::iterator it = threadprivate_references.begin();
                        it != threadprivate_references.end();
                        it++)
                {
                    // Get its declaration
                    Declaration decl = it->get_declaration();

                    // A declaration has two parts, a DeclarationSpec and a list of
                    // DeclaredEntity.
                    //
                    // const int static * a, b; 
                    //  
                    //    const int static -> DeclarationSpec
                    //    *a, b            -> ObjectList<DeclaredEntity>
                    //
                    DeclarationSpec decl_spec = decl.get_declaration_specifiers();
                    ObjectList<DeclaredEntity> declared_entities = decl.get_declared_entities();

                    // This will hold the remade declaration
                    Source remade_declaration;

                    // For every entity declared
                    for (ObjectList<DeclaredEntity>::iterator it2 = declared_entities.begin();
                            it2 != declared_entities.end();
                            it2++)
                    {
                        // Prettyprint the DeclarationSpec (to make it like it was before)
                        remade_declaration << decl_spec.prettyprint() << " ";

                        // And if the declaration appears in the threadprivate
                        // add the non-portable decl-specifier "__thread". 
                        //
                        // Note that it must be at the end of the declaration
                        // specifiers (this is a gcc requirement)
                        if (it2->get_declared_entity().get_symbol() == it->get_symbol())
                        {
                            remade_declaration << " __thread ";
                        }

                        remade_declaration << it2->prettyprint();

                        // If the entity has an initializer like "b" below
                        //
                        //    int a, b = 3, c;
                        //
                        //  then, write it down
                        if (it2->has_initializer())
                        {
                            remade_declaration << it2->get_initializer().prettyprint()
                                ;
                        }

                        // End the declaration (obviously this only work for declaration statements,
                        // arguments, at the moment, cannot be threadprivatized :)
                        remade_declaration << ";"
                            ;
                    }

                    // Now parse the remade declarations
                    AST_t redeclaration_tree = remade_declaration.parse_declaration(decl.get_ast(),
                            // And explicitly allow to redeclarate objects otherwise the compiler
                            // will complain (for debugging purposes)
                            scope_link, Source::ALLOW_REDECLARATION);

                    // Now replace the whole declaration with this new one
                    decl.get_ast().replace(redeclaration_tree);
                }

                // This directive must be removed
                threadprivate_directive.get_ast().remove_in_list();
            }

            void task_postorder(OpenMP::CustomConstruct task_construct)
            {
                // One more parallel seen
                num_parallels++;

                // Get the directive of the task construct
                OpenMP::Directive directive = task_construct.directive();

                // Get the related statement of this task construct
                Statement construct_body = task_construct.body();

                // Get the enclosing function definition
                FunctionDefinition function_definition = task_construct.get_enclosing_function();
                // and its scope
                Scope function_scope = function_definition.get_scope();
                // and the id-expression of the function name
                IdExpression function_name = function_definition.get_function_name();
                // create the outlined function name
                Source outlined_function_name = get_outlined_function_name(function_name);

                // Get references in local clause
                ObjectList<std::string> local_names;
                local_names.append("local");
                local_names.append("taskprivate");
                local_names.append("task_private");
                OpenMP::CustomClause local_clause = directive.custom_clause(local_names);
                ObjectList<IdExpression> local_references_in_clause = local_clause.id_expressions();
                // Those stated by the user to be local are local_references
                ObjectList<IdExpression> local_references = local_references_in_clause;

                // Get references in captureaddress clause
                ObjectList<std::string> captureaddress_names;
                captureaddress_names.append("captureaddress");
                captureaddress_names.append("capture_address");
                OpenMP::CustomClause captureaddress_clause = directive.custom_clause(captureaddress_names);

                // Get all the identifiers of the captureaddress clause
                ObjectList<IdExpression> captureaddress_references;
                ObjectList<IdExpression> captureaddress_references_in_clause = captureaddress_clause.id_expressions();
                {
                    // We discard symbols here referenced in captureaddress
                    // clause that can be referenced in the outline (thus, they
                    // come from an outer scope to this whole function)
                    for (ObjectList<IdExpression>::iterator it = captureaddress_references_in_clause.begin();
                            it != captureaddress_references_in_clause.end();
                            it++)
                    {
                        Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                        if (!global_sym.is_valid() 
                                || global_sym != it->get_symbol()
                                || is_unqualified_member_symbol(*it, function_definition))
                        {
                            // If the symbol found in the function scope is not
                            // the same as the one referenced in the
                            // captureaddress it will be really
                            // 'captureaddressed', otherwise it can be
                            // referenced from the outline
                            captureaddress_references.append(*it);
                        }
                    }
                }

                ObjectList<std::string> capturevalue_names;
                capturevalue_names.append("capturevalue");
                capturevalue_names.append("capture_value");
                OpenMP::CustomClause capturevalue_clause = directive.custom_clause(capturevalue_names);
                // Get the identifiers of the capturevalue clause
                ObjectList<IdExpression> capturevalue_references_in_clause = capturevalue_clause.id_expressions();

                // As stated by the user, everything in the clause is already
                // capturevalued (no pruning here as we did for captureaddress)
                ObjectList<IdExpression> capturevalue_references = capturevalue_references_in_clause;

                OpenMP::DefaultClause default_clause = directive.default_clause();

                enum 
                {
                    DK_TASK_INVALID = 0,
                    DK_TASK_CAPTUREADDRESS,
                    DK_TASK_CAPTUREVALUE,
                    DK_TASK_LOCAL,
                    DK_TASK_NONE
                } default_task_data_sharing = DK_TASK_INVALID;

                if (!default_clause.is_defined())
                {
                    // By default capturevalue
                    default_task_data_sharing = DK_TASK_CAPTUREADDRESS;
                }
                else if (default_clause.is_none())
                {
                    default_task_data_sharing = DK_TASK_NONE;
                }
                else if (default_clause.is_custom(local_names))
                {
                    default_task_data_sharing = DK_TASK_LOCAL;
                }
                else if (default_clause.is_custom(capturevalue_names))
                {
                    default_task_data_sharing = DK_TASK_CAPTUREVALUE;
                }
                else if (default_clause.is_custom(captureaddress_names))
                {
                    default_task_data_sharing = DK_TASK_CAPTUREADDRESS;
                }
                else
                {
                    std::cerr << "Warning: Unknown default clause '" 
                        << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                        << "Assuming 'default(capturevalue)'."
                        << std::endl;
                    default_task_data_sharing = DK_TASK_CAPTUREVALUE;
                }

                // Now deal with the references of the body
                {
                    // Get all id-expressions in the body construct
                    ObjectList<IdExpression> references_body_all
                        = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);

                    for (ObjectList<IdExpression>::iterator it = references_body_all.begin();
                            it != references_body_all.end();
                            it++)
                    {
                        Symbol global_sym = function_scope.get_symbol_from_id_expr(it->get_ast());

                        // If this symbol appears in any data-sharing clause,
                        // ignore it since it already has an explicit data
                        // sharing attribute
                        //
                        // Note that all captureaddressed things are in
                        // 'captureaddress_references_in_clause',
                        // 'captureaddress_references' might contain less of
                        // them if they are globally accessible
                        if (captureaddress_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)) 
                                || capturevalue_references_in_clause.contains(*it, functor(&IdExpression::get_symbol))
                                || local_references_in_clause.contains(*it, functor(&IdExpression::get_symbol)))
                            continue;

                        bool will_be_visible_from_outline = false;
                        bool is_unqualified_member = false;
                        if (global_sym.is_valid()
                                && (global_sym == it->get_symbol()))
                        {
                            // If the function-scope accessible symbol is the same found
                            // then it must be implicitly captureaddress, instead of capturevalue
                            // but since it is accessible it does not have to be passed
                            //
                            // As an exception member symbols must be passed as
                            // captureaddress and they will be converted to
                            // _this->member
                            will_be_visible_from_outline = true;
                            is_unqualified_member = is_unqualified_member_symbol(*it, function_definition);
                        }

                        switch ((int)default_task_data_sharing)
                        {
                            case DK_TASK_NONE :
                                {
                                    std::cerr << "Warning: '" << it->prettyprint() << "' in " << it->get_ast().get_locus() 
                                        << " does not have a data sharing attribute and 'default(none)' was specified. "
                                        << "It will be considered capturevalue." << std::endl;
                                    /* Fall through capturevalue */
                                }
                            case DK_TASK_CAPTUREVALUE :
                                {
                                    capturevalue_references.insert(*it, functor(&IdExpression::get_symbol));
                                    break;
                                }
                            case DK_TASK_CAPTUREADDRESS :
                                {
                                    // If is not visible from the outline (or
                                    // if it is, it is an unqualified member)
                                    // then add to the captureaddress
                                    if (!will_be_visible_from_outline
                                            || is_unqualified_member)
                                    {
                                        captureaddress_references.insert(*it, functor(&IdExpression::get_symbol));
                                    }
                                    break;
                                }
                            case DK_TASK_LOCAL :
                                {
                                    local_references.insert(*it, functor(&IdExpression::get_symbol));
                                    break;
                                }
                            case DK_TASK_INVALID :
                            default:
                                break;
                        }
                    }
                }

                ObjectList<IdExpression> empty;
                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;
                ObjectList<ParameterInfo> parameter_info_list;

                ObjectList<IdExpression> captured_references;
                captured_references.append(captureaddress_references);
                captured_references.append(capturevalue_references);

                ReplaceIdExpression replace_references  = 
                    set_replacements(function_definition,
                            directive,
                            construct_body,
                            captured_references, // Captured entities (captureaddress and capturevalue)
                            local_references, // Private entities (local clause)
                            empty,
                            empty,
                            reduction_empty,
                            reduction_empty,
                            empty,
                            empty,
                            parameter_info_list,
                            /* all_shared */ true);


                // Fix parameter_info_list
                // Currently set_replacement assumes that everything will be passed BY_POINTER
                // for every entity found in capturevalue_references will be set to BY_VALUE
                //
                // The proper way should be fixing "set_replacements" one day, but already
                // takes too much parameters so a more creative approach will be required
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (capturevalue_references.contains(it->id_expression, functor(&IdExpression::get_symbol)))
                    {
                        it->kind = ParameterInfo::BY_VALUE;
                    }
                }

                // Get the code of the outline
                AST_t outline_code  = get_outline_task(
                        function_definition,
                        outlined_function_name, 
                        construct_body,
                        replace_references,
                        parameter_info_list,
                        local_references);

                // Now prepend the outline
                function_definition.get_ast().prepend_sibling_function(outline_code);

                // Here the spawning code will be created
                Source task_queueing;
                Source task_parameters;
                Source task_parameter_list;

                Source size_vector;

                // For each capture address entity just pass a reference to it
                int num_reference_args = 0;

                // This might be needed for nonstatic member functions
                if (is_nonstatic_member_function(function_definition))
                {
                    task_parameter_list.append_with_separator("this", ",");
                    num_reference_args++;
                }

                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_POINTER)
                        continue;

                    task_parameter_list.append_with_separator("&" + it->id_expression.prettyprint(), ",");
                    num_reference_args++;
                }

                // This vector will hold the sizeof's of entities passed as
                // private references
                bool copy_construction_needed = false;
                size_vector << "size_t nth_size[] = {0";
                int vector_index = 1;
                int num_value_args = 0;

                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_VALUE)
                        continue;

                    // Add the size in the vector
                    size_vector << ", sizeof(" << it->id_expression.prettyprint() << ")"
                        ;

                    // A reference to the vector
                    Source vector_ref;
                    vector_ref << "&nth_size[" << vector_index << "]"
                        ;

                    // First an address with the size must be passed
                    task_parameter_list.append_with_separator(vector_ref.get_source(), ",");
                    task_parameter_list.append_with_separator("&" + it->id_expression.prettyprint(), ",");

                    CXX_LANGUAGE()
                    {
                        Symbol sym = it->id_expression.get_symbol();
                        Type type = sym.get_type();

                        if (type.is_class())
                        {
                            copy_construction_needed = true;
                        }
                    }

                    vector_index++;
                }
                size_vector << "};"
                    ;
                num_value_args = vector_index - 1;

                // A comma only needed when the parameter list is non empty
                if (!task_parameter_list.empty())
                {
                    task_parameters << ", " << task_parameter_list;
                }

                // 'switch' clause support
                Source task_type;
                if (directive.custom_clause("switch").is_defined())
                {
                    task_type << "0xa";
                }
                else
                {
                    task_type << "0xe";
                }

                // This is the code that will be executed if the task cannot be created
                // (i.e. NTH_CANNOT_ALLOCATE_TASK is returned)
                Source fallback_capture_values;
                Source fallback_arguments;

                // This might be needed for nonstatic member functions
                if (is_nonstatic_member_function(function_definition))
                {
                    fallback_arguments.append_with_separator("this", ",");
                }

                // Capture address entities are easy, just pass the vector
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_POINTER)
                        continue;

                    fallback_arguments.append_with_separator("&" + it->id_expression.prettyprint(), ",");
                }

                // For capture value we will be passing pointers to local copies
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_VALUE)
                        continue;

                    Symbol sym = it->id_expression.get_symbol();
                    Type type = sym.get_type();

                    if (!type.is_array())
                    {
                        fallback_capture_values
                            << type.get_declaration_with_initializer(
                                    it->id_expression.get_scope(),
                                    "cval_" + it->id_expression.mangle_id_expression(), 
                                    it->id_expression.prettyprint()) 
                            << ";"
                            ;
                    }
                    else
                    {
                        Source src_array_copy = array_copy(type, "cval_" + it->id_expression.mangle_id_expression(),
                                it->id_expression.prettyprint(), 0);

                        fallback_capture_values
                            << type.get_declaration(it->id_expression.get_scope(),
                                    "cval_" + it->id_expression.mangle_id_expression())
                            << ";"
                            << src_array_copy
                            ;
                    }

                    fallback_arguments.append_with_separator("&cval_" + it->id_expression.mangle_id_expression(), ",");
                }

                Source task_dependency;

                // For C++ only
                Source copy_construction_part;
                if (copy_construction_needed)
                {
                    // The task cannot start immediately because first we have
                    // to copy-construct capture valued entities
                    task_dependency << "1";

                    Source copy_sequence;

                    copy_construction_part 
                        << "else"
                        << "{"
                        <<   "int nth_one_dep = 1;"
                        <<   copy_sequence
                        <<   "nth_depsub(&nth, &nth_one_dep);"
                        << "}"
                        ;

                    int vector_index = 1;
                    for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                            it != parameter_info_list.end();
                            it++)
                    {
                        if (it->kind != ParameterInfo::BY_VALUE)
                            continue;

                        Symbol sym = it->id_expression.get_symbol();
                        Type type = sym.get_type();

                        if (type.is_class())
                        {
                            copy_sequence
                                << "new (nth_arg_addr[" << vector_index << "])" 
                                << type.get_declaration(it->id_expression.get_scope(), "")
                                << "(" << it->id_expression.prettyprint() << ");"
                                ;
                        }

                        vector_index++;
                    }
                }
                else
                {
                    // No dependencies if no construction has to be performed,
                    // i.e. the task can start immediately
                    task_dependency << "0";
                }

                Source outlined_function_reference;
                Source selector_cast;

                outlined_function_reference << get_outline_function_reference(function_definition, parameter_info_list);

                Source instrument_code_task_creation;

                task_queueing
                    << "{"
                    <<    "nth_desc * nth;"
                    <<    "int nth_type = " << task_type << ";"
                    <<    "int nth_ndeps = " << task_dependency << ";"
                    <<    "int nth_vp = 0;"
                    <<    "nth_desc_t* nth_succ = (nth_desc_t*)0;"
                    <<    "int nth_nargs_ref = " << num_reference_args << ";"
                    <<    "int nth_nargs_val = " << num_value_args << ";"
                    <<    "void* nth_arg_addr[" << num_value_args << " + 1];"
                    <<    "void** nth_arg_addr_ptr = nth_arg_addr;"

                    <<    size_vector

                    <<    "nth = nth_create((void*)(" << outlined_function_reference << "), "
                    <<             "&nth_type, &nth_ndeps, &nth_vp, &nth_succ, &nth_arg_addr_ptr, "
                    <<             "&nth_nargs_ref, &nth_nargs_val" << task_parameters << ");"
                    <<    instrument_code_task_creation
                    <<    "if (nth == NTH_CANNOT_ALLOCATE_TASK)"
                    <<    "{"
                    // <<       "fprintf(stderr, \"Cannot allocate task at '%s'\\n\", \"" << task_construct.get_ast().get_locus() << "\");"
                    <<       fallback_capture_values
                    <<       outlined_function_reference << "(" << fallback_arguments << ");"
                    <<    "}"
                    <<    copy_construction_part
                    << "}"
                    ;

                if (instrumentation_requested())
                {
                    std::string file_name = "\"task enqueue: " + function_definition.get_ast().get_file() + "\"";

                    int file_line = construct_body.get_ast().get_line();

                    std::string mangled_function_name = 
                        "\"" + function_definition.get_function_name().mangle_id_expression() + "\"";

                    instrument_code_task_creation
                        // TODO we want to know if threadswitch was enabled
                        << "const int EVENT_TASK_ENQUEUE = 60000010;"
                        << "int _user_function_event = mintaka_index_get(" << file_name << "," << file_line << ");"
                        << "if (_user_function_event == -1)"
                        << "{"
                        << "     nthf_spin_lock_((nth_word_t*)&_nthf_unspecified_critical);"
                        << "     _user_function_event = mintaka_index_allocate2(" << file_name << "," 
                        <<                file_line << "," << mangled_function_name << ", EVENT_TASK_ENQUEUE);"
                        << "     nthf_spin_unlock_((nth_word_t*)&_nthf_unspecified_critical);"
                        << "}"
                        << "mintaka_event(EVENT_TASK_ENQUEUE, _user_function_event);"
                        << "if (nth != NTH_CANNOT_ALLOCATE_TASK)"
                        << "{"
                        // Adjust to 32 bit
                        << "     uint32_t id_nth = (((intptr_t)(nth)) >> (32*((sizeof(nth)/4) - 1)));"
                        << "     mintaka_send(id_nth, 1);"
                        << "     mintaka_state_run();"
                        << "}"
                        ;

                    define_global_mutex("_nthf_unspecified_critical", function_definition.get_ast(),
                            function_definition.get_scope_link());
                }

                // Parse the code
                AST_t task_code = task_queueing.parse_statement(task_construct.get_ast(),
                        task_construct.get_scope_link());

                // And replace the whole thing
                task_construct.get_ast().replace(task_code);
            }

            void taskwait_postorder(OpenMP::CustomConstruct taskwait_construct)
            {
                Source taskwait_source;
                Statement taskwait_body = taskwait_construct.body();

                Source instrumentation_code_before, instrumentation_code_after;

                if (instrumentation_requested())
                {
                    instrumentation_code_before
                        << "int __previous_state = mintaka_get_state();"
                        << "mintaka_state_synch();"
                        ;

                    instrumentation_code_after
                        << "mintaka_set_state(__previous_state);"
                        ;
                }

                taskwait_source
                    << "{"
                    <<    instrumentation_code_before
                    <<    "nthf_task_block_();"
                    <<    instrumentation_code_after
                    <<    taskwait_body.prettyprint() // This will avoid breakage if you did not write ';' after the taskwait pragma
                    << "}"
                    ;

                AST_t taskwait_code = taskwait_source.parse_statement(taskwait_construct.get_ast(),
                        taskwait_construct.get_scope_link());

                taskwait_construct.get_ast().replace(taskwait_code);
            }

            void taskyield_postorder(OpenMP::CustomConstruct taskyield_construct)
            {
                Source taskyield_source;
                Statement taskyield_body = taskyield_construct.body();

                taskyield_source
                    << "{"
                    <<    "nth_yield();"
                    <<    taskyield_body.prettyprint() // This will avoid breakage if you did not write ';' after the taskyield pragma
                    << "}"
                    ;

                AST_t taskyield_code = taskyield_source.parse_statement(taskyield_construct.get_ast(),
                        taskyield_construct.get_scope_link());

                taskyield_construct.get_ast().replace(taskyield_code);
            }

            void taskgroup_postorder(OpenMP::CustomConstruct taskgroup_construct)
            {
                Source taskgroup_source;
                Statement taskgroup_body = taskgroup_construct.body();

                Source instrumentation_code_before, instrumentation_code_after;

                if (instrumentation_requested())
                {
                    instrumentation_code_before
                        << "int __previous_state = mintaka_get_state();"
                        << "mintaka_state_synch();"
                        ;

                    instrumentation_code_after
                        << "mintaka_set_state(__previous_state);"
                        ;
                }

                taskgroup_source
                    << "{"
                    <<    "nthf_push_taskgroup_scope_();"
                    <<    taskgroup_body.prettyprint()
                    <<    instrumentation_code_before
                    <<    "nthf_task_block_();"
                    <<    instrumentation_code_after
                    <<    "nthf_pop_taskgroup_scope_();"
                    << "}"
                    ;

                AST_t taskgroup_code = taskgroup_source.parse_statement(taskgroup_construct.get_ast(),
                        taskgroup_construct.get_scope_link());

                taskgroup_construct.get_ast().replace(taskgroup_code);
            }


            void flush_postorder(OpenMP::FlushDirective flush_directive)
            {
                Source flush_source;

                flush_source
                    << "{"
//                    <<    "extern void synchronize();"
                    <<    "synchronize();"
                    << "}"
                    ;

                AST_t flush_tree = flush_source.parse_statement(flush_directive.get_ast(),
                        flush_directive.get_scope_link());

                flush_directive.get_ast().replace(flush_tree);
            }


            // This function returns a common spawn code suitable for parallel 'something' constructs
            AST_t get_parallel_spawn_code(
                    AST_t ref_tree, // Reference tree, needed for correct parsing
                    FunctionDefinition function_definition,
                    Scope scope,
                    ScopeLink scope_link,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    OpenMP::Clause num_threads_clause,
                    OpenMP::CustomClause groups_clause,
                    Source& instrument_code_before,
                    Source& instrument_code_after)
            {
                Source spawn_code;
                Source reduction_vectors;
                Source groups_definition;
                Source referenced_parameters;

                Source reduction_code;

                // FIXME. This should be moved out of here like
                // instrument_code_before and instrument_code_after
                Source instrument_code_block;
                Source size_vector;

                Source src_num_args_val;
                Source src_num_args_ref;

                Source nth_creation_function;
                Source additional_declarations;

                Source outlined_function_name_decl;

                // Calculate the proper expression referring this function
                outlined_function_name_decl << get_outline_function_reference(function_definition, parameter_info_list);

                // The skeleton of the spawn code will be this one
                spawn_code
                    << "{"
                    << "  int nth_nprocs;"
                    << "  nth_desc *nth_selfv;"
                    << "  int nth_num_deps;"
                    << "  int nth_p;"
                    <<    reduction_vectors
                    <<    instrument_code_before
                    <<    groups_definition
                    <<    size_vector 
                    << "  int nth_nargs_ref = " << src_num_args_ref << ";"
                    <<    additional_declarations
                    << "  nth_selfv = nthf_self_();"
                    << "  nthf_team_set_nplayers_ (&nth_nprocs);"
                    << "  nth_num_deps = 0;"
                    << "  for (nth_p = 0; nth_p < nth_nprocs; nth_p++)"
                    << "  {"
                    <<       nth_creation_function
                    << "  }"
                    <<    instrument_code_block // This is crummy here
                    << "  nthf_block_();"
                    <<    reduction_code
                    <<    instrument_code_after
                    << "}"
                    ;


                // I don't like this
                if (instrumentation_requested())
                {
                    instrument_code_block 
                        << "mintaka_state_synch();"
                        ;
                }

                // For every entity in the reduction_references list
                ObjectList<OpenMP::ReductionIdExpression> merged_reduction_references;

                merged_reduction_references.append(reduction_references);
                merged_reduction_references.append(inner_reductions_stack.top());

                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = merged_reduction_references.begin();
                        it != merged_reduction_references.end();
                        it++)
                {
                    // create a reduction vector after the name of the mangled entity
                    std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                    // get its type
                    Symbol reduction_symbol = it->get_symbol();
                    Type reduction_type = reduction_symbol.get_type();

                    // create a tree of expression 128
                    // FIXME: hardcoded to 128 processors
                    Source array_length;
                    array_length << "128";
                    AST_t array_length_tree = array_length.parse_expression(it->get_id_expression().get_ast(), 
                            it->get_id_expression().get_scope_link());

                    // and get an array of 128 elements
                    Type reduction_vector_type = reduction_type.get_array_to(array_length_tree, 
                            it->get_id_expression().get_scope());

                    // now get the code that declares this reduction vector
                    reduction_vectors
                        << comment("Reduction vector for '" + it->get_id_expression().prettyprint() + "'")
                        << reduction_vector_type.get_declaration(it->get_id_expression().get_scope(), 
                                reduction_vector_name) << ";";
                }
                
                // Referenced parameters
                //
                // "this" might be needed
                int num_args_ref = 0;
                if (is_nonstatic_member_function(function_definition))
                {
                    referenced_parameters << ", this";
                    num_args_ref++;
                }
                // First the pointer ones
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_POINTER)
                        continue;

                    // Simply pass its reference (its address)
                    referenced_parameters << ", " << it->argument_name;

                    num_args_ref++;
                }
                src_num_args_ref << num_args_ref;

                // Now the value ones. We fill the nth_sizes vector here
                size_vector << "size_t nth_sizes[] = {0";
                int num_args_val = 0;
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_VALUE)
                        continue;

                    IdExpression id_expr = it->id_expression;

                    // Size and reference
                    size_vector << ", sizeof(" << id_expr.prettyprint() << ")";

                    referenced_parameters << ", &nth_sizes[" << (num_args_val + 1) << "], &" << id_expr.prettyprint();

                    num_args_val++;
                }
                size_vector << "};";


                if ((num_args_val == 0)
                        && (ExternalVars::get("nanos_new_interface", "0") != "1"))
                {
                    nth_creation_function
                        << "     nthf_create_1s_vp_((void*)(" << outlined_function_name_decl << "), &nth_num_deps, &nth_p, &nth_selfv, 0, "
                        << "        &nth_nargs_ref " << referenced_parameters << ");"
                        ;
                }
                else
                {
                    if (ExternalVars::get("nanos_new_interface", "0") == "0")
                    {
                        // FIXME. We are giving an approximate locus but this
                        // should not be very important since in some near 
                        // future we will remove the old interface :)
                        std::cerr << "Warning, OpenMP construct in function '" 
                            << function_definition.get_function_name().prettyprint() 
                            << "' at " 
                            << function_definition.get_ast().get_locus() 
                            << " requires using the new interface since something must be passed by value."
                            << std::endl;
                    }

                    additional_declarations
                        << "  int nth_task_type = 0x4;"
                        << "  int nth_nargs_val = " << src_num_args_val << ";"
                        << "  void *nth_arg_addr[" << src_num_args_val << " + 1];"
                        << "  void **nth_arg_addr_ptr = nth_arg_addr;"
                        ;
                    nth_creation_function 
                        << "     nth_create((void*)(" << outlined_function_name_decl << "), "
                        << "            &nth_task_type, &nth_num_deps, &nth_p, &nth_selfv, "
                        << "            &nth_arg_addr_ptr, &nth_nargs_ref, &nth_nargs_val" << referenced_parameters << ");"
                        ;
                }
                
                if (num_args_val == 0)
                {
                    // Disable the declaration of the size vector to avoid a warning
                    size_vector = TL::Source("");
                }

                src_num_args_val << num_args_val;

                // Groups definition
                if (!groups_clause.is_defined() && !num_threads_clause.is_defined())
                {
                    groups_definition 
                        << "nth_nprocs =  nthf_cpus_actual_();"
                        ;
                }
                else if (num_threads_clause.is_defined())
                {
                    ObjectList<Expression> clause_exprs = num_threads_clause.get_expression_list();

                    std::string num_threads_value = clause_exprs[0].prettyprint();
                    groups_definition 
                        << "nth_nprocs =" << num_threads_value << ";"
                        ;
                }
                else /* groups is defined */
                {
                    groups_definition << "int nth_groups_num;"
                        ;

                    ObjectList<Expression> groups_expressions = groups_clause.get_expression_list();

                    switch (groups_expressions.size())
                    {
                        case 1 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();

                                groups_definition 
                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_compute_uniform_groups_(&nthf_groups_num);"
                                    ;
                                break;
                            }
                        case 2 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();
                                std::string howmany_groups = groups_expressions[1].prettyprint();

                                groups_definition
                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_compute_groups_vec_(&nthf_groups_num, " << howmany_groups << ");"
                                    ;
                        
                                break;
                            }
                        case 3 :
                            {
                                std::string num_groups = groups_expressions[0].prettyprint();
                                std::string who_groups = groups_expressions[1].prettyprint();
                                std::string howmany_groups = groups_expressions[2].prettyprint();

                                groups_definition
                                    << "nth_groups_num = " << num_groups << ";"
                                    << "nthf_define_groups_(&nthf_groups_num, " << who_groups << ", " << howmany_groups << ");"
                                    ;

                                break;
                            }
                        default:
                            break;
                    }

                    groups_definition
                        << "nth_nprocs = nth_groups_num;"
                        ;
                }
                
                // Reduction code
                //
                // If there is any reduction reference
                reduction_code = get_noncritical_reduction_code(reduction_references);
                
                // std::cerr << "SPAWN CODE" << std::endl;
                // std::cerr << spawn_code.get_source(true) << std::endl;
                // std::cerr << "End SPAWN CODE" << std::endl;
                
                // Parse the spawn code and return it
                AST_t result = spawn_code.parse_statement(ref_tree, scope_link);
                return result;
            }
            
            // This function computes a proper reference to the function
            std::string get_outline_function_reference(FunctionDefinition function_definition,
                    ObjectList<ParameterInfo>& parameter_info_list)
            {
                IdExpression function_name = function_definition.get_function_name();
                Symbol function_symbol = function_name.get_symbol();

                Source outlined_function_name_decl;

                bool additional_parentheses = false;
                
                // We have to ensure that this qualification refers to the proper function
                // in C++ this is achieved via a casting. A cast of an overload function name
                // does not obey unconditionally the programmer but selects the proper overloaded
                // function (if any, otherwise the program is ill-formed)
                if (function_symbol.is_template_function())
                {
                    Source overload_selector_cast;

                    overload_selector_cast << "(";
                    additional_parentheses = true;
                    overload_selector_cast << "(void (*) (";

                    bool first = true;
                    if (is_nonstatic_member_function(function_definition))
                    {
                        // Do not forget the "this" type
                        Statement function_body = function_definition.get_function_body();
                        Scope function_body_scope = function_body.get_scope();

                        Symbol this_symbol = function_body_scope.get_symbol_from_name("this");

                        // decl_scope.printscope();
                        Type class_type = this_symbol.get_type();

                        overload_selector_cast << class_type.get_declaration(function_body_scope, "");

                        // There is already a first parameter
                        first = false;
                    }

                    for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                            it != parameter_info_list.end();
                            it++)
                    {
                        if (!first)
                        {
                            overload_selector_cast << ", ";
                        }
                        else
                        {
                            first = false;
                        }

                        overload_selector_cast << it->type.get_declaration(function_definition.get_scope(), "");
                    }

                    overload_selector_cast << "))";

                    outlined_function_name_decl << overload_selector_cast;
                }


                if (function_symbol.is_template_function())
                {
                    ObjectList<AST_t> template_headers = function_definition.get_template_header();
                    // std::cerr << "(2) Num templates " << template_headers.size() << std::endl;

                    Source outlined_function_name = get_outlined_function_name(function_name, /*qualif=*/true, 
                            /*template=*/ !template_headers.empty());
                    outlined_function_name_decl << outlined_function_name;

                    if (!template_headers.empty())
                    {
                        outlined_function_name_decl << "<";
                        AST_t last_template_header = *(template_headers.rbegin());

                        PredicateBool<LANG_IS_TEMPLATE_PARAMETER> template_parameter_pred;
                        ObjectList<AST_t> template_parameters = last_template_header.depth_subtrees(template_parameter_pred);

                        for (ObjectList<AST_t>::iterator it = template_parameters.begin();
                                it != template_parameters.end();
                                it++)
                        {
                            if (it != template_parameters.begin())
                            {
                                outlined_function_name_decl << ", ";
                            }

                            outlined_function_name_decl << it->prettyprint();
                        }

                        outlined_function_name_decl << ">";
                    }
                }
                else
                {
                    Source outlined_function_name = get_outlined_function_name(function_name);
                    outlined_function_name_decl << outlined_function_name;
                }

                if (additional_parentheses)
                {
                    outlined_function_name_decl << ")";
                }

                return outlined_function_name_decl.get_source();
            }

            Source get_critical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_code;

                if (reduction_references.empty())
                {
                    // Nothing to do if the reduction set is empty
                    return reduction_code;
                }

                Source reduction_gathering;

                reduction_code
                    << comment("Reduction implemented with a spin lock since this construct is orphaned")
                    << "{"
                    <<    "static nth_word_t default_mutex;"
//                    <<    "extern nthf_spin_lock_(void*);"
//                    <<    "extern nthf_spin_unlock_(void*);"

                    <<    "nthf_spin_lock_(&default_mutex);"
                    <<    reduction_gathering
                    <<    "nthf_spin_unlock_(&default_mutex);"
                    << "}"
                    ; 

                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    if (!it->is_user_defined())
                    {
                    // get the operator involved
                    std::string reduced_var_name = it->get_id_expression().mangle_id_expression();
                    std::string reduction_var_name = "rdp_" + it->get_id_expression().mangle_id_expression();

                    std::string op = it->get_operation().prettyprint();

                    reduction_gathering 
                        << reduced_var_name << " = " << reduced_var_name << op << reduction_var_name << ";"
                        ;
                    }
                    else
                    {
                        Source one_urd_reduction = get_one_user_defined_gathering(*it);
                        reduction_gathering << one_urd_reduction;
                    }
                }

                return reduction_code;
            }


            Source get_noncritical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_code;
                
                if (reduction_references.empty())
                {
                    return reduction_code;
                }

                // Create the source code that gathers the values computed by every thread
                Source reduction_gathering;

                reduction_code
                    << comment("Reduction code noncritical performed after the join")
                    << "int rdv_i;"
                    << "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                    << "{"
                    <<    reduction_gathering
                    << "}"
                    ;

                Source reduction_gethering;

                reduction_gathering = get_reduction_gathering(reduction_references);

                return reduction_code;
            }

            Source get_noncritical_inlined_reduction_code(
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_code;

                if (reduction_references.empty())
                {
                    return reduction_code;
                }

                Source reduction_update;
                Source reduction_gathering;

                reduction_code
                    << comment("Inlined reduction code since this construct is not orphaned")
                    << reduction_update
//                    << "extern void in__tone_barrier_();"
//                    << "extern char in__tone_is_master_();"

                    << "in__tone_barrier_();"
                    << "if (in__tone_is_master_())"
                    << "{"
                    <<    "int rdv_i;"
//                    <<    "extern int nthf_cpus_actual_();"

                    <<    "int nth_nprocs = nthf_cpus_actual_();"
                    <<    "for (rdv_i = 0; rdv_i < nth_nprocs; rdv_i++)"
                    <<    "{"
                    <<       reduction_gathering
                    <<    "}"
                    << "}"
                    ;

                reduction_update = get_reduction_update(reduction_references);
                reduction_gathering = get_reduction_gathering(reduction_references);

                // We push them onto the stack of inner_reductions because this
                // functions is only called when this for is not orphaned
                ObjectList<OpenMP::ReductionIdExpression>& inner_reductions = inner_reductions_stack.top();
                inner_reductions.insert(reduction_references, functor(&OpenMP::ReductionIdExpression::get_symbol));

                return reduction_code;
            }

            Source get_reduction_update(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_update;
                
                // Discard those that came from inner constructions
                reduction_references = reduction_references.filter(
                        not_in_set(inner_reductions_stack.top(), functor(&OpenMP::ReductionIdExpression::get_symbol)));

                if (reduction_references.empty())
                {
                    return reduction_update;
                }

                reduction_update 
                    << "{"
                    //                        <<    "extern int in__tone_thread_id_ ();"
                    <<    "int nth_thread_id = in__tone_thread_id_();"
                    ;

                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    reduction_update
                        << "rdv_" << it->get_id_expression().mangle_id_expression() << "[nth_thread_id] = "
                        << "rdp_" << it->get_id_expression().mangle_id_expression() << ";";
                }

                reduction_update
                    << "}"
                    ;


                return reduction_update;
            }

            Source get_reduction_gathering(ObjectList<OpenMP::ReductionIdExpression> reduction_references)
            {
                Source reduction_gathering;

                // For every entity being reduced
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    // And reduce for this element of the reduction vector
                    if (!it->is_user_defined())
                    {
                        // If it is not a user defined one it is easy

                        // Construct the name of its related reduction vector
                        std::string reduced_var_name = it->get_id_expression().prettyprint();
                        std::string reduction_vector_name = "rdv_" + it->get_id_expression().mangle_id_expression();

                        // get the operator involved
                        std::string op = it->get_operation().prettyprint();
                        reduction_gathering
                            << reduced_var_name << " = " << reduced_var_name << op << reduction_vector_name << "[rdv_i]" << ";";
                    }
                    else
                    {
                        Source one_urd_reduction = get_one_user_defined_gathering(*it);
                        reduction_gathering << one_urd_reduction;
                    }
                }

                return reduction_gathering;
            }

            Source get_one_user_defined_gathering(OpenMP::ReductionIdExpression reduction_id_expr)
            {
                IdExpression reductor = reduction_id_expr.get_user_defined_reductor();
                Symbol reductor_symbol = reductor.get_symbol();

                Type reductor_type = reductor_symbol.get_type();

                if (!reductor_type.is_function())
                {
                    std::cerr << "User defined reduction in " 
                        << reductor.get_ast().get_locus() << " does not refer a function. Ignoring" << std::endl;
                    return Source("");
                }
                
                // Construct the name of its related reduction vector
                std::string reduced_var_name = reduction_id_expr.get_id_expression().prettyprint();
                std::string reduction_vector_name = "rdv_" + reduction_id_expr.get_id_expression().mangle_id_expression();

                Source reduction_gathering;

                // FIXME - For C++ this is more difficult. Currently not implemented
                // Extract the unqualified part of the id-expression
                // and if it is a member construct a member-access with function call
                //
                // The id-expression
                //
                // Lets "happily" assume that if the reductor returns void is of the form
                //
                //    void f(T*, T);
                //    void f(T&, T);
                //
                // otherwise we will assume it is of type 
                //
                //    T f(T, T);
                //
                if (reductor_type.returns().is_void())
                {
                    // If the first parameter is a pointer we will assume that the reductor is of this form
                    //
                    //    void f(T*, t);
                    //
                    // otherwise it will be assumed to be
                    //
                    //    void f(T&, t);
                    //
                    ObjectList<Type> parameters = reductor_type.parameters();

                    if (parameters[0].is_pointer())
                    {
                        reduction_gathering
                            << reductor.prettyprint() << "(&" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
                    }
                    else
                    {
                        reduction_gathering
                            << reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
                    }
                }
                else
                {
                    reduction_gathering
                        << reduced_var_name << " = " << reductor.prettyprint() << "(" << reduced_var_name << "," << reduction_vector_name << "[rdv_i]" << ");";
                }

                return reduction_gathering;
            }

            Source get_member_function_declaration(
                    FunctionDefinition function_definition,
                    Declaration function_declaration,
                    Source outlined_function_name,
                    ObjectList<ParameterInfo> parameter_info_list
                    )
            {
                Source result;
                Source formal_parameters;
                Source template_header;
                Scope decl_scope = function_declaration.get_scope();

                result
                    << template_header
                    << "static void " << outlined_function_name << "(" << formal_parameters << ");"
                    ;

                if (function_declaration.is_templated())
                {
                    ObjectList<AST_t> template_headers = function_declaration.get_template_header();
                    // std::cerr << "(3) Num templates " << template_headers.size() << std::endl;
                    for (ObjectList<AST_t>::iterator it = template_headers.begin();
                            it != template_headers.end();
                            it++)
                    {
                        template_header << "template <" << it->prettyprint(/*comma=*/true) << ">";
                    }
                }

                formal_parameters = get_formal_parameters(function_definition, 
                        parameter_info_list,
                        decl_scope);

                return result;
            }

            Source get_outline_common(
                    FunctionDefinition function_definition,
                    Source& specific_body,
                    Source outlined_function_name,
                    ObjectList<ParameterInfo> parameter_info_list
                    )
            {
                Source formal_parameters;
                Source reduction_code;

                Source static_qualifier;

                Source forward_declaration;

                Source template_header;

                IdExpression function_name = function_definition.get_function_name();

                Source result;
                result
                    << forward_declaration
                    << template_header
                    << static_qualifier
                    << "void " << outlined_function_name << "(" << formal_parameters << ")"
                    << "{"
                    <<    specific_body
                    << "}"
                    ;

                Symbol function_symbol = function_name.get_symbol();

                if (function_definition.is_templated())
                {
                    ObjectList<AST_t> template_headers = function_definition.get_template_header();
                    // std::cerr << "(1) Num templates " << template_headers.size() << std::endl;
                    for (ObjectList<AST_t>::iterator it = template_headers.begin();
                            it != template_headers.end();
                            it++)
                    {
                        template_header << "template <" << it->prettyprint(/*comma=*/true) << ">";
                    }
                }

                // If the function is a member and is not qualified we need an additional
                // static here
                if (function_symbol.is_member() 
                        && !function_name.is_qualified())
                {
                    static_qualifier << "static ";
                }

                formal_parameters = get_formal_parameters(
                        function_definition, 
                        parameter_info_list,
                        function_definition.get_function_body().get_scope());

                if (!function_symbol.is_member())
                {
                    // We want to forward the declaration
                    Declaration point_of_decl = function_name.get_declaration();
                    DeclarationSpec decl_specs = point_of_decl.get_declaration_specifiers();
                    ObjectList<DeclaredEntity> declared_entities = point_of_decl.get_declared_entities();
                    DeclaredEntity declared_entity = *(declared_entities.begin());

                    forward_declaration 
                        << template_header
                        << decl_specs.prettyprint()
                        << " "
                        << declared_entity.prettyprint()
                        << ";";
                }

                return result;
            }

            Source get_formal_parameters(
                    FunctionDefinition function_definition,
                    ObjectList<ParameterInfo> parameter_info_list,
                    Scope decl_scope)
            {
                int num_params = 0;
                Source formal_parameters;

                // Add _this if needed
                if (is_nonstatic_member_function(function_definition))
                {
                    IdExpression function_name = function_definition.get_function_name();

                    Statement function_body = function_definition.get_function_body();
                    Scope function_body_scope = function_body.get_scope();
                    Symbol this_symbol = function_body_scope.get_symbol_from_name("this");

                    // decl_scope.printscope();

                    Type class_type = this_symbol.get_type();
                    formal_parameters.append_with_separator(
                            // Fix this scope
                            class_type.get_declaration(decl_scope, "_this"), 
                            ",");
                    num_params++;
                }

                // First the pointer ones
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_POINTER)
                        continue;

                    IdExpression id_expr = it->id_expression;
                    Type type = it->type;
                    std::string name = it->parameter_name;

                    formal_parameters.append_with_separator(
                            type.get_declaration(decl_scope, name), ",");
                    num_params++;
                }

                // Now the value ones
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    if (it->kind != ParameterInfo::BY_VALUE)
                        continue;

                    IdExpression id_expr = it->id_expression;
                    Type type = it->type;
                    std::string name = it->parameter_name;

                    formal_parameters.append_with_separator(
                            type.get_declaration(decl_scope, name), ",");
                    num_params++;
                }

                if (num_params == 0)
                {
                    formal_parameters << "void";
                }

                return formal_parameters;
            }

            AST_t get_outline_parallel(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<IdExpression> private_references,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    ObjectList<IdExpression> copyin_references,
                    ObjectList<IdExpression> copyprivate_references,
                    bool never_instrument = false
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
                        parameter_info_list);
                
                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                Source private_declarations = get_privatized_declarations(
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        parameter_info_list
                        ); 

                Source reduction_update = get_reduction_update(reduction_references);

                Source instrumentation_code_before;
                Source instrumentation_code_after;

                if (!never_instrument)
                {
                    instrumentation_outline(instrumentation_code_before,
                            instrumentation_code_after, 
                            function_definition,
                            construct_body);
                }

                // Debug information
                Source comment = debug_parameter_info(
                        parameter_info_list);

                Source task_block_code;
                
                parallel_body 
                    << comment
                    << private_declarations
                    << instrumentation_code_before
                    << modified_parallel_body_stmt.prettyprint()
                    << reduction_update
                    << instrumentation_code_after
                    << task_block_code
                    ;

                task_block_code = get_task_block_code();

                // std::cerr << "OUTLINE CODE" << std::endl;
                // std::cerr << outline_parallel.get_source(true) << std::endl;
                // std::cerr << "End OUTLINE CODE" << std::endl;

                IdExpression function_name = function_definition.get_function_name();
                Symbol function_symbol = function_name.get_symbol();

                // If the function is a member and is qualified (therefore the
                // function definition is outside the class) we have to create
                // an additional declaration for the new member
                if (function_symbol.is_member() 
                        && function_name.is_qualified())
                {
                    Source outline_function_decl = get_outlined_function_name(function_name, /*qualified=*/false);

                    Declaration decl = function_name.get_declaration();
                    Scope class_scope = decl.get_scope();
                    Type class_type = function_symbol.get_class_type();

                    Source member_declaration = get_member_function_declaration(
                            function_definition,
                            decl,
                            outline_function_decl,
                            parameter_info_list);

                    AST_t member_decl_tree = member_declaration.parse_member(decl.get_ast(), 
                            decl.get_scope_link(), class_type);

                    decl.get_ast().append(member_decl_tree);
                }

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_ast(), 
                         function_definition.get_scope_link());

                return result;
            }

            AST_t get_outline_task(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<IdExpression> local_references
                    )
            {
                ObjectList<OpenMP::ReductionIdExpression> reduction_references;

                Source outline_parallel;
                Source parallel_body;

                outline_parallel = get_outline_common(
                        function_definition,
                        parallel_body, // The body of the outline
                        outlined_function_name,
                        parameter_info_list);

                // Replace references using set "replace_references" over construct body
                Statement modified_parallel_body_stmt = replace_references.replace(construct_body);

                ObjectList<IdExpression> empty;
                ObjectList<OpenMP::ReductionIdExpression> reduction_empty;
                Source private_declarations = get_privatized_declarations(
                        local_references,
                        empty,
                        empty,
                        reduction_empty,
                        empty,
                        parameter_info_list
                        ); 

                Source instrumentation_code_before, instrumentation_code_after;

                instrumentation_outline(instrumentation_code_before,
                        instrumentation_code_after, 
                        function_definition,
                        construct_body);

                Source instrumentation_start_task;
                if (instrumentation_requested())
                {
                    instrumentation_start_task
                        << "{"
                        << "   nth_desc * nth;"
                        << "   nth = nthf_self_();"
                        << "   uint32_t id_nth = (((intptr_t)(nth)) >> (32*((sizeof(nth)/4) - 1)));"
                        << "   mintaka_receive(id_nth, 1);"
                        << "   mintaka_state_run();"
                        << "}"
                        ;
                }

                parallel_body 
                    << private_declarations
                    << instrumentation_code_before
                    << instrumentation_start_task
                    << modified_parallel_body_stmt.prettyprint()
                    << instrumentation_code_after
                    ;

                IdExpression function_name = function_definition.get_function_name();
                Symbol function_symbol = function_name.get_symbol();

                if (function_symbol.is_member() 
                        && function_name.is_qualified())
                {
                    Source outline_function_decl = get_outlined_function_name(function_name, /*qualified=*/false);

                    Declaration decl = function_name.get_declaration();
                    Scope class_scope = decl.get_scope();
                    Type class_type = function_symbol.get_class_type();

                    Source member_declaration = get_member_function_declaration(
                            function_definition,
                            decl,
                            outline_function_decl,
                            parameter_info_list);

                    AST_t member_decl_tree = member_declaration.parse_member(decl.get_ast(), decl.get_scope_link(), class_type);

                    decl.get_ast().append(member_decl_tree);
                }

                AST_t result;

                result = outline_parallel.parse_global(function_definition.get_ast(), 
                        function_definition.get_scope_link());

                return result;
            }

            // Create outline for parallel sections
            AST_t get_outline_parallel_sections(
                    FunctionDefinition function_definition,
                    Source outlined_function_name, 
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<IdExpression> private_references,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    ObjectList<IdExpression> copyin_references,
                    ObjectList<IdExpression> copyprivate_references)
            {
                ObjectList<IdExpression> pass_by_value;

                Source outline_parallel_sections;
                Source parallel_sections_body;
                
                // Get the source of the common parallel X outline
                outline_parallel_sections = get_outline_common(
                        function_definition,
                        parallel_sections_body,
                        outlined_function_name,
                        parameter_info_list);

                Source private_declarations = get_privatized_declarations(
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

                Source instrumentation_code_before, instrumentation_code_after;
                instrumentation_outline(instrumentation_code_before,
                        instrumentation_code_after, 
                        function_definition,
                        construct_body);

                Source task_block_code;

                parallel_sections_body 
                    << private_declarations
                    << instrumentation_code_before
                    << loop_distribution
                    << lastprivate_code
                    << reduction_update
                    << loop_finalization
                    << instrumentation_code_after
                    << task_block_code
                    ;

                task_block_code = get_task_block_code();

                // std::cerr << "OUTLINE PARALLEL SECTIONS CODE" << std::endl;
                // std::cerr << outline_parallel_sections.get_source(true) << std::endl;
                // std::cerr << "End OUTLINE PARALLEL SECTIONS CODE" << std::endl;

                AST_t result;

                result = outline_parallel_sections.parse_global(function_definition.get_ast(), 
                        function_definition.get_scope_link());

                return result;
            }

            AST_t get_outline_parallel_single(
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

            Source get_task_block_code()
            {
                Source task_block_code;
                Source instrumentation_task_block_before, instrumentation_task_block_after;

                if (instrumentation_requested())
                {
                    instrumentation_task_block_before
                        << "{"
                        << "   int __previous_state = mintaka_get_state();"
                        << "   mintaka_state_synch();"
                        ;

                    instrumentation_task_block_after
                        << "   mintaka_set_state(__previous_state);"
                        << "}"
                        ;
                }

                task_block_code
                    << instrumentation_task_block_before
                    << "nthf_task_block_();"
                    << instrumentation_task_block_after
                    ;

                return task_block_code;
            }


            // Create outline for parallel for
            AST_t get_outline_parallel_for(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    ForStatement for_statement,
                    Statement loop_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<IdExpression> private_references,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    ObjectList<IdExpression> copyin_references,
                    ObjectList<IdExpression> copyprivate_references,
                    OpenMP::Directive directive
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
                        parameter_info_list);

                Source private_declarations = get_privatized_declarations(
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        parameter_info_list
                        ); 

                Source loop_distribution = get_loop_distribution_code(for_statement, 
                        replace_references, function_definition, directive);

                Source lastprivate_code;

                if (!lastprivate_references.empty())
                {
                    Source lastprivate_assignments = get_lastprivate_assignments(
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


                Source task_block_code;

                parallel_for_body 
                    << private_declarations
                    << loop_distribution
                    << lastprivate_code
                    << reduction_update
                    << loop_finalization
                    << task_block_code
                    ;

                task_block_code = get_task_block_code();

                // std::cerr << "OUTLINE PARALLEL FOR CODE" << std::endl;
                // std::cerr << outline_parallel_for.get_source(true) << std::endl;
                // std::cerr << "End OUTLINE PARALLEL FOR CODE" << std::endl;

                AST_t result;

                result = outline_parallel_for.parse_global(function_definition.get_ast(), 
                        function_definition.get_scope_link());

                return result;
            }

            void get_data_explicit_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references,
                    ObjectList<IdExpression>& copyin_references,
                    ObjectList<IdExpression>& copyprivate_references)
            {
                // Get references in shared clause
                OpenMP::Clause shared_clause = directive.shared_clause();
                shared_references = shared_clause.id_expressions();

                // Get references in private_clause
                OpenMP::Clause private_clause = directive.private_clause();
                private_references = private_clause.id_expressions();

                // Get references in firstprivate clause
                OpenMP::Clause firstprivate_clause = directive.firstprivate_clause();
                firstprivate_references = firstprivate_clause.id_expressions();

                // Get references in lastprivate clause
                OpenMP::Clause lastprivate_clause = directive.lastprivate_clause();
                lastprivate_references = lastprivate_clause.id_expressions();

                // Get references in reduction clause
                OpenMP::ReductionClause reduction_clause = directive.reduction_clause();
                reduction_references = reduction_clause.id_expressions();

                // Get references in copyin
                OpenMP::Clause copyin_clause = directive.copyin_clause();
                copyin_references = copyin_clause.id_expressions();

                // Get references in copyprivate
                OpenMP::Clause copyprivate_clause = directive.copyprivate_clause();
                copyprivate_references = copyprivate_clause.id_expressions();
            }


            void get_data_attributes(
                    Scope function_scope,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references,
                    ObjectList<IdExpression>& copyin_references,
                    ObjectList<IdExpression>& copyprivate_references)
            {
                get_data_explicit_attributes(
                        function_scope,
                        directive,
                        construct_body,
                        shared_references,
                        private_references,
                        firstprivate_references,
                        lastprivate_references,
                        reduction_references,
                        copyin_references,
                        copyprivate_references);

                enum
                {
                    PK_DATA_INVALID = 0,
                    PK_DATA_SHARED, 
                    PK_DATA_PRIVATE,
                    PK_DATA_NONE,
                } default_data_sharing = PK_DATA_INVALID;

                OpenMP::DefaultClause default_clause = directive.default_clause();

                if (!default_clause.is_defined())
                {
                    // By default it is shared
                    default_data_sharing = PK_DATA_SHARED;
                }
                else if (default_clause.is_none())
                {
                    default_data_sharing = PK_DATA_NONE;
                }
                else if (default_clause.is_shared())
                {
                    default_data_sharing = PK_DATA_SHARED;
                }
                // An extension that we consider sensible
                else if (default_clause.is_custom("private"))
                {
                    default_data_sharing = PK_DATA_PRIVATE;
                }
                else
                {
                    std::cerr << "Warning: Unknown default clause '" 
                        << default_clause.prettyprint() << "' at " << default_clause.get_ast().get_locus() << ". "
                        << "Assuming 'default(shared)'."
                        << std::endl;
                    default_data_sharing = PK_DATA_SHARED;
                }

                // Get every non local reference: this is, not defined in the
                // construct itself, but visible at the point where the
                // construct is defined
                ObjectList<IdExpression> non_local_references = construct_body.non_local_symbol_occurrences(Statement::ONLY_VARIABLES);
                ObjectList<Symbol> non_local_symbols = non_local_references.map(functor(&IdExpression::get_symbol));
                
                // Filter shareds, privates, firstprivate, lastprivate or
                // reduction that are useless
                ObjectList<IdExpression> unreferenced;
                // Add to unreferenced symbols that appear in shared_references but not in non_local_references
                unreferenced.append(shared_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // shared_references now only contains references that appear in non_local_references
                shared_references = shared_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in private_references but not in non_local_references
                unreferenced.append(private_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // private_references now only contains references that appear in non_local_references
                private_references = private_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in lastprivate_references but not in non_local_references
                unreferenced.append(firstprivate_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // firstprivate_references now only contains references that appear in non_local_references
                firstprivate_references = firstprivate_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in lastprivate_references but not in non_local_references
                unreferenced.append(lastprivate_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // lastprivate_references now only contains references that appear in non_local_references
                lastprivate_references = lastprivate_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in copyin_references but not in non_local_references
                unreferenced.append(copyin_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // copyin_references now only contains references that appear in non_local_references
                copyin_references = copyin_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in copyprivate_references but not in non_local_references
                unreferenced.append(copyprivate_references.filter(not_in_set(non_local_references, functor(&IdExpression::get_symbol))));
                // copyprivate_references now only contains references that appear in non_local_references
                copyprivate_references = copyprivate_references.filter(in_set(non_local_references, functor(&IdExpression::get_symbol)));

                // Add to unreferenced symbols that appear in reduction_references but not in non_local_references
                unreferenced.append(
                        reduction_references.filter(not_in_set(non_local_symbols, 
                                functor(&OpenMP::ReductionIdExpression::get_symbol)))
                        .map(functor(&OpenMP::ReductionIdExpression::get_id_expression))
                        );
                // reduction_references now only contains references that appear in non_local_references
                reduction_references = reduction_references.filter(in_set(non_local_symbols, 
                            functor(&OpenMP::ReductionIdExpression::get_symbol)));

                // Will give a warning for every unreferenced element
                unreferenced.map(functor(&OpenMPTransform::warn_unreferenced_data, *this));

                // If a symbol appears into shared_references, private_references, firstprivate_references, lastprivate_references
                // or copyin_references, copyprivate_references, remove it from non_local_references
                non_local_references = non_local_references.filter(not_in_set(shared_references, functor(&IdExpression::get_symbol)));
                non_local_references = non_local_references.filter(not_in_set(private_references, functor(&IdExpression::get_symbol)));
                non_local_references = non_local_references.filter(not_in_set(firstprivate_references, functor(&IdExpression::get_symbol)));
                non_local_references = non_local_references.filter(not_in_set(lastprivate_references, functor(&IdExpression::get_symbol)));
                non_local_references = non_local_references.filter(not_in_set(copyin_references, functor(&IdExpression::get_symbol)));
                non_local_references = non_local_references.filter(not_in_set(copyprivate_references, functor(&IdExpression::get_symbol)));
                
                // Get every id-expression related to the ReductionIdExpression list
                ObjectList<IdExpression> reduction_id_expressions = 
                    reduction_references.map(functor(&OpenMP::ReductionIdExpression::get_id_expression));
                // and remove it from non_local_references
                non_local_references = non_local_references.filter(not_in_set(reduction_id_expressions, functor(&IdExpression::get_symbol)));

                switch ((int)default_data_sharing)
                {
                    case PK_DATA_NONE :
                        {
                            non_local_references.map(functor(&OpenMPTransform::warn_no_data_sharing, *this));
                            /* Fall through shared */
                        }
                    case PK_DATA_SHARED :
                        {
                            shared_references.insert(non_local_references, functor(&IdExpression::get_symbol));
                            break;
                        }
                    case PK_DATA_PRIVATE :
                        {
                            private_references.insert(non_local_references, functor(&IdExpression::get_symbol));
                            break;
                        }
                    case PK_DATA_INVALID :
                    default:
                        {
                            break;
                        }
                }
            }

            ReplaceIdExpression set_replacements(FunctionDefinition function_definition,
                    OpenMP::Directive directive,
                    Statement construct_body,
                    ObjectList<IdExpression>& shared_references,
                    ObjectList<IdExpression>& private_references,
                    ObjectList<IdExpression>& firstprivate_references,
                    ObjectList<IdExpression>& lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression>& reduction_references,
                    ObjectList<OpenMP::ReductionIdExpression>& inner_reduction_references,
                    ObjectList<IdExpression>& copyin_references,
                    ObjectList<IdExpression>& copyprivate_references,
                    ObjectList<ParameterInfo>& parameter_info,
                    bool share_always = false)
            {
                Symbol function_symbol = function_definition.get_function_name().get_symbol();
                Scope function_scope = function_definition.get_scope();
                ReplaceIdExpression result;

                // SHARED references
                for (ObjectList<IdExpression>::iterator it = shared_references.begin();
                        it != shared_references.end();
                        it++)
                {
                    // We ignore unqualified/qualified references that are function accessible
                    // or unqualified references that are data members of the same class
                    // of this function because they can be accessed magically
                    if (!share_always 
                            && is_function_accessible(*it, function_definition)
                            && !is_unqualified_member_symbol(*it, function_definition))
                        continue;

                    Symbol symbol = it->get_symbol();
                    if (!is_unqualified_member_symbol(*it, function_definition))
                    {
                        Type type = symbol.get_type();
                        Type pointer_type = type.get_pointer_to();

                        ParameterInfo parameter(it->mangle_id_expression(), 
                                "&" + it->prettyprint(), *it, pointer_type, ParameterInfo::BY_POINTER);
                        parameter_info.append(parameter);
                        result.add_replacement(symbol, "(*" + it->mangle_id_expression() + ")", 
                                it->get_ast(), it->get_scope_link());
                    }
                    else
                    {
                        // Only if this function is a nonstatic one we need _this access
                        if (is_nonstatic_member_function(function_definition))
                        {
                            result.add_replacement(symbol, "_this->" + it->prettyprint(), 
                                    it->get_ast(), it->get_scope_link());
                        }
                    }
                }
                
                // PRIVATE references
                for (ObjectList<IdExpression>::iterator it = private_references.begin();
                        it != private_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    result.add_replacement(symbol, "p_" + it->mangle_id_expression(),
                            it->get_ast(), it->get_scope_link());
                }
                
                // FIRSTPRIVATE references
                for (ObjectList<IdExpression>::iterator it = firstprivate_references.begin();
                        it != firstprivate_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo parameter("flp_" + it->mangle_id_expression(), 
                            "&" + it->prettyprint(), 
                            *it, pointer_type, ParameterInfo::BY_POINTER);
                    parameter_info.append(parameter);

                    result.add_replacement(symbol, "p_" + it->mangle_id_expression(),
                            it->get_ast(), it->get_scope_link());
                }
                
                // LASTPRIVATE references
                for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
                        it != lastprivate_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo parameter("flp_" + it->mangle_id_expression(), 
                            "&" + it->prettyprint(),
                            *it, pointer_type, ParameterInfo::BY_POINTER);
                    parameter_info.append(parameter);

                    result.add_replacement(symbol, "p_" + it->mangle_id_expression(),
                            it->get_ast(), it->get_scope_link());
                }
                
                // REDUCTION references
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    IdExpression id_expr = it->get_id_expression();
                    Symbol symbol = id_expr.get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo parameter("rdv_" + id_expr.mangle_id_expression(), 
                            "rdv_" + id_expr.mangle_id_expression(),
                            id_expr, pointer_type, ParameterInfo::BY_POINTER);
                    parameter_info.append(parameter);

                    result.add_replacement(symbol, "rdp_" + id_expr.mangle_id_expression(),
                            id_expr.get_ast(), id_expr.get_scope_link());
                }

                // Inner REDUCTION references (those coming from lexical enclosed DO's inner to this PARALLEL)
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = inner_reduction_references.begin();
                        it != inner_reduction_references.end();
                        it++)
                {
                    IdExpression id_expr = it->get_id_expression();
                    Symbol symbol = id_expr.get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo reduction_vector_parameter("rdv_" + id_expr.mangle_id_expression(), 
                            "rdv_" + id_expr.mangle_id_expression(),
                            id_expr, pointer_type, ParameterInfo::BY_POINTER);

                    parameter_info.append(reduction_vector_parameter);

                    ParameterInfo parameter(id_expr.mangle_id_expression(), 
                            "&" + id_expr.prettyprint(),
                            id_expr, pointer_type, ParameterInfo::BY_POINTER);

                    result.add_replacement(symbol, "(*" + id_expr.mangle_id_expression() + ")",
                            id_expr.get_ast(), id_expr.get_scope_link());
                }

                // COPYIN references
                for (ObjectList<IdExpression>::iterator it = copyin_references.begin();
                        it != copyin_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo parameter("cin_" + it->mangle_id_expression(), 
                            "&" + it->prettyprint(),
                            *it, type, ParameterInfo::BY_POINTER);
                    parameter_info.append(parameter);
                }

                // COPYPRIVATE references
                for (ObjectList<IdExpression>::iterator it = copyprivate_references.begin();
                        it != copyprivate_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    Type pointer_type = type.get_pointer_to();

                    ParameterInfo parameter("cout_" + it->mangle_id_expression(), 
                            "&" + it->prettyprint(),
                            *it, pointer_type, ParameterInfo::BY_POINTER);
                    parameter_info.append(parameter);
                }

                if (is_nonstatic_member_function(function_definition))
                {
                    // Calls to nonstatic member functions within the body of the construct
                    // of a nonstatic member function
                    ObjectList<IdExpression> function_references = 
                        construct_body.non_local_symbol_occurrences(Statement::ONLY_FUNCTIONS);
                    for (ObjectList<IdExpression>::iterator it = function_references.begin();
                            it != function_references.end();
                            it++)
                    {
                        if (is_unqualified_member_symbol(*it, function_definition))
                        {
                            Symbol symbol = it->get_symbol();
                            result.add_replacement(symbol, "_this->" + it->prettyprint(),
                                    it->get_ast(), it->get_scope_link());
                        }
                    }
                }

                return result;
            }

            Source get_loop_distribution_in_sections(
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

            Source get_loop_distribution_code(ForStatement for_statement,
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
                    << "int nth_low;"
                    << "int nth_upper;"
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

                    << "in__tone_begin_for_(&nth_low, &nth_upper, &nth_step, &nth_chunk, &nth_schedule);"

                    // Get a slice of the iteration space
                    << "while (in__tone_next_iters_(&intone_start, &intone_end, &intone_last) != 0)"
                    << "{"
                    << instrumentation_code_before
                           // And do something with it
                    << "   for (" << induction_var_name << " = intone_start; "
                    << "        nth_step >= 1 ? " << induction_var_name << " <= intone_end : " << induction_var_name << ">= intone_end;"
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

            Source get_loop_finalization(bool do_barrier)
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

            Source get_privatized_declarations(
                    ObjectList<IdExpression> private_references,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    ObjectList<IdExpression> copyin_references,
                    ObjectList<ParameterInfo> parameter_info_list
                    )
            {
                Source private_declarations;
                
                // PRIVATE
                for (ObjectList<IdExpression>::iterator it = private_references.begin();
                        it != private_references.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    private_declarations << 
                        comment("Private entity : '" + it->mangle_id_expression() + "'");
                    private_declarations
                        << type.get_declaration(
                                it->get_scope(),
                                "p_" + it->mangle_id_expression())
                        << ";"
                        ;
                }

                // FIRSTPRIVATE
                for (ObjectList<IdExpression>::iterator it = firstprivate_references.begin();
                        it != firstprivate_references.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    Source initializer_value;

                    if (parameter_info_list.contains(functor(&ParameterInfo::symbol), it->get_symbol()))
                    {
                        initializer_value << "(*flp_" << it->prettyprint() << ")";
                    }
                    else
                    {
                        initializer_value << it->prettyprint();
                    }

                    private_declarations << 
                        comment("Firstprivate entity : 'p_" + it->mangle_id_expression() + "'");

                    if (type.is_array())
                    {
                        // Both in C and C++ the firstprivatized array must be properly copied
                        private_declarations 
                            << type.get_declaration(
                                    it->get_scope(),
                                    "p_" + it->mangle_id_expression())
                            << ";"
                            ;

                        private_declarations 
                            << comment("This firstprivate entity is an array and must be initialized element-wise");

                        Source array_assignment = array_copy(type, "p_" + it->mangle_id_expression(),
                                initializer_value.get_source(), 0);

                        private_declarations << array_assignment;
                    }
                    else
                    {
                        C_LANGUAGE()
                        {
                            // If it is not an array just assign
                            private_declarations 
                                << type.get_declaration(
                                        it->get_scope(),
                                        "p_" + it->mangle_id_expression())
                                << ";"
                                << comment("Using plain assignment to initialize firstprivate entity")
                                << "p_" + it->mangle_id_expression() << "=" << initializer_value.get_source() << ";"
                                ;
                        }
                        CXX_LANGUAGE()
                        {
                            // In C++ if this is a class we invoke the copy-constructor
                            if (type.is_class())
                            {
                                private_declarations 
                                    << comment("Using copy constructor to initialize firstprivate entity")
                                    << type.get_declaration(
                                            it->get_scope(),
                                            "p_" + it->mangle_id_expression())
                                    << "(" << initializer_value.get_source() << ")"
                                    << ";"
                                    ;
                            }
                            else
                            {
                                // Otherwise simply assign
                                private_declarations 
                                    << type.get_declaration(
                                            it->get_scope(),
                                            "p_" + it->mangle_id_expression())
                                    << ";"
                                    << comment("Using assignment operator to initialize firstprivate entity")
                                    << "p_" + it->mangle_id_expression() << "=" << initializer_value.get_source() << ";"
                                    ;
                            }
                        }
                    }
                }
                
                // LASTPRIVATE
                for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
                        it != lastprivate_references.end();
                        it++)
                {
                    Symbol sym = it->get_symbol();
                    Type type = sym.get_type();

                    private_declarations
                        << comment("Lastprivate entity : 'p_" + it->mangle_id_expression() + "'")
                        << type.get_declaration(
                                it->get_scope(),
                                "p_" + it->mangle_id_expression())
                        << ";"
                        ;
                }
                
                // REDUCTION
                for (ObjectList<OpenMP::ReductionIdExpression>::iterator it = reduction_references.begin();
                        it != reduction_references.end();
                        it++)
                {
                    IdExpression id_expr = it->get_id_expression();
                    Symbol sym = id_expr.get_symbol();
                    Type type = sym.get_type();
                    
                    private_declarations
                        << comment("Reduction private entity : 'rdp_" + id_expr.mangle_id_expression() + "'")
                        << type.get_declaration_with_initializer(
                                id_expr.get_scope(),
                                "rdp_" + id_expr.mangle_id_expression(),
                                it->get_neuter().prettyprint())
                        << ";"
                        ;
                }

                // COPYIN
                for (ObjectList<IdExpression>::iterator it = copyin_references.begin();
                        it != copyin_references.end();
                        it++)
                {
                    private_declarations
                        << comment("Initializing copyin entity '" + it->prettyprint() + "'")
                        << it->prettyprint() << " = " << "(*cin_" + it->mangle_id_expression() << ");"
                        ;
                }

                return private_declarations;
            }

            Source get_lastprivate_assignments(
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<IdExpression> copyprivate_references,
                    ObjectList<ParameterInfo> parameter_info_list)
            {
                Source lastprivate_assignments;

                // LASTPRIVATE
                for (ObjectList<IdExpression>::iterator it = lastprivate_references.begin();
                        it != lastprivate_references.end();
                        it++)
                {
                    Symbol symbol = it->get_symbol();
                    Type type = symbol.get_type();

                    std::string output_object;

                    if (parameter_info_list.contains(functor(&ParameterInfo::symbol), it->get_symbol()))
                    {
                        output_object = "(*flp_" + it->mangle_id_expression() + ")";
                    }
                    else
                    {
                        output_object = it->prettyprint();
                    }

                    lastprivate_assignments
                        << comment("Assignment of lastprivate entity: '" + output_object + "'");

                    if (type.is_array())
                    {
                        Source array_assignment = array_copy(type, output_object,
                                "p_" + it->mangle_id_expression(), 0);

                        lastprivate_assignments 
                            << comment("Entity is an array and must be assigned element-wise")
                            << array_assignment;
                    }
                    else
                    {
                        lastprivate_assignments
                            << output_object << " = p_" << it->mangle_id_expression() << ";"
                            ;
                    }
                }
                
                // COPYPRIVATE
                for (ObjectList<IdExpression>::iterator it = copyprivate_references.begin();
                        it != copyprivate_references.end();
                        it++)
                {
                    lastprivate_assignments
                        << comment("Assignment of copyprivate entity 'cout_" + it->mangle_id_expression() + "'")
                        << "(*cout_" << it->mangle_id_expression() << ")" << " = p_" << it->mangle_id_expression() << ";"
                        ;
                }

                return lastprivate_assignments;
            }

            Source get_outlined_function_name(IdExpression function_name, 
                    bool want_fully_qualified = true, 
                    bool want_templated_name = false)
            {
                Source result;
                if (function_name.is_qualified() && want_fully_qualified)
                {
                    result
                        << function_name.get_qualified_part()
                        ;
                }
                if (function_name.is_qualified() && want_templated_name)
                {
                    result << " template ";
                }
                result
                    << "nth__" << function_name.get_unqualified_part() << "_" << num_parallels;

                return result;
            }

            bool is_nonstatic_member_function(FunctionDefinition function_definition)
            {
                IdExpression function_name = function_definition.get_function_name();
                Symbol function_symbol = function_name.get_symbol();

                // It must be a member
                if (!function_symbol.is_member())
                {
                    return false;
                }

                Statement function_body = function_definition.get_function_body();
                Scope function_body_scope = function_body.get_scope();

                Symbol sym = function_body_scope.get_symbol_from_name("this");

                if (!sym.is_valid())
                {
                    return false;
                }

                return true;
            }

            Source array_copy(Type t, const std::string& dest, const std::string& orig, int level)
            {
                Source result;

                std::stringstream subscript;

                for (int i = 0; i < level; i++)
                {
                    subscript << "[_i_" << i << "]";
                }

                if (!t.is_array())
                {
                    result 
                        << dest << subscript.str() << "=" << orig << subscript.str() << ";"
                        ;
                }
                else
                {
                    std::stringstream index_var;
                    index_var << "_i_" << level;

                    Source next_dim_array_copy = array_copy(t.array_element(), dest, orig, level+1);

                    result 
                        << "{"
                        << "  int " << index_var.str() << ";"
                        << "  for (" << index_var.str() << " = 0;" 
                        <<              index_var.str() 
                        <<                 " < (sizeof(" << dest 
                        <<                 subscript.str() << ")/sizeof(" << dest << subscript.str() << "[0]));"
                        <<              index_var.str() << "++" << ")"
                        << "  {"
                        <<       next_dim_array_copy
                        << "  }"
                        << "}"
                        ;
                }

                return result;
            }

            bool is_unqualified_member_symbol(IdExpression id_expression, FunctionDefinition function_definition)
            {
                Symbol current_symbol = id_expression.get_symbol();
                Symbol function_symbol = function_definition.get_function_name().get_symbol();

                if (function_symbol.is_member()
                        && current_symbol.is_member()
                        && (function_symbol.get_class_type() 
                            == current_symbol.get_class_type()))
                {
                    return 1;
                }

                return 0;
            }

            bool is_function_accessible(IdExpression id_expression, 
                    FunctionDefinition function_definition)
            {
                Symbol current_symbol = id_expression.get_symbol();

                Scope function_scope = function_definition.get_scope();
                Symbol function_visible_symbol = function_scope.get_symbol_from_id_expr(id_expression.get_ast());

                return (function_visible_symbol.is_valid()
                        && function_visible_symbol == current_symbol);
            }

            void instrumentation_outline(Source& instrumentation_code_before,
                    Source& instrumentation_code_after,
                    FunctionDefinition function_definition,
                    Statement construct_body)
            {
                if (instrumentation_requested())
                {
                    std::string file_name = "\"" + function_definition.get_ast().get_file() + "\"";

                    int file_line = construct_body.get_ast().get_line();

                    std::string mangled_function_name = 
                        "\"" + function_definition.get_function_name().mangle_id_expression() + "\"";

                    instrumentation_code_before
                        << "const int EVENT_CALL_USER_FUNCTION = 60000018;"
                        << "int _user_function_event = mintaka_index_get(" << file_name << "," << file_line << ");"
                        << "if (_user_function_event == -1)"
                        << "{"
                        << "     nthf_spin_lock_((nth_word_t*)&_nthf_unspecified_critical);"
                        << "     _user_function_event = mintaka_index_allocate2(" << file_name << "," 
                        <<                file_line << "," << mangled_function_name << ", EVENT_CALL_USER_FUNCTION);"
                        << "     nthf_spin_unlock_((nth_word_t*)&_nthf_unspecified_critical);"
                        << "}"
                        << "mintaka_event(EVENT_CALL_USER_FUNCTION, _user_function_event);"
                        << "int __previous_state = mintaka_get_state();"
                        << "mintaka_state_run();"
                        ;

                    instrumentation_code_after
                        << "mintaka_event(EVENT_CALL_USER_FUNCTION, 0);"
                        << "mintaka_set_state(__previous_state);"
                        ;

                    // Ensure that it has been defined
                    define_global_mutex("_nthf_unspecified_critical", function_definition.get_ast(),
                            function_definition.get_scope_link());
                }
            }

            IdExpression warn_unreferenced_data(IdExpression id_expr)
            {
                std::cerr << "Warning: Entity '" << id_expr.prettyprint() << "' in " << id_expr.get_ast().get_locus() 
                    << " is not referenced in the body of the construct" << std::endl;
                return id_expr;
            }

            IdExpression warn_no_data_sharing(IdExpression id_expr)
            {
                std::cerr << "Warning: '" << id_expr.prettyprint() << "' in " << id_expr.get_ast().get_locus() 
                    << " does not have a data sharing attribute and 'default(none)' was specified. "
                    << "It will be considered shared." << std::endl;
                return id_expr;
            }

            Source debug_parameter_info(
                    ObjectList<ParameterInfo> parameter_info_list)
            {
                std::stringstream info;

                info << "Parameter information: " << std::endl;

                if (parameter_info_list.empty())
                {
                    info << "No parameters" << std::endl;
                }
                else
                for (ObjectList<ParameterInfo>::iterator it = parameter_info_list.begin();
                        it != parameter_info_list.end();
                        it++)
                {
                    info << "'" << it->parameter_name << "' ";

                    if (it->kind == ParameterInfo::BY_VALUE)
                    {
                        info << "Passed by value (private pointer). ";
                    }
                    else if (it->kind == ParameterInfo::BY_POINTER)
                    {
                        info << "Passed by reference (global pointer). ";
                    }

                    info << "Original type: " 
                        << it->type.get_declaration(it->id_expression.get_scope(), "") << ". ";

                    info << "Related id-expression: " 
                        << it->id_expression.get_ast().get_locus() << ". ";

                    info << std::endl;
                }

                return comment(info.str());
            }

            // Debug purposes
            IdExpression print_id_expression(IdExpression id_expression)
            {
                std::cerr << "-> " << id_expression.prettyprint() << std::endl;

                return id_expression;
            }

            // ###########################################################
            //    #pragma omp transaction
            //       statement
            // ###########################################################
            
            class ExpressionReplacement 
            {
                private:
                    ObjectList<Symbol> _considered_symbols;
                public:
                    ExpressionReplacement(ObjectList<Symbol>& considered_symbols)
                        : _considered_symbols(considered_symbols)
                    {
                    }

                    void get_address(Expression expression)
                    {
                        Source address_expression;
                        // var => (&var) 
                        if (expression.is_id_expression())
                        {
                            address_expression << "(&" << expression.prettyprint() << ")";
                        }
                        // *e1 => READ(e1)
                        else if (expression.is_unary_operation()
                                && expression.get_operation_kind() == Expression::DERREFERENCE)
                        {
                            replace_expression(expression.get_unary_operand());
                            
                            address_expression << "(" << expression.get_unary_operand().prettyprint() << ")";
                        }
                        // e1[e2] => READ(e1) + READ(e2)
                        else if (expression.is_array_subscript())
                        {

                            Source original_array;
                            original_array << expression.get_subscripted_expression().prettyprint();

                            replace_expression(expression.get_subscripted_expression());
                            replace_expression(expression.get_subscript_expression());

                            address_expression
                                << "((void*)&(" << original_array << ") == ((void*)&(" << original_array << "[0])) ? "
                                << "(" << original_array << " + " << expression.get_subscript_expression().prettyprint()    << ")"
                                << ": ("
                                << expression.get_subscripted_expression().prettyprint()
                                << " + "
                                << expression.get_subscript_expression().prettyprint()
                                << ")"
                                << ")"
                                ;
                        }
                        // e1->e2 => (&(READ(e1)->e2))
                        else if (expression.is_pointer_member_access())
                        {
                            replace_expression(expression.get_accessed_entity());

                            address_expression
                                << "(&( ( "
                                << expression.get_accessed_entity().prettyprint()
                                << ") -> "
                                << expression.get_accessed_member().prettyprint()
                                << "))"
                                ;
                        }
                        // (*e1).e2 => (&(READ(e1))->e2)
                        else if (expression.is_member_access() 
                                && expression.get_accessed_entity().is_unary_operation()
                                && (expression.get_accessed_entity().get_operation_kind() 
                                    == Expression::DERREFERENCE))
                        {
                            Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                            replace_expression(accessed_entity);

                            address_expression
                                << "(&( ( "
                                << accessed_entity.prettyprint()
                                << ") -> "
                                << expression.get_accessed_member().prettyprint()
                                << "))"
                                ;
                        }
                        // e1.e2 => (&((ADDR(e1))->e2))
                        else if (expression.is_member_access())
                        {
                            get_address(expression.get_accessed_entity());

                            address_expression
                                << "(&( ( "
                                << expression.get_accessed_entity().prettyprint()
                                << ") -> "
                                << expression.get_accessed_member().prettyprint()
                                << "))"
                                ;
                        }
                        // e1 ## e2 => 
                        else if (expression.is_binary_operation())
                        {
                            std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
                        }
                        // ## e1
                        else if (expression.is_unary_operation())
                        {
                            std::cerr << "Lvalue not valid '" << expression.prettyprint() << std::endl;
                        }
                        // Other expressions (function calls and literals)
                        else
                        {
                            address_expression << expression.prettyprint();
                        }

                        AST_t address_expression_tree = address_expression.parse_expression(expression.get_ast(), expression.get_scope_link());

                        expression.get_ast().replace(address_expression_tree);
                    }

                    void replace_expression(Expression expression, bool replace_outermost = true)
                    {
                        Source read_expression;
                        // e1 = e2 => write(__t, ADDR(e1), READ(e2))
                        if (expression.is_assignment())
                        {
                            get_address(expression.get_first_operand());
                            replace_expression(expression.get_second_operand());

                            read_expression
                                << "write(__t, "
                                << expression.get_first_operand().prettyprint()
                                << ","
                                << expression.get_second_operand().prettyprint()
                                << ")"
                                ;
                        }
                        // var => *read(__t, &var)
                        else if (expression.is_id_expression())
                        {
                            read_expression
                                << "*read(__t, "
                                << "&" << expression.prettyprint()
                                << ")"
                                ;
                        }
                        // *e =>  *read(__t, READ(e))
                        else if (expression.is_unary_operation()
                                && expression.get_operation_kind() == Expression::DERREFERENCE)
                        {
                            replace_expression(expression.get_unary_operand());

                            read_expression
                                << "*read(__t, "
                                << expression.get_unary_operand().prettyprint()
                                << ")"
                                ;
                        }
                        // e1[e2] => *read(__t, READ(e1) + READ(e2))
                        else if (expression.is_array_subscript())
                        {
                            Source original_array;
                            original_array << expression.get_subscripted_expression().prettyprint();

                            replace_expression(expression.get_subscripted_expression());
                            replace_expression(expression.get_subscript_expression());

                            read_expression
                                << "((void*)&(" << original_array << ") == ((void*)&(" << original_array << "[0])) ? "
                                << "* (" << original_array << " + " << expression.get_subscript_expression().prettyprint()  << ")"
                                << ": *read(__t, "
                                << expression.get_subscripted_expression().prettyprint()
                                << " + "
                                << expression.get_subscript_expression().prettyprint()
                                << ")"
                                << ")"
                                ;
                        }
                        // e1->e2 => *read(__t, (READ(e1))->e2)
                        else if (expression.is_pointer_member_access())
                        {
                            replace_expression(expression.get_accessed_entity());

                            read_expression
                                << "*read(__t, "
                                << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                                << " -> "
                                << expression.get_accessed_member().prettyprint()
                                << ")"
                                << ")"
                                ;
                        }
                        // (*e1).e2 => *read(__t, (READ(e1))->e2)
                        else if (expression.is_member_access()
                                && expression.get_accessed_entity().is_unary_operation()
                                && (expression.get_accessed_entity().get_operation_kind() 
                                    == Expression::DERREFERENCE))
                        {
                            Expression accessed_entity = expression.get_accessed_entity().get_unary_operand();
                            replace_expression(accessed_entity);

                            read_expression
                                << "*read(__t, "
                                << "&((" << accessed_entity.prettyprint() << ")"
                                << " -> "
                                << expression.get_accessed_member().prettyprint()
                                << ")"
                                << ")"
                                ;
                        }
                        // e1.e2 => *read(__t, (ADDR(e1))->e2)
                        else if (expression.is_member_access())
                        {
                            get_address(expression.get_accessed_entity());

                            read_expression
                                << "*read(__t, "
                                << "&((" << expression.get_accessed_entity().prettyprint() << ")"
                                << " -> "
                                << expression.get_accessed_member().prettyprint()
                                << ")"
                                << ")"
                                ;
                        }
                        // e1 ## e2 =>
                        else if (expression.is_binary_operation())
                        {
                            replace_expression(expression.get_first_operand());
                            replace_expression(expression.get_second_operand());

                            // Don't do anything else
                            return;
                        }
                        // ## e1
                        else if (expression.is_unary_operation())
                        {
                            if (expression.get_operation_kind() == Expression::REFERENCE)
                            {
                                // & e1
                                Expression address_expr = expression.get_unary_operand();
                                get_address(address_expr);
                                expression.get_ast().replace_with(address_expr.get_ast());
                            }
                            else if (expression.get_operation_kind() == Expression::PREINCREMENT ||
                                expression.get_operation_kind() == Expression::PREDECREMENT)
                            {
                                // ++e1
                                // e1 = e1 + 1
                                Source increment_code;

                                if (expression.get_operation_kind() == Expression::PREINCREMENT)
                                {
                                    increment_code << " + 1";
                                }
                                else // (expression.get_operation_kind() == Expression::PREDECREMENT)
                                {
                                    increment_code << " - 1";
                                }

                                Source flat_code;
                                flat_code << expression.get_unary_operand().prettyprint()
                                    << " = "
                                    << expression.get_unary_operand().prettyprint()
                                    << increment_code;

                                AST_t flat_code_tree = flat_code.parse_expression(expression.get_ast(),
                                        expression.get_scope_link());
                                Expression flat_code_expr(flat_code_tree, expression.get_scope_link());
                                replace_expression(flat_code_expr);

                                Source derref_write;
                                derref_write << "*(" << flat_code_expr.prettyprint() << ")";

                                AST_t derref_write_tree = derref_write.parse_expression(expression.get_ast(),
                                        expression.get_scope_link());

                                expression.get_ast().replace_with(derref_write_tree);
                            }
                            else if (expression.get_operation_kind() == Expression::POSTINCREMENT
                                    || expression.get_operation_kind() == Expression::POSTDECREMENT)
                            {
                                Source post_source;
                                Source incremented_operand, increment_operand;

                                Source read_operand_src;
                                read_operand_src << expression.get_unary_operand().prettyprint();

                                post_source 
                                    << "({"
                                    << "__typeof__(" << read_operand_src << ") "
                                    << "__temp = " << incremented_operand << ";"
                                    << increment_operand << ";"
                                    << "__temp;"
                                    << "})"
                                    ;

                                AST_t read_operand_tree = 
                                    read_operand_src.parse_expression(expression.get_ast(),
                                        expression.get_scope_link());

                                Expression read_operand_expr(read_operand_tree, expression.get_scope_link());
                                replace_expression(read_operand_expr);

                                incremented_operand << read_operand_expr.prettyprint();

                                Source increment_source;
                                Source increment_code;

                                if (expression.get_operation_kind() == Expression::POSTINCREMENT)
                                {
                                    increment_code << " + 1";
                                }
                                else // (expression.get_operation_kind() == Expression::POSTDECREMENT)
                                {
                                    increment_code << " - 1";
                                }
                                increment_source << read_operand_src 
                                    << " = "
                                    << read_operand_src
                                    << increment_code
                                    ;

                                AST_t increment_tree =
                                    increment_source.parse_expression(expression.get_ast(),
                                        expression.get_scope_link());
                                Expression increment_expr(increment_tree, expression.get_scope_link());
                                replace_expression(increment_expr);

                                increment_operand << increment_expr.prettyprint()
                                    ;

                                AST_t post_tree = post_source.parse_expression(expression.get_ast(),
                                        expression.get_scope_link());

                                expression.get_ast().replace_with(post_tree);
                            }
                            else
                            {
                                replace_expression(expression.get_unary_operand());
                            }
                            
                            // Don't do anything else
                            return;
                        }
                        else if (expression.is_function_call())
                        {
                            Expression called_expression = expression.get_called_expression();
                            if (called_expression.is_id_expression())
                            {
                                // A simple function call of the form "f(...)"
                                Source replace_call, replace_args;

                                replace_call
                                    << "__stm_" << called_expression.prettyprint() 
                                    << "(" << replace_args << ")"
                                    ;

                                replace_args.append_with_separator("__t", ",");

                                ObjectList<Expression> arguments = expression.get_argument_list();
                                for (ObjectList<Expression>::iterator it = arguments.begin();
                                        it != arguments.end();
                                        it++)
                                {
                                    replace_args.append_with_separator(it->prettyprint(), ",");
                                }

                                // Now parse the function call
                                std::cerr << "Parsing '" << replace_call.get_source() << "'" << std::endl; 
                                AST_t replace_call_tree = replace_call.parse_expression(
                                        called_expression.get_ast(),
                                        called_expression.get_scope_link());

                                Expression replaced_function_call(replace_call_tree, 
                                        called_expression.get_scope_link());

                                // This is a function call
                                arguments = replaced_function_call.get_argument_list();
                                for (ObjectList<Expression>::iterator it = arguments.begin();
                                        it != arguments.end();
                                        it++)
                                {
                                    if (it == arguments.begin())
                                        continue;
                                    replace_expression(*it);
                                }

                                expression.get_ast().replace_with(replace_call_tree);
                            }

                            return;
                        }
                        // Other expressions (function calls and literals)
                        else
                        {
                            // Don't do anything else
                            return;
                        }

                        // Replace the expression
                        AST_t read_expression_tree = read_expression.parse_expression(expression.get_ast(),
                                expression.get_scope_link());

                        expression.get_ast().replace(read_expression_tree);
                    }
            };

            void transaction_postorder(OpenMP::CustomConstruct protect_construct)
            {
                ObjectList<Symbol> considered_symbols;
                ObjectList<Symbol> excluded_symbols;

                Statement protect_statement = protect_construct.body();
                OpenMP::Directive protect_directive = protect_construct.directive();

                OpenMP::CustomClause exclude_clause = protect_directive.custom_clause("exclude");
                OpenMP::CustomClause converted_function = 
                    protect_directive.custom_clause("converted_function");

                if (exclude_clause.is_defined())
                {
                    excluded_symbols = 
                        exclude_clause.id_expressions().map(functor(&IdExpression::get_symbol));
                }

                OpenMP::CustomClause only_clause = protect_directive.custom_clause("only");

                if (only_clause.is_defined())
                {
                    considered_symbols = 
                        only_clause.id_expressions().map(functor(&IdExpression::get_symbol));
                }
                else
                {
                    considered_symbols = 
                        protect_statement.non_local_symbol_occurrences().map(functor(&IdExpression::get_symbol));
                }

                considered_symbols = considered_symbols.filter(not_in_set(excluded_symbols));

                // For every expression, replace it properly with read and write
                PredicateBool<LANG_IS_EXPRESSION_NEST> expression_pred;
                ExpressionReplacement expression_replacement(considered_symbols);

                ObjectList<AST_t> expressions = protect_statement.get_ast().depth_subtrees(expression_pred, AST_t::NON_RECURSIVE);

                for (ObjectList<AST_t>::iterator it = expressions.begin();
                        it != expressions.end();
                        it++)
                {
                    Expression expression(*it, protect_statement.get_scope_link());

                    expression_replacement.replace_expression(expression);
                }

                // And now find every 'return' statement and protect it
                PredicateBool<LANG_IS_RETURN_STATEMENT> return_pred;
                ObjectList<AST_t> returns = protect_statement.get_ast().depth_subtrees(return_pred, AST_t::NON_RECURSIVE);
                for (ObjectList<AST_t>::iterator it = returns.begin();
                        it != returns.end();
                        it++)
                {
                    Source return_replace_code;

                    Statement return_statement(*it, protect_statement.get_scope_link());

                    FunctionDefinition enclosing_function_def = return_statement.get_enclosing_function();

                    IdExpression function_name = enclosing_function_def.get_function_name();
                    Symbol function_symbol = function_name.get_symbol();
                    Type function_type = function_symbol.get_type();
                    Type return_type = function_type.returns();

                    Source return_value;
                    ObjectList<AST_t> return_expression_list = return_statement.get_ast().depth_subtrees(
                            PredicateBool<LANG_IS_EXPRESSION_NEST>(), 
                            AST_t::NON_RECURSIVE);
                    if (!return_expression_list.empty()
                            && !return_type.is_void())
                    {
                        // Only if we have a value non-void
                        Expression returned_expression(*(return_expression_list.begin()), 
                                enclosing_function_def.get_scope_link());

                        AST_t node = enclosing_function_def.get_ast();
                        ObjectList<AST_t> functional_declarator = 
                            node.depth_subtrees(PredicateBool<LANG_IS_FUNCTIONAL_DECLARATOR>(), 
                                    AST_t::NON_RECURSIVE);

                        AST_t first_functional_declarator = *(functional_declarator.begin());
                        ObjectList<AST_t> declared_parameters = 
                            first_functional_declarator.depth_subtrees(
                                    PredicateBool<LANG_IS_DECLARED_PARAMETER>(),
                                    AST_t::NON_RECURSIVE);

                        Source cancel_source;
                        {
                            ObjectList<AST_t>::iterator it = declared_parameters.begin();
                            if (converted_function.is_defined())
                            {
                                // Skip the first one if the converted_function clause
                                // was defined
                                it++;
                            }
                            for (; it != declared_parameters.end();
                                    it++)
                            {
                                AST_t declared_name = it->get_attribute(LANG_DECLARED_PARAMETER);
                                cancel_source
                                    << "invalidateAdrInTx(__t, &" << declared_name.prettyprint() << ");"
                                    ;
                            }
                        }

                        return_value
                            << return_type.get_declaration(function_name.get_scope(), 
                                    "__tx_retval") << ";"
                            << "     __tx_retval = " << returned_expression.prettyprint() << ";"
                            << cancel_source
                            << "     return __tx_retval;"
                            ;
                    }
                    else
                    {
                        return_value << return_statement.prettyprint();
                    }

                    if (!converted_function.is_defined())
                    {
                    return_replace_code
                        << "{"
                        << "  _tx_commit_start = rdtscf();"
                        << "  if (0 == committx(__t))"
                        << "  {"
                        << "       _tx_commit_end = rdtscf();"
                        << "       _tx_commit_total += (_tx_commit_end - _tx_commit_start);"
                        << "       _tx_end = rdtscf();"
                        << "       _tx_total_time += (_tx_end - _tx_start);"
                        << "       destroytx(__t);"
                        // Assumption: transaction is completely inside the function.
                        // FIXME: Think about it
                        << return_value
                        << "  }"
                        << "  else" 
                        << "  {"
                        << "     _tx_abort_count++;"
                        << "     aborttx(__t);"
                        // TODO : This will break when the return is contained in another loop (while, for, do..while)
                        << "     continue;"
                        << "  }"
                        << "}"
                        ;
                    }
                    else
                    {
                        return_replace_code
                            << "{"
                            << return_value
                            << "}"
                            ;
                    }
                
                    AST_t return_tree = return_replace_code.parse_statement(return_statement.get_ast(),
                            return_statement.get_scope_link());

                    it->replace(return_tree);
                }

                Source replaced_code;
                
                if (converted_function.is_defined())
                {
                    replaced_code
                        << "{"
                        << "   uint64_t _tx_start, _tx_end;"
                        << "   uint64_t _tx_commit_start, _tx_commit_end;"
                        << "   _tx_start = rdtscf();"
                        <<         protect_statement.prettyprint()
                        << "}"
                        ;
                }
                else
                {
                    replaced_code
                        << "{"
                        << "   Transaction* __t = createtx();"
                        << "   uint64_t _tx_start, _tx_end;"
                        << "   uint64_t _tx_commit_start, _tx_commit_end;"
                        << "   _tx_start = rdtscf();"
                        << "   _tx_total_count++;"
                        << "   while(1)"
                        << "   {"
                        << "     starttx(__t);"
                        // << "     int ret = setjmp(__t->context);"
                        << "     if((__t->nestingLevel > 0) || (0 == setjmp(__t->context)))"
                        << "     {"
                        <<         comment("Protected code")
                        <<         protect_statement.prettyprint()
                        <<         comment("End of protected code")
                        << "       _tx_commit_start = rdtscf();"
                        << "       if (0 == committx(__t)) "
                        << "       {"
                        << "         _tx_commit_end = rdtscf();"
                        << "         _tx_commit_total += (_tx_commit_end - _tx_commit_start);"
                        << "         break;"
                        << "       }"
                        << "       else"
                        << "       {"
                        << "          _tx_abort_count++;"
                        << "          aborttx(__t);"
                        << "       }"
                        << "     }"
                        << "     else"
                        << "     {"
                        << "        _tx_abort_count++;"
                        << "        aborttx(__t);"
                        << "     }"
                        << "   }"
                        << "   _tx_end = rdtscf();"
                        << "   _tx_total_time += (_tx_end - _tx_start);"
                        << "   destroytx(__t);"
                        << "}"
                        ;
                }

                AST_t replaced_tree = replaced_code.parse_statement(protect_statement.get_ast(),
                        protect_statement.get_scope_link());

                protect_construct.get_ast().replace(replaced_tree);
            }

    };

}

EXPORT_PHASE(TL::OpenMPTransform);
