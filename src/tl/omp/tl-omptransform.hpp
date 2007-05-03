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
#ifndef TL_OMPTRANSFORM_HPP
#define TL_OMPTRANSFORM_HPP

#include "cxx-utils.h"
#include "tl-omp.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include "tl-externalvars.hpp"
#include "tl-functionfilter.hpp"
#include "tl-parameterinfo.hpp"
#include <iostream>
#include <utility>
#include <stack>
#include <set>

namespace TL
{
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

			// Function filter for STM
			FunctionFilterFile function_filter;
			int transaction_nesting;
        public:
            OpenMPTransform();

            bool instrumentation_requested();

            virtual ~OpenMPTransform();

            // Initialization function called from OpenMP::OpenMPPhase
            virtual void init();
            
            void parallel_preorder(OpenMP::ParallelConstruct parallel_construct);
            void parallel_postorder(OpenMP::ParallelConstruct parallel_construct);

            void parallel_for_preorder(OpenMP::ParallelForConstruct parallel_for_construct);
            void parallel_for_postorder(OpenMP::ParallelForConstruct parallel_for_construct);

            void for_preorder(OpenMP::ForConstruct for_construct);
            void for_postorder(OpenMP::ForConstruct for_construct);

            void parallel_sections_preorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct);

            void parallel_sections_postorder(OpenMP::ParallelSectionsConstruct parallel_sections_construct);

            void sections_preorder(OpenMP::SectionsConstruct sections_construct);
            void sections_postorder(OpenMP::SectionsConstruct sections_construct);

            void section_postorder(OpenMP::SectionConstruct section_construct);

            void barrier_postorder(OpenMP::BarrierDirective barrier_directive);

            void atomic_postorder(OpenMP::AtomicConstruct atomic_construct);

            void ordered_postorder(OpenMP::OrderedConstruct ordered_construct);

            void master_postorder(OpenMP::MasterConstruct master_construct);

            void single_postorder(OpenMP::SingleConstruct single_construct);

            void parallel_single_preorder(OpenMP::ParallelSingleConstruct parallel_single_construct);
            void parallel_single_postorder(OpenMP::ParallelSingleConstruct parallel_single_construct);

            void critical_postorder(OpenMP::CriticalConstruct critical_construct);

            void define_global_mutex(std::string mutex_variable, AST_t ref_tree, ScopeLink sl);

            void threadprivate_postorder(OpenMP::ThreadPrivateDirective threadprivate_directive);

            void task_postorder(OpenMP::CustomConstruct task_construct);

            void taskwait_postorder(OpenMP::CustomConstruct taskwait_construct);

            void taskyield_postorder(OpenMP::CustomConstruct taskyield_construct);

            void taskgroup_postorder(OpenMP::CustomConstruct taskgroup_construct);

            void flush_postorder(OpenMP::FlushDirective flush_directive);

            AST_t get_parallel_spawn_code(
                    AST_t ref_tree, 
                    FunctionDefinition function_definition,
                    Scope scope,
                    ScopeLink scope_link,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    OpenMP::Clause num_threads_clause,
                    OpenMP::CustomClause groups_clause,
                    Source& instrument_code_before,
                    Source& instrument_code_after);
            
            std::string get_outline_function_reference(FunctionDefinition function_definition,
                    ObjectList<ParameterInfo>& parameter_info_list);

            Source get_critical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references);
            Source get_noncritical_reduction_code(ObjectList<OpenMP::ReductionIdExpression> reduction_references);
            Source get_noncritical_inlined_reduction_code(
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references);
            Source get_reduction_update(ObjectList<OpenMP::ReductionIdExpression> reduction_references);
            Source get_reduction_gathering(ObjectList<OpenMP::ReductionIdExpression> reduction_references);

            Source get_one_user_defined_gathering(OpenMP::ReductionIdExpression reduction_id_expr);

            Source get_member_function_declaration(
                    FunctionDefinition function_definition,
                    Declaration function_declaration,
                    Source outlined_function_name,
                    ObjectList<ParameterInfo> parameter_info_list
                    );

            Source get_outline_common(
                    FunctionDefinition function_definition,
                    Source& specific_body,
                    Source outlined_function_name,
                    ObjectList<ParameterInfo> parameter_info_list
                    );

            Source get_formal_parameters(
                    FunctionDefinition function_definition,
                    ObjectList<ParameterInfo> parameter_info_list,
                    Scope decl_scope);

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
                    );

            AST_t get_outline_task(
                    FunctionDefinition function_definition,
                    Source outlined_function_name,
                    Statement construct_body,
                    ReplaceIdExpression replace_references,
                    ObjectList<ParameterInfo> parameter_info_list,
                    ObjectList<IdExpression> local_references
                    );

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
                    ObjectList<IdExpression> copyprivate_references);

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
                    );

            Source get_task_block_code();

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
                    );

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
                    ObjectList<IdExpression>& copyprivate_references);

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
                    ObjectList<IdExpression>& copyprivate_references);

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
                    bool share_always = false);

            Source get_loop_distribution_in_sections(
                    int num_sections,
                    Statement construct_body,
                    ReplaceIdExpression replace_references);

            Source get_loop_distribution_code(ForStatement for_statement,
                    ReplaceIdExpression replace_references,
                    FunctionDefinition function_definition,
                    OpenMP::Directive directive);

            Source get_loop_finalization(bool do_barrier);

            Source get_privatized_declarations(
                    ObjectList<IdExpression> private_references,
                    ObjectList<IdExpression> firstprivate_references,
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<OpenMP::ReductionIdExpression> reduction_references,
                    ObjectList<IdExpression> copyin_references,
                    ObjectList<ParameterInfo> parameter_info_list
                    );

            Source get_lastprivate_assignments(
                    ObjectList<IdExpression> lastprivate_references,
                    ObjectList<IdExpression> copyprivate_references,
                    ObjectList<ParameterInfo> parameter_info_list);

            Source get_outlined_function_name(IdExpression function_name, 
                    bool want_fully_qualified = true, 
                    bool want_templated_name = false);

            bool is_nonstatic_member_function(FunctionDefinition function_definition);

            Source array_copy(Type t, const std::string& dest, const std::string& orig, int level);

            bool is_unqualified_member_symbol(IdExpression id_expression, FunctionDefinition function_definition);

            bool is_function_accessible(IdExpression id_expression, 
                    FunctionDefinition function_definition);

            void instrumentation_outline(Source& instrumentation_code_before,
                    Source& instrumentation_code_after,
                    FunctionDefinition function_definition,
                    Statement construct_body);

            IdExpression warn_unreferenced_data(IdExpression id_expr);

            IdExpression warn_no_data_sharing(IdExpression id_expr);

            Source debug_parameter_info(
                    ObjectList<ParameterInfo> parameter_info_list);

            // Debug purposes
            IdExpression print_id_expression(IdExpression id_expression);

            // STM support
            class ExpressionReplacement;

            void transaction_preorder(OpenMP::CustomConstruct protect_construct);
            void transaction_postorder(OpenMP::CustomConstruct protect_construct);
            
            // Experimental
            void task_while_postorder(OpenMP::CustomConstruct while_construct);
            void task_while_preorder(OpenMP::CustomConstruct while_construct);

            void task_postorder_with_chunk(OpenMP::CustomConstruct task_construct);

			void retry_postorder(OpenMP::CustomConstruct protect_construct);
    };
}

#endif // TL_OMPTRANSFORM_HPP
