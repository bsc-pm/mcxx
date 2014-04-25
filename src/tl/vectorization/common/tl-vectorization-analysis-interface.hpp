/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#ifndef TL_VECTORIZATION_ANALYSIS_INTERFACE_HPP
#define TL_VECTORIZATION_ANALYSIS_INTERFACE_HPP


#include "tl-vectorizer-environment.hpp"
#include "tl-vectorization-common.hpp"

#include "tl-vectorization-analysis-new.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-nodecl-utils.hpp"

#include <list>

namespace TL
{
namespace Vectorization
{
    class VectorizationAnalysisMaps
    {
        protected:
            Nodecl::Utils::NodeclDeepCopyMap _orig_to_copy_nodes;
            Nodecl::Utils::NodeclDeepCopyMap _copy_to_orig_nodes;

            Nodecl::Utils::SymbolDeepCopyMap _orig_to_copy_symbols;
            Nodecl::Utils::SymbolDeepCopyMap _copy_to_orig_symbols;

            std::list<Nodecl::NodeclBase> _registered_nodes;
    };


    class VectorizationAnalysisInterface : public VectorizationAnalysisMaps,
                                           public Analysis::VectorizationAnalysis
    {
        private:
            static VectorizationAnalysisInterface *_vectorizer_analysis;

            Nodecl::NodeclBase _original_node;

            Nodecl::FunctionCode copy_function_code(const Nodecl::FunctionCode& n);

            Nodecl::NodeclBase translate_input(const Nodecl::NodeclBase& n);
            objlist_nodecl_t translate_input(
                    const objlist_nodecl_t& list);
            TL::Symbol translate_input(const TL::Symbol& n) const;
            std::map<TL::Symbol, int> translate_input(
                    const std::map<TL::Symbol, int>& map);

            Nodecl::NodeclBase translate_output(const Nodecl::NodeclBase& n);
            objlist_nodecl_t translate_output(const objlist_nodecl_t& list);
            TL::Symbol translate_output(const TL::Symbol& n) const;

            Nodecl::Utils::NodeclDeepCopyMap::iterator find_equal_nodecl(
                    const Nodecl::NodeclBase& n,
                    Nodecl::Utils::NodeclDeepCopyMap& map);
            Nodecl::NodeclBase get_translated_copy(const Nodecl::NodeclBase& n);

            void register_node(const Nodecl::NodeclBase& n);
            void unregister_node(const Nodecl::NodeclBase& n);
            void unregister_nodes();

        public:
            static void initialize_analysis(
                    const Nodecl::FunctionCode& enclosing_function);
            static void finalize_analysis();

            VectorizationAnalysisInterface(const Nodecl::NodeclBase& n,
                    Analysis::WhichAnalysis analysis_mask,
                    Analysis::WhereAnalysis nested_analysis_mask,
                    int nesting_level);

            ~VectorizationAnalysisInterface();

            virtual bool is_constant(const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n) __attribute__((deprecated));
            virtual bool has_been_defined( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& s );

            virtual bool is_induction_variable( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool contains_induction_variable(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual bool is_basic_induction_variable( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_non_reduction_basic_induction_variable(
                    const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n );

            virtual Nodecl::NodeclBase get_induction_variable_lower_bound(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual Nodecl::NodeclBase get_induction_variable_upper_bound(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual Nodecl::NodeclBase get_induction_variable_increment(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual objlist_nodecl_t get_induction_variable_increment_list(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_induction_variable_increment_one(
                    const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n );
            virtual ObjectList<Analysis::Utils::InductionVariableData*> get_induction_variables(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );

            virtual objlist_nodecl_t get_ivs_nodecls(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_adjacent_access( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_induction_variable_dependent_expression(
                    const Nodecl::NodeclBase& ivs_scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_constant_access( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_simd_aligned_access( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n,
                    const std::map<TL::Symbol, int>& aligned_expressions,
                    const objlist_nodecl_t& suitable_expressions,
                    int unroll_factor, int alignment );
            virtual bool is_suitable_expression( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n,
                    const objlist_nodecl_t& suitable_expressions,
                    int unroll_factor, int alignment, int& vector_size_module );
            virtual bool iv_lb_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);
            virtual bool iv_ub_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);
            virtual bool iv_step_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);

            virtual bool nodecl_is_constant_at_statement(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);

            //
            // SIMD-specific methods
            //
            virtual bool is_nested_induction_variable_dependent_access(
                    const VectorizerEnvironment& environment,
                    const Nodecl::NodeclBase& n);
            virtual bool is_nested_non_reduction_basic_induction_variable(
                    const VectorizerEnvironment& environment,
                    const Nodecl::NodeclBase& n);

            friend class VectorizerEnvironment;
            friend class VectorizerLoopInfo;
            friend class VectorizerVisitorFor;
            friend class VectorizerVisitorForEpilog;
            friend class VectorizerVisitorLoopCond;
            friend class VectorizerVisitorLoopNext;
            friend class VectorizerVisitorFunction;
            friend class VectorizerVisitorStatement;
            friend class VectorizerVisitorExpression;
            friend class VectorizerVisitorLoopHeader;
            friend class StrideSplitterVisitor;
    };
}
}

#endif //TL_VECTORIZATION_ANALYSIS_INTERFACE_HPP

