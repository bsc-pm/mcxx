/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
#include "tl-analysis-interface.hpp"
#include "tl-vectorization-common.hpp"
#include "tl-nodecl-base.hpp"
#include "tl-nodecl-utils.hpp"

#include <list>

namespace TL
{
namespace Vectorization
{
    class VectorizationAnalysisCopyMaps
    {
        protected:
            Nodecl::Utils::NodeclDeepCopyMap _orig_to_copy_nodes;
            Nodecl::Utils::NodeclDeepCopyMap _copy_to_orig_nodes;

            Nodecl::Utils::SymbolDeepCopyMap _orig_to_copy_symbols;
            Nodecl::Utils::SymbolDeepCopyMap _copy_to_orig_symbols;

            std::list<Nodecl::NodeclBase> _registered_nodes;
    };

    typedef std::map<Nodecl::NodeclBase, bool> map_node_bool_t;
    typedef std::pair<Nodecl::NodeclBase, bool> pair_node_bool_t;
    typedef std::map<Nodecl::NodeclBase, std::pair<bool, int> > map_node_boolint_t;
    typedef std::pair<bool, int> pair_bool_int_t;
    typedef std::pair<Nodecl::NodeclBase, pair_bool_int_t> pair_node_boolint_t;

    struct VectorizationAnalysisInfo
    {
        map_node_bool_t uniform_nodes;
        map_node_bool_t linear_nodes;
        map_node_bool_t ivs_nodes;
        map_node_bool_t non_red_iv_nodes;
        map_node_bool_t adjacent_nodes;
        map_node_boolint_t simd_aligned_nodes;
    };

    typedef std::map<Nodecl::NodeclBase,
            VectorizationAnalysisInfo> map_scope_analysis_info_t;
    typedef std::pair<Nodecl::NodeclBase,
            VectorizationAnalysisInfo> pair_scope_analysis_info_t;

    class VectorizationAnalysisInterface : public VectorizationAnalysisCopyMaps,
                                           public Analysis::AnalysisInterface
    {
        private:
   
            Nodecl::NodeclBase _original_node;
            map_scope_analysis_info_t _scope_analysis_info;
 
            Nodecl::FunctionCode copy_function_code(const Nodecl::NodeclBase& n);

            Nodecl::NodeclBase translate_input(
                    const Nodecl::NodeclBase& n);
            objlist_nodecl_t translate_input(
                    const objlist_nodecl_t& list);
            TL::Symbol translate_input(
                    const TL::Symbol& n) const;
            objlist_tlsym_t translate_input(
                    const objlist_tlsym_t& n) const;
            std::map<TL::Symbol, int> translate_input(
                    const std::map<TL::Symbol, int>& map);

            void register_copy_base(
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& n_copy);

            Nodecl::NodeclBase translate_output(const Nodecl::NodeclBase& n);
            objlist_nodecl_t translate_output(const objlist_nodecl_t& list);
            TL::Symbol translate_output(const TL::Symbol& n) const;

        public:
            VectorizationAnalysisInterface(const Nodecl::NodeclBase& n,
                    const Analysis::WhichAnalysis analysis_mask);
            virtual ~VectorizationAnalysisInterface();

            virtual bool is_uniform(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& stmt,
                    const Nodecl::NodeclBase& n);

            virtual bool is_linear(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            
            Nodecl::NodeclBase get_linear_step(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
 
            objlist_nodecl_t get_linear_nodecls(
                    const Nodecl::NodeclBase& n );

            virtual bool has_been_defined(const Nodecl::NodeclBase& n);
 
            // IVS 
            virtual bool is_induction_variable( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
//            DEPRECATED bool is_non_reduction_basic_induction_variable(
//                    const Nodecl::NodeclBase& scope,
//                    const Nodecl::NodeclBase& n );
            Nodecl::NodeclBase get_induction_variable_lower_bound(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
//            DEPRECATED Nodecl::NodeclBase get_induction_variable_increment(
//                    const Nodecl::NodeclBase& scope,
//                    const Nodecl::NodeclBase& n );
//            DEPRECATED objlist_nodecl_t get_ivs_nodecls(
//                    const Nodecl::NodeclBase& n );

            //
            // SIMD-specific methods
            //
            virtual bool is_adjacent_access(const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual bool is_simd_aligned_access(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n,
                    const map_nodecl_int_t& aligned_expressions,
                    const objlist_nodecl_t& suitable_expressions,
                    int vec_factor, int alignment,
                    int& alignment_module);
            virtual int get_assume_aligned_attribute(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::Symbol& n);

            virtual bool is_suitable_expression(
                const Nodecl::NodeclBase &scope,
                const Nodecl::NodeclBase &n,
                const objlist_nodecl_t &suitable_expressions,
                unsigned int suitable_factor,
                unsigned int vec_factor,
                int &suitable_module);

            virtual void register_identical_copy(
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& n_copy);

            virtual Nodecl::NodeclBase shallow_copy(
                    const Nodecl::NodeclBase& n);

            virtual Nodecl::NodeclBase deep_copy(
                    const Nodecl::NodeclBase& n,
                    TL::ReferenceScope ref_scope);

/*
            DEPRECATED virtual bool is_nested_induction_variable_dependent_access(
                    const VectorizerEnvironment& environment,
                    const Nodecl::NodeclBase& n);
            DEPRECATED virtual bool is_nested_non_reduction_basic_induction_variable(
                    const VectorizerEnvironment& environment,
                    const Nodecl::NodeclBase& n);

            DEPRECATED virtual bool iv_lb_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);
            DEPRECATED virtual bool iv_ub_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);
            DEPRECATED virtual bool iv_step_depends_on_ivs_from_scope(
                    const Nodecl::NodeclBase& n_scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& ivs_scope);

            virtual bool nodecl_value_is_uniform_in_scope(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& stmt,
                    const Nodecl::NodeclBase& n);

            Nodecl::Utils::NodeclDeepCopyMap::iterator find_equal_nodecl(
                    const Nodecl::NodeclBase& n,
                    Nodecl::Utils::NodeclDeepCopyMap& map);
            Nodecl::NodeclBase get_translated_copy(const Nodecl::NodeclBase& n);

            void register_node(const Nodecl::NodeclBase& n);
            void unregister_node(const Nodecl::NodeclBase& n);
            void unregister_nodes();

            virtual bool is_constant(const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n) __attribute__((deprecated));
            virtual bool has_been_defined( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n,
                    const Nodecl::NodeclBase& s );

*/

/*
            virtual bool contains_induction_variable(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual bool is_basic_induction_variable( const Nodecl::NodeclBase& scope,
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
            virtual bool is_induction_variable_dependent_expression(
                    const Nodecl::NodeclBase& ivs_scope,
                    const Nodecl::NodeclBase& n );
            virtual bool is_constant_access( const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n );
                   */
    };
}
}

#endif //TL_VECTORIZATION_ANALYSIS_INTERFACE_HPP

