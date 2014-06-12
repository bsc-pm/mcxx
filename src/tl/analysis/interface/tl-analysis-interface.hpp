/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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

#ifndef TL_ANALYSIS_INTERFACE_HPP
#define TL_ANALYSIS_INTERFACE_HPP

#include "tl-analysis-singleton.hpp"

#include "tl-tribool.hpp"
#include "tl-omp.hpp"
#include <set>

//#include "tl-induction-variables-data.hpp"
//#include "tl-nodecl-visitor.hpp"
//#include "tl-objectlist.hpp"
//#include "tl-omp.hpp"

namespace TL {
namespace Analysis {

    // ********************************************************************************************* //
    // ********************** Class to define which analysis are to be done ************************ //

    struct WhichAnalysis
    {
        // Macros defining the analysis to be computed
        enum Analysis_tag
        {
            PCFG_ANALYSIS           = 1u << 1,
            USAGE_ANALYSIS          = 1u << 2,
            LIVENESS_ANALYSIS       = 1u << 3,
            REACHING_DEFS_ANALYSIS  = 1u << 4,
            INDUCTION_VARS_ANALYSIS = 1u << 5,
            CONSTANTS_ANALYSIS      = 1u << 6,
            AUTO_SCOPING            = 1u << 7
        } _which_analysis;

        WhichAnalysis( Analysis_tag a );
        WhichAnalysis( int a );
        WhichAnalysis operator|( WhichAnalysis a );
    };

    // ******************** END class to define which analysis are to be done ********************** //
    // ********************************************************************************************* //

    typedef std::map<Nodecl::NodeclBase, ExtensibleGraph*> nodecl_to_pcfg_map_t;

    typedef std::map<Nodecl::NodeclBase, Node*> nodecl_to_node_map_t;
    typedef std::pair<Nodecl::NodeclBase, Node*> nodecl_to_node_pair_t;

    class AnalysisInterface 
    {
        private:
            nodecl_to_pcfg_map_t _func_to_pcfg_map;
            nodecl_to_node_map_t _scope_nodecl_to_node_map;     
 
        protected:
            Node* retrieve_scope_node_from_nodecl(const Nodecl::NodeclBase& scope,
                    ExtensibleGraph* pcfg);
            ExtensibleGraph* retrieve_pcfg_from_func(const Nodecl::NodeclBase& n) const;

        public:
            // *** Constructors *** //
            //! Constructor useful to make queries that do not require previous analyses

            AnalysisInterface( );

            AnalysisInterface( const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask, bool ompss_mode_enabled );

            ~AnalysisInterface();

            virtual bool is_uniform(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& stmt,
                    const Nodecl::NodeclBase& n);

            virtual bool has_been_defined(const Nodecl::NodeclBase& n);

            virtual bool is_induction_variable(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual bool is_non_reduction_basic_induction_variable(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual Nodecl::NodeclBase get_induction_variable_lower_bound(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            virtual Nodecl::NodeclBase get_induction_variable_increment(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            Utils::InductionVarList get_induction_variables(
                    const Nodecl::NodeclBase& scope);

            // *** Queries about Auto-Scoping *** //

//            virtual void print_auto_scoping_results( const Nodecl::NodeclBase& scope );
//            virtual Utils::AutoScopedVariables get_auto_scoped_variables( const Nodecl::NodeclBase scope );


            bool is_ompss_reduction( const Nodecl::NodeclBase& n,
                    RefPtr<OpenMP::FunctionTaskSet> function_tasks ) const;

            /*
            DEPRECATED bool reach_defs_depend_on_iv(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            */
            /*
            virtual bool nodecl_is_constant_at_statement(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n); */
    };
}
}

#endif 
