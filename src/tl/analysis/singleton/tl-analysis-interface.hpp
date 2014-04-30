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

#include "tl-analysis-static-info.hpp"
#include "tl-analysis-singleton.hpp"

//#include "tl-induction-variables-data.hpp"
//#include "tl-nodecl-visitor.hpp"
//#include "tl-objectlist.hpp"
//#include "tl-omp.hpp"

namespace TL {
namespace Analysis {

    typedef std::map<Nodecl::NodeclBase, Node*> nodecl_to_node_map_t;
    typedef std::pair<Nodecl::NodeclBase, Node*> nodecl_to_node_pair_t;

    class AnalysisInterface : public AnalysisStaticInfo // To be removed
    {
        private:
            nodecl_to_node_map_t _scope_nodecl_to_node_map;     
 
            bool nodecl_has_property_in_scope(
                    Node* const scope_node,
                    Node* const stmt_node,
                    const Nodecl::NodeclBase& n,
                    ExtensibleGraph* const pcfg,
                    const bool consider_control_structures);
       
        protected:
            Node* retrieve_scope_node_from_nodecl(const Nodecl::NodeclBase& scope,
                    ExtensibleGraph* pcfg);
            ExtensibleGraph* retrieve_pcfg_from_func(const Nodecl::NodeclBase& n) const;
/*
            bool nodecl_is_constant_at_statement(
                    Node* const scope_node,
                    Node* const stmt_node,
                    const Nodecl::NodeclBase& n,
                    ExtensibleGraph* const pcfg);
*/
            bool nodecl_is_invariant_in_scope(
                    Node* const scope_node,
                    Node* const stmt_node,
                    const Nodecl::NodeclBase& n,
                    ExtensibleGraph* const pcfg);
 
            bool nodecl_value_is_invariant_in_scope(
                    Node* const scope_node,
                    Node* const stmt_node,
                    const Nodecl::NodeclBase& n,
                    ExtensibleGraph* const pcfg);
 
        public:
            // *** Constructors *** //
            //! Constructor useful to make queries that do not require previous analyses
            AnalysisInterface( );

            AnalysisInterface( const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask,
                               WhereAnalysis nested_analysis_mask, int nesting_level );

            ~AnalysisInterface();


            DEPRECATED bool reach_defs_depend_on_iv(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n);
            /*
            virtual bool nodecl_is_constant_at_statement(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& n); */

            bool nodecl_is_invariant_in_scope(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& stmt,
                    const Nodecl::NodeclBase& n);

            bool nodecl_value_is_invariant_in_scope(
                    const Nodecl::NodeclBase& scope,
                    const Nodecl::NodeclBase& stmt,
                    const Nodecl::NodeclBase& n);
    };
}
}

#endif 
