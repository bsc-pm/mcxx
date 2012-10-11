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

#ifndef TL_LOOP_ANALYSIS_HPP
#define TL_LOOP_ANALYSIS_HPP

#include "tl-nodecl.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-iv-analysis.hpp"
#include "tl-node.hpp"
#include "tl-symbol.hpp"

namespace TL {
namespace Analysis {

    //! Class implementing Loop Analysis
    class LIBTL_CLASS LoopAnalysis
    {
    private:
        //! Graph which loop analysis we are performing of
        ExtensibleGraph* _graph;

        //! Set of induction variables found for the current graph
        Utils::InductionVarsPerNode _induction_vars;

        //! Recursive method that actually computes the loop ranges of \_graph
        void compute_loop_ranges_rec( Node* current );



        // *** Private methods *** //
//         void traverse_loop_init( Node* loop_node, Nodecl::NodeclBase init );
//         void traverse_loop_cond( Node* loop_node, Nodecl::NodeclBase cond );
//         void traverse_loop_stride( Node* loop_node, Nodecl::NodeclBase stride );

//         void compute_ranges_for_variables_in_loop( Node* node, Node* loop_node );

//         Nodecl::NodeclBase set_access_range( Node* node, Node* loop_node, const char use_type, Nodecl::NodeclBase nodecl,
//                                              std::map<Symbol, Nodecl::NodeclBase> ind_var_map,
//                                              Nodecl::NodeclBase reach_def_var = Nodecl::NodeclBase::null( ) );

        /*!
        * Looks for any induction variable contained in a list of nodecls and
        * substitutes its scalar access by the corresponding ranged access computed from the loop boundaries
        * \param node Node of the graph we are analysing right now
        * \param nodecl_l list containing the potential nodecls where we want to substitute an scalar by a range
        * \param use_type kind of the list we are analysing; it can be a UpperExposed list, a Killed list, a Undefined_behaviour list
        *                 or a ReachingDefintions list
        */
//         void set_access_range_in_ext_sym_set( Node* node, Node* loop_node, Utils::ext_sym_set nodecl_l, const char use_type );
        /*!
        * Wrapping method for #set_access_range_in_ext_sym_list in the case we traverse a nodecl map container
        */
//         void set_access_range_in_nodecl_map( Node* node, Node* loop_node, nodecl_map nodecl_m );


    public:

        //! Constructor
        LoopAnalysis( ExtensibleGraph* graph, Utils::InductionVarsPerNode ivs );

        void compute_loop_ranges( );

        // *** Modifiers *** //
        /*!
        * Calculates the induction variables from a loop control
        * \param loop_node node of the graph containing a ForStatement graph node
        */
//         void compute_induction_variables_info( Node* loop_node );

        /*!
        * Looks for loops in the graph hanging up from a node and
        * computes the ranges for induction variables of each loop
        * \param node Node form the graph used to start the up-bottom analysis
        */
//         void compute_ranges_for_variables( Node* node );

        friend class StaticAnalysis;
    };
}
}

#endif      // TL_LOOP_ANALYSIS_HPP