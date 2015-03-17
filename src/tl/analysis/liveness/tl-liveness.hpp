/*--------------------------------------------------------------------
 ( C) Copyright 2006-2014 Barcelona* Supercomputing Center
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

#ifndef TL_LIVENESS_HPP
#define TL_LIVENESS_HPP

#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    // **************************************************************************************************** //
    // ******************************* Class implementing liveness analysis ******************************* //
    
    /*! Bottom-up data-flow analysis to determine the liveness of the variables of a PCFG
     *  The data-flow equations are the following:
     *  - Live In variables for node 'x':
     *      - General case:                 LI(x) = UE(x) U ( LO(x) - KILL(x) )
     *      - x is task:                    LI(x) = UE(x) U ( LO(x) - KILL(x) - Private(x) )
     *  - Live Out variables for node 'x':
     *      - General case:                 LO(x) = U LI(y),
     *                                      where y = all successors of x
     *      - x is a task:                  L0(x) = UE(x) U ( LO(x) - (KILL(x) - Private|Firstprivate(x)) ), 
     */
    class LIBTL_CLASS Liveness
    {
    private:
        ExtensibleGraph* _graph;
        bool _propagate_graph_nodes;

        //! Computes the liveness information of each node regarding only its inner statements
        //! Live In (X) = Upper exposed (X)
        void initialize_live_sets(Node* current);

        //! Computes liveness equations for a given node and calls recursively to its children
        void solve_live_equations_rec(Node* current, bool& changed);
        void solve_task_live_equations_rec(Node* n, bool& changed, Node* task);

        //! Initiates computation of a task liveness sets
        //! Excludes from the exit node LiveOut those variables private to the task
        void solve_specific_live_in_tasks(Node* current);

        //! U(Live In(Y)), for all Y successors of X
        NodeclSet compute_successors_live_in(Node* n);

        //! Propagates liveness information from inner to outer nodes
        void set_graph_node_liveness(Node* current);

    public:
        //! Constructor
        Liveness(ExtensibleGraph* graph, bool propagate_graph_nodes);

        //! Method computing the Liveness information on the member #graph
        void compute_liveness();
    };

    // ***************************** End class implementing liveness analysis ***************************** //
    // **************************************************************************************************** //

}
}

#endif      // TL_LIVENESS_HPP
