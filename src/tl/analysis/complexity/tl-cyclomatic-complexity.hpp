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



#ifndef TL_CYCLOMATIC_COMPLEXITY_HPP
#define TL_CYCLOMATIC_COMPLEXITY_HPP

#include "tl-extensible-graph.hpp"
#include "tl-nodecl-visitor.hpp"

namespace TL {
namespace Analysis {
    
    class LIBTL_CLASS CyclomaticComplexity {
    private:
        // *** Class members *** //
        ExtensibleGraph* _pcfg;
        unsigned int _num_edges;
        unsigned int _num_nodes;
        unsigned int _num_exits;
        
        // *** Class private member functions *** //
        void compute_cyclomatic_complexity_rec(Node* n, bool in_split_node);
        
    public:
        // *** Constructor *** //
        CyclomaticComplexity(ExtensibleGraph* pcfg);
        
        /*! The cyclomatic complexity of a code is the count of the number of linearly independent paths
         * through the code. Mathematically, it is defined with reference to the Control Flow Graph.
         * The complexity M is then defined as M = E - N + 2P, where:
         *     E is the number of edges of the graph
         *     N is the number of nodes of the graph
         *     P is the number of connected components (exit nodes)
         */
        unsigned int compute_cyclomatic_complexity();
    };
    
}
}

#endif      // TL_CYCLOMATIC_COMPLEXITY_HPP