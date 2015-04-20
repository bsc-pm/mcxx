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

        //! Set of variables determining the limits of the loop
        Utils::InductionVarsPerNode _loop_limits;

        //! Recursive method that actually computes the loop ranges of \_graph
        void compute_loop_ranges_rec(Node* current);

        //! Method to compute the number of iterations of a loop
        void get_loop_limits(Nodecl::NodeclBase cond, int loop_id);

    public:
        //! Constructor
        LoopAnalysis( ExtensibleGraph* graph, Utils::InductionVarsPerNode ivs );

        void compute_loop_ranges( );

        friend class StaticAnalysis;
    };
}
}

#endif      // TL_LOOP_ANALYSIS_HPP
