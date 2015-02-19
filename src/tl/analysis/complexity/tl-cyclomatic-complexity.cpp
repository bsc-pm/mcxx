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



#include "tl-cyclomatic-complexity.hpp"

namespace TL {
namespace Analysis {

    CyclomaticComplexity::CyclomaticComplexity(ExtensibleGraph* pcfg)
        : _pcfg(pcfg), _num_edges(0), _num_nodes(0), _num_exits(0)
    {}

    void CyclomaticComplexity::compute_cyclomatic_complexity_rec(Node* n, bool in_split_node)
    {
        if(n->is_visited())
            return;
        
        n->set_visited(true);
        if(n->is_graph_node())
        {
            // When traversing a split_node, add a new node for each inner graph node
            if(in_split_node && !n->is_context_node())
                _num_nodes++;
            
            bool inner_in_split_node = in_split_node;
            if(n->is_split_statement())
                inner_in_split_node = true;
            else
                compute_cyclomatic_complexity_rec(n->get_graph_entry_node(), inner_in_split_node);
        }
        else
        {   
            if(!n->is_entry_node() && !n->is_exit_node() && !in_split_node)
            {   // If this is neither a graph node nor an entry|exit node
                // then it is a Basic Bloc => increment the number of nodes
                _num_nodes++;
            }
        }
        
        ObjectList<Node*> children = n->get_children();
        
        // If the node does not have any children, then increment the number of exits of the current graph
        if(children.empty())
            _num_exits++;
        // If this is a basic block, then add the number of edges to the total account
        if(!n->is_graph_node())
            _num_edges += children.size();
        // Keep iterating with the children
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            compute_cyclomatic_complexity_rec(*it, in_split_node);
    }
    
    unsigned int CyclomaticComplexity::compute_cyclomatic_complexity()
    {
        // Make the partial computations while traversing the PCFG
        Node* graph = _pcfg->get_graph();
        compute_cyclomatic_complexity_rec(graph, false);
        ExtensibleGraph::clear_visits(graph);
        
        // Compute the cyclomatic complexity following the formula M = E - N + 2P
        int res = _num_edges - _num_nodes + (2 * _num_exits);
        ERROR_CONDITION(res<0, "Negative number %d computed for the cyclomatic complexity. This cannot happen.\n", res);
        return (unsigned int) res;
    }
}
}