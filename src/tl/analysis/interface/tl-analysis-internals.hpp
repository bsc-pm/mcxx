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

#ifndef TL_ANALYSIS_QUERIES_HPP
#define TL_ANALYSIS_QUERIES_HPP

#include "tl-extensible-graph.hpp"
#include "tl-tribool.hpp"

namespace TL {
namespace Analysis {

    /*
     *  PROPERTIES
     */

    TL::tribool invariant_property( Node* const scope_node,
            Node* const stmt_node, const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes);

    /*
     *  QUERIES
     */
    
    bool is_invariant_internal(Node* const scope_node, Node* const stmt_node,
            Node* const n_node, const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes = std::set<Nodecl::NodeclBase>());

    bool has_been_defined_internal(Node* const n_node,
            const Nodecl::NodeclBase& n,
            const std::set<TL::Symbol>& global_variables);
 
    // IVS
    bool is_iv_internal(Node* const scope_node, const Nodecl::NodeclBase& n);
    bool is_non_reduction_basic_iv_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);
    Nodecl::NodeclBase get_iv_lower_bound_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);
    Nodecl::NodeclBase get_iv_increment_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);

    // Generic queries
    template <typename PropertyFunctor>
    TL::tribool reach_defs_have_property_in_scope(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const original_n,
            const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            PropertyFunctor property_functor,
            std::set<Nodecl::NodeclBase> visited_nodes);

    template <typename PropertyFunctor>
    TL::tribool nodecl_has_property_in_scope(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const original_n,
            const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            PropertyFunctor property_functor,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        // Add node to visited
        if (!visited_nodes.insert(n).second)
        {
            internal_error("Node already visited", 0);
        }

        // Check if n satisfy base cases
        TL::tribool t = property_functor(scope_node, stmt_node, n, pcfg,
                visited_nodes);

        if (!t.is_unknown())
        {
            // if true return true, else return false
            return t.is_true();
        }

        // If 'n' is not a base case, then study its RDs
        TL::tribool result = reach_defs_have_property_in_scope(
                scope_node, stmt_node, original_n, n, pcfg,
                property_functor, visited_nodes);

        // Remove node from visited
        if (visited_nodes.erase(n) != 1)
        {
            internal_error("Erase node", 0);
        }

        return result;
    }

    template <typename PropertyFunctor>
    TL::tribool reach_defs_have_property_in_scope(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const original_n,
            const Nodecl::NodeclBase& n,
            ExtensibleGraph* const pcfg,
            PropertyFunctor property_functor,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        Utils::ext_sym_map all_reach_defs_in = stmt_node->get_reaching_definitions_in();

        // Get all memory accesses and study their RDs 
        // Note that we want all memory access, not only the symbols.
        // Example: a[i]
        // retrieving all symbols will return: a, i
        // retrieving all memory accesses will return: a, i, a[i]
        const ObjectList<Nodecl::NodeclBase> n_mem_accesses = Nodecl::Utils::get_all_memory_accesses(n);
 
        for(ObjectList<Nodecl::NodeclBase>::const_iterator n_ma_it =
                n_mem_accesses.begin(); 
                n_ma_it != n_mem_accesses.end();
                n_ma_it++)
        {
            //std::cerr << "   Mem access: " << n_ma_it->prettyprint() << " - " 
            //    << nodecl_get_ast(n_ma_it->get_internal_nodecl()) << std::endl;

            Utils::ExtendedSymbol n_ma_es(*n_ma_it);
            if(all_reach_defs_in.find(n_ma_es) == all_reach_defs_in.end())
            {
                if(n_ma_it->is<Nodecl::ArraySubscript>() || n_ma_it->is<Nodecl::ClassMemberAccess>())
                {   // For sub-objects, if no reaching definition arrives, then we assume it is Undefined
                    continue;
                }
                else
                {
                    WARNING_MESSAGE("No reaching definition arrives for nodecl %s.\n", 
                                    n_ma_it->prettyprint().c_str());
                }
            }

            std::pair<Utils::ext_sym_map::iterator, Utils::ext_sym_map::iterator> bounds =
                all_reach_defs_in.equal_range(*n_ma_it);

            // REACHING DEFINITIONS
            for(Utils::ext_sym_map::iterator rd_it = bounds.first;
                rd_it != bounds.second;
                rd_it++)
            {
                // Skip Unknown RDs
                if(!rd_it->second.first.is<Nodecl::Unknown>())
                {
                    // Get the PCFG nodes where the reaching definitions where produced
                    const Nodecl::NodeclBase& reach_def_nodecl = 
                        rd_it->second.second.is_null() ? rd_it->second.first : rd_it->second.second;

                    // std::cerr << "      RD of " << n.prettyprint() <<": " << stmt_reach_def.prettyprint() << " - " 
                    //    << nodecl_get_ast(stmt_reach_def.get_internal_nodecl()) << std::endl << std::endl;

                    // Skip visided nodes. Recursive RD (IV step)
                    if (visited_nodes.find(reach_def_nodecl) == visited_nodes.end())
                    {
                        Node* reach_defs_node = pcfg->find_nodecl_pointer(reach_def_nodecl);

                        // Visit and check property in current RD
                        TL::tribool reach_def_property =
                            nodecl_has_property_in_scope(scope_node, reach_defs_node, 
                                    original_n, reach_def_nodecl, pcfg, property_functor,
                                    visited_nodes);

                        //TODO: Combine all results with a parametrized operator (OR / AND)
                        // If property is false, we don't need to continue
                        if (reach_def_property.is_false())
                            return reach_def_property;

                        // CONDITIONAL NODES
                        Node* control_structure = 
                            ExtensibleGraph::get_enclosing_control_structure(reach_defs_node);

                        if((control_structure != NULL) && 
                                !(control_structure->is_loop_node() &&
                                    ExtensibleGraph::node_contains_node(control_structure, original_n)))
                                // If the original node (not any RD) is enclosed in the loop,
                                // the condition of the loop doesn't define the value of that
                                // node inside the loop
                        {
                            Node* cond_node = control_structure->get_condition_node();
                            ObjectList<Nodecl::NodeclBase> cond_node_stmts = cond_node->get_statements();

                            // if cond_node == stmt_node means that we are in a loop asking for the condition,
                            // let say j < 10. We get the RD of 'j' and then we get the conditon node of them,
                            // which is again the j < 10.
                            //if (cond_node == stmt_node)
                            //    continue;

                            Node* cond_outer_node = cond_node->get_outer_node();

                            // If it's a loop node and ORIGINAL node (not RD) is enclosed
                            // we don't have to take into account the condition since it's not
                            // defining the value of the ORIGINAL node inside the loop.
                            if(!(cond_node->is_loop_node() && 
                                        original_n->node_is_enclosed_by(cond_outer_node)))
                            {
                                // Sara? Will be there more than one statement here? If so, the previous condition
                                // will have to be more sophisticated
                                ERROR_CONDITION(cond_node_stmts.size() > 1, "More than one cond_statement", 0);

                                for(ObjectList<Nodecl::NodeclBase>::const_iterator it = cond_node_stmts.begin(); 
                                        it != cond_node_stmts.end(); 
                                        ++it)
                                {
                                    const ObjectList<Nodecl::NodeclBase> stms_mem_accesses = 
                                        Nodecl::Utils::get_all_memory_accesses(*it);
                                    for(ObjectList<Nodecl::NodeclBase>::const_iterator itt = stms_mem_accesses.begin();
                                            itt != stms_mem_accesses.end();
                                            ++itt)
                                    {
                                        // Skip recursive RD 
                                        if (visited_nodes.find(*itt) == visited_nodes.end())
                                        {
                                            TL::tribool cond_stmt_ma_property = 
                                                nodecl_has_property_in_scope(scope_node,
                                                        cond_node, original_n, *itt, pcfg, 
                                                        property_functor, visited_nodes);

                                            if(cond_stmt_ma_property.is_false())
                                                return cond_stmt_ma_property;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        return true;
    }
}
}
#endif
