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

#ifndef TL_ANALYSIS_QUERIES_HPP
#define TL_ANALYSIS_QUERIES_HPP

#include "tl-extensible-graph.hpp"
#include "tl-tribool.hpp"

namespace TL {
namespace Analysis {

    /*
     *  PROPERTIES
     */

    TL::tribool uniform_property( Node* const scope_node,
            Node* const stmt_node, 
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& prev_n, 
            ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes);

    /*
     *  QUERIES
     */
    int get_assume_aligned_attribute_internal(
            Node* const stmt_node,
            const Nodecl::Symbol& n);

    bool is_uniform_internal(Node* const scope_node, Node* const stmt_node,
            const Nodecl::NodeclBase& n, ExtensibleGraph* const pcfg,
            std::set<Nodecl::NodeclBase> visited_nodes = std::set<Nodecl::NodeclBase>());

    bool is_linear_internal(
            Node* const scope_node, 
            const Nodecl::NodeclBase& n);
    Utils::InductionVarList get_linear_variables_internal(
            Node* const scope_node);
    NodeclSet get_linear_variable_lower_bound_internal(
            Node* const scope_node,
            const Nodecl::NodeclBase& n);
    NBase get_linear_variable_increment_internal(
            Node* const scope_node,
            const Nodecl::NodeclBase& n);
    
    bool has_been_defined_internal(Node* const n_node,
            const Nodecl::NodeclBase& n,
            const NodeclSet& global_variables);
 
    // IVS
    bool is_iv_internal(Node* const scope_node, const Nodecl::NodeclBase& n);
    bool is_non_reduction_basic_iv_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);
    NodeclSet get_iv_lower_bound_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);
    Nodecl::NodeclBase get_iv_increment_internal(Node* const scope_node,
            const Nodecl::NodeclBase& n);

    // Generic queries
    template <typename PropertyFunctor>
    TL::tribool reach_defs_have_property_in_scope(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const original_stmt,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& prev_n,
            ExtensibleGraph* const pcfg,
            PropertyFunctor property_functor,
            std::set<Nodecl::NodeclBase> visited_nodes);

//#define DEBUG_PROPERTY
    template <typename PropertyFunctor>
    TL::tribool nodecl_has_property_in_scope(
            Node* const scope_node,
            Node* const stmt_node,
            Node* const original_stmt,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& prev_n,
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
        TL::tribool t = property_functor(scope_node, 
                stmt_node, n, prev_n,
                pcfg, visited_nodes);

        if (!t.is_unknown())
        {
            // if true return true, else return false
            return t.is_true();
        }

        // If 'n' is not a base case, then study its RDs
        TL::tribool result = reach_defs_have_property_in_scope(
                scope_node, stmt_node, original_stmt, n, prev_n,
                pcfg, property_functor, visited_nodes);

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
            Node* const original_stmt,
            const Nodecl::NodeclBase& n,
            const Nodecl::NodeclBase& prev_n,
            ExtensibleGraph* const pcfg,
            PropertyFunctor property_functor,
            std::set<Nodecl::NodeclBase> visited_nodes)
    {
        NodeclMap all_reach_defs;
        const NodeclSet& killed_vars = stmt_node->get_killed_vars();

        // If n is being defined (killed) in this stmt, we don't look into their RD in
        // We study their RD out (LHS) instead.
        if (Utils::nodecl_set_contains_nodecl_pointer(n, killed_vars))
        {

#ifdef DEBUG_PROPERTY
            std::cerr << "   " << n.prettyprint() << 
                " is killed! Look into the RD OUT"
                << std::endl;
#endif
            all_reach_defs = stmt_node->get_reaching_definitions_out();
        }
        else
        {
            all_reach_defs = stmt_node->get_reaching_definitions_in();
        }

        // Get all memory accesses and study their RDs 
        // Note that we want all memory access, not only the symbols.
        // Example: a[i]
        // retrieving all symbols will return: a, i
        // retrieving all memory accesses will return: a, i, a[i]
        const NodeclList n_mem_accesses = Nodecl::Utils::get_all_memory_accesses(n);
 
        for(NodeclList::const_iterator n_ma_it =
                n_mem_accesses.begin(); 
                n_ma_it != n_mem_accesses.end();
                n_ma_it++)
        {
            const Nodecl::NodeclBase& n_ma = n_ma_it->no_conv();

#ifdef DEBUG_PROPERTY
            std::string parent_str = "NULL";

            if (!n_ma.get_parent().is_null())
            {
                Nodecl::NodeclBase parent_node = n_ma.get_parent();
                if (parent_node.is<Nodecl::Conversion>())
                    parent_str = parent_node.get_parent().prettyprint();
                else
                    parent_str = parent_node.prettyprint();
            }
            
            std::cerr << "   Mem access: " << n_ma.prettyprint() 
                << " --> Parent: " << parent_str 
                << " . Original node: " << original_stmt->get_id() 
                << std::endl;
#endif
            if(all_reach_defs.find(n_ma) == all_reach_defs.end())
            {
                if(n_ma.is<Nodecl::ArraySubscript>() || n_ma.is<Nodecl::ClassMemberAccess>() ||
                        (n_ma.is<Nodecl::Symbol>() && n_ma.get_type().no_ref().is_array()))
                {   // For sub-objects, if no reaching definition arrives, then we assume it is Undefined
                    continue;
                }
                else
                {
                    WARNING_MESSAGE("No reaching definition arrives for nodecl %s.\n", 
                                    n_ma.prettyprint().c_str());
                }
            }

            std::pair<NodeclMap::iterator, NodeclMap::iterator> bounds =
                all_reach_defs.equal_range(n_ma);

            // REACHING DEFINITIONS
            for(NodeclMap::iterator rd_it = bounds.first;
                rd_it != bounds.second;
                rd_it++)
            {
                // Get the PCFG nodes where the reaching definitions where produced
                const Nodecl::NodeclBase& reach_def_nodecl = 
                    rd_it->second.second.is_null() ? rd_it->second.first : rd_it->second.second;

#ifdef DEBUG_PROPERTY
                std::cerr << "      RD: " << reach_def_nodecl.prettyprint() << std::endl;
#endif
                // Skip visided nodes. Recursive RD (IV step)
                if (visited_nodes.find(reach_def_nodecl) == visited_nodes.end())
                {
#ifdef DEBUG_PROPERTY
                    std::cerr << "           Visiting it" << std::endl;
#endif
                    Node* reach_defs_node = NULL;
                    bool unknown_rd = reach_def_nodecl.is<Nodecl::Unknown>();

                    if (!unknown_rd)
                        reach_defs_node = pcfg->find_nodecl_pointer(reach_def_nodecl);

                    // Visit and check property in current RD
                    TL::tribool reach_def_property =
                        nodecl_has_property_in_scope(scope_node, reach_defs_node, 
                                original_stmt, reach_def_nodecl, n_ma, pcfg, property_functor,
                                visited_nodes);
#ifdef DEBUG_PROPERTY
                    std::cerr << "       <---End previous visit" << std::endl;
#endif

                    //TODO: Combine all results with a parametrized operator (OR / AND)
                    // If property is false, we don't need to continue
                    if (reach_def_property.is_false())
                    {
#ifdef DEBUG_PROPERTY
                        std::cerr << "          FALSE" << std::endl;
#endif
                        return reach_def_property;
                    }

                    // CONDITIONAL NODES
                    if (!unknown_rd)
                    {
                        Node* control_structure = 
                            ExtensibleGraph::get_enclosing_control_structure(reach_defs_node);

                        if((control_structure != NULL) && 
                                (control_structure != scope_node))
                        {
                           bool control_is_loop_node = control_structure->is_loop_node();
                           bool skip_loop_condition = false;

                           Node* cond_node = control_structure->get_condition_node();

                           if (control_is_loop_node)
                           {
                               if(cond_node == NULL)
                                   internal_error("Conditional node is null", 0);

                               // Skip loop condition only if the
                               // node is contained in the loop
                               skip_loop_condition = 
                                   (cond_node == original_stmt) ||
                                   ExtensibleGraph::node_contains_node(
                                           cond_node,
                                           original_stmt);
#ifdef DEBUG_PROPERTY
                               std::cerr << "          Original " << original_stmt->get_id()
                                  << " is contained in loop cond " << cond_node->get_id()
                                  << "?: " << skip_loop_condition << std::endl;
#endif

                               // Look into the increment of the loop
                               if (!skip_loop_condition)
                               {
                                   ObjectList<Edge*> entries = cond_node->get_entry_edges();

                                   ERROR_CONDITION(entries.size() != 2, 
                                           "Loop Condition with %d entry edges", entries.size());

                                   for(ObjectList<Edge*>::iterator it = entries.begin(); it != entries.end(); ++it)
                                   {
                                       Edge* current_entry = *it;
                                       Node* loop_increment_node = current_entry->get_source();
                                       if(current_entry->is_back_edge() &&
                                               // If there is a context node there is no 
                                               // expression in the loop next
                                               !loop_increment_node->is_context_node() )
                                       {
                                           skip_loop_condition =
                                               ((loop_increment_node == original_stmt) ||
                                               ExtensibleGraph::node_contains_node(
                                                       loop_increment_node,
                                                       original_stmt));
#ifdef DEBUG_PROPERTY
                                           std::cerr << "          Original " << original_stmt->get_id()
                                               << " is contained in loop incr " << loop_increment_node->get_id()
                                               << "?: " << skip_loop_condition << std::endl;
#endif
                                       }
                                   }
                               }

                               // Check if the original node is 
                               // contained in the stmts (context node) of the loop
                               if (!skip_loop_condition)
                               {
                                   ObjectList<Edge*> exits = cond_node->get_exit_edges();
                                   for(ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
                                   {
                                       if((*it)->is_true_edge())
                                       {
                                           Node* loop_stmts_node = (*it)->get_target();
                                           if(loop_stmts_node != NULL &&
                                                   loop_stmts_node->is_context_node())
                                           {
                                               skip_loop_condition = 
                                                   (loop_stmts_node == original_stmt) ||
                                                   ExtensibleGraph::node_contains_node(
                                                           loop_stmts_node,
                                                           original_stmt);
#ifdef DEBUG_PROPERTY
                                               std::cerr << "          Original contained in loop: " 
                                                   << skip_loop_condition << std::endl;
                                           std::cerr << "          Original " << original_stmt->get_id()
                                               << " is contained in loop stmts " << loop_stmts_node->get_id()
                                               << "?: " << skip_loop_condition << std::endl;
#endif
                                           }
                                       }
                                   }
                               }

                               /*
                               // If original node is not contained in the previo
                               if (!loop_stmts_contains_node)
                               {
                                   bool node_is_previous = false;
                                   std::queue<Node*> buffer;
                                   buffer.push(control_structure);
                                   while(!buffer.empty())
                                   {
                                       Node* current = buffer.top();
                                       buffer.pop();

                                       if(current==original_stmt)
                                       {
                                           node_is_previous = true;
                                           break;
                                       }

                                       
                                   }

                                    Node* current = controscope_node
                                    control_structure 
                               }
                               */
 
                           }

                           if (!control_is_loop_node || 
                                   (control_is_loop_node && !skip_loop_condition))
                           { 
                               // If the loop contains BreakStatements, the property is false
                               // TODO:: Move this to properties?
                               // TODO:: ReturnStatement?
                               if (Nodecl::Utils::nodecl_contains_nodecl_of_kind
                                       <Nodecl::BreakStatement>(
                                           control_structure->get_graph_related_ast()))
                                   return false;

                               // If the original node (not any RD) is enclosed in the loop,
                               // the condition of the loop doesn't define the value of that
                               // node inside the loop
                               ObjectList<Nodecl::NodeclBase> cond_node_stmts = cond_node->get_statements();

                               // if cond_node == stmt_node means that we are in a loop asking for the condition,
                               // let say j < 10. We get the RD of 'j' and then we get the conditon node of them,
                               // which is again the j < 10.
                               //    if (ExtensibleGraph::node_contains_node(cond_node, original_stmt)
                               //          || (cond_node == original_stmt))
                               //          continue;

                               // Sara? Will be there more than one statement here? If so, the previous condition
                               // will have to be more sophisticated
                               ERROR_CONDITION(cond_node_stmts.size() > 1, "More than one cond_statement", 0);


#ifdef DEBUG_PROPERTY
                               std::cerr << "          Visiting condition'" << std::endl;
#endif

                               for(ObjectList<Nodecl::NodeclBase>::const_iterator it = cond_node_stmts.begin(); 
                                       it != cond_node_stmts.end(); 
                                       ++it)
                               {
#ifdef DEBUG_PROPERTY
                                   std::cerr << "              stmt: '" << it->prettyprint() << std::endl;
#endif
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
                                                       cond_node, original_stmt, *itt, n_ma, pcfg, 
                                                       property_functor, visited_nodes);

                                           if(cond_stmt_ma_property.is_false())
                                               return cond_stmt_ma_property;
                                       }
                                   }
                               }
                           }

#ifdef DEBUG_PROPERTY
                           else
                           {
                               if (control_structure->is_loop_node())
                                   std::cerr << "          control_structure is LOOP:"
                                       << control_structure->get_id() << std::endl;

                               if (ExtensibleGraph::node_contains_node(control_structure, original_stmt))
                                   std::cerr << "          control_structure contains ORIGINAL STMT:"
                                       << original_stmt->get_id() << std::endl;
                               else
                                   std::cerr << "          control_structure DON'T contains ORIGINAL STMT:"
                                       << original_stmt->get_id() << std::endl;
                           }
#endif
                        }
#ifdef DEBUG_PROPERTY
                        else
                        {
                            if(control_structure == NULL)
                                std::cerr << "          control_structure is NULL" << std::endl;
                            else
                            {
                                if (control_structure == scope_node)
                                    std::cerr << "          control_structure is SCOPE_NODE: " 
                                        << control_structure->get_id() << std::endl;
                            }
                        }
#endif
                    }
                }
#ifdef DEBUG_PROPERTY                
                else
                {
                    std::cerr << "          Already visited" << std::endl
                        << std::endl;
                }
#endif
            }
        }
        
        return true;
    }
}
}
#endif
