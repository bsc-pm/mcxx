/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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


#include <typeinfo>

#include "tl-cfg-analysis-visitor.hpp"
#include "tl-cfg-renaming-visitor.hpp"
#include "tl-cfg-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"
#include "tl-loop-analysis.hpp"
#include "tl-static-analysis.hpp"


namespace TL
{
    namespace Analysis
    {
        // *** AUTO-SCOPING STRUCT *** //
        
        auto_scope_tag::auto_scope_tag(Node* task, ext_sym_set live_after_task_sched)
            : _task(task), _live_after_task_sched(live_after_task_sched),
              _private_vars(), _firstprivate_vars(), _shared_vars(), _undef_sc_vars()
        {}
        
        Node* auto_scope_tag::get_task()
        {
            return _task;
        }
        
        ext_sym_set auto_scope_tag::get_private_vars()
        {
            return _private_vars;
        }
        
        void auto_scope_tag::set_private_var(ExtensibleSymbol s)
        {
            if (!ext_sym_set_contains_sym(s, _private_vars))
                _private_vars.insert(s);
        }

        ext_sym_set auto_scope_tag::get_firstprivate_vars()
        {
            return _firstprivate_vars;
        }
        
        void auto_scope_tag::set_firstprivate_var(ExtensibleSymbol s)
        {
            if (!ext_sym_set_contains_sym(s, _firstprivate_vars))
                _firstprivate_vars.insert(s);
        }
        
        ext_sym_set auto_scope_tag::get_shared_vars()
        {
            return _shared_vars;
        }
        
        void auto_scope_tag::set_shared_var(ExtensibleSymbol s)
        {
            if (!ext_sym_set_contains_sym(s, _shared_vars))
                _shared_vars.insert(s);
        }
        
        ext_sym_set auto_scope_tag::get_undef_sc_vars()
        {
            return _undef_sc_vars;
        }
         
        void auto_scope_tag::set_undef_sc_var(ExtensibleSymbol s)
        {
            if (!ext_sym_set_contains_sym(s, _undef_sc_vars))
                _undef_sc_vars.insert(s);
        }
         
        bool auto_scope_tag::is_live_after_task_sched(ExtensibleSymbol s)
        {
            if (ext_sym_set_contains_sym(s, _live_after_task_sched))
                return true;
            return false;
        }
        
        
        // *** NODE *** //
        
        void Node::fill_use_def_sets(Nodecl::NodeclBase n, bool defined)
        {
            ExtensibleSymbol s(n);
            if (defined)
            {
                set_killed_var(ExtensibleSymbol(n));
            }
            else
            {
                ext_sym_set killed_vars = get_killed_vars();
                if (!killed_vars.contains(s))
                {
                    set_ue_var(s);
                }
            }
        }   
    
        void Node::fill_use_def_sets(Nodecl::List n_l, bool defined)
        {
            for(Nodecl::List::iterator it = n_l.begin(); it != n_l.end(); ++it)
            {
                fill_use_def_sets(*it, defined);
            }
        }

        /*!
        * This method merges the reaching definitions of a list of nodes
        * All definitions that exist in all parents and are the same for all of them, are included directly in the list.
        * The rest of definitions are included with an initial value of null.
        */
        static nodecl_map intersect_parents_reach_def(ObjectList<nodecl_map> reach_defs_m_l, ObjectList<Edge*> entry_edges)
        {
            nodecl_map result;
            
            if (!reach_defs_m_l.empty())
            {
                // Get the first reach defs map which do not come from a back edge
                // Delete the map form the list in order to do not repeat the search in this node
                nodecl_map non_back_edge_parent;
                ObjectList<nodecl_map>::iterator itrd = reach_defs_m_l.begin();
                ObjectList<Edge*>::iterator ite = entry_edges.begin();
                while(itrd != reach_defs_m_l.end() && (*ite)->is_back_edge())
                {
                    ++itrd; ++ite;
                }
                nodecl_map init_map = *itrd;
                reach_defs_m_l.erase(itrd);
                entry_edges.erase(ite);
                
                // Keep those values that comes from all parents
                for (nodecl_map::iterator it = init_map.begin(); it != init_map.end(); ++it)
                {
                    ite = entry_edges.begin();
                    ObjectList<nodecl_map>::iterator itm = reach_defs_m_l.begin();
                    for (; itm != reach_defs_m_l.end(); ++itm, ++ite)
                    {
                        if ( (*itm).find(it->first) != (*itm).end() )
                        {   // The REACH DEF is defined in 'itm' parent
                            if (!Nodecl::Utils::equal_nodecls((*itm)[it->first], it->second))
                            {   // Values are different
                                if ((*itm)[it->first].is<Nodecl::Range>() || it->second.is<Nodecl::Range>())
                                {   // Some value is a range, we try to mix the values
                                    Nodecl::NodeclBase first = it->first;
                                    Nodecl::NodeclBase second = it->second;
                                    Nodecl::NodeclBase second_ = (*itm)[it->first];
                                    Nodecl::NodeclBase renamed_value = CfgRenamingVisitor::combine_variable_values((*itm)[it->first], it->second);
                                    if (renamed_value.is_null())
                                    {
                                        result[it->first] = Nodecl::NodeclBase::null();
                                        break;
                                    }
                                    init_map[it->first] = renamed_value;
                                }
                                else
                                {   // We don't know how to mix the values
                                    result[it->first] = Nodecl::NodeclBase::null();
                                    break;
                                }
                            }
                        }
                        else
                        {
                            if (!(*ite)->is_back_edge())
                            {   // When the value is not defined but the REACH DEFS list is from a back edge node, we do not take it into account
                                result[it->first] = Nodecl::NodeclBase::null();
                                break;
                            }
                        }
                    }
                    
                    // If we have traversed all the list, then we have to insert the value in the result map
                    if (itm == reach_defs_m_l.end())
                    {
                        result[it->first] = init_map[it->first];
                    }
                }
                
                // The other lists are included when the values don't exist yet in the result map
                for (ObjectList<nodecl_map>::iterator it = reach_defs_m_l.begin(); it != reach_defs_m_l.end(); ++it)
                {
                    for (nodecl_map::iterator itm = it->begin(); itm != it->end(); ++itm)
                    {
                        if (result.find(itm->first) == result.end())
                        {
                            result[itm->first] = Nodecl::NodeclBase::null();
                        }
                    }
                }
            }
            
            return result;
        }

        nodecl_map StaticAnalysis::compute_parents_reach_defs(Node* node)
        {
            ObjectList<Edge*> entry_edges;
            ObjectList<Node*> parents;
            ObjectList<Edge_type> entry_edge_types;
            if (node->get_type() == BASIC_ENTRY_NODE)
            {   // Get info of the outer_graph parents
                Node* outer_node = node->get_outer_node();
                parents = outer_node->get_parents();
                while(parents.size() == 1 && parents[0]->get_type() == BASIC_ENTRY_NODE)
                {   // Advance over outer graphs while our parent is the entry node
                    outer_node = outer_node->get_outer_node();
                    parents = outer_node->get_parents();
                }
                entry_edges = outer_node->get_entry_edges();
                entry_edge_types = outer_node->get_entry_edge_types();
            }
            else
            {
                parents = node->get_parents();
                entry_edges = node->get_entry_edges();
                entry_edge_types = node->get_entry_edge_types();
            }
            
            ObjectList<nodecl_map> reach_defs;
            for (ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {
                nodecl_map parent_reach_defs = (*it)->get_reaching_definitions();
                nodecl_map parent_aux_reach_defs = (*it)->get_auxiliar_reaching_definitions();
                parent_reach_defs.insert(parent_aux_reach_defs.begin(), parent_aux_reach_defs.end());
                reach_defs.append(parent_reach_defs);
            }
        
            return intersect_parents_reach_def(reach_defs, entry_edges);
        }

        static bool is_range(Nodecl::NodeclBase nodecl)
        {
            if (nodecl.is<Nodecl::Symbol>() || nodecl.is<Nodecl::IntegerLiteral>())
            {
                return false;
            }
            else if (nodecl.is<Nodecl::Range>())
            {
                return true;
            }
            else if (nodecl.is<Nodecl::List>())
            {
                bool result = false;
                Nodecl::List aux = nodecl.as<Nodecl::List>();
                for(Nodecl::List::iterator it = aux.begin(); it != aux.end(); ++it)
                {
                    result = result || is_range(*it);
                }
                return result;
            }
            else if (nodecl.is<Nodecl::ArraySubscript>())
            {
                Nodecl::ArraySubscript aux = nodecl.as<Nodecl::ArraySubscript>();
                return (is_range(aux.get_subscripted()) || is_range(aux.get_subscripts()));
            }
            else if (nodecl.is<Nodecl::ClassMemberAccess>())
            {
                Nodecl::ClassMemberAccess aux = nodecl.as<Nodecl::ClassMemberAccess>();
                return (is_range(aux.get_lhs()) || is_range(aux.get_member()));
            }
            else if (nodecl.is<Nodecl::Reference>())
            {
                Nodecl::Reference aux = nodecl.as<Nodecl::Reference>();
                return (is_range(aux.get_rhs()));
            }
            else if (nodecl.is<Nodecl::Derreference>())
            {
                Nodecl::Derreference aux = nodecl.as<Nodecl::Derreference>();
                return (is_range(aux.get_rhs()));
            }
            // Many different expression can be built while the renaming period: so, here we can find any kind of expression
            else if (nodecl.is<Nodecl::Conversion>())
            {
                Nodecl::Conversion aux = nodecl.as<Nodecl::Conversion>();
                return is_range(aux.get_nest());
            }
            else if (nodecl.is<Nodecl::Add>())
            {
                Nodecl::Add aux = nodecl.as<Nodecl::Add>();
                return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
            }
            else if (nodecl.is<Nodecl::Minus>())
            {
                Nodecl::Minus aux = nodecl.as<Nodecl::Minus>();
                return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
            }
            else if (nodecl.is<Nodecl::Mul>())
            {
                Nodecl::Mul aux = nodecl.as<Nodecl::Mul>();
                return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
            }
            else if (nodecl.is<Nodecl::Div>())
            {
                Nodecl::Div aux = nodecl.as<Nodecl::Div>();
                return (is_range(aux.get_lhs()) || is_range(aux.get_rhs()));
            }        
            else
            {
                internal_error("Unexpected node type '%s' while traversing a nodecl embedded in an extensible symbol", 
                            ast_print_node_type(nodecl.get_kind()));
            }
        }

        void StaticAnalysis::propagate_reaching_definitions_to_graph_node(Node* node, std::map<Symbol, Nodecl::NodeclBase> induct_vars,
                                                                        const char* filename, int line)
        {
            if (node->get_type() == GRAPH_NODE)
            {   // When node is a LOOP graph, we have to look for the 'next' node of the loop
                // otherwise, we can keep the value of the exit node
                Node* exit_node = node->get_graph_exit_node();
                if (node->get_graph_type() == LOOP)
                {
                    Node* stride = node->get_stride_node();
                
                    nodecl_map old_reach_defs = node->get_reaching_definitions();
                    nodecl_map stride_reach_defs = stride->get_reaching_definitions();
                    
                    if (old_reach_defs.empty())
                    {   // When the graph node has no reach definition defined, we initially propagate the values of the stride node
                        node->set_reaching_definition_list(stride_reach_defs);
                    }
                    
                    for(nodecl_map::iterator it = stride_reach_defs.begin(); it != stride_reach_defs.end(); ++it)
                    {
                        Nodecl::NodeclBase first = it->first, second = it->second;
                        CfgRenamingVisitor renaming_v(induct_vars, filename, line);
                        renaming_v.set_computing_range_limits(true);
                    
                        // The rhs only must be renamed when it is not accessing an array
                        if (!is_range(first))
                        {
                            ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(it->second);
                            if (!renamed.empty())
                            {
                                node->set_reaching_definition(first, renamed[0]);
                            }
                        }
                    }
                }
                else
                {
                    node->set_data(_REACH_DEFS, exit_node->get_reaching_definitions());
                }
            }
            else
            {
                internal_error("Propagating reaching definitions in node '%d' with type '%s' while "
                                "here it is mandatory a Graph node.\n",
                            node->get_id(), node->get_type_as_string().c_str());
            }
        }

        static void make_permanent_auxiliar_values(Node* node)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);
                
                nodecl_map aux_reach_defs = node->get_auxiliar_reaching_definitions();
                node->set_reaching_definition_list(aux_reach_defs);
            
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    make_permanent_auxiliar_values(*it);
                }
            }
        }

        void StaticAnalysis::propagate_reach_defs_among_nodes(Node* node, bool& changes)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);
                
                Node_type ntype = node->get_type();
                
                if (ntype == GRAPH_NODE)
                {  
                    std::map<Symbol, Nodecl::NodeclBase> induction_vars_m;
                    if (node->get_graph_type() == LOOP)
                    {   // FIXME This case is only for FORstatements, not WHILE or DOWHILE
                        _loop_analysis->propagate_reach_defs_in_for_loop_special_nodes(node);
                        induction_vars_m = _loop_analysis->get_induction_vars_mapping(node);
                    }

                    Node* entry = node->get_graph_entry_node();
                    
                    // Compute recursively info from nodes within the graph node
                    propagate_reach_defs_among_nodes(entry, changes);
                    ExtensibleGraph::clear_visits(entry);
                    make_permanent_auxiliar_values(entry);
                
                    // Extend info to the graph node
                    const char* filename;
                    int line;
                    if (node->get_graph_type() == LOOP)
                    {
                        Nodecl::NodeclBase label = node->get_graph_label();
                        filename = label.get_filename().c_str();
                        line = label.get_line();
                    }
                    propagate_reaching_definitions_to_graph_node(node, induction_vars_m, filename, line);
                }
                
                // Compute reaching parents definitions
                nodecl_map combined_parents_reach_defs = compute_parents_reach_defs(node);
                
                // Propagate parents info to the actual node as auxiliary values
                nodecl_map actual_reach_defs = node->get_reaching_definitions();
                nodecl_map actual_aux_reach_defs = node->get_auxiliar_reaching_definitions();
                actual_reach_defs.insert(actual_aux_reach_defs.begin(), actual_aux_reach_defs.end());
                for(nodecl_map::iterator it = combined_parents_reach_defs.begin(); it != combined_parents_reach_defs.end(); ++it)
                {
                    Nodecl::NodeclBase first = it->first, second = it->second;
                    if (actual_reach_defs.find(it->first) == actual_reach_defs.end())
                    {   // Only if the definition is not performed within the node, we store the parents values
                        node->set_auxiliar_reaching_definition(it->first, it->second);
                        changes = true;
                    }
                }
                
                // Compute info for the children
                ObjectList<Node*> children = node->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    propagate_reach_defs_among_nodes(*it, changes);
                }
            } 
        }
       
        Nodecl::NodeclBase StaticAnalysis::rename_nodecl(Nodecl::NodeclBase nodecl, std::map<Symbol, Nodecl::NodeclBase> rename_map)
        {
            CfgRenamingVisitor renaming_v(rename_map, nodecl.get_filename().c_str(), nodecl.get_line());
            ObjectList<Nodecl::NodeclBase> renamed = renaming_v.walk(nodecl);
            if (!renamed.empty())
            {
                if (renamed.size() == 1)
                {
                    std::cerr << "Renaming performed: " << nodecl.prettyprint() << " --> " << renamed[0].prettyprint() << std::endl;
                    return renamed[0];
                }
                else
                {
                    internal_error("More than one nodecl returned while renaming nodecl ", nodecl.prettyprint().c_str());
                }
            }
            else
            {
                return Nodecl::NodeclBase::null();
            }
        }
        
        /*!
        * This method substitute those reaching definitions based on other values which are known
        * For example:
        *      - node A Reach Defs: i = n;
        *      - node B (dependent on A) Reach Defs: i = i - 1;
        *      - We can compute value on B as: i = n - 1;
        */
        void StaticAnalysis::substitute_reaching_definition_known_values(Node* node)
        {
            if (node->is_visited())
            {
                node->set_visited(true);
            
                if (node->get_type() == GRAPH_NODE)
                {
                    Node* entry = node->get_graph_entry_node();
                    substitute_reaching_definition_known_values(entry);
                    
                    node->set_graph_node_reaching_definitions();
                }
                
                Nodecl::NodeclBase renamed;
                
                // Create the renaming map
                nodecl_map actual_reach_defs = node->get_reaching_definitions();
                std::map<Symbol, Nodecl::NodeclBase> rename_map;
                for (nodecl_map::iterator it = actual_reach_defs.begin(); it != actual_reach_defs.end(); ++it)
                {
                    Nodecl::NodeclBase first = it->first;
                    if (first.is<Nodecl::Symbol>())
                    {
                        rename_map.insert(std::map<Symbol, Nodecl::NodeclBase>::value_type(first.get_symbol(), it->second));
                    }
                    // FIXME More Nodecls can be renamed here: MemberAccess, Pointer to member
                }
                // Rename the reaching definitions which depend in other reaching definitions
                for (nodecl_map::iterator it = actual_reach_defs.begin(); it != actual_reach_defs.end(); ++it)
                {
                    Nodecl::NodeclBase first = it->first, second = it->second;
                    Nodecl::NodeclBase new_first, new_second;
                    if (first.is<Nodecl::Symbol>())
                    {   // Nothing to be renamed in a symbol located in the left hand side
                    }
                    else
                    {
                        new_first = rename_nodecl(first, rename_map);
                        if (!new_first.is_null())
                        {
                            node->rename_reaching_defintion_var(first, new_first);
                            first = new_first;
                        }
                    }
                    new_second = rename_nodecl(second, rename_map);
                    if (!new_second.is_null())
                    {
                        node->set_reaching_definition(first, new_second);
                    }
                    
                }
                
                ObjectList<Node*> children = node->get_children();
                for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    substitute_reaching_definition_known_values(*it);
                }
            }
        }
        
        void StaticAnalysis::extend_reaching_definitions_info(Node* node)
        {
            bool changes = true;
            while (changes)
            {
                changes = false;
                ExtensibleGraph::clear_visits(node);
                propagate_reach_defs_among_nodes(node, changes);
            }
        
            ExtensibleGraph::clear_visits(node);
            make_permanent_auxiliar_values(node);
            
            ExtensibleGraph::clear_visits(node);
            substitute_reaching_definition_known_values(node);
        }
        
        StaticAnalysis::StaticAnalysis(LoopAnalysis* loop_analysis)
            :_loop_analysis(loop_analysis)
        {}
        
        void StaticAnalysis::live_variable_analysis(Node* node)
        {
            solve_live_equations(node);
            ExtensibleGraph::clear_visits(node);
        }
        
        void StaticAnalysis::solve_live_equations(Node* node)
        {
            bool changed = true;
            int i = 0;
            while (changed && i<20)
            {
                i++;
                changed = false;
                solve_live_equations_recursive(node, changed);
                ExtensibleGraph::clear_visits(node);
            }
        }

        static std::string prettyprint_ext_sym_set(ext_sym_set s)
        {
            std::string result;
            
            for(ext_sym_set::iterator it = s.begin(); it != s.end(); ++it)
            {
                if (it->get_nodecl().is_null())
                {
                    result += it->get_name() + ", ";
                }
                else
                {
                    std::string nodecl_string(codegen_to_str(it->get_nodecl().get_internal_nodecl()));
                    result += nodecl_string + ", ";                
                }
            }
            
            result = result.substr(0, result.size()-2);
            
            return result;
        }

        void StaticAnalysis::solve_live_equations_recursive(Node* actual, bool& changed)
        {
            while (!actual->is_visited())
            {
                actual->set_visited(true);
                
                Node_type ntype = actual->get_type();
                if (ntype != BASIC_EXIT_NODE)
                {
                    ObjectList<Node*> children = actual->get_children();
                    
                    if (ntype == GRAPH_NODE)
                    {
                        if (!actual->has_key(_NODE_LABEL))
                        {
                            internal_error("Graph node '%d' with no specified label."
                                        "Expecting a Pragma, Function_call, Conditional Espression "
                                        "or Splitted instruction as a Graph node here",
                                        actual->get_id());
                        }
                        Node* entry_node = actual->get_graph_entry_node();
                        solve_live_equations_recursive(entry_node, changed);
                        
                        // spread the liveness inside the node to the Graph node
                        actual->set_graph_node_liveness();
                    }
                    else
                    {
                        
                        if (ntype != BASIC_ENTRY_NODE)
                        {
                            ext_sym_set old_live_in = actual->get_live_in_vars();
                            ext_sym_set old_live_out = actual->get_live_out_vars();
                            ext_sym_set live_out, live_in, aux_live_in, aux;
                            
                            // Computing Live out
                            for(ObjectList<Node*>::iterator it = children.begin();it != children.end();++it)
                            {
                                Node_type nt = (*it)->get_type();
                                if (nt == GRAPH_NODE)
                                {
                                    ObjectList<Node*> inner_children = (*it)->get_graph_entry_node()->get_children();
                                    for(ObjectList<Node*>::iterator itic = inner_children.begin();
                                        itic != inner_children.end();
                                        ++itic)
                                    {
                                        ext_sym_set current_live_in = (*itic)->get_live_in_vars();
                                        for (ext_sym_set::iterator itli = current_live_in.begin(); itli != current_live_in.end(); ++itli)
                                        {
                                            if ((*it)->get_scope().is_valid())
                                                if (!itli->get_symbol().get_scope().scope_is_enclosed_by((*it)->get_scope()))
                                                    aux_live_in.insert(*itli);
                                            else
                                                aux_live_in.insert(*itli);
                                        }
                                    }
                                }
                                else if (nt == BASIC_EXIT_NODE)
                                {
                                    ObjectList<Node*> outer_children = (*it)->get_outer_node()->get_children();
                                    for(ObjectList<Node*>::iterator itoc = outer_children.begin();
                                        itoc != outer_children.end();
                                        ++itoc)
                                    {
                                        ext_sym_set outer_live_in = (*itoc)->get_live_in_vars();
                                        for (ext_sym_set::iterator itli = outer_live_in.begin(); itli != outer_live_in.end(); ++itli)
                                        {
                                            aux_live_in.insert(*itli);
                                        }
                                    }
                                }
                                else
                                {
                                    aux_live_in = (*it)->get_live_in_vars();
                                }
                                live_out.insert(aux_live_in);
                            }
                           
                            // Computing Live In
                            aux = sets_difference(live_out, actual->get_killed_vars());
                            live_in = sets_union(actual->get_ue_vars(), aux);
                       
                            if (!sets_equals(old_live_in, live_in) || 
                                !sets_equals(old_live_out, live_out))
                            {
                                actual->set_live_in(live_in);
                                actual->set_live_out(live_out);
                                changed = true;
                            }
                        }
                    }
                    
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    {
                        solve_live_equations_recursive(*it, changed);
                    }
                    continue;
                }
                return;
            }
        }
        
        // FIXME For the moment we assume the user has used the 'auto-deps' clause
        /*!
         * Input values al those which are Live in and are not Live out
         * Output values al those which are Live out and are not Live in
         * Inout values al those which are Live in and Live out
         */
        void StaticAnalysis::analyse_task(Node* task_node)
        {
            Node* entry = task_node->get_graph_entry_node();
        
//             compute_auto_scoping(task_node);
            
            ObjectList<Node*> node_l = task_node->get_inner_nodes_in_level();
            
            // Compute the actions performed over the symbols in all nodes
            ObjectList<ExtensibleSymbol> in_symbols;
            ObjectList<ExtensibleSymbol> out_symbols;
            
            ext_sym_set ue_vars = task_node->get_ue_vars();
            ext_sym_set killed_vars = task_node->get_killed_vars();
            
            ext_sym_set in_vars = task_node->get_live_in_vars();
            for(ext_sym_set::iterator it_in = in_vars.begin(); it_in != in_vars.end(); ++it_in)
            {
                if (!it_in->get_symbol().get_scope().scope_is_enclosed_by(
                    task_node->get_task_context().retrieve_context())
                    && /*not yet included*/ !in_symbols.contains(*it_in) 
                    && /*used within the task*/(ue_vars.contains(*it_in)))
                {
                    if (task_node->has_key(_TASK_FUNCTION))
                    {   // Only if the symbol is a parameter, we include it
                        Symbol function_sym = task_node->get_task_function();
                        scope_entry_t* function_header = function_sym.get_internal_symbol();
                        int num_params = function_header->entity_specs.num_related_symbols;
                        scope_entry_t** related_symbols = function_header->entity_specs.related_symbols;
                        for (int i=0; i<num_params; ++i)
                        {
                            Symbol s(related_symbols[i]);
                            if (s == it_in->get_symbol())
                            {
                                in_symbols.insert(*it_in);
                                break;
                            }
                        }
                    }
                    else
                    {
                        in_symbols.insert(*it_in);
                    }
                }
            }
            
            ext_sym_set out_vars = task_node->get_live_out_vars();
            for(ext_sym_set::iterator it_out = out_vars.begin(); it_out != out_vars.end(); ++it_out)
            {
                if (!it_out->get_symbol().get_scope().scope_is_enclosed_by(
                    task_node->get_task_context().retrieve_context())
                    && /*not yet included*/ !out_symbols.contains(*it_out)
                    && /*used within the task*/ (ue_vars.contains(*it_out) || killed_vars.contains(*it_out)))
                {
                    if (task_node->has_key(_TASK_FUNCTION))
                    {   // Only if the symbol is a parameter, we include it
                        Symbol function_sym = task_node->get_task_function();
                        scope_entry_t* function_header = function_sym.get_internal_symbol();
                        int num_params = function_header->entity_specs.num_related_symbols;
                        scope_entry_t** related_symbols = function_header->entity_specs.related_symbols;
                        for (int i=0; i<num_params; ++i)
                        {
                            Symbol s(related_symbols[i]);
                            if (s == it_out->get_symbol())
                            {
                                out_symbols.insert(*it_out);
                                break;
                            }
                        }
                    }
                    else
                    {
                        out_symbols.insert(*it_out);
                    }
                }
            }
            
            // Compute auto-deps
            ext_sym_set input_deps, output_deps, inout_deps, undef_deps = task_node->get_undefined_behaviour_vars();
            for (ObjectList<ExtensibleSymbol>::iterator it = in_symbols.begin(); it != in_symbols.end(); ++it)
            {
                if (!ext_sym_set_contains_nodecl(it->get_nodecl(), undef_deps))
                {
                    if (out_symbols.contains(*it))
                        inout_deps.insert(*it);
                    else
                        input_deps.insert(*it);
                }
            }
            for (ObjectList<ExtensibleSymbol>::iterator it = out_symbols.begin(); it != out_symbols.end(); ++it)
            {
                if (!in_symbols.contains(*it) && !ext_sym_set_contains_nodecl(it->get_nodecl(), undef_deps))
                {
                    output_deps.insert(*it);
                }
            }
            
            task_node->set_input_deps(input_deps);
            task_node->set_output_deps(output_deps);
            task_node->set_inout_deps(inout_deps);
            task_node->set_undef_deps(undef_deps);
            
            task_node->set_deps_computed();
        }
        
        void StaticAnalysis::analyse_tasks_rec(Node* current)
        {
            if (!current->is_visited())
            {
                current->set_visited(true);
                if (current->get_type() == GRAPH_NODE)
                {
                    if (current->get_graph_type() == TASK)
                    {
                        if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                            CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                            std::cerr << std::endl << "   ==> Task '" << current->get_graph_label().prettyprint() << "'" << std::endl;
                                
                        StaticAnalysis::analyse_task(current);

                        if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                            CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                        {
                            current->print_use_def_chains();
                            current->print_liveness();
                            current->print_task_dependencies();
                        }
                    }
                    else
                    {
                        analyse_tasks_rec(current->get_graph_entry_node());
                    }
                }
                    
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                    analyse_tasks_rec(*it);
            }
        }
        
        void StaticAnalysis::analyse_tasks(Node* graph_node)
        {
            analyse_tasks_rec(graph_node);
            ExtensibleGraph::clear_visits(graph_node);
        }
        
        /*!
         * This method analyses the behaviour of the symbol 's' between the point of synchronization of 'task' and the next synchronization point
         * '0' if 's' is only read, '1' if 's' is written but cannot exist race condition and '2' if can exist a race condition
         * 
         */
        static void analyse_usage_till_next_sync(Node* task, Nodecl::NodeclBase s, auto_scope_tag* auto_scope_t)
        {
            if (s.is<Nodecl::Symbol>())
            {   // Scalar analysis
                // TODO: mirar tots els accessos que es fan entre 1 i 2 i decidir el valor a retornar
                
//                     char state;
//                     switch(state)
//                     {
//                         case '0': auto_scope_t->set_firstprivate_var(s);
//                                   break;
//                         case '1': auto_scope_t->set_shared_var(s);
//                                   break;
//                         case '2': auto_scope_t->set_undef_sc_var(s);
//                                   break;
//                         default:  internal_error("Unexpected result '%s' when computing usage between the scheduling point of a task "\
//                                                  "and the next synchronization point", 0);
//                     }
            }
            else if (s.is<Nodecl::Reference>() || s.is<Nodecl::Derreference>() || s.is<Nodecl::ArraySubscript>())
            {
                
            }
            else
            {   // This values can be a pointer, a reference or an array access
//                 internal_error("Auto-scoping for '%s' as '%s' is not yet implemented", ast_print_node_type(s.get_kind()),
//                                s.prettyprint().c_str());
            }
        }
        
        void StaticAnalysis::compute_auto_scoping_rec(auto_scope_tag * auto_scope_t)
        {
            Node* task = auto_scope_t->get_task();
            
            ext_sym_set ue_vars = auto_scope_t->get_task()->get_ue_vars();
            for(ext_sym_set::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
            {
                if (auto_scope_t->is_live_after_task_sched(*it))
                {
                    analyse_usage_till_next_sync(task, it->get_nodecl(), auto_scope_t);
                }
                else
                {
                    auto_scope_t->set_firstprivate_var(*it);
                }
            }
            
            ext_sym_set killed_vars = task->get_killed_vars();
            for(ext_sym_set::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
            {
                if (auto_scope_t->is_live_after_task_sched(*it))
                {
                    analyse_usage_till_next_sync(task, it->get_nodecl(), auto_scope_t);
                }
                else
                {
                    if (!ext_sym_set_contains_sym(*it, ue_vars))
                        auto_scope_t->set_private_var(*it);
                    else {} // The variable has already been added to the firstprivate list
                }
            }
            
            ext_sym_set undef_vars = task->get_undefined_behaviour_vars();
            for(ext_sym_set::iterator it = undef_vars.begin(); it != undef_vars.end(); ++it)
            {
                auto_scope_t->set_undef_sc_var(*it);
            }

        }
        
        /*!
         * Algorithm:
         * 1.- Determine task scheduling point (1) and next synchronization points (2, 3, ..., n)
         *    - '1' is the node/s parent/s from the task  ->  init_point
         *    - '2' is first taskwait or barrier founded after '1'  ->  end_point
         * 2.- For each scalar variable 'v' appearing within the task:
         *    - if 'v' is dead after '1':
         *        - if the first action performed in 'v' is a write  =>  PRIVATE
         *        - if the first action performed in 'v' is a read  =>  FIRSTPRIVATE
         *    - if 'v' is alive between '1' and 'n':
         *        - if 'v' is only read between '1' and 'n' and within the task  =>  FIRSTPRIVATE
         *        - if 'v' is written in some point between '1' and 'n' and/or within the task:
         *              - if there exist race condition  =>  UNDEFINED SCOPING
         *              - if there is no race condition  =>  SHARED
         * 3.- For each array variable appearing within the task defined the scoping for the whole array as follows:
         *    - Apply the algorithm above for every use of the array within the task
         *          - if the whole array or the used regions of the array have the same scope  =>  propagate scope
         *          - if there are different scoping for different regions of the array:
         *              - if some part has an UNDEFINED SCOPING  =>  UNDEFINED SCOPING
         *              - if at least one part is FIRSTPRIVATE and the rest is PRIVATE  =>  FIRSTPRIVATE
         *              - if at least one part is SHARED and the rest is PRIVATE/FIRSTPRIVATE  =>  SHARED???  (sequential consistency rules)
         * NOTE: There exist race condition when more than one thread can access to the same variable at the same time 
         * and at least one of the accesses is a write. 
         * NOTE: 'atomic' and 'critical' constructs affect in race condition determining. 
         * NOTE: 'ordered' construct affects in determining variables as 'private' or 'firstprivate'
         */
        void StaticAnalysis::compute_auto_scoping(Node* current)
        {
            ObjectList<Node*> init_points = current->get_parents();
            ObjectList<Node*> end_point = current->get_children();
            
            // Compute alive variables between '1' and '2' out of the task
            ObjectList<Node*> non_task_init_children;
            ext_sym_set live_after_task_sched;
            for (ObjectList<Node*>::iterator it = init_points.begin(); it != init_points.end(); ++it)
            {
                if ( ((*it)->get_type() != GRAPH_NODE) || ((*it)->get_type() == GRAPH_NODE && (*it)->get_graph_type() != TASK) )
                    non_task_init_children.append(*it);
            }
            for (ObjectList<Node*>::iterator it = non_task_init_children.begin(); it != non_task_init_children.end(); ++it)
            {
                live_after_task_sched.append((*it)->get_live_in_vars());
            }
            
            // Analyse the variables appearing inside the task
            auto_scope_tag * auto_scope_t = new auto_scope_tag(current, live_after_task_sched);
            compute_auto_scoping_rec(auto_scope_t);
            delete auto_scope_t;
        }
    

        // *** CFG_VISITOR *** //
        
        static bool nodecl_is_base_of_symbol(Symbol s, Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::Symbol>())
            {
                if (n.get_symbol() == s)
                {
                    return true;
                }
                else
                {
                    return false;
                }
            }
            else
            {
                if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference n_ = n.as<Nodecl::Derreference>();
                    return nodecl_is_base_of_symbol(s, n_.get_rhs());
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    return nodecl_is_base_of_symbol(s, n_.get_lhs());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    return nodecl_is_base_of_symbol(s, n_.get_subscripted());
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for base symbol in an extensible symbol", 
                                ast_print_node_type(n.get_kind()));
                }
            }
        }
        
        static bool nodecl_is_base_of_symbol(ObjectList<Symbol> s_l, Nodecl::NodeclBase n)
        {
            for (ObjectList<Symbol>::iterator it = s_l.begin(); it != s_l.end(); ++it)
            {
                if (nodecl_is_base_of_symbol(*it, n))
                {
                    return true;
                }
            }
            return false;
        }
        
        static Symbol get_symbol_of_lvalue(Nodecl::NodeclBase n)
        {
            while (!n.is<Nodecl::Symbol>())
            {
                if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess n_ = n.as<Nodecl::ClassMemberAccess>();
                    get_symbol_of_lvalue(n_.get_member());
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript n_ = n.as<Nodecl::ArraySubscript>();
                    get_symbol_of_lvalue(n_.get_subscripted());
                }
                else if (n.is<Nodecl::FunctionCall>())
                {
                    return Symbol();
                }
                else
                {
                    internal_error("Unexpected type of node '%s' while looking for the symbol in an lvalue Nodecl",
                                    ast_print_node_type(n.get_kind()));
                }
            }
            
        }

        static bool nodecl_is_constant_value(Nodecl::NodeclBase n)
        {
            if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() 
                || n.is<Nodecl::ComplexLiteral>() || n.is<Nodecl::BooleanLiteral>()
                || n.is<Nodecl::StringLiteral>())
            {
                return true;
            }
            return false;
        }

        /*!
        * \param n Nodecl being used/defined which has been stored in an ExtensibleSymbol                  -> a.b.c
        * \param s Symbol containing a parameter of the function where de nodecl has being used/defined    -> A& a
        * \param s_map Nodecl containing the argument corresponding to de parameter                        -> a' of type A&
        */
        static Nodecl::NodeclBase match_nodecl_with_symbol(Nodecl::NodeclBase n, Symbol s, Nodecl::NodeclBase s_map)
        {
            if (!nodecl_is_constant_value(s_map))
            {
                if (n.is<Nodecl::IntegerLiteral>() || n.is<Nodecl::FloatingLiteral>() || n.is<Nodecl::ComplexLiteral>()
                    || n.is<Nodecl::StringLiteral>() || n.is<Nodecl::BooleanLiteral>())
                {   // When the argument is a literal, no symbol is involved
                }
                else if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>())
                {
                    if (n.get_symbol() == s)
                    {
                        return s_map.copy();
                    }
                }
                else if (n.is<Nodecl::ClassMemberAccess>())
                {
                    Nodecl::ClassMemberAccess aux = n.as<Nodecl::ClassMemberAccess>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, s_map);
                    if (!lhs.is_null())
                    {
                        return Nodecl::ClassMemberAccess::make(lhs, aux.get_member(), s_map.get_type(), 
                                                            s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::ArraySubscript>())
                {
                    Nodecl::ArraySubscript aux = n.as<Nodecl::ArraySubscript>();
                    Nodecl::NodeclBase subscripted = match_nodecl_with_symbol(aux.get_subscripted(), s, s_map);
                    if (!subscripted.is_null())
                    {
                        return Nodecl::ArraySubscript::make(subscripted, aux.get_subscripts(), s_map.get_type(),
                                                            s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Reference>())
                {
                    Nodecl::Reference aux = n.as<Nodecl::Reference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Reference::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Derreference>())
                {
                    Nodecl::Derreference aux = n.as<Nodecl::Derreference>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Derreference::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Conversion>())
                {
                    Nodecl::Conversion aux = n.as<Nodecl::Conversion>();
                    Nodecl::NodeclBase nest = match_nodecl_with_symbol(aux.get_nest(), s, s_map);
                    if (!nest.is_null())
                    {
                        return Nodecl::Conversion::make(nest, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Cast>())
                {
                    Nodecl::Cast aux = n.as<Nodecl::Cast>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Conversion::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                // While checking parameters, we can have many types of "Extensible symbols"
                else if (n.is<Nodecl::Minus>())
                {
                    Nodecl::Minus aux = n.as<Nodecl::Minus>();
                    Nodecl::NodeclBase lhs = match_nodecl_with_symbol(aux.get_lhs(), s, s_map);
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!lhs.is_null() || !rhs.is_null())
                    {
                        return Nodecl::Minus::make(lhs, rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }                
                }
                else if (n.is<Nodecl::Preincrement>())
                {
                    Nodecl::Preincrement aux = n.as<Nodecl::Preincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Preincrement::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Postincrement>())
                {
                    Nodecl::Postincrement aux = n.as<Nodecl::Postincrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postincrement::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Predecrement>())
                {
                    Nodecl::Predecrement aux = n.as<Nodecl::Predecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Predecrement::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else if (n.is<Nodecl::Postdecrement>())
                {
                    Nodecl::Postdecrement aux = n.as<Nodecl::Postdecrement>();
                    Nodecl::NodeclBase rhs = match_nodecl_with_symbol(aux.get_rhs(), s, s_map);
                    if (!rhs.is_null())
                    {
                        return Nodecl::Postdecrement::make(rhs, s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
                    }
                }
                else
                {
                    internal_error("Node type '%s' in node '%s' not yet implemented while parsing an Extensible symbol matching",
                                ast_print_node_type(n.get_kind()), n.prettyprint().c_str());
                }
            }

            return Nodecl::NodeclBase::null();
        }
    
        /*!
        * This method searches a parameter of a symbol list that matches with a nodecl
        * The method returns a copy of the symbol, if matched
        * \param n                Nodecl we are looking for
        * \param s_l              List of parameters
        * \param s_map-l          List of arguments
        * \param matching_symbol  Symbol in the parameter list which matches 
        */
        static Nodecl::NodeclBase match_nodecl_in_symbol_l(Nodecl::NodeclBase n, ObjectList<Symbol> s_l, 
                                                           Nodecl::List s_map_l,
                                                           Symbol& matching_symbol)
        {
            ObjectList<Symbol>::iterator it_s = s_l.begin();
            Nodecl::List::iterator it_s_map = s_map_l.begin();
            Nodecl::NodeclBase actual_nodecl;
            for (; (it_s != s_l.end()) && (it_s_map != s_map_l.end()); ++it_s, ++it_s_map)
            {
                actual_nodecl = match_nodecl_with_symbol(n, *it_s, *it_s_map);
                if (!actual_nodecl.is_null())
                {
                    matching_symbol = *it_s;
                    break;
                }
            }
            
            return actual_nodecl;
        }
        
        //! Set the variable in #n to UE or KILLED list
        static void set_usage_to_symbol_in_node(Node* node, struct var_usage_t* n, ExtensibleGraph* called_func_graph)
        {
            char usage = n->get_usage();
            if (usage == '0')
            {
                ExtensibleSymbol ue_var(n->get_nodecl());
                node->set_ue_var(ue_var);
            }
            else if (usage == '1')
            {
                ExtensibleSymbol killed_var(n->get_nodecl());
                node->set_killed_var(killed_var);
            }
            else if (usage == '2')
            {
                ExtensibleSymbol ue_killed_var(n->get_nodecl());
                node->set_ue_var(ue_killed_var);
                node->set_killed_var(ue_killed_var);
            }
            else
            {
                internal_error("Unexpected usage value '%c' while computing global variable usage in function ", 
                            usage, called_func_graph->get_function_symbol().get_name().c_str());
            }
        }
        
        bool CfgVisitor::func_has_cyclic_calls_rec(Symbol reach_func, Symbol stop_func, ExtensibleGraph * graph)
        {
            ObjectList<Symbol> called_syms = graph->get_function_calls();
           
            for (ObjectList<Symbol>::iterator it = called_syms.begin(); it != called_syms.end(); ++it)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(*it, _cfgs);
                if (called_func_graph != NULL)
                {
                    if (called_func_graph->get_function_symbol() == reach_func)
                    {
                        return true;
                    }
                    else
                    {
                        if (stop_func != *it)
                            if (func_has_cyclic_calls(reach_func, called_func_graph))
                                return true;
                    }
                }
            }
            
            return false;
        }
        
        bool CfgVisitor::func_has_cyclic_calls(Symbol reach_func, ExtensibleGraph * graph)
        {
            return func_has_cyclic_calls_rec(reach_func, graph->get_function_symbol(), graph);
        }

        void CfgVisitor::set_live_initial_information(Node* node)
        {
            if (node->get_type() == BASIC_FUNCTION_CALL_NODE)
            {
                ExtensibleGraph* called_func_graph = find_function_for_ipa(node->get_function_node_symbol(), _cfgs);
                if (called_func_graph != NULL)
                {
                    // Create a map between the parameters of the called function and the current arguments of the call
                    ObjectList<Symbol> params;
                    Nodecl::List args;
                    std::map<Symbol, Nodecl::NodeclBase> params_to_args;
                    params_to_args = map_params_to_args(/* Nodecl with the function call */node->get_statements()[0], 
                                                        called_func_graph, params, args);
                        
                    Symbol function_sym = called_func_graph->get_function_symbol();
                    if (func_has_cyclic_calls(function_sym, called_func_graph))
                    {   // Recursive analysis: we are only interested in global variables and pointed parameters
                        ObjectList<var_usage_t*> glob_vars = called_func_graph->get_global_variables();
                        ObjectList<Symbol> params = called_func_graph->get_function_parameters();
                        if (!glob_vars.empty() || !params.empty())
                        {
                            // Compute liveness for global variables and parameters
                            CfgIPAVisitor ipa_visitor(called_func_graph, _cfgs, glob_vars, params, params_to_args);
                            ipa_visitor.compute_usage();
                            
                            // Propagate this information to the current graph analysis
                            ext_sym_set ue_vars, killed_vars, undef_vars;
                            ObjectList<struct var_usage_t*> ipa_usage = ipa_visitor.get_usage();
                            for (ObjectList<struct var_usage_t*>::iterator it = ipa_usage.begin(); it != ipa_usage.end(); ++it)
                            {
                                char usage = (*it)->get_usage();
                                Nodecl::NodeclBase s = (*it)->get_nodecl();
                                if (usage == '0')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '1')
                                {
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '2')
                                {
                                    killed_vars.insert(ExtensibleSymbol(s));
                                    ue_vars.insert(ExtensibleSymbol(s));
                                }
                                else if (usage == '3')
                                {
                                    undef_vars.insert(ExtensibleSymbol(s));
                                }
                                else
                                {
                                    internal_error("Undefined usage %s for symbol %s\n", usage, s.prettyprint().c_str())
                                }
                            }
                            node->set_ue_var(ue_vars);
                            node->set_killed_var(killed_vars);
                            node->set_undefined_behaviour_var(undef_vars);
                        }
                    }
                    else
                    {
                        if (!_visited_functions.contains(called_func_graph->get_function_symbol()))
                        {
                            _visited_functions.insert(called_func_graph->get_function_symbol());
                            
                            char has_use_def_computed = called_func_graph->has_use_def_computed();
                            if (has_use_def_computed == '0')
                            {
                                _actual_cfg = called_func_graph;
                                Node* graph_node = called_func_graph->get_graph();
                                compute_use_def_chains(graph_node);
                                if (called_func_graph->has_use_def_computed() == '2')
                                {
                                    has_use_def_computed = '2';
                                    _actual_cfg->set_use_def_computed('2');
                                }
                                else
                                {
                                    called_func_graph->set_use_def_computed('1');
                                    has_use_def_computed = '1';
                                }
                            }
                            
                            // Filter map maintaining those arguments that are constants for "constant propagation" in USE-DEF info
                            std::map<Symbol, Nodecl::NodeclBase> const_args;
                            for(std::map<Symbol, Nodecl::NodeclBase>::iterator it = params_to_args.begin(); it != params_to_args.end(); ++it)
                            {
                                if (it->second.is_constant())
                                {
                                    const_args[it->first] = it->second;
                                }
                            }
                            
                            // compute use-def chains for the function call
                            ext_sym_set called_func_ue_vars = called_func_graph->get_graph()->get_ue_vars();                                
                            ext_sym_set node_ue_vars;
                            for(ext_sym_set::iterator it = called_func_ue_vars.begin(); it != called_func_ue_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase new_ue = match_nodecl_in_symbol_l(it->get_nodecl(), params, args, s);
                                if ( !new_ue.is_null() )
                                {   // UE variable is a parameter or a part of a parameter
                                    if (new_ue.is<Nodecl::Symbol>() || new_ue.is<Nodecl::Cast>() || new_ue.is<Nodecl::ClassMemberAccess>()
                                        || new_ue.is<Nodecl::Reference>() || new_ue.is<Nodecl::Derreference>() 
                                        || new_ue.is<Nodecl::ArraySubscript>()) 
                                    {
                                        ExtensibleSymbol ei(new_ue);
                                        ei.propagate_constant_values(const_args);
                                        node_ue_vars.insert(ei);
                                    }
                                    else if (new_ue.is<Nodecl::FunctionCall>() || new_ue.is<Nodecl::VirtualFunctionCall>())
                                    {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                                    else
                                    {
                                        SymbolVisitor sv;
                                        sv.walk(new_ue);
                                        ObjectList<Nodecl::Symbol> nodecl_symbols = sv.get_symbols();
                                        for (ObjectList<Nodecl::Symbol>::iterator its = nodecl_symbols.begin(); 
                                                its != nodecl_symbols.end(); ++its)
                                        {
                                            ExtensibleSymbol ei(*its);
                                            ei.propagate_constant_values(const_args);
                                            node_ue_vars.insert(ei);
                                        }
                                    }
                                }
                                else if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                        && it->get_symbol().get_scope() != function_sym.get_scope() )
                                {   // UE variable is global
                                    node_ue_vars.insert(*it);
                                }
                            }
                            node->set_ue_var(node_ue_vars);
                        
                            ext_sym_set called_func_killed_vars = called_func_graph->get_graph()->get_killed_vars();
                            ext_sym_set node_killed_vars;
                            for(ext_sym_set::iterator it = called_func_killed_vars.begin(); it != called_func_killed_vars.end(); ++it)
                            {
                                Symbol s(NULL);
                                Nodecl::NodeclBase new_killed = match_nodecl_in_symbol_l(it->get_nodecl(), params, args, s);
                                if ( !new_killed.is_null() )
                                {   // KILLED variable is a parameter or a part of a parameter
                                    decl_context_t param_context =
                                            function_sym.get_internal_symbol()->entity_specs.related_symbols[0]->decl_context;
//                                         if (!params_to_args[s].is<Nodecl::Derreference>()   // Argument is not an address
//                                             && ( s.get_type().is_any_reference()                // Parameter is passed by reference
//                                                 || s.get_type().is_pointer() )              // Parameter is a pointer
//                                             )     // FIXME The argument is not a temporal value /*&& (s.get_scope() != Scope(param_context))*/
//                                         {
                                        ExtensibleSymbol ei(new_killed);
                                        ei.propagate_constant_values(const_args);
                                        node_killed_vars.insert(ei);
//                                         }
                                }
                                else if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                                        && it->get_symbol().get_scope() != function_sym.get_scope() )
                                {   // KILLED variable is global
                                    node_killed_vars.insert(it->get_nodecl());
                                }
                            }
                            node->set_killed_var(node_killed_vars);
                            
                            ext_sym_set called_func_undef_vars = called_func_graph->get_graph()->get_undefined_behaviour_vars();
                            node->set_undefined_behaviour_var(called_func_undef_vars);
                        }
                        else
                        {}  // We already have analysed this function
                    }
                }
                else
                {
                    _actual_cfg->set_use_def_computed('2');
                   
                    // Set undefined_behaviour to all parameters in the function call
                    ext_sym_set undef_behaviour_vars;
                    Nodecl::List args = get_func_call_args(node->get_statements()[0]);
                    for(Nodecl::List::iterator it = args.begin(); it != args.end(); ++it)
                    {
                        if (it->is<Nodecl::Symbol>() || it->is<Nodecl::Cast>() || it->is<Nodecl::ClassMemberAccess>()
                            || it->is<Nodecl::Reference>() || it->is<Nodecl::Derreference>() 
                            || it->is<Nodecl::ArraySubscript>()) 
                        {
                            ExtensibleSymbol ei(*it);
                            undef_behaviour_vars.insert(ei);
                        }
                        else if (it->is<Nodecl::FunctionCall>() || it->is<Nodecl::VirtualFunctionCall>())
                        {}  // Nothing to do, we don't need to propagate the usage of a temporal value
                        else
                        {   // Nothing to do, this arguments are not addresses and if they are passed by refernce, a temporary value will be changed
                            // FIXME We can define a variable here!!
//                             SymbolVisitor sv;
//                             sv.walk(*it);
//                             ObjectList<Nodecl::Symbol> nodecl_symbols = sv.get_symbols();
//                             for (ObjectList<Nodecl::Symbol>::iterator its = nodecl_symbols.begin(); 
//                                     its != nodecl_symbols.end(); ++its)
//                             {
//                                 ExtensibleSymbol ei(*its);
//                                 undef_behaviour_vars.insert(ei);
//                             }
                        }
                    }
                    node->set_undefined_behaviour_var(undef_behaviour_vars);
                    
                    // Se undefined behaviour to the global variables appearing in the current cfg, only if their usage was not 'killed'
                    // FIXME Here we should take into account any global variable, but from now we only have access to the global variables appearing in the current cfg
                    ObjectList<struct var_usage_t*> current_global_vars = _actual_cfg->get_global_variables();
                    for (ObjectList<var_usage_t*>::iterator it = current_global_vars.begin(); it != current_global_vars.end(); ++it)
                    {
                        char usage = (*it)->get_usage();
                        if (usage != '0' && usage != '2')
                          (*it)->set_usage('3');
                    }
                }
            }
            else
            {
                ObjectList<Nodecl::NodeclBase> basic_block = node->get_statements();
                for (ObjectList<Nodecl::NodeclBase>::iterator it = basic_block.begin();
                        it != basic_block.end();
                        ++it)
                {
                    CfgAnalysisVisitor cfg_analysis_visitor(node);
                    cfg_analysis_visitor.walk(*it);
                }
            }
        }
        
        void CfgVisitor::gather_live_initial_information(Node* node)
        {
            if (!node->is_visited())
            {
                node->set_visited(true);

                Node_type ntype = node->get_type();
                if (ntype != BASIC_EXIT_NODE)
                {
                    if (ntype == GRAPH_NODE)
                    {
                        Node* entry = node->get_graph_entry_node();
                        gather_live_initial_information(entry);
                        ExtensibleGraph::clear_visits_in_level(entry, node);
                        node->set_visited(false);
                        node->set_graph_node_use_def();
                        node->set_visited(true);
                    }
                    else if (ntype != BASIC_ENTRY_NODE)
                    {
                        set_live_initial_information(node);
                    }
                        
                    ObjectList<Edge*> exit_edges = node->get_exit_edges();
                    for (ObjectList<Edge*>::iterator it = exit_edges.begin();
                            it != exit_edges.end();
                            ++it)
                    {
                        gather_live_initial_information((*it)->get_target());
                    }
                }
                return;
            }
        }    
        
        void CfgVisitor::compute_use_def_chains(Node* node)
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose)
                std::cerr << "  ==> Graph '" << _actual_cfg->get_name() << "'" << std::endl;
            gather_live_initial_information(node);
            ExtensibleGraph::clear_visits(node);
            _visited_functions.clear();
        }
        
        void CfgVisitor::analyse_loops(Node* node)
        {
            LoopAnalysis loop_analysis;
            loop_analysis.analyse_loops(node);
            
//             DEBUG_CODE()
            {
                std::cerr << "=== INDUCTION VARIABLES INFO AFTER LOOP ANALYSIS ===" << std::endl;
                loop_analysis.print_induction_vars_info();
            }
            
            StaticAnalysis static_analysis(&loop_analysis);
            static_analysis.extend_reaching_definitions_info(node);
            ExtensibleGraph::clear_visits(node);
        }
    }
}
