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


#include "tl-cfg-analysis-visitor.hpp"

#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"

namespace TL
{
    //! This function returns the set which is the union of the two input sets
    static ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2);
    
    //! This function returns the set which is the subtraction of @set1 less @set2
    static ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2);
    
    //! This function returns the set which is the intersection of the two input sets
    static ext_sym_set sets_intersection(ext_sym_set set1, ext_sym_set set2);
    
    //! This function returns true if the two sets contain the same elements
    static bool sets_equals(ext_sym_set set1, ext_sym_set set2);

                    
    void ExtensibleGraph::live_variable_analysis()
    {
        Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Function Live Variable analysis ===" << std::endl;
        }
        
        gather_live_initial_information(entry);
        clear_visits(entry);
        
        solve_live_equations();
        clear_visits(entry);
        
        analyse_tasks();
    }

    void ExtensibleGraph::gather_live_initial_information(Node* actual)
    {
        Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
        while (!actual->is_visited())
        {
            actual->set_visited(true);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    gather_live_initial_information(actual->get_data<Node*>(_ENTRY_NODE));
                }
                else if (ntype != BASIC_ENTRY_NODE)
                {
                    actual->set_live_initial_information();
                }
                    
                ObjectList<Edge*> exit_edges = actual->get_exit_edges();
                for (ObjectList<Edge*>::iterator it = exit_edges.begin();
                        it != exit_edges.end();
                        ++it)
                {
                    gather_live_initial_information((*it)->get_target());
                }
                continue;
            }
            return;
        }
    }
    
    void ExtensibleGraph::solve_live_equations()
    {
        bool changed = true;
        Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);
        while (changed)
        {
            changed = false;
            solve_live_equations_recursive(entry, changed);
            clear_visits(entry);
        }
    }
    
    void ExtensibleGraph::solve_live_equations_recursive(Node* actual, bool& changed)
    {
        while (!actual->is_visited())
        {
            actual->set_visited(true);
            
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
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
                    Node* entry_node = actual->get_data<Node*>(_ENTRY_NODE);
                    solve_live_equations_recursive(entry_node, changed);
                    
                    // spread the liveness inside the node to the Graph node
                    actual->set_graph_node_liveness();
                }
                else
                {
                    
                    if (ntype != BASIC_ENTRY_NODE)
                    {
                        ext_sym_set old_live_in = 
                            actual->get_live_in_vars();
                        ext_sym_set old_live_out = 
                            actual->get_live_out_vars();
                        ext_sym_set live_out, live_in, 
                            aux_live_in, aux;
                        
                        for(ObjectList<Node*>::iterator it = children.begin();
                            it != children.end();
                            ++it)
                        {
                            Node_type nt = (*it)->get_data<Node_type>(_NODE_TYPE);
                            if (nt == GRAPH_NODE)
                            {
                                ObjectList<Node*> inner_children = 
                                    (*it)->get_data<Node*>(_ENTRY_NODE)->get_children();
                                for(ObjectList<Node*>::iterator itic = inner_children.begin();
                                    itic != inner_children.end();
                                    ++itic)
                                {
                                    ext_sym_set aux_set = 
                                        (*itic)->get_live_in_vars();
                                    aux_live_in.insert(aux_set.begin(), aux_set.end());
                                }
                            }
                            else if (nt == BASIC_EXIT_NODE && (*it)->get_id() != _graph->get_data<Node*>(_EXIT_NODE)->get_id())
                            {
                                ObjectList<Node*> outer_children = 
                                    (*it)->get_data<Node*>(_OUTER_NODE)->get_children();
                                for(ObjectList<Node*>::iterator itoc = outer_children.begin();
                                    itoc != outer_children.end();
                                    ++itoc)
                                {
                                    ext_sym_set aux_set = 
                                        (*itoc)->get_live_in_vars();
                                    aux_live_in.insert(aux_set.begin(), aux_set.end());
                                }
                            }
                            else
                            {
                                aux_live_in = (*it)->get_live_in_vars();
                            }
                            live_out.insert(aux_live_in.begin(), aux_live_in.end());
                        }
                        
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
                
                for(ObjectList<Node*>::iterator it = children.begin();
                    it != children.end();
                    ++it)
                {
                    solve_live_equations_recursive(*it, changed);
                }
                continue;
            }
            return;
        }
    }
    
    void Node::set_live_initial_information()
    {
        if (has_key(_NODE_STMTS)) 
        {
            ObjectList<Nodecl::NodeclBase> basic_block = get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS);
            for (ObjectList<Nodecl::NodeclBase>::iterator it = basic_block.begin();
                    it != basic_block.end();
                    ++it)
            {
                CfgAnalysisVisitor cfg_analysis_visitor(this);
                cfg_analysis_visitor.walk(*it);
            }
        }
    }
    
    void Node::fill_use_def_sets(Symbol s, bool defined, Nodecl::NodeclBase n)
    {
        if (defined)
        {
            set_killed_var(ExtensibleSymbol(s, n));
        }
        else
        {
            ext_sym_set killed_vars = get_killed_vars();
            if (killed_vars.find(ExtensibleSymbol(s)) == killed_vars.end())
            {
                set_ue_var(ExtensibleSymbol(s, n));
            }
        }
    }

    void ExtensibleGraph::analyse_tasks()
    {
        for (ObjectList<Node*>::iterator it = _task_nodes_l.begin();
            it != _task_nodes_l.end();
            ++it)
        {
            analyse_task(*it);
            clear_visits(*it);
        }
    }
    
    // FIXME For the moment we assume the user has used the 'auto-deps' clause
    void ExtensibleGraph::analyse_task(Node* task_node)
    {
        Node* entry = task_node->get_data<Node*>(_ENTRY_NODE);
      
        ObjectList<Node*> node_l = task_node->get_inner_nodes();
        
//         // Compute the actions performed over the symbols in all nodes
        ObjectList<sym_info_t*> symbols;
        ext_sym_set li_vars = task_node->get_data<ext_sym_set>(_LIVE_IN);
        for(ObjectList<Node*>::iterator it = node_l.begin(); it != node_l.end(); ++it)
        {
            ext_sym_set ue_vars = (*it)->get_data<ext_sym_set>(_UPPER_EXPOSED);
            ext_sym_set shared_ue_vars = sets_intersection(ue_vars, li_vars);
            for(ext_sym_set::iterator it_ue = ue_vars.begin(); it_ue != ue_vars.end(); ++it_ue)
            {
                sym_info_t* s = new sym_info_t(*it_ue);
                if (symbols.contains(s))
                {
                    sym_info_t* sym_info = symbols.find(s)[0];  // The element will appear only once in the list
                    sym_info->_different_action = '1';
                }
                else
                {
                    sym_info_t* sym_info = new sym_info_t(*it_ue, '0', '0');
                    symbols.insert(sym_info);
                }
            }
                
            ext_sym_set kill_vars = (*it)->get_data<ext_sym_set>(_KILLED);
            for(ext_sym_set::iterator it_kill = kill_vars.begin(); it_kill != kill_vars.end(); ++it_kill)
            {
                sym_info_t* s = new sym_info_t(*it_kill);
                if (symbols.contains(s))
                {
                    sym_info_t* sym_info = symbols.find(s)[0];  // The element will appear only once in the list
                    sym_info->_different_action = '1';
                }
                else
                {
                    // do nothing: When the first action over a Symbol is its definition, the it can only has an 'output' dependence
                    sym_info_t* sym_info = new sym_info_t(*it_kill, '1', '0');
                    symbols.insert(sym_info);
                }
            }            
        }
//         Compute auto-deps
        ext_sym_set input_deps, output_deps, inout_deps;
        for (ObjectList<sym_info_t*>::iterator it = symbols.begin(); it != symbols.end(); ++it)
        {
            if ((*it)->_first_action == '0')
            {
                if ((*it)->_different_action == '0')
                {
                    input_deps.insert((*it)->_sym);
                }
                else
                {
                    inout_deps.insert((*it)->_sym);
                }
            }
            else
            {
                output_deps.insert((*it)->_sym);
            }
        }
        
        task_node->set_data(_IN_DEPS, input_deps);
        task_node->set_data(_OUT_DEPS, output_deps);
        task_node->set_data(_INOUT_DEPS, inout_deps);
    }
    
    static ext_sym_set sets_union(ext_sym_set set1, ext_sym_set set2)
    {
        std::vector<ExtensibleSymbol> v_result(set1.size() + set2.size());
        std::vector<ExtensibleSymbol>::iterator it;
        ext_sym_set result;
        
        it = set_union(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
        
        for(int i=0; i<int(it-v_result.begin()); i++)
        {    
            result.insert(v_result.at(i));
        }
        
        return result;
    }

   
    static ext_sym_set sets_difference(ext_sym_set set1, ext_sym_set set2)
    {
        std::vector<ExtensibleSymbol> v_result(set1.size());
        std::vector<ExtensibleSymbol>::iterator it;
        ext_sym_set result;
        
        it = set_difference(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
        
        for(int i=0; i<int(it-v_result.begin()); i++)
        {    
            result.insert(v_result.at(i));
        }
        
        return result;
    }
    
    static ext_sym_set sets_intersection(ext_sym_set set1, ext_sym_set set2)
    {
        std::vector<ExtensibleSymbol> v_result(set1.size());
        std::vector<ExtensibleSymbol>::iterator it;
        ext_sym_set result;
        
        it = set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
        
        for(int i=0; i<int(it-v_result.begin()); i++)
        {    
            result.insert(v_result.at(i));
        }
        
        return result;        
    }

    static bool sets_equals(ext_sym_set set1, ext_sym_set set2)
    {
        if (set1.size() == set2.size())
        {
            std::vector<ExtensibleSymbol>::iterator it;
            std::vector<ExtensibleSymbol> v_result(set1.size());
            
            it = set_intersection(set1.begin(), set1.end(), set2.begin(), set2.end(), v_result.begin());
            
            return (int(it-v_result.begin()) == set1.size());
        }
        else
        {    
            return false;
        }
    }
}