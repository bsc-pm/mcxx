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
#include "tl-cfg-visitor.hpp"
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
    
    
    // *** NODE *** //
    
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
    
    void Node::fill_use_def_sets(ObjectList<Symbol> syms, bool defined)
    {
        for(ObjectList<Symbol>::iterator it = syms.begin(); it != syms.end(); ++it)
        {
            fill_use_def_sets(*it, defined, Nodecl::NodeclBase::null());
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


    // *** EXTENSIBLE_GRAPH *** //
    
    void ExtensibleGraph::compute_use_def_chains(Node* node)
    {
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Use-Def computation ===" << std::endl;
        }
        gather_live_initial_information(node);
        clear_visits(node);
    }
    
    void ExtensibleGraph::live_variable_analysis(Node* node)
    {
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Function Live Variable analysis ===" << std::endl;
        }
        
        solve_live_equations(node);
        clear_visits(node);
    }

    void ExtensibleGraph::gather_live_initial_information(Node* actual)
    {
        if (!actual->is_visited())
        {
            actual->set_visited(true);

            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                if (ntype == GRAPH_NODE)
                {
                    Node* entry = actual->get_data<Node*>(_ENTRY_NODE);
                    gather_live_initial_information(entry);
                    clear_visits(entry);
                    actual->set_graph_node_use_def();
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
            }
            return;
        }
    }
    
    void ExtensibleGraph::solve_live_equations(Node* node)
    {
        bool changed = true;
        while (changed)
        {
            changed = false;
            solve_live_equations_recursive(node, changed);
            clear_visits(node);
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
                            else if (nt == BASIC_EXIT_NODE)
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

    void ExtensibleGraph::analyse_tasks(ObjectList<Node*> tasks_l)
    {
        for (ObjectList<Node*>::iterator it = tasks_l.begin(); it != tasks_l.end(); ++it)
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
        ObjectList<ExtensibleSymbol> in_symbols;
        ObjectList<ExtensibleSymbol> out_symbols;
        ext_sym_set li_vars = task_node->get_data<ext_sym_set>(_LIVE_IN);
        for(ObjectList<Node*>::iterator it = node_l.begin(); it != node_l.end(); ++it)
        {
            ext_sym_set ue_vars = (*it)->get_data<ext_sym_set>(_UPPER_EXPOSED);
            ext_sym_set shared_ue_vars = sets_intersection(ue_vars, li_vars);
            for(ext_sym_set::iterator it_ue = ue_vars.begin(); it_ue != ue_vars.end(); ++it_ue)
            {
                if (!it_ue->get_symbol().get_scope().scope_is_enclosed_by(
                    task_node->get_data<Nodecl::NodeclBase>(_TASK_CONTEXT).retrieve_context())
                    && !in_symbols.contains(*it_ue))
                {
                    in_symbols.insert(*it_ue);
                }
            }
                
            ext_sym_set kill_vars = (*it)->get_data<ext_sym_set>(_KILLED);
            for(ext_sym_set::iterator it_kill = kill_vars.begin(); it_kill != kill_vars.end(); ++it_kill)
            {
                if (!it_kill->get_symbol().get_scope().scope_is_enclosed_by(
                    task_node->get_data<Nodecl::NodeclBase>(_TASK_CONTEXT).retrieve_context())
                    && !out_symbols.contains(*it_kill))
                {
                    out_symbols.insert(*it_kill);
                }
            }          
        }
//         Compute auto-deps
        ext_sym_set input_deps, output_deps, inout_deps;
        for (ObjectList<ExtensibleSymbol>::iterator it = in_symbols.begin(); it != in_symbols.end(); ++it)
        {
            if (out_symbols.contains(*it))
            {
                inout_deps.insert(*it);
            }
            else
            {
                input_deps.insert(*it);
            }
        }
        for (ObjectList<ExtensibleSymbol>::iterator it = out_symbols.begin(); it != out_symbols.end(); ++it)
        {
            if (!in_symbols.contains(*it))
            {
                output_deps.insert(*it);
            }
        }
        
        task_node->set_data(_IN_DEPS, input_deps);
        task_node->set_data(_OUT_DEPS, output_deps);
        task_node->set_data(_INOUT_DEPS, inout_deps);
    }
    
    
    // *** CFG_VISITOR *** //
    
    bool CfgVisitor::propagate_use_def_ipa(Node* node)
    {
        DEBUG_CODE()
        {
            std::cerr << "=== CFG IPA of Use/Def chains ===" << std::endl;
        }
        
        bool result = propagate_use_rec(node);
        ExtensibleGraph::clear_visits(node);
       
        return false;
    }
    
    bool CfgVisitor::propagate_use_rec(Node* actual)
    {
        if (!actual->is_visited())
        {
            actual->set_visited(true);
            
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
            if (ntype != BASIC_EXIT_NODE)
            {
                bool result = false;
                if (ntype == GRAPH_NODE)
                {
                    result = propagate_use_rec(actual->get_data<Node*>(_ENTRY_NODE));
                }
                else if (ntype == BASIC_FUNCTION_CALL_NODE)
                {
                    ExtensibleGraph* called_func = find_function_for_ipa(actual);
                    if (called_func != NULL)
                    {
                        result = true;
                        
                        // Get arguments
                        std::vector<Nodecl::NodeclBase> args;
                        Nodecl::NodeclBase func_nodecl = actual->get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS)[0];
                        if (func_nodecl.is<Nodecl::FunctionCall>())
                        {
                            Nodecl::FunctionCall func_call_nodecl = func_nodecl.as<Nodecl::FunctionCall>();
                            args = func_call_nodecl.get_arguments().as<Nodecl::List>();
                        }
                        else
                        {   // is VirtualFunctionCall
                            Nodecl::VirtualFunctionCall func_call_nodecl = func_nodecl.as<Nodecl::VirtualFunctionCall>();
                            args = func_call_nodecl.get_arguments().as<Nodecl::List>();
                        }
                        
                        // Get parameters
                        ObjectList<Symbol> params;
                        Symbol function_sym = called_func->get_function_symbol();
                        scope_entry_t* function_header = function_sym.get_internal_symbol();
                        int num_params = function_header->entity_specs.num_related_symbols;
                        scope_entry_t** related_symbols = function_header->entity_specs.related_symbols;
                        for (int i=0; i<num_params; ++i)
                        {
                            params.append(Symbol(related_symbols[i]));
                        }
                        
                        // Map parameters with arguments
                        std::map<Symbol, Nodecl::NodeclBase> params_to_args;
                        int i = 0;
                        for(ObjectList<Symbol>::iterator it = params.begin(); it != params.end(); ++it, ++i)
                        {
                            params_to_args[*it] = args[i];
                        }
                        
                        // TODO When the use_def info of the called method contains a parameter, it is propagated to the actual node
//                         ext_sym_set called_func_ue_vars = actual->get_ue_vars();
//                         for(ext_sym_set::iterator it = called_func_ue_vars.begin(); it != called_func_ue_vars.end(); ++it)
//                         {
//                             if (params.contains(it->get_symbol()))
//                             {
//                                 
//                             }
//                         }
//                         
//                         ext_sym_set called_func_killed_vars = actual->get_killed_vars();
//                         for(ext_sym_set::iterator it = called_func_killed_vars.begin(); it != called_func_killed_vars.end(); ++it)
//                         {
//                             if (params.contains(it->get_symbol()))
//                             {
//                                 
//                             }                            
//                         }
                    }
                }
                
                ObjectList<bool> results;
                ObjectList<Node*> children = actual->get_children();
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    results.append(propagate_use_rec(*it));
                }
                return (results.contains(true) || result);
            }
            else
            {
                return false;
            }
        }
        return false;
    }
    
    
    ExtensibleGraph* CfgVisitor::find_function_for_ipa(Node* function_call)
    {
        for (ObjectList<ExtensibleGraph*>::iterator it = _cfgs.begin(); it != _cfgs.end(); ++it)
        {
            Symbol actual_sym = (*it)->get_function_symbol();
            if (actual_sym.is_valid() && function_call->get_function_node_symbol() == actual_sym)
            {
                return *it;
            }
        }
        
        return NULL;
    }
}