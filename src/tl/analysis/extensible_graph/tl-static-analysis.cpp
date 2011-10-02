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
#include "tl-cfg-renaming-visitor.hpp"
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
    
    void CfgVisitor::inline_functions_for_ipa()
    {
        ObjectList<ExtensibleGraph*> ipa_cfgs;
        for (ObjectList<ExtensibleGraph*>::iterator it = _cfgs.begin();
            it != _cfgs.end(); 
            ++it)
        {
            _actual_cfg = *it;
            std::cerr << "IPA for graph '" << _actual_cfg->get_name() << "'" << std::endl;
            ObjectList<Node*> actual_function_calls = _actual_cfg->get_function_calls();
            
            for (ObjectList<Node*>::iterator its = actual_function_calls.begin();
                its != actual_function_calls.end(); 
                ++its)
            {
                std::cerr << "It has function call to symbol '" << (*its)->get_function_node_symbol().get_name() << "'" << std::endl;
                // Search the function in our list of graphs
                find_function_for_inline(*its);
            }
        }        
    }
    
    void CfgVisitor::find_function_for_inline(Node* function_call)
    {
        for (ObjectList<ExtensibleGraph*>::iterator itg = _cfgs.begin();
            itg != _cfgs.end(); 
            ++itg)
        {
            Symbol actual_sym = (*itg)->get_function_symbol();
            if (actual_sym.is_valid() && function_call->get_function_node_symbol() == actual_sym)
            {
                std::cerr << "Going to inline function " << std::endl;
                // Inline the function into the graph
                inline_function_in_graph(function_call, *itg);
            }
        }
    }
    
    void CfgVisitor::inline_function_in_graph(Node* function_call, 
                                              ExtensibleGraph* func_graph)
    {
        Nodecl::NodeclBase func_nodecl_base = function_call->get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS)[0];
       
        std::cerr << "Anem a fer inline del graf " << func_graph->get_name() << std::endl;
        ExtensibleGraph* inlined_func_graph = func_graph->copy();
        Node* func_graph_node = inlined_func_graph->get_graph();
        
        // Get the arguments and the parameters
        std::vector<Nodecl::NodeclBase> args, params;
        Symbol function_header = func_graph->get_function_symbol();
        std::cerr << "Function header = " << c_cxx_codegen_to_str(function_header.get_initialization().get_internal_nodecl()) << std::endl;
        if (func_nodecl_base.is<Nodecl::FunctionCall>())
        {
            Nodecl::FunctionCall func_nodecl = func_nodecl_base.as<Nodecl::FunctionCall>();
            args = func_nodecl.get_arguments().as<Nodecl::List>(); // This list contains always one
        }
        else
        {   // is VirtualFunctionCall
            Nodecl::VirtualFunctionCall func_nodecl = func_nodecl_base.as<Nodecl::VirtualFunctionCall>();
            args = func_nodecl.get_arguments().as<Nodecl::List>(); // This list contains always one
        }
//         params = function_header.as<Nodecl::FunctionCode>().get_initializers().as<Nodecl::List>();
//         std::cerr << "PARAMS has size: " << params.size() << std::endl;
        
        
        // Rename arguments
        ObjectList<Nodecl::NodeclBase> tmp_args_l = rename_arguments(func_nodecl_base, args);
        Node* args_rename_node = new Node(_actual_cfg->_nid, BASIC_NORMAL_NODE, func_graph_node, tmp_args_l);
        
        // Connect this renaming node properly
        Node* inlined_entry = func_graph_node->get_data<Node*>(_ENTRY_NODE);
        ObjectList<Node*> inlined_entry_children = inlined_entry->get_children();
        ObjectList<Edge_type> inlined_entry_exit_types = inlined_entry->get_exit_edge_types();
        ObjectList<std::string> inlined_entry_exit_labels = inlined_entry->get_exit_edge_labels();
        _actual_cfg->disconnect_nodes(inlined_entry, inlined_entry_children);
        _actual_cfg->connect_nodes(inlined_entry, args_rename_node);
        _actual_cfg->connect_nodes(args_rename_node, inlined_entry_children, 
                                   inlined_entry_exit_types, inlined_entry_exit_labels);
        
        
        // Create a map between the argument symbol, and the temporary symbol
//         std::vector<Nodecl::NodeclBase>::iterator it_args; ObjectList<Nodecl::NodeclBase>::iterator it_params;
//         for(it_args = args.begin(), it_params = tmp_args_l.begin(); 
//             it_args != args.end(), it_params != tmp_args_l.end();
//             ++it_args, ++it_params)
//         {
//             _tmp_args_map[it_args->get_symbol()] = *it_params;
//         }
        
        // Propagate argument new values
//         propagate_tmp_args(inlined_func_graph);

        // Substitute the function call by a copy of the graph
        Node* node_to_replace = function_call->get_data<Node*>(_OUTER_NODE);
        func_graph_node->set_data(_OUTER_NODE, node_to_replace->get_data<Node*>(_OUTER_NODE));
        _actual_cfg->replace_node(node_to_replace, func_graph_node);
        _actual_cfg->print_graph_to_dot();
    }
    
    ObjectList<Nodecl::NodeclBase> CfgVisitor::rename_arguments(Nodecl::NodeclBase func_nodecl_base, 
                                                                std::vector<Nodecl::NodeclBase> args)
    {
        // Create one nodecl containing a new temporary variable and its initialization with the argument value
        ObjectList<Nodecl::NodeclBase> tmp_args_l;
        int i = 0;
        for (std::vector<Nodecl::NodeclBase>::iterator it = args.begin();
            it != args.end(); 
            ++it)
        {
            const char* filename =  func_nodecl_base.get_filename().c_str();
            int line = func_nodecl_base.get_line();
          
            // Build a new symbol 
            Symbol called_symbol;
            if (func_nodecl_base.is<Nodecl::FunctionCall>())
            {
                Nodecl::FunctionCall aux = func_nodecl_base.as<Nodecl::FunctionCall>();
                called_symbol = aux.get_called().get_symbol();
            }
            else 
            {   // VirtualFunctionCall
                Nodecl::VirtualFunctionCall aux = func_nodecl_base.as<Nodecl::VirtualFunctionCall>();
                called_symbol = aux.get_called().get_symbol();
            }

            // FIXME When ellipsis, i > called_symbol_->entity_specs.num_related_symbols
            // We must control that and, in that case, get the type from the argument and not from the parameter
            std::stringstream ss; ss << _i;
            std::string sym_name = "_tmp_" + ss.str();
            decl_context_t sym_context = func_nodecl_base.retrieve_context().get_decl_context();
            scope_entry_t* new_sym = new_symbol(sym_context, sym_context.current_scope, sym_name.c_str());
                new_sym->kind = SK_VARIABLE;
            scope_entry_t* called_symbol_ = called_symbol.get_internal_symbol();
            scope_entry_t* param = called_symbol_->entity_specs.related_symbols[i];
                new_sym->type_information = param->type_information;
                new_sym->value = it->get_internal_nodecl();
            
            nodecl_t tmp_arg = nodecl_make_object_init(new_sym, 
                                                       filename, line);
            Nodecl::ObjectInit new_obj_init(tmp_arg);
            
            tmp_args_l.append(new_obj_init);
             ++i;
        }
        
        return tmp_args_l;
    }
    
    void CfgVisitor::propagate_tmp_args(ExtensibleGraph* inlined_func_graph)
    {
        propagate_tmp_args_rec(inlined_func_graph->_graph);
        inlined_func_graph->clear_visits(inlined_func_graph->_graph);
    }
    
    void CfgVisitor::propagate_tmp_args_rec(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            switch(actual_node->get_data<Node_type>(_NODE_TYPE))
            {
                case GRAPH_NODE:                propagate_tmp_args_rec(actual_node->get_data<Node*>(_ENTRY_NODE));
                                                break;
                case BASIC_BREAK_NODE:
                case BASIC_ENTRY_NODE:  
                case FLUSH_NODE:
                case BARRIER_NODE:
                case UNCLASSIFIED_NODE:
                case BASIC_PRAGMA_DIRECTIVE_NODE:
                                                break; // nothing to do
                case BASIC_EXIT_NODE:           return;
                                                break;
                case BASIC_FUNCTION_CALL_NODE:  find_function_for_inline(actual_node);
                                                break;
                case BASIC_NORMAL_NODE:
                case BASIC_LABELED_NODE:         // The node contains statements, check the symbols inside
                                                // TODO
                {
                    ObjectList<Nodecl::NodeclBase> stmts = actual_node->get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS);
                    ObjectList<Nodecl::NodeclBase> renamed_stmts;
//                     for (ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin();
//                         it != stmts.end();
//                         ++it)
//                     {
//                         CfgRenamingVisitor rename_v(_tmp_args_map, it->get_filename().c_str(), it->get_line(), _i);
//                         ObjectList<Nodecl::NodeclBase> renamed_nodecl = rename_v.walk(*it);
//                         if (renamed_nodecl.empty())
//                         {
//                             renamed_stmts.append(*it);
//                         }
//                         else
//                         {    
//                             renamed_stmts.append(renamed_nodecl);
//                         }
//                     }
                    actual_node->set_data(_NODE_STMTS, renamed_stmts);
                    break;
                }
                default:                        internal_error("Unexpected type of node '%s' while inlining function",
                                                               actual_node->get_node_type_as_string().c_str());
            }

            ObjectList<Node*> actual_children = actual_node->get_children();
            for (ObjectList<Node*>::iterator it = actual_children.begin();
                it != actual_children.end();
                ++it)
            {
                propagate_tmp_args_rec(*it);
            }
        }
    }
}