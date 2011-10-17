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
#include "tl-cfg-visitor.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-extensible-symbol.hpp"
#include "tl-loop-analysis.hpp"


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
        for(std::vector<Nodecl::NodeclBase>::iterator it = n_l.begin(); it != n_l.end(); ++it)
        {
            fill_use_def_sets(*it, defined);
        }
    }

    void ExtensibleGraph::extend_reaching_defintions_info(Node* node)
    {
        if (!node->is_visited())
        {
            std::cerr << " ***** NODE " << node->get_id() << std::endl;
            node->set_visited(true);

            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            
            if (ntype != GRAPH_NODE)
            {
                ObjectList<Edge*> entry_edges;
                ObjectList<Node*> parents;
                if (ntype == BASIC_ENTRY_NODE)
                {
                    entry_edges = node->get_outer_node()->get_entry_edges();
                    parents = node->get_outer_node()->get_parents();
                }
                else
                {
                    entry_edges = node->get_entry_edges();
                    parents = node->get_parents();
                }
                
                nodecl_map reach_defs;
                if (entry_edges.size() > 0)
                {
                    for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
                    {
                        if ((*it)->has_key(_REACH_DEFS))
                        {
                            nodecl_map parent_reach_defs = (*it)->get_data<nodecl_map>(_REACH_DEFS);
                            for(nodecl_map::iterator itm = parent_reach_defs.begin(); itm != parent_reach_defs.end(); ++itm)
                            {
                                Nodecl::NodeclBase a = itm->first, b = itm->second;
                                if (reach_defs.find(itm->first) != reach_defs.end())
                                {   // When the symbol already existed in other parent, invalidate the value
                                    reach_defs[itm->first] = Nodecl::NodeclBase::null();                                
                                }
                                else
                                {   // otherwise, insert the new symbol definition
                                    reach_defs[itm->first] = itm->second;                        
   
                                }
                            }
                        }
                    }
                }
                
                if (node->has_key(_REACH_DEFS))
                {
                    nodecl_map actual_reach_defs = node->get_data<nodecl_map>(_REACH_DEFS);
                    for(nodecl_map::iterator it = actual_reach_defs.begin(); it != actual_reach_defs.end(); ++it)
                    {
                        reach_defs[it->first] = it->second;
                    }
                }
                
                // Set the new value if necessary: more than one parent or some new definition
                if (entry_edges.size() > 1 || !reach_defs.empty())
                {
                    node->set_data(_REACH_DEFS, reach_defs);
                }
                else if (entry_edges.size() == 1)
                {
                    if(parents[0]->has_key(_REACH_DEFS) && reach_defs.empty())
                    {
                        node->set_data(_REACH_DEFS, parents[0]->get_data<nodecl_map>(_REACH_DEFS));
                    }
                }                
            }
            else
            {
                Node* entry = node->get_data<Node*>(_ENTRY_NODE);
                extend_reaching_defintions_info(entry);
                ExtensibleGraph::clear_visits(entry);
                
                node->set_graph_node_reaching_defintions();
            }
            
            // Recursive call for children
            ObjectList<Node*> children = node->get_children();
            for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                extend_reaching_defintions_info(*it);
            }
        }
    }
    

    // *** EXTENSIBLE_GRAPH *** //
    
    void ExtensibleGraph::live_variable_analysis(Node* node)
    {
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Function Live Variable analysis ===" << std::endl;
        }
        
        solve_live_equations(node);
        clear_visits(node);
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
                                    aux_live_in.insert((*itic)->get_live_in_vars());
                                }
                            }
                            else if (nt == BASIC_EXIT_NODE)
                            {
                                ObjectList<Node*> outer_children = (*it)->get_outer_node()->get_children();
                                for(ObjectList<Node*>::iterator itoc = outer_children.begin();
                                    itoc != outer_children.end();
                                    ++itoc)
                                {
                                    aux_live_in.insert((*itoc)->get_live_in_vars());
                                }
                            }
                            else
                            {
                                aux_live_in = (*it)->get_live_in_vars();
                            }
                            live_out.insert(aux_live_in);
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
                
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
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
        for (ObjectList<Node*>::iterator it = _task_nodes_l.begin(); it != _task_nodes_l.end(); ++it)
        {
            analyse_task(*it);
            clear_visits(*it);
        }
    }
    
    // FIXME For the moment we assume the user has used the 'auto-deps' clause  
    void ExtensibleGraph::analyse_task(Node* task_node)
    {
        Node* entry = task_node->get_graph_entry_node();
      
        ObjectList<Node*> node_l = task_node->get_inner_nodes();
        
        // Compute the actions performed over the symbols in all nodes
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
                    task_node->get_task_context().retrieve_context())
                    && !in_symbols.contains(*it_ue))
                {
                    in_symbols.insert(*it_ue);
                }
            }
                
            ext_sym_set kill_vars = (*it)->get_data<ext_sym_set>(_KILLED);
            for(ext_sym_set::iterator it_kill = kill_vars.begin(); it_kill != kill_vars.end(); ++it_kill)
            {
                if (!it_kill->get_symbol().get_scope().scope_is_enclosed_by(
                    task_node->get_task_context().retrieve_context())
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
        {   // When the argument is a literal, no symbol is involved
            if (n.is<Nodecl::Symbol>() || n.is<Nodecl::PointerToMember>())
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
            else if (n.is<Nodecl::ArraySection>())
            {
                Nodecl::ArraySection aux = n.as<Nodecl::ArraySection>();
                Nodecl::NodeclBase subscripted = match_nodecl_with_symbol(aux.get_subscripted(), s, s_map);
                if (!subscripted.is_null())
                {
                    std::cerr << "Returning an array section of symbol " << subscripted.prettyprint() << std::endl;
                    return Nodecl::ArraySection::make(subscripted, aux.get_lower(), aux.get_upper(),
                                                    s_map.get_type(), s_map.get_filename().c_str(), s_map.get_line());
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
            else
            {
                internal_error("Unexpected type of node '%s' founded while parsing an Extensible symbol",
                            ast_print_node_type(n.get_kind()));
            }
        }

        return Nodecl::NodeclBase::null();
    }
   
    /*!
     * This method searches a parameter of a symbol list that matches with a nodecl
     * The method returns a copy of the symbol, if matched
     * \param n                Nodecl we are looking for
     * \param s_l List         List of parameters
     * \param s_map-l          List of arguments
     * \param matching_symbol  Symbol in the parameter list which matches 
     */
    static Nodecl::NodeclBase match_nodecl_in_symbol_l(Nodecl::NodeclBase n, ObjectList<Symbol> s_l, 
                                                       std::vector<Nodecl::NodeclBase> s_map_l,
                                                       Symbol& matching_symbol)
    {
        ObjectList<Symbol>::iterator it_s = s_l.begin();
        std::vector<Nodecl::NodeclBase>::iterator it_s_map = s_map_l.begin();
        Nodecl::NodeclBase actual_nodecl;
        for (; (it_s != s_l.end()) && (it_s_map != s_map_l.end()); ++it_s, ++it_s_map)
        {
            actual_nodecl = match_nodecl_with_symbol(n, *it_s, *it_s_map);
            if (!actual_nodecl.is_null())
            {
                std::cerr << "Matching symbol is" << it_s->get_name() << std::endl;
                matching_symbol = *it_s;
                break;
            }
        }
        
        return actual_nodecl;
    }
    
    void CfgVisitor::set_live_initial_information(Node* node)
    {
        if (node->get_type() == BASIC_FUNCTION_CALL_NODE)
        {   // FIXME Recursive funtions!
        
            // Compute use-def of the called function if necessary
            ExtensibleGraph* called_func_graph = find_function_for_ipa(node);
            if (called_func_graph != NULL && !called_func_graph->has_use_def_computed())
            {
                compute_use_def_chains(called_func_graph->get_graph());
                called_func_graph->set_use_def_computed();
            }
            
            // Map this information between arguments and parameters
            // For info abut variables which are not parameters, look at the context:
            //       - if they are in the called function context, do nothing
            //       - otherwise, their info must be also propagated to the actual node
            std::vector<Nodecl::NodeclBase> args;
            Nodecl::NodeclBase func_nodecl = node->get_statements()[0];
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
            ObjectList<Symbol> params;
            Symbol function_sym = called_func_graph->get_function_symbol();
            scope_entry_t* function_header = function_sym.get_internal_symbol();
            int num_params = function_header->entity_specs.num_related_symbols;
            scope_entry_t** related_symbols = function_header->entity_specs.related_symbols;
            for (int i=0; i<num_params; ++i)
            {
                Symbol s(related_symbols[i]);
                params.append(s);
            }
            std::map<Symbol, Nodecl::NodeclBase> params_to_args;
            int i = 0;
            for(ObjectList<Symbol>::iterator it = params.begin(); it != params.end() && i < args.size(); ++it, ++i)
            {
                params_to_args[*it] = args[i];
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
            ext_sym_set::iterator it = called_func_ue_vars.begin();
            for(; it != called_func_ue_vars.end(); ++it)
            {
                Symbol s(NULL);
                Nodecl::NodeclBase new_ue = match_nodecl_in_symbol_l(it->get_nodecl(), params, args, s);
                if ( !new_ue.is_null() )
                {   // UE variable is a parameter or a part of a parameter
                    ExtensibleSymbol ei(new_ue);
                    ei.propagate_constant_values(const_args);
                    node_ue_vars.insert(ei);
                }
                else if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                          && it->get_symbol().get_scope() != function_sym.get_scope() )
                {   // UE variable is global
                    node_ue_vars.insert(*it);
                }
            }
            if (!node_ue_vars.empty())
            {
                node->set_data<ext_sym_set>(_UPPER_EXPOSED, node_ue_vars);
            }
           
            ext_sym_set called_func_killed_vars = called_func_graph->get_graph()->get_killed_vars(), node_killed_vars;
            for(ext_sym_set::iterator it = called_func_killed_vars.begin(); it != called_func_killed_vars.end(); ++it)
            {
                Symbol s(NULL);
                Nodecl::NodeclBase new_killed = match_nodecl_in_symbol_l(it->get_nodecl(), params, args, s);
                if ( !new_killed.is_null() )
                {   // KILLED variable is a parameter or a part of a parameter
                    decl_context_t param_context = function_sym.get_internal_symbol()->entity_specs.related_symbols[0]->decl_context;
                    if (!params_to_args[s].is<Nodecl::Derreference>()   // Argument is not an address
                        && ( s.get_type().is_reference()                // Parameter is passed by reference
                            || s.get_type().is_pointer() )              // Parameter is a pointer
                        /*&& (s.get_scope() != Scope(param_context))*/)     // FIXME The argument is not a temporal value
                    {
                        ExtensibleSymbol ei(new_killed);
                        ei.propagate_constant_values(const_args);
                        node_killed_vars.insert(ei);
                    }
                }
                else if ( !it->get_symbol().get_scope().scope_is_enclosed_by(function_sym.get_scope()) 
                        && it->get_symbol().get_scope() != function_sym.get_scope() )
                {   // KILLED variable is global
                    node_killed_vars.insert(it->get_nodecl());
                }
            }
            if (!node_killed_vars.empty())
            {
                node->set_data<ext_sym_set>(_KILLED, node_killed_vars);
            }

        }
        else if (node->has_key(_NODE_STMTS)) 
        {
            ObjectList<Nodecl::NodeclBase> basic_block = node->get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS);
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
                    ExtensibleGraph::clear_visits(entry);
                    node->set_graph_node_use_def();
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
        DEBUG_CODE()
        {
            std::cerr << "=== CFG Use-Def computation ===" << std::endl;
        }

        gather_live_initial_information(node);
        ExtensibleGraph::clear_visits(node);
        
        /*ExtensibleGraph::extend_reaching_defintions_info(node);
        ExtensibleGraph::clear_visits(node);*/
        
        LoopAnalysis loop_anal;
        loop_anal.analyse_loops(node);
        ExtensibleGraph::clear_visits(node);
    }
}