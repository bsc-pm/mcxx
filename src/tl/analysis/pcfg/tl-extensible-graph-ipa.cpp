/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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

//! Note: This file is not compiled because it is not necessary so far
//! In case we want to nest PCFGs into other PCFGs we should finish the implementation and add it to the Makefile

#include "tl-extensible-graph.hpp"
#include "tl-rename-visitor.hpp"

namespace TL {
namespace Analysis {
    
    void rename_nodes_id_rec(Node* current, unsigned int& id)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            current->set_id(++id);
            
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                rename_nodes_id_rec(*it, id);
        }
    }
    
    void ExtensibleGraph::rename_nodes_id(unsigned int& id)
    {
        Node* graph_entry = _graph->get_graph_entry_node();
        rename_nodes_id_rec(graph_entry, id);
        ExtensibleGraph::clear_visits(graph_entry);
    }
    
    void store_returns_in_temporary_vars_rec(Node* current, Node* graph_exit)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            if(current->is_graph_node())
            {
                store_returns_in_temporary_vars_rec(current->get_graph_entry_node(), graph_exit);
            }
            else if(current->is_return_node())
            {
                Nodecl::ReturnStatement ret_stmt = current->get_statements()[0].as<Nodecl::ReturnStatement>();
                NBase ret_value = ret_stmt.get_value();
                if(!ret_value.is_null())
                {
                    Type ret_value_type = ret_value.get_type();
                    
                    // Build a symbol to store the temporary value
                    scope_entry_t* temp_sc_entry = NEW0(scope_entry_t);
                    temp_sc_entry->symbol_name = UNIQUESTR_LITERAL("__tmp_return_val__");
                    temp_sc_entry->kind = SK_VARIABLE;
                    temp_sc_entry->type_information = ret_value_type.get_internal_type();
                    temp_sc_entry->decl_context = ret_value.retrieve_context().get_decl_context();
                    Nodecl::Symbol temp_sym = Nodecl::Symbol::make(Symbol(temp_sc_entry), ret_stmt.get_locus());
                    
                    // Build the statement that stores the return value in the temporary symbol 
                    Nodecl::Assignment tmp_stmt = Nodecl::Assignment::make(temp_sym, ret_value, ret_value_type, 
                                                                           ret_stmt.get_locus());
                    
                    // Modify the 'current' node with the new information
                    current->set_type(__Normal);
                    current->set_statements(ObjectList<NBase>(1, tmp_stmt));
                }
                else
                {   // We have to delete this node and connect its parent with the graph exit node
                    ObjectList<Node*> parents = current->get_parents();
                    ObjectList<EdgeType> entry_types = current->get_entry_edge_types();
                    ObjectList<std::string> entry_labels = current->get_entry_edge_labels();
                    delete_node(current);
                    connect_nodes(parents, graph_exit, entry_types, entry_labels);
                }
            }
            
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                store_returns_in_temporary_vars_rec(*it, graph_exit);
        }
    }
    
    void ExtensibleGraph::store_returns_in_temporary_vars()
    {
        Node* graph_entry = _graph->get_graph_entry_node();
        store_returns_in_temporary_vars_rec(graph_entry, graph_entry, _graph->get_graph_exit_node());
        ExtensibleGraph::clear_visits(graph_entry);
    }
    
    void propagate_argument_rec(Node* current, NodeclReplacer* nr)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            if(current->is_entry_node() || current->is_exit_node())
                return;
            
            if(current->is_graph_node())
            {
                propagate_argument_rec(current->get_graph_entry_node(), nr);
            }
            else
            {
                ObjectList<NBase> stmts = current->get_statements();
                ObjectList<NBase> new_stmts;
                for(ObjectList<NBase>::iterator it = stmts.begin(); it != stmts.end(); ++it)
                {
                    NBase it_copy = Nodecl::Utils::deep_copy(*it, *it);
                    nr->walk(*it);
                    new_stmts.insert(it_copy);
                }
                current->set_statements(new_stmts);
            }
                
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                propagate_argument_rec(*it, nr);
            }
        }
    }
    
    void ExtensibleGraph::propagate_arguments_in_function_graph(NBase arguments)
    {
        ERROR_CONDITION(_nodecl.is_null(), "Found a null nodecl for a graph that is supposed to contain a FunctionCode", 0);
        ERROR_CONDITION(!_nodecl.is<Nodecl::FunctionCode>(), "Expected FunctionCode but '%s' found", 
                         ast_print_node_type(_nodecl.get_kind()));
        Symbol func_sym(_nodecl.get_symbol());
        ERROR_CONDITION(!func_sym.is_valid(), "Invalid symbol for a nodecl that is supposed to contain a FunctionCode", 0);
        
        Nodecl::List args = arguments.as<Nodecl::List>();
        ObjectList<Symbol> params = func_sym.get_function_parameters();
        int n_common_params = std::max(args.size(), params.size());
        
        SymToNodeclMap rename_map;
        for(int i = 0; i < n_common_params; ++i)
        {
            rename_map[params[i]] = args[i];
        }
        NodeclReplacer nr(rename_map);
        Node* graph_entry = _graph->get_graph_entry_node();
        propagate_argument_rec(graph_entry, &nr);
        ExtensibleGraph::clear_visits(graph_entry);
    }
    
    //! Replace node @old_node with node @new_node
    void replace_node(Node* old_node, Node* new_node)
    {   // TODO
        WARNING_MESSAGE("Replacing a node in the PCFG is not yet implemented.\n", 0);
    }
    
    //! Method that inserts the PCFG corresponding to a function call
    void ExtensibleGraph::nest_pcfgs_rec(Node* current, ObjectList<ExtensibleGraph*>* pcfgs, unsigned int& min_id)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            if(current->is_exit_node())
                return;
            
            if(current->is_graph_node())
            {
                // Inline inner nodes, if there is any inner function call
                nest_pcfgs_rec(current->get_graph_entry_node(), pcfgs, min_id);
            }
            else if(current->is_function_call_node())
            {
                // Look for the graph in the list of created pcfgs
                ExtensibleGraph* pcfg = NULL;
                Nodecl::FunctionCall called_func = current->get_statements()[0].as<Nodecl::FunctionCall>();
                Symbol called_sym(called_func.get_called().get_symbol());
                std::cerr << "Called function: " << called_func.prettyprint() << std::endl;
                for(ObjectList<ExtensibleGraph*>::iterator it = pcfgs->begin(); it != pcfgs->end() && pcfg == NULL; ++it)
                {
                    Symbol pcfg_sym((*it)->get_function_symbol());
                    if(pcfg_sym.is_valid())
                    {
                        if(called_sym == pcfg_sym)
                        {
                            pcfg = *it;
                        }
                    }
                }
                if(pcfg != NULL)
                {
                    // Copy the graph we want to embed, since we don't want to change the original 
                    ExtensibleGraph* new_nested_graph = pcfg->copy_graph();
                    
                    // Rename the id of the nodes to avoid having nodes with the same id
                    new_nested_graph->rename_nodes_id(min_id);
                    
                    // Create nodes containing temporary variables with the return values
                    new_nested_graph->store_returns_in_temporary_vars();
                    
                    // Rename the parameters used in the copied graph with the corresponding arguments used in the call
                    new_nested_graph->propagate_arguments_in_function_graph(called_func.get_arguments());
                    
                    Node* function_code_node = new_nested_graph->get_graph()->get_graph_entry_node()->get_children()[0];
                    replace_node(current, function_code_node);
                }
            }
            
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                nest_pcfgs_rec(*it, pcfgs, min_id);
            }
        }
    }
    
    void get_graph_highest_id_rec(unsigned int& highest_id, Node* current)
    {
        if(!current->is_visited())
        {
            current->set_visited(true);
            
            if(current->get_id() > (int)highest_id)
            {
                highest_id = current->get_id();
            }
            
            ObjectList<Node*> children = current->get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                get_graph_highest_id_rec(highest_id, *it);
            }
        }
    }
    
    unsigned int ExtensibleGraph::get_graph_highest_id()
    {
        unsigned int res;
        get_graph_highest_id_rec(res, _graph);
        ExtensibleGraph::clear_visits(_graph);
        return res;
    }
    
    ExtensibleGraph* ExtensibleGraph::nest_pcfgs(ObjectList<ExtensibleGraph*> pcfgs)
    {
        ExtensibleGraph* new_pcfg = this->copy_graph();
        new_pcfg->set_name(new_pcfg->get_name() + "_inlined");
        unsigned int id(get_graph_highest_id());
        Node* graph = new_pcfg->get_graph();
        nest_pcfgs_rec(graph, &pcfgs, id);
        ExtensibleGraph::clear_visits(graph);
        return new_pcfg;
    }

}
}
