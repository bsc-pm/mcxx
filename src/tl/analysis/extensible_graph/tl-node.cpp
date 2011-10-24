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


#include "cxx-codegen.h"
#include "cxx-process.h"

#include "tl-cfg-renaming-visitor.hpp"
#include "tl-node.hpp"


namespace TL
{
    Node::Node()
        : _id(-1), _entry_edges(), _exit_edges(), _visited(false)
    {
        set_data(_NODE_TYPE, UNCLASSIFIED_NODE);
    }
    
    Node::Node(int& id, Node_type ntype, Node* outer_graph)
        : _id(++id), _entry_edges(), _exit_edges(), _visited(false)
    {
        set_data(_NODE_TYPE, ntype);
        
        if (outer_graph != NULL)
        {
            set_data(_OUTER_NODE, outer_graph);
        }

        if (ntype == GRAPH_NODE)
        {
            set_data(_ENTRY_NODE, new Node(id, BASIC_ENTRY_NODE, NULL));
            int a = -2; set_data(_EXIT_NODE, new Node(a, BASIC_EXIT_NODE, NULL));
        }
    }
    
    Node::Node(int& id, Node_type type, Node* outer_graph, ObjectList<Nodecl::NodeclBase> nodecls)
        : _id(++id), _entry_edges(), _exit_edges(), _visited(false)
    {        
        set_data(_NODE_TYPE, type);
        
        if (outer_graph != NULL)
        {    
            set_data(_OUTER_NODE, outer_graph);
        }
        
        set_data(_NODE_STMTS, nodecls);
    }
    
    Node::Node(int& id, Node_type type, Node* outer_graph, Nodecl::NodeclBase nodecl)
        : _id(++id), _entry_edges(), _exit_edges(), _visited(false)
    {      
        set_data(_NODE_TYPE, type);
        
        if (outer_graph != NULL)
        {    
            set_data(_OUTER_NODE, outer_graph);
        }
        
        set_data(_NODE_STMTS, ObjectList<Nodecl::NodeclBase>(1,nodecl));
    }
    
    bool Node::operator==(const Node& node) const
    {
        return (_id == node._id);
    }
    
    void Node::erase_entry_edge(Node* source)
    {
        ObjectList<Edge*>::iterator it;
        for (it = _entry_edges.begin(); 
                it != _entry_edges.end();
                ++it)
        {
            if ((*it)->get_source() == source)
            {
                _entry_edges.erase(it);
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if (it == _entry_edges.end())
        {
            std::cerr << " ** Node.cpp :: erase_entry_edge() ** "
                      << "Trying to delete an non-existent edge " 
                      << "between nodes '" << source->_id << "' and '" << _id << "'" << std::endl;
        }
    }
    
    void Node::erase_exit_edge(Node* target)
    {
        ObjectList<Edge*>::iterator it;
        for (it = _exit_edges.begin(); 
                it != _exit_edges.end();
                ++it)
        {
            if ((*it)->get_target() == target)
            {
                _exit_edges.erase(it);
                --it;   // Decrement to allow the correctness of the comparison outside the loop
                break;
            }
        }
        if (it == _exit_edges.end())
        {
            std::cerr << " ** Node.cpp :: exit_entry_edge() ** "
                      << "Trying to delete an non-existent edge " 
                      << "between nodes '" << _id << "' and '" << target->_id << "'" << std::endl;  
     
        }
    }
    
    int Node::get_id() const
    {
        return _id;
    }
    
    void Node::set_id(int id)
    {
        _id = id;
    }
    
    bool Node::is_visited() const
    {
        return _visited;
    }
    
    void Node::set_visited(bool visited)
    {
        _visited = visited;
    }
    
    bool Node::is_empty_node()
    {
        return (_id==-1 && get_data<Node_type>(_NODE_TYPE) == UNCLASSIFIED_NODE);
    }
    
    ObjectList<Edge*> Node::get_entry_edges() const
    {
        return _entry_edges;
    }
    
    void Node::set_entry_edge(Edge *entry_edge)
    {
        _entry_edges.append(entry_edge);
    }
    
    ObjectList<Edge_type> Node::get_entry_edge_types()
    {
        ObjectList<Edge_type> result;
        
        for(ObjectList<Edge*>::iterator it = _entry_edges.begin();
                it != _entry_edges.end();
                ++it)
        {
            result.append((*it)->get_data<Edge_type>(_EDGE_TYPE));
        }
        
        return result;
    }
    
    ObjectList<std::string> Node::get_entry_edge_labels()
    {
        ObjectList<std::string> result;
        
        for(ObjectList<Edge*>::iterator it = _entry_edges.begin();
                it != _entry_edges.end();
                ++it)
        {
            result.append((*it)->get_label());
        }
        
        return result;
    }
    
    ObjectList<Node*> Node::get_parents()
    {
        ObjectList<Node*> result;
        
        for(ObjectList<Edge*>::iterator it = _entry_edges.begin();
                it != _entry_edges.end();
                ++it)
        {
            result.append((*it)->get_source());
        }
        
        return result;
    }
    
    ObjectList<Edge*> Node::get_exit_edges() const
    {
        return _exit_edges;
    }
    
    void Node::set_exit_edge(Edge *exit_edge)
    {
        _exit_edges.append(exit_edge);
    }
    
    ObjectList<Edge_type> Node::get_exit_edge_types()
    {
        ObjectList<Edge_type> result;
        
        for(ObjectList<Edge*>::iterator it = _exit_edges.begin();
                it != _exit_edges.end();
                ++it)
        {
            result.append((*it)->get_data<Edge_type>(_EDGE_TYPE));
        }
        
        return result;
    }
    
    ObjectList<std::string> Node::get_exit_edge_labels()
    {
        ObjectList<std::string> result;
        
        for(ObjectList<Edge*>::iterator it = _exit_edges.begin();
                it != _exit_edges.end();
                ++it)
        {
            result.append((*it)->get_label());
        }
        
        return result;
    }
    
    Edge* Node::get_exit_edge(Node* target)
    {
        Edge* result = NULL;
        int id = target->get_id();
        for(ObjectList<Edge*>::iterator it = _exit_edges.begin();
            it != _exit_edges.end();
            ++it)
        {
            if ((*it)->get_target()->get_id() == id)
            { 
                result = *it;
                break;
            }
        }
        return result;
    }
    
    ObjectList<Node*> Node::get_children()
    {
        ObjectList<Node*> result;
        
        for(ObjectList<Edge*>::iterator it = _exit_edges.begin();
                it != _exit_edges.end();
                ++it)
        {
            result.append((*it)->get_target());
        }
        
        return result;
    }

    bool Node::is_basic_node()
    {
        if (has_key(_NODE_TYPE))
        {
            Node_type nt = get_data<Node_type>(_NODE_TYPE);
            return (nt != GRAPH_NODE);
        }
        else
        {
            std::cerr << " ** Node.cpp :: is_basic_node() ** "
                      << "warning: The node '" << _id << "' has not type." << std::endl;
            return false;
        }
    }

    bool Node::is_connected()
    {
        return (!_entry_edges.empty() || !_exit_edges.empty());
    }

    bool Node::has_child(Node* n)
    {
        bool result = false;
        int id = n->_id;
        
        for (ObjectList<Edge*>::iterator it = _exit_edges.begin(); it != _exit_edges.end(); ++it)
        {
            if ((*it)->get_target()->_id == id)
            {    
                result = true;
                break;
            }
        }
        
        return result;
    }

    bool Node::has_parent(Node* n)
    {
        bool result = false;
        int id = n->_id;
        
        for (ObjectList<Edge*>::iterator it = _entry_edges.begin(); it != _entry_edges.end(); ++it)
        {
            if ((*it)->get_source()->_id == id)
            {    
                result = true;
                break;
            }
        }
        
        return result;
    }

    bool Node::operator==(const Node* &n) const
    {
        return ((_id == n->_id) && (_entry_edges == n->_entry_edges) && (_exit_edges == n->_exit_edges));
    }

    ObjectList<Node*> Node::get_inner_nodes()
    {
        if (get_data<Node_type>(_NODE_TYPE) != GRAPH_NODE)
        {
            return ObjectList<Node*>();
        }
        else
        {
            ObjectList<Node*> node_l;
            get_inner_nodes_rec(node_l);
            
            return node_l;
        }
    }

    void Node::get_inner_nodes_rec(ObjectList<Node*>& node_l)
    {
        if (!_visited)
        {
            set_visited(true);
            
            Node* actual = this;
            Node_type ntype = get_data<Node_type>(_NODE_TYPE);
            if (ntype == GRAPH_NODE)
            {
                // Get the nodes inside the graph
                Node* entry = get_data<Node*>(_ENTRY_NODE);
                entry->set_visited(true);
                actual = entry;
            }
            else if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            else if ((ntype == BASIC_NORMAL_NODE) || (ntype == BASIC_LABELED_NODE) || (ntype == BASIC_FUNCTION_CALL_NODE))
            {
                // Include the node into the list
                node_l.insert(this);
            }
            else if ((ntype == BASIC_GOTO_NODE) || (ntype == BASIC_BREAK_NODE) 
                     || (ntype == FLUSH_NODE) || (ntype == BARRIER_NODE) || (ntype == BASIC_PRAGMA_DIRECTIVE_NODE))
            {   // do nothing
            }
            else
            {
                internal_error("Unexpected kind of node '%s'", get_type_as_string().c_str());
            }
            
            ObjectList<Node*> next_nodes = actual->get_children();
            for (ObjectList<Node*>::iterator it = next_nodes.begin();
                it != next_nodes.end();
                ++it)
            {
                (*it)->get_inner_nodes_rec(node_l);
            }            
        }
        
        return;
    }

    Symbol Node::get_function_node_symbol()
    {
       
        if (get_data<Node_type>(_NODE_TYPE) != BASIC_FUNCTION_CALL_NODE)
        {
            return Symbol();
        }
        
        Nodecl::NodeclBase stmt = get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS)[0];
        Symbol s;
        if (stmt.is<Nodecl::FunctionCall>())
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>();
            s = f.get_called().get_symbol();
        }
        else if (stmt.is<Nodecl::VirtualFunctionCall>())
        {
            Nodecl::FunctionCall f = stmt.as<Nodecl::FunctionCall>();
            s = f.get_called().get_symbol();
        }
        
        return s;
    }
    
    Node_type Node::get_type()
    {
        if (has_key(_NODE_TYPE))
            return get_data<Node_type>(_NODE_TYPE);
        else
            return UNCLASSIFIED_NODE;
    }

    std::string Node::get_type_as_string()
    {
        std::string type = "";
        if (has_key(_NODE_TYPE))
        {
            Node_type ntype = get_data<Node_type>(_NODE_TYPE);
            switch(ntype)
            {
                case BASIC_ENTRY_NODE:              type = "BASIC_ENTRY_NODE";
                break;
                case BASIC_EXIT_NODE:               type = "BASIC_EXIT_NODE";
                break;
                case BASIC_NORMAL_NODE:             type = "BASIC_NORMAL_NODE";
                break;
                case BASIC_LABELED_NODE:            type = "BASIC_LABELED_NODE";
                break;
                case BASIC_BREAK_NODE:              type = "BASIC_BREAK_NODE";
                break;
                case BASIC_CONTINUE_NODE:           type = "BASIC_CONTINUE_NODE";
                break;
                case BASIC_GOTO_NODE:               type = "BASIC_GOTO_NODE";
                break;
                case BASIC_FUNCTION_CALL_NODE:      type = "BASIC_FUNCTION_CALL_NODE";
                break;
                case BASIC_PRAGMA_DIRECTIVE_NODE:   type = "BASIC_PRAGMA_DIRECTIVE_NODE";
                break;
                case FLUSH_NODE:                    type = "FLUSH_NODE";
                break;
                case BARRIER_NODE:                  type = "BARRIER_NODE";
                break;
                case GRAPH_NODE:                    type = "GRAPH_NODE";
                break;
                case UNCLASSIFIED_NODE:             type = "UNCLASSIFIED_NODE";
                break;
                default: internal_error("Unexpected type of node '%s'", ntype);
            };
        }
        else
        {
            internal_error("The node '%s' has no type assigned, this operation is not allowed", 0);
        }
        
        return type;
    }

    std::string Node::get_graph_type_as_string()
    {
        std::string graph_type = "";
        if (has_key(_GRAPH_TYPE))
        {
            Graph_type ntype = get_data<Graph_type>(_GRAPH_TYPE);
            switch(ntype)
            {
                case SPLIT_STMT:    graph_type = "SPLIT_STMT";
                break;
                case FUNC_CALL:     graph_type = "FUNC_CALL";
                break;
                case COND_EXPR:     graph_type = "COND_EXPR";
                break;
                case LOOP:          graph_type = "LOOP";
                break;
                case OMP_PRAGMA:    graph_type = "OMP_PRAGMA";
                break;
                case TASK:          graph_type = "TASK";
                break;
                default: internal_error("Unexpected type of node '%s'", ntype);
            };
        }
        else
        {
            internal_error("The node '%s' has no graph type assigned, this operation is not allowed", 0);
        }
        
        return graph_type;  
    }

    Node* Node::get_graph_entry_node()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            return get_data<Node*>(_ENTRY_NODE);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the entry node to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }

    void Node::set_graph_entry_node(Node* node)
    {
        if (node->get_data<Node_type>(_NODE_TYPE) != BASIC_ENTRY_NODE)
        {
            internal_error("Unexpected node type '%s' while setting the entry node to node '%s'. BASIC_ENTRY_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
        else if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            return set_data<Node*>(_ENTRY_NODE, node);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the entry node to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }

    Node* Node::get_graph_exit_node()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            return get_data<Node*>(_EXIT_NODE);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the exit node to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }

    void Node::set_graph_exit_node(Node* node)
    {
        if (node->get_data<Node_type>(_NODE_TYPE) != BASIC_EXIT_NODE)
        {
            internal_error("Unexpected node type '%s' while setting the exit node to node '%s'. BASIC_EXIT_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
        else if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            return set_data<Node*>(_EXIT_NODE, node);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the exit node to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }

    Node* Node::get_outer_node()
    {
        Node* outer_node = NULL;
        if (has_key(_OUTER_NODE))
        {
            outer_node = get_data<Node*>(_OUTER_NODE);
        }
        return outer_node;
    }
    
    void Node::set_outer_node(Node* node)
    {
        if (node->get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {    
            set_data(_OUTER_NODE, node);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the exit node to node '%s'. GRAPH_NODE expected.",
                           node->get_type_as_string().c_str(), _id);
        }
    }

    ObjectList<Nodecl::NodeclBase> Node::get_statements()
    {
        ObjectList<Nodecl::NodeclBase> stmts;
        if (has_key(_NODE_STMTS))
        {
            stmts = get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS);
        }
        return stmts;
    }

    void Node::set_statements(ObjectList<Nodecl::NodeclBase> stmts)
    {
        Node_type ntype = get_data<Node_type>(_NODE_TYPE);
        if (ntype == BASIC_NORMAL_NODE || ntype == BASIC_FUNCTION_CALL_NODE || ntype == BASIC_LABELED_NODE
            || ntype == BASIC_BREAK_NODE || ntype == BASIC_CONTINUE_NODE)
        {
            set_data(_NODE_STMTS, stmts);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the statements to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }

    Nodecl::NodeclBase Node::get_graph_label(Nodecl::NodeclBase n)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            Nodecl::NodeclBase res = Nodecl::NodeclBase::null();
            if (has_key(_NODE_LABEL))
            {
                res = get_data<Nodecl::NodeclBase>(_NODE_LABEL, n);
            }
            return res;
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the label to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);            
        }
    }
    
    void Node::set_graph_label(Nodecl::NodeclBase n)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            set_data(_NODE_LABEL, n);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the label to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);            
        }
    }

    Graph_type Node::get_graph_type()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            return get_data<Graph_type>(_GRAPH_TYPE);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting graph type to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);            
        }
    }
           
    void Node::set_graph_type(Graph_type graph_type)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            set_data(_GRAPH_TYPE, graph_type);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting graph type to node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);            
        }
    }

    Loop_type Node::get_loop_node_type()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            if (get_data<Graph_type>(_GRAPH_TYPE) == LOOP)
            {
                return get_data<Loop_type>(_LOOP_TYPE);
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting the loop type of the graph node '%s'. LOOP expected.",
                            get_type_as_string().c_str(), _id);
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the loop type of the graph node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }
    }
           
    void Node::set_loop_node_type(Loop_type loop_type)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            if (get_data<Graph_type>(_GRAPH_TYPE) == LOOP)
            {
                set_data(_LOOP_TYPE, loop_type);
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting the loop type of the graph node '%s'. LOOP expected.",
                            get_type_as_string().c_str(), _id);
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the loop type of the graph node '%s'. GRAPH_NODE expected.",
                           get_type_as_string().c_str(), _id);
        }        
    }

    Symbol Node::get_label()
    {
        Node_type ntype = get_data<Node_type>(_NODE_TYPE);
        if (ntype == BASIC_GOTO_NODE || ntype == BASIC_LABELED_NODE)
        {
            return get_data<Symbol>(_NODE_LABEL);
        }
        else
        {
            internal_error("Unexpected node type '%s' while getting the label to node '%s'. GOTO or LABELED NODES expected.",
                           get_type_as_string().c_str(), _id);            
        }  
    }
           
    void Node::set_label(Symbol s)
    {
        Node_type ntype = get_data<Node_type>(_NODE_TYPE);
        if (ntype == BASIC_GOTO_NODE || ntype == BASIC_LABELED_NODE)
        {
            set_data(_NODE_LABEL, s);
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the label to node '%s'. GOTO or LABELED NODES expected.",
                           get_type_as_string().c_str(), _id);            
        }
    }

    Nodecl::NodeclBase Node::get_task_context()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
            if (graph_type == TASK)
            {
                return get_data<Nodecl::Context>(_TASK_CONTEXT);
            }
            else
            {
                internal_error("Unexpected graph type '%s' while getting the context of the task node '%d'. " \
                               "\"task\" type expected", get_graph_type_as_string().c_str(), _id);   
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                           get_type_as_string().c_str(), _id);                
        }
    }

    void Node::set_task_context(Nodecl::NodeclBase c)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
            if (graph_type == TASK)
            {
                return set_data(_TASK_CONTEXT, c);
            }
            else
            {
                internal_error("Unexpected graph type '%s' while getting the context of the task node '%d'. " \
                               "\"task\" type expected", get_graph_type_as_string().c_str(), _id);                  
            }
        }
        else
        {
            internal_error("Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                           get_type_as_string().c_str(), _id);                
        }
    }

    Node* Node::advance_over_non_statement_nodes()
    {
        ObjectList<Node*> children = get_children();
        Node_type ntype;
        Node* result = this;
        
        while ( (children.size() == 1) && (!result->has_key(_NODE_STMTS)) )
        {
            Node* c0 = children[0];
            ntype = c0->get_data<Node_type>(_NODE_TYPE);
            if (ntype == GRAPH_NODE)
            {
                result = c0->get_data<Node*>(_ENTRY_NODE);
            }
            else if (ntype == BASIC_EXIT_NODE)
            {
                if (c0->has_key(_OUTER_NODE))
                {
                    result = c0->get_data<Node*>(_OUTER_NODE);
                }
                else
                {    
                    break;
                }
            }
            else
            {
                result = c0;
            }
            children = result->get_children();
        }
       
        return result;
    }

    Node* Node::back_over_non_statement_nodes()
    {
        ObjectList<Node*> parents = get_parents();
        Node_type ntype;
        Node* result = this;
        
        while ( (parents.size() == 1) && (!result->has_key(_NODE_STMTS)) )
        {
            Node* p0 = parents[0];
            ntype = p0->get_data<Node_type>(_NODE_TYPE);
            
            if (ntype == GRAPH_NODE)
            {
                result = p0->get_data<Node*>(_EXIT_NODE);
            }
            else if (ntype == BASIC_ENTRY_NODE)
            {
                if (p0->has_key(_OUTER_NODE))
                {
                    result = p0->get_data<Node*>(_OUTER_NODE);
                }
                else
                {    
                    break;
                }
            }
            else
            {
                result = p0;
            }
            parents = result->get_parents();
        }
       
        return result;
    }
   
    ObjectList<ext_sym_set> Node::get_use_def_over_nodes()
    {
        ObjectList<ext_sym_set> use_def, use_def_aux;
        
        if (!_visited)          
        {
            _visited = true;
            ext_sym_set ue_vars, killed_vars, ue_aux, killed_aux;
           
            // Compute upper exposed variables
            ue_aux = get_ue_vars();
            for(ext_sym_set::iterator it = ue_aux.begin(); it != ue_aux.end(); ++it)
            {
                if (!killed_vars.contains(*it))
                {
                    ue_vars.insert(*it);
                }
            }
            // Compute killed variables
            killed_vars.insert(get_killed_vars());
            
            // Complete the use-def info for every children of the node
            ObjectList<Node*> children = get_children();
            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                use_def_aux = (*it)->get_use_def_over_nodes();
                
                if (!use_def_aux.empty())
                {
                    // Compute upper exposed variables
                    ue_aux = use_def_aux[0];
                    for(ext_sym_set::iterator it = ue_aux.begin(); it != ue_aux.end(); ++it)
                    {
                        if (!killed_vars.contains(*it))
                        {
                            ue_vars.insert(*it);
                        }
                    }
                    // Compute killed variables
                    killed_vars.insert(use_def_aux[1]);
                }
            }
            
            use_def.append(ue_vars); use_def.append(killed_vars);
        }
        
        return use_def;
    }
   
    void Node::set_graph_node_use_def()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            _visited = true;
            Node* entry_node = get_data<Node*>(_ENTRY_NODE);
            
            // Get upper_exposed and killed variables
            ObjectList<ext_sym_set> use_def = entry_node->get_use_def_over_nodes();
            
            set_data(_UPPER_EXPOSED, use_def[0]);
            set_data(_KILLED, use_def[1]);
        }
        else
        {
            internal_error("Getting inner use-def information from node '%d' with type '%s' while "
                            "here it is mandatory a Graph node.\n",
                           _id, get_type_as_string().c_str());
        }
    }

    ext_sym_set Node::get_live_in_over_nodes()
    {
        ext_sym_set live_in_vars;
        
        // Advance over non-statements nodes while the path is unique
        Node* actual = advance_over_non_statement_nodes();
        
        // Get the live_in variables
        if (actual->has_key(_NODE_STMTS))
        {
            live_in_vars = actual->get_live_in_vars();
        }
        else
        {   // Node has more than one child
            ObjectList<Node*> actual_children = actual->get_children();
            ext_sym_set live_in_aux;
            Node_type ntype;
            
            for(ObjectList<Node*>::iterator it = actual_children.begin();
                it != actual_children.end();
                ++it)
            {
                // Advance over non-statements nodes while the path is unique
                ntype = (*it)->get_data<Node_type>(_NODE_TYPE);
                if (ntype == GRAPH_NODE)
                {
                    actual = (*it)->get_data<Node*>(_ENTRY_NODE);
                }
                else
                {    
                    actual = (*it);
                }
                actual = actual->advance_over_non_statement_nodes();
               
                live_in_vars.insert(actual->get_live_in_vars());
            }
        }
        
        return live_in_vars;
    }
    
    ext_sym_set Node::get_live_out_over_nodes()
    {
        ext_sym_set live_out_vars;

        // Back over non-statements nodes while the path is unique
        Node* actual = back_over_non_statement_nodes();
        
        // Get the live_out variables
        if (actual->has_key(_NODE_STMTS))
        {
            live_out_vars = actual->get_live_out_vars();
        }
        else
        {   // Node has more than one parent
            ObjectList<Node*> actual_parents = actual->get_parents();
           
            for(ObjectList<Node*>::iterator it = actual_parents.begin();
                it != actual_parents.end();
                ++it)
            {
                Node_type ntype = (*it)->get_data<Node_type>(_NODE_TYPE);
                if (ntype == GRAPH_NODE)
                {
                    actual = (*it)->get_data<Node*>(_EXIT_NODE);
                }
                else
                {    
                    actual = (*it);
                }
                actual = actual->back_over_non_statement_nodes();
               
                live_out_vars.insert(actual->get_live_out_vars());
            }
        }
        
        return live_out_vars;
    }
    
    void Node::set_graph_node_liveness()
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {
            // Get live in variables from entry node children
            Node* entry_node = get_data<Node*>(_ENTRY_NODE);
            set_data(_LIVE_IN, entry_node->get_live_in_over_nodes());
            
            // Get live out variables from exit node parents
            Node* exit_node = get_data<Node*>(_EXIT_NODE);
            set_data(_LIVE_OUT, exit_node->get_live_out_over_nodes());
        }
        else
        {
            internal_error("Getting inner liveness analysis from node '%d' with type '%s' while "
                            "here it is mandatory a Graph node.\n",
                           _id, get_type_as_string().c_str());
        }
    }
    
    void Node::set_graph_node_reaching_defintions(std::map<Symbol, Nodecl::NodeclBase> induct_vars,
                                                  const char* filename, int line)
    {
        if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
        {   // When node is a LOOP graph, we have to look for the 'next' node of the loop
            // otherwise, we can keep the value of the exit node
            Node* exit_node = get_data<Node*>(_EXIT_NODE);
            if (get_data<Graph_type>(_GRAPH_TYPE) == LOOP)
            {
                Node* cond = exit_node->get_parents()[0];
                
                // Get the parent corresponding to the 'next' node
                ObjectList<Edge*> cond_entries = cond->get_entry_edges();
                Node* next = NULL;
                for (ObjectList<Edge*>::iterator it = cond_entries.begin(); it != cond_entries.end(); ++it)
                {
                    if ((*it)->is_back_edge())
                    {
                        next = (*it)->get_source();
                    }
                }
                
                if (next == NULL)
                {
                    internal_error("Cannot found node corresponding to the 'stride node' of the loop graph node '%d'", _id);
                }
                
                // Now, all ranges must be converted to the first/last value depending on the sign of the stride
                nodecl_map old_reach_defs = get_reaching_definitions();
                nodecl_map stride_reach_defs = next->get_data<nodecl_map>(_REACH_DEFS);
                for(nodecl_map::iterator it = stride_reach_defs.begin(); it != stride_reach_defs.end(); ++it)
                {
                    Nodecl::NodeclBase first = it->first, second = it->second;
                    CfgRenamingVisitor renaming_v(induct_vars, filename, line);
                    renaming_v.set_computing_range_limits(true);

                    ObjectList<Nodecl::NodeclBase> renamed;
                    // Visit lhs of the reach def
                    if (!first.is<Nodecl::Symbol>())
                    {
                        renamed = renaming_v.walk(it->first);
                        if (!renamed.empty())
                        {
                            first = renamed[0];
                            if (old_reach_defs.empty() || old_reach_defs.find(first) != old_reach_defs.end())
                            {
                                rename_reaching_defintion_var(first, renamed[0]);
                            }
                            else if (old_reach_defs.find(renamed[0]) != old_reach_defs.end())
                            {   // The renaming was already done. Nothing to do
                            }
                            else
                            {
                                internal_error("A previous renaming in node '%d' has been performed for initial value '%s' and "\
                                               " actual renaming '%s'", _id, first.prettyprint().c_str(), renamed[0].prettyprint().c_str());
                            }
                        }                        
                    }
                    
                    // Visit rhs of the reach def
                    renamed = renaming_v.walk(it->second);
                    if (!renamed.empty())
                    {
                        set_reaching_definition(first, renamed[0]);
                    }
                }
            }
            else
            {
                set_data(_REACH_DEFS, exit_node->get_data<nodecl_map>(_REACH_DEFS));
            }
        }
        else
        {
            internal_error("Getting inner reaching definitions from node '%d' with type '%s' while "
                            "here it is mandatory a Graph node.\n",
                           _id, get_type_as_string().c_str());
        }        
    }
    
    ext_sym_set Node::get_live_in_vars()
    {
        ext_sym_set live_in_vars;
        
        if (has_key(_LIVE_IN))
        {
            live_in_vars = get_data<ext_sym_set>(_LIVE_IN);
        }

        return live_in_vars;
    }
    
    void Node::set_live_in(ExtensibleSymbol new_live_in_var)
    {
        ext_sym_set live_in_vars;
        
        if (has_key(_LIVE_IN))
        {
            live_in_vars = get_data<ext_sym_set>(_LIVE_IN);
        }        
        live_in_vars.insert(new_live_in_var);

        set_data(_LIVE_IN, live_in_vars);
    }
    
    void Node::set_live_in(ext_sym_set new_live_in_set)
    {
        set_data(_LIVE_IN, new_live_in_set);
    }
    
    ext_sym_set Node::get_live_out_vars()
    {
        ext_sym_set live_out_vars;
        
        if (has_key(_LIVE_OUT))
        {
            live_out_vars = get_data<ext_sym_set>(_LIVE_OUT);
        }
        
        return live_out_vars;
    }
    
    void Node::set_live_out(ExtensibleSymbol new_live_out_var)
    {
        ext_sym_set live_out_vars;
        
        if (has_key(_LIVE_OUT))
        {
            live_out_vars = get_data<ext_sym_set>(_LIVE_OUT);
        }
        live_out_vars.insert(new_live_out_var);
        
        set_data(_LIVE_OUT, live_out_vars);
    }
    
    void Node::set_live_out(ext_sym_set new_live_out_set)
    {
        set_data(_LIVE_OUT, new_live_out_set);
    }
    
    ext_sym_set Node::get_ue_vars()
    {
        ext_sym_set ue_vars;
        
        if (has_key(_UPPER_EXPOSED))
        {
            ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
        }
        
        return ue_vars;
    }
    
    void Node::set_ue_var(ExtensibleSymbol new_ue_var)
    {
        ext_sym_set ue_vars;
      
        if (this->has_key(_UPPER_EXPOSED))
        {   
            ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
        }
        ue_vars.insert(new_ue_var);
        
        set_data(_UPPER_EXPOSED, ue_vars);
    }
    
    void Node::unset_ue_var(ExtensibleSymbol old_ue_var)
    {
        ext_sym_set ue_vars;
      
        if (this->has_key(_UPPER_EXPOSED))
        {   
            ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
            ue_vars = ue_vars.not_find(old_ue_var);
        }
        
        set_data(_UPPER_EXPOSED, ue_vars);
    }    
    
    ext_sym_set Node::get_killed_vars()
    {
        ext_sym_set killed_vars;
        
        if (has_key(_KILLED))
        {
            killed_vars = get_data<ext_sym_set>(_KILLED);
        }
        
        return killed_vars;
    }
    
    void Node::set_killed_var(ExtensibleSymbol new_killed_var)
    {
        ext_sym_set killed_vars;
        
        if (has_key(_KILLED))
        {
            killed_vars = get_data<ext_sym_set>(_KILLED);
        }
        killed_vars.insert(new_killed_var);
        
        set_data(_KILLED, killed_vars);
    }

    void Node::unset_killed_var(ExtensibleSymbol old_killed_var)
    {
        ext_sym_set killed_vars;
        
        if (has_key(_KILLED))
        {
            killed_vars = get_data<ext_sym_set>(_KILLED);
            killed_vars = killed_vars.not_find(old_killed_var);
        }
        
        set_data(_KILLED, killed_vars);        
    }

    ext_sym_set Node::get_undefined_behaviour_vars()
    {
        ext_sym_set undef_vars;
        
        if (has_key(_UNDEF))
        {
            undef_vars = get_data<ext_sym_set>(_UNDEF);
        }
        
        return undef_vars;
    }
    
    void Node::set_undefined_behaviour_var(ExtensibleSymbol new_undefined_behaviour_var)
    {
        ext_sym_set undef_vars;
        
        if (has_key(_UNDEF))
        {
            undef_vars = get_data<ext_sym_set>(_UNDEF);
        }
        undef_vars.insert(new_undefined_behaviour_var);
        
        set_data(_UNDEF, undef_vars);
    }

    ext_sym_set Node::get_input_deps()
    {
        ext_sym_set input_deps;
        
        if (has_key(_IN_DEPS))
        {
            input_deps = get_data<ext_sym_set>(_IN_DEPS);
        }
        
        return input_deps;
    }    

    ext_sym_set Node::get_output_deps()
    {
        ext_sym_set output_deps;
        
        if (has_key(_OUT_DEPS))
        {
            output_deps = get_data<ext_sym_set>(_OUT_DEPS);
        }
        
        return output_deps;
    }  
    
    ext_sym_set Node::get_inout_deps()
    {
        ext_sym_set inout_deps;
        
        if (has_key(_INOUT_DEPS))
        {
            inout_deps = get_data<ext_sym_set>(_INOUT_DEPS);
        }
        
        return inout_deps;
    }
    
    nodecl_map Node::get_reaching_definitions()
    {
        nodecl_map reaching_defs;
        
        if (has_key(_REACH_DEFS))
        {
            reaching_defs = get_data<nodecl_map>(_REACH_DEFS);
        }
        
        return reaching_defs;
    }
    

    void Node::set_reaching_definition(Nodecl::NodeclBase var, Nodecl::NodeclBase init)
    {
        nodecl_map reaching_defs;
        if (has_key(_REACH_DEFS))
        {
            reaching_defs = get_data<nodecl_map>(_REACH_DEFS);
        }
        reaching_defs[var] = init;
        set_data(_REACH_DEFS, reaching_defs);
    }
    
    void Node::set_reaching_definition_list(nodecl_map reach_defs_l)
    {
        nodecl_map reaching_defs;
        if (has_key(_REACH_DEFS))
        {
            reaching_defs = get_data<nodecl_map>(_REACH_DEFS);
        }
        for(nodecl_map::iterator it = reach_defs_l.begin(); it != reach_defs_l.end(); ++it)
        {
            reaching_defs[it->first] = it->second;
        }
       
        set_data(_REACH_DEFS, reaching_defs);
    }    
    
    void Node::rename_reaching_defintion_var(Nodecl::NodeclBase old_var, Nodecl::NodeclBase new_var)
    {
        nodecl_map reaching_defs;
        if (has_key(_REACH_DEFS))
        {
            reaching_defs = get_data<nodecl_map>(_REACH_DEFS);
            if (reaching_defs.find(old_var) != reaching_defs.end())
            {
                Nodecl::NodeclBase init = reaching_defs[old_var];
                reaching_defs.erase(old_var);
                reaching_defs[new_var] = init;
            }
            else
            {
                std::cerr << "warning: Trying to rename reaching definition '" << old_var.prettyprint()
                          << "', which not exists in reaching definition list of node '" << _id << "'" << std::endl;
            }
        }
        set_data(_REACH_DEFS, reaching_defs);
    }
    
    nodecl_map Node::get_auxiliar_reaching_definitions()
    {
        nodecl_map aux_reaching_defs;
        
        if (has_key(_AUX_REACH_DEFS))
        {
            aux_reaching_defs = get_data<nodecl_map>(_AUX_REACH_DEFS);
        }
        
        return aux_reaching_defs;
    }
    
    void Node::set_auxiliar_reaching_definition(Nodecl::NodeclBase var, Nodecl::NodeclBase init)
    {
        nodecl_map aux_reaching_defs;
        if (has_key(_AUX_REACH_DEFS))
        {
            aux_reaching_defs = get_data<nodecl_map>(_AUX_REACH_DEFS);
        }
        aux_reaching_defs[var] = init;
        set_data(_AUX_REACH_DEFS, aux_reaching_defs);        
    }
    
    void Node::unset_reaching_definition(Nodecl::NodeclBase var)
    {
        nodecl_map reaching_defs;
        if (has_key(_REACH_DEFS))
        {
            reaching_defs = get_data<nodecl_map>(_REACH_DEFS);
            reaching_defs.erase(var);
        }
        set_data(_REACH_DEFS, reaching_defs);
    }
}