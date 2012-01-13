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
#include "tl-analysis-common.hpp"
#include "tl-cfg-renaming-visitor.hpp"
#include "tl-node.hpp"


namespace TL
{
    namespace Analysis
    {
        Node::Node()
            : _id(-1), _entry_edges(), _exit_edges(), _visited(false), _has_deps_computed(false)
        {
            set_data(_NODE_TYPE, UNCLASSIFIED_NODE);
        }
        
        Node::Node(int& id, Node_type ntype, Node* outer_node)
            : _id(++id), _entry_edges(), _exit_edges(), _visited(false), _has_deps_computed(false)
        {
            set_data(_NODE_TYPE, ntype);
            
            if (outer_node != NULL)
            {
                set_data(_OUTER_NODE, outer_node);
            }

            if (ntype == GRAPH_NODE)
            {
                set_data(_ENTRY_NODE, new Node(id, BASIC_ENTRY_NODE, NULL));
                int a = -2; set_data(_EXIT_NODE, new Node(a, BASIC_EXIT_NODE, NULL));
            }
        }
        
        Node::Node(int& id, Node_type type, Node* outer_node, ObjectList<Nodecl::NodeclBase> nodecls)
            : _id(++id), _entry_edges(), _exit_edges(), _visited(false), _has_deps_computed(false)
        {        
            set_data(_NODE_TYPE, type);
            
            if (outer_node != NULL)
            {    
                set_data(_OUTER_NODE, outer_node);
            }
            
            set_data(_NODE_STMTS, nodecls);
        }
        
        Node::Node(int& id, Node_type type, Node* outer_node, Nodecl::NodeclBase nodecl)
            : _id(++id), _entry_edges(), _exit_edges(), _visited(false), _has_deps_computed(false)
        {      
            set_data(_NODE_TYPE, type);
            
            if (outer_node != NULL)
            {    
                set_data(_OUTER_NODE, outer_node);
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
        
        bool Node::has_deps_computed()
        {
            return _has_deps_computed;
        }
        
        void Node::set_deps_computed()
        {
            _has_deps_computed = true;
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

        ObjectList<Node*> Node::get_inner_nodes_in_level()
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
    //                 Node* entry = get_data<Node*>(_ENTRY_NODE);
    //                 entry->set_visited(true);
    //                 actual = entry;
                    node_l.insert(this);
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
                        || (ntype == FLUSH_NODE) || (ntype == BARRIER_NODE) 
                        || (ntype == BASIC_PRAGMA_DIRECTIVE_NODE) || (ntype == TASKWAIT_NODE))
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
                    case TASKWAIT_NODE:                 type = "TASKWAIT_NODE";
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
                    case SPLIT_STMT:        graph_type = "SPLIT_STMT";
                    break;
                    case FUNC_CALL:         graph_type = "FUNC_CALL";
                    break;
                    case COND_EXPR:         graph_type = "COND_EXPR";
                    break;
                    case LOOP:              graph_type = "LOOP";
                    break;
                    case OMP_PRAGMA:        graph_type = "OMP_PRAGMA";
                    break;
                    case TASK:              graph_type = "TASK";
                    break;
                    case EXTENSIBLE_GRAPH:  graph_type = "EXTENSIBLE_GRAPH";
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
                return set_data(_ENTRY_NODE, node);
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
                return set_data(_EXIT_NODE, node);
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

        Scope Node::get_scope()
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                if (has_key(_SCOPE))
                {
                    return get_data<Scope>(_SCOPE);
                }
                else
                {
                    return Scope();
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting the scope of the graph node '%s'. GRAPH_NODE expected.",
                            get_type_as_string().c_str(), _id);
            } 
        }
        
        void Node::set_scope(Scope sc)
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                set_data(_SCOPE, sc);
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting the scope of the graph node '%s'. GRAPH_NODE expected.",
                            get_type_as_string().c_str(), _id);
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
                || ntype == BASIC_BREAK_NODE || ntype == BASIC_CONTINUE_NODE || ntype == BASIC_GOTO_NODE)
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
                    internal_error("Unexpected node type '%s' while setting the loop type of the graph node '%s'. LOOP expected.",
                                get_type_as_string().c_str(), _id);
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while setting the loop type of the graph node '%s'. GRAPH_NODE expected.",
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
                internal_error("Unexpected node type '%s' while getting the context of the task node '%d'. \"task\" type expected.",
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
                    internal_error("Unexpected graph type '%s' while setting the context of the task node '%d'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(), _id);                  
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while setting the label to node '%d'. GRAPH NODE expected.",
                            get_type_as_string().c_str(), _id);                
            }
        }

        Symbol Node::get_task_function()
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
                if (graph_type == TASK)
                {
                    return get_data<Symbol>(_TASK_FUNCTION);
                }
                else
                {
                    internal_error("Unexpected graph type '%s' while getting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(), 
                                get_data<Nodecl::NodeclBase>(_NODE_LABEL).prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting the symbol of the function embedded in a task'. GRAPH NODE expected.",
                            get_type_as_string().c_str());
            }
        }

        void Node::set_task_function(Symbol func_sym)
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
                if (graph_type == TASK)
                {
                    return set_data(_TASK_FUNCTION, func_sym);
                }
                else
                {
                    internal_error("Unexpected graph type '%s' while setting the symbol of the function embedded in the task '%s'. " \
                                "\"task\" type expected", get_graph_type_as_string().c_str(), 
                                get_data<Nodecl::NodeclBase>(_NODE_LABEL).prettyprint().c_str());
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while setting the symbol of the function embedded in a task. GRAPH NODE expected.",
                            get_type_as_string().c_str());
            }
        }

        Node* Node::get_stride_node()
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
                if (graph_type == LOOP)
                {
                    return get_data<Node*>(_STRIDE_NODE);
                }
                else
                {
                    internal_error("Unexpected graph type '%s' while getting the stride node of loop node '%d'. LOOP expected", 
                                get_graph_type_as_string().c_str(), _id);                  
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while getting stride node of loop graph node '%d'. GRAPH NODE expected.",
                            get_type_as_string().c_str(), _id);                
            }
        }

        void Node::set_stride_node(Node* stride)
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                Graph_type graph_type = get_data<Graph_type>(_GRAPH_TYPE);
                if (graph_type == LOOP)
                {
                    set_data(_STRIDE_NODE, stride);
                }
                else
                {
                    internal_error("Unexpected graph type '%s' while setting the stride node to loop node '%d'. LOOP expected", 
                                get_graph_type_as_string().c_str(), _id);                  
                }
            }
            else
            {
                internal_error("Unexpected node type '%s' while setting stride node to loop graph node '%d'. GRAPH NODE expected.",
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
    
        /*!
         * Try to insert a new variable in a list
         * If an englobing variable of the current variable already exists, then we don't include the variable
         * If any variable englobed by the current variable exists, then we delete the variable
         * If the variable is an array en we can form a range with the to access, we do that deleting the existing element of the list and
         * including the new ranged access
         */
        static ext_sym_set insert_var_in_list(Nodecl::NodeclBase var, ext_sym_set list)
        {
            ext_sym_set new_list;
            if (!ext_sym_set_contains_englobing_nodecl(var, list))
            {
                // Create a new list with the elements of 'list' that are not englobed by 'var'
                ext_sym_set aux_list; aux_list.append(ExtensibleSymbol(var));
                for(ext_sym_set::iterator it = list.begin(); it != list.end(); ++it)
                {
                    if (!ext_sym_set_contains_englobing_nodecl(it->get_nodecl(), aux_list))
                    {
                        new_list.append(*it);
                    }
                }
                
                // Insert the new variable
                new_list.append(var);
            }
            else
            {   // No need to insert the variable, its englobing symbol is already there
                // FIXME We can create ranges for array accesses here
                new_list = list;
            }
            return new_list;
        }
    
        /*!
         * Inserts the elements in 'l' to the list 'in_l' when they are not in the list 'avoid_l_1' nor in 'avoid_l_2'
         * When avoiding lists, it take cares of elements englobing the current variable and of elements englobed by the current variable
         */
        static ext_sym_set compute_use_def_with_children(ext_sym_set l, ext_sym_set in_l, ext_sym_set avoid_l_1, ext_sym_set avoid_l_2)
        {
            ext_sym_set new_l = in_l;
            for (ext_sym_set::iterator it = l.begin(); it != l.end(); ++it)
            {
                Nodecl::NodeclBase var = it->get_nodecl();
                if (!ext_sym_set_contains_englobing_nodecl(var, avoid_l_1))
                {   // No englobing variable in the avoiding list 1
                    // Look for variables in avoiding list 1 englobed by 'var'
                    ext_sym_set aux_set; aux_set.insert(var);
                    for (ext_sym_set::iterator ita = avoid_l_2.begin(); ita != avoid_l_2.end(); ++ita)
                    {
                        if (ext_sym_set_contains_englobing_nodecl(ita->get_nodecl(), aux_set))
                        {   // Delete from 'var' the englobed part of (*ita) and put the result in 'var'
                            // TODO
                            std::cerr << "warning: Part of nodecl founded in the current var that must be avoided " << std::endl; 
                        }
                    }
                    
                    if (!ext_sym_set_contains_englobing_nodecl(var, avoid_l_2))
                    {   // No englobing variable in the avoiding list 2
                        // Look for variables in avoiding list 2 englobed by 'var'
                        ext_sym_set aux_set; aux_set.insert(*it);
                        for (ext_sym_set::iterator ita = avoid_l_2.begin(); ita != avoid_l_2.end(); ++ita)
                        {
                            if (ext_sym_set_contains_englobing_nodecl(ita->get_nodecl(), aux_set))
                            {   // Delete from var the englobed part of (*ita) and put the result in 'var'
                                // TODO
                                std::cerr << "warning: Part of nodecl founded in the current var that must be avoided " << std::endl;
                            }
                        }
                        new_l = insert_var_in_list(var, new_l);
                    }
                }
            }
            return new_l;
        }
    
        ObjectList<ext_sym_set> Node::get_use_def_over_nodes()
        {
            ObjectList<ext_sym_set> use_def, use_def_aux;
            
            if (!_visited)          
            {
                _visited = true;
               
                // Use-Def in current node
                ext_sym_set ue_vars = get_ue_vars();
                ext_sym_set killed_vars = get_killed_vars();
                ext_sym_set undef_vars = get_undefined_behaviour_vars();
                
                // Concatenate info from children nodes
                ObjectList<Node*> children = get_children();
                ext_sym_set ue_children, killed_children, undef_children;
                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    use_def_aux = (*it)->get_use_def_over_nodes();
                    if (!use_def_aux.empty())
                    {
                        ue_children.insert(use_def_aux[0]);
                        killed_children.insert(use_def_aux[1]);
                        undef_children.insert(use_def_aux[2]);
                    }
                }
                
                // Append to current node info from children
                ue_vars = compute_use_def_with_children(ue_children, ue_vars, killed_vars, undef_children);
                killed_vars = compute_use_def_with_children(killed_children, killed_vars, killed_vars, undef_children);
                ext_sym_set null_list;
                undef_vars = compute_use_def_with_children(undef_children, undef_vars, killed_vars, null_list);
                
                use_def.append(ue_vars); use_def.append(killed_vars); use_def.append(undef_vars);
            }
            
            return use_def;
        }
    
        void Node::set_graph_node_use_def()
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                if (!_visited)
                {
                    _visited = true;
                    
                    Node* entry_node = get_data<Node*>(_ENTRY_NODE);
                
                    // Get upper_exposed, killed and undefined variables
                    ObjectList<ext_sym_set> use_def = entry_node->get_use_def_over_nodes();
                    set_data(_UPPER_EXPOSED, use_def[0]);
                    set_data(_KILLED, use_def[1]);
                    set_data(_UNDEF, use_def[2]);
                }
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
                ext_sym_set inner_live_in = entry_node->get_live_in_over_nodes();
                ext_sym_set outer_live_in;
                if (get_scope().is_valid())
                {   // Delete those variables who are local to the graph
                    for(ext_sym_set::iterator it = inner_live_in.begin(); it != inner_live_in.end(); ++it)
                        if (!it->get_symbol().get_scope().scope_is_enclosed_by(get_scope()) 
                            && it->get_symbol().get_scope() != get_scope())
                            outer_live_in.append(*it);
                }
                else
                    outer_live_in = inner_live_in;
                set_data(_LIVE_IN, outer_live_in);
                
                // Get live out variables from exit node parents
                Node* exit_node = get_data<Node*>(_EXIT_NODE);
                ext_sym_set inner_live_out = exit_node->get_live_out_over_nodes();
                ext_sym_set outer_live_out;
                if (get_scope().is_valid())
                {   // Delete those variables who are local to the graph
                    for(ext_sym_set::iterator it = inner_live_out.begin(); it != inner_live_out.end(); ++it)
                        if (!it->get_symbol().get_scope().scope_is_enclosed_by(get_scope())
                            && it->get_symbol().get_scope() != get_scope() )
                            outer_live_out.append(*it);
                }
                else
                    outer_live_out = inner_live_out;
                set_data(_LIVE_OUT, outer_live_out);
            }
            else
            {
                internal_error("Getting inner liveness analysis from node '%d' with type '%s' while "
                                "here it is mandatory a Graph node.\n",
                            _id, get_type_as_string().c_str());
            }
        }

        void Node::set_graph_node_reaching_definitions()
        {
            if (get_data<Node_type>(_NODE_TYPE) == GRAPH_NODE)
            {
                set_data(_REACH_DEFS, get_data<Node*>(_EXIT_NODE)->get_data<nodecl_map>(_REACH_DEFS));
            }
            else
            {
                internal_error("Propagating reaching definitions in node '%d' with type '%s' while "
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
            
            if (new_ue_var.get_nodecl().is<Nodecl::Div>() || new_ue_var.get_nodecl().is<Nodecl::Minus>())
                std::cerr << "No hauriem d'estar introdutint " << new_ue_var.get_nodecl().prettyprint() << std::endl;
            
            if (this->has_key(_UPPER_EXPOSED))
            {   
                ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
            }
            ue_vars.insert(new_ue_var);
            
            set_data(_UPPER_EXPOSED, ue_vars);
        }
        
        void Node::set_ue_var(ext_sym_set new_ue_vars)
        {
            ext_sym_set ue_vars;
       
            for (ext_sym_set::iterator it = new_ue_vars.begin(); it != new_ue_vars.end(); ++it)
                if (it->get_nodecl().is<Nodecl::Div>() || it->get_nodecl().is<Nodecl::Minus>())
                {   
                    std::cerr << "No hauriem d'estar introdutint " << it->get_nodecl().prettyprint() << " (llista)!" << std::endl;
                }
            if (this->has_key(_UPPER_EXPOSED))
            {   
                ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
            }
            ue_vars.insert(new_ue_vars);
            
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
        
        void Node::set_killed_var(ext_sym_set new_killed_vars)
        {
            ext_sym_set killed_vars;

            if (has_key(_KILLED))
            {
                killed_vars = get_data<ext_sym_set>(_KILLED);
            }
            killed_vars.insert(new_killed_vars);
            
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
        
        void Node::set_undefined_behaviour_var(ExtensibleSymbol new_undef_var)
        {
            ext_sym_set undef_vars;
            
            if (has_key(_UNDEF))
            {
                undef_vars = get_data<ext_sym_set>(_UNDEF);
            }
            undef_vars.insert(new_undef_var);
            
            set_data(_UNDEF, undef_vars);
        }

        void Node::unset_undefined_behaviour_var(ExtensibleSymbol old_undef_var)
        {
            ext_sym_set undef_vars;
            
            if (has_key(_UNDEF))
            {
                undef_vars = get_data<ext_sym_set>(_UNDEF);
                undef_vars = undef_vars.not_find(old_undef_var);
            }
            
            set_data(_UNDEF, undef_vars);   
        }

        void Node::set_undefined_behaviour_var(ext_sym_set new_undef_vars)
        {
            ext_sym_set undef_vars;
            
            if (has_key(_UNDEF))
            {
                undef_vars = get_data<ext_sym_set>(_UNDEF);
            }
            undef_vars.insert(new_undef_vars);
            
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

        void Node::set_input_deps(ext_sym_set new_input_deps)
        {
            ext_sym_set input_deps;
            
            if (has_key(_IN_DEPS))
            {
                input_deps = get_data<ext_sym_set>(_IN_DEPS);
            }
            
            input_deps.insert(new_input_deps);
            set_data(_IN_DEPS, input_deps);;
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
       
        void Node::set_output_deps(ext_sym_set new_output_deps)
        {
            ext_sym_set output_deps;
            
            if (has_key(_OUT_DEPS))
            {
                output_deps = get_data<ext_sym_set>(_OUT_DEPS);
            }
            
            output_deps.insert(new_output_deps);
            set_data(_OUT_DEPS, output_deps);
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
                
        void Node::set_inout_deps(ext_sym_set new_inout_deps)
        {
            ext_sym_set inout_deps;
            
            if (has_key(_INOUT_DEPS))
            {
                inout_deps = get_data<ext_sym_set>(_INOUT_DEPS);
            }
            
            inout_deps.insert(new_inout_deps);
            set_data(_INOUT_DEPS, inout_deps);
        }
        
        ext_sym_set Node::get_undef_deps()
        {
            ext_sym_set undef_deps;
              
            if (has_key(_UNDEF_DEPS))
            {
              undef_deps = get_data<ext_sym_set>(_UNDEF_DEPS);
            }
              
            return undef_deps;
        }
                
        void Node::set_undef_deps(ext_sym_set new_undef_deps)
        {
            ext_sym_set undef_deps;
              
            if (has_key(_UNDEF_DEPS))
            {
               undef_deps = get_data<ext_sym_set>(_UNDEF_DEPS);
            }
              
            undef_deps.insert(new_undef_deps);
            set_data(_UNDEF_DEPS, undef_deps);
        }
        
        ext_sym_set Node::get_shared_vars()
        {
            ext_sym_set shared_vars;
              
            if (has_key(_SHARED))
            {
                shared_vars = get_data<ext_sym_set>(_SHARED);
            }
              
            return shared_vars;
        }
                
        void Node::set_shared_var(ExtensibleSymbol ei)
        {
            ext_sym_set shared_vars;
  
            if (has_key(_SHARED))
            {
                shared_vars = get_data<ext_sym_set>(_SHARED);
            }
            
            shared_vars.insert(ei);
            set_data(_SHARED, shared_vars);
        }

        ext_sym_set Node::get_private_vars()
        {
            ext_sym_set private_vars;
              
            if (has_key(_PRIVATE))
            {
                private_vars = get_data<ext_sym_set>(_PRIVATE);
            }
              
            return private_vars;
        }
        
        void Node::set_private_var(ExtensibleSymbol ei)
        {
            ext_sym_set private_vars;
              
            if (has_key(_PRIVATE))
            {
                private_vars = get_data<ext_sym_set>(_PRIVATE);
            }
              
            private_vars.insert(ei);
            set_data(_PRIVATE, private_vars);
        }
        
        ext_sym_set Node::get_firstprivate_vars()
        {
            ext_sym_set firstprivate_vars;
              
            if (has_key(_FIRSTPRIVATE))
            {
                firstprivate_vars = get_data<ext_sym_set>(_FIRSTPRIVATE);
            }
              
            return firstprivate_vars;
        }
        
        void Node::set_firstprivate_var(ExtensibleSymbol ei)
        {
            ext_sym_set firstprivate_vars;
              
            if (has_key(_FIRSTPRIVATE))
            {
                firstprivate_vars = get_data<ext_sym_set>(_FIRSTPRIVATE);
            }
              
            firstprivate_vars.insert(ei);
            set_data(_FIRSTPRIVATE, firstprivate_vars);
        }
        
        ext_sym_set Node::get_undef_sc_vars()
        {
            ext_sym_set undef_sc_vars;
              
            if (has_key(_UNDEF_SC))
            {
                undef_sc_vars = get_data<ext_sym_set>(_UNDEF_SC);
            }
              
            return undef_sc_vars;
        }
        
        void Node::set_undef_sc_var(ExtensibleSymbol ei)
        {
            ext_sym_set undef_sc_vars;
              
            if (has_key(_UNDEF_SC))
            {
                undef_sc_vars = get_data<ext_sym_set>(_UNDEF_SC);
            }
              
            undef_sc_vars.insert(ei);
            set_data(_UNDEF_SC, undef_sc_vars);
        }
        
        void Node::set_undef_sc_var(ext_sym_set new_undef_sc_vars)
        {
            ext_sym_set undef_sc_vars;
              
            if (has_key(_UNDEF_SC))
            {
                undef_sc_vars = get_data<ext_sym_set>(_UNDEF_SC);
            }
              
            undef_sc_vars.insert(new_undef_sc_vars);
            set_data(_UNDEF_SC, undef_sc_vars);
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
        
        void Node::print_use_def_chains()
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose)
            {
                ext_sym_set ue_vars = get_data<ext_sym_set>(_UPPER_EXPOSED);
                std::cerr << std::endl << "      - UE VARS: ";
                for(ext_sym_set::iterator it = ue_vars.begin(); it != ue_vars.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint() << ", ";
                }
                std::cerr << std::endl;
                
                ext_sym_set killed_vars = get_data<ext_sym_set>(_KILLED);
                std::cerr << "      - KILLED VARS: ";
                for(ext_sym_set::iterator it = killed_vars.begin(); it != killed_vars.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint() << ", ";
                }
                std::cerr << std::endl;
                
                ext_sym_set undef_vars = get_data<ext_sym_set>(_UNDEF);
                std::cerr << "      - UNDEF VARS: ";
                for(ext_sym_set::iterator it = undef_vars.begin(); it != undef_vars.end(); ++it)
                {
                  std::cerr << it->get_nodecl().prettyprint() << ", ";
                }
                std::cerr << std::endl;
            }
        }
        
        void Node::print_liveness()
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose)
            {
                ext_sym_set live_in_vars = get_data<ext_sym_set>(_LIVE_IN);
                std::cerr << std::endl << "      - LIVE IN VARS: ";
                for(ext_sym_set::iterator it = live_in_vars.begin(); it != live_in_vars.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint() << ", ";
                }
                std::cerr << std::endl;
            
                ext_sym_set live_out_vars = get_data<ext_sym_set>(_LIVE_OUT);
                std::cerr << "      - LIVE OUT VARS: ";
                for(ext_sym_set::iterator it = live_out_vars.begin(); it != live_out_vars.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint() << ", ";
                }
                std::cerr << std::endl;
            }
        }
        
        void Node::print_task_dependencies()
        {
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
            {
                ext_sym_set private_deps = get_private_vars();
                std::cerr << std::endl << "     - Private(";
                for(ext_sym_set::iterator it = private_deps.begin(); it != private_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != private_deps.end()-1)
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;

                ext_sym_set firstprivate_deps = get_firstprivate_vars();
                std::cerr << "     - Firstprivate(";
                for(ext_sym_set::iterator it = firstprivate_deps.begin(); it != firstprivate_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != firstprivate_deps.end()-1)
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
                
                ext_sym_set input_deps = get_input_deps();
                std::cerr << "     - Input(";
                for(ext_sym_set::iterator it = input_deps.begin(); it != input_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != input_deps.end()-1)
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
                    
                ext_sym_set output_deps = get_output_deps();
                std::cerr << "     - Output(";
                for(ext_sym_set::iterator it = output_deps.begin(); it != output_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != output_deps.end()-1)
                       std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
                    
                ext_sym_set inout_deps = get_inout_deps();
                std::cerr << "     - Inout(";
                for(ext_sym_set::iterator it = inout_deps.begin(); it != inout_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != inout_deps.end()-1)
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
                
                ext_sym_set undef_deps = get_undef_deps();
                std::cerr << "     - Undef(";
                for(ext_sym_set::iterator it = undef_deps.begin(); it != undef_deps.end(); ++it)
                {
                    std::cerr << it->get_nodecl().prettyprint();
                    if (it != undef_deps.end()-1)
                        std::cerr << ", ";
                }
                std::cerr << ")" << std::endl;
            }
        }
    }
}