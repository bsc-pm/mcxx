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


#include "tl-extensible-graph.hpp"

namespace TL
{  
    ExtensibleGraph::ExtensibleGraph(std::string name)
        : _graph(NULL), _name(name), _nid(-1),
          _function_sym(NULL), _function_calls(), nodes_m(),
          _continue_stack(), _break_stack(),
          _labeled_node_l(), _goto_node_l(),
          _last_nodes(), _outer_node(), 
          _task_nodes_l(), _use_def_computed(false)
    {
        _graph = create_graph_node(NULL, Nodecl::NodeclBase::null(), EXTENSIBLE_GRAPH);
        _last_nodes.append(_graph->get_graph_entry_node());
    }

    ExtensibleGraph* ExtensibleGraph::copy()
    {
        ExtensibleGraph* new_ext_graph = new ExtensibleGraph(this->_name);
        
        new_ext_graph->_nid = this->_nid;
        new_ext_graph->_name = this->_name;
        new_ext_graph->_function_sym = this->_function_sym;
        new_ext_graph->_function_calls = this->_function_calls;
        new_ext_graph->_continue_stack = this->_continue_stack;
        new_ext_graph->_break_stack = this->_break_stack;
        new_ext_graph->_labeled_node_l = this->_labeled_node_l;
        new_ext_graph->_goto_node_l = this->_goto_node_l;
        new_ext_graph->_last_nodes = this->_last_nodes;
        new_ext_graph->_outer_node = this->_outer_node;
        new_ext_graph->_task_nodes_l = this->_task_nodes_l;
        new_ext_graph->_use_def_computed = this->_use_def_computed;
       
        // First, just copy the nodes and create a map connecting the old nodes with the new nodes
        new_ext_graph->copy_and_map_nodes(_graph);
        clear_visits(_graph);
        
        // Now, we can connect all the nodes
        new_ext_graph->connect_copied_nodes(_graph);
        clear_visits(_graph);
        
        return new_ext_graph;
    }

    void ExtensibleGraph::copy_and_map_nodes(Node* old_node)
    {
        if (!old_node->is_visited())
        {
            old_node->set_visited(true);
            Node* new_node;
            
            Node_type ntype = old_node->get_type();
            
            // Create the node
            switch (ntype)
            {
                case GRAPH_NODE:
                {
                    // Get the outer node of the new node
                    Node* outer_node = NULL;
                    bool most_outer_node = true;
                    if (nodes_m.find(old_node->get_outer_node()) != nodes_m.end())
                    {   // We are not in the most outer node of the ExtensibleGraph
                        outer_node == nodes_m[old_node->get_outer_node()];
                        most_outer_node = false;
                    }
                    
                    int id = old_node->get_id() - 1;
                    new_node = new Node(id, ntype, outer_node);
                    
                    if (most_outer_node)
                    {
                        _graph = new_node;
                    }
                    
                    // Set the label and the graph type
                    Nodecl::NodeclBase label = old_node->get_graph_label();
                    Graph_type graph_type = old_node->get_graph_type();
                    new_node->set_graph_label(label);
                    new_node->set_graph_type(graph_type);
                    
                    // Set additional info for pragma nodes
                    if (graph_type == OMP_PRAGMA || graph_type == TASK)
                    {
                        ObjectList<Nodecl::NodeclBase> clauses = old_node->get_data<ObjectList<Nodecl::NodeclBase> >(_CLAUSES);
                        ObjectList<Nodecl::NodeclBase> args = old_node->get_data<ObjectList<Nodecl::NodeclBase> >(_ARGS);
                        
                        new_node->set_data(_CLAUSES, clauses);
                        new_node->set_data(_ARGS, args);
                    }
                   
                    break;
                }
                case BASIC_ENTRY_NODE:                
                case BASIC_EXIT_NODE:                   
                case BASIC_LABELED_NODE:
                case BASIC_GOTO_NODE:
                case BASIC_BREAK_NODE:
                case FLUSH_NODE:
                case BARRIER_NODE:
                case BASIC_PRAGMA_DIRECTIVE_NODE:
                case BASIC_FUNCTION_CALL_NODE:
                case UNCLASSIFIED_NODE:
                case BASIC_NORMAL_NODE:
                {
                    int id = old_node->get_id() - 1;
                    new_node = new Node(id, ntype, nodes_m[old_node->get_outer_node()]);
                    break;
                }
                default: 
                {
                    internal_error("Unexpected kind of node '%s' while copying a Graph",
                                   old_node->get_type_as_string().c_str());
                }
            }
            
            // Set some special attributes of the node
            if (ntype == BASIC_LABELED_NODE || ntype == BASIC_GOTO_NODE)
            {   // We set this value here because before the node is not already created
                new_node->set_label(old_node->get_label());
            }
            else if (ntype == BASIC_ENTRY_NODE)
            {
                Node* new_outer = nodes_m[old_node->get_outer_node()];
                new_outer->set_graph_entry_node(new_node);
            }
            else if (ntype == BASIC_EXIT_NODE)
            {
                Node* new_outer = nodes_m[old_node->get_outer_node()];
                new_outer->set_graph_exit_node(new_node);          
            }
            else if (ntype == BASIC_LABELED_NODE || ntype == BASIC_NORMAL_NODE)
            {
                new_node->set_statements(old_node->get_statements());
            }
            
            // Set some other properties of the node
            if (old_node->has_key(_LIVE_IN))
            {   // Liveness analysis has been performed, so we copy this information too
                new_node->set_data(_LIVE_IN, old_node->get_data<ext_sym_set>(_LIVE_IN));
                new_node->set_data(_LIVE_OUT, old_node->get_data<ext_sym_set>(_LIVE_OUT));
                new_node->set_data(_UPPER_EXPOSED, old_node->get_data<ext_sym_set>(_UPPER_EXPOSED));
                new_node->set_data(_KILLED, old_node->get_data<ext_sym_set>(_KILLED));
            }
            if (old_node->has_key(_IN_DEPS))
            {   // Auto-deps analysis has been performed
                new_node->set_data(_IN_DEPS, old_node->get_data<ext_sym_set>(_IN_DEPS));
                new_node->set_data(_OUT_DEPS, old_node->get_data<ext_sym_set>(_OUT_DEPS));
                new_node->set_data(_INOUT_DEPS, old_node->get_data<ext_sym_set>(_INOUT_DEPS));                    
            }
            
            // Append the new node to the mapping structure
            nodes_m[old_node] = new_node;
                   
            if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            else if (ntype == GRAPH_NODE)
            {   // Copy the inner nodes of the graph node
                copy_and_map_nodes(old_node->get_graph_entry_node());
            }            
            
            // Copy the children of the actual node
            ObjectList<Node*> children = old_node->get_children();
            for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                copy_and_map_nodes(*it);
            }
        }
    }
   
    void ExtensibleGraph::connect_copied_nodes(Node* old_node)
    {
        if (!old_node->is_visited())
        {
            old_node->set_visited(true);
            
            switch(old_node->get_type())
            {
                case BASIC_EXIT_NODE: return;
                case GRAPH_NODE: connect_copied_nodes(old_node->get_graph_entry_node());
                default:
                {
                    // Connect the node with its parents 
                    ObjectList<Edge*> old_entry_edges = old_node->get_entry_edges();
                    ObjectList<Node*> new_parents;
                    ObjectList<Edge*> new_entry_edges;
                    for (ObjectList<Edge*>::iterator it = old_entry_edges.begin(); it != old_entry_edges.end(); ++it)
                    {
                        Node* new_source = nodes_m[(*it)->get_source()];
                        Node* new_target = nodes_m[old_node];
                        new_parents.append(new_source);
                        new_entry_edges.append(new Edge(new_source, new_target, (*it)->is_back_edge(), (*it)->get_type(), (*it)->get_label()));
                    }
                    connect_nodes(new_parents, nodes_m[old_node], new_entry_edges);
                    
                    // Connect the node with its children
                    ObjectList<Edge*> old_exit_edges = old_node->get_exit_edges();
                    ObjectList<Node*> new_children;
                    ObjectList<Edge*> new_exit_edges;        
                    for (ObjectList<Edge*>::iterator it = old_exit_edges.begin(); it != old_exit_edges.end(); ++it)
                    {
                        Node* new_source = nodes_m[old_node];
                        Node* new_target = nodes_m[(*it)->get_target()];
                        new_children.append(new_target);
                        new_exit_edges.append(new Edge(new_source, new_target, (*it)->is_back_edge(), (*it)->get_type(), (*it)->get_label()));   
                    }
                    connect_nodes(nodes_m[old_node], new_children, new_exit_edges);
                    
                    ObjectList<Node*> old_children = old_node->get_children();
                    for (ObjectList<Node*>::iterator it = old_children.begin(); it != old_children.end(); ++it)
                    {
                        connect_copied_nodes(*it);
                    }
                }
            }
        }
    }

    Node* ExtensibleGraph::append_new_node_to_parent(ObjectList<Node*> parents, ObjectList<Nodecl::NodeclBase> nodecls,
                                                    Node_type ntype, Edge_type etype)
    {
        if (ntype == GRAPH_NODE)
        {
            internal_error("A Graph node must be created with the function 'create_graph_node' "
                        "and connected by hand [new id = %d]", _nid);
        }

        if (!parents.empty())
        {
            Node* new_node;
            if (!nodecls.empty())
            {
                new_node = new Node(_nid, ntype, _outer_node.top());
                new_node->set_statements(nodecls);
                connect_nodes(parents, new_node, etype);
            }
            else if (nodecls.empty() && ntype != BASIC_NORMAL_NODE)
            {
                new_node = new Node(_nid, ntype, _outer_node.top());
                connect_nodes(parents, new_node, etype);   
            }
            return new_node;
        }
        else
        {
            internal_error("Cannot create the new node '%d' to a NULL parent", _nid+1);
        }
    }

    Node* ExtensibleGraph::append_new_node_to_parent(Node* parent, Nodecl::NodeclBase nodecl,
                                                    Node_type ntype, Edge_type etype)
    {
        return append_new_node_to_parent(ObjectList<Node*>(1, parent), ObjectList<Nodecl::NodeclBase>(1, nodecl), ntype, etype);
    }

    Node* ExtensibleGraph::append_new_node_to_parent(Node* parent, ObjectList<Nodecl::NodeclBase> nodecl,
                                                    Node_type ntype, Edge_type etype)
    {
        return append_new_node_to_parent(ObjectList<Node*>(1, parent), nodecl, ntype, etype);
    }

    Node* ExtensibleGraph::append_new_node_to_parent(ObjectList<Node*> parents, Nodecl::NodeclBase nodecl,
                                                    Node_type ntype, Edge_type etype)
    {
        return append_new_node_to_parent(parents, ObjectList<Nodecl::NodeclBase>(1, nodecl), ntype, etype);
    }

    Edge* ExtensibleGraph::connect_nodes(Node* parent, Node* child, Edge_type etype, std::string label, bool is_back_edge)
    {
        if (parent != NULL && child != NULL)
        {
            if (!parent->has_child(child))
            {
//                 std::cerr << "Connecting " << parent->get_id() << " with " << child->get_id() << std::endl;
                Edge* new_edge = new Edge(parent, child, is_back_edge, etype, label);
                parent->set_exit_edge(new_edge);
                child->set_entry_edge(new_edge);
                return new_edge;
            }
        }
        else
        {
            internal_error("Using a NULL node when connecting two nodes. Parent is NULL? '%s', Child is NULL? '%s'",
                           (parent == NULL)?"true":"false", (child == NULL)?"true":"false");
        }
    }

    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, ObjectList<Node*> children, 
                                        ObjectList<Edge_type> etypes, ObjectList<std::string> elabels)
    {
        if ((etypes.size() != elabels.size()) || (parents.size() * children.size() != etypes.size()))
        {
            internal_error("Wrong list size while connecting a list of nodes as children of " 
                           "other node (parents '%d', children '%d', edge types '%d', edge labels '%d')\n",
                           parents.size(), children.size(), etypes.size(), elabels.size());
        }
        
        int children_size = children.size();
        ObjectList<Edge_type>::iterator itt = etypes.begin();
        ObjectList<std::string>::iterator itl = elabels.begin();
        for(ObjectList<Node*>::iterator it = parents.begin();
            it != parents.end();
            ++it, itt+=children_size, itl+=children_size)
        {
            ObjectList<Edge_type> actual_etypes(itt, itt + children_size);
            ObjectList<std::string> actual_elabels(itl, itl + children_size);
            connect_nodes(*it, children, actual_etypes, actual_elabels);
        }
    }

    void ExtensibleGraph::connect_nodes(Node* parent, ObjectList<Node*> children, 
                                        ObjectList<Edge_type> etypes, ObjectList<std::string> labels)
    {
        ObjectList<Edge_type>::iterator itt = etypes.begin();
        ObjectList<std::string>::iterator itl = labels.begin();
        ObjectList<Node*>::iterator it = children.begin();
        for(;
            it != children.end(), itt != etypes.end(), itl != labels.end();
            ++it, ++itt, ++itl)
        {
            connect_nodes(parent, *it, *itt, *itl);
        }
        
        if (it != children.end() || itt != etypes.end() || itl != labels.end())
        {
            internal_error("Wrong list size while connecting a list of nodes as children of "
                           "other node (children '%d', edge types '%d', edge labels '%d')\n",
                           children.size(), etypes.size(), labels.size());
        }
    }

    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, Node* child, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> labels)
    {
        ObjectList<Edge_type>::iterator itt = etypes.begin();
        ObjectList<std::string>::iterator itl = labels.begin();
        ObjectList<Node*>::iterator it = parents.begin();
        for(;
            it != parents.end(), itt != etypes.end(), itl != labels.end();
            ++it, ++itt, ++itl)
        {
            connect_nodes(*it, child, *itt, *itl);
        }
        
        if (it != parents.end() || itt != etypes.end() || itl != labels.end())
        {
            internal_error("Wrong list size while connecting a list of nodes as parent of "
                           "other node (parents '%d', edge types '%d', edge labels '%d')\n",
                           parents.size(), etypes.size(), labels.size());
        }        
    }

    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, Node* child, Edge_type etype, std::string label, bool is_back_edge)
    {
        for(ObjectList<Node*>::iterator it = parents.begin();
            it != parents.end();
            ++it)
        {
            connect_nodes(*it, child, etype, label, is_back_edge);
        }
    }

    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, Node* child, ObjectList<Edge*> edges)
    {
        ObjectList<Node*>::iterator itn = parents.begin();
        ObjectList<Edge*>::iterator ite = edges.begin();
        for (; itn!=parents.end(), ite!=edges.end(); ++itn, ++ite)
        {
            if (!child->has_parent(*itn))
            {
                (*itn)->set_exit_edge(*ite);
                child->set_entry_edge(*ite);                
            }
            else
            {
//                 edges.erase(ite);
//                 --ite;
            }
        }
    }

    void ExtensibleGraph::connect_nodes(Node* parent, ObjectList<Node*> children, ObjectList<Edge*> edges)
    {
        ObjectList<Node*>::iterator itn = children.begin();
        ObjectList<Edge*>::iterator ite = edges.begin();
        for (; itn!=children.end(), ite!=edges.end(); ++itn, ++ite)
        {
            if (!parent->has_child(*itn))
            {
                (*itn)->set_entry_edge(*ite);
                parent->set_exit_edge(*ite);
            }
            else
            {
//                 edges.erase(ite);
//                 --ite;
            }
        }
    }

    void ExtensibleGraph::disconnect_nodes(ObjectList<Node*> parents, Node* child)
    {
        for(ObjectList<Node*>::iterator it = parents.begin();
                it != parents.end();
                ++it)
        {
            disconnect_nodes(*it, child);
        }
    }    

    void ExtensibleGraph::disconnect_nodes(Node* parent, ObjectList<Node*> children)
    {
        for(ObjectList<Node*>::iterator it = children.begin();
                it != children.end();
                ++it)
        {
            disconnect_nodes(parent, *it);
        }
    }

    void ExtensibleGraph::disconnect_nodes(Node *parent, Node *child)
    {
        parent->erase_exit_edge(child);
        child->erase_entry_edge(parent);
    }

    Node* ExtensibleGraph::create_graph_node(Node* outer_node, Nodecl::NodeclBase label, 
                                             Graph_type graph_type, Nodecl::NodeclBase context)
    {
        Node* result = new Node(_nid, GRAPH_NODE, outer_node);
        
        Node* entry_node = result->get_graph_entry_node();
        entry_node->set_outer_node(result);
        Node* exit_node = result->get_graph_exit_node();
        exit_node->set_outer_node(result);
    
        result->set_graph_label(label);
        result->set_graph_type(graph_type);
        if (graph_type == TASK)
        {    
            result->set_task_context(context);
        }
        
        _outer_node.push(result);
        
        return result;
    }
    
    void ExtensibleGraph::create_barrier_node(Node* outer_node)
    {
        Node* flush_node = new Node(_nid, FLUSH_NODE, outer_node);
        connect_nodes(_last_nodes, flush_node);
        Node* barrier_node = new Node(_nid, BARRIER_NODE, outer_node);
        connect_nodes(flush_node, barrier_node);
        flush_node = new Node(_nid, FLUSH_NODE, outer_node);
        connect_nodes(barrier_node, flush_node);
        _last_nodes.clear(); _last_nodes.append(flush_node);
    }
    
    Node* ExtensibleGraph::create_unconnected_node(Nodecl::NodeclBase nodecl)
    {
        Node* result = new Node(_nid, BASIC_NORMAL_NODE, _outer_node.top());
        result->set_statements(ObjectList<Nodecl::NodeclBase>(1, nodecl));
        
        return result;
    }
   
    void ExtensibleGraph::delete_node(Node* n)
    {
        // Delete the node from its parents
        ObjectList<Node*> entry_nodes = n->get_parents();
        for(ObjectList<Node*>::iterator it = entry_nodes.begin();
            it != entry_nodes.end();
            ++it)
        {
            (*it)->erase_exit_edge(n);
        }
        
        // Delete the node from its children
        ObjectList<Node*> exit_nodes = n->get_children();
        for(ObjectList<Node*>::iterator it = exit_nodes.begin();
            it != exit_nodes.end();
            ++it)
        {
            (*it)->erase_entry_edge(n);
        }
        
        // Delete the node
        delete (n);
    }
    
    void ExtensibleGraph::dress_up_graph()
    {
        clear_unnecessary_nodes();
//         concat_sequential_nodes();
    }
    
    void ExtensibleGraph::concat_sequential_nodes()
    {
        Node* entry = _graph->get_graph_entry_node();

        ObjectList<Node*> seq_l;
        concat_sequential_nodes_recursive(entry, seq_l);
        
        clear_visits(entry);
    }
    
    void ExtensibleGraph::concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            Node_type ntype = actual_node->get_type();
            
            if (ntype != BASIC_ENTRY_NODE)
            {
                if (ntype != BASIC_NORMAL_NODE || actual_node->get_exit_edges().size() > 1
                    || actual_node->get_entry_edges().size() > 1)
                {
                    concat_nodes(last_seq_nodes);                    
                    last_seq_nodes.clear();
                    
                    if (ntype == GRAPH_NODE)
                    {
                        concat_sequential_nodes_recursive(actual_node->get_graph_entry_node(), last_seq_nodes);
                    }
                    else if (ntype == BASIC_EXIT_NODE)
                    {
                        return;
                    }
                    
                }
                else if (ntype != BASIC_ENTRY_NODE)
                {
                    last_seq_nodes.append(actual_node);
                }
            }
            
            ObjectList<Node*> actual_exits = actual_node->get_children();
            for(ObjectList<Node*>::iterator it = actual_exits.begin();
                it != actual_exits.end();
                ++it)
            {
                concat_sequential_nodes_recursive(*it, last_seq_nodes);
            }
        }
    }
    
    void ExtensibleGraph::concat_nodes(ObjectList<Node*> node_l)
    {
        if (!node_l.empty())
        {
            if (node_l.size() != 1)
            {
                // Create the new node
                ObjectList<Nodecl::NodeclBase> stmt_l;
                for(ObjectList<Node*>::iterator it = node_l.begin();
                    it != node_l.end();
                    ++it)
                {
                    stmt_l.append((*it)->get_statements());
                }
                Node* new_node = new Node(_nid, BASIC_NORMAL_NODE, node_l[0]->get_outer_node(), stmt_l);
                new_node->set_visited(true);
                Node* front = node_l.front();
                Node* back = node_l.back();
                ObjectList<Node*> front_parents = front->get_parents();
                ObjectList<Edge_type> front_entry_edge_types = front->get_entry_edge_types();
                ObjectList<std::string> front_entry_edge_labels = front->get_entry_edge_labels();
                ObjectList<Node*> back_children = back->get_children();
                ObjectList<Edge_type> back_exit_edge_types = back->get_exit_edge_types();
                ObjectList<std::string> back_exit_edge_labels = back->get_exit_edge_labels();

                // Destroy the nodes which has been concatenated
                for(ObjectList<Node*>::iterator it = node_l.begin();
                    it != node_l.end();
                    ++it)
                {
                    delete_node(*it);
                }
                
                // Connect the node
                connect_nodes(front_parents, new_node, front_entry_edge_types, front_entry_edge_labels);
                connect_nodes(new_node, back_children, back_exit_edge_types, back_exit_edge_labels);
            }
        }
        else
        {
            std::cerr << "warning: trying to concatenate an empty list of nodes" << std::endl;
        }
    }

    void ExtensibleGraph::recompute_identifiers(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
          
            actual_node->set_id(++_nid);
            
            switch(actual_node->get_type())
            {
                case BASIC_EXIT_NODE:   return;
                case GRAPH_NODE:        recompute_identifiers(actual_node->get_graph_entry_node());
                case BASIC_BREAK_NODE:
                case BASIC_ENTRY_NODE:  
                case FLUSH_NODE:
                case BARRIER_NODE:
                case UNCLASSIFIED_NODE:
                case BASIC_PRAGMA_DIRECTIVE_NODE:
                case BASIC_FUNCTION_CALL_NODE:
                case BASIC_NORMAL_NODE:
                case BASIC_LABELED_NODE: 
                {
                    ObjectList<Node*> children = actual_node->get_children();
                    for (ObjectList<Node*>::iterator it = children.begin();
                        it != children.end();
                        ++it)
                    {
                        recompute_identifiers(*it);
                    }
                }
            }
        }
    }  

    void ExtensibleGraph::replace_node(Node* old_node, Node* new_node)
    {
        if (!new_node->get_entry_edges().empty() || !new_node->get_exit_edges().empty())
        {
            internal_error("Trying to replace a node with a connected node. The new node must be unconnected", 0);
        }
        
        // Recompute identifiers to avoid duplicities
        recompute_identifiers(new_node);
        clear_visits(new_node);
        
        // Reconnect the nodes properly
        ObjectList<Node*> parents = old_node->get_parents();
        ObjectList<Node*> children = old_node->get_children();
        
        ObjectList<Edge_type> entry_edge_types = old_node->get_entry_edge_types();
        ObjectList<std::string> entry_edge_labels = old_node->get_entry_edge_labels();
        ObjectList<Edge_type> exit_edge_types = old_node->get_exit_edge_types();
        ObjectList<std::string> exit_edge_labels = old_node->get_exit_edge_labels();
     
        disconnect_nodes(parents, old_node);
        disconnect_nodes(old_node, children);
       
        connect_nodes(parents, new_node, entry_edge_types, entry_edge_labels);
        connect_nodes(new_node, children, exit_edge_types, exit_edge_labels);
        
        // Set some properties of the old node to the new node
        new_node->set_outer_node(old_node->get_outer_node());
        
        // Destroy the old node
        delete(old_node);
    }
    
    void ExtensibleGraph::clear_unnecessary_nodes()
    {   
//         std::cerr << "Clearing unnecessary nodes" << std::endl;
        // Clear all the Entry / Exit nodes except the first and the last ones
        Node* entry = _graph->get_graph_entry_node();
        Node* exit = _graph->get_graph_exit_node();
       
        // FIXME Can this case happen in the Nodecl Graph construction way???
//         clear_orphaned_nodes(exit);
//         clear_visits(entry);
        
        erase_unclassified_nodes(entry);
        clear_visits(entry);
        
        erase_break_nodes(entry);
        clear_visits(entry);
    }
    
    void ExtensibleGraph::clear_orphaned_nodes(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            ObjectList<Edge*> entries = actual_node->get_entry_edges();
            
            Node_type ntype = actual_node->get_type();
            if (entries.empty() && ntype != BASIC_ENTRY_NODE)
            {
                clear_orphaned_cascade(actual_node);
            }
            else
            {
                if (ntype == GRAPH_NODE)
                {   // Traverse the inner nodes
                    clear_orphaned_nodes_in_subgraph(actual_node->get_graph_exit_node());
                }
                // Continue with the outer traversal
                for (ObjectList<Edge*>::iterator it = entries.begin();
                        it != entries.end();
                        ++it)
                {
                    clear_orphaned_nodes((*it)->get_source());
                }
            }
        }
        return;
    }
    
    void ExtensibleGraph::clear_orphaned_nodes_in_subgraph(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {    
            Node_type ntype = actual_node->get_type();
            
            if (ntype == BASIC_ENTRY_NODE)
            {
                actual_node->set_visited(true);
                return;
            }
            
            ObjectList<Node*> parents = actual_node->get_parents();
            if (ntype == GRAPH_NODE)
            {
                actual_node->set_visited(true);
                clear_orphaned_nodes_in_subgraph(actual_node->get_graph_exit_node());
                
                for (ObjectList<Node*>::iterator it = parents.begin();
                        it != parents.end();
                        ++it)
                {
                    clear_orphaned_nodes_in_subgraph(*it);
                }
            }
            else
            {
                if (parents.empty())
                {
                    clear_orphaned_cascade(actual_node);
                }
                else
                {
                    actual_node->set_visited(true);
                    for (ObjectList<Node*>::iterator it = parents.begin();
                            it != parents.end();
                            ++it)
                    {
                        clear_orphaned_nodes_in_subgraph(*it);
                    }
                }
            }
        }
        return;
    }
    
    void ExtensibleGraph::clear_orphaned_cascade(Node* actual_node)
    {
        ObjectList<Node*> children = actual_node->get_children();
        disconnect_nodes(actual_node, children);
        delete_node(actual_node);
        
        for(ObjectList<Node*>::iterator it = children.begin();
                it != children.end();
                ++it)
        {
            if (!(*it)->get_entry_edges().empty())
            {
                clear_orphaned_cascade((*it));
            }
        }
    }
    
    void ExtensibleGraph::erase_unclassified_nodes(Node* actual)
    {
        if (!actual->is_visited())
        {
            actual->set_visited(true);
            
            Node_type ntype = actual->get_type();
            if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            else
            {
                ObjectList<Node*> children = actual->get_children();
                
                if (ntype == UNCLASSIFIED_NODE)
                {
                    bool non_always_entries = false, non_always_exits = false;
                    
                    // Check correctness
                    ObjectList<Edge*>::iterator it;
                    ObjectList<Edge*> entries = actual->get_entry_edges();
                    for(it = entries.begin(); it != entries.end(); ++it)
                    {
                        if ((*it)->get_data<Edge_type>(_EDGE_TYPE) != ALWAYS_EDGE)
                        {
                            non_always_entries = true;
                        }
                    }
                    ObjectList<Edge*> exits = actual->get_exit_edges();
                    for(it = exits.begin(); it != exits.end(); ++it)
                    {
                        if ((*it)->get_data<Edge_type>(_EDGE_TYPE) != ALWAYS_EDGE)
                        {
                            non_always_exits = true;
                        }
                    }
                    if (non_always_entries && non_always_exits)
                    {
                        internal_error("For an UNCLASSIFIED_NODE, or some entry is not an ALWAYS_EDGE or" \
                                       " some exit is not an ALWAYS_EDGE, but both is not correct", 0);
                    }

                    ObjectList<Node*> parents = actual->get_parents();
                    int n_connects = parents.size() * children.size();
                    ObjectList<Edge_type> etypes;
                    ObjectList<std::string> elabels;
                    if (non_always_entries)
                    {
                        int n_children = children.size();
                        ObjectList<Edge_type> entry_types = actual->get_entry_edge_types();
                        ObjectList<std::string> entry_labels = actual->get_entry_edge_labels();
                        while (n_children > 0)
                        {
                            etypes.append(entry_types);
                            elabels.append(entry_labels);
                            n_children--;
                        }                        
                    }
                    else if (non_always_exits)
                    {
                        int n_children = children.size();
                        ObjectList<Edge_type> exit_types = actual->get_exit_edge_types();
                        ObjectList<std::string> exit_labels = actual->get_exit_edge_labels();
                        while (n_children > 0)
                        {
                            etypes.append(exit_types);
                            elabels.append(exit_labels);
                            n_children--;
                        }
                    }
                    else
                    {
                        etypes = ObjectList<Edge_type>(n_connects, ALWAYS_EDGE);
                        elabels = ObjectList<std::string>(n_connects, "");
                    }
                    
                    disconnect_nodes(parents, actual);
                    disconnect_nodes(actual, children);
                    connect_nodes(parents, children, etypes, elabels);
                   
                    delete (actual);
                }
                else if (ntype == GRAPH_NODE)
                {
                    erase_unclassified_nodes(actual->get_graph_entry_node());
                }
                
                for(ObjectList<Node*>::iterator it = children.begin();
                        it != children.end();
                        ++it)
                {
                    erase_unclassified_nodes(*it);
                }
            }
        }
    }
   
    void ExtensibleGraph::erase_break_nodes(Node* node)
    {
        if (!node->is_visited())
        {
            node->set_visited(true);
            
            ObjectList<Node*> children = node->get_children();
            Node_type ntype = node->get_data<Node_type>(_NODE_TYPE);
            if (ntype == BASIC_BREAK_NODE)
            {
                // Check correctness
                if (children.size() != 1)
                {
                    internal_error("A Break node should have just one child. Break node '%d' has '%d'", node->get_id(), children.size());
                }
                if (children[0]->get_data<Node_type>(_NODE_TYPE) != BASIC_EXIT_NODE)
                {
                    internal_error("The child of a Break node should be an Exit node. Break node '%d' child is a '%s'", 
                                   node->get_id(), children[0]->get_type_as_string().c_str());
                }
                
                ObjectList<Node*> parents = node->get_parents();
                ObjectList<Edge_type> etypes = node->get_entry_edge_types();
                ObjectList<std::string> elabels = node->get_entry_edge_labels();
                
                // Disconnect nodes
                disconnect_nodes(parents, node);
                disconnect_nodes(node, children);
                connect_nodes(parents, children, etypes, elabels);
                
                // Delete the node
                delete (node);
            }
            else if (ntype == GRAPH_NODE)
            {
                erase_break_nodes(node->get_data<Node*>(_ENTRY_NODE));
            }
            
            for (ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                erase_break_nodes(*it);
            }
        }
    }    
    
    bool ExtensibleGraph::belongs_to_the_same_graph(Edge* edge)
    {
        Node* source = edge->get_source();
        Node* target = edge->get_target();
        bool result;
        
        if (source->has_key(_OUTER_NODE) && target->has_key(_OUTER_NODE))
        {
            Node* source_outer_node = source->get_outer_node();
            Node* target_outer_node = target->get_outer_node();

            if (source_outer_node->get_id() == target_outer_node->get_id())
            {
                result = true;
            }
            else
            {
                result = false;
            }
        }
        else
        {
            if (!source->has_key(_OUTER_NODE) && !target->has_key(_OUTER_NODE))
            {
                result = true;
            }
            else
            {
                result = false;
            }
        }
        
        return result;
    }
       
    void ExtensibleGraph::clear_visits(Node* node)
    {
        if (node->is_visited())
        {
//             std::cerr << "           clear visits --> " << node->get_id() << std::endl;
            node->set_visited(false);
            
            Node_type ntype = node->get_type();
            if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            else if (ntype == GRAPH_NODE)
            {
                clear_visits(node->get_graph_entry_node());
            }
            
            ObjectList<Node*> children = node->get_children();
            for(ObjectList<Node*>::iterator it = children.begin();
                    it != children.end();
                    ++it)
            {
                if ((*it)->is_visited())
                {
                    clear_visits(*it);
                }
            }
        }
    }
    
    void ExtensibleGraph::clear_visits_in_level(Node* node)
    {
        if (node->is_visited())
        {
//             std::cerr << "           clear visits in level --> " << node->get_id() << std::endl;
            node->set_visited(false);
            
            Node_type ntype = node->get_type();
            if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            
            ObjectList<Node*> children = node->get_children();
            for(ObjectList<Node*>::iterator it = children.begin();
                    it != children.end();
                    ++it)
            {
                if ((*it)->is_visited())
                {
                    clear_visits_in_level(*it);
                }
            }
        }
    }
    
    std::string ExtensibleGraph::get_name() const
    {
        return _name;
    }
    
    Symbol ExtensibleGraph::get_function_symbol() const
    {
        return _function_sym;
    }
    
    ObjectList<Node*> ExtensibleGraph::get_function_calls() const
    {
        return _function_calls;
    }
    
    Node* ExtensibleGraph::get_graph() const
    {
        return _graph;
    }
    
    ObjectList<Node*> ExtensibleGraph::get_tasks_list() const
    {
        return _task_nodes_l;
    }
    
    bool ExtensibleGraph::has_use_def_computed() const
    {
        return _use_def_computed;
    }
    
    void ExtensibleGraph::set_use_def_computed()
    {
        _use_def_computed = true;
    }
    
    //! This method returns the most outer node of a node before finding a loop node
    static Node* advance_over_outer_nodes_until_loop(Node* node)
    {
        Node* outer = node->get_outer_node();
        if (outer->get_graph_type() == LOOP)
        {
            return node;
        }
        else if (outer != NULL)
        {
            return advance_over_outer_nodes_until_loop(outer);
        }
        
        return outer;
    }
    
    // A node will be the increment of a FOR loop if its only children has 
    // - as parent a ENTRY_NODE, 
    // - as one of its children, joined with a FALSE_EDGE, a EXIT_NODE
    Node* ExtensibleGraph::is_for_loop_increment(Node* node)
    {
        // Get outer node of the actual node which is the potential increment of a loop (jump over func_calls, split_exprs, ...)
        Node* potential_loop_increment = advance_over_outer_nodes_until_loop(node);
        Node* loop_node = potential_loop_increment->get_outer_node();
        Node* loop_entry = loop_node->get_graph_entry_node();
        
        ObjectList<Node*> children = potential_loop_increment->get_children();
        if ( (children.size() == 1) && (*children[0] == *loop_entry->get_children()[0]) )
        {
            return potential_loop_increment;
        }
        
        return NULL;
    }
}
