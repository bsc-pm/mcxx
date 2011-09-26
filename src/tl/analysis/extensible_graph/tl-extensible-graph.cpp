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
// #include "tl-pragmasupport.hpp"

namespace TL
{   
    static bool is_worksharing(std::string directive);
    static bool is_combined_worksharing(std::string directive);
    
    ExtensibleGraph::ExtensibleGraph(std::string name)
        : _graph(NULL), _name(name), _nid(-1),
          _continue_stack(), _break_stack(),
          _labeled_node_l(), _goto_node_l(), _tasks_node_l(),
          _last_nodes(), _outer_node(), _task_nodes_l()
    {
        _graph = create_graph_node(NULL, Nodecl::NodeclBase::null(), "extensible_graph");
        _last_nodes.append(_graph->get_data<Node*>(_ENTRY_NODE));
    }

    ExtensibleGraph::ExtensibleGraph(const ExtensibleGraph& graph)
    {
        _graph = graph._graph;
        _name = graph._name;
        _nid = graph._nid;
        _continue_stack = graph._continue_stack;
        _break_stack = graph._break_stack;
        _labeled_node_l = graph._labeled_node_l;
        _goto_node_l = graph._goto_node_l;
        _tasks_node_l = graph._tasks_node_l;
        _last_nodes = graph._last_nodes;
        _outer_node = graph._outer_node;
        _task_nodes_l = graph._task_nodes_l;
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
                new_node->set_data(_NODE_STMTS, nodecls);
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

    Edge* ExtensibleGraph::connect_nodes(Node* parent, Node* child, Edge_type etype, std::string label)
    {
        if (parent != NULL && child != NULL)
        {
            if (parent->has_child(child))
            {
                std::cerr << "warning: Trying to connect node '" << parent->get_id() << "' with '" << child->get_id() 
                           << "' and these nodes are already connected" << std::endl;
                return NULL;
            }
            else
            {
//                 std::cerr << "Connecting " << parent->get_id() << " with " << child->get_id() << std::endl;
                Edge* new_edge = new Edge(parent, child, etype, label);
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

    void ExtensibleGraph::connect_nodes(ObjectList<Node*> parents, Node* child, Edge_type etype, std::string label)
    {
        for(ObjectList<Node*>::iterator it = parents.begin();
            it != parents.end();
            ++it)
        {
            connect_nodes(*it, child, etype, label);         
        }
    }

    Node* ExtensibleGraph::create_graph_node(Node* outer_node, Nodecl::NodeclBase label, std::string graph_type)
    {
        Node* result = new Node(_nid, GRAPH_NODE, outer_node);
        
        Node* entry_node = result->get_data<Node*>(_ENTRY_NODE);
        entry_node->set_data(_OUTER_NODE, result);
        Node* exit_node = result->get_data<Node*>(_EXIT_NODE);
        exit_node->set_data(_OUTER_NODE, result);
    
        result->set_data(_NODE_LABEL, label);
        result->set_data(_GRAPH_TYPE, graph_type);
        
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
        result->set_data(_NODE_STMTS, ObjectList<Nodecl::NodeclBase>(1, nodecl));
        
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
        concat_sequential_nodes();
    }
    
    void ExtensibleGraph::concat_sequential_nodes()
    {
        Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);

        ObjectList<Node*> seq_l;
        concat_sequential_nodes_recursive(entry, seq_l);
        
        clear_visits(entry);
    }
    
    void ExtensibleGraph::concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            Node_type ntype = actual_node->get_data<Node_type>(_NODE_TYPE);
            
            if (ntype != BASIC_ENTRY_NODE)
            {
                if (ntype != BASIC_NORMAL_NODE || actual_node->get_exit_edges().size() > 1
                    || actual_node->get_entry_edges().size() > 1)
                {
                    concat_nodes(last_seq_nodes);                    
                    last_seq_nodes.clear();
                    
                    if (ntype == GRAPH_NODE)
                    {
                        concat_sequential_nodes_recursive(actual_node->get_data<Node*>(_ENTRY_NODE), last_seq_nodes);
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
                    stmt_l.append((*it)->get_data<ObjectList<Nodecl::NodeclBase> >(_NODE_STMTS));
                }
                Node* new_node = new Node(_nid, BASIC_NORMAL_NODE, node_l[0]->get_data<Node*>(_OUTER_NODE), stmt_l);
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
    
    void ExtensibleGraph::clear_unnecessary_nodes()
    {   
//         std::cerr << "Clearing unnecessary nodes" << std::endl;
        // Clear all the Entry / Exit nodes except the first and the last ones
        Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);
        Node* exit = _graph->get_data<Node*>(_EXIT_NODE);
       
        // FIXME Can this case happen in the Nodecl Graph construction way???
//         clear_orphaned_nodes(exit);
//         clear_visits(entry);
        
        erase_unclassified_nodes(entry);
        clear_visits(entry);
    }
    
    void ExtensibleGraph::clear_orphaned_nodes(Node* actual_node)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            ObjectList<Edge*> entries = actual_node->get_entry_edges();
            
            Node_type ntype = actual_node->get_data<Node_type>(_NODE_TYPE);
            if (entries.empty() && ntype != BASIC_ENTRY_NODE)
            {
                clear_orphaned_cascade(actual_node);
            }
            else
            {
                if (ntype == GRAPH_NODE)
                {   // Traverse the inner nodes
                    clear_orphaned_nodes_in_subgraph(actual_node->get_data<Node*>(_EXIT_NODE));
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
            Node_type ntype = actual_node->get_data<Node_type>(_NODE_TYPE);
            
            if (ntype == BASIC_ENTRY_NODE)
            {
                actual_node->set_visited(true);
                return;
            }
            
            ObjectList<Node*> parents = actual_node->get_parents();
            if (ntype == GRAPH_NODE)
            {
                actual_node->set_visited(true);
                clear_orphaned_nodes_in_subgraph(actual_node->get_data<Node*>(_EXIT_NODE));
                
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
            
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
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
                    erase_unclassified_nodes(actual->get_data<Node*>(_ENTRY_NODE));
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
    
    std::string ExtensibleGraph::get_name() const
    {
        return _name;
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

    static bool is_worksharing(std::string directive)
    {
        return (directive == "for" || directive == "do" ||
            directive == "workshare" || directive == "sections" ||
            directive == "single");
    }

    static bool is_combined_worksharing(std::string directive)
    {
        return (directive == "parallel|for" || directive == "parallel|do" ||
                directive == "parallel|workshare" || directive == "parallel|sections");
    }

    bool ExtensibleGraph::belongs_to_the_same_graph(Edge* edge)
    {
        Node* source = edge->get_source();
        Node* target = edge->get_target();
        bool result;
        
        if (source->has_key(_OUTER_NODE) && target->has_key(_OUTER_NODE))
        {
            Node* source_outer_graph = source->get_data<Node*>(_OUTER_NODE);
            Node* target_outer_graph = target->get_data<Node*>(_OUTER_NODE);

            if (source_outer_graph->get_id() == target_outer_graph->get_id())
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
       
    void ExtensibleGraph::clear_visits(Node* actual)
    {
        if (actual->is_visited())
        {
            actual->set_visited(false);
            
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
            if (ntype == BASIC_EXIT_NODE)
            {
                return;
            }
            else if (ntype == GRAPH_NODE)
            {
                clear_visits(actual->get_data<Node*>(_ENTRY_NODE));
            }
            
            ObjectList<Node*> children = actual->get_children();
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
}
