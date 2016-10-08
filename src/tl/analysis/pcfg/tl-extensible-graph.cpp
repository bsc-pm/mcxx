/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include <queue>

#include "tl-datareference.hpp"
#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    ExtensibleGraph::ExtensibleGraph(std::string name, const NBase& nodecl, PCFGVisitUtils* utils)
        : _name(name), _graph(NULL), _utils(utils),
          _nodecl(nodecl), _sc(nodecl.retrieve_context()),
          _global_vars(), _function_sym(NULL), _post_sync(NULL), _pointer_to_size_map(), nodes_m(),
          _task_nodes_l(), _func_calls(),
          _concurrent_tasks(), _last_sync_tasks(), _last_sync_sequential(), _next_sync_tasks(), _next_sync_sequential(),
          _cluster_to_entry_map(), _usage_computed(false)
    {

        _graph = create_graph_node(NULL, nodecl, __ExtensibleGraph);
        _utils->_last_nodes = ObjectList<Node*>(1, _graph->get_graph_entry_node());
    }

    Node* ExtensibleGraph::append_new_child_to_parent(ObjectList<Node*> parents, NodeclList stmts,
                                                      NodeType ntype, EdgeType etype)
    {
        if(ntype == __Graph)
            internal_error("A Graph node must be created with the function 'create_graph_node' "
                            "and connected by hand [new id = %d]", _utils->_nid);

        if(!parents.empty())
        {
            Node* new_node = new Node(_utils->_nid, ntype, _utils->_outer_nodes.top());
            new_node->set_statements(stmts);
            connect_nodes(parents, new_node, ObjectList<EdgeType>(parents.size(), etype));

            return new_node;
        }
        else
        {
            internal_error("Cannot append the new node '%d' to a NULL parent", _utils->_nid + 1);
        }
    }

    Node* ExtensibleGraph::append_new_child_to_parent(Node* parent, NBase stmt,
                                                      NodeType ntype, EdgeType etype)
    {
        return append_new_child_to_parent(ObjectList<Node*>(1, parent), NodeclList(1, stmt), ntype, etype);
    }

    Node* ExtensibleGraph::append_new_child_to_parent(ObjectList<Node*> parents, NBase stmt,
                                                      NodeType ntype, EdgeType etype)
    {
        return append_new_child_to_parent(parents, NodeclList(1, stmt), ntype, etype);
    }

    Edge* ExtensibleGraph::connect_nodes(Node* parent, Node* child, 
                                         EdgeType etype, const NBase& label,
                                         bool is_task_edge, bool is_back_edge)
    {
        Edge* edge = NULL;
        if(parent != NULL && child != NULL)
        {
            if(!parent->has_child(child))
            {
                edge = new Edge(parent, child, is_task_edge, etype, label, is_back_edge);
                parent->set_exit_edge(edge);
                child->set_entry_edge(edge);
            }
            else
            {
                ObjectList<Edge*> exit_edges = parent->get_exit_edges();
                for(ObjectList<Edge*>::iterator it = exit_edges.begin(); it != exit_edges.end(); ++it)
                {
                    if((*it)->get_target() == child)
                    {
                        edge = *it;
                        break;
                    }
                }

                edge->set_type(etype);
                edge->set_label(label);
            }
        }
        else
        {
            internal_error("Using a NULL node when connecting two nodes. Parent is NULL? '%s', Child is NULL? '%s'",
                            (parent == NULL) ? "true" : "false", (child == NULL) ? "true" : "false");
        }
        return edge;
    }

    void ExtensibleGraph::connect_nodes(
            const ObjectList<Node*>& parents, const ObjectList<Node*>& children,
            const ObjectList<EdgeType>& etypes, const NodeclList& elabels)
    {
        unsigned int n_conn = parents.size() * children.size();
        ObjectList<EdgeType> actual_etypes = (etypes.empty() ? ObjectList<EdgeType>(n_conn, __Always) : etypes);
        NodeclList actual_elabels = 
                (elabels.empty() ? NodeclList(n_conn, NBase::null()) : elabels);
        
        if((actual_etypes.size() != actual_elabels.size()) || (parents.size() * children.size() != actual_etypes.size()))
        {
            internal_error("Wrong list size while connecting a list of nodes as children of "
                            "other node (parents '%d', children '%d', edge types '%d', edge labels '%d')\n",
                           parents.size(), children.size(), actual_etypes.size(), actual_elabels.size());
        }

        int children_size = children.size();
        ObjectList<EdgeType>::const_iterator itt = actual_etypes.begin();
        NodeclList::const_iterator itl = actual_elabels.begin();
        for(ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end();
             ++it, itt+=children_size, itl+=children_size)
        {
            ObjectList<EdgeType> current_etypes(itt, itt + children_size);
            NodeclList current_elabels(itl, itl + children_size);
            connect_nodes(*it, children, current_etypes, current_elabels);
        }
    }

    void ExtensibleGraph::connect_nodes(
            Node* parent, const ObjectList<Node*>& children,
            const ObjectList<EdgeType>& etypes, const NodeclList& elabels)
    {
        unsigned int n_conn = children.size();
        ObjectList<EdgeType> actual_etypes = (etypes.empty() ? ObjectList<EdgeType>(n_conn, __Always) : etypes);
        NodeclList actual_elabels = 
                (elabels.empty() ? NodeclList(n_conn, NBase::null()) : elabels);

        // Check correctness in the parameters
        if((children.size() != actual_etypes.size()) || (actual_etypes.size() != actual_elabels.size()))
        {
            internal_error("Wrong list size while connecting a list of nodes as children of "
                            "other node (children '%d', edge types '%d', edge labels '%d')\n",
                           children.size(), actual_etypes.size(), actual_elabels.size());
        }

        ObjectList<EdgeType>::const_iterator itt = actual_etypes.begin();
        NodeclList::const_iterator itl = actual_elabels.begin();
        ObjectList<Node*>::const_iterator it = children.begin();
        for(; it != children.end()
                && itt != actual_etypes.end()
                && itl != actual_elabels.end();
            ++it, ++itt, ++itl)
        {
            connect_nodes(parent, *it, *itt, *itl);
        }
    }

    void ExtensibleGraph::connect_nodes(
            const ObjectList<Node*>& parents, Node* child,
            const ObjectList<EdgeType>& etypes, const NodeclList& elabels,
            bool is_task_edge, bool is_back_edge)
    {
        // When etypes|labels are empty, the default parameter is a set of 
        // __Always|NBase::null() connections
        unsigned int n_conn = parents.size();
        ObjectList<EdgeType> actual_etypes = (etypes.empty() ? ObjectList<EdgeType>(n_conn, __Always) : etypes);
        NodeclList actual_elabels = 
                (elabels.empty() ? NodeclList(n_conn, NBase::null()) : elabels);

        // Check correctness in the parameters
        if((parents.size() != actual_etypes.size()) || (actual_etypes.size() != actual_elabels.size()))
        {
            internal_error("Wrong list size while connecting a list of nodes as parent of "
                            "other node (parents '%d', edge types '%d', edge labels '%d')\n",
                           parents.size(), actual_etypes.size(), actual_elabels.size());
        }

            ObjectList<Node*>::const_iterator it = parents.begin();
        ObjectList<EdgeType>::const_iterator itt = actual_etypes.begin();
        NodeclList::const_iterator itl = actual_elabels.begin();
        for(; it != parents.end()
                && itt != actual_etypes.end()
                && itl != actual_elabels.end();
             ++it, ++itt, ++itl)
        {
            connect_nodes(*it, child, *itt, *itl, is_task_edge, is_back_edge);
        }
    }

    void ExtensibleGraph::disconnect_nodes(ObjectList<Node*> parents, Node* child)
    {
        for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            disconnect_nodes(*it, child);
    }

    void ExtensibleGraph::disconnect_nodes(Node* parent, ObjectList<Node*> children)
    {
        for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            disconnect_nodes(parent, *it);
    }

    void ExtensibleGraph::disconnect_nodes(Node *parent, Node *child)
    {
        parent->erase_exit_edge(child);
        child->erase_entry_edge(parent);
    }

    Node* ExtensibleGraph::create_graph_node(Node* outer_node, NBase label,
                                              GraphType graph_type, NBase context)
    {
        Node* result = new Node(_utils->_nid, __Graph, outer_node);

        Node* entry_node = result->get_graph_entry_node();
        entry_node->set_outer_node(result);
        Node* exit_node = result->get_graph_exit_node();
        exit_node->set_outer_node(result);

        result->set_graph_label(label);
        result->set_graph_type(graph_type);
        if(graph_type == __OmpTask)
        {
            result->set_task_context(context);
        }

        _utils->_outer_nodes.push(result);

        return result;
    }

    Node* ExtensibleGraph::create_flush_node(Node* outer_node, NBase n)
    {
        Node* flush_node = new Node(_utils->_nid, __OmpFlush, outer_node);

        flush_node->set_pragma_node_info(PCFGPragmaInfo(n));

        connect_nodes(_utils->_last_nodes, flush_node);
        _utils->_last_nodes = ObjectList<Node*>(1, flush_node);

        return flush_node;
    }

    Node* ExtensibleGraph::create_unconnected_node(NodeType type, NBase nodecl)
    {
        Node* result = new Node(_utils->_nid, type, _utils->_outer_nodes.top());
        if(!nodecl.is_null())
            result->set_statements(NodeclList(1, nodecl));
        return result;
    }

    void ExtensibleGraph::delete_node(Node* n)
    {
        // Delete the node from its parents
        ObjectList<Node*> entry_nodes = n->get_parents();
        for(ObjectList<Node*>::iterator it = entry_nodes.begin(); it != entry_nodes.end(); ++it)
        {
            (*it)->erase_exit_edge(n);
        }

        // Delete the node from its children
        ObjectList<Node*> exit_nodes = n->get_children();
        for(ObjectList<Node*>::iterator it = exit_nodes.begin(); it != exit_nodes.end(); ++it)
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
        Node* entry = _graph->get_graph_entry_node();

        ObjectList<Node*> seq_l;
        concat_sequential_nodes_recursive(entry, seq_l);

        clear_visits_extgraph(entry);
    }

    void ExtensibleGraph::concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes)
    {
        if(!actual_node->is_visited_extgraph())
        {
            actual_node->set_visited_extgraph(true);

            if(!actual_node->is_entry_node())
            {
                if(!actual_node->is_normal_node() || actual_node->get_exit_edges().size() > 1
                    || actual_node->get_entry_edges().size() > 1)
                {
                    concat_nodes(last_seq_nodes);
                    last_seq_nodes.clear();

                    if(actual_node->is_graph_node())
                        concat_sequential_nodes_recursive(actual_node->get_graph_entry_node(), last_seq_nodes);
                    else if(actual_node->is_exit_node())
                        return;
                }
                else
                {
                    last_seq_nodes.append(actual_node);
                }
            }

            ObjectList<Node*> actual_exits = actual_node->get_children();
            for(ObjectList<Node*>::iterator it = actual_exits.begin(); it != actual_exits.end(); ++it)
                concat_sequential_nodes_recursive(*it, last_seq_nodes);
        }
        else
        {
            concat_nodes(last_seq_nodes);
            last_seq_nodes.clear();
        }
    }

    void ExtensibleGraph::concat_nodes(ObjectList<Node*> node_l)
    {
        if(node_l.size() > 1)
        {
            // Create the new node
            NodeclList stmt_l;
            for(ObjectList<Node*>::iterator it = node_l.begin(); it != node_l.end(); ++it)
            {
                stmt_l.append((*it)->get_statements());
            }
            Node* new_node = new Node(_utils->_nid, __Normal, node_l[0]->get_outer_node(), stmt_l);
            new_node->set_visited_extgraph(true);

            // Capture required information before it is destroyed
            Node* front = node_l.front();
            Node* back = node_l.back();
            ObjectList<Node*> front_parents = front->get_parents();
            ObjectList<EdgeType> front_entry_edge_types = front->get_entry_edge_types();
            NodeclList front_entry_edge_labels = front->get_entry_edge_labels();
            ObjectList<Node*> back_children = back->get_children();
            ObjectList<EdgeType> back_exit_edge_types = back->get_exit_edge_types();
            NodeclList back_exit_edge_labels = back->get_exit_edge_labels();

            // Destroy the nodes which has been concatenated
            for(ObjectList<Node*>::iterator it = node_l.begin(); it != node_l.end(); ++it)
            {
                delete_node(*it);
            }

            // Connect the node
            connect_nodes(front_parents, new_node, front_entry_edge_types, front_entry_edge_labels);
            connect_nodes(new_node, back_children, back_exit_edge_types, back_exit_edge_labels);
        }
    }

    void ExtensibleGraph::clear_unnecessary_nodes()
    {
        Node* entry = _graph->get_graph_entry_node();

        erase_unclassified_nodes(entry);
        clear_visits_extgraph(entry);

        erase_jump_nodes(entry);
        clear_visits_extgraph(entry);
    }

    void ExtensibleGraph::erase_unclassified_nodes(Node* current)
    {
        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);

            if(current->is_exit_node())
            {
                return;
            }
            else
            {
                ObjectList<Node*> children = current->get_children();

                if(current->is_unclassified_node())
                {
                    bool non_always_entries = false, non_always_exits = false;

                    // Check correctness
                    ObjectList<Edge*> entries = current->get_entry_edges();
                    ObjectList<Edge*> exits = current->get_exit_edges();
                    for(ObjectList<Edge*>::iterator it = entries.begin(); it != entries.end(); ++it)
                    {
                        if(!(*it)->is_always_edge())
                        {
                            non_always_entries = true;
                        }
                    }
                    for(ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
                    {
                        if(!(*it)->is_always_edge())
                        {
                            non_always_exits = true;
                        }
                    }
                    if(non_always_entries && non_always_exits)
                    {
                        internal_error("For an UNCLASSIFIED_NODE, or some entry is not an ALWAYS or" \
                                    " some exit is not an ALWAYS, but both is not correct", 0);
                    }

                    // Find out connection types for the new connections
                    ObjectList<Node*> parents = current->get_parents();
                    int n_connects = parents.size() * children.size();
                    ObjectList<EdgeType> etypes;
                    NodeclList elabels;
                    if(non_always_entries)
                    {
                        int n_children = children.size();
                        ObjectList<EdgeType> entry_types = current->get_entry_edge_types();
                        NodeclList entry_labels = current->get_entry_edge_labels();
                        while (n_children > 0)
                        {
                            etypes.append(entry_types);
                            elabels.append(entry_labels);
                            n_children--;
                        }
                    }
                    else if(non_always_exits)
                    {
                        int n_children = children.size();
                        ObjectList<EdgeType> exit_types = current->get_exit_edge_types();
                        NodeclList exit_labels = current->get_exit_edge_labels();
                        while (n_children > 0)
                        {
                            etypes.append(exit_types);
                            elabels.append(exit_labels);
                            n_children--;
                        }
                    }
                    else
                    {
                        etypes = ObjectList<EdgeType>(n_connects, __Always);
                        elabels = NodeclList(n_connects, NBase::null());
                    }

                    disconnect_nodes(parents, current);
                    disconnect_nodes(current, children);
                    connect_nodes(parents, children, etypes, elabels);

                    delete(current);
                }
                else if(current->is_graph_node())
                {
                    erase_unclassified_nodes(current->get_graph_entry_node());
                }

                for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                {
                    erase_unclassified_nodes(*it);
                }
            }
        }
    }

    void ExtensibleGraph::erase_jump_nodes(Node* current)
    {
        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);

            ObjectList<Node*> children = current->get_children();
            if(current->is_break_node() || current->is_continue_node() /*|| current->is_goto_node()*/)
            {
                // Check correctness
                if(children.size() != 1)
                {
                    internal_error("A Break/Continue current should have just one child. " \
                                   "Break current '%d' has '%d'", current->get_id(), children.size());
                }

                // Get current current connecting information
                ObjectList<Node*> parents = current->get_parents();
                ObjectList<EdgeType> etypes = current->get_entry_edge_types();
                NodeclList elabels = current->get_entry_edge_labels();

                // Disconnect currents
                disconnect_nodes(parents, current);
                disconnect_nodes(current, children);
                connect_nodes(parents, children, etypes, elabels);

                // Delete the current
                delete(current);
            }
            else if(current->is_graph_node())
            {
                erase_jump_nodes(current->get_graph_entry_node());
            }

            for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
            {
                erase_jump_nodes(*it);
            }
        }
    }

    bool ExtensibleGraph::has_been_defined(Node* current, Node* scope, const NBase& n)
    {
        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);

            const NodeclSet& killed = current->get_killed_vars();
            if(Utils::nodecl_set_contains_nodecl(n, killed))
                return true;

                ObjectList<Node*> parents;
                if(current->is_entry_node())
                {
                    // Check if graph parents are still inside the scope
                    Node* outer_node = current->get_outer_node();
                if(outer_node != scope)
                        parents = outer_node->get_parents();
                    }
                else
                {
                    parents = current->get_parents();
                }

            for(ObjectList<Node*>::iterator it = parents.begin(); it != parents.end(); ++it)
            {
                if(!ExtensibleGraph::is_backward_parent(current, *it) &&  has_been_defined(*it, scope, n))
                    return true;
                    ExtensibleGraph::clear_visits_extgraph_aux(current);
            }
        }

        return false;
    }

    void ExtensibleGraph::clear_visits(Node* n)
    {
        if (n->is_visited())
        {
            n->set_visited(false);

            if (n->is_graph_node())
                clear_visits(n->get_graph_entry_node());

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if (!n->get_graph_entry_node()->is_visited())
                    n->set_visited(false);  // Be sure we do not miss any node
                                            // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& children = n->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits(*it);
        }
    }

    void ExtensibleGraph::clear_visits_aux(Node* n)
    {
        if (n->is_visited_aux())
        {
            n->set_visited_aux(false);

            if (n->is_graph_node())
                clear_visits_aux(n->get_graph_entry_node());

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if (!n->get_graph_entry_node()->is_visited_aux())
                    n->set_visited_aux(false);  // Be sure we do not miss any node
                                                // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& children = n->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits_aux(*it);
        }
    }

    void ExtensibleGraph::clear_visits_extgraph(Node* n)
    {
        if (n->is_visited_extgraph())
        {
            n->set_visited_extgraph(false);

            if (n->is_graph_node())
                clear_visits_extgraph(n->get_graph_entry_node());

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if (!n->get_graph_entry_node()->is_visited_extgraph())
                    n->set_visited_extgraph(false);     // Be sure we do not miss any node
                                                        // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& children = n->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits_extgraph(*it);
        }
    }

    void ExtensibleGraph::clear_visits_extgraph_aux(Node* n)
    {
        if (n->is_visited_extgraph_aux())
        {
            n->set_visited_extgraph_aux(false);

            if (n->is_graph_node())
                clear_visits_extgraph_aux(n->get_graph_entry_node());

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if (!n->get_graph_entry_node()->is_visited_extgraph_aux())
                    n->set_visited_extgraph_aux(false);     // Be sure we do not miss any node
                                                            // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& children = n->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits_extgraph_aux(*it);
        }
    }

    void ExtensibleGraph::clear_visits_in_level(Node* n, Node* outer_node)
    {
        if (n->is_visited()
            && (n->node_is_enclosed_by(outer_node)
                || n->is_omp_task_node()
                || n->is_omp_async_target_node()
                || n->is_omp_virtual_tasksync()))
        {
            n->set_visited(false);

            if(n->is_graph_node())
            {
                Node* new_outer = (n->is_omp_task_node() || n->is_omp_async_target_node() ? n : outer_node);
                clear_visits_in_level(n->get_graph_entry_node(), new_outer);
            }

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if(n->node_is_enclosed_by(outer_node)
                    || n->is_omp_task_node()
                    || n->is_omp_async_target_node()
                    || n->is_omp_virtual_tasksync())
                {
                    n->set_visited(false);  // Be sure we do not miss any node
                                            // in case we clean up from the middle of the graph
                }
            }
            const ObjectList<Node*>& children = n->get_children();
            for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits_in_level(*it, outer_node);
        }
    }

    void ExtensibleGraph::clear_visits_in_level_no_nest(Node* n, Node* outer_node)
    {
        if (n->is_visited()
            && (n->node_is_enclosed_by(outer_node)
                || n->is_omp_task_node()
                || n->is_omp_async_target_node()
                || n->is_omp_virtual_tasksync()))
        {
            n->set_visited(false);
            
            if(n->is_exit_node())
                return;
            
            const ObjectList<Node*>& children = n->get_children();
            for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            {
                clear_visits_in_level_no_nest(*it, outer_node);
            }
        }
    }
    
    void ExtensibleGraph::clear_visits_aux_in_level(Node* n, Node* outer_node)
    {
        if (n->is_visited_aux()
            && (n->node_is_enclosed_by(outer_node)
                || n->is_omp_task_node()
                || n->is_omp_async_target_node()))
        {
            n->set_visited_aux(false);

            if(n->is_graph_node())
            {
                Node* new_outer = ((n->is_omp_task_node() || n->is_omp_async_target_node()) ? n : outer_node);
                clear_visits_aux_in_level(n->get_graph_entry_node(), new_outer);
            }

            if (n->is_exit_node())
            {
                n = n->get_outer_node();
                if (n->node_is_enclosed_by(outer_node)
                    || n->is_omp_task_node()
                    || n->is_omp_async_target_node())
                    n->set_visited_extgraph_aux(false);     // Be sure we do not miss any node
                                                            // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& children = n->get_children();
            for(ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
                clear_visits_aux_in_level(*it, outer_node);
        }
    }

    // This method only keeps cleaning up backwards when
    // all children of an outer node are already cleaned up
    static void clear_visits_backwards_in_level_rec(Node* n, Node* sc)
    {
        if (!n->is_visited())
            return;

        n->set_visited(false);
        if (n->is_graph_node())
            clear_visits_backwards_in_level_rec(n->get_graph_exit_node(), sc);

        if (n->is_entry_node())
        {
            // If the parents of the outer node have still one child that must be clean up
            // within the scope of the cleaning, then do not keep cleaning this path
            // because we will still visit the outer from that child
            bool all_cleaned = true;
            const ObjectList<Node*>& children = n->get_outer_node()->get_children();
            for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
            {
                if ((*it)->is_visited() && ExtensibleGraph::node_contains_node(sc, *it))
                    all_cleaned = false;
            }
            if (all_cleaned)
                n = n->get_outer_node();
            n->set_visited(false);  // Be sure we do not miss any node
                                    // in case we clean up from the middle of the graph
        }
        const ObjectList<Node*>& parents = n->get_parents();
        for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
            clear_visits_backwards_in_level_rec(*it, sc);
    }

    void ExtensibleGraph::clear_visits_backwards_in_level(Node* n, Node* sc)
    {
        if (n->is_graph_node())
        {
            n->set_visited(false);
            const ObjectList<Node*>& parents = n->get_parents();
            for(ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
                clear_visits_backwards_in_level_rec(*it, n);
        }
        else
        {
            clear_visits_backwards_in_level_rec(n, sc);
        }
    }

    static void clear_visits_backwards_rec(Node* n)
    {
        if (n->is_visited())
        {
            n->set_visited(false);
            if (n->is_graph_node())
                clear_visits_backwards_rec(n->get_graph_exit_node());

            if (n->is_entry_node())
            {
                n = n->get_outer_node();
                n->set_visited(false);  // Be sure we do not miss any node
                                        // in case we clean up from the middle of the graph
            }
            const ObjectList<Node*>& parents = n->get_parents();
            for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
                clear_visits_backwards_rec(*it);
        }
    }

    // This methods cleans backwards regardless any visit from outer nodes children
    void ExtensibleGraph::clear_visits_backwards(Node* n)
    {
        if(n->is_graph_node())
        {
            n->set_visited(false);
            const ObjectList<Node*>& parents = n->get_parents();
            for(ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
                clear_visits_backwards_rec(*it);
        }
        else
        {
            clear_visits_backwards_rec(n);
        }
    }

    std::string ExtensibleGraph::get_name() const
    {
        return _name;
    }

    NBase ExtensibleGraph::get_nodecl() const
    {
        return _nodecl;
    }

    Scope ExtensibleGraph::get_scope() const
    {
        return _sc;
    }

    const NodeclSet& ExtensibleGraph::get_global_variables() const
    {
        return _global_vars;
    }

    void ExtensibleGraph::set_global_vars(const NodeclSet& global_vars)
    {
        _global_vars.insert(global_vars.begin(), global_vars.end());
    }

    Symbol ExtensibleGraph::get_function_symbol() const
    {
        return _function_sym;
    }

    Node* ExtensibleGraph::get_post_sync() const
    {
        return _post_sync;
    }

    void ExtensibleGraph::set_post_sync(Node* post_sync)
    {
        _post_sync = post_sync;
    }

    void ExtensibleGraph::set_pointer_n_elems(const NBase& s, const NBase& size)
    {
        if(_pointer_to_size_map.find(s)==_pointer_to_size_map.end())
            _pointer_to_size_map[s] = size;
        else
            _pointer_to_size_map[s] = NBase::null();
    }
    
    NBase ExtensibleGraph::get_pointer_n_elems(const NBase& s)
    {
        NBase result = NBase::null();
        if(_pointer_to_size_map.find(s)!=_pointer_to_size_map.end())
            result = _pointer_to_size_map[s];
        return result;
    }
    
    SizeMap ExtensibleGraph::get_pointer_n_elements_map()
    {
        return _pointer_to_size_map;
    }
    
    void ExtensibleGraph::purge_non_constant_pointer_n_elems()
    {
        for(SizeMap::iterator it = _pointer_to_size_map.begin(); it != _pointer_to_size_map.end();)
        {
            if(it->second.is_null())
                _pointer_to_size_map.erase(it++);
            else
                ++it;
        }
    }

    Node* ExtensibleGraph::get_graph() const
    {
        return _graph;
    }

    ObjectList<Node*> ExtensibleGraph::get_tasks_list() const
    {
        return _task_nodes_l;
    }

    ObjectList<Symbol> ExtensibleGraph::get_function_parameters() const
    {
        if(_function_sym.is_valid())
        {
            ObjectList<Symbol> params;

            scope_entry_t* function_header = _function_sym.get_internal_symbol();
            int num_params = symbol_entity_specs_get_num_related_symbols(function_header);
            for(int i=0; i<num_params; ++i)
            {
                Symbol s(symbol_entity_specs_get_related_symbols_num(function_header, i));
                params.append(s);
            }

            return params;
        }
        else
        {
            internal_error("Asking for the parameters of a function in an extensible graph that does not contain a Function Code", 0);
        }
    }

    void ExtensibleGraph::add_func_call_symbol(Symbol s)
    {
        _func_calls.insert(s);
    }

    ObjectList<Symbol> ExtensibleGraph::get_function_calls() const
    {
        return _func_calls;
    }

    ObjectList<Node*> ExtensibleGraph::get_task_concurrent_tasks(Node* task) const
    {
        ObjectList<Node*> result;
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
        {
            WARNING_MESSAGE("Trying to get the simultaneous tasks of a node that is not a task. Only tasks accepted.", 0);
        }
        else
        {
            std::map<Node*, ObjectList<Node*> >::const_iterator it = _concurrent_tasks.find(task);
            if(it == _concurrent_tasks.end())
            {
                WARNING_MESSAGE("Simultaneous tasks of task '%d' have not been computed", task->get_id());
            }
            else
            {
                result = it->second;
            }
        }
        return result;
    }

    void ExtensibleGraph::add_concurrent_task_group(Node* task, ObjectList<Node*> concurrent_tasks)
    {
        if(_concurrent_tasks.find(task) != _concurrent_tasks.end())
        {
            WARNING_MESSAGE("You are trying to insert a task that already exists in the map of "
                            "synchronous tasks of a PCFG. This should never happen so we skip it", 0);
            return;
        }

        _concurrent_tasks[task] = concurrent_tasks;
    }
    
    ObjectList<Node*> ExtensibleGraph::get_task_last_sync_for_tasks(Node* task) const
    {
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
            return ObjectList<Node*>();

        std::map<Node*, ObjectList<Node*> >::const_iterator it = _last_sync_tasks.find(task);
        if (it != _last_sync_tasks.end())
        {
            return it->second;
        }

        WARNING_MESSAGE("Last synchronization points for tasks of task '%d' have not been computed", task->get_id());
        return ObjectList<Node*>();
    }

    ObjectList<Node*> ExtensibleGraph::get_task_last_sync_for_sequential_code(Node* task) const
    {
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
            return ObjectList<Node*>();

        std::map<Node*, ObjectList<Node*> >::const_iterator it = _last_sync_sequential.find(task);
        if (it != _last_sync_sequential.end())
        {
            return it->second;
        }

        WARNING_MESSAGE("Last synchronization points for sequential code of task '%d' have not been computed", task->get_id());
        return ObjectList<Node*>();
    }

    void ExtensibleGraph::add_last_sync_for_tasks(Node* task, Node* last_sync)
    {
        _last_sync_tasks[task].append(last_sync);
    }

    void ExtensibleGraph::set_last_sync_for_tasks(Node* task, Node* last_sync)
    {
        _last_sync_tasks[task] = ObjectList<Node*>(1, last_sync);
    }

    void ExtensibleGraph::add_last_sync_for_sequential_code(Node* task, Node* last_sync)
    {
        _last_sync_sequential[task].append(last_sync);
    }

    ObjectList<Node*> ExtensibleGraph::get_task_next_sync_for_tasks(Node* task) const
    {
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
            return ObjectList<Node*>();

        std::map<Node*, ObjectList<Node*> >::const_iterator it = _next_sync_tasks.find(task);
        if (it != _next_sync_tasks.end())
        {
            return it->second;
        }

        WARNING_MESSAGE("Next synchronization points for tasks of task '%d' have not been computed", task->get_id());
        return ObjectList<Node*>();
    }

    ObjectList<Node*> ExtensibleGraph::get_task_next_sync_for_sequential_code(Node* task) const
    {
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
            return ObjectList<Node*>();

        std::map<Node*, ObjectList<Node*> >::const_iterator it = _next_sync_sequential.find(task);
        if (it != _next_sync_sequential.end())
        {
            return it->second;
        }

        WARNING_MESSAGE("Next synchronization points for tasks of task '%d' have not been computed", task->get_id());
        return ObjectList<Node*>();
    }

    void ExtensibleGraph::add_next_sync_for_tasks(Node* task, Node* last_sync)
    {
        _next_sync_tasks[task].append(last_sync);
    }

    void ExtensibleGraph::add_next_sync_for_sequential_code(Node* task, Node* last_sync)
    {
        _next_sync_sequential[task].append(last_sync);
    }

    void ExtensibleGraph::remove_next_sync_for_tasks(Node* task, Node* next_sync)
    {
        if (_next_sync_tasks.find(task) == _next_sync_tasks.end())
        {
            WARNING_MESSAGE("Task %d is not in the map of next synchronizations. "
                            "We are unable to remove %d from its list of next synchronization points.\n",
                            task->get_id(), next_sync->get_id());
            return;
        }
        
        ObjectList<Node*>& next_syncs = _next_sync_tasks[task];
        next_syncs = next_syncs.not_find(next_sync);
    }
    
    void ExtensibleGraph::remove_concurrent_task(Node* task, Node* old_concurrent_task)
    {
        if (_concurrent_tasks.find(task) == _concurrent_tasks.end())
        {
            WARNING_MESSAGE("Task %d is not in the map of tasks concurrency. "
                            "We are unable to remove %d from its list of concurrent tasks.\n",
                            task->get_id(), old_concurrent_task->get_id());
            return;
        }

        ObjectList<Node*>& concurrent_tasks = _concurrent_tasks[task];
        concurrent_tasks = concurrent_tasks.not_find(old_concurrent_task);
    }

    //! This method returns the most outer node of a node before finding a loop node
    static Node* advance_over_outer_nodes_until_loop(Node* node)
    {
        Node* outer = node->get_outer_node();
        GraphType outer_type = outer->get_graph_type();
        if((outer_type == __LoopDoWhile) || (outer_type == __LoopFor) || (outer_type == __LoopWhile))
            return node;
        else if(outer != NULL)
            return advance_over_outer_nodes_until_loop(outer);
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
        if((children.size() == 1) && (*children[0] == *loop_entry->get_children()[0]))
        {
            return potential_loop_increment;
        }

        return NULL;
    }


    bool ExtensibleGraph::node_is_in_loop(Node* current)
    {
        bool res = false;
        Node* outer_node = current->get_outer_node();
        while(outer_node != NULL)
        {
            if(outer_node->is_loop_node())
            {
                res = true;
                break;
            }
            outer_node = outer_node->get_outer_node();
        }
        return res;
    }

    bool ExtensibleGraph::node_is_in_conditional_branch(Node* current, Node* max_outer)
    {
        bool res = false;

        Node* outer_node = current->get_outer_node();
        while((outer_node != NULL) && !res
               && ((max_outer == NULL) ? true : (outer_node->get_id() != max_outer->get_id())))
        {
            if(outer_node->is_ifelse_statement() || outer_node->is_switch_statement()
                || outer_node->is_loop_node())
            {
                res = true;
            }
            outer_node = outer_node->get_outer_node();
        }

        return res;
    }

    bool ExtensibleGraph::node_is_in_synchronous_construct(Node* current)
    {
        bool res = false;

        Node* outer_node = current->get_outer_node();
        while((outer_node != NULL) && !res)
        {
            if(outer_node->is_omp_atomic_node() || outer_node->is_omp_critical_node())
                res = true;
            outer_node = outer_node->get_outer_node();
        }

        return res;
    }

    bool ExtensibleGraph::is_backward_parent(Node* current, Node* parent)
    {
        bool result = false;
        if(!current->is_visited_extgraph_aux())
        {
            current->set_visited_extgraph_aux(true);

            if(current == parent)
            {
                result = true;
            }
            else
            {
                if(!current->is_exit_node())
                {
                    if(current->is_graph_node())
                    {
                        result = is_backward_parent(current->get_graph_entry_node(), parent);
                    }
                    ObjectList<Node*> children = current->get_children();
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end() && !result; ++it)
                    {
                        result = result || is_backward_parent(*it, parent);
                    }
                }
            }
        }
        return result;
    }

    bool ExtensibleGraph::node_contains_node(Node* container, Node* contained)
    {
        bool result = false;
        if(container->is_graph_node())
        {
            Node* outer_node = contained->get_outer_node();
            while((outer_node != NULL) && !result)
            {
                if(outer_node == container)
                {
                    result = true;
                }
                outer_node = outer_node->get_outer_node();
            }
        }
        return result;
    }

    Node* ExtensibleGraph::get_extensible_graph_from_node(Node* node)
    {
        Node* graph = node;

        if(node != NULL)
        {
            while(graph->get_outer_node() != NULL)
                graph = graph->get_outer_node();
        }

        return graph;
    }

    static bool node_is_ancestor_of_node_rec(Node* ancestor, Node* descendant)
    {
        if (ancestor->is_visited_extgraph())
            return false;

        ancestor->set_visited_extgraph(true);

        bool res = false;
        // 1.- Target node found
        if (ancestor == descendant)
        {
            res = true;
        }
        // 2.- Target node not found => keep iterating
        else
        {
            // Treat inner nodes of a graph node
            if (ancestor->is_graph_node())
            {
                res = node_is_ancestor_of_node_rec(ancestor->get_graph_entry_node(), descendant);
            }

            // Keep iterating if 'descendant' has not yet been found
            if (!res)
            {
                ObjectList<Edge*> exits;
                if (ancestor->is_exit_node())
                    exits = ancestor->get_outer_node()->get_exit_edges();
                else
                    exits = ancestor->get_exit_edges();
                while (!exits.empty() && !res)
                {
                    Edge* e = exits.back();
                    exits.pop_back();
                    if (e->is_back_edge())
                    {   // Follow only the FALSE edge
                        Node* next_ancestor = e->get_target();
                        if (next_ancestor->is_visited_extgraph())
                            continue;
                        next_ancestor->set_visited_extgraph(true);

                        ObjectList<Edge*> next_exits = next_ancestor->get_exit_edges();
                        for (ObjectList<Edge*>::iterator it = next_exits.begin();
                             it != next_exits.end(); ++it)
                        {
                            if ((*it)->is_false_edge())
                            {
                                exits.push_back(*it);
                                break;
                            }
                        }
                    }
                    else
                    {
                        res = node_is_ancestor_of_node_rec(e->get_target(), descendant);
                    }
                }
            }
        }

        return res;
    }

    bool ExtensibleGraph::node_is_ancestor_of_node(Node* ancestor, Node* descendant)
    {
        // Base case: both nodes are the same
        if (ancestor == descendant)
            return false;

        // Look for 'descendant' traversing the PCFG from 'ancestor'
        ancestor->set_visited_extgraph(true);
        bool res = false;
        const ObjectList<Edge*>& exits = ancestor->get_exit_edges();
        for (ObjectList<Edge*>::const_iterator it = exits.begin();
             it != exits.end() && !res ; ++it)
        {
            res = node_is_ancestor_of_node_rec((*it)->get_target(), descendant);
        }

        ExtensibleGraph::clear_visits_extgraph(ancestor);

        return res;
    }

    Node* ExtensibleGraph::get_omp_enclosing_node(Node* current)
    {
        Node* result = current->get_outer_node();
        while(result != NULL && !result->is_omp_node())
            result = result->get_outer_node();
        if(result != NULL && !result->is_omp_node())
            result = NULL;
        return result;
    }

    Edge* ExtensibleGraph::get_edge_between_nodes(Node* source, Node* target)
    {
        Edge* result = NULL;
        ObjectList<Edge*> exits = source->get_exit_edges();
        for(ObjectList<Edge*>::iterator it = exits.begin(); it != exits.end(); ++it)
        {
            if((*it)->get_target() == target)
            {
                result = *it;
                break;
            }
        }

        ERROR_CONDITION(result == NULL,
                         "Asking for the connection edge between two nodes (%d, %d) that are not connected\n",
                         source->get_id(), target->get_id());
        return result;
    }

    Node* ExtensibleGraph::get_enclosing_context(Node* n)
    {
        Node* sc = NULL;
        Node* outer = (n->is_context_node() ? n->get_outer_node() : n);
        while(sc == NULL && outer != NULL)
        {
            if(outer->is_context_node())
                sc = outer;
            outer = outer->get_outer_node();
        }
        return sc;
    }

    Node* ExtensibleGraph::get_enclosing_task(Node* n)
    {
        Node* result = NULL;
        Node* current = (n->is_omp_task_node() ? n->get_parents()[0] : n);
        while(result == NULL && current != NULL)
        {
            if (current->is_omp_task_node() || current->is_omp_async_target_node())
                result = current;
            else
                current = current->get_outer_node();
        }
        return result;
    }

    bool ExtensibleGraph::task_encloses_task(Node* container, Node* contained)
    {
        Node* enclosing_task = get_enclosing_task(contained);
        while((enclosing_task != NULL) && (enclosing_task != container))
            enclosing_task = get_enclosing_task(enclosing_task);
        return (enclosing_task == NULL ? false : true);
    }

    static bool node_contains_tasks_rec(Node* graph_node, Node* current, ObjectList<Node*>& tasks)
    {
        bool result = false;
        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);
            if(current != graph_node->get_graph_exit_node())
            {
                // Insert current in the list of tasks if it is a task node
                if(current->is_omp_task_creation_node())
                {
                    result = true;
                    tasks.insert(current);
                }

                if(current->is_graph_node())
                    result = node_contains_tasks_rec(graph_node, current->get_graph_entry_node(), tasks) || result;

                if(current != graph_node)
                {
                    ObjectList<Node*> children = current->get_children();
                    for(ObjectList<Node*>::iterator it = children.begin(); it != children.end(); ++it)
                        if(!(*it)->is_omp_task_node() && !(*it)->is_omp_async_target_node())
                            result = node_contains_tasks_rec(graph_node, *it, tasks) || result;
                }
            }
        }
        return result;
    }

    bool ExtensibleGraph::node_contains_tasks(Node* graph_node, Node* current, ObjectList<Node*>& tasks)
    {
        bool res = node_contains_tasks_rec(graph_node, current, tasks);
        ExtensibleGraph::clear_visits_extgraph(current);
        return res;
    }

    Node* ExtensibleGraph::get_enclosing_control_structure(Node* node)
    {
        if (node->is_omp_task_node() || node->is_omp_async_target_node())
            node = node->get_parents()[0];

        Node* outer_node = node->get_outer_node();
        while (outer_node != NULL)
        {
            if (outer_node->is_loop_node()
                    || outer_node->is_ifelse_statement()
                    || outer_node->is_switch_case_node())
            {
                return outer_node;
            }
            outer_node = outer_node->get_outer_node();
        }
        return NULL;
    }

    Node* ExtensibleGraph::get_task_creation_from_task(Node* task)
    {
        if (!task->is_omp_task_node()
            && !task->is_omp_async_target_node())
            return NULL;

        const ObjectList<Node*>& parents = task->get_parents();
        Node* creation_node = NULL;
        for (ObjectList<Node*>::const_iterator it = parents.begin(); it != parents.end(); ++it)
        {
            if ((*it)->is_omp_task_creation_node())
            {
                creation_node = *it;
                break;
            }
        }

        if (VERBOSE && (creation_node == NULL))
        {
            WARNING_MESSAGE("The creation node of task %s (node %d) could not be found.\n", 
                            task->get_graph_related_ast().get_locus_str().c_str(), task->get_id());
        }

        return creation_node;
    }

    Node* ExtensibleGraph::get_task_from_task_creation(Node* task_creation)
    {
        if (!task_creation->is_omp_task_creation_node())
            return NULL;

        const ObjectList<Node*>& children = task_creation->get_children();
        Node* task = NULL;
        for (ObjectList<Node*>::const_iterator it = children.begin(); it != children.end(); ++it)
        {
            if ((*it)->is_omp_task_node() || (*it)->is_omp_async_target_node())
            {
                task = *it;
                break;
            }
        }

        if (VERBOSE && (task == NULL))
        {
            WARNING_MESSAGE("The task created in node %d could not be found.\n", task->get_id());
        }
        
        return task;
    }

    bool ExtensibleGraph::task_synchronizes_in_post_sync(Node* task)
    {
        std::set<Node*> visited;
        ObjectList<Node*> children = task->get_children();
        while (!children.empty())
        {
            Node* n = children.back();
            children.pop_back();

            if(visited.find(n) != visited.end())
                continue;
            visited.insert(n);

            if (n->is_omp_virtual_tasksync())
                return true;

            children.append(n->get_children());
        }
        return false;
    }

    Node* ExtensibleGraph::find_nodecl_rec(Node* current, const NBase& n)
    {
        Node* result = NULL;

        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);

            if(!current->is_exit_node())
            {
                // Look first in nested nodes, if graph, or the current node, is not graph
                if(current->is_graph_node())
                {
                    NBase current_ast = current->get_graph_related_ast();
                    if(Nodecl::Utils::structurally_equal_nodecls(current_ast, n, /*skip conversion nodes*/ true))
                        result = current;
                    else
                        result = find_nodecl_rec(current->get_graph_entry_node(), n);
                }
                else
                {
                    NodeclList stmts = current->get_statements();
                    for(NodeclList::iterator it = stmts.begin();
                         (it != stmts.end()) && (result == NULL); ++it)
                    {
                        if(Nodecl::Utils::nodecl_contains_nodecl_by_structure(*it, n))
                            result = current;
                    }
                }

                // If not found, look in the children
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin();
                     it != children.end() && (result == NULL); ++it)
                {
                    result = find_nodecl_rec(*it, n);
                }
            }
        }
        return result;
    }

    Node* ExtensibleGraph::find_nodecl(const NBase& n)
    {
        Node* result = find_nodecl_rec(_graph, n);
        ExtensibleGraph::clear_visits_extgraph(_graph);
        return result;
    }

    Node* ExtensibleGraph::find_nodecl_pointer_rec(Node* current, const NBase& n)
    {
        Node* result = NULL;

        if(!current->is_visited_extgraph())
        {
            current->set_visited_extgraph(true);

            if(!current->is_exit_node())
            {
                // Look first in nested nodes, if graph, or the current node, is not graph
                if(current->is_graph_node())
                {
                    NBase current_ast = current->get_graph_related_ast();
                    if(current_ast == n)
                        result = current;
                    else
                        result = find_nodecl_pointer_rec(current->get_graph_entry_node(), n);
                }
                else
                {
                    NodeclList stmts = current->get_statements();
                    for(NodeclList::iterator it = stmts.begin();
                        (it != stmts.end()) && (result == NULL); ++it)
                    {
                        if(Nodecl::Utils::nodecl_contains_nodecl_by_pointer(*it, n))
                            result = current;
                    }
                }

                // If not found, look in the children
                ObjectList<Node*> children = current->get_children();
                for(ObjectList<Node*>::iterator it = children.begin();
                    it != children.end() && (result == NULL); ++it)
                {
                    result = find_nodecl_pointer_rec(*it, n);
                }
            }
        }
        return result;
    }

    Node* ExtensibleGraph::find_nodecl_pointer(const NBase& n)
    {
        Node* result = find_nodecl_pointer_rec(_graph, n);
        ExtensibleGraph::clear_visits_extgraph(_graph);
        return result;
    }

    // ******* Getters and setters for analyses built on top of the PCFG ******* //

    bool ExtensibleGraph::usage_is_computed() const
    {
        return _usage_computed;
    }

    void ExtensibleGraph::set_usage_computed()
    {
        _usage_computed = true;
    }

    // ***** END Getters and setters for analyses built on top of the PCFG ***** //

}
}
