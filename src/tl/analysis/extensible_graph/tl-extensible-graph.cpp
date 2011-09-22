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
#include "tl-pragmasupport.hpp"

namespace TL
{   
    static bool is_worksharing(std::string directive);
    static bool is_combined_worksharing(std::string directive);
    
    ExtensibleGraph::ExtensibleGraph(std::string name)
        : _graph(NULL), _name(name), _nid(-1),
          _continue_stack(), _break_stack(),
          _labeled_node_l(), _goto_node_l(), _tasks_node_l(),
          _last_nodes(), _outer_node()
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
                                        ObjectList<Edge_type> etypes, ObjectList<std::string> labels)
    {
        ObjectList<Edge_type>::iterator itt = etypes.begin();
        ObjectList<std::string>::iterator itl = labels.begin();
        ObjectList<Node*>::iterator it = parents.begin();
        for(;
            it != parents.end(), itt != etypes.end(), itl != labels.end();
            ++it, ++itt, ++itl)
        {
            connect_nodes(*it, children, ObjectList<Edge_type>(children.size(), *itt), 
                          ObjectList<std::string>(children.size(), *itl));
        }
        
        if (it != parents.end() || itt != etypes.end() || itl != labels.end())
        {
            internal_error("Wrong list size while connecting a list of nodes as children of " 
                           "other node (children '%d', edge types '%d', edge labels '%d')\n",
                           parents.size(), etypes.size(), labels.size());
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
//         std::cerr << "Creating Graph node " << _nid+1 << std::endl;
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
    
    void ExtensibleGraph::concat_sequential_nodes()
    {
        Node* entry = _graph->get_data<Node*>(_ENTRY_NODE);

        ObjectList<Node*> seq_l;
        concat_sequential_nodes_recursive(entry, seq_l);
    }
    
    void ExtensibleGraph::concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes)
    {
        if (!actual_node->is_visited())
        {
            actual_node->set_visited(true);
            
            Node_type ntype = actual_node->get_data<Node_type>(_NODE_TYPE);
            
            if ((ntype != BASIC_NORMAL_NODE || actual_node->get_exit_edges().size() > 2) 
                && ntype != BASIC_ENTRY_NODE)
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
            // Create the new node
            ObjectList<Nodecl::NodeclBase> stmt_l;
            for(ObjectList<Node*>::iterator it = node_l.begin();
                it != node_l.end();
                ++it)
            {
                stmt_l.append((*it)->get_data<Nodecl::NodeclBase>(_NODE_STMTS, Nodecl::NodeclBase::null()));
            }
            Node* new_node = new Node(_nid, BASIC_NORMAL_NODE, _outer_node.top(), stmt_l);
            
            // Connect the node
            Node* front = node_l.front();
            Node* back = node_l.back();
            connect_nodes(front->get_parents(), new_node,
                        front->get_entry_edge_types(), front->get_entry_edge_labels());
            connect_nodes(new_node, back->get_children(),
                        back->get_exit_edge_types(), back->get_exit_edge_labels());
            
            // Destroy the previous nodes
            for(ObjectList<Node*>::iterator it = node_l.begin();
                it != node_l.end();
                ++it)
            {
                delete_node(*it);
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
        
//         clear_orphaned_nodes(exit);
//         clear_visits(entry);
        
        erase_unclassified_nodes(entry);
        clear_visits(entry);
        
//         join_unhalted_statements(entry, ObjectList<Node*>());
//         clear_visits(entry);
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
                    ObjectList<Node*> parents = actual->get_parents();
                    ObjectList<Edge_type> entry_types = actual->get_entry_edge_types();
                    ObjectList<std::string> entry_labels = actual->get_entry_edge_labels();
                    
                    disconnect_nodes(parents, actual);
                    disconnect_nodes(actual, children);
                    
                    connect_nodes(parents, children, entry_types, entry_labels);
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
    
    
    
    
    
    
    
    
    
    
    







    
    Node* ExtensibleGraph::build_pragma_construct(Node* parent, Statement pragma_stmt, 
                                                  Node* outer_graph)
    {
//         PragmaCustomConstruct pragma_construct(pragma_stmt.get_ast(), pragma_stmt.get_scope_link());
//         std::string preffix = pragma_construct.get_pragma();
//         std::string directive = pragma_construct.get_directive();
//         
//         if (preffix == "omp")
//         {
//             if (directive == "task")
//             {
//                 Node* flush_node = append_new_node_to_parent(parent, ObjectList<AST_t>(1, AST_t()),
//                                                              outer_graph, FLUSH_NODE);
//                 
//                 Node* pragma_graph_node = create_graph_node(NULL,
//                                                             pragma_stmt.get_pragma_line().get_ast(),
//                                                             "omp_pragma");
//                 Node* entry_node = pragma_graph_node->get_data<Node*>(_ENTRY_NODE);
//                 Node* exit_node = pragma_graph_node->get_data<Node*>(_EXIT_NODE);
//                 
//                 Node* inner_start_flush_node = 
//                     append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, AST_t()),
//                                               pragma_graph_node, FLUSH_NODE);
//                 Node* pragma_stmt_node = 
//                     build_graph_from_statement(inner_start_flush_node,
//                                                pragma_stmt.get_pragma_construct_statement(),
//                                                pragma_graph_node);
//                 Node* inner_end_flush_node = 
//                     append_new_node_to_parent(pragma_stmt_node, ObjectList<AST_t>(1, AST_t()),
//                                               pragma_graph_node, FLUSH_NODE);
//                 
//                 exit_node->set_id(++_nid);
//                 connect_nodes(inner_end_flush_node, exit_node);
//                 
//                 _tasks_node_list.append(pragma_graph_node);
//                 
//                 return flush_node;
//             }
//             else
//             {
//                 Node* pragma_graph_node = create_graph_node(outer_graph,
//                                                             pragma_stmt.get_pragma_line().get_ast(),
//                                                             "omp_pragma");
//                 connect_nodes(parent, pragma_graph_node);
//                 Node* entry_node = pragma_graph_node->get_data<Node*>(_ENTRY_NODE);
//                 Node* exit_node = pragma_graph_node->get_data<Node*>(_EXIT_NODE);
//                 
//                 Node* last_node;
//                 
//                 if (is_worksharing(directive))
//                 {
//                     // Flush node at the end of the Worksharing constructions
//                     if ( (directive == "for") || (directive == "workshare") )
//                     {
//                         Node* pragma_stmt_node = 
//                             build_graph_from_statement(entry_node,
//                                                        pragma_stmt.get_pragma_construct_statement(),
//                                                        pragma_graph_node);
//                         
//                         last_node = create_barrier_node(ObjectList<Node*>(1, pragma_stmt_node),
//                                                         pragma_graph_node);
//                     }
//                     else if (directive == "single")
//                     {
//                         Node* inner_single_node = 
//                             build_graph_from_statement(entry_node,
//                                                        pragma_stmt.get_pragma_construct_statement(),
//                                                        pragma_graph_node);
//                                                                             
//                         ObjectList<Node*> barrier_parents;
//                         barrier_parents.append(entry_node);
//                         barrier_parents.append(inner_single_node);
//                         last_node = create_barrier_node(barrier_parents, pragma_graph_node);
//                     }
//                     else if ( directive == "sections" )
//                     {
//                         last_node = build_sections_node(entry_node, pragma_stmt, pragma_graph_node);
//                     }
//                     else if (directive == "do")
//                     {
//                         // TODO
//                     }
//                     
//                     last_node = append_new_node_to_parent(last_node, ObjectList<AST_t>(1, AST_t()),
//                                                           pragma_graph_node, FLUSH_NODE);
//                 }
//                 else if (is_combined_worksharing(directive))
//                 {   // Flush node at the beginning and the end of a Combined Worksharing
//                     Node* flush_node = append_new_node_to_parent(entry_node, 
//                                                                  ObjectList<AST_t>(1, AST_t()),
//                                                                  pragma_graph_node, FLUSH_NODE);
//                     
//                     if ((directive == "parallel|for") || (directive == "parallel|workshare"))
//                     {
//                         Node* pragma_stmt_node = 
//                             build_graph_from_statement(flush_node,
//                                                        pragma_stmt.get_pragma_construct_statement(),
//                                                        pragma_graph_node);
//                         
//                         last_node = create_barrier_node(ObjectList<Node*>(1, pragma_stmt_node),
//                                                         pragma_graph_node);
//                     }
//                     else if (directive == "parallel|sections")
//                     {
//                         last_node = build_sections_node(flush_node, pragma_stmt, pragma_graph_node);
//                     }
//                     else if (directive == "parallel|do")
//                     {
//                         // TODO
//                     }
// 
//                     last_node = append_new_node_to_parent(last_node, ObjectList<AST_t>(1, AST_t()),
//                                                           pragma_graph_node, FLUSH_NODE);
//                 }
//                 else
//                 {
//                     if (directive == "parallel" || directive == "critical" || 
//                         directive == "ordered")
//                     {   // Flush node at the beginning and at the end of these constructions
//                         Node* flush_node = append_new_node_to_parent(entry_node,
//                                                                      ObjectList<AST_t>(1, AST_t()),
//                                                                      pragma_graph_node, FLUSH_NODE);
//                         
//                         if (directive == "parallel")
//                         {
//                             Node* pragma_stmt_node = 
//                                 build_graph_from_statement(flush_node,
//                                                       pragma_stmt.get_pragma_construct_statement(), 
//                                                            pragma_graph_node);
//                             last_node = create_barrier_node(ObjectList<Node*>(1, pragma_stmt_node),
//                                                             pragma_graph_node);
//                         }
//                         else if (directive == "critical")
//                         {
//                             Node* pragma_stmt_node = 
//                                 build_graph_from_statement(flush_node,                              
//                                                     pragma_stmt.get_pragma_construct_statement(), 
//                                                            pragma_graph_node);
//                             connect_nodes(pragma_stmt_node, pragma_stmt_node);
//                             
//                             last_node = create_barrier_node(ObjectList<Node*>(1, pragma_stmt_node),
//                                                             pragma_graph_node);
//                         }
//                         else if (directive == "ordered")
//                         {
//                             Node* barrier_node = 
//                                 create_barrier_node(ObjectList<Node*>(1, entry_node),
//                                                     pragma_graph_node);
//                             last_node = build_graph_from_statement(barrier_node,
//                                                     pragma_stmt.get_pragma_construct_statement(), 
//                                                                    pragma_graph_node);
//                             connect_nodes(last_node, last_node);
//                         }
//                         
//                         last_node = append_new_node_to_parent(last_node,
//                                                               ObjectList<AST_t>(1, AST_t()),
//                                                               pragma_graph_node, FLUSH_NODE);
//                     }
//                     else
//                     {
//                         if ((directive == "atomic"))
//                         {
//                             Node* flush_node = 
//                                 append_new_node_to_parent(entry_node, ObjectList<AST_t>(1, AST_t()),
//                                                           pragma_graph_node, FLUSH_NODE);
//                             Node* pragma_stmt_node = 
//                                 build_graph_from_statement(flush_node,
//                                                     pragma_stmt.get_pragma_construct_statement(), 
//                                                            pragma_graph_node);
//                             last_node = create_barrier_node(ObjectList<Node*>(1, pragma_stmt_node),
//                                                             pragma_graph_node);
//                         }
//                         else if (directive == "master")
//                         {
//                             Node* master_graph_node = 
//                                 create_graph_node(pragma_graph_node, 
//                                                   pragma_stmt.get_pragma_line().get_ast(),
//                                                   "omp_pragma");
//                             connect_nodes(entry_node, master_graph_node);
//                             
//                             Node* inner_master_node =                               
//                                 build_graph_from_statement(
//                                     master_graph_node->get_data<Node*>(_ENTRY_NODE),
//                                     pragma_stmt.get_pragma_construct_statement(),
//                                     master_graph_node);
//                             
//                             Node* exit_node = master_graph_node->get_data<Node*>(_EXIT_NODE);
//                             exit_node->set_id(++_nid);
//                             connect_nodes(inner_master_node, exit_node);
//                             
//                             last_node = inner_master_node;            
//                         }
//                         else if (directive == "section")
//                         {
//                             last_node = build_graph_from_statement(entry_node,
//                                                     pragma_stmt.get_pragma_construct_statement(), 
//                                                                    pragma_graph_node);
//                         }
//                         else
//                         {
//                             internal_error("Unexpected Pragma '%s' while building "
//                                            "Pragma Construct node in the Control Flow Graph",
//                                            directive.c_str());
//                         }
//                     }
//                 }
//                 
//                 exit_node->set_id(++_nid);
//                 connect_nodes(last_node, exit_node);
//                 return pragma_graph_node;
//             }
//         }
//         else
//         {
            return NULL;
//         }
    }
    
    
    Node* ExtensibleGraph::build_sections_node(Node* parent, Statement pragma_stmt, 
                                               Node* outer_graph)
    {
        // Create a graph for each 'section' statement and the statements among them
//         Statement sections_stmt = pragma_stmt.get_pragma_construct_statement();
//         ObjectList<Statement> section_stmts;
//         
//         if (sections_stmt.is_compound_statement())
//         {   // As many statements as "sections" inside the "sections directive"
//             section_stmts = sections_stmt.get_inner_statements();
//         }
//         else
//         {
//             section_stmts.append(sections_stmt);
//         }
//         
//         ObjectList<Node*> section_nodes;
//         
//         // If orphaned nodes, wrap them within a new "section" graph
//         if (!section_stmts[0].is_pragma_construct())
//         {
//             Source pragma_line;
//             pragma_line << "#pragma omp section \n {\n}";
//             AST_t pragma_line_ast = pragma_line.parse_statement(pragma_stmt.get_ast(), _sl);
//             Node* section_graph_node =create_graph_node(outer_graph, pragma_line_ast, "omp_pragma");
//             connect_nodes(parent, section_graph_node);
//             
//             Node* inner_orphaned_node = 
//                 build_graph_from_statements(section_graph_node->get_data<Node*>(_ENTRY_NODE), 
//                                             ObjectList<Statement>(1, section_stmts[0]), 
//                                             section_graph_node);
//                                                                     
//             Node* exit_node = section_graph_node->get_data<Node*>(_EXIT_NODE);
//             exit_node->set_id(++_nid);
//             connect_nodes(inner_orphaned_node, exit_node);
//             
//             section_nodes.append(section_graph_node);
//         }
//         
//         // Build each section graph
//         for (ObjectList<Statement>::iterator it = section_stmts.begin()+1;
//             it != section_stmts.end();
//             ++it)
//         {
//             section_nodes.append(build_graph_from_statements(parent, ObjectList<Statement>(1, *it), 
//                                                              outer_graph));
//         }
//         
//         return create_barrier_node(section_nodes, outer_graph);
        return NULL;
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
    
    /*
     * The recursion is done down-top because this is the only way to found unreachable nodes.
     * By the construction of the graph, we will never found paths following a top-down traversal
     * which contain unreachable nodes
     */
    void ExtensibleGraph::join_unhalted_statements(Node* actual, ObjectList<Node*> node_set)
    {
        
        if (!actual->is_visited())
        {    
            actual->set_visited(true);
            Node_type ntype = actual->get_data<Node_type>(_NODE_TYPE);
            if (ntype == BASIC_EXIT_NODE)
            {
                join_node_set(node_set);
                return;
            }
            else
            {
                ObjectList<Node*> next_children = actual->get_children();
                
                if ((ntype == FLUSH_NODE) || (ntype == BARRIER_NODE) ||
                    (ntype == BASIC_LABELED_NODE) || (ntype == BASIC_GOTO_NODE) ||
                    (ntype == BASIC_FUNCTION_CALL_NODE))
                {
                    join_node_set(node_set);
                }
                else if (ntype == GRAPH_NODE)
                {
                    join_node_set(node_set);
                    join_unhalted_statements(actual->get_data<Node*>(_ENTRY_NODE), node_set);
                }
                else if (ntype == BASIC_ENTRY_NODE)
                {   // Do nothing
                }
                else if (ntype == BASIC_NORMAL_NODE)
                {
                    if (actual->get_entry_edges().size() > 1)
                    {
                        join_node_set(node_set);
                    }
                    
                    node_set.append(actual);
                    if ( (next_children.size() > 1) || 
                         (next_children.size() == 1 && next_children[0]->is_visited()) )
                    {
                        join_node_set(node_set);
                    }
                }
                else
                {
                    internal_error("Unexpected type '%s' of node while clearing unnecessary nodes", 
                                   actual->get_node_type_as_string().c_str());
                }
                
                for (ObjectList<Node*>::iterator it = next_children.begin();
                        it != next_children.end();
                        ++it)
                {
                    join_unhalted_statements(*it, node_set);
                }
            }
        }
    }
    
    void ExtensibleGraph::join_node_set(ObjectList<Node*>& node_set)
    {
//         if (node_set.size() > 1)
//         {
//             // Create the new node
//             ObjectList<AST_t> stmts;
//             std::string n;
//             for (ObjectList<Node*>::iterator it = node_set.begin();
//                 it != node_set.end();
//                 ++it)
//             {
//                 std::stringstream ss;
//                 ss << (*it)->get_id();
//                 n += ss.str() + ", ";
//                 stmts.append((*it)->get_data<ObjectList<AST_t> >(_NODE_STMTS));
//             }
//             
//             Node* first = node_set[0];
//             Node* last = node_set[node_set.size()-1];
//             
//             Node* outer_graph = NULL;
//             if (first->has_key(_OUTER_NODE))
//             {    
//                 outer_graph = first->get_data<Node*>(_OUTER_NODE);
//             }
//             Node* new_joined_node = new Node(_nid, BASIC_NORMAL_NODE, outer_graph);
//             new_joined_node->set_data(_NODE_STMTS, stmts);
//             new_joined_node->set_visited(true);
//             
//             // Join the new node with its parents
//             ObjectList<Node*> first_parents = first->get_parents();
//             ObjectList<Node*> last_parents = last->get_parents();
//             ObjectList<Edge_type> etypesp = 
//                 first->get_entry_edge_types().append(last->get_entry_edge_types());
//             ObjectList<std::string> elabelsp = 
//                 first->get_entry_edge_labels().append(last->get_entry_edge_labels());
//             disconnect_nodes(first_parents, first);
//             disconnect_nodes(last_parents, last);
//             connect_nodes(first_parents.append(last_parents), new_joined_node, etypesp, elabelsp);
//             
//             // Join the new node with its children
//             ObjectList<Node*> children = last->get_children();
//             ObjectList<Edge_type> etypesc = last->get_exit_edge_types();
//             ObjectList<std::string> elabelsc = last->get_exit_edge_labels();
//             disconnect_nodes(last, children);
//             connect_nodes(new_joined_node, children, etypesc, elabelsc);
//         }
//         
//         // Clear the list
//         node_set.clear();
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
