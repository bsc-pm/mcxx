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


#ifndef EXTENSIBLE_GRAPH_HPP
#define EXTENSIBLE_GRAPH_HPP

#include <algorithm>
#include <map>
#include <stack>

#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "tl-node.hpp"
#include "tl-nodecl.hpp"

namespace TL
{
    class LIBTL_CLASS ExtensibleGraph
    {
        protected:
            // *** Class attributes *** //
            Node* _graph;    // Node with type GRAPH_NODE which contains the whole graph
            std::string _name;
            int _nid;
            
            //! Symbol of the function is contained in the graph.
            /*! This symbol is empty when the code contained in the graph do not correspond to a function
             */
            Symbol _function_sym;
            
            //! List of nodes containing a function call
            ObjectList<Node*> _function_calls;         
            
            //! Map of nodes with the relationship between a new node and an old node when a piece of graph is copied
            /*! The key is the old node and the value is the new node
             */
            std::map<Node*, Node*> nodes_m;            
            
            // *** Values used during the construction of the graph *** //
            
            //! Stacks to keep the exit nodes of Loop Statements
            std::stack<Node*> _continue_stack;
            std::stack<Node*> _break_stack;
            
            //! Lists to keep special nodes that breaks the expected behaviour of the flow
            ObjectList<Node*> _labeled_node_l;
            ObjectList<Node*> _goto_node_l;
            ObjectList<Node*> _tasks_node_l;
            
            //! List of nodes that will be parents of a new node
            ObjectList<Node*> _last_nodes;
            
            //! This attribute contains the node we are building
            //! CfgVisitor will create and destroy this node depending on the statements it is traversing
            std::stack<Node*> _outer_node;
            
            //! List of nodes containing task's code
            ObjectList<Node*> _task_nodes_l;
            
            
    private:
            //! We don't want to allow this kind of constructions
            ExtensibleGraph(const ExtensibleGraph& graph);
            ExtensibleGraph& operator=(const ExtensibleGraph&);
        
            //! Makes up the content of the nodes by deleting the line feeds and escaping all
            //! those symbols that can not be freely write in DOT language.
            void makeup_dot_block(std::string &str);
            
            //! This method removes all those nodes that are unreachable and those that were created
            //! as auxiliary nodes when the graph was created. 
            //! It also joins those nodes that are always consecutively executed and non of them
            //! are the target of a jump.
            void clear_unnecessary_nodes();
            
            //! This method concatenates all those nodes that form a Basic Block in one only node.
            //! It creates a new node containing all the statements and deleted the previous nodes.
            void concat_sequential_nodes();
            
            
            void concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes);
            
            //! Prints nodes and relations between them in a string in a recursive way.
            /*!
              \param actual_node Source node from which the printing is started.
              \param dot_graph Inout parameter where the DOT is printed.
              \param outer_edges Set of edges that must be printed in an outer DOT cluster.
              \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
              \param indent Indentation for the actual node when it is printed.
              \param subgraph_id Identifier for the actual cluster.
             */
            void get_nodes_dot_data(Node* actual_node, std::string& dot_graph, 
                                    std::vector<std::string>& outer_edges, 
                                    std::vector<Node*>& outer_nodes,
                                    std::string indent, int& subgraph_id);
                                     
            //! Prints both nodes and edges within a cfg subgraph
                        //! Prints nodes and relations between them in a string in a recursive way.
            /*!
              \param actual_node Source node from which the printing is started.
              \param graph_data Inout parameter where the DOT is printed.
              \param outer_edges Set of edges that must be printed in an outer DOT cluster.
              \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
              \param indent Indentation for the actual node when it is printed.
              \param subgraph_id Identifier for the actual cluster.
             */
            void get_dot_subgraph(Node* actual_node, std::string& graph_data, 
                                  std::vector<std::string>& outer_edges,
                                  std::vector<Node*>& outer_nodes, 
                                  std::string indent, int& subgraph_id);
                                  
            //! Prints the data of an only node.                                    
            void get_node_dot_data(Node* node, std::string& graph_data, std::string indent);
           
            
            //! Returns whether the source and the target of an edge belongs to the same outer node.
            /*!
              If both the source and the target do not have an outer node, then true is returned.
             */
            bool belongs_to_the_same_graph(Edge* edge);
            
            //! Computes the liveness information of each node regarding only its inner statements
            /*!
              A variable is Killed (X) when it is defined before than used in X.
              A variable is Upper Exposed (X) when it is used before than defined in X.
             */
            void gather_live_initial_information(Node* actual);
            
            //! Computes the data-flow equation for each node in a iterative way 
            //! until the information stops changing.
            /*!
              It is mandatory to use before #gather_live_initial_information.
             */
            void solve_live_equations();
            
            //! Computes on iteration of the method #solve_live_equations.
            /*!
              Live out (X) = Union of all Live in (Y),
                             for all Y successors of X.
              Live in (X) = Upper exposed (X) + 
                            ( Live out (X) - Killed (X) )
             */
            void solve_live_equations_recursive(Node* actual, bool& changed);
            
            //! Recompute the identifiers of the nodes graph hanging from actual_node from the value of _nid
            //! This method is used when a node s replaced by another, because the identifiers may be repeated
            void recompute_identifiers(Node* actual_node);

            //! Method used during the copy method when the edges must be copied before connecting the nodes
            void connect_nodes(ObjectList<Node*> parents, Node* child, ObjectList<Edge*> edges);
            
            //! Method used during the copy method when the edges must be copied before connecting the nodes
            void connect_nodes(Node* parent, ObjectList<Node*> children, ObjectList<Edge*> edges);
            
            //! Method used during the copy method that copies the graph recursively
            
            
            void copy_nodes_and_map_nodes(Node* old_node);
            
            void connect_copied_nodes(Node* old_node);
            
        public:
            // *** Constructors *** //
            
            //! Constructor used by the different phases to build a new ExtensibleGraph.
            /*!
              \param name Name which will identify the graph.
             */
            explicit ExtensibleGraph(std::string name);
            
            //! Creates a new graph with the same characteristics of the actual graph
            ExtensibleGraph* copy();
            
            
            // *** Modifiers *** //

            //! This method creates a new node containing a Basic Block and connects it to its
            //! parent node with a new edge.
            /*!
             * \param parents Set of Parents of the new node.
             * \param nodecls Set of statements forming the new node.
             * \param ntype Type of the node to be created. 
             *              By default is BASIC_NORMAL_NODE.
             * \param etype Type of the new edge that connects the new node with @_last_node.
             *              By default is ALWAYS_EDGE.
             * \return The new node created
             */
            Node* append_new_node_to_parent(ObjectList<Node*> parent, ObjectList<Nodecl::NodeclBase> nodecl,
                                           Node_type ntype = BASIC_NORMAL_NODE, 
                                           Edge_type etype = ALWAYS_EDGE);
            
            
            Node* append_new_node_to_parent(Node* parent, Nodecl::NodeclBase nodecl,
                                           Node_type ntype = BASIC_NORMAL_NODE, 
                                           Edge_type etype = ALWAYS_EDGE);
            
            Node* append_new_node_to_parent(Node* parent, ObjectList<Nodecl::NodeclBase> nodecl,
                                           Node_type ntype = BASIC_NORMAL_NODE, 
                                           Edge_type etype = ALWAYS_EDGE);            

            Node* append_new_node_to_parent(ObjectList<Node*> parents, Nodecl::NodeclBase nodecl,
                                           Node_type ntype = BASIC_NORMAL_NODE, 
                                           Edge_type etype = ALWAYS_EDGE);                   
            
            //! Connects two nodes by creating a new edge between them.
            /*!
             * \param parent Source node of the connection.             
             * \param child Target node of the connection.
             * \param etype Type of the connection between the two nodes.
             * \param label Label for the connection. It will be used when a Catch or a Case edges
             *              are built.
             * \return The new edge created between the two nodes             
             */
            Edge* connect_nodes(Node* parent, Node* child, 
                               Edge_type etype = ALWAYS_EDGE, std::string label = "");        
           
            //! Wrapper method for #connect_nodes when a set of parents must be connected to a
            //! set of children and each connection may be different from the others. 
            //! A set of edge types and labels must be provided. It is assumed that each parent is
            //! connected to all its children with the same type of edge.
            void connect_nodes(ObjectList<Node*> parents, ObjectList<Node*> children, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> elabels);
           
            //! Wrapper method for #connect_nodes when a parent must be connected to a set of
            //! children and each connection may be different from the others. 
            //! A set of edge types and labels must be provided.            
            void connect_nodes(Node* parent, ObjectList<Node*> children, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> labels);

            //! Wrapper method for #connect_nodes when a set of parents must be connected to an
            //! only child and the nature of the connection is the same for all of them.
            void connect_nodes(ObjectList<Node*> parents, Node* child, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> labels);
            
            //! Wrapper method for #connect_nodes when a set of parents must be connected to an
            //! only child and the nature of the connection is the same for all of them.
            void connect_nodes(ObjectList<Node*> parents, Node* child, 
                               Edge_type etype = ALWAYS_EDGE, std::string label = "");
            
            //! Wrapper method for #disconnect_nodes when a set of parents is connected to a child.
            void disconnect_nodes(ObjectList<Node*> parents, Node* child);
            
            //! Wrapper method for #disconnect_nodes when a set of children is connected to a parent
            void disconnect_nodes(Node* parent, ObjectList<Node*> children);
            
            //! Disconnects two nodes.
            /*!
              The method warnings if it is tried to remove an non-existent edge.
              \param parent Source of the connection to be removed.
              \param child Target of the connection to be removed.
             */
            void disconnect_nodes(Node *parent, Node *child);            
            
            //! Builds a composite node with an entry and an exit node.
            /*!
             * \param outer_node Node to which the new structure will belong to.
             *                   It must be a Graph node.
             * \param label Nodecl containing the Statement represented with the new node. It will be
             *              the label of the node.
             * \param graph_type Type of the composite node. 
             *                   It must be some of these values: 'split_stmt',
             *                   'function_call', 'conditional_expression', 'omp_pragma'.
             * \param context Nodecl containing the context of the graph
             *                It is only not null for graph nodes containing tasks
             * \return The new composite node.
             */
            Node* create_graph_node(Node* outer_node, Nodecl::NodeclBase label, 
                                    std::string graph_type, Nodecl::NodeclBase context = Nodecl::NodeclBase::null());
            
            //! Builds a Barrier node with its corresponding Flush nodes and connects it with the existent graph
            /*!
             * As defined in OpenMP standard, a flush occurs while a barrier point. To describe this behaviour 
             * with the graph, we add a Flush node before and after a Barrier node.
             * The method modifies the attribute #_last_nodes to the last Flush node created.
             * \param outer_node Node to which the new nodes will belong to.
             *                   It must be a Graph node.
             */
            void create_barrier_node(Node* outer_node);
            
            //! Builds a basic normal node (BASIC_NORMAL_NODE)
            /*!
             * \param nodecl Statement that will be added to the new node
             */
            Node* create_unconnected_node(Nodecl::NodeclBase nodecl);
            
            //! Deletes a node from the graph
            /*!
             * The method erases the node from the list of children of its parent and
             * from the list of parents of its children as well. Finally, frees the pointer.
             * \param n Pointer to the node to be deleted
             */
            void delete_node(Node* n);

            //! This method traverses the graph and clears all the unreachable nodes and  those nodes created
            //! during the construction of the graph but do not represent any statement of the code, and also 
            //! concatenates the nodes that will be executed sequentially for sure (Basic Blocks)
            void dress_up_graph();
            
            //! This method concatenates a list of nodes into only one
            /*!
             * The method assumes that each node is parent of the next node in the list, and only with it.
             * For the first node in the list, it can be child of an unspecified list of nodes.
             * For the last node in the list, it can be parent of an unspecified list of nodes.
             * 
             * The method also assumes that all the nodes in the list are BASIC_NORMAL_NODEs.
             * 
             * The new node will be child of the parents of the first node in the list, and 
             * it will be parent of all the children of the last node in the list.
             * 
             * The new node will contain the statements of all the nodes of the list.
             */
            void concat_nodes(ObjectList<Node*> node_l);
            
            //!
            void replace_node(Node* old_node, Node* new_node);
            
            //! This function clears the attribute #visited from nodes bellow @actual node.
            //! It works properly if there isn't any unreachable node in the graph bellow @actual.
            void clear_visits(Node* actual);
                       
            //! Set of methods that removes those nodes that can never be reached.                 
            void clear_orphaned_nodes(Node* actual_node);
            void clear_orphaned_nodes_in_subgraph(Node* actual_node);
            void clear_orphaned_cascade(Node* actual_node);
            
            //! Removes those nodes that has UNCLASSIFIED_NODE type and reconects parents and
            //! children nodes properly.
            void erase_unclassified_nodes(Node* actual);
            
            
            // *** DOT Graph *** //
            
            //! Build a DOT file that represents the CFG
            void print_graph_to_dot();
            
            
            // *** Static CFG Analysis *** //
            
            //! Computes the define-use chain of the cfg
            void live_variable_analysis();            
            
            //! Computes dependences for all task node in the Extensible Graph
            void analyse_tasks();
            
            //! Computes dependences for a node containing a task code
            void analyse_task(Node* task_node);
           
            
            // *** Getters and Setters *** //
            
            //! Returns the name of the graph
            std::string get_name() const;
            
            //! Returns the symbol of the function contained in the graph
            //! It is null when the graph do not corresponds to a function code
            Symbol get_function_symbol() const;
            
            //! Returns a list with all the function call symbols contained in the graph
            ObjectList<Node*> get_function_calls() const;
            
            //! Returns the node containing the graph
            Node* get_graph() const;

        friend class CfgVisitor;
    };
}

#endif // EXTENSIBLE_GRAPH_HPP
