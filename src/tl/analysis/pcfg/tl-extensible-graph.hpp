/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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



#ifndef TL_EXTENSIBLE_GRAPH_HPP
#define TL_EXTENSIBLE_GRAPH_HPP

#include <algorithm>
#include <map>
#include <stack>

#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "tl-analysis-utils.hpp"
#include "tl-edge.hpp"
#include "tl-extended-symbol-utils.hpp"
#include "tl-node.hpp"
#include "tl-nodecl.hpp"
#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {
    
    class LIBTL_CLASS ExtensibleGraph
    {
    protected:
        // *** Class attributes *** //
        std::string _name;  /*!< Unique name representing the graph */
        Node* _graph;       /*!< Node with type GRAPH_NODE which contains the whole graph */

        PCFGVisitUtils* _utils;      /*!< Class storing temporary values for the construction of the graph */

        const Nodecl::NodeclBase _nodecl;  /*!< Nodecl corresponding to the code contained in the graph */

        /*!Graph scope (This variable is used when the variables are tagged as global)
         * If the graph contains a function code, the scope is the function's scope.
         * If the graph contains a block of code, the scope is the scope of the function containing the code.
         */
        Scope _sc;

        //! Set of global variables appearing in the graph or, eventually (when use-def analysis is performed),
        //* also global variables appearing in functions called in this graph ( any level of function nesting )
        std::set<Symbol> _global_vars;

        /*! Symbol of the function contained in the graph.
         *  This symbol is empty when the code contained in the graph do not correspond to a function
         */
        Symbol _function_sym;

        //! Map of nodes with the relationship between a new node and an old node when a piece of graph is copied
        /*! The key is the old node and the value is the new node
        */
        std::map<Node*, Node*> nodes_m;


        // *** Values used during the analysis *** //

        //! List of nodes containing task's code
        ObjectList<Node*> _task_nodes_l;

        //! List of functions called by the function stored in the graph
        ObjectList<Symbol> _func_calls;

        //! Map that relates each task in the graph with the tasks that are concurrent with it
        std::map<Node*, ObjectList<Node*> > _concurrent_tasks;
        //! Map that relates each task in the graph with the immediately previous nodes where it is synchronized
        std::map<Node*, ObjectList<Node*> > _last_sync;
        //! Map that relates each task in the graph with the immediately next nodes where it is synchronized
        std::map<Node*, ObjectList<Node*> > _next_sync;
        
        // *** DOT Graph *** //
        //! Map used during PCFG outlining that contains the mapping between DOT cluster and its ENTRY node
        std::map<int, int> _cluster_to_entry_map;

    private:
        //! We don't want to allow this kind of constructions
        ExtensibleGraph(const ExtensibleGraph& graph);
        ExtensibleGraph& operator=(const ExtensibleGraph&);

        //! This method removes all those nodes that are unreachable and those that were created
        //! as auxiliary nodes when the graph was created.
        //! It also joins those nodes that are always consecutively executed and non of them
        //! are the target of a jump.
        void clear_unnecessary_nodes( );

        //! This method concatenates all those nodes that form a Basic Block in one only node.
        //! It creates a new node containing all the statements and deleted the previous nodes.
        void concat_sequential_nodes( );
        void concat_sequential_nodes_recursive( Node* actual_node, ObjectList<Node*>& last_seq_nodes );

        //! Method used during the copy method when the edges must be copied before connecting the nodes
        void connect_nodes(ObjectList<Node*> parents, Node* child, ObjectList<Edge*> edges);

        //! Method used during the copy method when the edges must be copied before connecting the nodes
        void connect_nodes(Node* parent, ObjectList<Node*> children, ObjectList<Edge*> edges);

        //! Method used during the copy method that copies the graph recursively
        void copy_and_map_nodes(Node* old_node);

        void connect_copied_nodes(Node* old_node);

        //! Removes those nodes that has UNCLASSIFIED_NODE type and reconnects parents and
        //! children nodes properly.
        void erase_unclassified_nodes( Node* current );

        void erase_jump_nodes( Node* current );

        //! Structurally looks for nodecl 'n' in 'current' and its successors
        Node* find_nodecl_rec( Node* current, const Nodecl::NodeclBase& n );
        
        //! Looks for the same pointer nodecl 'n' in 'current' and its successors
        Node* find_nodecl_pointer_rec( Node* current, const Nodecl::NodeclBase& n );
        
        
        // *************************************************************************************** //
        // ********************************* DOT printing methods ******************************** //
        
        //! Prints nodes and relations between them in a string in a recursive way.
        /*!
         * \param actual_node Source node from which the printing is started.
         * \param dot_graph Inout parameter where the DOT is printed.
         * \param outer_edges Set of edges that must be printed in an outer DOT cluster.
         * \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
         * \param indent Indentation for the actual node when it is printed.
         * \param subgraph_id Identifier for the actual cluster.
         */
        void get_nodes_dot_data( Node* actual_node, std::string& dot_graph, std::string& dot_analysis_info,
                                 std::vector<std::vector< std::string> >& outer_edges, 
                                 std::vector<std::vector<Node*> >& outer_nodes, std::string indent );
        
        //! Prints both nodes and edges within a pcfg subgraph
        //! Prints nodes and relations between them in a string in a recursive way.
        /*!
         * \param actual_node Source node from which the printing is started.
         * \param graph_data Inout parameter where the DOT is printed.
         * \param outer_edges Set of edges that must be printed in an outer DOT cluster.
         * \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
         * \param indent Indentation for the actual node when it is printed.
         * \param subgraph_id Identifier for the actual cluster.
         */
        void get_dot_subgraph( Node* actual_node, std::string& graph_data, std::string& graph_analysis_info,
                               std::vector<std::vector< std::string> >& outer_edges, 
                               std::vector<std::vector<Node*> >& outer_nodes, std::string indent );
        
        //! Prints the data of an only node.
        void get_node_dot_data( Node* node, std::string& graph_data, std::string& graph_analysis_info, std::string indent );
        
        //! Method printing the nodes containing analysis info into the DOT file
        void print_node_analysis_info( Node* current, std::string& dot_analysis_info,
                                       std::string cluster_name );
        
        //! Prints OpenMP clauses information only for OpenMP nodes
        std::string print_pragma_node_clauses( Node* current, std::string indent, std::string cluster_name );
        
        // ******************************* END DOT printing methods ****************************** //
        // *************************************************************************************** //
        

    public:
        // *** Constructors *** //

        //! Constructor used by the different phases to build a new ExtensibleGraph.
        /*!
        \param name Name which will identify the graph.
        */
        ExtensibleGraph( std::string name, const Nodecl::NodeclBase& nodecl, PCFGVisitUtils* utils );

        //! Creates a new graph with the same characteristics of the actual graph
//         ExtensibleGraph* copy( );


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
        Node* append_new_node_to_parent( ObjectList<Node*> parent, ObjectList<Nodecl::NodeclBase> nodecl,
                                         Node_type ntype = __Normal, Edge_type etype = __Always);


        Node* append_new_node_to_parent( Node* parent, Nodecl::NodeclBase nodecl,
                                         Node_type ntype = __Normal, Edge_type etype = __Always );

        Node* append_new_node_to_parent( Node* parent, ObjectList<Nodecl::NodeclBase> nodecl,
                                         Node_type ntype = __Normal, Edge_type etype = __Always );

        Node* append_new_node_to_parent( ObjectList<Node*> parents, Nodecl::NodeclBase nodecl,
                                         Node_type ntype = __Normal, Edge_type etype = __Always );

        //! Connects two nodes by creating a new edge between them.
        /*!
        * \param parent Source node of the connection.
        * \param child Target node of the connection.
        * \param etype Type of the connection between the two nodes.
        * \param label Label for the connection. It will be used when a Catch or a Case edges
        *              are built.
        * \param is_task_edge Bool indicating whether the target node is a task.
        *                     Thus, the edge will have special type _TASK_EDGE
        * \return The new edge created between the two nodes
        */
        Edge* connect_nodes( Node* parent, Node* child, Edge_type etype = __Always, 
                             Nodecl::NodeclBase label = Nodecl::NodeclBase::null( ),
                             bool is_task_edge = false );

        //! Wrapper method for #connect_nodes when a set of parents must be connected to a
        //! set of children and each connection may be different from the others.
        //! A set of edge types and labels must be provided. It is assumed that each parent is
        //! connected to all its children with the same type of edge.
        void connect_nodes( ObjectList<Node*> parents, ObjectList<Node*> children,
                            ObjectList<Edge_type> etypes, ObjectList<Nodecl::NodeclBase> elabels );

        //! Wrapper method for #connect_nodes when a parent must be connected to a set of
        //! children and each connection may be different from the others.
        //! A set of edge types and labels must be provided.
        void connect_nodes( Node* parent, ObjectList<Node*> children,
                            ObjectList<Edge_type> etypes, ObjectList<Nodecl::NodeclBase> labels );

        //! Wrapper method for #connect_nodes when a set of parents must be connected to an
        //! only child and the nature of the connection is the same for all of them.
        void connect_nodes( ObjectList<Node*> parents, Node* child, ObjectList<Edge_type> etypes,
                            ObjectList<Nodecl::NodeclBase> labels, bool is_task_edge = false );

        //! Wrapper method for #connect_nodes when a set of parents must be connected to an
        //! only child and the nature of the connection is the same for all of them.
        void connect_nodes( ObjectList<Node*> parents, Node* child, Edge_type etype = __Always,
                            Nodecl::NodeclBase label = Nodecl::NodeclBase::null( ) );

        //! Wrapper method for #disconnect_nodes when a set of parents is connected to a child.
        void disconnect_nodes( ObjectList<Node*> parents, Node* child );

        //! Wrapper method for #disconnect_nodes when a set of children is connected to a parent
        void disconnect_nodes( Node* parent, ObjectList<Node*> children );

        //! Disconnects two nodes.
        /*!
        The method warnings if it is tried to remove an non-existent edge.
        \param parent Source of the connection to be removed.
        \param child Target of the connection to be removed.
        */
        void disconnect_nodes( Node *parent, Node *child );

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
        Node* create_graph_node( Node* outer_node, Nodecl::NodeclBase label,
                                 Graph_type graph_type, Nodecl::NodeclBase context = Nodecl::NodeclBase::null( ) );

        //! Builds a Flush node and connects it with the existent graph
        Node* create_flush_node( Node* outer_node, Nodecl::NodeclBase n = Nodecl::NodeclBase::null( ) );

        //! Builds a basic normal node (BASIC_NORMAL_NODE)
        /*!
        * \param nodecl Statement that will be added to the new node
        */
        Node* create_unconnected_node( Node_type type, Nodecl::NodeclBase nodecl );

        //! Deletes a node from the graph
        /*!
        * The method erases the node from the list of children of its parent and
        * from the list of parents of its children as well. Finally, frees the pointer.
        * \param n Pointer to the node to be deleted
        */
        void delete_node( Node* n );

        //! This method traverses the graph and clears all the unreachable nodes and  those nodes created
        //! during the construction of the graph but do not represent any statement of the code, and also
        //! concatenates the nodes that will be executed sequentially for sure (Basic Blocks)
        void dress_up_graph( );

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
        void concat_nodes( ObjectList<Node*> node_l );

        //!
        void replace_node( Node* old_node, Node* new_node );

        //! This function clears the attribute #visited from nodes bellow @actual node.
        //! It works properly if there isn't any unreachable node in the graph bellow @actual.
        static void clear_visits( Node* node );
        static void clear_visits_aux( Node* node );
        static void clear_visits_extgraph( Node* node );
        static void clear_visits_extgraph_aux( Node* node );
        static void clear_visits_in_level( Node* node, Node* outer_node );
        static void clear_visits_aux_in_level( Node* node, Node* outer_node );
        static void clear_visits_backwards( Node* node );
        static void clear_visits_backwards_in_level( Node* current, Node* outer_node );
        
        
        // *** DOT Graph *** //

        //! Build a DOT file that represents the CFG
        void print_graph_to_dot( bool usage = false, bool liveness = false, 
                                 bool reaching_defs = false, bool induction_vars = false, bool ranges = false,
                                 bool auto_scoping = false, bool auto_deps = false );


        // *** Getters and Setters *** //

        //! Returns the name of the graph
        std::string get_name( ) const;

        //! Returns the nodecl contained in the graph
        Nodecl::NodeclBase get_nodecl( ) const;

        //! Returns the scope enclosing the code contained in the graph
        Scope get_scope( ) const;

        std::set<Symbol> get_global_variables( ) const;
        void set_global_vars( const std::set<Symbol>& global_vars );

        //! Returns the symbol of the function contained in the graph
        //! It is null when the graph do not corresponds to a function code
        Symbol get_function_symbol( ) const;

        //! Returns the node containing the graph
        Node* get_graph( ) const;

        //! Returns the list of nodes containing a task which are created within this graph
        ObjectList<Node*> get_tasks_list( ) const;

        ObjectList<Symbol> get_function_parameters( ) const;

        void add_func_call_symbol( Symbol s );

        ObjectList<Symbol> get_function_calls( ) const;
        
        // Task synchronization analysis
        // We need this information here because it is used in multiple analysis (liveness, auto-scoping)
        ObjectList<Node*> get_task_concurrent_tasks( Node* task );
        void add_concurrent_task_group( Node* task, ObjectList<Node*> concurrent_tasks );
        ObjectList<Node*> get_task_last_synchronization( Node* task );
        void add_last_synchronization( Node* task, ObjectList<Node*> last_sync );
        ObjectList<Node*> get_task_next_synchronization( Node* task );
        void add_next_synchronization( Node* task, ObjectList<Node*> next_sync );
        
        
        // *** Consultants *** //
        static Node* is_for_loop_increment( Node* node );
        static bool node_is_in_loop( Node* current );
        static bool node_is_in_conditional_branch( Node* current, Node* max_outer = NULL );
        static bool node_is_in_synchronous_construct( Node* current );
        static bool is_backward_parent( Node* son, Node* parent );
        static bool node_contains_node( Node* container, Node* contained );
        static Node* get_extensible_graph_from_node( Node* node );
        static bool node_is_ancestor_of_node( Node* ancestor, Node* descendant );
        static Node* get_omp_enclosing_node( Node* current );
        static Edge* get_edge_between_nodes( Node* source, Node* target );
        static Node* get_enclosing_context( Node* n );
        static Node* get_enclosing_task( Node* n );
        static bool task_encloses_task( Node* container, Node* contained );
        static bool node_contains_tasks( Node* graph_node, Node* current, ObjectList<Node*>& tasks );
        static Node* get_enclosing_control_structure( Node* node );
        bool is_first_statement_node(Node* node);
        
        // *** Analysis methods *** //
        //!Returns true if a given nodecl is not modified in a given context
        static bool is_constant_in_context( Node* context, Nodecl::NodeclBase c );
        
        static bool has_been_defined( Node* current, Node* scope, const Nodecl::NodeclBase& n );
        
        Node* find_nodecl( const Nodecl::NodeclBase& n );           // structural search
        Node* find_nodecl_pointer( const Nodecl::NodeclBase& n );   // pointer search
        
        bool usage_is_computed( );

    friend class PCFGVisitor;
    };

}
}

#endif // TL_EXTENSIBLE_GRAPH_HPP
