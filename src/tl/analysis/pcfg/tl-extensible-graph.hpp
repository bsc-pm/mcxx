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



#ifndef TL_EXTENSIBLE_GRAPH_HPP
#define TL_EXTENSIBLE_GRAPH_HPP

#include <algorithm>
#include <map>
#include <stack>

#include "cxx-codegen.h"
#include "cxx-utils.h"
#include "tl-analysis-utils.hpp"
#include "tl-edge.hpp"
#include "tl-node.hpp"
#include "tl-nodecl.hpp"
#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {
    
    typedef std::map<NBase, NBase, Nodecl::Utils::Nodecl_structural_less> SizeMap;
    
    class LIBTL_CLASS ExtensibleGraph
    {
    protected:
        // *** Class attributes *** //
        std::string _name;  /*!< Unique name representing the graph */
        Node* _graph;       /*!< Node with type GRAPH_NODE which contains the whole graph */

        PCFGVisitUtils* _utils;      /*!< Class storing temporary values for the construction of the graph */

        const NBase _nodecl;  /*!< Nodecl corresponding to the code contained in the graph */

        /*!Graph scope (This variable is used when the variables are tagged as global)
         * If the graph contains a function code, the scope is the function's scope.
         * If the graph contains a block of code, the scope is the scope of the function containing the code.
         */
        Scope _sc;

        //! Set of global variables appearing in the graph or, eventually (when use-def analysis is performed),
        //* also global variables appearing in functions called in this graph (any level of function nesting)
        NodeclSet _global_vars;

        /*! Symbol of the function contained in the graph.
         *  This symbol is empty when the code contained in the graph do not correspond to a function
         */
        Symbol _function_sym;

        /*! Unique node in the PCFG representing an ulterior synchronization point
         *  for any task in the graph not synchronized within the graph
         */
        Node* _post_sync;

        //! Map relating a symbol with pointer type and the number of elements hidden in the pointer
        SizeMap _pointer_to_size_map;
        
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

        //! Map that relates each task with the previous points in the code 
        //! from where to look for sequential code concurrent with the task
        std::map<Node*, ObjectList<Node*> > _last_sync_tasks;
        std::map<Node*, ObjectList<Node*> > _last_sync_sequential;

        //! Map that relates each task with the posterior points in the code
        //! from where to look for sequential code concurrent with the task
        std::map<Node*, ObjectList<Node*> > _next_sync_tasks;
        std::map<Node*, ObjectList<Node*> > _next_sync_sequential;


        // *** DOT Graph *** //
        //! Map used during PCFG outlining that contains the mapping between DOT cluster and its ENTRY node
        std::map<int, int> _cluster_to_entry_map;

        // *** Variables storing info about analyses built on top of the PCFG *** //
        bool _usage_computed;

    private:
        //! We don't want to allow this kind of constructions
        ExtensibleGraph(const ExtensibleGraph& graph);
        ExtensibleGraph& operator=(const ExtensibleGraph&);

        //! This method removes all those nodes that are unreachable and those that were created
        //! as auxiliary nodes when the graph was created.
        //! It also joins those nodes that are always consecutively executed and non of them
        //! are the target of a jump.
        void clear_unnecessary_nodes();
        
        void remove_unnecessary_connections_rec(Node* n);

        //! This method concatenates all those nodes that form a Basic Block in one only node.
        //! It creates a new node containing all the statements and deleted the previous nodes.
        void concat_sequential_nodes();
        void concat_sequential_nodes_recursive(Node* actual_node, ObjectList<Node*>& last_seq_nodes);

        //! Removes those nodes that has UNCLASSIFIED_NODE type and reconnects parents and
        //! children nodes properly.
        void erase_unclassified_nodes(Node* current);

        void erase_jump_nodes(Node* current);

        //! Structurally looks for nodecl 'n' in 'current' and its successors
        Node* find_nodecl_rec(Node* current, const NBase& n);
        
        //! Looks for the same pointer nodecl 'n' in 'current' and its successors
        Node* find_nodecl_pointer_rec(Node* current, const NBase& n);
        
        
        // *************************************************************************************** //
        // ********************************* DOT printing methods ******************************** //
        
        void create_and_connect_node(Node* source, Node* target, 
                Node* real_source, Node* real_target, 
                std::string& dot_graph, std::string& dot_analysis_info,
                std::vector<std::vector<std::string> >& outer_edges, 
                std::vector<std::vector<Node*> >& outer_nodes, std::string indent);
        
        //! Prints nodes and relations between them in a string in a recursive way.
        /*!
         * \param actual_node Source node from which the printing is started.
         * \param dot_graph Inout parameter where the DOT is printed.
         * \param outer_edges Set of edges that must be printed in an outer DOT cluster.
         * \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
         * \param indent Indentation for the actual node when it is printed.
         * \param subgraph_id Identifier for the actual cluster.
         */
        void get_nodes_dot_data(Node* actual_node, std::string& dot_graph, std::string& dot_analysis_info,
                                 std::vector<std::vector< std::string> >& outer_edges, 
                                 std::vector<std::vector<Node*> >& outer_nodes, std::string indent);
        
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
        void get_dot_subgraph(Node* actual_node, std::string& graph_data, std::string& graph_analysis_info,
                               std::vector<std::vector< std::string> >& outer_edges, 
                               std::vector<std::vector<Node*> >& outer_nodes, std::string indent);
        
        //! Prints the data of an only node.
        void get_node_dot_data(Node* node, std::string& graph_data, std::string& graph_analysis_info, std::string indent);
        
        //! Method printing the nodes containing analysis info into the DOT file
        void print_node_analysis_info(Node* current, std::string& dot_analysis_info,
                                       std::string cluster_name);
        
        //! Prints OpenMP clauses information only for OpenMP nodes
        std::string print_pragma_node_clauses(Node* current, std::string indent, std::string cluster_name);
        
        // ******************************* END DOT printing methods ****************************** //
        // *************************************************************************************** //
        

    public:
        // *** Constructors *** //

        //! Constructor used by the different phases to build a new ExtensibleGraph.
        /*!
        \param name Name which will identify the graph.
        */
        ExtensibleGraph(std::string name, const NBase& nodecl, PCFGVisitUtils* utils);


        // *** Modifiers *** //

        //! This method creates a new node containing a Basic Block and connects it to its
        //! parent node with a new edge.
        /*!
        * \param parents Set of Parents of the new node.
         * \param stmts      Set of statements enclosed in the new node.
         * \param ntype      Type of the node to be created. Default: Basic_Normal.
         * \param etype      Type of the new edge/s connecting the new node with \p parents. Default: Always.
         * \return           The new node
        */
        Node* append_new_child_to_parent(ObjectList<Node*> parents, ObjectList<NBase> stmts,
                                         NodeType ntype = __Normal, EdgeType etype = __Always);

        //! Overladed method for unique \p parent and unique \p statement
        Node* append_new_child_to_parent(Node* parent, NBase stmt,
                                         NodeType ntype = __Normal, EdgeType etype = __Always);

        //! Overladed method for multiple \p parents and unique \p statement
        Node* append_new_child_to_parent(ObjectList<Node*> parents, NBase stmt,
                                         NodeType ntype = __Normal, EdgeType etype = __Always);

        //! Connects two nodes by creating a new edge between them. Only if they were not connected.
        /*!
        * \param parent Source node of the connection.
        * \param child Target node of the connection.
        * \param etype Type of the connection between the two nodes.
         * \param label         Label for the connection. Only necessary for Catch and Case edges.
         * \param is_task_edge  Boolean indicating whether the target node is a task.
         *                      In that case, the edge will have special attribute Task_Edge.
         * \param is_back_edge  Boolean indicating whether the connection is a back edge.
         *                      In such case, the edge will have a special attribute Back_Edge.
        * \return The new edge created between the two nodes
        */
        Edge* connect_nodes(Node* parent, Node* child, EdgeType etype = __Always,
                            const NBase& label = NBase::null(),
                             bool is_task_edge = false, bool is_back_edge = false);

        //! Overloaded method for a set of \p parents and a set of \p children.
        void connect_nodes(const ObjectList<Node*>& parents, const ObjectList<Node*>& children,
                           const ObjectList<EdgeType>& etypes=ObjectList<EdgeType>(),
                           const ObjectList<NBase>& elabels=ObjectList<NBase>());

        //! Overloaded method for a \p parent and a set of \p children.
        void connect_nodes(Node* parent, const ObjectList<Node*>& children,
                           const ObjectList<EdgeType>& etypes=ObjectList<EdgeType>(),
                           const ObjectList<NBase>& elabels=ObjectList<NBase>());

        //! Overloaded method for a set of \p parents and a \p children.
        void connect_nodes(const ObjectList<Node*>& parents, Node* child, 
                           const ObjectList<EdgeType>& etypes=ObjectList<EdgeType>(),
                           const ObjectList<NBase>& elabels=ObjectList<NBase>(), 
                            bool is_task_edge = false, bool is_back_edge = false);

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
        Node* create_graph_node(Node* outer_node, NBase label,
                                GraphType graph_type, NBase context = NBase::null());

        //! Builds a Flush node and connects it with the existent graph
        Node* create_flush_node(Node* outer_node, NBase n = NBase::null());

        //! Builds a basic normal node (BASIC_NORMAL_NODE)
        /*!
        * \param nodecl Statement that will be added to the new node
        */
        Node* create_unconnected_node(NodeType type, NBase nodecl);

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

        //! Set to false the attribute #_visited of those nodes whose dominator is node @node
        static void clear_visits(Node* node);

        //! Set to false the attribute #_visited_aux of those nodes whose dominator is node @node
        static void clear_visits_aux(Node* node);

        //! Set to false the attribute #_visited_extgraph of those nodes whose dominator is node @node
        static void clear_visits_extgraph(Node* node);

        //! Set to false the attribute #_visited_extgraph_aux of those nodes whose dominator is node @node
        static void clear_visits_extgraph_aux(Node* node);

        //! Set to false the attribute #_visited of those nodes fulfilling the conditions:
        //! - their dominator is node @node
        //! - they are enclosed in the graph node @outer_node
        static void clear_visits_in_level(Node* node, Node* outer_node);

        //! Set to false the attribute #_visited of those nodes fulfilling the conditions:
        //! - their dominator is node @node
        //! - they are immediately enclosed in the graph node @outer_node => no other graph node is in between them and @outer_node
        static void clear_visits_in_level_no_nest(Node* node, Node* outer_node);

        //! Set to false the attribute #_visited_aux of those nodes fulfilling the conditions:
        //! - their dominator is node @node
        //! - they are enclosed in the graph node @outer_node
        static void clear_visits_aux_in_level(Node* node, Node* outer_node);

        //! Set to false the attribute #_visited of those nodes whose post-dominator is node @node
        // within the scope of @graph
        static void clear_visits_backwards_in_level(Node* node, Node* graph);

        //! Set to false the attribute #_visited of those nodes whose post-dominator is node @node
        static void clear_visits_backwards(Node* node);

        // *** DOT Graph *** //

        //! Build a DOT file that represents the CFG
        void print_graph_to_dot(bool usage = false, bool liveness = false, 
                                 bool reaching_defs = false, bool induction_vars = false, bool ranges = false,
                                 bool auto_scoping = false, bool auto_deps = false);


        // *** Getters and Setters *** //

        //! Returns the name of the graph
        std::string get_name() const;

        //! Returns the nodecl contained in the graph
        NBase get_nodecl() const;

        //! Returns the scope enclosing the code contained in the graph
        Scope get_scope() const;

        const NodeclSet& get_global_variables() const;
        void set_global_vars(const NodeclSet& global_vars);

        //! Returns the symbol of the function contained in the graph
        //! It is null when the graph do not corresponds to a function code
        Symbol get_function_symbol() const;

        Node* get_post_sync() const;
        void set_post_sync(Node* post_sync);

        void set_pointer_n_elems(const NBase& s, const NBase& size);
        NBase get_pointer_n_elems(const NBase& s);
        SizeMap get_pointer_n_elements_map();
        void purge_non_constant_pointer_n_elems();
        
        //! Returns the node containing the graph
        Node* get_graph() const;

        //! Returns the list of nodes containing a task which are created within this graph
        ObjectList<Node*> get_tasks_list() const;

        ObjectList<Symbol> get_function_parameters() const;

        void add_func_call_symbol(Symbol s);

        ObjectList<Symbol> get_function_calls() const;
        
        // Task synchronization analysis
        // We need this information here because it is used in multiple analysis (liveness, auto-scoping)
        ObjectList<Node*> get_task_concurrent_tasks(Node* task) const;
        void add_concurrent_task_group(Node* task, ObjectList<Node*> concurrent_tasks);

        ObjectList<Node*> get_task_last_sync_for_tasks(Node* task) const;
        ObjectList<Node*> get_task_last_sync_for_sequential_code(Node* task) const;
        void add_last_sync_for_tasks(Node* task, Node* last_sync);
        void set_last_sync_for_tasks(Node* task, Node* last_sync);
        void add_last_sync_for_sequential_code(Node* task, Node* last_sync);

        ObjectList<Node*> get_task_next_sync_for_tasks(Node* task) const;
        ObjectList<Node*> get_task_next_sync_for_sequential_code(Node* task) const;
        void add_next_sync_for_tasks(Node* task, Node* next_sync);
        void add_next_sync_for_sequential_code(Node* task, Node* next_sync);
        void remove_next_sync_for_tasks(Node* task, Node* next_sync);
        void remove_concurrent_task(Node* task, Node* old_concurrent_task);

        // *** Consultants *** //
        static Node* is_for_loop_increment(Node* node);
        static bool node_is_in_loop(Node* current);
        static bool node_is_in_conditional_branch(Node* current, Node* max_outer = NULL);
        static bool node_is_in_synchronous_construct(Node* current);
        static bool is_backward_parent(Node* son, Node* parent);
        static bool node_contains_node(Node* container, Node* contained);
        static Node* get_extensible_graph_from_node(Node* node);
        static bool node_is_ancestor_of_node(Node* ancestor, Node* descendant);
        static Node* get_omp_enclosing_node(Node* current);
        static Edge* get_edge_between_nodes(Node* source, Node* target);
        static Node* get_enclosing_context(Node* n);
        static Node* get_most_outer_parallel(Node* n);
        static Node* get_most_outer_loop(Node* n);
        static Node* get_enclosing_task(Node* n);
        static bool task_encloses_task(Node* container, Node* contained);
        static bool node_contains_tasks(Node* graph_node, Node* current, ObjectList<Node*>& tasks);
        static Node* get_enclosing_control_structure(Node* node);
        static Node* get_task_creation_from_task(Node* task);
        static Node* get_task_from_task_creation(Node* task_creation);
        static bool task_synchronizes_in_post_sync(Node* task);
        
        // *** Analysis methods *** //
        static bool has_been_defined(Node* current, Node* scope, const NBase& n);

        Node* find_nodecl(const NBase& n);           // structural search
        Node* find_nodecl_pointer(const NBase& n);   // pointer search

        // *** Getters and setters for analyses built on top of the PCFG *** //
        bool usage_is_computed() const;
        void set_usage_computed();

    friend class PCFGVisitor;
    };

}
}

#endif // TL_EXTENSIBLE_GRAPH_HPP
