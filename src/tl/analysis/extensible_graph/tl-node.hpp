/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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



#ifndef NODE_HPP
#define NODE_HPP

#include <map>

#include "tl-edge.hpp"
#include "tl-extensible-symbol.hpp"
#include "tl-structures.hpp"

#include "tl-builtin.hpp"
#include "tl-nodecl.hpp"
#include "tl-nodecl-alg.hpp"
#include "tl-objectlist.hpp"

namespace TL 
{
    namespace Analysis
    {
        typedef std::tr1::unordered_map<Nodecl::NodeclBase, Nodecl::NodeclBase, Nodecl::Utils::Nodecl_hash, Nodecl::Utils::Nodecl_comp> nodecl_map;
        
        class Edge;
        
        ///////////////////////////////////////////////////////////////
        /// Induction Variables data structure
        ///////////////////////////////////////////////////////////////
        struct InductionVariableData {
            Nodecl::NodeclBase _lb;         /*!< Lower bound within a loop */
            Nodecl::NodeclBase _ub;         /*!< Upper bound within a loop (included) */
            Nodecl::NodeclBase _stride;     /*!< Stride within a loop */
            char _type;                     /*!< Type of iv: '1' = basic, '2' = derived */
            Nodecl::NodeclBase _family;     /*!< Family of the IV. For basic IVs, the family is the IV itself */
            
            InductionVariableData(char type, Nodecl::NodeclBase family)
                : _lb(Nodecl::NodeclBase::null()), _ub(Nodecl::NodeclBase::null()), _stride(Nodecl::NodeclBase::null()),
                  _type(type), _family(family)
            {
                if ( type != '1' && type != '2' )
                    internal_error("Unexpected Induction Variable type '%c' while expecting 1 (basic) or 2 (derived)", 0);
            }
            
            bool is_basic()
            {
                return (_type == '1');
            }
        };
        
        typedef std::tr1::unordered_map<Nodecl::NodeclBase, InductionVariableData, Nodecl::Utils::Nodecl_hash, Nodecl::Utils::Nodecl_comp> IV_map;
        
        
        ///////////////////////////////////////////////////////////////
        /// A Node in the Extensible Graph
        ///////////////////////////////////////////////////////////////
        class LIBTL_CLASS Node : public LinkData {
            
            private:
                // *** Class attributes *** //
            
                int _id;
                ObjectList<Edge*> _entry_edges;
                ObjectList<Edge*> _exit_edges;
                bool _visited;
                bool _visited_aux;
            
                bool _has_deps_computed;    // This boolean only makes sense for Task nodes
                                            // It is true when the auto-dependencies for the node has been computed
                
                // *** Not allowed construction methods *** //
                Node(const Node& n);
                Node& operator=(const Node&);
                
                // *** Analysis *** //
                
                //! Traverses forward the nodes that do not contain Statements inside them.
                /*!
                The method stops when the processed node has a number of children different from 1 or
                does not contain statements.
                \return Pointer to the last node processed.
                */
                Node* advance_over_non_statement_nodes();
                
                //! Traverses backward the nodes that do not contain Statements inside them.
                /*!
                The method stops when the processed node has a number of parents different from 1 or
                does not contain statements.
                \return Pointer to the last node processed.
                */
                Node* back_over_non_statement_nodes();
                
                //! Returns a list with two elements. The firs is the list of upper exposed variables of the graph node;
                //! The second is the list of killed variables of the graph node (Used in composite nodes)
                ObjectList<ext_sym_set> get_use_def_over_nodes();
                
                //! Returns the list of live in variables in the node (Used in composite nodes)
                ext_sym_set get_live_in_over_nodes();
                
                //! Returns the list of live out variables in the node (Used in composite nodes)
                ext_sym_set get_live_out_over_nodes();
            
                //! Sets the variable represented by a symbol as a killed or an upper exposed variable 
                //! depending on @defined attribute
                /*!
                * A variable is killed when it is defined or redefined
                * A variable is upper exposed when it is used before of being killed
                * \param defined Action performed over the symbol: 1 if defined, 0 if not
                * \param n Nodecl containg the whole expression about the use/definition
                */
                void fill_use_def_sets(Nodecl::NodeclBase n, bool defined);
                
                //! Wrapper method for #fill_use_def_sets when there is more than one symbol to be analysed
                void fill_use_def_sets(Nodecl::List n_l, bool defined);
                
            public:
                // *** Constructors *** //
                
                //! Empty Node Constructor.
                /*!
                The method sets to -1 the node identifier and has empty entry and exit edges lists.
                The type of Node_type is, by default, UNCLASSIFIED_NODE.
                */
                Node();
                
                //! Node Constructor.
                /*!
                The entry and exit edges lists are empty.
                A node may contain other nodes, depending on its type.
                \param id Last identifier used to built a node (the method increments it by 1).
                \param outer_node Pointer to the wrapper node. If the node does not belong to other
                                    node, then this parameter must be NULL.
                */
                Node(int& id, Node_type type, Node* outer_node);
                
                //! Node Constructor for Basic Normal Nodes.
                /*!
                * The entry and exit edges lists are empty.
                * A node may contain other nodes, depending on its type.
                * \param id Last identifier used to built a node (the method increments it by 1).
                * \param outer_node Pointer to the wrapper node. If the node does not belong to other
                *                   node, then this parameter must be NULL.
                * \param nodecls List of Nodecl containing the Statements to be included in the new node
                */
                Node(int& id, Node_type type, Node* outer_node, ObjectList<Nodecl::NodeclBase> nodecls);

                //! Wrapper constructor in the for Basic Nodes with statements in the case that only one statement
                //! must be included in the list
                Node(int& id, Node_type type, Node* outer_node, Nodecl::NodeclBase nodecl);
                
                bool operator==(const Node& node) const;
            
                
                // *** Modifiers *** //
                
                //! Removes an entry edge from the correspondent list.
                /*!
                If the source node does not exist, then a warning message is shown.
                \param source Pointer to the source node of the Edge that will be erased.
                */
                void erase_entry_edge(Node* source);
                
                //! Removes an exit edge from the correspondent list.
                /*!
                * If the target node does not exist, then a warning message is shown.
                * \param source Pointer to the target node of the Edge that will be erased.
                */
                void erase_exit_edge(Node* target);
                
                
                // *** Getters and setters *** //
                
                //! Returns the node identifier
                int get_id() const;
                
                //! Sets the node identifier
                void set_id(int id);
                
                //! Returns a boolean indicating whether the node was visited or not.
                /*!
                * This method is useful when traversals among the nodes are performed.
                * Once the traversal is ended, all nodes must be set to non-visited using
                * set_visited method.
                */
                bool is_visited() const;
                bool is_visited_aux() const;
                
                //! Sets the node as visited.
                void set_visited(bool visited);
                void set_visited_aux(bool visited);
                
                //! Returns true when the node is a task node and its dependencies have already been calculated
                bool has_deps_computed();
                
                //! Sets node dependencies computation to true
                void set_deps_computed();
                
                //! Returns a boolean indicating whether the node is empty or not
                /*!
                * A empty node is created in the cases when we need a node to be returned but 
                * no node is needed to represent data.
                */
                bool is_empty_node();
                
                //! Returns the list of entry edges of the node.
                ObjectList<Edge*> get_entry_edges() const;
                
                //! Adds a new entry edge to the entry edges list.
                void set_entry_edge(Edge *entry_edge);
                
                //! Returns the list of entry edges types of the node.
                ObjectList<Edge_type> get_entry_edge_types();
                
                //! Returns the list of entry edges labels of the node.
                ObjectList<std::string> get_entry_edge_labels();
                
                //! Returns the list parent nodes of the node.
                ObjectList<Node*> get_parents();
                
                //! Returns the list of exit edges of the node.
                ObjectList<Edge*> get_exit_edges() const;
                
                //! Adds a new exit edge to the exit edges list.
                void set_exit_edge(Edge *exit_edge);
                
                //! Returns the list of exit edges types of the node.
                ObjectList<Edge_type> get_exit_edge_types();
                
                //! Returns the list of exit edges labels of the node.
                ObjectList<std::string> get_exit_edge_labels();
                
                //! Returns the edge between the node and a target node, if exists
                Edge* get_exit_edge(Node* target);
                
                //! Returns the list children nodes of the node.
                ObjectList<Node*> get_children();
            
                //! Returns true when the node is not a composite node (does not contain nodes inside).
                bool is_basic_node();
                
                //! Returns true when the node is connected to any parent and/or any child
                bool is_connected();
                
                //! Returns true when the node is in its children list
                bool has_child(Node* n);
                
                //! Returns true when the node is in its parents list
                bool has_parent(Node* n);
                
                //! Returns the list of nodes contained inside a node with type graph
                /*!
                * When the node is not a GRAPH_NODE, the list is empty.
                * Otherwise, returns all nodes in the the same level of nesting as the entry node of the graph node
                */
                ObjectList<Node*> get_inner_nodes_in_level();
                
                //! Recursive method to store a chain of nodes into a list
                /*!
                * \param node_l list where the nodes are stored
                */
                void get_inner_nodes_rec(ObjectList<Node*>& node_l);
                
                //! Returns the symbol of the function call contained in the node
                //! This method only works for composite nodes of type "function_call"
                Symbol get_function_node_symbol();
                
                
                
                // *** Getters and setters for liked data *** //
            
                //! Returns the node type.
                Node_type get_type();
                
                //! Returns a string with the node type of the node.
                std::string get_type_as_string();
                
                //! Returns a string with the graph type of the node.
                //! Node must be a GRAPH_NODE
                std::string get_graph_type_as_string();
                
                //! Returns the entry node of a Graph node. Only valid for graph nodes
                Node* get_graph_entry_node();

                //! Set the entry node of a graph node. Only valid for graph nodes
                void set_graph_entry_node(Node* node);
                
                //! Returns the exit node of a Graph node. Only valid for graph nodes
                Node* get_graph_exit_node();
                
                //! Set the exit node of a graph node. Only valid for graph nodes
                void set_graph_exit_node(Node* node);
                
                //! Returns the nodecl containing the label of the graph node (Only valid for graph nodes)
                //! If the graph doesn't have a label, a null Nodecl is returned
                Nodecl::NodeclBase get_graph_label(Nodecl::NodeclBase n = Nodecl::NodeclBase::null());
                
                //! Set the label of the graph node (Only valid for graph nodes)
                void set_graph_label(Nodecl::NodeclBase n);
                
                //! Returns type of the graph (Only valid for graph nodes)
                Graph_type get_graph_type();
                
                //! Set the graph type to the node (Only valid for graph nodes)
                void set_graph_type(Graph_type graph_type);
                
                //! Returns the type of the loop contained in the node. (Only valid for loop graph nodes)
                Loop_type get_loop_node_type();
                
                //! Set the type of loop contained in a loop graph node
                void set_loop_node_type(Loop_type loop_type);
                
                //! Returns the map of induction variables associated to the node (Only valid for loop graph nodes)
                IV_map get_induction_variables();
                
                //! Set a new induction variable in a loop graph node
                void set_induction_variable(Nodecl::NodeclBase iv, struct InductionVariableData iv_data);
                
                //! Returns a pointer to the node which contains the actual node
                //! When the node don't have an outer node, NULL is returned
                Node* get_outer_node();
                
                //! Set the node that contains the actual node. It must be a graph node
                void set_outer_node(Node* node);
                
                //! Returns the scope of a graph node containing a block of code.
                //! If no block is contained in the grah node, then returns an empty scope
                Scope get_scope();
                
                //! Set the scope of a graph node containing a block code
                void set_scope(Scope sc);
                
                //! Returns the list of statements contained in the node
                //! If the node does not contain statements, an empty list is returned
                ObjectList<Nodecl::NodeclBase> get_statements();
                
                //! Set the node that contains the actual node. It must be a graph node
                //! It is only valid for Normal nodes, Labeled nodes or Function Call nodes
                void set_statements(ObjectList<Nodecl::NodeclBase> stmts);
                
                //! Returns the Symbol of the statement label contained in the node
                //! If is only valid for Goto or Labeled nodes
                Symbol get_label();
                
                //! Returns the symbol of the statement label contained in the node
                //! If is only valid for Goto or Labeled nodes
                void set_label(Symbol s);
                
                Nodecl::NodeclBase get_task_context();
                
                void set_task_context(Nodecl::NodeclBase c);
                
                Symbol get_task_function();
                
                void set_task_function(Symbol func_sym);
                
                Node* get_stride_node();
                
                void set_stride_node(Node* stride);
                
                bool is_stride_node();
                
                // *** Consultants *** //
                //! Returns true if the node has the same identifier and the same entries and exits
                bool operator==(const Node* &n) const;
                
                
                // *** Analysis *** //
                
                void set_graph_node_use_def();
                
                //! This method computes the reaching definitions of a graph node from the reaching definitions in the nodes within it
                void set_graph_node_reaching_definitions();
            
                
                //! Returns the set of variables that are alive at the entry of the node.
                ext_sym_set get_live_in_vars();
                
                //! Adds a new live in variable to the node.
                void set_live_in(ExtensibleSymbol new_live_in_var);
                
                //! Sets the list of live in variables.
                /*!
                 * If there was any other data in the list, it is removed.
                 */
                void set_live_in(ext_sym_set new_live_in_set);
                
                //! Returns the set of variables that are alive at the exit of the node.
                ext_sym_set get_live_out_vars();
                
                //! Adds a new live out variable to the node.
                void set_live_out(ExtensibleSymbol new_live_out_var);
                
                //! Sets the list of live out variables.
                /*!
                If there was any other data in the list, it is removed.
                */
                void set_live_out(ext_sym_set new_live_out_set);
                
                //! Returns the list of upper exposed variables of the node
                ext_sym_set get_ue_vars();
                
                //! Adds a new upper exposed variable to the node
                void set_ue_var(ExtensibleSymbol new_ue_var);
                
                //! Adds a new set of upper exposed variable to the node
                void set_ue_var(ext_sym_set new_ue_vars);
                
                //! Deletes an old upper exposed variable from the node
                void unset_ue_var(ExtensibleSymbol old_ue_var);
                
                //! Returns the list of killed variables of the node
                ext_sym_set get_killed_vars();
                
                //! Adds a new killed variable to the node
                void set_killed_var(ExtensibleSymbol new_killed_var);

                //! Adds a new set of killed variables to the node
                void set_killed_var(ext_sym_set new_killed_vars);
                
                //! Deletes an old killed variable from the node
                void unset_killed_var(ExtensibleSymbol old_killed_var);            
                
                //! Returns the list of undefined behaviour variables of the node
                ext_sym_set get_undefined_behaviour_vars();
                
                //! Adds a new undefined behaviour variable to the node
                void set_undefined_behaviour_var(ExtensibleSymbol new_undef_var);            

                //! Adds a set of undefined behaviour variables to the node
                void set_undefined_behaviour_var(ext_sym_set new_undef_vars);

                //! Deletes an old undefined behaviour variable from the node
                void unset_undefined_behaviour_var(ExtensibleSymbol old_undef_var);
                
                //! Returns the list of input dependences of a task node
                ext_sym_set get_input_deps();
                
                //! Insert a list of input dependencies to the node
                void set_input_deps(ext_sym_set new_input_deps);
                
                //! Returns the list of output dependences of a task node
                ext_sym_set get_output_deps();
                               
                //! Insert a list of output dependencies to the node
                void set_output_deps(ext_sym_set new_output_deps);
                
                //! Returns the list of inout dependences of a task node
                ext_sym_set get_inout_deps();
                               
                //! Insert a list of inout dependencies to the node
                void set_inout_deps(ext_sym_set new_inout_deps);
                
                //! Returns the list of undefined dependences of a task node
                ext_sym_set get_undef_deps();
                               
                //! Insert a list of undefined dependencies to the node
                void set_undef_deps(ext_sym_set new_undef_deps);
                
                ext_sym_set get_shared_vars();
                
                void set_shared_var(ExtensibleSymbol ei);
                
                void set_shared_var(ext_sym_set new_shared_vars);

                ext_sym_set get_private_vars();
                
                void set_private_var(ExtensibleSymbol ei);
                
                void set_private_var(ext_sym_set new_private_vars);
                
                ext_sym_set get_firstprivate_vars();
                
                void set_firstprivate_var(ExtensibleSymbol ei);
                
                void set_firstprivate_var(ext_sym_set new_firstprivate_vars);
                
                ext_sym_set get_undef_sc_vars();
                
                void set_undef_sc_var(ExtensibleSymbol ei);
                
                void set_undef_sc_var(ext_sym_set new_undef_sc_vars);
                
                ext_sym_set get_race_vars();

                void set_race_var(ExtensibleSymbol ei);
                
                //! Return the map containing, for each symbol defined until this moment, its correspondent expression
                nodecl_map get_reaching_definitions();

                //! Set to one variable a new expression value and append this relationship to the node
                void set_reaching_definition(Nodecl::NodeclBase var, Nodecl::NodeclBase init);
                void set_reaching_definition_list(nodecl_map reach_defs_l);
                void rename_reaching_defintion_var(Nodecl::NodeclBase old_var, Nodecl::NodeclBase new_var);
            
                nodecl_map get_auxiliar_reaching_definitions();
                void set_auxiliar_reaching_definition(Nodecl::NodeclBase var, Nodecl::NodeclBase init);
                
                //!Deletes an old reaching definition from the node
                void unset_reaching_definition(Nodecl::NodeclBase var);
                
                
                // *** Utils *** //
                void print_use_def_chains();
                void print_liveness();
                void print_auto_scoping();
                void print_task_dependencies();
                
            friend class CfgAnalysisVisitor;
        };
    }
}

#endif // NODE_HPP
