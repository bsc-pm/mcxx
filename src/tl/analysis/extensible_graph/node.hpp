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


#ifndef NODE_HPP
#define NODE_HPP

#include "edge.hpp"
#include "extensible_symbol.hpp"
#include "structures.hpp"
#include "tl-ast.hpp"
#include "tl-builtin.hpp"
#include "tl-objectlist.hpp"
#include "tl-statement.hpp"

namespace TL 
{
    class Edge;
    
    class LIBTL_CLASS Node : public LinkData {
        private:
            // *** Class attributes *** //
           
            int _id;
            ObjectList<Edge*> _entry_edges;
            ObjectList<Edge*> _exit_edges;
            bool _visited;
           
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
            
            //! Returns the list of live in variables in the node (Used in composite nodes)
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_live_in_over_nodes();
            
            //! Returns the list of live out variables in the node (Used in composite nodes)
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_live_out_over_nodes();
            
            //! Computes liveness information (used and defined variables) from an Expression.
            /*!
              The method computes the used and defined variables of a node taking into
              account only the inner statements.
              \param e Expression that provides the liveness information.
              \param defined 0 when the expression is a left-hand one, 1 otherwise.
             */
            void set_live_initial_expression_information(Expression e, bool defined);
           
            //! Sets the variable represented by a symbol as a killed or an upper exposed variable 
            //! depending on @defined attribute
            /*!
              A variable is killed when it is defined or redefined
              A variable is upper exposed when it is used before of being killed
             */
            void fill_use_def_sets(Symbol s, bool defined);
            
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
            Node(int& id, Node_type type, Node* outer_graph);

            
            // *** Modifiers *** //
            
            //! Removes an entry edge from the correspondent list.
            /*!
              If the source node does not exist, then a warning message is shown.
              \param source Pointer to the source node of the Edge that will be erased.
             */
            void erase_entry_edge(Node* source);
            
            //! Removes an exit edge from the correspondent list.
            /*!
              If the target node does not exist, then a warning message is shown.
              \param source Pointer to the target node of the Edge that will be erased.
             */
            void erase_exit_edge(Node* target);
            
            
            // *** Getters and setters *** //
            
            //! Returns the node identifier
            int get_id() const;
            
            //! Sets the node identifier
            void set_id(int id);
            
            //! Returns a boolean indicating whether the node was visited or not.
            /*!
              This method is useful when traversals among the nodes are performed.
              Once the traversal is ended, all nodes must be set to non-visited using 
              set_visited method.
             */
            bool is_visited() const;
            
            //! Sets node member #visited.
            void set_visited(bool visited);
            
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
            
            //! Returns the list children nodes of the node.
            ObjectList<Node*> get_children();
            
            //! Returns the node type.
            Node_type get_node_type();
            
            //! Returns a string representing the node type of the node.
            std::string get_node_type_as_string();
            
            //! Returns true when the node is not a composite node (does not contain nodes inside).
            bool is_basic_node();
            
            
            // *** Analysis *** //
            
            //! Sets the initial liveness information of the node.
            /*!
              The method computes the used and defined variables of a node taking into account only
              the inner statements.
             */
            void set_live_initial_information(ScopeLink sl);
            
            //! Applies liveness analysis in a composite node.
            /*!
              The method extends the liveness information precomputed in the inner nodes of a
              composite node to the outer node.
              The method fails when it is tried to apply it in a basic node.
             */
            void set_graph_node_liveness();
            
            //! Returns the set of variables that are alive at the entry of the node.
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_live_in_vars();
            
            //! Adds a new live in variable to the node.
            void set_live_in(ExtensibleSymbol new_live_in_var);
            
            //! Sets the list of live in variables.
            /*!
              If there was any other data in the list, it is removed.
             */
            void set_live_in(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> new_live_in_set);
            
            //! Returns the set of variables that are alive at the exit of the node.
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_live_out_vars();
            
            //! Adds a new live out variable to the node.
            void set_live_out(ExtensibleSymbol new_live_out_var);
            
            //! Sets the list of live out variables.
            /*!
              If there was any other data in the list, it is removed.
             */
            void set_live_out(std::set<ExtensibleSymbol, ExtensibleSymbol_comp> new_live_out_set);
            
            //! Returns the list of upper exposed variables of the node
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_ue_vars();
            
            //! Adds a new upper exposed variable to the node
            void set_ue_var(ExtensibleSymbol new_ue_var);
            
            //! Returns the list of killed variables of the node
            std::set<ExtensibleSymbol, ExtensibleSymbol_comp> get_killed_vars();
            
            //! Adds a new killed variable to the node
            void set_killed_var(ExtensibleSymbol new_killed_var);
    };
}

#endif // NODE_HPP