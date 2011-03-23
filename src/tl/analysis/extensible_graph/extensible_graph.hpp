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

#ifndef CFG_HPP
#define CFG_HPP

#include <algorithm>
#include <stack>

#include "node.hpp"
#include "tl-statement.hpp"
#include "tl-scopelink.hpp"

namespace TL
{
    class ExtensibleGraph
    {
        private:
            Node* _entry;
            Node* _exit;
            ScopeLink _sl;
            std::string _name;
            int _nid;
            
            //! Stacks to keep the exit nodes of Loop Statements in order to deal whit Continue and Break Statements
            std::stack<Node*> _continue_stack;
            std::stack<Node*> _break_stack;
            
            //! Lists to keep special nodes that breaks the expected behaviour of the flow
            ObjectList<Node*> _unhand_try_excpt_list;
            ObjectList<Node*> _labeled_node_list;
            ObjectList<Node*> _goto_node_list;
            ObjectList<Node*> _throw_node_list;
            
            bool _continue_stmt;
            bool _break_stmt;
            bool _goto_stmt;
            
            bool belongs_to_the_same_graph(Edge* edge);
            
        public:
            // *** Constructors *** //
            
            //! Constructor used by the different phases to build a new ExtensibleGraph.
            //! It creates an empty Extensible graph in the context of a specified scope and an identifier name.
            //! @param sl ScopeLink where the code is located.
            //! @param name Name which will identify the graph.
            ExtensibleGraph(ScopeLink sl, std::string name="");
            
            //! Copy constructor
            //! @param graph ExtensibleGraph from which the new graph is copied.
            ExtensibleGraph(const ExtensibleGraph& graph);
            
            
            // *** Modifiers *** //
            
            //! This method builds the Control Flow Graph of a piece of code represented by a statement given an empty graph.
            //! @param stmt Statement containing the code to be represented as a graph.
            void build_CFG(Statement stmt);
            
            //! This method is a wrapper of 'build_graph_from_statements' function to allow build graphs from a simple/compound statement.
            //! @param parent Node which is the parent of the first of the new nodes to be built.
            //! @param stmt Statement that contains the code to be parsed.
            //! @param graph_parent Node with type GRAPH_NODE which will contain the new node to be built.
            //!                     By default this value is NULL.
            //! @return The method returns a node:
            //!         - In the case of have been constructed a GRAPH_NODE, the whole node is returned.
            //!         - In the case of have been constructed a set of BASIC nodes, the last node of the chain is returned.
            Node* build_graph_from_statement(Node *parent, Statement stmt, Node* graph_parent = NULL);
            
            //! This method builds a node or a set of them containing the code represented by a list of consecutive statements.
            //! This node may contain several nodes inside.
            //! @param parent Node which is the parent of the first of the new nodes to be built.
            //! @param stmts List of statements containing the code to be represented as a node or a set of them.
            //! @param graph_parent Node with type GRAPH_NODE which will contain the new node to be built.
            //!                     By default this value is NULL.
            //! @return The method returns a node:
            //!         - In the case of have been constructed a GRAPH_NODE, the whole node is returned.
            //!         - In the case of have been constructed a set of BASIC nodes, the last node of the chain is returned.
            Node* build_graph_from_statements(Node *parent, ObjectList<Statement> stmts, Node* graph_parent = NULL);
            
            //! This method builds a node or a set of them containing the code represented by a set of statements.
            //! The set of statement may fulfil the condition of been sequential as statements.
            //! (It means not to be For, While, Do, If, Switch, Continue, Break, Goto, Try, Return, Labeled or Pragma statements).
            //! @param parent Node which is the parent of the first of the new nodes to be built.
            //! @param stmts List of statements containing the code to be represented as a node or a set of them.
            //! @return The method returns a node:
            //!         - In the case of have been constructed a GRAPH_NODE, the whole node is returned.
            //!         - In the case of have been constructed a set of BASIC nodes, the last node of the chain is returned.
            Node* build_node_from_sequential_statements(Node *parent, ObjectList<Statement> stmts, Node* graph_parent = NULL);
            
            //! This recursive method builds a node or a set of them containing the code represented by a expression.
            //! @param parent Node which is the parent of the first of the new nodes to be built.
            //! @param stmts List of statements containing the code to be represented as a node or a set of them.
            //! @return The method returns a node:
            //!         - In the case of have been constructed a GRAPH_NODE, the whole node is returned.
            //!         - In the case of have been constructed a set of BASIC nodes, the last node of the chain is returned.
            Node* build_node_from_expression(Node* parent, Expression expr, bool always_create_node, Node* graph_parent = NULL);
            
            Node* build_for_node(Node* parent, Statement for_stmt, Node* graph_node = NULL);
            Node* build_while_node(Node* parent, Statement while_stmt, Node* graph_node = NULL);
            Node* build_dowhile_node(Node* parent, Statement dowhile_stmt, Node* graph_node = NULL);
            Node* build_if_node(Node* parent, Statement if_stmt, Node* graph_node = NULL);
            Node* build_switch_node(Node* parent, Statement switch_stmt, Node* graph_node = NULL);
            Node* build_case_node(Node* condition_node, ObjectList<Node*>& previous_parents,
                                  ObjectList<Node*>& switch_exit_parents, int& actual_edge_index,
                                  ObjectList<Statement>::iterator& it, ObjectList<Statement>::iterator end, 
                                  Node* graph_node = NULL);
            Node* build_try_node(Node* parent, Statement try_block, Node* graph_node = NULL);
            Node* build_labeled_statement(Node* parent, Statement labeled_stmt, Node* graph_node = NULL);
            Node* build_goto_statement(Node* parent, Statement goto_stmt, Node* graph_node = NULL);
            Node* build_pragma_construct(Node* parent, Statement pragma_stmt, Node* outer_graph = NULL);
            Node* build_return_node(Node* parent, Statement return_stmt, Node* graph_node = NULL);
            
            Node* create_graph_node(Node* outer_graph);
            
            //! This method creates a new node and connects it to its parent node with a new edge
            //! @param parent Node which is parent of the new created node
            //! @param ntype Type of the node to be created
            //! @param etype Type of the new edge that connects the node parent with its new child
            //! @return The new node created
            Node* append_new_node_to_parent(Node* parent, ObjectList<AST_t> stmts,
                                            Node* parent_graph = NULL, Node_type ntype=BASIC_NORMAL_NODE,
                                            Edge_type etype=ALWAYS_EDGE);
            //! Wrapper method
            Node* append_new_node_to_parent(Node* parent, ObjectList<Statement> stmts, 
                                            Node* parent_graph = NULL, Node_type ntype=BASIC_NORMAL_NODE,
                                            Edge_type etype=ALWAYS_EDGE);
            
            
            void connect_nodes(ObjectList<Node*> parents, Node* child, Edge_type etype=ALWAYS_EDGE, std::string label = "");
            void connect_nodes(Node* parent, Node* child, Edge_type etype=ALWAYS_EDGE, std::string label = "");
            
            void disconnect_nodes(Node *parent, Node *child);
            
            
            //! The recursion is done down-top because this is the only way to found unreachable nodes.
            //! By the construction of the graph, we will never found paths following a top-down traversal
            //! which contain unreachable nodes
            void clear_unnecessary_nodes();
            
            void clear_orphaned_nodes(Node* actual_node);
            void clear_orphaned_nodes_in_subgraph(Node* actual_node);
            void clear_orphaned_cascade(Node* actual_node);
            
            //! This function clears the attribute 'visited' from nodes bellow 'actual' node
            //! It works properly if there isn't any unreachable node in the graph bellow 'actual' node
            void clear_visits(Node* actual);
            
            // DOT Graph
            
            //! Makes up the content of the nodes by deleting the line feeds and escaping double quotes
            void makeup_dot_block(std::string &str);
            
            //! Build a DOT file that represents the CFG
            void print_graph_to_dot();
            
            //! Print nodes and relations between them
            Node* get_nodes_dot_data(Node* actual_node, std::string& dot_graph, 
                                     std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes, 
                                     std::string indent, int& subgraph_id);
            
            //! Print both nodes and edges within a cfg subgraph
            void get_dot_subgraph(Node* actual_node, std::string& graph_data, 
                                  std::vector<std::string>& outer_edges, std::vector<Node*>& outer_nodes, 
                                  std::string indent, int& subgraph_id);
            
            void get_node_dot_data(Node* node, std::string& graph_data, std::string indent);
            
            
            
            

            
            
            
            
            
            
            
            
            
            
            
            
            

            
            //! This function is called recursively for building the CFG
            ExtensibleGraph* recursive_build_CFG(ObjectList<Statement> stmt, ObjectList<Node*> &parent, int parent_graph_id);
            
            //! This function builds a CFG from a Expression by visiting its nested expressions
            //! @return_always_CFG  points if the function must return a CFG.
            //!                     In the case the expression @expr is not a ConditionExpression or a FunctionCall
            //!                     the function will return NULL when @return_always_CFG = false
            //! @expression_nodes_type points the type of the nodes that will be built
            ExtensibleGraph* build_cfg_expression(Node* parent, Expression expr, bool return_always_CFG, bool belongs_to_subgraph, int parent_graph_id);
            
            //! This function builds a CFG from a set of Expressions which Statements aren't definitive by themselves
            //! The function searches for function calls and special operands like the question mark to built the basic blocs
            //! It also checks whether the expression has a label, the this expression is a potential destinations of a jump (goto)
            ExtensibleGraph* build_cfg_from_statements(Node* parent, ObjectList<Statement> stmts, int parent_graph_id);


//             ExtensibleGraph* build_pragma_construct(Node* parent, Statement pragma_stmt, int parent_graph_id);



            
            // Static CFG Analysis
            
            //! Computes the define-use chain of the cfg
            void live_variable_analysis();
            
            //! Computes the data-flow equation of each node in a iterative way until the information stops changing
            void solve_live_equations();
    };
}

#endif // CFG_HPP