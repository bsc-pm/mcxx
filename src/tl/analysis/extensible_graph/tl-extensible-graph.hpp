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
#include <stack>

#include "cxx-utils.h"
#include "tl-node.hpp"
#include "tl-nodecl.hpp"
#include "tl-fortran.hpp"
#include "tl-scopelink.hpp"
#include "tl-statement.hpp"

namespace TL
{
    class LIBTL_CLASS ExtensibleGraph
    {
        private:
            // *** Class attributes *** //
            Node* _entry;
            Node* _exit;
            ScopeLink _sl;
            std::string _name;
            int _nid;
            
            //! Stacks to keep the exit nodes of Loop Statements
            std::stack<Node*> _continue_stack;
            std::stack<Node*> _break_stack;
            
            //! Lists to keep special nodes that breaks the expected behaviour of the flow
            ObjectList<Node*> _unhand_try_excpt_list;
            ObjectList<Node*> _labeled_node_list;
            ObjectList<Node*> _goto_node_list;
            ObjectList<Node*> _throw_node_list;
            ObjectList<Node*> _tasks_node_list;
            
            //! Auxiliary values to know where we are
            bool _continue_stmt;
            bool _break_stmt;
            bool _goto_stmt;
            Node* _last_node;
            Node* _next_node;
            std::stack<Node*> _outer_graph;
            
            // *** Private methods *** //

            
            //! Builds a set of connected nodes that are contained between two iterators.
            //! These statements must be a set contained within a SwitchStatement.
            /*!
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
                            It will be the condition of a SwitchStatement.
              \param previous_parents List of other parents formed for all the exit statements of
                                      the immediate previous CaseStatements without a
                                      BreakStatement at the end.
              \param switch_exit_parents List of exits formed by the exit node of each
                                         CaseStatement with a BreakStatement at the end.
              \param actual_edge_index Counter used to label the edges between the condition of the
                                       SwitchStatement and each CaseStatement.
              \param it Iterator to the first statement to be parsed.
              \param end Iterator to the end of the list of statements to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
            */ 
            void build_case_node(Node* parent, ObjectList<Node*>& previous_parents,
                                 ObjectList<Node*>& switch_exit_parents, int& actual_edge_index,
                                 ObjectList<Statement>::iterator& it, 
                                 ObjectList<Statement>::iterator end, 
                                 Node* outer_graph = NULL);
            
            //! Set of methods that removes those nodes that can never be reached.                 
            void clear_orphaned_nodes(Node* actual_node);
            void clear_orphaned_nodes_in_subgraph(Node* actual_node);
            void clear_orphaned_cascade(Node* actual_node);
            
            //! Removes those nodes that has UNCLASSIFIED_NODE type and reconects parents and
            //! children nodes properly.
            void erase_unclassified_nodes(Node* actual);
            
            //! Joins all these statements that form a Basic Block.
            //! It assumes that there is no node with UNCLASSIFIED_NODE type in the graph
            //! otherwise, call before to method 'erase_unclassified_nodes'.
            void join_unhalted_statements(Node* actual, ObjectList<Node*> node_set);
            void join_node_set(ObjectList<Node*>& node_set);                     
                                
            //! Makes up the content of the nodes by deleting the line feeds and escaping all
            //! those symbols that can not be freely write in DOT language.
            void makeup_dot_block(std::string &str);
            
            //! Prints nodes and relations between them in a string in a recursive way.
            /*!
              \param actual_node Source node from which the printing is started.
              \param dot_graph Inout parameter where the DOT is printed.
              \param outer_edges Set of edges that must be printed in an outer DOT cluster.
              \param outer_nodes Set of nodes that must be printed in an outer DOT cluster.
              \param indent Indentation for the actual node when it is printed.
              \param subgraph_id Identifier for the actual cluster.
              \return Last node printed.
             */
            Node* get_nodes_dot_data(Node* actual_node, std::string& dot_graph, 
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
            
        public:
            // *** Constructors *** //
            
            //! Constructor used by the different phases to build a new ExtensibleGraph.
            /*! 
              \param sl ScopeLink context where the code is located.
              \param name Name which will identify the graph.
             */
            ExtensibleGraph(ScopeLink sl, std::string name);
            
            //! Copy constructor
            ExtensibleGraph(const ExtensibleGraph& graph);
            
           
            // *** Modifiers *** //
            
            //! This method creates a new node containing a Basic Block and connects it to its
            //! parent node with a new edge.
            /*!
              The method modifies the attribute #_last_node that will be the new node created.
              \param parent Parent of the new node.
              \param nodecls Set of NodeclBase containing the Statements of a Basic Bloc.
              \param ntype Type of the node to be created. 
                           By default is BASIC_NORMAL_NODE.
              \param etype Type of the new edge that connects the new node with @_last_node.
                           By default is ALWAYS_EDGE.
             */
            void append_new_node_to_parent(Node* parent,
                                           ObjectList<Nodecl::NodeclBase> child_nodecls,
                                           Node_type ntype = BASIC_NORMAL_NODE,
                                           Edge_type etype = ALWAYS_EDGE);
            
            //! Connects two nodes by creating a new edge between them.
            /*!
              \param parent Source node of the connection.             
              \param child Target node of the connection.
              \param etype Type of the connection between the two nodes.
              \param label Label for the connection. It will be used when a Catch or a Case edges
                           are built.
             */
            void connect_nodes(Node* parent, Node* child, Edge_type etype = ALWAYS_EDGE, 
                               std::string label = "");        
            
           
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            // *** Modifiers *** //
           
            //! Wrapper method for #build_CFG, when the code is contained in an only Statement
            void build_CFG(Statement stmt);
            
            //! Builds a Control Flow Graph of a piece of code contained in a list of statements
            void build_CFG(ObjectList<Statement> stmts);
            
            //! Wrapper for #build_graph_from_statements when only one statement.
            Node* build_graph_from_statement(Node *parent, Statement stmt, 
                                             Node* outer_graph = NULL);
            
            //! Builds a node or a set of them containing a set of statements
            /*!
              \param parent Parent of the first of the new nodes to be built.
              \param stmt Set of statements to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Depending on the type of the node/s built:  
                        - if composite node, the whole node is returned.
                        - otherwise, an empty node which is the exit of the chain.
            */
            Node* build_graph_from_statements(Node *parent, ObjectList<Statement> stmts, 
                                              Node* outer_graph = NULL);
            
            //! Builds a node or a set of them containing a set of statements.
            /*!
              The statements must not be break the the flow (as statements). That means that the
              set must not contain statements of the type: ForStatement, WhileStatement,
              IfStatement, DoWhileStatement, SwitchStatement, BreakStatement, ContinueStatement,
              TryStatement, ReturnStatement, GotoStatement or LabeledStatement.
              \param parent Parent of the first of the new nodes to be built.
              \param stmts Set of statements to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Depending on the type of the node/s built:  
                        - if composite node, the whole node is returned.
                        - otherwise, an empty node which is the exit of the chain.
             */
            Node* build_node_from_sequential_statements(Node *parent, 
                                                        ObjectList<Statement> stmts,
                                                        Node* outer_graph = NULL);
            
            //! Builds a node or a set of them containing a expression.
            /*!
              The method splits the expression until it founds a literal or an id_expression.
              It tries to build the minimum number of nodes as possible. When it can not be sure of
              the atomicity of the expression, then it builds different nodes and concatenates them
              within a composite node. It happens when:
                - Function call.
                - Conditional expression.
                - Some of the previous cases occurs within other expression.
              \param parent Parent of the first of the new nodes to be built.
              \param expr Expression to be parsed.
              \param always_create_node true when the expression must build always a node, 
                                        even if the instruction represented does not split the flow.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Depending on the type of the node/s built:  
                        - if composite node, the whole node is returned.
                        - otherwise, an empty node which is the exit of the chain.
             */
            Node* build_node_from_expression(Node* parent, Expression expr, 
                                             bool always_create_node, Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains a ForStatement.
            /*!
              \param parent Parent of the first of the new nodes to be built.
              \param for_stmt ForStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
             */
            Node* build_for_node(Node* parent, Statement for_stmt, Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains a WhileStatement.
            /*!
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
              \param while_stmt WhileStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
             */            
            Node* build_while_node(Node* parent, Statement while_stmt, Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains a DoWhileStatement.
            /*!
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
              \param dowhile_stmt DoWhileStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
            */               
            Node* build_dowhile_node(Node* parent, Statement dowhile_stmt, 
                                     Node* outer_graph = NULL);
                                     
            //! Builds a set of connected nodes that contains a IfStatement.
            /*!
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
              \param if_stmt IfStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
            */   
            Node* build_if_node(Node* parent, Statement if_stmt, Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains a SwitchStatement.
            /*!
              Since the inner statements in a SwitchStatement are a Compound Statement where the
              the CaseStatements are only linked with the immediatly next statement, we must parse
              each statement remembering what happened in the past. 
              For that reason a CaseStatement can not be built regardless of the rest of statements
              within the SwitchStatement.
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
              \param switch_stmt SwitchStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
            */ 
            Node* build_switch_node(Node* parent, Statement switch_stmt, Node* outer_graph = NULL);

            //! Builds a set of connected nodes that contains a TryStatement.
            /*!
              The exit of the set of nodes is an empty node with type UNCLASSIFIED_NODE. All this
              kind of nodes are removed by calling the function #clear_unnecessary_nodes.
              \param parent Parent of the first of the new nodes to be built.
              \param try_block TryStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Empty node with type UNCLASSIFIED_NODE.
                      All this kind of nodes will be removed by calling #clear_unnecessary_nodes.
            */ 
            Node* build_try_node(Node* parent, Statement try_block, Node* outer_graph = NULL);
            
            //! Builds a node that contains a LabeledStatement.
            /*!
              The new node is stored in a buffer where, when a GotoStatement arrives, will search
              for any connection.
              Also, the method searchs in a buffer where the targets of the GotoStatements arrived
              until the moment are stored for any matching.
              \param parent Parent of the first of the new nodes to be built.
              \param labeled_stmt LabeledStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Node containing the labeled statement.
            */ 
            Node* build_labeled_statement(Node* parent, Statement labeled_stmt, 
                                          Node* outer_graph = NULL);
            
            //! Builds a node that contains a GotoStatement.
            /*!
              Since the target of a Goto Statement will potentially not be in the following
              statement, the Goto node is stored in a buffer where, when a Labeled Statement
              arrives, the connexion will be searched.
              \param parent Parent of the first of the new nodes to be built.
              \param goto_stmt GotoStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return NULL pointer.
            */ 
            Node* build_goto_statement(Node* parent, Statement goto_stmt, Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains a Pragma Construct Statement
            /*!
              When a non-omp pragma is founded, then the method don't do anything.
              \param parent Parent of the first of the new nodes to be built.
              \param pragma_stmt Pragma Construct Statement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Composite node containing the set of pragma statements nodes.
            */             
            Node* build_pragma_construct(Node* parent, Statement pragma_stmt, 
                                         Node* outer_graph = NULL);
            
            //! Builds a set of connected nodes that contains an omp sections statement
            /*!
              \param parent Parent of the first of the new nodes to be built.
              \param pragma_stmt Omp Sections Directive to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Composite node containing the set of statements of the directive.
            */                       
            Node* build_sections_node(Node* parent, Statement pragma_stmt, Node* outer_graph);
            
            //! Builds a node that contains a ReturnStatement.
            /*!
              \param parent Parent of the first of the new nodes to be built.
              \param return_stmt ReturnStatement to be parsed.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Node containing the ReturnStatement.
            */ 
            Node* build_return_node(Node* parent, Statement return_stmt, Node* outer_graph = NULL);
            
            
            //! Builds a composite node with an entry and an exit node.
            /*!
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \param label AST containing the Statement represented with the new node. It will be
                           the label of the node.
              \param graph_type Type of the composite node. 
                                It must be some of these values: 'splitted_instruction',
                                'function_call', 'conditional_expression', 'omp_pragma'.
              \return The new composite node.
             */
            Node* create_graph_node(Node* outer_graph, AST_t label, std::string graph_type);
            
            //! Builds a barrier node with its flush nodes.
            /*!
              Since a Flush is implied during a Barrier region, a Flush Node is added before and
              after a Barrier node.
              \param parents Set of parents of the new barrier node.
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \return Flush node created after the barrier node.
             */
            Node* create_barrier_node(ObjectList<Node*> parents, Node* outer_graph);
           
            //! Wrapper method for #append_new_node_to_parent when the set of statements is
            //! represented as a set of AST_t's
            Node* append_new_node_to_parent(Node* parent, ObjectList<AST_t> stmts, 
                                            Node* parent_graph = NULL, 
                                            Node_type ntype=BASIC_NORMAL_NODE,
                                            Edge_type etype=ALWAYS_EDGE);
            
            //! This method creates a new node containing a Basic Block and connects it to its
            //! parent node with a new edge.
            /*!
              \param parent Node which is parent of the new created node
              \param stmts Set of Statements that forms a Basic Bloc
              \param outer_graph Node to which the new structure will belong to.
                                 It must be a Composite node.
              \param ntype Type of the node to be created.
              \param etype Type of the new edge that connects the node parent with its new child
              \return The new node created.
             */
            Node* append_new_node_to_parent(Node* parent, ObjectList<Statement> stmts,
                                            Node* outer_graph = NULL, 
                                            Node_type ntype = BASIC_NORMAL_NODE,
                                            Edge_type etype = ALWAYS_EDGE);
            
            //! Wrapper method for #connect_nodes when a set of parents must be connected to an
            //! only child and the nature of the connection is the same for all of them.
            void connect_nodes(ObjectList<Node*> parents, Node* child, Edge_type etype=ALWAYS_EDGE,
                               std::string label = "");
                               
            //! Wrapper method for #connect_nodes when a parent must be connected to a set of
            //! children and each connection may be different from the others. 
            //! A set of edge types and labels must be provided.
            void connect_nodes(Node* parent, ObjectList<Node*> children, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> labels);
            
            //! Wrapper method for #connect_nodes when a set of parents must be connected to a
            //! child and each connection may be different from the others. 
            //! A set of edge types and labels must be provided.
            void connect_nodes(ObjectList<Node*> parents, Node* children, 
                                ObjectList<Edge_type> etypes, ObjectList<std::string> labels);
                                
            //! Wrapper method for #connect_nodes when a set of parents must be connected to a
            //! set of children and each connection may be different from the others. 
            //! A set of edge types and labels must be provided. It is assumed that each parent is
            //! connected to all its children with the same type of edge.
            void connect_nodes(ObjectList<Node*> parents, ObjectList<Node*> children, 
                               ObjectList<Edge_type> etypes, ObjectList<std::string> labels);
            
            
            // OLD method
            //! Connects two nodes by creating a new edge between them.
            /*!
              \param parent Source node of the connection. 
              \param child Target node of the connection.
              \param etype Type of the connection between the two nodes.
              \param label Label for the connection. It will be used when a Catch or a Case edges
                           are built.
             */
//             void connect_nodes(Node* parent, Node* child, Edge_type etype = ALWAYS_EDGE, 
//                                std::string label = "");
            
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
            
            
            //! This method removes all those nodes that are unreachable and those that were created
            //! as auxiliary nodes when the graph was created. 
            //! It also joins those nodes that are always consecutively executed and non of them
            //! are the target of a jump.
            void clear_unnecessary_nodes();
            
            //! This function clears the attribute #visited from nodes bellow @actual node.
            //! It works properly if there isn't any unreachable node in the graph bellow @actual.
            void clear_visits(Node* actual);
            
            
            // *** Utils *** //
            
            //! Returns true when some of the following statements: ForStatement, WhileStatement,
            //! IfStatement, DoWhileStatement, SwitchStatement, BreakStatement, ContinueStatement,
            //! TryStatement, ReturnStatement, GotoStatement or LabeledStatement.
            bool statement_breaks_flow(Statement stmt);
            
            
            // *** DOT Graph *** //
            
            //! Build a DOT file that represents the CFG
            void print_graph_to_dot();

            
            // *** Static CFG Analysis *** //
            
            //! Computes the define-use chain of the cfg
            void live_variable_analysis();
            
        friend class CfgVisitor;
    };
}

#endif // EXTENSIBLE_GRAPH_HPP