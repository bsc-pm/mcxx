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


#ifndef STRUCTURES_HPP
#define STRUCTURES_HPP

namespace TL
{
    //! Enumeration of the different node types
    enum Node_type {
        UNCLASSIFIED_NODE,
        // BASIC
        BASIC_ENTRY_NODE,               //! Entry point of a composite node
        BASIC_EXIT_NODE,                //! Exit point of a composite node
        BASIC_NORMAL_NODE,              //! Node representing a Basic Bloc
        BASIC_LABELED_NODE,             //! Node containing an only Labeled Statement
        BASIC_GOTO_NODE,                //! Node containing a GotoStatement
        BASIC_BREAK_NODE,               //! Node containing a BreakStatement
        BASIC_CONTINUE_NODE,            //! Node containing a ContinueStatement
        BASIC_FUNCTION_CALL_NODE,       //! Node containing a Function Call
        FLUSH_NODE,                     //! Node containing an OMP Flush directive
        BARRIER_NODE,                   //! Node containing an OMP Barrier directive
        BASIC_PRAGMA_DIRECTIVE_NODE,    //! Node containing an OMP Pragma directive (not permanent)
        // COMPOSITE
        GRAPH_NODE                      //! Composite node
    };
    
    //! Enumeration of the different edge types
    enum Edge_type
    {
        UNCLASSIFIED_EDGE,
        TRUE_EDGE,              //! Taken when a previous condition is evaluated true
        FALSE_EDGE,             //! Taken when a previous condition is evaluated false
        ALWAYS_EDGE,            //! Always taken edge
        CASE_EDGE,              //! Edge within a Switch statement representing a case/default stmt
        CATCH_EDGE              //! Handler edge for a Try/Catch statement
    };
    
    //! Definitions of the different node attributes
    /*! \def _NODE_TYPE
      Type of a node. This will be a value of the enumeration Node_type.
      Mandatory in all nodes.
     */
    #define _NODE_TYPE      "node_type"
    /*! \def _OUTER_NODE
      Pointer to the node which contains a node. 
      Only the nodes that are within other nodes has this value set.
      Available in all type of nodes.
     */
    #define _OUTER_NODE    "outer_node"
    /*! \def _NODE_LABEL
      String containing the label of a node.
      It may have different meanings depending on the node type:
        - Composite: is the Statement that defines the composition.
        - Goto / Label: label that identifies the source or target of the Statement contained.
      Available and mandatory in 'Composite', 'Labeled' or 'Goto' nodes.
     */    
    #define _NODE_LABEL     "node_label"
    /*! \def _LIVE_IN
      Set of variables that are alive at the entry point of a node.
      Available in all nodes (Mandatory once the Liveness analysis is performed).
     */   
    #define _LIVE_IN        "live_in_vars"
    /*! \def _LIVE_OUT
      Set of variables that are alive at the exit point of a node.
      Available in all nodes (Mandatory once the Liveness analysis is performed).
     */       
    #define _LIVE_OUT       "live_out_vars"
    /*! \def _UPPER_EXPOSED
      Set of upper exposed variables within a node.
      Available in all nodes (Mandatory once the Liveness analysis is performed).
     */ 
    #define _UPPER_EXPOSED  "ue_vars"
    /*! \def _KILLED
      Set of killed variables within a node.
      Available in all nodes (Mandatory once the Liveness analysis is performed).
     */ 
    #define _KILLED         "killed_vars"
    /*! \def _NODE_STMTS
      List of Statements within the Basic Bloc contained in a node.
      Available and mandatory in basic normal nodes.
     */
    #define _NODE_STMTS     "statements"
    /*!  \def _ENTRY_NODE
      Node which is the entry point of a composite node. 
      All paths will cross this point when traversing the outer composite node.
      Available and mandatory in composite nodes.
     */
    #define _ENTRY_NODE     "entry"
    /*! \def _EXIT_NODE
      Node which is the exit point of a composite node. 
      All paths will cross this point when traversing the outer composite node.
      Available and mandatory in composite nodes.
    */
    #define _EXIT_NODE      "exit"
    /*! \def _GRAPH_TYPE
      String containing the type of a composite node. The possible values are:
        - splitted_instruction
        - function_call
        - conditional_expression
        - omp_pragma
      Available only in composite nodes.
    */    
    #define _GRAPH_TYPE     "graph_type"
                                            
    //! Definitions of the different edge attributes
    /*! \def _EDGE_TYPE
      Type of the edge. This will be a value of the enumeration Edge_type.
      Mandatory in all edges.
     */
    #define _EDGE_TYPE      "edge_type"
    /*! \def _EDGE_LABEL
      String containing the label of an edge.
      Available and mandatory in all edges but those with 'Always' type.
     */
    #define _EDGE_LABEL     "edge_label"
}

#endif // STRUCTURES_HPP