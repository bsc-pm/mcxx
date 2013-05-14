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



#ifndef TL_STRUCTURES_HPP
#define TL_STRUCTURES_HPP

namespace TL {
namespace Analysis {

    //! Enumeration of the different node types
    // Modifications here require modifying Node::get_type_as_string
    enum Node_type {
        UNCLASSIFIED_NODE,
        // BASIC
        ASM_OP,              //! Node containing an ASM operand
        BREAK,               //! Node containing a BreakStatement
        CONTINUE,            //! Node containing a ContinueStatement
        ENTRY,               //! Entry point of a composite node
        EXIT,                //! Exit point of a composite node
        FUNCTION_CALL,       //! Node containing a Function Call
        GOTO,                //! Node containing a GotoStatement
        LABELED,             //! Node containing an only Labeled Statement
        NORMAL,              //! Node representing a Basic Bloc
        // OMP
        OMP_BARRIER,
        OMP_FLUSH,
        OMP_TASKWAIT,
        OMP_TASKYIELD,
        OMP_VIRTUAL_TASKSYNC,//! Node representing a task synchronization that occurs
                             //! when the function that creates the task has ended
        // COMPOSITE
        GRAPH                //! Composite node
    };

    // Modifications here require modifying Node::get_graph_type_as_string
    enum Graph_type {
        ASM_DEF,             //! Node containing an ASM definition
        COND_EXPR,           //! Conditional expression
        EXTENSIBLE_GRAPH,    //! Special node containing all nodes in a given graph
        FUNC_CALL,           //! Function Call
        IF_ELSE,             //! IfElse statement
        LOOP_DOWHILE,        //! Set of nodes of a for loop (but the initialization)
        LOOP_FOR,
        LOOP_WHILE,
        OMP_ATOMIC,
        OMP_CRITICAL,
        OMP_LOOP,
        OMP_MASTER,
        OMP_PARALLEL,
        OMP_SECTION,
        OMP_SECTIONS,
        OMP_SINGLE,
        OMP_TASK,
        SIMD,
        SIMD_FUNCTION,
        SPLIT_STMT,          //! Expression being split because it contains a sub-expression with a separated node
        SWITCH               //! Switch statement
    };

    //! Enumeration of the different edge types
    // Modifications here require modifying Edge::get_type_as_string
    enum Edge_type {
        UNCLASSIFIED_EDGE,
        ALWAYS,                 //! Always taken edge
        CASE,                   //! Edge within a Switch statement representing a case/default stmt
        CATCH,                  //! Handler edge for a Try/Catch statement
        FALSE_EDGE,             //! Taken when a previous condition is evaluated false
        GOTO_EDGE,              //! Edge between a GotoNode and a LabeledNode containing the label
        TRUE_EDGE               //! Taken when a previous condition is evaluated true
    };

    enum ASM_node_info {
        ASM_DEF_TEXT,
        ASM_DEF_INPUT_OPS,
        ASM_DEF_OUTPUT_OPS,
        ASM_DEF_CLOBBERED_REGS,
        ASM_OP_CONSTRAINT,
        ASM_OP_EXPRESSION
    };

    //! Definitions of the different node attributes
    /*! \def _NODE_TYPE
    * Type of a node. This will be a value of the enumeration Node_type.
    * Mandatory in all nodes.
    */
    #define _NODE_TYPE      "node_type"

    /*! \def _OUTER_NODE
    * Pointer to the node which contains a node.
    * Only the nodes that are within other nodes has this value set.
    * Mandatory in all nodes.
    */
    #define _OUTER_NODE    "outer_node"

    /*! \def _NODE_LABEL
    * String containing the label of a node.
    * It may have different meanings depending on the node type:
    *   - Graph / ASM_OP: is the Statement that builds the node.
    *   - Goto / Label: label that identifies the source or target of the Statement contained.
    * Mandatory and only available in 'Composite', 'Labeled' or 'Goto' nodes.
    */
    #define _NODE_LABEL     "node_label"

    /*! \def _NODE_STMTS
    * List of Statements within the Basic Bloc contained in a node.
    * Mandatory and only available in basic normal nodes.
    */
    #define _NODE_STMTS     "statements"

    /*!  \def _ENTRY_NODE
    * Node which is the entry point of a composite node.
    * All paths will cross this point when traversing the outer composite node.
    * Mandatory and only available in composite nodes.
    */
    #define _ENTRY_NODE     "entry"

    /*! \def _EXIT_NODE
    * Node which is the exit point of a composite node.
    * All paths will cross this point when traversing the outer composite node.
    * Mandatory and only available in composite nodes.
    */
    #define _EXIT_NODE      "exit"

    /*! \def _STRIDE_NODE
    * Node containing the loop stride statement within a loop graph node.
    * Mandatory and only available in Loop Graph nodes.
    */
    #define _STRIDE_NODE      "stride"

    /*! \def _GRAPH_TYPE
    * Type of the graph node. This will be a value of the enumeration Graph_type.
    * Mandatory in all graph nodes.
    */
    #define _GRAPH_TYPE     "graph_type"

    /*! \def _OMP_INFO
     * Data associated to an OmpSs pragma: pragma type and list of clauses withs its arguments
     * Mandatory in all OMP_PRAGMA graph nodes.
     */
    #define _OMP_INFO       "omp_info"

    /*! \def _ASM_INFO
     * Data indicating which info of the ASM function is contained in the ASM node
     * Mandatory in all ASM_DEF and ASM_OP nodes.
     */
    #define _ASM_INFO       "asm_info"

    /*!
    * Nodecl containing the context associated to a task
    * Mandatory and only available in composite nodes with _GRAPH_TYPE "task"
    */
    #define _TASK_CONTEXT   "task_context"

    /*!
    * Nodecl containing the symbol of the function contained contained in task
    * Mandatory and only available in composite nodes with _GRAPH_TYPE "task" of declaration level tasks
    */
    #define _TASK_FUNCTION  "task_function"


    // ************************************************************************************************* //
    // ************************************* Constant propagation ************************************** //

    /*! \def _LATTICE_VALS
        * Values in the Lattice Cell of all Objects modified in a given node
        * Available and mandatory in all simple nodes after constant propagation optimization
        */
    #define _LATTICE_VALS         "lattice_vals"

    /*! \def _IS_EXECUTABLE
        * Boolean value indicating whether an edge is executable or not
        * Available and mandatory in all edges after constant propagation optimization
        */
    #define _IS_EXECUTABLE      "is_executables"

    // *********************************** End constant propagation ************************************ //
    // ************************************************************************************************* //



    // ************************************************************************************************* //
    // ************************************ Use-definition analysis ************************************ //

    /*! \def _UPPER_EXPOSED
        * Set of upper exposed variables within a node.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
    #define _UPPER_EXPOSED  "ue_vars"

    /*! \def _KILLED
        * Set of killed variables within a node.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
    #define _KILLED         "killed_vars"

    /*! \def _UNDEF
        * Set of variables within a node that we cannot define the behaviour.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
    #define _UNDEF          "undefined_behaviour_vars"


    // ********************************** End use-definition analysis ********************************** //
    // ************************************************************************************************* //



    // ************************************************************************************************* //
    // *************************************** Liveness analysis *************************************** //

    /*! \def _LIVE_IN
    * Set of variables that are alive at the entry point of a node.
    * Available in all nodes (Mandatory once the Liveness analysis is performed).
    */
    #define _LIVE_IN        "live_in_vars"

    /*! \def _LIVE_OUT
    * Set of variables that are alive at the exit point of a node.
    * Available in all nodes (Mandatory once the Liveness analysis is performed).
    */
    #define _LIVE_OUT       "live_out_vars"

    // ************************************* End liveness analysis ************************************* //
    // ************************************************************************************************* //



    // ************************************************************************************************* //
    // ********************************* Reaching Definitions analysis ********************************* //

    /*! \def _GEN
     * Map containing the statements that generate a definition of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _GEN                 "gen_stmts"

    /*! \def _REACH_DEFS_IN
     * Map containing the reaching definitions at the entry of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _REACH_DEFS_IN       "reaching_defs_in"

    /*! \def _REACH_DEFS_OUT
     * Map containing the reaching definitions at the exit of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _REACH_DEFS_OUT      "reaching_defs_out"

    /*! \def _AUX_REACH_DEFS
     * Map containing the propagated reaching definitions in a given point
     * This variable is used while propagating the reaching definitions among the nodes to differentiate
     * those definitions performed within the node and those that has been propagated
     * At the end of the propagation, the reaching definitions stored in this value are copied in the _REACH_DEFS variable
     * an this is deleted
     * Available in all nodes (Mandatory once the Liveness analysis is performed).
     */
    #define _AUX_REACH_DEFS      "aux_reaching_defs"

    // ******************************* End reaching Definitions analysis ******************************* //
    // ************************************************************************************************* //



    // ************************************************************************************************* //
    // ****************************************** Loop analysis **************************************** //

    /*! \def _INDUCTION_VARS
     * Map containing the induction variables associated with a Loop Node
     * Available only in Loop (Graph) nodes (Mandatory once the Loop analysis is performed).
     */
    #define _INDUCTION_VARS     "induction_vars"

    // **************************************** End loop analysis ************************************** //
    // ************************************************************************************************* //



    // ************************************************************************************************* //
    // ****************************************** Auto-scoping ***************************************** //

    /*! \def _SC_AUTO
     * This value is set when a task has the clause default(AUTO)
     */
    #define _SC_AUTO                        "sc_auto"

    /*! \def _SHARED
    * Set of symbols with shared auto-scoping in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
    */
    #define _SC_SHARED                      "sc_shared"

    /*! \def _PRIVATE
    * Set of symbols with private auto-scoping in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
    */
    #define _SC_PRIVATE                     "sc_private"

    /*! \def _FIRSTPRIVATE
    * Set of symbols with lastprivate auto-scoping in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
    */
    #define _SC_FIRSTPRIVATE                "sc_firstprivate"

    /*! \def _SHARED_OR_FIRSTPRIVATE
     * Set of symbols with lastprivate auto-scoping in a task
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
     */
    #define _SC_SHARED_OR_FIRSTPRIVATE      "sc_shared_or_firstprivate"

    /*! \def _UNDEF_SC
    * Set of symbols with non-computable auto-scoping in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
    */
    #define _SC_UNDEF                       "sc_undef_scope"

    /*! \def _RACE
    * Set of symbols in a race situation in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
    */
    #define _SC_RACE                        "sc_race"

    // **************************************** End auto-scoping *************************************** //
    // ************************************************************************************************* //


    // ************************************************************************************************* //
    // *************************************** Auto-dependencies *************************************** //

    /*! \def _DEPS_SHARED
     * Set of symbols classified as PRIVATE during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_PRIVATE       "deps_private"

    /*! \def _DEPS_FIRSTPRIVATE
     * Set of symbols classified as FIRSTPRIVATE during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_FIRSTPRIVATE  "deps_firstprivate"

    /*! \def _DEPS_SHARED
     * Set of symbols classified as SHARED during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_SHARED        "deps_shared"

    /*! \def _IN_DEPS
    * Set of symbols with input dependence in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
    */
    #define _DEPS_IN            "deps_input"

    /*! \def _OUT_DEPS
    * Set of symbols with output dependence in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
    */
    #define _DEPS_OUT           "deps_output"

    /*! \def _INOUT_DEPS
    * Set of symbols with inout dependence in a task
    * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
    */
    #define _DEPS_INOUT         "deps_inout"

    /*! \def _UNDEF_DEPS
        * Set of symbols with which we are unable to compute the proper dependencies in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
    #define _DEPS_UNDEF         "deps_undef"

    // ************************************* End auto-dependencies ************************************* //
    // ************************************************************************************************* //





    /*! \def _CLAUSES
    * Set of clauses associated to a pragma
    * Available in Graph nodes of type 'omp_pragma' and 'task' but not mandat
    */
    #define _CLAUSES            "clauses"

    /*! \def _ARGS
    * Empty clause associated to a pragma
    * Available in Graph nodes of type 'omp_pragma' and 'task'.
    */
    #define _ARGS               "args"

    //! Definitions of the different edge attributes
    /*! \def _EDGE_TYPE
    * Type of the edge. This will be a value of the enumeration Edge_type.
    * Mandatory in all edges.
    */
    #define _EDGE_TYPE          "edge_type"

    /*! \def _EDGE_LABEL
    * String containing the label of an edge.
    * Available and mandatory in all edges but those with 'Always' type.
    */
    #define _EDGE_LABEL     "edge_label"

    /*! \def _IS_TASK_EDGE
    * Boolean indicating whether an edge connects a target being a Task
    * Available and mandatory in all edges.
    */
    #define _IS_TASK_EDGE       "is_task_edge"

    /*! \def _IS_EXECUTABLE_EDGE
        * Boolean indicating whether an edge is executable or not
        * Available and mandatory in all edges after constant propagation optimization.
        */
    #define _IS_EXECUTABLE_EDGE "is_executable_edge"
}
}

#endif          // TL_STRUCTURES_HPP