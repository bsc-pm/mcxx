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

#ifndef TL_PCFGVISIT_UTILS_HPP
#define TL_PCFGVISIT_UTILS_HPP

#include "tl-analysis-utils.hpp"
#include "tl-nodecl-utils.hpp"
#include "tl-objectlist.hpp"
#include <set>
#include <stack>

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // **************************** PCFG enumerations and defines *************************** //
    
    /*! Enumeration of the different node types
     *  This nodes can be grouped in three categories: 
     *  - Basic nodes: ASM_OP, BREAK, CONTINUE, ENTRY, EXIT, FUNCTION_CALL, GOTO, LABELED, NORMAL
     *  - OpenMP nodes: OMP_XX
     *  - Composite nodes: GRAPH
     */
    #define NODE_TYPE_LIST \
    NODE_TYPE(UnclassifiedNode) \
    NODE_TYPE(AsmOp) \
    NODE_TYPE(Break) \
    NODE_TYPE(Builtin) \
    NODE_TYPE(Continue) \
    NODE_TYPE(Entry) \
    NODE_TYPE(Exit) \
    NODE_TYPE(FunctionCall) \
    NODE_TYPE(Goto) \
    NODE_TYPE(Labeled) \
    NODE_TYPE(Normal) \
    NODE_TYPE(OmpBarrier) \
    NODE_TYPE(OmpFlush) \
    NODE_TYPE(OmpTaskCreation) \
    NODE_TYPE(OmpTaskwait) \
    NODE_TYPE(OmpTaskyield) \
    NODE_TYPE(OmpVirtualTaskSync) \
    NODE_TYPE(OmpWaitonDeps) \
    NODE_TYPE(Return) \
    NODE_TYPE(VectorFunctionCall) \
    NODE_TYPE(VectorGather) \
    NODE_TYPE(VectorLoad) \
    NODE_TYPE(VectorNormal) \
    NODE_TYPE(VectorReduction) \
    NODE_TYPE(VectorScatter) \
    NODE_TYPE(VectorStore) \
    NODE_TYPE(Graph)
    
    enum NodeType {
        #undef NODE_TYPE
        #define NODE_TYPE(X) __##X,
        NODE_TYPE_LIST
        #undef NODE_TYPE
    };
    
    /*!Enum containing the possible types of Graph nodes. Some special cases are:
     * EXTENSIBLE_GRAPH Graph containing all nodes in an ExtensibleGraph. Its outer node is NULL
     * SPLIT_STMT Expression being split because it contains a sub-expression with a separated node
     */
    #define GRAPH_NODE_TYPE_LIST \
    GRAPH_TYPE(AsmDef) \
    GRAPH_TYPE(CondExpr) \
    GRAPH_TYPE(Context) \
    GRAPH_TYPE(ExtensibleGraph) \
    GRAPH_TYPE(FunctionCallGraph) \
    GRAPH_TYPE(FunctionCode) \
    GRAPH_TYPE(IfElse) \
    GRAPH_TYPE(LoopDoWhile) \
    GRAPH_TYPE(LoopFor) \
    GRAPH_TYPE(LoopWhile) \
    GRAPH_TYPE(OmpAsyncTarget) \
    GRAPH_TYPE(OmpAtomic) \
    GRAPH_TYPE(OmpBarrierGraph) \
    GRAPH_TYPE(OmpCritical) \
    GRAPH_TYPE(OmpForAppendix) \
    GRAPH_TYPE(OmpLoop) \
    GRAPH_TYPE(OmpMaster) \
    GRAPH_TYPE(OmpParallel) \
    GRAPH_TYPE(OmpSection) \
    GRAPH_TYPE(OmpSections) \
    GRAPH_TYPE(OmpSimd) \
    GRAPH_TYPE(OmpSimdFor) \
    GRAPH_TYPE(OmpSimdFunction) \
    GRAPH_TYPE(OmpSimdParallelFor) \
    GRAPH_TYPE(OmpSingle) \
    GRAPH_TYPE(OmpSyncTarget) \
    GRAPH_TYPE(OmpWorkshare) \
    GRAPH_TYPE(OmpTask) \
    GRAPH_TYPE(SplitStmt) \
    GRAPH_TYPE(Switch) \
    GRAPH_TYPE(SwitchCase) \
    GRAPH_TYPE(VectorCondExpr) \
    GRAPH_TYPE(VectorFunctionCallGraph)
    
    enum GraphType {
        #undef GRAPH_TYPE
        #define GRAPH_TYPE(X) __##X,
        GRAPH_NODE_TYPE_LIST
        #undef GRAPH_TYPE
    };
    
    //! Enumeration of the different edge types
    #define EDGE_TYPE_LIST \
    EDGE_TYPE(UnclassifiedEdge) \
    EDGE_TYPE(Always) \
    EDGE_TYPE(Case) \
    EDGE_TYPE(Catch) \
    EDGE_TYPE(FalseEdge) \
    EDGE_TYPE(GotoEdge) \
    EDGE_TYPE(TrueEdge)
    
    enum EdgeType {
        #undef EDGE_TYPE
        #define EDGE_TYPE(X) __##X,
        EDGE_TYPE_LIST
        #undef EDGE_TYPE
    };

    #define SYNC_KIND_LIST \
    SYNC_KIND(Static) \
    SYNC_KIND(Maybe) \
    SYNC_KIND(Post)

    enum SyncKind {
        #undef SYNC_KIND
        #define SYNC_KIND(X) __##X,
        SYNC_KIND_LIST
        #undef SYNC_KIND
    };
    
    enum ASM_node_info {
        ASM_DEF_TEXT,
        ASM_DEF_INPUT_OPS,
        ASM_DEF_OUTPUT_OPS,
        ASM_DEF_CLOBBERED_REGS,
        ASM_OP_CONSTRAINT,
        ASM_OP_EXPRESSION
    };
    
    // Node attributes
    //////////////////
    
    enum PCFGAttribute {
        /*! \def _NODE_LABEL
        * String containing the label of a node.
        * It may have different meanings depending on the node type:
        *   - Graph / ASM_OP: is the Statement that builds the node.
        *   - Goto / Label: label that identifies the source or target of the Statement contained.
        * Mandatory and only available in 'Composite', 'Labeled' or 'Goto' nodes.
        */
        _NODE_LABEL,

        /*! \def _NODE_STMTS
        * List of Statements within the Basic Block contained in a node.
        * Mandatory and only available in basic normal nodes.
        */
        _NODE_STMTS,

        /*!  \def _ENTRY_NODE
        * Node which is the entry point of a composite node.
        * Mandatory and only available in composite nodes.
        */
        _ENTRY_NODE,

        /*! \def _EXIT_NODE
        * Node which is the exit point of a composite node.
        * Mandatory and only available in composite nodes.
        */
        _EXIT_NODE,

        /*! \def _CONDITION_NODE
        * Node containing the loop condition statement within a loop graph node.
        * Mandatory and only available in Loop Graph nodes.
        */
        _CONDITION_NODE,

        /*! \def _STRIDE_NODE
        * Node containing the loop stride statement within a loop graph node.
        * Mandatory and only available in Loop Graph nodes.
        */
        _STRIDE_NODE,

        /*! \def _GRAPH_TYPE
        * Type of the graph node. This will be a value of the enumeration GraphType.
        * Mandatory in all graph nodes.
        */
        _GRAPH_TYPE,

        /*! \def _OMP_INFO
        * Data associated to an OmpSs pragma: pragma type and list of clauses withs its arguments
        * Mandatory in all OMP_PRAGMA graph nodes.
        */
        _OMP_INFO,

        /*! \def _ASM_INFO
        * Data indicating which info of the ASM function is contained in the ASM node
        * Mandatory in all ASM_DEF and ASM_OP nodes.
        */
        _ASM_INFO,

        /*! \def _TASK_CONTEXT
        * Nodecl containing the context associated to a task
        * Mandatory and only available in composite nodes with _GRAPH_TYPE "task"
        */
        _TASK_CONTEXT,

        /*! \def _TASK_FUNCTION
        * Nodecl containing the symbol of the function contained contained in task
        * Mandatory and only available in composite nodes with _GRAPH_TYPE "task" of declaration level tasks
        */
        _TASK_FUNCTION,

        /*! \def _CLAUSES
        * Set of clauses associated to a pragma
        * Available in Graph nodes of type 'omp_pragma' and 'task' but not mandat
        */
        _CLAUSES,

        /*! \def _ARGS
        * Empty clause associated to a pragma
        * Available in Graph nodes of type 'omp_pragma' and 'task'.
        */
        _ARGS,


        // Edge attributes
        //////////////////

        //! Definitions of the different edge attributes
        /*! \def _EDGE_TYPE
        * Type of the edge. This will be a value of the enumeration EdgeType.
        * Mandatory in all edges.
        */
        _EDGE_TYPE,

        /*! \def _EDGE_LABEL
        * String containing the label of an edge.
        * Available and mandatory in all edges but those with 'Always' type.
        */
        _EDGE_LABEL,

        /*! \def _SYNC_KIND
        * SyncKind containing the type of synchronization the edge represents
        * Available and mandatory in all synchronization edges.
        */
        _SYNC_KIND,

        /*! \def _CONDITION
        * Nodecl containing the condition that must fulfill to have a real dependency 
        * between the two tasks connected with the edge
        */
        _CONDITION,

        /*! \def _IS_TASK_EDGE
        * Boolean indicating whether an edge connects a target being a Task
        * Available and mandatory in all edges.
        */
        _IS_TASK_EDGE,

        /*! \def _IS_BACK_EDGE
        * Boolean indicating whether an edge connects a target which appears before in the source code
        * Available and mandatory in all edges.
        */
        _IS_BACK_EDGE,


        // Constant propagation attributes
        //////////////////////////////////

        /*! \def _LATTICE_VALS
        * Values in the Lattice Cell of all Objects modified in a given node
        * Available and mandatory in all simple nodes after constant propagation optimization
        */
        _LATTICE_VALS,

        /*! \def _IS_EXECUTABLE
        * Boolean value indicating whether an edge is executable or not
        * Available and mandatory in all edges after constant propagation optimization
        */
        _IS_EXECUTABLE,

        /*! \def _IS_EXECUTABLE_EDGE
        * Boolean indicating whether an edge is executable or not
        * Available and mandatory in all edges after constant propagation optimization.
        */
        _IS_EXECUTABLE_EDGE,


        // PCFG attributes
        //////////////////

        /*! \def _LIVE_IN_TASKS
        * Set of tasks that are alive at the Entry of a node
        * Available in all nodes during PCFG construction
        * FIXME Think about deleting this data after PCFG construction
        */
        _LIVE_IN_TASKS,

        /*! \def _LIVE_OUT_TASKS
        * Set of tasks that are alive at the Exit of a node
        * Available in all nodes during PCFG construction
        * FIXME Think about deleting this data after PCFG construction
        */
        _LIVE_OUT_TASKS,


        // UseDef attributes
        ////////////////////

        /*! \def _UPPER_EXPOSED
        * Set of upper exposed variables within a node.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _UPPER_EXPOSED,

        /*! \def _KILLED
        * Set of killed variables within a node.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _KILLED,

        /*! \def _UNDEF
        * Set of variables within a node that we cannot define the behaviour.
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _UNDEF,

        /*! \def _PRIVATE_UPPER_EXPOSED
        * Set of private upper exposed variables within a node (after purging based on OpenMP variables scope).
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _PRIVATE_UPPER_EXPOSED,

        /*! \def _PRIVATE_KILLED
        * Set of killed variables within a node (after purging based on OpenMP variables scope).
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _PRIVATE_KILLED,

        /*! \def _PRIVATE_UNDEF
        * Set of variables within a node that we cannot define the behaviour (after purging based on OpenMP variables scope).
        * Available in all nodes (Mandatory once the UseDef analysis is performed).
        */
        _PRIVATE_UNDEF,

        /*! \def _USED_ADDRESSES
        * Set of addresses being used within a node
        * Available in all nodes (Mandatory once the UseDef analysis is performed)
        */
        _USED_ADDRESSES,


        // Liveness attributes
        //////////////////////

        /*! \def _LIVE_IN
        * Set of variables that are alive at the entry point of a node.
        * Available in all nodes (Mandatory once the Liveness analysis is performed).
        */
        _LIVE_IN,

        /*! \def _LIVE_OUT
        * Set of variables that are alive at the exit point of a node.
        * Available in all nodes (Mandatory once the Liveness analysis is performed).
        */
        _LIVE_OUT,


        // Reaching definitions attributes
        //////////////////////////////////

        /*! \def _GEN
        * Map containing the statements that generate a definition of a given node
        * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
        */
        _GEN,

        /*! \def _REACH_DEFS_IN
        * Map containing the reaching definitions at the entry of a given node
        * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
        */
        _REACH_DEFS_IN,

        /*! \def _REACH_DEFS_OUT
        * Map containing the reaching definitions at the exit of a given node
        * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
        */
        _REACH_DEFS_OUT,

        /*! \def _AUX_REACH_DEFS
        * Map containing the propagated reaching definitions in a given point
        * This variable is used while propagating the reaching definitions among the nodes to differentiate
        * those definitions performed within the node and those that has been propagated
        * At the end of the propagation, the reaching definitions stored in this value are copied in the _REACH_DEFS variable
        * an this is deleted
        * Available in all nodes (Mandatory once the Liveness analysis is performed).
        */
        _AUX_REACH_DEFS,


        // Loop analysis attributes
        ///////////////////////////
        
        /*! \def _INDUCTION_VARS
        * Map containing the induction variables associated with a Loop Node
        * Available only in Loop (Graph) nodes (Mandatory once the Loop analysis is performed).
        */
        _INDUCTION_VARS,


        // Range Analysis
        /////////////////

        /*! \def _RANGES
        * Map containing range of values assigned to a value in a given node
        * Available in all nodes (Mandatory after Range Analysis is performed).
        */
        _RANGES,


        // Auto-scoping attributes
        //////////////////////////

        /*! \def _SC_AUTO
        * This value is set when a task has the clause default(AUTO)
        */
        _SC_AUTO,

        /*! \def _SHARED
        * Set of symbols with shared auto-scoping in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
        */
        _SC_SHARED,

        /*! \def _PRIVATE
        * Set of symbols with private auto-scoping in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
        */
        _SC_PRIVATE,

        /*! \def _FIRSTPRIVATE
        * Set of symbols with lastprivate auto-scoping in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
        */
        _SC_FIRSTPRIVATE,

        /*! \def _UNDEF_SC
        * Set of symbols with non-computable auto-scoping in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
        */
        _SC_UNDEF,

        /*! \def _RACE
        * Set of symbols in a race situation in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once Auto-scoping is performed).
        */
        _SC_RACE,


        // Auto-deps attributes
        ///////////////////////

        /*! \def _DEPS_SHARED
        * Set of symbols classified as PRIVATE during the auto-deps routine
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_PRIVATE,

        /*! \def _DEPS_FIRSTPRIVATE
        * Set of symbols classified as FIRSTPRIVATE during the auto-deps routine
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_FIRSTPRIVATE,

        /*! \def _DEPS_SHARED
        * Set of symbols classified as SHARED during the auto-deps routine
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_SHARED,

        /*! \def _IN_DEPS
        * Set of symbols with input dependence in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_IN,

        /*! \def _OUT_DEPS
        * Set of symbols with output dependence in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_OUT,

        /*! \def _INOUT_DEPS
        * Set of symbols with inout dependence in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_INOUT,

        /*! \def _UNDEF_DEPS
        * Set of symbols with which we are unable to compute the proper dependencies in a task
        * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
        */
        _DEPS_UNDEF,


        // Correctness analysis attributes
        //////////////////////////////////

        /*! \def _CORRECTNESS_AUTO_STORAGE_VARS
        * Set of variables with automatic storage which and used in a task that is not synchronized before the variables are deallocated
        * Available only in task nodes after OMP/OMPSS correctness phase
        */
        _CORRECTNESS_AUTO_STORAGE_VARS,

        /*! \def _CORRECTNESS_DEAD_VARS
        * Set of variables detected as dead variables during OMP/OMPSS correctness phase
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_DEAD_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_FP_VARS
        * Set of variables incoherently scoped as Firstprivate: the first use of the variable within the task is a definition
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_INCOHERENT_FP_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_IN_VARS
        * Set of variables defined as an input dependence of a task that does not read the value of the variable
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_INCOHERENT_IN_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_IN_POINTED_VARS
        * Set of variables defined as an output dependence of a task that actually reads the value pointed by the variable or some sub-object of the variable
        */
        _CORRECTNESS_INCOHERENT_IN_POINTED_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_OUT_VARS
        * Set of variables defined as an output dependence of a task that does not write the value of the variable
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_INCOHERENT_OUT_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_OUT_POINTED_VARS
        * Set of variables defined as an output dependence of a task that actually writes the value pointed by the variable or some sub-object of the variable
        */
        _CORRECTNESS_INCOHERENT_OUT_POINTED_VARS,

        /*! \def _CORRECTNESS_INCOHERENT_P_VARS
        * Set of variables incoherently scoped as Private: the first use of the variable within the task is read
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_INCOHERENT_P_VARS,

        /*! \def _CORRECTNESS_RACE_VARS
        * Set of variables that are in a race condition in a given task
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_RACE_VARS,

        /*! \def _CORRECTNESS_UNNECESSARILY_SCOPED_VARS
        * Set of variables that are unnecessarily scoped in a given task (they do not appear at all in the task)
        * Available only in task nodes OMP/OMPSS correctness phase
        */
        _CORRECTNESS_UNNECESSARILY_SCOPED_VARS,


        // Analysis checking attributes
        ///////////////////////////////

        /*! \def _ASSERT_UPPER_EXPOSED
        * Set of expressions marked by the user as upper exposed in a given point of the program
        */
        _ASSERT_UPPER_EXPOSED,

        /*! \def _ASSERT_KILLED
        * Set of expressions marked by the user as defined in a given point of the program
        */
        _ASSERT_KILLED,

        /*! \def _ASSERT_UNDEFINED
        * Set of expressions marked by the user as undefined behavior in a given point of the program
        */
        _ASSERT_UNDEFINED,

        /*! \def _ASSERT_LIVE_IN
        * Set of expressions marked by the user as live in in a given point of the program
        */
        _ASSERT_LIVE_IN,

        /*! \def _ASSERT_LIVE_OUT
        * Set of expressions marked by the user as live_out in a given point of the program
        */
        _ASSERT_LIVE_OUT,

        /*! \def _ASSERT_DEAD
        * Set of expressions marked by the user as dead in a given point of the program
        */
        _ASSERT_DEAD,

        /*! \def _ASSERT_REACH_DEFS_IN
        * Set of reaching definitions at the entry of a given point of the program
        */
        _ASSERT_REACH_DEFS_IN,

        /*! \def _ASSERT_REACH_DEFS_OUT
        * Set of reaching definitions at the exit of a given point of the program
        */
        _ASSERT_REACH_DEFS_OUT,

        /*! \def _ASSERT_INDUCTION_VARS
        * Set of induction variables in a given point of the program
        */
        _ASSERT_INDUCTION_VARS,

        /*! \def _ASSERT_AUTOSC_FIRSTPRIVATE
        * Set of variables autoscoped as firstprivate in a given point of the program
        */
        _ASSERT_AUTOSC_FIRSTPRIVATE,

        /*! \def _ASSERT_AUTOSC_PRIVATE
        * Set of variables autoscoped as private in a given point of the program
        */
        _ASSERT_AUTOSC_PRIVATE,

        /*! \def _ASSERT_AUTOSC_SHARED
        * Set of variables autoscoped as shared in a given point of the program
        */
        _ASSERT_AUTOSC_SHARED,

        /*! \def _ASSERT_RANGE
         * Set of variables associated with their corresponding ranges
         */
        _ASSERT_RANGE,

        /*! \def _ASSERT_CORRECTNESS_AUTO_STORAGE_VARS
        * Set of variables with automatic storage which are used in a task that is not synchronized before the variables are deallocated
        */
        _ASSERT_CORRECTNESS_AUTO_STORAGE_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_FP_VARS
        * Set of variables incoherently scoped as Firstprivate: the first use of the variable within the task is a definition
        */
        _ASSERT_CORRECTNESS_INCOHERENT_FP_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_IN_VARS
        * Set of variables defined as an input dependence of a task that does not read the value of the variable
        */
        _ASSERT_CORRECTNESS_INCOHERENT_IN_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS
        * Set of variables defined as an input dependence of a task that actually reads the value pointed by the variable or some sub-object of the variable
        */
        _ASSERT_CORRECTNESS_INCOHERENT_IN_POINTED_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS
        * Set of variables defined as an output dependence of a task that does not write the value of the variable
        */
        _ASSERT_CORRECTNESS_INCOHERENT_OUT_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS
        * Set of variables defined as an output dependence of a task that actually writes the value pointed by the variable or some sub-object of the variable
        */
        _ASSERT_CORRECTNESS_INCOHERENT_OUT_POINTED_VARS,

        /*! \def _ASSERT_CORRECTNESS_INCOHERENT_P_VARS
        * Set of variables incoherently scoped as Private: the first use of the variable within the task is read
        */
        _ASSERT_CORRECTNESS_INCOHERENT_P_VARS,

        /*! \def _ASSERT_CORRECTNESS_RACE_VARS
        * Set of variables that are in a race condition in a given task
        */
        _ASSERT_CORRECTNESS_RACE_VARS,

        /*! \def _ASSERT_CORRECTNESS_DEAD_VARS
        * Set of variables modified within a task which values are not used within the task (instead, the value is used after the task synchronizes)
        */
        _ASSERT_CORRECTNESS_DEAD_VARS
    };
    // ************************** END PCFG enumerations and defines ************************* //
    // ************************************************************************************** //
    
    
    
    // ************************************************************************************** //
    // ******************************* PCFG Loop Control class ****************************** //

    class Node;

    //! Class storing information about nodes of a loop control
    class PCFGLoopControl
    {
    private:
        Node* _init;
        Node* _cond;
        Node* _next;

    public:
        //! Empty constructor
        PCFGLoopControl();

        //! Destructor
        ~PCFGLoopControl();

    friend class PCFGVisitor;
    };

    // ***************************** END PCFG Loop Control class **************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ******************************** PCFG Try block class ******************************** //

    //! Class storing information about nodes in a try-block
    class PCFGTryBlock
    {
    private:
        ObjectList<Node*> _handler_parents;
        ObjectList<Node*> _handler_exits;
        int _nhandlers;

    public:
        //! Empty constructor
        PCFGTryBlock();

        //! Destructor
        ~PCFGTryBlock();

    friend class PCFGVisitor;
    };

    class PCFGSwitch
    {
    private:
        Node* _condition;       // Is not the condition itself, but the entry node of the context generated by the Switch Statement
        Node* _exit;

    public:
        //! Empty constructor
        PCFGSwitch(Node* condition, Node* exit);

        //! Destructor
        ~PCFGSwitch();
        
        //! Setters
        void set_condition(Node* condition);

    friend class PCFGVisitor;
    };

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ***************************** PCFG OmpSs pragma classes ****************************** //

    //! Class storing information about pragma clauses
    class PCFGPragmaInfo
    {
    private:
        ObjectList<NBase> _clauses;
        
    public:
        //! Constructor
        PCFGPragmaInfo(const NBase& clause);
        
        //! Deafault constructor (needed to use this object in LinkData)
        PCFGPragmaInfo();
        
        //! Copy constructor (needed to use this object in ObjectList, vector, etc.)
        PCFGPragmaInfo(const PCFGPragmaInfo& p);
        
        bool has_clause(node_t kind) const;
        NBase get_clause(node_t kind) const;
        void add_clause(const NBase& clause);
        
        ObjectList<NBase> get_clauses() const;

    friend class PCFGVisitor;
    };

    // **************************** END PCFG OmpSs pragma classes *************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ********************************** PCFG utils class ********************************** //

    //! Class storing temporary values used during the construction of a PCFG
    //! for both PCFGVisitor and ExtensibleGraph classes
    class LIBTL_CLASS PCFGVisitUtils
    {
    private:

        // ******************************************* //
        // ***** Members storing temporal values ***** //

        //! List of nodes that will be parents of a new node
        ObjectList<Node*> _last_nodes;

        //! Nodes found in the current function that are an exit point of the function
        ObjectList<Node*> _return_nodes;

        //! This attribute contains the node we are building
        //! PCFGVisitor will create and destroy this node depending on the statements it is traversing
        std::stack<Node*> _outer_nodes;

        //! Stacks to keep the exit nodes of Loop Statements
        std::stack<Node*> _continue_nodes;
        std::stack<Node*> _break_nodes;

        //! Lists to keep special nodes that breaks the expected behavior of the flow
        ObjectList<Node*> _labeled_nodes;
        ObjectList<Node*> _goto_nodes;


        //! Container to keep information of complex statements during its nodes the construction
        std::stack<PCFGSwitch*> _switch_nodes;
        std::stack<PCFGLoopControl*> _nested_loop_nodes;
        ObjectList<PCFGTryBlock*> _tryblock_nodes;

        //! Stack to keep track of current nested OmpSs pragmas
        std::stack<PCFGPragmaInfo> _pragma_nodes;

        //! Container to store information about the current context
        std::stack<NBase> _context_nodecl;

        //! Container to store all SECTION nodes within a SECTIONS
        std::stack<ObjectList<Node*> > _section_nodes;

        //! Stack to keep track of current nested assert pragmas
        std::stack<Node*> _assert_nodes;
        
        //! Container to store the ENTRY and EXIT nodes to be used when the ENVIRONMENT
        //! creates new nodes
        std::stack<std::pair<Node*, Node*> > _environ_entry_exit;

        //! Boolean indicating whether we are building vector nodes or not
        bool _is_vector;
        
        //! Counter used to create a unique key for each new node
        unsigned int _nid;

        // *** END members storing temporal values *** //
        // ******************************************* //

    public:

        //! Constructor
        PCFGVisitUtils();

    friend class ExtensibleGraph;
    friend class PCFGVisitor;
    };

    std::string print_node_list(const ObjectList<Node*>& list);
    
    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //
    
    // ******************************************************************************************* //
    // **************************** Class for task synchronizations ****************************** //
    
    struct AliveTaskItem
    {
        Node* node;
        // Arbitrary domain id. Every nesting domain has its own domain id
        int domain;
        
        AliveTaskItem(Node* node_, int domain_)
            : node(node_), domain(domain_)
        {}
        
        bool operator<(const AliveTaskItem& it) const
        {
            return (this->node < it.node)
                    || (!(it.node < this->node) && 
                    (this->domain < it.domain));
        }
        
        bool operator==(const AliveTaskItem& it) const
        {
            return (this->node == it.node)
                    && (this->domain == it.domain);
        }
        
    };
    
    typedef std::set<AliveTaskItem> AliveTaskSet;
    
    // ************************** END class for task synchronizations **************************** //
    // ******************************************************************************************* //

    class Node;
    class Edge;

    typedef ObjectList<Node*> NodeList;
    typedef ObjectList<Edge*> EdgeList;
    typedef ObjectList<EdgeType> EdgeTypeList;

}
}

#endif          // TL_PCFGVISIT_UTILS_HPP
