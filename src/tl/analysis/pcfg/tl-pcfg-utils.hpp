/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion

  This file is part of Mercurium C/C++ source-to-source compiler.

  See AUTHORS file in the top level directory for information
  regarding developers and contributors.

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option ) any later version.

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

#include "tl-nodecl.hpp"
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
    NODE_TYPE(VectorFunctionCall) \
    NODE_TYPE(VectorGather) \
    NODE_TYPE(VectorLoad) \
    NODE_TYPE(VectorNormal) \
    NODE_TYPE(VectorReduction) \
    NODE_TYPE(VectorScatter) \
    NODE_TYPE(VectorStore) \
    NODE_TYPE(Graph)
    
    enum Node_type {
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
    GRAPH_TYPE(IfElse) \
    GRAPH_TYPE(LoopDoWhile) \
    GRAPH_TYPE(LoopFor) \
    GRAPH_TYPE(LoopWhile) \
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
    GRAPH_TYPE(OmpWorkshare) \
    GRAPH_TYPE(OmpTask) \
    GRAPH_TYPE(SplitStmt) \
    GRAPH_TYPE(Switch) \
    GRAPH_TYPE(VectorCondExpr) \
    GRAPH_TYPE(VectorFunctionCallGraph)
    
    enum Graph_type {
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
    
    enum Edge_type {
        #undef EDGE_TYPE
        #define EDGE_TYPE(X) __##X,
        EDGE_TYPE_LIST
        #undef EDGE_TYPE
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
    
    /*! \def _NODE_TYPE
     * Type of a node. This will be a value of the enumeration Node_type.
     * Mandatory in all nodes.
     */
    #define _NODE_TYPE                      "node_type"
    
    /*! \def _OUTER_NODE
     * Pointer to the node that contains a node.
     * __ExtensibleGraph node is the only on with this value equals to NULL
     * Mandatory in all nodes.
     */
    #define _OUTER_NODE                     "outer_node"
    
    /*! \def _NODE_LABEL
     * String containing the label of a node.
     * It may have different meanings depending on the node type:
     *   - Graph / ASM_OP: is the Statement that builds the node.
     *   - Goto / Label: label that identifies the source or target of the Statement contained.
     * Mandatory and only available in 'Composite', 'Labeled' or 'Goto' nodes.
     */
    #define _NODE_LABEL                     "node_label"
    
    /*! \def _NODE_STMTS
     * List of Statements within the Basic Block contained in a node.
     * Mandatory and only available in basic normal nodes.
     */
    #define _NODE_STMTS                     "statements"
    
    /*!  \def _ENTRY_NODE
     * Node which is the entry point of a composite node.
     * Mandatory and only available in composite nodes.
     */
    #define _ENTRY_NODE                     "entry"
    
    /*! \def _EXIT_NODE
     * Node which is the exit point of a composite node.
     * Mandatory and only available in composite nodes.
     */
    #define _EXIT_NODE                      "exit"
    
    /*! \def _STRIDE_NODE
     * Node containing the loop stride statement within a loop graph node.
     * Mandatory and only available in Loop Graph nodes.
     */
    #define _STRIDE_NODE                    "stride"
    
    /*! \def _GRAPH_TYPE
     * Type of the graph node. This will be a value of the enumeration Graph_type.
     * Mandatory in all graph nodes.
     */
    #define _GRAPH_TYPE                     "graph_type"
    
    /*! \def _OMP_INFO
     * Data associated to an OmpSs pragma: pragma type and list of clauses withs its arguments
     * Mandatory in all OMP_PRAGMA graph nodes.
     */
    #define _OMP_INFO                       "omp_info"
    
    /*! \def _ASM_INFO
     * Data indicating which info of the ASM function is contained in the ASM node
     * Mandatory in all ASM_DEF and ASM_OP nodes.
     */
    #define _ASM_INFO                       "asm_info"
    
    /*! \def _TASK_CONTEXT
     * Nodecl containing the context associated to a task
     * Mandatory and only available in composite nodes with _GRAPH_TYPE "task"
     */
    #define _TASK_CONTEXT                   "task_context"
    
    /*! \def _TASK_FUNCTION
     * Nodecl containing the symbol of the function contained contained in task
     * Mandatory and only available in composite nodes with _GRAPH_TYPE "task" of declaration level tasks
     */
    #define _TASK_FUNCTION                  "task_function"
    
    /*! \def _CLAUSES
     * Set of clauses associated to a pragma
     * Available in Graph nodes of type 'omp_pragma' and 'task' but not mandat
     */
    #define _CLAUSES                        "clauses"
    
    /*! \def _ARGS
     * Empty clause associated to a pragma
     * Available in Graph nodes of type 'omp_pragma' and 'task'.
     */
    #define _ARGS                           "args"
    
    
    // Edge attributes
    //////////////////
    
    //! Definitions of the different edge attributes
    /*! \def _EDGE_TYPE
     * Type of the edge. This will be a value of the enumeration Edge_type.
     * Mandatory in all edges.
     */
    #define _EDGE_TYPE                      "edge_type"
    
    /*! \def _EDGE_LABEL
     * String containing the label of an edge.
     * Available and mandatory in all edges but those with 'Always' type.
     */
    #define _EDGE_LABEL                     "edge_label"
    
    /*! \def _IS_TASK_EDGE
     * Boolean indicating whether an edge connects a target being a Task
     * Available and mandatory in all edges.
     */
    #define _IS_TASK_EDGE                   "is_task_edge"
    
    
    // Constant propagation attributes
    //////////////////////////////////
    
    /*! \def _LATTICE_VALS
     * Values in the Lattice Cell of all Objects modified in a given node
     * Available and mandatory in all simple nodes after constant propagation optimization
     */
    #define _LATTICE_VALS                   "lattice_vals"
    
    /*! \def _IS_EXECUTABLE
     * Boolean value indicating whether an edge is executable or not
     * Available and mandatory in all edges after constant propagation optimization
     */
    #define _IS_EXECUTABLE                  "is_executables"
    
    /*! \def _IS_EXECUTABLE_EDGE
     * Boolean indicating whether an edge is executable or not
     * Available and mandatory in all edges after constant propagation optimization.
     */
    #define _IS_EXECUTABLE_EDGE             "is_executable_edge"
    
    // PCFG attributes
    //////////////////
    
    /*! \def _LIVE_IN_TASKS
     * Set of tasks that are alive at the Entry of a node
     * Available in all nodes during PCFG construction
     * FIXME Think about deleting this data after PCFG construction
     */
    #define _LIVE_IN_TASKS                  "live_in_tasks"
    
    /*! \def _LIVE_OUT_TASKS
     * Set of tasks that are alive at the Exit of a node
     * Available in all nodes during PCFG construction
     * FIXME Think about deleting this data after PCFG construction
     */
    #define _LIVE_OUT_TASKS                 "live_out_tasks"
    
    
    // UseDef attributes
    ////////////////////
    
    /*! \def _UPPER_EXPOSED
     * Set of upper exposed variables within a node.
     * Available in all nodes (Mandatory once the UseDef analysis is performed).
     */
    #define _UPPER_EXPOSED                  "ue_vars"
    
    /*! \def _KILLED
     * Set of killed variables within a node.
     * Available in all nodes (Mandatory once the UseDef analysis is performed).
     */
    #define _KILLED                         "killed_vars"
    
    /*! \def _UNDEF
     * Set of variables within a node that we cannot define the behaviour.
     * Available in all nodes (Mandatory once the UseDef analysis is performed).
     */
    #define _UNDEF                          "undefined_behaviour_vars"
    
    
    // Liveness attributes
    ///////////**/////////
    
    /*! \def _LIVE_IN
     * Set of variables that are alive at the entry point of a node.
     * Available in all nodes (Mandatory once the Liveness analysis is performed).
     */
    #define _LIVE_IN                        "live_in_vars"
    
    /*! \def _LIVE_OUT
     * Set of variables that are alive at the exit point of a node.
     * Available in all nodes (Mandatory once the Liveness analysis is performed).
     */
    #define _LIVE_OUT                       "live_out_vars"
    
    
    // Reaching definitions attributes
    //////////////////////////////////
    
    /*! \def _GEN
     * Map containing the statements that generate a definition of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _GEN                            "gen_stmts"
    
    /*! \def _REACH_DEFS_IN
     * Map containing the reaching definitions at the entry of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _REACH_DEFS_IN                  "reaching_defs_in"
    
    /*! \def _REACH_DEFS_OUT
     * Map containing the reaching definitions at the exit of a given node
     * Available in all nodes (Mandatory once the Reaching Definitions analysis is performed).
     */
    #define _REACH_DEFS_OUT                 "reaching_defs_out"
    
    /*! \def _AUX_REACH_DEFS
     * Map containing the propagated reaching definitions in a given point
     * This variable is used while propagating the reaching definitions among the nodes to differentiate
     * those definitions performed within the node and those that has been propagated
     * At the end of the propagation, the reaching definitions stored in this value are copied in the _REACH_DEFS variable
     * an this is deleted
     * Available in all nodes (Mandatory once the Liveness analysis is performed).
     */
    #define _AUX_REACH_DEFS                 "aux_reaching_defs"
    
    
    // Loop analysis attributes
    ///////////////////////////
    
    /*! \def _INDUCTION_VARS
     * Map containing the induction variables associated with a Loop Node
     * Available only in Loop (Graph) nodes (Mandatory once the Loop analysis is performed).
     */
    #define _INDUCTION_VARS                 "induction_vars"
    
    
    // Auto-scoping attributes
    //////////////////////////
    
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
    

    // Auto-deps attributes
    ///////////////////////
    
    /*! \def _DEPS_SHARED
     * Set of symbols classified as PRIVATE during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_PRIVATE                   "deps_private"
    
    /*! \def _DEPS_FIRSTPRIVATE
     * Set of symbols classified as FIRSTPRIVATE during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_FIRSTPRIVATE              "deps_firstprivate"
    
    /*! \def _DEPS_SHARED
     * Set of symbols classified as SHARED during the auto-deps routine
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_SHARED                    "deps_shared"
    
    /*! \def _IN_DEPS
     * Set of symbols with input dependence in a task
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_IN                        "deps_input"
    
    /*! \def _OUT_DEPS
     * Set of symbols with output dependence in a task
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_OUT                       "deps_output"
    
    /*! \def _INOUT_DEPS
     * Set of symbols with inout dependence in a task
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_INOUT                     "deps_inout"
    
    /*! \def _UNDEF_DEPS
     * Set of symbols with which we are unable to compute the proper dependencies in a task
     * Available Graph nodes with 'task' _GRAPH_TYPE (Mandatory once the Auto-deps is performed).
     */
    #define _DEPS_UNDEF                     "deps_undef"
    
    
    // Analysis checking attributes
    ///////////////////////////////
    
    /*! \def _ASSERT_UPPER_EXPOSED
     * Set of expressions marked by the user as upper exposed in a given point of the program
     */
    #define _ASSERT_UPPER_EXPOSED           "assert_upper_exposed"
    
    /*! \def _ASSERT_KILLED
     * Set of expressions marked by the user as defined in a given point of the program
     */
    #define _ASSERT_KILLED                  "assert_killed"

    /*! \def _ASSERT_UNDEFINED
     * Set of expressions marked by the user as undefined behavior in a given point of the program
     */
    #define _ASSERT_UNDEFINED               "assert_undefined"
    
    /*! \def _ASSERT_LIVE_IN
     * Set of expressions marked by the user as live in in a given point of the program
     */
    #define _ASSERT_LIVE_IN                 "assert_live_in"
    
    /*! \def _ASSERT_LIVE_OUT
     * Set of expressions marked by the user as live_out in a given point of the program
     */
    #define _ASSERT_LIVE_OUT                "assert_live_out"
    
    /*! \def _ASSERT_DEAD
     * Set of expressions marked by the user as dead in a given point of the program
     */
    #define _ASSERT_DEAD                    "assert_dead"
    
    /*! \def _ASSERT_REACH_DEFS_IN
     * Set of reaching definitions at the entry of a given point of the program
     */
    #define _ASSERT_REACH_DEFS_IN           "assert_reach_defs_in"
    
    /*! \def _ASSERT_REACH_DEFS_OUT
     * Set of reaching definitions at the exit of a given point of the program
     */
    #define _ASSERT_REACH_DEFS_OUT          "assert_reach_defs_out"
    
    /*! \def _ASSERT_INDUCTION_VARS
     * Set of induction variables in a given point of the program
     */
    #define _ASSERT_INDUCTION_VARS          "assert_induction_vars"
    
    /*! \def _ASSERT_AUTOSC_FIRSTPRIVATE
     * Set of variables autoscoped as firstprivate in a given point of the program
     */
    #define _ASSERT_AUTOSC_FIRSTPRIVATE     "assert_autosc_firstprivate"
    
    /*! \def _ASSERT_AUTOSC_PRIVATE
     * Set of variables autoscoped as private in a given point of the program
     */
    #define _ASSERT_AUTOSC_PRIVATE          "assert_autosc_private"
    
    /*! \def _ASSERT_AUTOSC_SHARED
     * Set of variables autoscoped as shared in a given point of the program
     */
    #define _ASSERT_AUTOSC_SHARED           "assert_autosc_shared"
    
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
        PCFGLoopControl( );

        //! Destructor
        ~PCFGLoopControl( );

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
        PCFGTryBlock( );

        //! Destructor
        ~PCFGTryBlock( );

    friend class PCFGVisitor;
    };

    class PCFGSwitch
    {
    private:
        Node* _condition;
        Node* _exit;

    public:
        //! Empty constructor
        PCFGSwitch( Node* condition, Node* exit );

        //! Destructor
        ~PCFGSwitch( );

    friend class PCFGVisitor;
    };

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ***************************** PCFG OmpSs pragma classes ****************************** //

    #define CLAUSE_LIST \
    CLAUSE(assert_autosc_firstprivate) \
    CLAUSE(assert_autosc_private) \
    CLAUSE(assert_autosc_shared) \
    CLAUSE(assert_dead) \
    CLAUSE(assert_defined) \
    CLAUSE(assert_induction_var) \
    CLAUSE(assert_live_in) \
    CLAUSE(assert_live_out) \
    CLAUSE(assert_reach_in) \
    CLAUSE(assert_reach_out) \
    CLAUSE(assert_upper_exposed) \
    CLAUSE(assert_undefined_behaviour) \
    CLAUSE(auto) \
    CLAUSE(cache) \
    CLAUSE(concurrent) \
    CLAUSE(commutative) \
    CLAUSE(copy_in) \
    CLAUSE(copy_out) \
    CLAUSE(copy_inout) \
    CLAUSE(device) \
    CLAUSE(final) \
    CLAUSE(firstprivate) \
    CLAUSE(firstlastprivate) \
    CLAUSE(flushed_vars) \
    CLAUSE(if) \
    CLAUSE(in) \
    CLAUSE(in_alloca) \
    CLAUSE(in_value) \
    CLAUSE(inout) \
    CLAUSE(lastprivate) \
    CLAUSE(length_for) \
    CLAUSE(mask) \
    CLAUSE(name) \
    CLAUSE(no_mask) \
    CLAUSE(nowait) \
    CLAUSE(out) \
    CLAUSE(priority) \
    CLAUSE(private) \
    CLAUSE(reduction) \
    CLAUSE(schedule) \
    CLAUSE(shared) \
    CLAUSE(suitable) \
    CLAUSE(target) \
    CLAUSE(undefined_clause) \
    CLAUSE(unroll) \
    CLAUSE(untied) \
    CLAUSE(wait_on)
    
    enum Clause {
#undef CLAUSE
#define CLAUSE(X) __##X,
        CLAUSE_LIST
#undef CLAUSE
    };
    
    class PCFGClause {
    private:
        Clause _clause;
        Nodecl::List _args;

    public:
        //! Empty constructor
        PCFGClause( );

        //! Constructor
        PCFGClause( Clause c );
        PCFGClause( Clause c, Nodecl::NodeclBase arg );

        //! Copy constructor
        PCFGClause( const PCFGClause& clause );

        //! Getters
        Clause get_clause( ) const;
        std::string get_clause_as_string( ) const;
        Nodecl::List get_args( ) const;
        
    friend class PCFGVisitor;
    friend class PCFGPragmaInfo;
    };

    //! Class storing information about pragma clauses
    class PCFGPragmaInfo
    {
    private:
        ObjectList<PCFGClause> _clauses;

    public:
        //! Empty Constructor
        PCFGPragmaInfo( );

        //! Constructor
        PCFGPragmaInfo( PCFGClause clause );

        //! Copy constructor
        PCFGPragmaInfo( const PCFGPragmaInfo& pragma );

        //! Destructor
        ~PCFGPragmaInfo( );

        bool has_clause( Clause clause ) const;
        PCFGClause get_clause( Clause clause ) const;

        void add_clause( PCFGClause pcfg_clause );
        
        ObjectList<PCFGClause> get_clauses( ) const;

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
        std::stack<Nodecl::NodeclBase> _context_nodecl;

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
        PCFGVisitUtils( );

    friend class ExtensibleGraph;
    friend class PCFGVisitor;
    };

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
    
    typedef std::set<AliveTaskItem> StaticSyncTaskSet;
    typedef std::set<AliveTaskItem> AliveTaskSet;
    
    // ************************** END class for task synchronizations **************************** //
    // ******************************************************************************************* //
}
}

#endif          // TL_PCFGVISIT_UTILS_HPP
