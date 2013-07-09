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

#include <stack>

namespace TL {
namespace Analysis {

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

    enum Clause {
        AUTO,
        DEP_IN,
        DEP_IN_VALUE,
        DEP_OUT,
        DEP_INOUT,
        DEP_CONCURRENT,
        DEP_COMMUTATIVE,
        COPY_IN,
        COPY_OUT,
        COPY_INOUT,
        FIRSTPRIVATE,
        FIRSTLASTPRIVATE,
        LASTPRIVATE,
        FLUSHED_VARS,       // Convenient clause for FLUSH flushed vars
        IF,
        FINAL_TASK,
        NAME,
        NOWAIT,
        PRIORITY,
        PRIVATE,
        REDUCTION,
        SCHEDULE,
        SHARED,
        TARGET,
        UNDEFINED_CLAUSE,
        UNTIED,
        WAITON,
        COMBINED_WORKSHARING,
    };

    class PCFGClause {
    private:
        Clause _clause;
        ObjectList<Nodecl::NodeclBase> _args;

    public:
        //! Empty constructor
        PCFGClause( );

        //! Constructor
        PCFGClause( Clause c );
        PCFGClause( Clause c, Nodecl::NodeclBase arg );

        //! Copy constructor
        PCFGClause( const PCFGClause& clause );

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

        bool has_clause( Clause clause );

        void add_clause( PCFGClause pcfg_clause );

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

        //! Container to store the ENTRY and EXIT nodes to be used when the ENVIRONMENT
        //! creates new nodes
        std::stack<std::pair<Node*, Node*> > _environ_entry_exit;

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
        {
        }
        
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
