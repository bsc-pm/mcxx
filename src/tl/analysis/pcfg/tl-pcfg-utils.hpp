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

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // ******************************* PCFG Loop Control class ****************************** //

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

        //! Copy constructor
        PCFGLoopControl( const PCFGLoopControl& loop_ctrl);

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
        PCFGTryBlock( );

        //! Copy constructor
        PCFGTryBlock( const PCFGTryBlock& loop_ctrl);

        //! Destructor
        ~PCFGTryBlock();

    friend class PCFGVisitor;
    };

    // ****************************** END PCFG Try block class ****************************** //
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
        std::stack<PCFGLoopControl*> _nested_loop_nodes;
        ObjectList<PCFGTryBlock*> _tryblock_nodes;


        //! Counter used to create a unique key for each new node
        int _nid;

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
}
}

#endif          // TL_PCFGVISIT_UTILS_HPP