/*--------------------------------------------------------------------
 ( C) Copyright 2006*-2012 Barcelona Supercomputing Center
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

#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // ******************************* PCFG Loop Control class ****************************** //

    LoopControlNode::LoopControlNode( )
        : init(NULL), cond(NULL), next(NULL)
    {}

    LoopControlNode::LoopControlNode( const LoopControlNode& loop_ctrl)
    {
        _init = loop_ctrl._init;
        _cond = loop_ctrl._cond;
        _next = loop_ctrl._next;
    }

    LoopControlNode::~LoopControlNode( )
    {
        delete _init;
        delete _cond;
        delete _next;
    }

    // ***************************** END PCFG Loop Control class **************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ******************************** PCFG Try block class ******************************** //

    PCFGTryBlock::PCFGTryBlock( )
        : _handler_parents( ), _handler_children( ), _nhandlers( -1 )
    {}

    //! Copy constructor
    PCFGTryBlock::PCFGTryBlock( const PCFGTryBlock& loop_ctrl)
    {
        _handler_parents = loop_ctrl._handler_parents;
        _handler_children = loop_ctrl._handler_children;
        _shandlers = loop_ctrl._nhandlers;
    }

    //! Destructor
    PCFGTryBlock::~PCFGTryBlock()
    {
        ~_handler_parents;
        ~_handler_children;
    }

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ********************************** PCFG utils class ********************************** //

    PCFGVisitUtils::PCFGVisitUtils( )
        : _last_nodes( ), _return_nodes( ), _outer_nodes( ),
          _continue_nodes( ), _break_nodes( ), _labeled_nodes( ), _goto_nodes( ),
          _nested_loop_nodes( ), _tryblock_nodes( ), _nid( -1 )
    {}

    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //

}
}