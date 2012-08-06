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

#include "tl-node.hpp"
#include "tl-pcfg-utils.hpp"

namespace TL {
namespace Analysis {

    // ************************************************************************************** //
    // ******************************* PCFG Loop Control class ****************************** //

    PCFGLoopControl::PCFGLoopControl( )
        : _init( NULL ), _cond( NULL ), _next( NULL )
    {}

    PCFGLoopControl::PCFGLoopControl( const PCFGLoopControl& loop_ctrl )
    {
        _init = loop_ctrl._init;
        _cond = loop_ctrl._cond;
        _next = loop_ctrl._next;
    }

    PCFGLoopControl::~PCFGLoopControl( )
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
        : _handler_parents( ), _handler_exits( ), _nhandlers( -1 )
    {}

    PCFGTryBlock::PCFGTryBlock( const PCFGTryBlock& try_block )
    {
        _handler_parents = try_block._handler_parents;
        _handler_exits = try_block._handler_exits;
        _nhandlers = try_block._nhandlers;
    }

    PCFGTryBlock::~PCFGTryBlock( )
    {}

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ***************************** PCFG OmpSs pragma classes ****************************** //

    PCFGClause::PCFGClause( )
        : _clause( UNDEFINED_CLAUSE ), _args( ObjectList<Nodecl::NodeclBase> ( 1, Nodecl::NodeclBase::null( ) ) )
    {}

    PCFGClause::PCFGClause( Clause c )
        : _clause( c ), _args( )
    {}

    PCFGClause::PCFGClause( Clause c, Nodecl::NodeclBase arg )
        : _clause( c ), _args( )
    {
        if( arg.is<Nodecl::List>( ) )
        {
            Nodecl::List arg_list = arg.as<Nodecl::List>( );
            for( Nodecl::List::iterator it = arg_list.begin( ); it != arg_list.end( ); ++it )
            {
                _args.append( *it );
            }
        }
        else
        {
            _args.append( arg );
        }
    }

    PCFGClause::PCFGClause( const PCFGClause& c )
    {
        _clause = c._clause;
        _args = c._args;
    }

    PCFGPragmaInfo::PCFGPragmaInfo( )
        : _clauses( )
    {}

    PCFGPragmaInfo::PCFGPragmaInfo( PCFGClause clause )
        : _clauses( ObjectList<PCFGClause>( 1, clause ) )
    {}

    PCFGPragmaInfo::PCFGPragmaInfo( const PCFGPragmaInfo& p )
    {
        _clauses = p._clauses;
    }

    PCFGPragmaInfo::~PCFGPragmaInfo( )
    {}

    bool PCFGPragmaInfo::has_clause( Clause clause )
    {
        for (ObjectList<PCFGClause>::iterator it = _clauses.begin( ); it != _clauses.end( ); ++it )
        {
            if ( it->_clause == clause )
                return true;
        }
        return false;
    }

    void PCFGPragmaInfo::add_clause( PCFGClause pcfg_clause )
    {
        _clauses.append( pcfg_clause );
    }

    // **************************** END PCFG OmpSs pragma classes *************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ********************************** PCFG utils class ********************************** //

    PCFGVisitUtils::PCFGVisitUtils( )
        : _last_nodes( ), _return_nodes( ), _outer_nodes( ),
          _continue_nodes( ), _break_nodes( ), _labeled_nodes( ), _goto_nodes( ),
          _switch_condition_nodes( ), _nested_loop_nodes( ), _tryblock_nodes( ),
          _pragma_nodes( ), _context_nodecl( ), _nid( -1 )
    {}

    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //

}
}