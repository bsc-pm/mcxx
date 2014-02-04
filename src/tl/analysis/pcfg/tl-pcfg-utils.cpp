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

    PCFGTryBlock::~PCFGTryBlock( )
    {}

    // ****************************** END PCFG Try block class ****************************** //
    // ************************************************************************************** //


    PCFGSwitch::PCFGSwitch( Node* condition, Node* exit  )
        : _condition( condition ), _exit( exit )
    {}

    PCFGSwitch::~PCFGSwitch( )
    {
        delete _condition;
        delete _exit;
    }


    // ************************************************************************************** //
    // ***************************** PCFG OmpSs pragma classes ****************************** //

    PCFGClause::PCFGClause( )
        : _clause( __undefined_clause ), _args( )
    {}

    PCFGClause::PCFGClause( Clause c )
        : _clause( c ), _args( )
    {}

    PCFGClause::PCFGClause( Clause c, Nodecl::NodeclBase arg )
        : _clause( c ), _args( )
    {
        _args.append( arg.shallow_copy( ) );
    }

    PCFGClause::PCFGClause( const PCFGClause& c )
    {
        _clause = c._clause;
        _args = c._args;
    }

    Clause PCFGClause::get_clause( ) const
    {
        return _clause;
    }
    
    //! Returns a string with the graph type of the node.
    inline std::string clause_to_str( Clause c )
    {
        switch( c )
        {
            #undef CLAUSE
            #define CLAUSE(X) case __##X : return #X;
            CLAUSE_LIST
            #undef CLAUSE
            default: WARNING_MESSAGE( "Unexpected clause type '%d'", c );
        }
        return "";
    }
    
    std::string PCFGClause::get_clause_as_string( ) const
    {
        return clause_to_str( _clause );
    }
    
    Nodecl::List PCFGClause::get_args( ) const
    {
        return _args;
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

    bool PCFGPragmaInfo::has_clause( Clause clause ) const
    {
        for (ObjectList<PCFGClause>::const_iterator it = _clauses.begin( ); it != _clauses.end( ); ++it )
        {
            if ( it->_clause == clause )
                return true;
        }
        return false;
    }

    PCFGClause PCFGPragmaInfo::get_clause( Clause clause ) const
    {
        PCFGClause pcfg_clause;
        for (ObjectList<PCFGClause>::const_iterator it = _clauses.begin( ); it != _clauses.end( ); ++it )
        {
            if ( it->_clause == clause )
            {
                pcfg_clause = *it;
                break;
            }
        }
        return pcfg_clause;
    }
    
    void PCFGPragmaInfo::add_clause( PCFGClause pcfg_clause )
    {
        _clauses.append( pcfg_clause );
    }

    ObjectList<PCFGClause> PCFGPragmaInfo::get_clauses( ) const
    {
        return _clauses;
    }
    
    // **************************** END PCFG OmpSs pragma classes *************************** //
    // ************************************************************************************** //



    // ************************************************************************************** //
    // ********************************** PCFG utils class ********************************** //

    PCFGVisitUtils::PCFGVisitUtils( )
        : _last_nodes( ), _return_nodes( ), _outer_nodes( ),
          _continue_nodes( ), _break_nodes( ), _labeled_nodes( ), _goto_nodes( ),
          _switch_nodes( ), _nested_loop_nodes( ), _tryblock_nodes( ),
          _pragma_nodes( ), _context_nodecl( ), _section_nodes( ), _assert_nodes( ),
          _environ_entry_exit( ), _is_vector( false ), _nid( -1 )
    {}

    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //

}
}
