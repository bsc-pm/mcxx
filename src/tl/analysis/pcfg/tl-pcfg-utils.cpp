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

    Clause PCFGClause::get_clause( ) const
    {
        return _clause;
    }
    
    std::string PCFGClause::get_clause_as_string( ) const
    {
        std::string clause;
        switch( _clause )
        {
            case ASSERT_DEAD:           clause = "dead";                break;
            case ASSERT_DEFINED:        clause = "defined";             break;
            case ASSERT_INDUCTION_VAR:  clause = "induction_var";       break;
            case ASSERT_LIVE_IN:        clause = "live_in";             break;
            case ASSERT_LIVE_OUT:       clause = "live_out";            break;
            case ASSERT_REACH_IN:       clause = "reach_in";            break;
            case ASSERT_REACH_OUT:      clause = "reach_out";           break;
            case ASSERT_UPPER_EXPOSED:  clause = "upper_exposed";       break;
            case AUTO:                  clause = "auto";                break;
            case DEP_IN:                clause = "in";                  break;
            case DEP_IN_VALUE:          clause = "in_value";            break;
            case DEP_OUT:               clause = "out";                 break;
            case DEP_INOUT:             clause = "inout";               break;
            case DEP_CONCURRENT:        clause = "concurrent";          break;
            case DEP_COMMUTATIVE:       clause = "commutative";         break;
            case COPY_IN:               clause = "copy_in";             break;
            case COPY_OUT:              clause = "copy_out";            break;
            case COPY_INOUT:            clause = "copy_inout";          break;
            case FIRSTPRIVATE:          clause = "firstprivate";        break;
            case FIRSTLASTPRIVATE:      clause = "firstlastprivate";    break;
            case LASTPRIVATE:           clause = "lastprivate";         break;
            case FLUSHED_VARS:          clause = "flush";               break;
            case IF:                    clause = "if";                  break;
            case FINAL_TASK:            clause = "final";               break;
            case NAME:                  clause = "name";                break;
            case NOWAIT:                clause = "nowait";              break;
            case PRIORITY:              clause = "priority";            break;
            case PRIVATE:               clause = "private";             break;
            case REDUCTION:             clause = "reduction";           break;
            case SCHEDULE:              clause = "schedule";            break;
            case SHARED:                clause = "shared";              break;
            case TARGET:                clause = "target";              break;
            case UNDEFINED_CLAUSE:      clause = "UNDEFINED";           break;
            case UNTIED:                clause = "untied";              break;
            case VECTOR_LENGTH_FOR:     clause = "vector_length_for";   break;
            case MASK:                  clause = "mask";                break;
            case NO_MASK:               clause = "no_mask";             break;
            case SUITABLE:              clause = "suitable";            break;
            case WAITON:                clause = "waiton";              break;
        }
        return clause;
    }
    
    ObjectList<Nodecl::NodeclBase> PCFGClause::get_args( ) const
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
          _pragma_nodes( ), _context_nodecl( ), _section_nodes( ), _environ_entry_exit( ),
          _is_vector( false ), _nid( -1 )
    {}

    // ************************************************************************************** //
    // ******************************** END PCFG utils class ******************************** //

}
}
