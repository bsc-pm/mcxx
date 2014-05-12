/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include <climits>

#include "tl-extended-symbol-utils.hpp"
#include "tl-omp-auto-scope.hpp"

namespace TL {
namespace OpenMP {

    // ****************************************************************************** //
    // *************** Phase for Automatic Data-Sharing computation ***************** //

    AutoScopePhase::AutoScopePhase( )
        : PragmaCustomCompilerPhase("omp-auto-scope"), _auto_scope_enabled( false )
    {
        set_phase_name( "Automatically compute the scope of variables in OpenMP tasks");
        set_phase_description( "This phase transforms the default(AUTO) clause in the proper data-sharing \n"\
                                "for each variable involved in the task" );

        register_parameter("auto_scope_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _auto_scope_enabled_str,
                           "0").connect(functor(&AutoScopePhase::set_auto_scope, *this));
    }

    void AutoScopePhase::pre_run(TL::DTO& dto)
    {
        this->PragmaCustomCompilerPhase::pre_run(dto);
    }

    void AutoScopePhase::run(TL::DTO& dto)
    {
        this->PragmaCustomCompilerPhase::run(dto);

        Nodecl::NodeclBase translation_unit = dto["nodecl"];

        if( _auto_scope_enabled )
        {
            DEBUG_CODE( )
            {
                std::cerr << "Phase calculating automatic scope for tasks =========" << std::endl;
            }
            AutoScopeVisitor sv;
            sv.walk( translation_unit );
        }
    }

    void AutoScopePhase::set_auto_scope( const std::string auto_scope_enabled_str )
    {
        if( auto_scope_enabled_str == "1" )
            _auto_scope_enabled = true;
    }

    // ************* END phase for Automatic Data-Sharing computation *************** //
    // ****************************************************************************** //



    // ****************************************************************************** //
    // ******************** Function Visitor looking for Tasks ********************** //

    Analysis::AnalysisInterface *AutoScopeVisitor::_analysis_info = 0;

    AutoScopeVisitor::AutoScopeVisitor( )
    {}

    AutoScopeVisitor::~AutoScopeVisitor( )
    {
        delete _analysis_info;
    }

    void AutoScopeVisitor::visit( const Nodecl::TopLevel& n )
    {
        // Automatically set the scope of the variables involved in the task, if possible
        AutoScopeVisitor::_analysis_info
                = new Analysis::AnalysisInterface(n, Analysis::WhichAnalysis::AUTO_SCOPING );

        // Print the results for each task with a default(AUTO) clause
        std::cerr << "***********************************************************" << std::endl;
        std::cerr << "****************** AUTO-SCOPING RESULTS: ******************" << std::endl;
        walk( n.get_top_level( ) );
        std::cerr << "***********************************************************" << std::endl;
    }

    void AutoScopeVisitor::visit( const Nodecl::OpenMP::Task& n )
    {
        // Retrieve the results of the Auto-Scoping process to the user
        _analysis_info->print_auto_scoping_results( n );

        // Modify the Nodecl with the new variables' scope
        Analysis::Utils::AutoScopedVariables autosc_vars = _analysis_info->get_auto_scoped_variables( n );
        Analysis::Utils::ext_sym_set private_ext_syms, firstprivate_ext_syms, race_ext_syms,
                                     shared_ext_syms, undef_ext_syms;
        Nodecl::NodeclBase user_private_vars, user_firstprivate_vars, user_shared_vars;

        // Get actual environment
        Nodecl::List environ = n.get_environment().as<Nodecl::List>();
        for( Nodecl::List::iterator it = environ.begin( ); it != environ.end( ); )
        {
            if( it->is<Nodecl::OpenMP::Auto>( ) )
            {
                it = environ.erase( it );
            }
            else
            {
                if( it->is<Nodecl::OpenMP::Private>( ) )
                {
                    user_private_vars = it->as<Nodecl::OpenMP::Private>( );
                }
                if( it->is<Nodecl::OpenMP::Firstprivate>( ) )
                {
                    user_firstprivate_vars = it->as<Nodecl::OpenMP::Firstprivate>( );
                }
                if( it->is<Nodecl::OpenMP::Shared>( ) )
                {
                    user_shared_vars = it->as<Nodecl::OpenMP::Shared>( );
                }
                ++it;
            }
        }

        // Remove user-scoped variables from auto-scoped variables and reset environment
        private_ext_syms = autosc_vars.get_private_vars( );
        if( !private_ext_syms.empty( ) )
        {
            ObjectList<Nodecl::NodeclBase> autosc_private_vars;
            for( Analysis::Utils::ext_sym_set::iterator it = private_ext_syms.begin( ); it != private_ext_syms.end( ); ++it )
            {
                autosc_private_vars.insert( it->get_nodecl( ) );
            }
            ObjectList<Nodecl::NodeclBase> purged_autosc_private_vars;
            for( ObjectList<Nodecl::NodeclBase>::iterator it = autosc_private_vars.begin( );
                it != autosc_private_vars.end( ); ++it )
                {
                    if( !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_firstprivate_vars.as<Nodecl::List>( ) )
                        && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_private_vars.as<Nodecl::List>( ) )
                        && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_shared_vars.as<Nodecl::List>( ) ) )
                    {
                        purged_autosc_private_vars.insert( it->shallow_copy() );
                    }
                }
                if( !purged_autosc_private_vars.empty( ) )
                {
                    Nodecl::OpenMP::Private private_node =
                    Nodecl::OpenMP::Private::make( Nodecl::List::make( purged_autosc_private_vars ),
                                                        n.get_locus( ) );
                    environ.append( private_node );
                }
        }

        firstprivate_ext_syms = autosc_vars.get_firstprivate_vars( );
        if( !firstprivate_ext_syms.empty( ) )
        {
            ObjectList<Nodecl::NodeclBase> autosc_firstprivate_vars;
            for( Analysis::Utils::ext_sym_set::iterator it = firstprivate_ext_syms.begin( ); it != firstprivate_ext_syms.end( ); ++it )
            {
                autosc_firstprivate_vars.insert( it->get_nodecl( ) );
            }
            ObjectList<Nodecl::NodeclBase> purged_autosc_firstprivate_vars;
            for( ObjectList<Nodecl::NodeclBase>::iterator it = autosc_firstprivate_vars.begin( );
                 it != autosc_firstprivate_vars.end( ); ++it )
            {
                if( !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_firstprivate_vars.as<Nodecl::List>( ) )
                    && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_private_vars.as<Nodecl::List>( ) )
                    && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_shared_vars.as<Nodecl::List>( ) ) )
                {
                    purged_autosc_firstprivate_vars.insert( it->shallow_copy() );
                }
            }
            if( !purged_autosc_firstprivate_vars.empty( ) )
            {
                Nodecl::OpenMP::Firstprivate firstprivate_node =
                        Nodecl::OpenMP::Firstprivate::make( Nodecl::List::make( purged_autosc_firstprivate_vars ),
                                                            n.get_locus( ) );
                environ.append( firstprivate_node );
            }
        }

        shared_ext_syms = autosc_vars.get_shared_vars( );
        if( !shared_ext_syms.empty( ) )
        {
            ObjectList<Nodecl::NodeclBase> autosc_shared_vars;
            for( Analysis::Utils::ext_sym_set::iterator it = shared_ext_syms.begin( ); it != shared_ext_syms.end( ); ++it )
            {
                autosc_shared_vars.insert( it->get_nodecl( ) );
            }
            ObjectList<Nodecl::NodeclBase> purged_autosc_shared_vars;
            for( ObjectList<Nodecl::NodeclBase>::iterator it = autosc_shared_vars.begin( );
                it != autosc_shared_vars.end( ); ++it )
                {
                    if( !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_firstprivate_vars.as<Nodecl::List>( ) )
                        && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_private_vars.as<Nodecl::List>( ) )
                        && !Nodecl::Utils::nodecl_is_in_nodecl_list( *it, user_shared_vars.as<Nodecl::List>( ) ) )
                    {
                        purged_autosc_shared_vars.insert( it->shallow_copy() );
                    }
                }
                if( !purged_autosc_shared_vars.empty( ) )
                {
                    Nodecl::OpenMP::Shared shared_node =
                    Nodecl::OpenMP::Shared::make( Nodecl::List::make( purged_autosc_shared_vars ),
                                                  n.get_locus( ) );
                    environ.append( shared_node );
                }
        }
    }

    // ****************** END function Visitor looking for Tasks ******************** //
    // ****************************************************************************** //
}
}

EXPORT_PHASE(TL::OpenMP::AutoScopePhase)
