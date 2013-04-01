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

    Analysis::AnalysisStaticInfo *AutoScopeVisitor::_analysis_info = 0;

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
                = new Analysis::AnalysisStaticInfo( n, Analysis::WhichAnalysis::AUTO_SCOPING,
                                                    Analysis::WhereAnalysis::NESTED_OPENMP_TASK_STATIC_INFO, INT_MAX );

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

    }

    // ****************** END function Visitor looking for Tasks ******************** //
    // ****************************************************************************** //
}
}

EXPORT_PHASE(TL::OpenMP::AutoScopePhase)
