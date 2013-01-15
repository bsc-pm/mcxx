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

#include "cxx-process.h"
#include "tl-analysis-utils.hpp"
#include "tl-analysis-static-info.hpp"

namespace TL  {
namespace Analysis {

    NodeclStaticInfo::NodeclStaticInfo( ObjectList<Analysis::Utils::InductionVariableData*> induction_variables,
                                        ObjectList<Nodecl::NodeclBase> constants )
            : _induction_variables( induction_variables ), _constants( constants )
    {}

    bool NodeclStaticInfo::is_constant( const Nodecl::NodeclBase& n ) const
    {
        return _constants.contains( n );
    }

    bool NodeclStaticInfo::is_induction_variable( const Nodecl::NodeclBase& n )
    {
        bool result = false;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n ) )
            {
                result = true;
                break;
            }
        }

        return result;
    }


    AnalysisStaticInfo::AnalysisStaticInfo( const Nodecl::NodeclBase n )
    {
        TL::Analysis::AnalysisSingleton& analysis = TL::Analysis::AnalysisSingleton::get_analysis( );

        TL::Analysis::PCFGAnalysis_memento analysis_state;

        // Look for parent Function Code
        Nodecl::NodeclBase parent_function = n;
        while( !parent_function.is<Nodecl::FunctionCode>( ) )
        {
            parent_function = parent_function.get_parent( );
        }

        // Compute "dynamic" analysis
        analysis.use_def( analysis_state, parent_function );
        analysis.liveness( analysis_state, parent_function );
        analysis.reaching_definitions( analysis_state, parent_function );
        analysis.induction_variables( analysis_state, parent_function );

        // Save static analysis
        NodeclStaticInfo static_info( analysis_state.get_induction_variables( n ),
                                        analysis_state.get_constants( n ) );
        _static_info_map.insert( static_info_pair_t( n, static_info ) );

        // TODO
        //Visit

    }

    bool AnalysisStaticInfo::is_constant( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );

        if( scope_static_info == _static_info_map.end( ) )
        {
            nodecl_t scope_t = scope.get_internal_nodecl( );

            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. Cannot get constant variables.'",
                             codegen_to_str( scope_t, nodecl_retrieve_context( scope_t ) ) );
        }

        return scope_static_info->second.is_constant( n );
    }

    bool AnalysisStaticInfo::is_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n )
    {
        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );

        if( scope_static_info == _static_info_map.end( ) )
        {
            nodecl_t scope_t = scope.get_internal_nodecl( );

            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. Cannot get induction variables.'",
                             codegen_to_str( scope_t, nodecl_retrieve_context( scope_t ) ) );
        }

        NodeclStaticInfo current_info = scope_static_info->second;
        return current_info.is_induction_variable( n );
    }

}
}



