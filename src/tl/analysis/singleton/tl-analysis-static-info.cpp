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

    // ********************************************************************************************* //
    // **************** Class to retrieve analysis info about one specific nodecl ****************** //

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

    // ************** END class to retrieve analysis info about one specific nodecl **************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************************** User interface for static analysis ***************************** //

    AnalysisStaticInfo::AnalysisStaticInfo( const Nodecl::NodeclBase n, analysis_tag analysis_mask,
                                            nested_analysis_tag nested_analysis_mask, int nesting_level)
    {
        TL::Analysis::AnalysisSingleton& analysis = TL::Analysis::AnalysisSingleton::get_analysis( );

        TL::Analysis::PCFGAnalysis_memento analysis_state;

        // Compute "dynamic" analysis

        ObjectList<Analysis::Utils::InductionVariableData*> induction_variables;
        ObjectList<Nodecl::NodeclBase> constants;

        if( analysis_mask & PCFG_ANALYSIS )
        {
            analysis.parallel_control_flow_graph( analysis_state, n );
        }
        if( analysis_mask & USAGE_ANALYSIS )
        {
            analysis.use_def( analysis_state, n );
        }
        if( analysis_mask & LIVENESS_ANALYSIS )
        {
            analysis.liveness( analysis_state, n );
        }
        if( analysis_mask & REACHING_DEFS_ANALYSIS )
        {
            analysis.reaching_definitions( analysis_state, n );
        }
        if( analysis_mask & INDUCTION_VARS_ANALYSIS )
        {
            analysis.induction_variables( analysis_state, n );
        }
        if( analysis_mask & CONSTANTS_ANALYSIS )
        {
//             analysis.constants( analysis_state, n );
        }

        // Save static analysis
        NestedBlocksStaticInfoVisitor v( analysis_mask, nested_analysis_mask, analysis_state, nesting_level );
        v.walk( n );
        static_info_map_t nested_blocks_static_info = v.get_analysis_info( );
        _static_info_map.insert( nested_blocks_static_info.begin( ), nested_blocks_static_info.end( ) );
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

    // ************************** END User interface for static analysis *************************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************* Visitor retrieving the analysis of a given Nodecl ********************* //

    NestedBlocksStaticInfoVisitor::NestedBlocksStaticInfoVisitor( analysis_tag analysis_mask,
                                                                  nested_analysis_tag nested_analysis_mask,
                                                                  PCFGAnalysis_memento state,
                                                                  int nesting_level)
            : _state( state ), _analysis_mask( analysis_mask ), _nested_analysis_mask( nested_analysis_mask ),
              _nesting_level( nesting_level ), _current_level( 0 ),  _analysis_info( )
    {}

    NestedBlocksStaticInfoVisitor::Ret NestedBlocksStaticInfoVisitor::join_list( ObjectList<static_info_map_t>& list )
    {
        for( ObjectList<static_info_map_t>::iterator it = list.begin( ); it != list.end( );  ++it)
        {
            _analysis_info.insert( it->begin( ), it->end( ) );
        }
    }

    static_info_map_t NestedBlocksStaticInfoVisitor::get_analysis_info( )
    {
        return _analysis_info;
    }

    void NestedBlocksStaticInfoVisitor::retrieve_current_node_static_info( Nodecl::NodeclBase n )
    {
        // The queries to the analysis info depend on the mask
        ObjectList<Nodecl::NodeclBase> cs;
        ObjectList<Analysis::Utils::InductionVariableData*> ivs;
        if( _analysis_mask | CONSTANTS_ANALYSIS )
            cs = _state.get_constants( n );
        if( _analysis_mask | INDUCTION_VARS_ANALYSIS )
            ivs = _state.get_induction_variables( n );

        NodeclStaticInfo static_info( ivs, cs );
        _analysis_info.insert( static_info_pair_t( n, static_info ) );

    }

    NestedBlocksStaticInfoVisitor::Ret NestedBlocksStaticInfoVisitor::visit(const Nodecl::DoStatement& n)
    {
        if( _nested_analysis_mask | NESTED_DO_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level)
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_statement( ) );
            }
        }
    }

    NestedBlocksStaticInfoVisitor::Ret NestedBlocksStaticInfoVisitor::visit(const Nodecl::IfElseStatement& n)
    {
        if( _nested_analysis_mask | NESTED_IF_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level)
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_then( ) );
                walk( n.get_else( ) );
            }
        }
    }

    NestedBlocksStaticInfoVisitor::Ret NestedBlocksStaticInfoVisitor::visit(const Nodecl::ForStatement& n)
    {
        if( _nested_analysis_mask | NESTED_FOR_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level)
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_statement( ) );
            }
        }
    }

    NestedBlocksStaticInfoVisitor::Ret NestedBlocksStaticInfoVisitor::visit(const Nodecl::WhileStatement& n)
    {
        if( _nested_analysis_mask | NESTED_WHILE_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level)
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodecl info
                walk( n.get_statement( ) );
            }
        }
    }

}
}
