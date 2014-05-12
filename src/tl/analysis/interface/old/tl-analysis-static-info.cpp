/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
#include "tl-expression-reduction.hpp"
#include "tl-use-def.hpp"

namespace TL  {
namespace Analysis {

    // ********************************************************************************************* //
    // ********************** Class to define which analysis are to be done ************************ //

    WhichAnalysis::WhichAnalysis( Analysis_tag a )
            : _which_analysis( a )
    {}

    WhichAnalysis::WhichAnalysis( int a )
            : _which_analysis( Analysis_tag( a ) )
    {}

    WhichAnalysis WhichAnalysis::operator|( WhichAnalysis a )
    {
        return WhichAnalysis( int( this->_which_analysis ) | int( a._which_analysis ) );
    }

    WhereAnalysis::WhereAnalysis( Nested_analysis_tag a )
            : _where_analysis( a )
    {}

    WhereAnalysis::WhereAnalysis( int a )
            : _where_analysis( Nested_analysis_tag( a ) )
    {}

    WhereAnalysis WhereAnalysis::operator|( WhereAnalysis a )
    {
        return WhereAnalysis( int(this->_where_analysis) | int( a._where_analysis ) );
    }

    // ******************** END class to define which analysis are to be done ********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************** Class to retrieve analysis info about one specific nodecl ****************** //

    NodeclStaticInfo::NodeclStaticInfo( ObjectList<Utils::InductionVariableData*> induction_variables,
                                        ObjectList<Symbol> reductions,
                                        Utils::ext_sym_set killed, ObjectList<ExtensibleGraph*> pcfgs,
                                        Node* autoscoped_task )
            : _induction_variables( induction_variables ), _reductions( reductions ) ,_killed( killed ),
              _pcfgs( pcfgs ), _autoscoped_task( autoscoped_task )
    {}

    NodeclStaticInfo::~NodeclStaticInfo()
    {}

    Node* NodeclStaticInfo::find_node_from_nodecl( const Nodecl::NodeclBase& n ) const
    {
        Node* result = NULL;
        for( ObjectList<ExtensibleGraph*>::const_iterator it = _pcfgs.begin( ); it != _pcfgs.end( ); ++it )
        {
            result = ( *it )->find_nodecl( n );
            if( result != NULL )
            {
                break;
            }
        }
        return result;
    }

    Node* NodeclStaticInfo::find_node_from_nodecl_pointer( const Nodecl::NodeclBase& n ) const
    {
        Node* result = NULL;
        for( ObjectList<ExtensibleGraph*>::const_iterator it = _pcfgs.begin( ); it != _pcfgs.end( ); ++it )
        {
            result = ( *it )->find_nodecl_pointer( n );
            if( result != NULL )
                break;
        }
        return result;
    }

    Node* NodeclStaticInfo::find_node_from_nodecl_in_scope( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& scope ) const
    {
        Node* result = NULL;
        for( ObjectList<ExtensibleGraph*>::const_iterator it = _pcfgs.begin( ); it != _pcfgs.end( ); ++it )
        {
            result = ( *it )->find_nodecl/*_in_scope*/( n/*, scope*/ );
            if( result != NULL )
            {
                break;
            }
        }
        return result;
    }

    ExtensibleGraph* NodeclStaticInfo::find_extensible_graph_from_nodecl( const Nodecl::NodeclBase& n ) const
    {
        ExtensibleGraph* result = NULL;
        for( ObjectList<ExtensibleGraph*>::const_iterator it = _pcfgs.begin( ); it != _pcfgs.end( ); ++it )
        {
            if( ( *it )->find_nodecl( n ) != NULL )
            {
                result = *it;
                break;
            }
        }
        return result;
    }

    bool NodeclStaticInfo::is_constant( const Nodecl::NodeclBase& n ) const
    {
        bool result = true;
        ObjectList<Nodecl::NodeclBase> n_mem_accesses = Nodecl::Utils::get_all_memory_accesses( n );
        for( ObjectList<Nodecl::NodeclBase>::iterator it = n_mem_accesses.begin( ); it != n_mem_accesses.end( ); ++it )
        {
            if( _killed.find( Utils::ExtendedSymbol( *it ) ) != _killed.end( ) )
            {
                result = false;
                break;
            }
        }
        return result;
    }

    bool NodeclStaticInfo::has_been_defined( const Nodecl::NodeclBase& n,
                                             const Nodecl::NodeclBase& s,
                                             const Nodecl::NodeclBase& scope ) const
    {
        bool result = false;
        if( n.is<Nodecl::Symbol>( ) || n.is<Nodecl::ArraySubscript>( ) || n.is<Nodecl::ClassMemberAccess>( ) )
        {
            Node* s_node = find_node_from_nodecl_pointer( s );
            Node* scope_node = find_node_from_nodecl_pointer( s );
            if( s_node == NULL )
            {
                WARNING_MESSAGE( "Nodecl '%s' not found in the current analysis. " \
                                 "Cannot compute whether '%s' has been defined. Returning false.\n",
                                 s.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
            }
            else if( ExtensibleGraph::node_contains_node( scope_node, s_node ) )
            {
                WARNING_MESSAGE( "Nodecl '%s' not found in the given analysis. " \
                                 "Cannot compute whether '%s' has been defined. Returning false.\n",
                                 s.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
            }
            else
            {
                // See if it has been defined in its own node, before 's'
                if( Utils::ext_sym_set_contains_nodecl( n, _killed ) )
                {
                    ObjectList<Nodecl::NodeclBase> stmts = s_node->get_statements( );
                    for( ObjectList<Nodecl::NodeclBase>::iterator it = stmts.begin( );
                         it != stmts.end( ); ++it )
                    {
                        if( !Nodecl::Utils::structurally_equal_nodecls( n, *it ) )
                        {
                            Node* fake_node = new Node( );
                            UsageVisitor uv( fake_node );
                            uv.compute_statement_usage( *it );
                            if( Utils::ext_sym_set_contains_nodecl( n, fake_node->get_killed_vars( ) ) )
                            {
                                result = true;
                                break;
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                }

                // Look for definitions in the parents
                s_node->set_visited( true );
                ObjectList<Node*> parents = s_node->get_parents( );
                for( ObjectList<Node*>::iterator it = parents.begin( );
                     it != parents.end( ) && !result; ++it )
                {
                    if( !ExtensibleGraph::is_backward_parent( s_node, *it ) )
                    {
                        result = result || ExtensibleGraph::has_been_defined( *it, scope_node, n );
                    }
                    ExtensibleGraph::clear_visits_aux( s_node );
                }
                ExtensibleGraph::clear_visits_backwards( s_node );
            }
        }
        else
        {
            WARNING_MESSAGE( "Nodecl '%s' is neither symbol, ArraySubscript or ClassMemberAccess. " \
                             "One of these types required as defined option. Returning false.\n", n.prettyprint( ).c_str( ) );
        }
        return result;
    }

    bool NodeclStaticInfo::is_induction_variable( const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = true;
                break;
            }
        }

        return result;
    }

    bool NodeclStaticInfo::is_basic_induction_variable( const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
            it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->is_basic( );
                break;
            }
        }

        return result;
    }

    bool NodeclStaticInfo::is_non_reduction_basic_induction_variable( const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        ObjectList<Analysis::Utils::InductionVariableData*> non_reduction_induction_variables;
        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if( !_reductions.contains( ( *it )->get_variable( ).get_symbol( ) ) )
                non_reduction_induction_variables.insert( *it );
        }

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = non_reduction_induction_variables.begin( );
             it != non_reduction_induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->is_basic( );
                break;
            }
        }

        return result;
    }

    bool NodeclStaticInfo::is_nested_induction_variable( Node* scope_node, Node* node, const Nodecl::NodeclBase& n )
    {
        bool result = false;
        
        Node* outer_node = node;
        while(((outer_node!=scope_node) || (outer_node!=NULL)) && !result )
        {
            if(outer_node->is_loop_node())
            {
                ObjectList<Utils::InductionVariableData*> loop_ivs = outer_node->get_induction_variables();
                for(ObjectList<Utils::InductionVariableData*>::const_iterator it = loop_ivs.begin( ); it != loop_ivs.end( ); ++it)
                {
                    if(Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable().get_nodecl(), n, /*skip_conversion_nodes*/ true))
                    {
                        result = true;
                        break;
                    }
                }
            }
            outer_node = outer_node->get_outer_node();
        }
        
        return result;
    }
    
    Utils::InductionVariableData* NodeclStaticInfo::get_nested_induction_variable(Node* scope_node, Node* node, const Nodecl::NodeclBase& n)
    {
        Utils::InductionVariableData* iv = NULL;
        
        Node* outer_node = node;
        while(((outer_node!=scope_node) || (outer_node!=NULL)) && (iv==NULL) )
        {
            if(outer_node->is_loop_node())
            {
                ObjectList<Utils::InductionVariableData*> loop_ivs = outer_node->get_induction_variables();
                for(ObjectList<Utils::InductionVariableData*>::const_iterator it = loop_ivs.begin( ); it != loop_ivs.end( ); ++it)
                {
                    if(Nodecl::Utils::structurally_equal_nodecls((*it)->get_variable().get_nodecl(), n, /*skip_conversion_nodes*/ true))
                    {
                        iv = *it;
                        break;
                    }
                }
            }
            outer_node = outer_node->get_outer_node();
        }
        
        return iv;
    }
    
    Nodecl::NodeclBase NodeclStaticInfo::get_induction_variable_lower_bound( const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it;
        for( it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_lb( );
                break;
            }
        }

        if( it == _induction_variables.end( ) )
        {
            WARNING_MESSAGE( "You are asking for the lower bound of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    Nodecl::NodeclBase NodeclStaticInfo::get_induction_variable_upper_bound( const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it;
        for( it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_ub( );
                break;
            }
        }

        if( it == _induction_variables.end( ) )
        {
            WARNING_MESSAGE( "You are asking for the upper bound of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    Nodecl::NodeclBase NodeclStaticInfo::get_induction_variable_increment( const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it;
        for( it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_increment( );
                break;
            }
        }

        if( it == _induction_variables.end( ) )
        {
            WARNING_MESSAGE( "You are asking for the increment of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    ObjectList<Nodecl::NodeclBase> NodeclStaticInfo::get_induction_variable_increment_list( const Nodecl::NodeclBase& n ) const
    {
        ObjectList<Nodecl::NodeclBase> result;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->get_increment_list( );
                break;
            }
        }

        if( result.empty( ) )
        {
            WARNING_MESSAGE( "You are asking for the increment of an Object ( %s ) "\
                             "which is not an Induction Variable\n", n.prettyprint( ).c_str( ) );
        }

        return result;
    }

    bool NodeclStaticInfo::is_induction_variable_increment_one( const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
             it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                result = ( *it )->is_increment_one( );
                break;
            }
        }

        return result;
    }

    Utils::InductionVariableData* NodeclStaticInfo::get_induction_variable( const Nodecl::NodeclBase& n ) const
    {
        Utils::InductionVariableData* iv = NULL;

        for( ObjectList<Analysis::Utils::InductionVariableData*>::const_iterator it = _induction_variables.begin( );
            it != _induction_variables.end( ); ++it )
        {
            if ( Nodecl::Utils::structurally_equal_nodecls( ( *it )->get_variable( ).get_nodecl( ), n, /* skip conversion nodes */ true ) )
            {
                iv = *it;
                break;
            }
        }

        return iv;
    }

    ObjectList<Utils::InductionVariableData*> NodeclStaticInfo::get_induction_variables( ) const
    {
        return _induction_variables;
    }

    void NodeclStaticInfo::print_auto_scoping_results( ) const
    {
        if( _autoscoped_task != NULL )
        {
            _autoscoped_task->print_auto_scoping( );
        }
    }

    Utils::AutoScopedVariables NodeclStaticInfo::get_auto_scoped_variables( )
    {
        Utils::AutoScopedVariables autosc_vars;
        if( _autoscoped_task != NULL )
        {
            autosc_vars = _autoscoped_task->get_auto_scoped_variables( );
        }
        return autosc_vars;
    }

    // ************** END class to retrieve analysis info about one specific nodecl **************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************************** User interface for static analysis ***************************** //

    AnalysisStaticInfo::AnalysisStaticInfo( )
        : _node( Nodecl::NodeclBase::null( ) ), _static_info_map( )
    {}

    AnalysisStaticInfo::AnalysisStaticInfo( const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask,
                                            WhereAnalysis nested_analysis_mask, int nesting_level )
    {
        _node = n;

        TL::Analysis::AnalysisSingleton& analysis = TL::Analysis::AnalysisSingleton::get_analysis( );

        TL::Analysis::PCFGAnalysis_memento analysis_state;

        // Compute "dynamic" analysis
        // Do it in such an order that the first is the most complete analysis and the last is the simplest one
        if( analysis_mask._which_analysis & WhichAnalysis::AUTO_SCOPING )
        {
            analysis.auto_scoping( analysis_state, n );
        }
        if( analysis_mask._which_analysis & WhichAnalysis::REACHING_DEFS_ANALYSIS )
        {
            analysis.reaching_definitions( analysis_state, n );
        }
        if( analysis_mask._which_analysis & WhichAnalysis::INDUCTION_VARS_ANALYSIS )
        {
            analysis.induction_variables( analysis_state, n );
        }
        if( analysis_mask._which_analysis & WhichAnalysis::LIVENESS_ANALYSIS )
        {
            analysis.liveness( analysis_state, n );
        }
        if( analysis_mask._which_analysis & ( WhichAnalysis::USAGE_ANALYSIS |
                                              WhichAnalysis::CONSTANTS_ANALYSIS ) )
        {
            analysis.use_def( analysis_state, n );
        }
        if( analysis_mask._which_analysis & WhichAnalysis::PCFG_ANALYSIS )
        {
            analysis.parallel_control_flow_graph( analysis_state, n );
        }

        if( CURRENT_CONFIGURATION->debug_options.print_pcfg )
            analysis.print_all_pcfg( analysis_state );

        // Save static analysis
        NestedBlocksStaticInfoVisitor v( analysis_mask, nested_analysis_mask, analysis_state, nesting_level );
        v.walk( n );
        static_info_map_t nested_blocks_static_info = v.get_analysis_info( );
        _static_info_map.insert( nested_blocks_static_info.begin( ), nested_blocks_static_info.end( ) );

        // Fill nodecl to pcfg map
        ObjectList<ExtensibleGraph*> pcfgs = analysis_state.get_pcfgs();
        for(ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            _func_to_pcfg_map[(*it)->get_nodecl()] = *it;
        }
    }

    AnalysisStaticInfo::~AnalysisStaticInfo()
    {}

    static_info_map_t AnalysisStaticInfo::get_static_info_map( ) const
    {
        return _static_info_map;
    }

    Nodecl::NodeclBase AnalysisStaticInfo::get_nodecl_origin( ) const
    {
        return _node;
    }

    DEPRECATED bool AnalysisStaticInfo::is_constant( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' is constant.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.is_constant( n );
        }

        return result;
    }

    bool AnalysisStaticInfo::has_been_defined( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n,
                                               const Nodecl::NodeclBase& s ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' has been defined.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.has_been_defined( n, s, scope );
        }
        return result;
    }

    bool AnalysisStaticInfo::is_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' is an induction variable.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result =  current_info.is_induction_variable( n );
        }

        return result;
    }

    bool AnalysisStaticInfo::is_basic_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' is a basic induction variable.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result =  current_info.is_basic_induction_variable( n );
        }

        return result;
    }

    bool AnalysisStaticInfo::is_non_reduction_basic_induction_variable( const Nodecl::NodeclBase& scope,
                                                                        const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' is a non-reduction basic induction variable.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result =  current_info.is_non_reduction_basic_induction_variable( n );
        }

        return result;
    }

    bool AnalysisStaticInfo::contains_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether '%s' contains the induction variable.'",
                             n.prettyprint( ).c_str( ), scope.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            Node* scope_node = current_info.find_node_from_nodecl_pointer( scope );
            if( scope_node == NULL )
                WARNING_MESSAGE( "No PCFG node found in the static info computed for nodecl %s.",
                                 scope.prettyprint( ).c_str( ) );

            result =  current_info.contains_induction_variable( n, scope_node );
        }

        return result;
    }

    Nodecl::NodeclBase AnalysisStaticInfo::get_induction_variable_lower_bound( const Nodecl::NodeclBase& scope,
                                                                               const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot get the lower bound of the induction variable '%s'.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.get_induction_variable_lower_bound( n );
        }

        return result;
    }

    Nodecl::NodeclBase AnalysisStaticInfo::get_induction_variable_upper_bound( const Nodecl::NodeclBase& scope,
                                                                               const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot get the upper bound of the induction variable '%s'.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.get_induction_variable_upper_bound( n );
        }

        return result;
    }

    Nodecl::NodeclBase AnalysisStaticInfo::get_induction_variable_increment( const Nodecl::NodeclBase& scope,
                                                                             const Nodecl::NodeclBase& n ) const
    {
        Nodecl::NodeclBase result;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot get the increment of the induction variable '%s'.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.get_induction_variable_increment( n );
        }

        return result;
    }

    ObjectList<Nodecl::NodeclBase> AnalysisStaticInfo::get_induction_variable_increment_list( const Nodecl::NodeclBase& scope,
                                                                                              const Nodecl::NodeclBase& n ) const
    {
        ObjectList<Nodecl::NodeclBase> result;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot get the list of increments of the induction variable '%s'.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.get_induction_variable_increment_list( n );
        }

        return result;
    }

    bool AnalysisStaticInfo::is_induction_variable_increment_one( const Nodecl::NodeclBase& scope,
                                                                  const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether the increment of '%s' is one.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.is_induction_variable_increment_one( n );
        }

        return result;
    }

    ObjectList<Utils::InductionVariableData*> AnalysisStaticInfo::get_induction_variables( const Nodecl::NodeclBase& scope,
                                                                                           const Nodecl::NodeclBase& n ) const
    {
        ObjectList<Utils::InductionVariableData*> result;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot get the induction variable list in '%s'.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.get_induction_variables( );
        }

        return result;
    }

    static bool nodecl_calls_outline_task( const Nodecl::NodeclBase& n, RefPtr<OpenMP::FunctionTaskSet> function_tasks )
    {
        if( n.is_null( ) )
            return false;

        bool result = false;

        // Check the current node
        if( n.is<Nodecl::FunctionCall>( ) )
        {
            Symbol s( n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ) );
            if( s.is_valid( ) && function_tasks->is_function_task( s ) )
                result = true;
        }

        // Check its children
        ObjectList<Nodecl::NodeclBase> children = n.children( );
        for( ObjectList<Nodecl::NodeclBase>::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
        {
            result = nodecl_calls_outline_task( *it, function_tasks );
        }

        return result;
    }

    static bool ompss_reduction_rhs_uses_lhs( const Nodecl::NodeclBase& n, const Nodecl::NodeclBase& lhs,
                                              RefPtr<OpenMP::FunctionTaskSet> function_tasks )
    {
        if( n.is_null( ) || n.is<Nodecl::ArraySubscript>( ) ||
            ( n.is<Nodecl::FunctionCall>( ) && ( !n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ).is_valid( ) ||
                                                 !function_tasks->is_function_task( n.as<Nodecl::FunctionCall>( ).get_called( ).get_symbol( ) ) ) ) )
            return false;

        // Check the current node
        if( Nodecl::Utils::structurally_equal_nodecls( n, lhs, /*skip conversion nodes*/ true ) )
            return true;

        // Check the children
        bool result = false;
        ObjectList<Nodecl::NodeclBase> children = n.children( );
        for( ObjectList<Nodecl::NodeclBase>::iterator it = children.begin( ); it != children.end( ) && !result; ++it )
        {
            result = ompss_reduction_rhs_uses_lhs( *it, lhs, function_tasks );
        }
        return result;
    }

    bool AnalysisStaticInfo::is_ompss_reduction( const Nodecl::NodeclBase& n, RefPtr<OpenMP::FunctionTaskSet> function_tasks ) const
    {
        bool result = false;

        if( n.is<Nodecl::Assignment>( ) ||
            n.is<Nodecl::AddAssignment>( ) || n.is<Nodecl::MinusAssignment>( ) ||
            n.is<Nodecl::DivAssignment>( ) || n.is<Nodecl::MulAssignment>( ) || n.is<Nodecl::ModAssignment>( ) ||
            n.is<Nodecl::BitwiseShlAssignment>( ) || n.is<Nodecl::BitwiseShrAssignment>( ) || n.is<Nodecl::ArithmeticShrAssignment>( ) ||
            n.is<Nodecl::BitwiseAndAssignment>( ) || n.is<Nodecl::BitwiseOrAssignment>( ) || n.is<Nodecl::BitwiseXorAssignment>( ) )
        {
            Nodecl::Assignment n_assig = n.as<Nodecl::Assignment>( );
            result = nodecl_calls_outline_task( n_assig.get_rhs( ), function_tasks );

            if( result && n.is<Nodecl::Assignment>( ) )
            {   // Check also if the LHS also contains the RHS
                Nodecl::NodeclBase rhs_c = n_assig.get_rhs( ).shallow_copy( );
                Optimizations::ReduceExpressionVisitor rev;
                rev.walk( rhs_c );
                if( !ompss_reduction_rhs_uses_lhs( rhs_c, n_assig.get_lhs( ), function_tasks ) )
                    result = false;
            }
        }

        return result;
    }

    bool AnalysisStaticInfo::is_induction_variable_dependent_expression( const Nodecl::NodeclBase& ivs_scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator ivs_scope_static_info = _static_info_map.find( ivs_scope );
        if( ivs_scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether expression '%s' is induction variable dependent.'",
                             ivs_scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = ivs_scope_static_info->second;
            Node* scope_node = current_info.find_node_from_nodecl_pointer( ivs_scope );
            if( scope_node == NULL )
                WARNING_MESSAGE( "No PCFG node found in the static info computed for ivs_scope nodecl %s.",
                                 ivs_scope.prettyprint( ).c_str( ) );

            result = current_info.is_induction_variable_dependent_expression( n, scope_node );
        }

        return result;
    }

    bool AnalysisStaticInfo::is_constant_access( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const
    {
        bool result = false;

        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot resolve whether the accesses to '%s' are constant.'",
                             scope.prettyprint( ).c_str( ), n.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            result = current_info.is_constant_access( n );
        }

        return result;
    }

    void AnalysisStaticInfo::print_auto_scoping_results( const Nodecl::NodeclBase& scope )
    {
        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot print its auto-scoping results.'",
                             scope.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            current_info.print_auto_scoping_results( );
        }
    }

    Utils::AutoScopedVariables AnalysisStaticInfo::get_auto_scoped_variables( const Nodecl::NodeclBase scope )
    {
        Utils::AutoScopedVariables res;
        static_info_map_t::const_iterator scope_static_info = _static_info_map.find( scope );
        if( scope_static_info == _static_info_map.end( ) )
        {
            WARNING_MESSAGE( "Nodecl '%s' is not contained in the current analysis. "\
                             "Cannot print its auto-scoping results.'",
                             scope.prettyprint( ).c_str( ) );
        }
        else
        {
            NodeclStaticInfo current_info = scope_static_info->second;
            res = current_info.get_auto_scoped_variables( );
        }
        return res;
    }

    // ************************** END User interface for static analysis *************************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************* Visitor retrieving the analysis of a given Nodecl ********************* //

    NestedBlocksStaticInfoVisitor::NestedBlocksStaticInfoVisitor( WhichAnalysis analysis_mask,
                                                                  WhereAnalysis nested_analysis_mask,
                                                                  PCFGAnalysis_memento state,
                                                                  int nesting_level)
            : _state( state ), _analysis_mask( analysis_mask ), _nested_analysis_mask( nested_analysis_mask ),
              _nesting_level( nesting_level ), _current_level( 0 ),  _analysis_info( )
    {}

    void NestedBlocksStaticInfoVisitor::join_list( ObjectList<static_info_map_t>& list )
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
        ObjectList<Analysis::Utils::InductionVariableData*> induction_variables;
        ObjectList<Symbol> reductions;
        Utils::ext_sym_set killed;
        ObjectList<ExtensibleGraph*> pcfgs;
        Node* autoscoped_task = NULL;
        if( _analysis_mask._which_analysis & WhichAnalysis::INDUCTION_VARS_ANALYSIS )
        {
            induction_variables = _state.get_induction_variables( n );
            reductions = _state.get_reductions( n );
        }
        if( _analysis_mask._which_analysis & WhichAnalysis::CONSTANTS_ANALYSIS )
        {
            killed = _state.get_killed( n );
            pcfgs = _state.get_pcfgs( );
        }
        if( ( _analysis_mask._which_analysis & WhichAnalysis::AUTO_SCOPING )
            && n.is<Nodecl::OpenMP::Task>( ) )
        {
            autoscoped_task = _state.get_autoscoped_task( n );
        }

        NodeclStaticInfo static_info( induction_variables, reductions, killed, pcfgs, autoscoped_task );
        _analysis_info.insert( static_info_pair_t( n, static_info ) );
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::DoStatement& n )
    {
        if( _nested_analysis_mask._where_analysis & WhereAnalysis::NESTED_DO_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level )
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_statement( ) );
            }
        }
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::ForStatement& n )
    {
        if( _nested_analysis_mask._where_analysis & WhereAnalysis::NESTED_FOR_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level )
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_statement( ) );
            }
        }
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::FunctionCode& n )
    {   // Assume that functions are always wanted to be parsed
        // Otherwise, this code should not be called...
        _current_level++;
        if( _current_level <= _nesting_level )
        {
            // Current nodecl info
            retrieve_current_node_static_info( n );

            // Nested nodecl info
            walk( n.get_statements( ) );
        }
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::IfElseStatement& n )
    {
        if( _nested_analysis_mask._where_analysis & WhereAnalysis::NESTED_IF_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level )
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodes info
                walk( n.get_then( ) );
                walk( n.get_else( ) );
            }
        }
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::OpenMP::Task& n )
    {
        if( _nested_analysis_mask._where_analysis & WhereAnalysis::NESTED_OPENMP_TASK_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level )
            {
                // Current nodecl info
                retrieve_current_node_static_info( n );

                // Nested nodecl info
                walk( n.get_statements( ) );
            }
        }
    }

    void NestedBlocksStaticInfoVisitor::visit( const Nodecl::WhileStatement& n )
    {
        if( _nested_analysis_mask._where_analysis & WhereAnalysis::NESTED_WHILE_STATIC_INFO )
        {
            _current_level++;
            if( _current_level <= _nesting_level )
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
