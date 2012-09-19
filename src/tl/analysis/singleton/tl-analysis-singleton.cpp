/*--------------------------------------------------------------------
 ( C) Copyright 2006-2012 Barcelona Supercomputing Center             *
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

#include "tl-analysis-singleton.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-constants-analysis.hpp"
#include "tl-pcfg-visitor.hpp"

namespace TL {
namespace Analysis {

    // ************************************************************************ //
    // ************************ Analysis Memento class ************************ //

    PCFGAnalysis_memento::PCFGAnalysis_memento( )
        : _constants( false ), _canonical( false ), _use_def( false ), _liveness( ),
          _loops( false ), _reaching_defs( false ), _auto_scoping( false ), _auto_deps( false )
    {}

    bool PCFGAnalysis_memento::is_constants_computed( )
    {
        return _constants;
    }

    void PCFGAnalysis_memento::set_constants_computed( )
    {
        _constants = true;
    }

    bool PCFGAnalysis_memento::is_canonical_computed( )
    {
        return _canonical;
    }

    void PCFGAnalysis_memento::set_canonical_computed( )
    {
        _canonical = true;
    }

    bool PCFGAnalysis_memento::is_usage_computed( )
    {
        return _use_def;
    }

    void PCFGAnalysis_memento::set_usage_computed( )
    {
        _use_def = true;
    }

    bool PCFGAnalysis_memento::is_liveness_computed( )
    {
        return _liveness;
    }

    void PCFGAnalysis_memento::set_liveness_computed( )
    {
        _liveness = true;
    }

    bool PCFGAnalysis_memento::is_loops_computed( )
    {
        return _loops;
    }

    void PCFGAnalysis_memento::set_loops_computed( )
    {
        _loops = true;
    }

    bool PCFGAnalysis_memento::is_reaching_defs_computed( )
    {
        return _reaching_defs;
    }

    void PCFGAnalysis_memento::set_reaching_defs_computed( )
    {
        _reaching_defs = true;
    }

    bool PCFGAnalysis_memento::is_auto_scoping_computed( )
    {
        return _auto_scoping;
    }

    void PCFGAnalysis_memento::set_auto_scoping_computed( )
    {
        _auto_scoping = true;
    }

    bool PCFGAnalysis_memento::is_auto_deps_computed( )
    {
        return _auto_deps;
    }

    void PCFGAnalysis_memento::set_auto_deps_computed( )
    {
        _auto_deps = true;
    }

    void PCFGAnalysis_memento::reset_state( )
    {
        _constants = false;
        _canonical = false;
        _use_def = false;
        _liveness = false;
        _loops = false;
        _reaching_defs = false;
        _auto_scoping = false;
        _auto_deps = false;
    }

    // ********************** END Analysis Memento class ********************** //
    // ************************************************************************ //



    // ************************************************************************ //
    // *********** Analysis Singleton class ( Memento originator ) ************ //

    // Private constructor
    AnalysisSingleton::AnalysisSingleton( )
        : _pcfgs( ), _states( )
    {}

    // Single instance constructor
    AnalysisSingleton& AnalysisSingleton::get_analysis( )
    {
        static AnalysisSingleton analysis;
        return analysis;
    }

    ObjectList<ExtensibleGraph*> AnalysisSingleton::parallel_control_flow_graph( Nodecl::NodeclBase ast, bool dress_up )
    {
        // Get all functions in \ast
        Utils::TopLevelVisitor tlv;
        tlv.walk_functions( ast );
        ObjectList<Nodecl::NodeclBase> functions = tlv.get_functions( );

        // Create one PCFG per function
        ObjectList<ExtensibleGraph*> result;
        for( ObjectList<Nodecl::NodeclBase>::iterator it = functions.begin( ); it != functions.end( ); ++it )
        {
            // Generate the hashed name corresponding to the AST of the function
            std::string pcfg_name = Utils::generate_hashed_name( *it );

            if( VERBOSE )
                std::cerr << "Generating PCFG '" << pcfg_name << "'" << std::endl;

            // Create the PCFG only if it has not been created previously
            if( _pcfgs.find( pcfg_name ) == _pcfgs.end( ) )
            {
                // Create the PCFG
                PCFGVisitor v( pcfg_name, it->retrieve_context( ) );
                ExtensibleGraph* pcfg = v.parallel_control_flow_graph( *it, dress_up );

                // Store the pcfg in the singleton
                _pcfgs[pcfg_name] = pcfg;
                _states[pcfg_name] = new PCFGAnalysis_memento( );
                result.append( pcfg );
            }
        }

        return result;
    }

    void AnalysisSingleton::conditional_constant_propagation( )
    {
//         ConstantsAnalysisPhase::run( );
    }

    void AnalysisSingleton::conditional_constant_propagation( ExtensibleGraph* pcfg, bool ipa )
    {
        ConditionalConstantAnalysis ca( ipa );
        ca.conditional_constant_propagation( pcfg );
    }

    void AnalysisSingleton::expression_canonicalization( Nodecl::NodeclBase ast )
    {

    }

    void AnalysisSingleton::use_def( Nodecl::NodeclBase ast )
    {

    }

    void AnalysisSingleton::liveness( Nodecl::NodeclBase ast )
    {

    }

    void AnalysisSingleton::induction_variables( Nodecl::NodeclBase ast )
    {

    }

    ObjectList<ExtensibleGraph*> get_ipa_pcfgs( )
    {

    }

    ObjectList<ExtensibleGraph*> get_non_ipa_pcfgs( )
    {

    }

    void AnalysisSingleton::print_pcfg( ExtensibleGraph* graph )
    {
        PCFGAnalysis_memento* current_state = _states[graph->get_name( )];
        graph->print_graph_to_dot( current_state->_use_def, current_state->_liveness, current_state->_reaching_defs,
                                   current_state->_auto_scoping, current_state->_auto_deps );
    }
}
}
