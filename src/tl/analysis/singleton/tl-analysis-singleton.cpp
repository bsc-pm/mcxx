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
#include "tl-iv-analysis.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    // ************************************************************************ //
    // ************************ Analysis Memento class ************************ //

    PCFGAnalysis_memento::PCFGAnalysis_memento( )
        : _pcfgs( ), _constants( false ), _canonical( false ), _use_def( false ), _liveness( ),
          _loops( false ), _reaching_defs( false ), _auto_scoping( false ), _auto_deps( false )
    {}

    ExtensibleGraph* PCFGAnalysis_memento::get_pcfg( std::string name )
    {
        ExtensibleGraph* pcfg = NULL;
        Name_to_pcfg_map::iterator pcfgs_it = _pcfgs.find( name );

        if( pcfgs_it != _pcfgs.end( ) )
            pcfg = _pcfgs[name];

        return pcfg;
    }

    void PCFGAnalysis_memento::set_pcfg( std::string name, ExtensibleGraph* pcfg )
    {
        _pcfgs[name] = pcfg;
    }

    bool PCFGAnalysis_memento::is_constants_computed( ) const
    {
        return _constants;
    }

    void PCFGAnalysis_memento::set_constants_computed( )
    {
        _constants = true;
    }

    bool PCFGAnalysis_memento::is_canonical_computed( ) const
    {
        return _canonical;
    }

    void PCFGAnalysis_memento::set_canonical_computed( )
    {
        _canonical = true;
    }

    bool PCFGAnalysis_memento::is_usage_computed( ) const
    {
        return _use_def;
    }

    void PCFGAnalysis_memento::set_usage_computed( )
    {
        _use_def = true;
    }

    bool PCFGAnalysis_memento::is_liveness_computed( ) const
    {
        return _liveness;
    }

    void PCFGAnalysis_memento::set_liveness_computed( )
    {
        _liveness = true;
    }

    bool PCFGAnalysis_memento::is_loops_computed( ) const
    {
        return _loops;
    }

    void PCFGAnalysis_memento::set_loops_computed( )
    {
        _loops = true;
    }

    bool PCFGAnalysis_memento::is_reaching_defs_computed( ) const
    {
        return _reaching_defs;
    }

    void PCFGAnalysis_memento::set_reaching_defs_computed( )
    {
        _reaching_defs = true;
    }

    bool PCFGAnalysis_memento::is_auto_scoping_computed( ) const
    {
        return _auto_scoping;
    }

    void PCFGAnalysis_memento::set_auto_scoping_computed( )
    {
        _auto_scoping = true;
    }

    bool PCFGAnalysis_memento::is_auto_deps_computed( ) const
    {
        return _auto_deps;
    }

    void PCFGAnalysis_memento::set_auto_deps_computed( )
    {
        _auto_deps = true;
    }

    // TODO
    bool is_constant( Nodecl::NodeclBase loop, Utils::ExtendedSymbol s )
    {
        bool result = false;

        return result;
    }

    // TODO
    ObjectList<Utils::ExtendedSymbol> get_induction_variables( Nodecl::NodeclBase loop )
    {
        return ObjectList<Utils::ExtendedSymbol>( );
    }

    // TODO
    bool is_induction_variable( Nodecl::NodeclBase loop, Utils::ExtendedSymbol s )
    {
        bool result = false;

        return result;
    }

    // TODO
    bool is_stride_one( Nodecl::NodeclBase loop, Utils::ExtendedSymbol s )
    {
        bool result = false;

        return result;
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
    {}

    // Single instance constructor
    AnalysisSingleton& AnalysisSingleton::get_analysis( )
    {
        static AnalysisSingleton analysis;
        return analysis;
    }

    ObjectList<ExtensibleGraph*> AnalysisSingleton::parallel_control_flow_graph( PCFGAnalysis_memento& memento,
                                                                                 Nodecl::NodeclBase ast )
    {
        ObjectList<ExtensibleGraph*> result;
        ObjectList<Nodecl::NodeclBase> unique_asts;

        // Get all unique ASTs embedded in 'ast'
        if( !ast.is<Nodecl::TopLevel>( ) )
        {
            unique_asts.append( ast );
        }
        else
        {
            // Get all functions in \ast
            Utils::TopLevelVisitor tlv;
            tlv.walk_functions( ast );
            unique_asts = tlv.get_functions( );
        }

        // Compute the PCFG corresponding to each AST
        for( ObjectList<Nodecl::NodeclBase>::iterator it = unique_asts.begin( ); it != unique_asts.end( ); ++it )
        {
            // Generate the hashed name corresponding to the AST of the function
            std::string pcfg_name = Utils::generate_hashed_name( *it );

            // Create the PCFG only if it has not been created previously
            if( memento.get_pcfg( pcfg_name ) == NULL )
            {
                if( VERBOSE )
                    std::cerr << "- Generating PCFG '" << pcfg_name << "'" << std::endl;

                // Create the PCFG
                    PCFGVisitor v( pcfg_name, it->retrieve_context( ) );
                    ExtensibleGraph* pcfg = v.parallel_control_flow_graph( *it );

                    // Store the pcfg in the singleton
                    memento.set_pcfg( pcfg_name, pcfg );
                    result.append( pcfg );
            }
            else
            {
                result.append( memento.get_pcfg( pcfg_name ) );
            }
        }

        return result;
    }

    void AnalysisSingleton::conditional_constant_propagation( PCFGAnalysis_memento& memento,
                                                              ExtensibleGraph* pcfg, bool ipa )
    {
        //         ConditionalConstantAnalysis ca( ipa );
        //         ca.conditional_constant_propagation( pcfg );
    }

    void AnalysisSingleton::conditional_constant_propagation( )
    {
//         ConstantsAnalysisPhase::run( );
    }

    void AnalysisSingleton::expression_canonicalization( PCFGAnalysis_memento& memento,
                                                         Nodecl::NodeclBase ast )
    {

    }

    void AnalysisSingleton::use_def( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast )
    {
        ObjectList<ExtensibleGraph*> pcfgs = parallel_control_flow_graph( memento, ast );

        if( !memento.is_usage_computed( ) )
        {
            memento.set_usage_computed( );

            for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it )
            {
                if( VERBOSE )
                    std::cerr << "- Use-Definition of PCFG '" << (*it)->get_name( ) << "'" << std::endl;
                UseDef ud( *it );
                ud.compute_usage( );
            }
        }
    }

    void AnalysisSingleton::liveness( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast )
    {

    }

    void AnalysisSingleton::induction_variables( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast )
    {
        ObjectList<ExtensibleGraph*> pcfgs = parallel_control_flow_graph( memento, ast );

        for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it )
        {
            InductionVariableAnalysis iva( *it );
            iva.compute_induction_variables( );
            // TODO
        }
    }

    void AnalysisSingleton::print_pcfg( PCFGAnalysis_memento& memento, std::string pcfg_name )
    {
        ExtensibleGraph* pcfg = memento.get_pcfg( pcfg_name );
        pcfg->print_graph_to_dot( memento.is_usage_computed( ), memento.is_liveness_computed( ),
                                  memento.is_reaching_defs_computed( ),
                                  memento.is_auto_scoping_computed( ), memento.is_auto_deps_computed( ) );
    }
}
}
