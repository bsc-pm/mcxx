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

    // Singleton object
    AnalysisSingleton* AnalysisSingleton::_analysis = NULL;

    // Private constructor
    AnalysisSingleton::AnalysisSingleton( TL::DTO& dto )
        : _dto( dto ), _non_inlined_pcfgs( ), _non_inlined_states( ), _inlined_pcfgs( ), _inlined_states( )
    {}

    // Single instance constructor
    AnalysisSingleton* AnalysisSingleton::get_analysis( TL::DTO& dto )
    {
        if ( !_analysis )
            _analysis = new AnalysisSingleton( dto );
        return _analysis;
    }

    // Public destructor
    AnalysisSingleton::~AnalysisSingleton( )
    {
        delete _analysis;
        _non_inlined_pcfgs.clear( );
        _non_inlined_states.clear( );
        _inlined_pcfgs.clear( );
        _inlined_states.clear( );
    }

    ObjectList<ExtensibleGraph*> AnalysisSingleton::parallel_control_flow_graph( Nodecl::NodeclBase ast, bool inline_pcfg )
    {
        // Get all functions in \ast
        Utils::TopLevelVisitor tlv;
        tlv.walk_functions( ast );

        ObjectList<Nodecl::NodeclBase> functions = tlv.get_functions( );
        ObjectList<ExtensibleGraph*> pcfgs;

        for( ObjectList<Nodecl::NodeclBase>::iterator it = functions.begin( ); it != functions.end( ); ++it )
        {
            std::string pcfg_name = Utils::generate_hashed_name( *it );

            // Only create the PCFG if it has not been created previously
            if( ( inline_pcfg && _inlined_pcfgs.find( pcfg_name ) == _inlined_pcfgs.end( ) )
                || ( !inline_pcfg && _non_inlined_pcfgs.find( pcfg_name ) == _non_inlined_pcfgs.end( ) ) )
            {
                // Create the PCFG
                PCFGVisitor v( pcfg_name, it->retrieve_context( ), inline_pcfg );
                ExtensibleGraph* pcfg = v.build_pcfg( ast );

                // Store the pcfg in the proper member depending on the analysis, whether it is IPA or not
                Analysis_st empty_analysis_state;
                if( inline_pcfg )
                {
                    _inlined_pcfgs[pcfg_name] = pcfg;
                    _inlined_states[pcfg_name] = empty_analysis_state;
                }
                else
                {   // Add the graph to the member maps
                    _non_inlined_pcfgs[pcfg_name] = pcfg;
                    _non_inlined_states[pcfg_name] = empty_analysis_state;
                }
            }
        }
    }

    void AnalysisSingleton::conditional_constant_propagation( )
    {
        ConstantsAnalysisPhase::run( _dto );
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

}
}
