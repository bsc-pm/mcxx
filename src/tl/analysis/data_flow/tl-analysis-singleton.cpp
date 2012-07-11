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


#include <cassert>

#include "tl-analysis-singleton.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-cfg-constants-analysis.hpp"
#include "tl-cfg-visitor.hpp"

namespace TL {
namespace Analysis {
    // Singleton object
    AnalysisSingleton* AnalysisSingleton::_analysis = NULL;
    
    // Private constructor
    AnalysisSingleton::AnalysisSingleton( )
        : _last_pcfg_id(0), _pcfgs( ), _states( )
    {}

    // Single instance constructor
    AnalysisSingleton* AnalysisSingleton::get_analysis()
    {
        if ( !_analysis )
            _analysis = new AnalysisSingleton( );
        return _analysis;
    }
    
    // Public destructor
    AnalysisSingleton::~AnalysisSingleton( )
    { 
        _pcfgs.clear( );
        _states.clear( );
        delete _analysis;
    }
    
    void AnalysisSingleton::parallel_control_flow_graph( Nodecl::NodeclBase ast )
    {
        std::string pcfg_name = Utils::generate_hashed_name( ast );
        if ( _pcfgs.find( pcfg_name ) != _pcfgs.end( ) )
        {   // Only create the PCFG if it has not been created previously
            return;
        }
        
        // Create the PCFG
        ExtensibleGraph* pcfg = new ExtensibleGraph( pcfg_name, ast.retrieve_context( ) );
        PCFGVisitor v( pcfg );
        v.build_pcfg( ast );
        
        // Add the graph to the member maps
        _pcfgs[pcfg_name] = pcfg;
        analysis_st empty_analysis_state;
        _states[pcfg_name] = empty_analysis_state;
    }
    
    void AnalysisSingleton::conditional_constant_propagation( Nodecl::NodeclBase ast )
    {
        std::string pcfg_name = Utils::generate_hashed_name( ast );
        if ( _pcfgs.find( pcfg_name ) == _pcfgs.end( ) )
        {   // Create the PCFG if it has not been created previously
            parallel_control_flow_graph( ast );
        }
        
        assert( _pcfgs.find( pcfg_name ) != _pcfgs.end( ) );
        ConstantsAnalysis ca;
        ca.conditional_constant_propagation( _pcfgs[pcfg_name] );
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
    
    ObjectList<ExtensibleGraph*> AnalysisSingleton::get_pcfgs( )
    {
        ObjectList<ExtensibleGraph*> pcfgs;
        for (analysis_pcfg_map::iterator it = _pcfgs.begin( ); it != _pcfgs.end( ); ++it)
        {
            pcfgs.append( it->second );
        }
        return pcfgs;
    }
}
}
