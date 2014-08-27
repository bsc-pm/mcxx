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

#include "tl-test-analysis-phase.hpp"
#include "tl-analysis-singleton.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-pcfg-visitor.hpp"

namespace TL {
namespace Analysis {

    TestAnalysisPhase::TestAnalysisPhase( )
        : _pcfg_enabled( false ), _use_def_enabled( false ), _liveness_enabled( false ), 
          _reaching_defs_enabled( false ), _induction_vars_enabled( false ), 
          _task_sync_tune_enabled( false ), _tdg_enabled( false ), _range_analysis_enabled( false )
    {
        set_phase_name("Experimental phase for testing compiler analysis");
        set_phase_description("This is a temporal phase called with code testing purposes.");

        register_parameter( "pcfg_enabled",
                            "If set to '1' enables pcfg analysis, otherwise it is disabled",
                            _pcfg_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_pcfg, *this ) );

        register_parameter( "use_def_enabled",
                            "If set to '1' enables pcfg analysis, otherwise it is disabled",
                            _use_def_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_use_def, *this ) );

        register_parameter( "liveness_enabled",
                            "If set to '1' enables pcfg analysis, otherwise it is disabled",
                            _liveness_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_liveness, *this ) );

        register_parameter( "reaching_defs_enabled",
                            "If set to '1' enables pcfg analysis, otherwise it is disabled",
                            _reaching_defs_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_reaching_defs, *this ) );

        register_parameter( "induction_vars_enabled",
                            "If set to '1' enables pcfg analysis, otherwise it is disabled",
                            _induction_vars_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_induction_vars, *this ) );
                            
        register_parameter( "task_sync_tune_enabled", 
                            "If set to '1' enables task synchronizations tunning in the PCFG", 
                            _task_sync_tune_enabled_str, 
                            "0" ).connect( functor( &TestAnalysisPhase::set_task_sync_tune, *this ) );
                            
        register_parameter( "tdg_enabled",
                            "If set to '1' enables tdg analysis, otherwise it is disabled",
                            _tdg_enabled_str,
                            "0" ).connect( functor( &TestAnalysisPhase::set_tdg, *this ) );

        register_parameter("range_analysis_enabled",
                            "If set to '1' enables range analysis, otherwise it is disabled",
                            _range_analysis_enabled_str,
                            "0").connect(functor(&TestAnalysisPhase::set_range_analsysis, *this));
                            
        register_parameter("cyclomatic_complexity_enabled",
                            "If set to '1' enables cyclomatic complexity calculation, otherwise it is disabled",
                            _cyclomatic_complexity_enabled_str,
                            "0").connect(functor(&TestAnalysisPhase::set_cyclomatic_complexity, *this));
                            
        register_parameter("ompss_mode",
                            "Enables OmpSs semantics instead of OpenMP semantics",
                            _ompss_mode_str,
                            "0").connect(functor(&TestAnalysisPhase::set_ompss_mode, *this));
        
    }

    void TestAnalysisPhase::run( TL::DTO& dto )
    {
        AnalysisSingleton& analysis = AnalysisSingleton::get_analysis(_ompss_mode_enabled);
        PCFGAnalysis_memento memento;

        Nodecl::NodeclBase ast = dto["nodecl"];

        // Test PCFG creation
        if( _pcfg_enabled )
        {
            if( VERBOSE )
                std::cerr << "====================  Testing PCFG creation  =================" << std::endl;
            analysis.parallel_control_flow_graph(memento, ast);
            if( VERBOSE )
                std::cerr << "=================  Testing PCFG creation done  ===============" << std::endl;
        }

        if( _use_def_enabled )
        {
            if( VERBOSE )
                std::cerr << "==============  Testing Use-Definition analysis  ==============" << std::endl;
            analysis.use_def(memento, ast);
            if( VERBOSE )
                std::cerr << "============  Testing Use-Definition analysis done  ===========" << std::endl;
        }

        if( _liveness_enabled )
        {
            if( VERBOSE )
                std::cerr << "=================  Testing Liveness analysis  ==================" << std::endl;
            analysis.liveness( memento, ast);
            if( VERBOSE )
                std::cerr << "===============  Testing Liveness analysis done  ===============" << std::endl;
        }

        if( _reaching_defs_enabled )
        {
            if( VERBOSE )
                std::cerr << "===========  Testing Reaching Definitions analysis  ============" << std::endl;
            analysis.reaching_definitions( memento, ast);
            if( VERBOSE )
                std::cerr << "=========  Testing Reaching Definitions analysis done  =========" << std::endl;
        }

        if( _induction_vars_enabled )
        {
            if( VERBOSE )
                std::cerr << "=============  Testing Induction Variables analysis  ==========" << std::endl;
            analysis.induction_variables( memento, ast);
            if( VERBOSE )
                std::cerr << "==========  Testing Induction Variables analysis done  ========" << std::endl;
        }
        
        if( _task_sync_tune_enabled )
        {
            if( VERBOSE )
                std::cerr << "============  Testing Tasks synchronization tunning  ===========" << std::endl;
            analysis.tune_task_synchronizations( memento, ast);
            if( VERBOSE )
                std::cerr << "=========  Testing Tasks synchronization tunning done  =========" << std::endl;
        }
        
        if( _range_analysis_enabled )
        {
            if( VERBOSE )
                std::cerr << "====================  Testing Range analysis  ===================" << std::endl;
            analysis.range_analysis( memento, ast);
            if( VERBOSE )
                std::cerr << "==========  Testing Induction Variables analysis done  ==========" << std::endl;
        }
        
        ObjectList<TaskDependencyGraph*> tdgs;
        if( _tdg_enabled )
        {
            if( VERBOSE )
                std::cerr << "====================  Testing TDG creation  ====================" << std::endl;
            tdgs = analysis.task_dependency_graph(memento, ast);
            if( VERBOSE )
                std::cerr << "==================  Testing TDG creation done  =================" << std::endl;
        }
        
        if(_cyclomatic_complexity_enabled)
        {
            if( VERBOSE )
                std::cerr << "============  Testing Cyclomatic Complexity analysis  ===========" << std::endl;
            analysis.cyclomatic_complexity(memento, ast);
            if( VERBOSE )
                std::cerr << "=========  Testing Cyclomatic Complexity analysis done  =========" << std::endl;
        }
        
        if( CURRENT_CONFIGURATION->debug_options.print_pcfg || 
            CURRENT_CONFIGURATION->debug_options.print_pcfg_w_context)
        {
            if( VERBOSE )
                std::cerr << "=================  Printing PCFG to dot file  ==================" << std::endl;
            ObjectList<ExtensibleGraph*> pcfgs = memento.get_pcfgs();
            for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it )
                analysis.print_pcfg( memento, (*it)->get_name( ) );
            if( VERBOSE )
                std::cerr << "===============  Printing PCFG to dot file done  ===============" << std::endl;
        }
        
        if( CURRENT_CONFIGURATION->debug_options.print_tdg )
        {
            if( VERBOSE )
                std::cerr << "==================  Printing TDG to dot file  =================" << std::endl;
            for( ObjectList<TaskDependencyGraph*>::iterator it = tdgs.begin( ); it != tdgs.end( ); ++it )
                analysis.print_tdg( memento, (*it)->get_name( ) );
            if( VERBOSE )
                std::cerr << "===============  Printing TDG to dot file done  ===============" << std::endl;
        }
        
        if( CURRENT_CONFIGURATION->debug_options.tdg_to_json )
        {
            if( VERBOSE )
                std::cerr << "==================  Printing TDG to json file  ================" << std::endl;
            for( ObjectList<TaskDependencyGraph*>::iterator it = tdgs.begin( ); it != tdgs.end( ); ++it )
                analysis.tdg_to_json( memento, (*it)->get_name( ) );
            if( VERBOSE )
                std::cerr << "===============  Printing TDG to json file done  ==============" << std::endl;
        }
    }

    void TestAnalysisPhase::set_pcfg( const std::string& pcfg_enabled_str )
    {
        if( pcfg_enabled_str == "1" )
            _pcfg_enabled = true;
    }

    void TestAnalysisPhase::set_use_def( const std::string& use_def_enabled_str )
    {
        if( use_def_enabled_str == "1" )
            _use_def_enabled = true;
    }

    void TestAnalysisPhase::set_liveness( const std::string& liveness_enabled_str )
    {
        if( liveness_enabled_str == "1" )
            _liveness_enabled = true;
    }

    void TestAnalysisPhase::set_reaching_defs( const std::string& reaching_defs_enabled_str )
    {
        if( reaching_defs_enabled_str == "1" )
            _reaching_defs_enabled = true;
    }

    void TestAnalysisPhase::set_induction_vars( const std::string& induction_vars_enabled_str )
    {
        if( induction_vars_enabled_str == "1" )
            _induction_vars_enabled = true;
    }
    
    void TestAnalysisPhase::set_task_sync_tune( const std::string& task_sync_tune_enabled_str )
    {
        if( task_sync_tune_enabled_str == "1" )
            _task_sync_tune_enabled = true;
    }
    
    void TestAnalysisPhase::set_tdg( const std::string& tdg_enabled_str )
    {
        if( tdg_enabled_str == "1" )
            _tdg_enabled = true;
    }

    void TestAnalysisPhase::set_range_analsysis( const std::string& range_analysis_enabled_str )
    {
        if( range_analysis_enabled_str == "1" )
            _range_analysis_enabled = true;
    }
    
    void TestAnalysisPhase::set_cyclomatic_complexity( const std::string& cyclomatic_complexity_enabled_str)
    {
        if( cyclomatic_complexity_enabled_str == "1")
            _cyclomatic_complexity_enabled = true;
    }
    
    void TestAnalysisPhase::set_ompss_mode( const std::string& ompss_mode_str)
    {
        if( ompss_mode_str == "1")
            _ompss_mode_enabled = true;
    }
}
}

EXPORT_PHASE(TL::Analysis::TestAnalysisPhase);
