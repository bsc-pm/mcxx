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
              _auto_scope_enabled( false )
        {
            set_phase_name("Experimental phase for testing compiler analysis");
            set_phase_description("This is a temporal phase called with code testing purposes.");

            register_parameter("pcfg_enabled",
                               "If set to '1' enables pcfg analysis, otherwise it is disabled",
                               _pcfg_enabled_str,
                               "0").connect(functor(&TestAnalysisPhase::set_pcfg, *this));

            register_parameter("use_def_enabled",
                               "If set to '1' enables pcfg analysis, otherwise it is disabled",
                               _use_def_enabled_str,
                               "0").connect(functor(&TestAnalysisPhase::set_use_def, *this));

            register_parameter("liveness_enabled",
                               "If set to '1' enables pcfg analysis, otherwise it is disabled",
                               _liveness_enabled_str,
                               "0").connect(functor(&TestAnalysisPhase::set_liveness, *this));

            register_parameter("reaching_defs_enabled",
                               "If set to '1' enables pcfg analysis, otherwise it is disabled",
                               _reaching_defs_enabled_str,
                               "0").connect(functor(&TestAnalysisPhase::set_reaching_defs, *this));

            register_parameter("induction_vars_enabled",
                               "If set to '1' enables pcfg analysis, otherwise it is disabled",
                               _induction_vars_enabled_str,
                               "0").connect(functor(&TestAnalysisPhase::set_induction_vars, *this));
        }

        void TestAnalysisPhase::run( TL::DTO& dto )
        {
            AnalysisSingleton& analysis = AnalysisSingleton::get_analysis( );
            PCFGAnalysis_memento memento;

            Nodecl::NodeclBase ast = dto["nodecl"];

            ObjectList<ExtensibleGraph*> pcfgs;

            // Test PCFG creation
            if( _pcfg_enabled )
            {
                if( VERBOSE )
                    std::cerr << "=========  Testing PCFG creation  =========" << std::endl;
                pcfgs = analysis.parallel_control_flow_graph( memento, ast );
            }

            if( _use_def_enabled )
            {
                if( VERBOSE )
                    std::cerr << "=========  Testing Use-Definition analysis =========" << std::endl;
                pcfgs = analysis.use_def( memento, ast );
            }

            if( _liveness_enabled )
            {
                if( VERBOSE )
                    std::cerr << "=========  Testing Liveness analysis =========" << std::endl;
                pcfgs = analysis.liveness( memento, ast );
            }

            if( _reaching_defs_enabled )
            {
                if( VERBOSE )
                    std::cerr << "=========  Testing Reaching Definitions analysis =========" << std::endl;
                pcfgs = analysis.reaching_definitions( memento, ast );
            }

            if( _induction_vars_enabled )
            {
                if( VERBOSE )
                    std::cerr << "=========  Testing Induction Variables analysis =========" << std::endl;
                pcfgs = analysis.induction_variables( memento, ast );
            }

            // if (_task_sync_enabled)
            {
                // if( VERBOSE )
                    std::cerr << "========= Task Sync analysis =========" << std::endl;
                pcfgs = analysis.task_sync( memento, ast );
            }

            if( CURRENT_CONFIGURATION->debug_options.print_pcfg )
            {
                if( VERBOSE )
                    std::cerr << "=========  Printing PCFG to dot file  =========" << std::endl;
                for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it)
                {
                    analysis.print_pcfg( memento, (*it)->get_name( ) );
                }
            }
        }

        void TestAnalysisPhase::set_pcfg( const std::string pcfg_enabled_str )
        {
            if( pcfg_enabled_str == "1" )
                _pcfg_enabled = true;
        }

        void TestAnalysisPhase::set_use_def( const std::string use_def_enabled_str )
        {
            if( use_def_enabled_str == "1" )
                _use_def_enabled = true;
        }

        void TestAnalysisPhase::set_liveness( const std::string liveness_enabled_str )
        {
            if( liveness_enabled_str == "1" )
                _liveness_enabled = true;
        }

        void TestAnalysisPhase::set_reaching_defs( const std::string reaching_defs_enabled_str )
        {
            if( reaching_defs_enabled_str == "1" )
                _reaching_defs_enabled = true;
        }

        void TestAnalysisPhase::set_induction_vars( const std::string induction_vars_enabled_str )
        {
            if( induction_vars_enabled_str == "1" )
                _induction_vars_enabled = true;
        }

        void TestAnalysisPhase::set_auto_scope( const std::string auto_scope_enabled_str )
        {
            if( auto_scope_enabled_str == "1" )
                _auto_scope_enabled = true;
        }
}
}

EXPORT_PHASE(TL::Analysis::TestAnalysisPhase);
