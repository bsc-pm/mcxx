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

#include "tl-test-analysis-phase.hpp"
#include "tl-analysis-singleton.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-pcfg-visitor.hpp"

namespace TL {
namespace Analysis {

        TestAnalysisPhase::TestAnalysisPhase( )
        {
            set_phase_name("Experimental phase for testing compiler analysis");
            set_phase_description("This is a temporal phase called with code testing purposes.");
        }

        void TestAnalysisPhase::run( TL::DTO& dto )
        {
            AnalysisSingleton& analysis = AnalysisSingleton::get_analysis( );
            PCFGAnalysis_memento memento;

            Nodecl::NodeclBase ast = dto["nodecl"];

            // Test PCFG creation
            if( VERBOSE )
                std::cerr << "=========  Testing PCFG creation  =========" << std::endl;
            ObjectList<ExtensibleGraph*> pcfgs =
                    analysis.parallel_control_flow_graph( memento, ast );

            if( VERBOSE )
                std::cerr << "=========  Testing Use-Definition analysis =========" << std::endl;
            analysis.use_def( memento, ast );

            if( VERBOSE )
                std::cerr << "=========  Testing Liveness analysis =========" << std::endl;
            analysis.liveness( memento, ast );

//             if( VERBOSE )
//                 std::cerr << "=========  Testing Reaching Definitions analysis =========" << std::endl;
//             analysis.reaching_definitions( memento, ast );
//
//             if( VERBOSE )
//                 std::cerr << "=========  Testing Induction Variables analysis =========" << std::endl;
//             analysis.induction_variables( memento, ast );

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
}
}

EXPORT_PHASE(TL::Analysis::TestAnalysisPhase);
