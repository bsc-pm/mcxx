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

            RefPtr<Nodecl::NodeclBase> ref_ast = RefPtr<Nodecl::NodeclBase>::cast_dynamic( dto["nodecl"] );
            Nodecl::NodeclBase ast = *ref_ast;

            // Test PCFG creation
            if ( VERBOSE )
                std::cerr << "Testing PCFG creation" << std::endl;
            ObjectList<ExtensibleGraph*> pcfgs = analysis.parallel_control_flow_graph( ast, /* dress up PCFGs */ true );

            if ( VERBOSE )
                std::cerr << "Printing PCFG to dot file" << std::endl;
            for( ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin( ); it != pcfgs.end( ); ++it)
            {
                analysis.print_pcfg( *it );
            }

            // Test constant propagation and constant folding
//             analysis->conditional_constant_propagation( );



//             RefPtr<Nodecl::NodeclBase> ast = RefPtr<Nodecl::NodeclBase>::cast_dynamic( dto["nodecl"] );
//             // *** Build graphs for every method in the translation unit *** //
//             // *** Tags global variables used within each graph *** //
//             if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
//                 CURRENT_CONFIGURATION->debug_options.enable_debug_code)
//                 std::cerr << std::endl << "=== CFG Construction ===" << std::endl;
//             analysis->parallel_control_flow_graph( *ast );

//             if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
//                 CURRENT_CONFIGURATION->debug_options.enable_debug_code)
//             {
//                 std::cerr << std::endl << "=== GLOBAL VARIABLES USED WITHIN GRAPHS ===" << std::endl;
//                 for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
//                 {
//                     std::cerr << "  ==> Graph '" << it->get_name() << "'" << std::endl;
//                     ObjectList<ExtendedSymbolUsage> glob_vars = (*it)->get_global_variables();
//                     for(ObjectList<ExtendedSymbolUsage>::iterator it_es = glob_vars.begin(); it_es != glob_vars.end(); ++it)
//                     {
//                         std::cerr << "       - " << it_es->get_nodecl().prettyprint() << std::endl;
//                     }
//                 }
//             }
        }
}
}

EXPORT_PHASE(TL::Analysis::TestAnalysisPhase);
