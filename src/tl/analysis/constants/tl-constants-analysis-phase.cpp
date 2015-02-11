/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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


#include "cxx-utils.h"
#include "tl-analysis-utils.hpp"
#include "tl-constants-analysis.hpp"
#include "tl-constants-analysis-phase.hpp"
#include "tl-pcfg-visitor.hpp"

namespace TL {
namespace Analysis {

    ConstantsAnalysisPhase::ConstantsAnalysisPhase( )
    {
        set_phase_name("Experimental phase for constants analysis");
        set_phase_description("This phase builds a unique Parallel Control Flow Graph \
                                and performs Constant Propagation and Constant Folding. ");
    }

    void ConstantsAnalysisPhase::run( TL::DTO& dto )
    {
        if ( VERBOSE )
            std::cerr << std::endl << "=== Conditional Costant Propagation Phase ===" << std::endl;

        std::shared_ptr<Nodecl::NodeclBase> ast = std::static_pointer_cast<Nodecl::NodeclBase>( dto["nodecl"] );
        Nodecl::NodeclBase main_func = Utils::find_main_function( *ast );

        if ( VERBOSE )
            std::cerr << std::endl << "Building PCFGs of the program unit" << std::endl;
        ObjectList<ExtensibleGraph*> pcfgs;

        if ( main_func.is_null( ) )
        {
            //!Build a PCFG for each method in \ast
            if ( VERBOSE )
                std::cerr << std::endl << "Building one PCFG for each method in the program unit" << std::endl;

            Utils::TopLevelVisitor tlv;
            tlv.walk_functions( main_func );

            ObjectList<Nodecl::NodeclBase> functions = tlv.get_functions( );
            for( ObjectList<Nodecl::NodeclBase>::iterator it = functions.begin( ); it != functions.end( ); ++it )
            {
                PCFGVisitor pcfg_visit( Utils::generate_hashed_name( *it ), *it );
                pcfgs.append( pcfg_visit.parallel_control_flow_graph( *it ) );
            }

            //!Apply Conditional Constant Propagation
            if ( VERBOSE )
                std::cerr << std::endl << "Applying Conditional Constant propagation to each PCFG just created" << std::endl;

            ConditionalConstantAnalysis cca( /* ipa */ false );
            cca.conditional_constant_propagation( pcfgs );
        }
        else
        {
            /*!Build a unique PCFG for the whole program unit
             * Only supported for C / C++ ( Fortran requires more efforts since there is no function called 'main' )
             */
            if ( VERBOSE )
                std::cerr << std::endl << "Building a unique PCFG for the program unit" << std::endl;

            PCFGVisitor pcfg_visit( Utils::generate_hashed_name( main_func ), main_func );
            pcfgs.append( pcfg_visit.parallel_control_flow_graph( main_func ) );

            //!Apply Conditional Constant Propagation
            if ( VERBOSE )
                std::cerr << std::endl << "Applying Conditional Constant propagation to the PCFG just created" << std::endl;

            ConditionalConstantAnalysis cca( /* ipa */ true );
            cca.conditional_constant_propagation( pcfgs );
        }
    }

}
}

EXPORT_PHASE(TL::Analysis::ConstantsAnalysisPhase);
