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



#ifndef TL_ANALYSIS_CHECK_PHASE_HPP
#define TL_ANALYSIS_CHECK_PHASE_HPP

#include "tl-analysis-base.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-pragmasupport.hpp"

namespace TL {
namespace Analysis {

    //! Phase checks the correctness of analysis
    class LIBTL_CLASS AnalysisCheckPhase : public PragmaCustomCompilerPhase
    {
    private:
        WhichAnalysis _analysis_mask;
        std::string _correctness_log_path;

        void check_pragma_clauses(
            PragmaCustomLine pragma_line, const locus_t* loc,
            Nodecl::List& environment);

    public:
        //! Constructor of this phase
        AnalysisCheckPhase( );

        void assert_handler_pre( TL::PragmaCustomStatement directive );
        void assert_handler_post( TL::PragmaCustomStatement directive );
        void assert_decl_handler_pre( TL::PragmaCustomDeclaration directive );
        void assert_decl_handler_post( TL::PragmaCustomDeclaration directive );
        
        //! Private checking methods
        void check_pcfg_consistency( ExtensibleGraph* graph );
        void check_analysis_assertions( ExtensibleGraph* graph );
        
        //! Members to check the programming model being used
        std::string _ompss_mode_str;
        bool _ompss_mode_enabled;
        void set_ompss_mode( const std::string& ompss_mode_str);
        
        //!Entry point of the phase
        virtual void run( TL::DTO& dto );
        
        virtual ~AnalysisCheckPhase( ) { }
    };
    
    //! Visitor to delete Nodecl::Analysis nodes
    class LIBTL_CLASS AnalysisCheckVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        
    public:
        Ret visit( const Nodecl::Analysis::Assert& n );
        Ret visit( const Nodecl::Analysis::AssertDecl& n );
    };
}
}

#endif  // TL_ANALYSIS_CHECK_PHASE_HPP
