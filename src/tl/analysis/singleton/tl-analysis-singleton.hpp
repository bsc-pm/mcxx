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

#ifndef TL_ANALYSIS_SINGLETON_HPP
#define TL_ANALYSIS_SINGLETON_HPP

#include <map>

#include "tl-constants-analysis-phase.hpp"
#include "tl-compilerphase.hpp"
#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    struct Analysis_st {
        bool _constants;    //!True when constant propagation and constant folding have been applied
        bool _canonical;    //!True when expressions canonicalization has been applied
        bool _use_def;      //!True when use-definition chains have been calculated
        bool _liveness;     //!True when liveness analysis has been applied

        Analysis_st( )
            : _constants( false ), _canonical( false ), _use_def( false ), _liveness( false )
        {}

        ~Analysis_st( )
        {
            _constants = false;
            _canonical = false;
            _use_def = false;
            _liveness = false;
        }
    };

    typedef std::map<std::string, Analysis_st> Analysis_state_map;
    typedef std::map<std::string, ExtensibleGraph*> Analysis_pcfg_map;

    //! Thsi class implements a set of analysis and optimizations
    class LIBTL_CLASS AnalysisSingleton : public ConstantsAnalysisPhase
    {
    private:

        // ************************************************************************** //
        // *************************** Private attributes *************************** //

        static AnalysisSingleton* _analysis;

        // Translation Unit used when analysis are called as phases
        DTO& _dto;

        // Variables used when one PCFGs are created mantaining called functions
        Analysis_pcfg_map _non_inlined_pcfgs;   // Set of ipa PCFGs created.
        Analysis_state_map _non_inlined_states; // State of each ipa PCFG analysis

        // Variables used when PCFGS are created inlining PCFGs of called functions
        Analysis_pcfg_map _inlined_pcfgs;   // Set of ipa PCFGs created.
        Analysis_state_map _inlined_states; // State of each ipa PCFG analysis

        // ************************* End private attributes ************************* //
        // ************************************************************************** //



        // ************************************************************************** //
        // **************************** Private methods ***************************** //

        //!Private constructor. Prevent calling constructor.
        AnalysisSingleton( TL::DTO& dto );

        //!Not implemented method. Prevent copy constructor.
        AnalysisSingleton( const AnalysisSingleton& );

        //!Not implemented method. Prevent assignment.
        void operator=( const AnalysisSingleton& );

        // ************************** End private methods *************************** //
        // ************************************************************************** //

    public:

        // ************************************************************************** //
        // *************************** Singleton methods **************************** //

        //!Destructor
        ~AnalysisSingleton( );

        //!Single instance constructor
        static AnalysisSingleton* get_analysis( TL::DTO& dto );

        // ************************* End singleton methods ************************** //
        // ************************************************************************** //



        // ************************************************************************** //
        // **************************** Analysis methods **************************** //

        //!This method creates the PCFGs corresponding to the code in \_dto
        void parallel_control_flow_graph( );

        /*!This analysis creates a Parallel Control Flow Graph from \ast
         * \param ast Tree containing the code to construct the PCFG(s)
         * \param ipa Boolean indicating whether Function Call nodes must be substituted by the called functions code
         * \return A list with the created PCFG(s)
         */
        ObjectList<ExtensibleGraph*> parallel_control_flow_graph( Nodecl::NodeclBase ast, bool inline_pcfg );

        /*!This optimization performs Conditional Constant Propagation (CCP) over \pcfg
         * This optimization is an extension of the Constant Propagation and Constant Folding algorithm
         * that takes conditional branches into account applying Unreachable Code Elimination.
         * @param pcfg PCFG where applying Constant Propagation Optimization
         * @param ipa Boolan indicating whether this analysis is Inter-procedural or not
         *            It is 'false' by default. Value 'true' is allowed only in case the ast contains a C/C++ main function.
         */
        void conditional_constant_propagation( ExtensibleGraph* pcfg, bool ipa = false );

        //!This overloaded method applies Conditional Constant propagation as a phase over the \_dto
        void conditional_constant_propagation( );

        void expression_canonicalization( Nodecl::NodeclBase ast );

        void use_def( Nodecl::NodeclBase ast );

        void liveness( Nodecl::NodeclBase ast );

        void induction_variables( Nodecl::NodeclBase ast );

        void print_pcfg( ExtensibleGraph* graph );

        // ************************** End analysis methods ************************** //
        // ************************************************************************** //



        // ************************************************************************** //
        // *************************** Getters and setters ************************** //

        ObjectList<ExtensibleGraph*> get_inlined_pcfgs( );

        ObjectList<ExtensibleGraph*> get_non_inlined_pcfgs( );

        // ************************* End getters and setters ************************ //
        // ************************************************************************** //
    };

}
}

#endif      // TL_ANALYSIS_SINGLETON_HPP