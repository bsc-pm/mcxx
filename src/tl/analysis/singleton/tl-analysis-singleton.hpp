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

// Set of classes implementing the Memento Pattern with Analysis purposes.
// ----------------         -------------        ----------------
// |  Care Taker  |<>------>|  Memento  |<-------|  Originator  |
// ----------------         |  -------  |        |  ----------  |
//                          | get_state |        |  set_memento |
//                          | set_state |        |  get_memento |
//                          -------------        ----------------
// - PCFGAnalysis_memento is the Memento class
// - AnalysisSingleton is a Singleton class implementing the Originator
// - Each client will be the Care Taker

namespace TL {
namespace Analysis {

    //! Memento class capturing the internal state of the PCFG regarding the analysis
    class PCFGAnalysis_memento {
    private:
        bool _constants;    //!True when constant propagation and constant folding have been applied
        bool _canonical;    //!True when expressions canonicalization has been applied
        bool _use_def;      //!True when use-definition chains have been calculated
        bool _liveness;     //!True when liveness analysis has been applied
        bool _loops;        //!True when loops analysis has been applied

    public:
        //! Class constructor
        PCFGAnalysis_memento( );

        // Getters and Setters
        bool get_constants( );
        void set_constants( );
        bool get_canonical( );
        void set_canonical( );
        bool get_use_def( );
        void set_use_def( );
        bool get_liveness( );
        void set_liveness( );
        bool get_loops( );
        void set_loops( );

        //! Resets the state of the memento
        void reset_state( );

    friend class AnalysisSingleton;
    };

    typedef std::map<std::string, PCFGAnalysis_memento*> Analysis_state_map;
    typedef std::map<std::string, ExtensibleGraph*> Analysis_pcfg_map;

    //! This class implements a Meyers Singleton that includes methods for any kind of analysis
    class LIBTL_CLASS AnalysisSingleton
    {
    private:

        // ************************************************************************** //
        // *************************** Private attributes *************************** //

        static AnalysisSingleton* _analysis;

        Analysis_pcfg_map _pcfgs;
        Analysis_state_map _states;

        // ************************* End private attributes ************************* //
        // ************************************************************************** //



        // ************************************************************************** //
        // **************************** Private methods ***************************** //

        //!Private constructor. Prevents calling construction.
        AnalysisSingleton( );

        //!Prevents copy construction.
        AnalysisSingleton( const AnalysisSingleton& analysis ){};

        //!Prevents assignment.
        void operator=( const AnalysisSingleton& analysis ){};

        //!Prevents destruction
        ~AnalysisSingleton( ){};

        // ************************** End private methods *************************** //
        // ************************************************************************** //

    public:

        // ************************************************************************** //
        // *************************** Singleton methods **************************** //

        //!Single instance constructor
        static AnalysisSingleton& get_analysis( );

        // ************************* End singleton methods ************************** //
        // ************************************************************************** //



        // ************************************************************************** //
        // **************************** Analysis methods **************************** //

        /*!This analysis creates one Parallel Control Flow Graph per each function contained in \ast
         * \param ast Tree containing the code to construct the PCFG(s)
         * \return A pointer to the created PCFG
         */
        ObjectList<ExtensibleGraph*> parallel_control_flow_graph( Nodecl::NodeclBase ast, bool dress_up );

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