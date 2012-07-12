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

#include "tl-extensible-graph.hpp"

namespace TL {
namespace Analysis {

    struct analysis_st {
        bool _constants;    //!True when constant propagation and constant folding have been applied
        bool _canonical;    //!True when expressions canonicalization has been applied
        bool _use_def;      //!True when use-definition chains have been calculated
        bool _liveness;     //!True when liveness analysis has been applied
        
        analysis_st( )
            : _constants( false ), _canonical( false ), _use_def( false ), _liveness( false )
        {}
        
        ~analysis_st( )
        {
            _constants = false;
            _canonical = false;
            _use_def = false;
            _liveness = false;
        }
    };
    
    typedef std::map<std::string, analysis_st> analysis_state_map;
    typedef std::map<std::string, ExtensibleGraph*> analysis_pcfg_map;
    
    //! Thsi class implements a set of analysis and optimizations
    class LIBTL_CLASS AnalysisSingleton
    {
    private:
        
        // ********************************* //
        // ******* Private attributes ****** //
        
        int _last_pcfg_id;
        analysis_state_map _states;
        analysis_pcfg_map _pcfgs;   // Set of pcfgs created. This attributed is needed for IPA analysis in PCFGVisitor class
        static AnalysisSingleton* _analysis;
        
        // **** End private attributes ***** //
        // ********************************* //
        
        
        // ********************************* //
        // ******** Private methods ******** //
        
        //!Private constructor. Prevent calling constructor.
        AnalysisSingleton( );
        
        //!Not implemented method. Prevent copy constructor.
        AnalysisSingleton( const AnalysisSingleton& );
        
        //!Not implemented method. Prevent assignment.
        void operator=( const AnalysisSingleton& );
        
        // ****** End private methods ****** //
        // ********************************* //
        
    public:
        
        // ********************************* //
        // ******* Singleton methods ******* //
        
        //!Destructor
        ~AnalysisSingleton( );
        
        //!Single instance constructor
        static AnalysisSingleton* get_analysis( );

        // ***** End singleton methods ***** //
        // ********************************* //
        
        
        // ********************************* //
        // ******** Analysis methods ******* //
        
        /*!This analysis creates a Parallel Control Flow Graph from \ast
         */
        void parallel_control_flow_graph( Nodecl::NodeclBase ast );
        
        /*!This optimization performs Conditional Constant Propagation (CCP) over the AST represented by \ast
         * This optimization is an extension of the Constant Propagation and Constant Folding algorithm
         * that takes conditional branches into account applying Unreachable Code Elimination.
         */
        void conditional_constant_propagation ( Nodecl::NodeclBase ast );
        
        /*!
         */
        void expression_canonicalization ( Nodecl::NodeclBase ast );
        
        
        void use_def ( Nodecl::NodeclBase ast );
        
        void liveness ( Nodecl::NodeclBase ast );
        
        void induction_variables ( Nodecl::NodeclBase ast );
        
        // ***** End analysis methods ****** //
        // ********************************* //

        
        // ********************************* //
        // ****** Getters and setters ****** //
        
        ObjectList<ExtensibleGraph*> get_pcfgs( );
        
        // **** End getters and setters **** //
        // ********************************* //
    };    
    
}
}

#endif      // TL_ANALYSIS_SINGLETON_HPP