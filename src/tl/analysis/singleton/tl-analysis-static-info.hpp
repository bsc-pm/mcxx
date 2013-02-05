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

#ifndef TL_ANALYSIS_STATIC_INFO_HPP
#define TL_ANALYSIS_STATIC_INFO_HPP

#include "tl-analysis-singleton.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-nodecl-visitor.hpp"
#include "tl-objectlist.hpp"

namespace TL {
namespace Analysis {

    // ********************************************************************************************* //
    // ********************** Class to define which analysis are to be done ************************ //

    struct WhichAnalysis
    {
        // Macros defining the analysis to be computed
        enum Analysis_tag
        {
            PCFG_ANALYSIS           = 1u << 1,
            USAGE_ANALYSIS          = 1u << 2,
            LIVENESS_ANALYSIS       = 1u << 3,
            REACHING_DEFS_ANALYSIS  = 1u << 4,
            INDUCTION_VARS_ANALYSIS = 1u << 5,
            CONSTANTS_ANALYSIS      = 1u << 6
        } _which_analysis;

        WhichAnalysis( Analysis_tag a );
        WhichAnalysis( int a );
        WhichAnalysis operator|( WhichAnalysis a );
    };

    struct WhereAnalysis
    {
        // Macros defining whether the Static Info must be computed in nested block
        enum Nested_analysis_tag
        {
            NESTED_NONE_STATIC_INFO     = 1u << 0,
            NESTED_IF_STATIC_INFO       = 1u << 2,
            NESTED_DO_STATIC_INFO       = 1u << 3,
            NESTED_WHILE_STATIC_INFO    = 1u << 4,
            NESTED_FOR_STATIC_INFO      = 1u << 5,
            NESTED_ALL_STATIC_INFO      = 0xFF
        } _where_analysis;

        WhereAnalysis( Nested_analysis_tag a );
        WhereAnalysis( int a );
        WhereAnalysis operator|( WhereAnalysis a );
    };

    // ******************** END class to define which analysis are to be done ********************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************** Class to retrieve analysis info about one specific nodecl ****************** //

    class NodeclStaticInfo
    {
        private:
            ObjectList<Analysis::Utils::InductionVariableData*> _induction_variables;
            Utils::ext_sym_set _killed;

        public:
            NodeclStaticInfo( ObjectList<Analysis::Utils::InductionVariableData*> induction_variables,
                              Utils::ext_sym_set killed );

            bool is_constant( const Nodecl::NodeclBase& n ) const;

            // *** Queries about induction variables *** //

            bool is_induction_variable( const Nodecl::NodeclBase& n ) const;

            bool is_basic_induction_variable( const Nodecl::NodeclBase& n ) const;

            const_value_t* get_induction_variable_increment( const Nodecl::NodeclBase& n ) const;

            bool is_induction_variable_increment_one( const Nodecl::NodeclBase& n ) const;

            //! Returns the induction variable containing the given nodecl
            //! If the nodecl is not an induction variable, then returns NULL
            Utils::InductionVariableData* get_induction_variable( const Nodecl::NodeclBase& n ) const;

            ObjectList<Utils::InductionVariableData*> get_induction_variables( const Nodecl::NodeclBase& n ) const;

            bool is_adjacent_access( const Nodecl::NodeclBase& n ) const;
    };

    // ************** END class to retrieve analysis info about one specific nodecl **************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // **************************** User interface for static analysis ***************************** //

    typedef std::map<Nodecl::NodeclBase, NodeclStaticInfo> static_info_map_t;
    typedef std::pair<Nodecl::NodeclBase, NodeclStaticInfo> static_info_pair_t;

    class AnalysisStaticInfo
    {
        private:
            static_info_map_t _static_info_map;

        public:
            AnalysisStaticInfo( const Nodecl::NodeclBase& n, WhichAnalysis analysis_mask,
                                WhereAnalysis nested_analysis_mask, int nesting_level );

            // *** Getters and Setters *** //

            static_info_map_t get_static_info_map( ) const;

            // *** Queries about Use-Def analysis *** //

            //! Returns true when an object is constant in a given scope
            bool is_constant( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;

            // *** Queries about induction variables *** //

            //! Returns true when an object is an induction variable in a given scope
            bool is_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;

            //! Returns true when an object is an induction variable in a given scope
            bool is_basic_induction_variable( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;

            //! Returns the const_value corresponding to the increment of an induction variable in a given scope
            const_value_t* get_induction_variable_increment( const Nodecl::NodeclBase& scope,
                                                             const Nodecl::NodeclBase& n ) const;

            //! Returns true when the increment of a given induction variable is constant and equal to 1
            bool is_induction_variable_increment_one( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;

            //! Returns a list with the induction variables of a given scope
            ObjectList<Utils::InductionVariableData*> get_induction_variables( const Nodecl::NodeclBase& scope,
                                                                               const Nodecl::NodeclBase& n ) const;

            //! Returns true if the given nodecl is an array accessed by adjacent positions
            bool is_adjacent_access( const Nodecl::NodeclBase& scope, const Nodecl::NodeclBase& n ) const;
    };

    // ************************** END User interface for static analysis *************************** //
    // ********************************************************************************************* //



    // ********************************************************************************************* //
    // ********************* Visitor retrieving the analysis of a given Nodecl ********************* //

    class LIBTL_CLASS NestedBlocksStaticInfoVisitor : public Nodecl::ExhaustiveVisitor<void>
    {
    private:
        //! State of the analysis for the given nodecl
        PCFGAnalysis_memento _state;

        //! Mask containing the analysis to be performed
        WhichAnalysis _analysis_mask;

        //! Mask containing the nested constructs to be parsed
        WhereAnalysis _nested_analysis_mask;

        //! Level of nesting of blocks inside the visited nodecl to be parsed
        int _nesting_level;

        //! Temporary member used during computation. Contains the current level of nesting
        int _current_level;

        //! Member where the analysis info is stored during the visit
        static_info_map_t _analysis_info;

        void retrieve_current_node_static_info( Nodecl::NodeclBase n );

    public:
        // *** Constructor *** //
        NestedBlocksStaticInfoVisitor( WhichAnalysis analysis_mask, WhereAnalysis nested_analysis_mask,
                                       PCFGAnalysis_memento state, int nesting_level );

        // *** Getters and Setters *** //
        static_info_map_t get_analysis_info( );

        // *** Visiting methods *** //
        Ret join_list( ObjectList<static_info_map_t>& list );
        Ret visit(const Nodecl::DoStatement& n);
        Ret visit(const Nodecl::IfElseStatement& n);
        Ret visit(const Nodecl::ForStatement& n);
        Ret visit(const Nodecl::WhileStatement& n);
    };

    // ******************* END Visitor retrieving the analysis of a given Nodecl ******************* //
    // ********************************************************************************************* //
}
}

#endif // TL_ANALYSIS_STATIC_INFO_HPP
