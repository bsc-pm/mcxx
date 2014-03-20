/*--------------------------------------------------------------------
 ( C) Copyright 2006-2013 Barcelona Supercomputing Center             *
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

#include "tl-extended-symbol.hpp"
#include "tl-extensible-graph.hpp"
#include "tl-induction-variables-data.hpp"
#include "tl-task-dependency-graph.hpp"

// Set of classes implementing the Memento Pattern with Analysis purposes.
// ----------------         -------------        ----------------
// |  Care Taker  |<>------>|  Memento  |<-------|  Originator  |
// ----------------         |  -------  |        |  ----------  |
//                          | get_state |        |  set_memento |
//                          | set_state |        |  get_memento |
//                          -------------        ----------------
// - PCFGAnalysis_memento is the Memento class
// - Each Client will be an Originator of a Memento class
// - AnalysisSingleton is the Care Taker

namespace TL {
namespace Analysis {

    // ************************************************************************************ //
    // *************** Class containing all analysis related to a given AST *************** //

    typedef std::map<std::string, ExtensibleGraph*> Name_to_pcfg_map;
    typedef std::map<std::string, TaskDependencyGraph*> Name_to_tdg_map;

    //! Memento class capturing the internal state of the PCFG regarding the analysis
    class PCFGAnalysis_memento {
    private:

        Name_to_pcfg_map _pcfgs;
        Name_to_tdg_map _tdgs;

        bool _pcfg;                 //!<True when parallel control flow graph have bee build
        bool _constants_propagation;//!<True when constant propagation and constant folding have been applied
        bool _canonical;            //!<True when expressions canonicalization has been applied
        bool _use_def;              //!<True when use-definition chains have been calculated
        bool _liveness;             //!<True when liveness analysis has been applied
        bool _loops;                //!<True when loops analysis has been applied
        bool _reaching_definitions; //!<True when reaching definitions has been calculated
        bool _induction_variables;  //!<True when induction variable analysis has been applied
        bool _tune_task_syncs;      //!<True when the task synchronization has been tuned
        bool _range;                //!<True when range analysis has been applied
        bool _auto_scoping;         //!<True when tasks auto-scoping has been calculated
        bool _auto_deps;            //!<True when tasks auto-dependencies has been calculated
        bool _tdg;                  //!<True when PCFG's tasks dependency graphs have been created

        /*!Returns the PCFG node enclosed in a PCFG node containing th flow of a nodecl
         * @param current PCFG node where to search the nodecl
         * @param n Nodecl to be searched in the flow graph
         * @return The PCFG node containing the Nodecl
         */
        Node* node_enclosing_nodecl_rec( Node* current, const Nodecl::NodeclBase& n );

        Node* node_enclosing_nodecl( const Nodecl::NodeclBase& n );

    public:
        //! Class constructor
        PCFGAnalysis_memento( );

        //! Resets the state of the memento
        void reset_state( );

        // ************* Getters and Setters ************* //

        ExtensibleGraph* get_pcfg( std::string name );
        void set_pcfg( std::string name, ExtensibleGraph* pcfg );
        ObjectList<ExtensibleGraph*> get_pcfgs( );

        TaskDependencyGraph* get_tdg( std::string name );
        void set_tdg( std::string name, TaskDependencyGraph* tdg );

        bool is_pcfg_computed() const;
        void set_pcfg_computed();
        bool is_constants_propagation_computed( ) const;
        void set_constants_propagation_computed( );
        bool is_canonical_computed( ) const;
        void set_canonical_computed( );
        bool is_usage_computed( ) const;
        void set_usage_computed( );
        bool is_liveness_computed( ) const;
        void set_liveness_computed( );
        bool is_loops_computed( ) const;
        void set_loops_computed( );
        bool is_reaching_definitions_computed( ) const;
        void set_reaching_definitions_computed( );
        bool is_induction_variables_computed( ) const;
        void set_induction_variables_computed( );
        bool is_task_synchronizations_tuned( ) const;
        void set_tune_task_synchronizations( );
        bool is_range_analysis_computed( ) const;
        void set_range_analysis_computed( );
        bool is_auto_scoping_computed( ) const;
        void set_auto_scoping_computed( );
        bool is_auto_deps_computed( ) const;
        void set_auto_deps_computed( );
        bool is_tdg_computed( ) const;
        void set_tdg_computed( );

        //! Returns the list of induction variables found in #n
        ObjectList<Utils::InductionVariableData*> get_induction_variables( const Nodecl::NodeclBase& n );

        //! Returns the list of reduction symbols found in #n
        ObjectList<Symbol> get_reductions( const Nodecl::NodeclBase& n );

        //! Returns a list of objects that are killed in #n
        Utils::ext_sym_set get_killed( const Nodecl::NodeclBase& n );

        Node* get_autoscoped_task( const Nodecl::NodeclBase& n );

    friend class AnalysisSingleton;
    };

    // ************* END class containing all analysis related to a given AST ************* //
    // ************************************************************************************ //



    // ************************************************************************************ //
    // ********* Class representing a Singleton object used for analysis purposes ********* //
    //! This class implements a Meyers Singleton that includes methods for any kind of analysis
    class LIBTL_CLASS AnalysisSingleton
    {
    private:

        // ************** Private attributes ************** //

        static AnalysisSingleton* _analysis;


        // *************** Private methods **************** //

        //!Private constructor. Prevents calling construction.
        AnalysisSingleton( );

        //!Prevents copy construction.
        AnalysisSingleton( const AnalysisSingleton& analysis ){};

        //!Prevents assignment.
        void operator=( const AnalysisSingleton& analysis ){};

        //!Prevents destruction
        ~AnalysisSingleton( ){};


    public:

        // ************** Singleton methods *************** //

        //!Single instance constructor
        static AnalysisSingleton& get_analysis( );


        // *************** Analysis methods *************** //

        /*!This analysis creates one Parallel Control Flow Graph per each function contained in \ast
         * If \ast contains no function, then the method creates a PCFG for the whole code in \ast
         * The memento is modified containing the PCFGs and a flag is set indicating the PCFG analysis has been performed
         * \param memento in/out object where the analysis is stored
         * \param ast Tree containing the code to construct the PCFG(s)
         * \return A list of pointer to the created PCFGs
         */
        ObjectList<ExtensibleGraph*> parallel_control_flow_graph( PCFGAnalysis_memento& memento,
                                                                  Nodecl::NodeclBase ast );

        /*!This optimization performs Conditional Constant Propagation (CCP) over \pcfg
         * This optimization is an extension of the Constant Propagation and Constant Folding algorithm
         * that takes conditional branches into account applying Unreachable Code Elimination.
         */
        void conditional_constant_propagation( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        //!This overloaded method applies Conditional Constant propagation as a phase over the \_dto
        void conditional_constant_propagation( );

        void expression_canonicalization( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> use_def( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> liveness( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> reaching_definitions( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        /*!This analysis computes the induction variables in \ast
         * It searches in \memento the PCFGs corresponding to \ast and, in case they do not exist, the PCFGs are created
         * The Induction Variables computed are attached to the corresponding LOOP nodes
         */
        ObjectList<ExtensibleGraph*> induction_variables( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> tune_task_synchronizations( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> range_analysis( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> auto_scoping( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<TaskDependencyGraph*> task_dependency_graph( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> all_analyses( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        ObjectList<ExtensibleGraph*> constants_analysis( PCFGAnalysis_memento& memento, Nodecl::NodeclBase ast );

        // ********************* Utils ******************** //

        void print_pcfg( PCFGAnalysis_memento& memento, std::string pcfg_name );

        void print_all_pcfg( PCFGAnalysis_memento& memento );

        void print_tdg( PCFGAnalysis_memento& memento, std::string tdg_name );

        void tdg_to_json( PCFGAnalysis_memento& memento, std::string tdg_name );
    };

    // ******* END class representing a Singleton object used for analysis purposes ******* //
    // ************************************************************************************ /
}
}

#endif      // TL_ANALYSIS_SINGLETON_HPP
