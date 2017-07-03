/*--------------------------------------------------------------------
 (C) Copyright 2006-2014 Barcelona Supercomputing Center             *
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
            CONSTANTS_ANALYSIS      = 1u << 6,
            AUTO_SCOPING            = 1u << 7,
            RANGE_ANALYSIS          = 1u << 8,
            CORRECTNESS             = 1u << 9,
            NONE                    = 0u
        } _which_analysis;

        WhichAnalysis(Analysis_tag a);
        WhichAnalysis(int a);
        WhichAnalysis operator|(WhichAnalysis a);
    };

    // ******************** END class to define which analysis are to be done ********************** //
    // ********************************************************************************************* //



    // ************************************************************************************ //
    // *************** Class containing all analysis related to a given AST *************** //

    typedef std::map<std::string, ExtensibleGraph*> Name_to_pcfg_map;
    typedef std::map<std::string, TaskDependencyGraph*> Name_to_tdg_map;

    // ************************************************************************************ //
    // ********* Class representing a Singleton object used for analysis purposes ********* //
    //! This class implements a Meyers Singleton that includes methods for any kind of analysis
    class LIBTL_CLASS AnalysisBase
    {
    private:

        // ************** Private attributes ************** //
        Name_to_pcfg_map _pcfgs;
        Name_to_tdg_map _tdgs;
        ObjectList<NBase> _all_functions;

        bool _is_ompss_enabled;
        
        bool _pcfg;                 //!<True when parallel control flow graph have bee build
//         bool _constants_propagation;//!<True when constant propagation and constant folding have been applied
        bool _canonical;            //!<True when expressions canonicalization has been applied
        bool _use_def;              //!<True when use-definition chains have been calculated
        bool _liveness;             //!<True when liveness analysis has been applied
        bool _loops;                //!<True when loops analysis has been applied
        bool _reaching_definitions; //!<True when reaching definitions has been calculated
        bool _induction_variables;  //!<True when induction variable analysis has been applied
        bool _range;                //!<True when range analysis has been applied
        bool _cyclomatic_complexity;//!<True when cyclomatic complexity has been computed
        bool _auto_scoping;         //!<True when tasks auto-scoping has been calculated
        bool _auto_deps;            //!<True when tasks auto-dependencies has been calculated
        bool _tdg;                  //!<True when PCFG's tasks dependency graphs have been created

        /*!Returns the PCFG node enclosed in a PCFG node containing the flow of a nodecl
         * @param current PCFG node where to search the nodecl
         * @param n Nodecl to be searched in the flow graph
         * @return The PCFG node containing the Nodecl
         */
        Node* node_enclosing_nodecl_rec(Node* current, const NBase& n);

        Node* node_enclosing_nodecl(const NBase& n);

        ExtensibleGraph* get_pcfg(std::string name) const;
        
        TaskDependencyGraph* get_tdg(std::string name) const;

        ExtensibleGraph* create_pcfg(
                const NBase& ast,
                const std::map<Symbol, NBase>& asserted_funcs,
                std::set<Symbol>& visited_funcs);
        void parallel_control_flow_graph_rec(
                ExtensibleGraph* pcfg,
                const std::map<Symbol, NBase>& asserted_funcs,
                std::set<Symbol>& visited_funcs);

        // *************** Private methods **************** //

        //!Prevents copy construction.
        AnalysisBase(const AnalysisBase& analysis){};

        //!Prevents assignment.
        void operator=(const AnalysisBase& analysis){};

    public:

        // *** Constructor *** //
        AnalysisBase(bool is_ompss_enabled);

        // *** Getters *** //
        ObjectList<ExtensibleGraph*> get_pcfgs() const;
        ObjectList<TaskDependencyGraph*> get_tdgs() const;
        
        // *** Modifiers *** //

        /*!This analysis creates one Parallel Control Flow Graph per each function contained in \ast
         * If \ast contains no function, then the method creates a PCFG for the whole code in \ast
         * The memento is modified containing the PCFGs and a flag is set indicating the PCFG analysis has been performed
         * \param memento in/out object where the analysis is stored
         * \param ast Tree containing the code to construct the PCFG(s)
         */
        void parallel_control_flow_graph(
                const NBase& ast,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        /*!This optimization performs Conditional Constant Propagation (CCP) over \pcfg
         * This optimization is an extension of the Constant Propagation and Constant Folding algorithm
         * that takes conditional branches into account applying Unreachable Code Elimination.
         */
//         void conditional_constant_propagation(const NBase& ast);

        //!This overloaded method applies Conditional Constant propagation as a phase over the \_dto
//         void conditional_constant_propagation();

        void use_def(
                const NBase& ast,
                bool propagate_graph_nodes,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        void liveness(
                const NBase& ast,
                bool propagate_graph_nodes,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        void reaching_definitions(
                const NBase& ast,
                bool propagate_graph_nodes,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        /*!This analysis computes the induction variables in \ast
         * It searches in \memento the PCFGs corresponding to \ast and, in case they do not exist, the PCFGs are created
         * The Induction Variables computed are attached to the corresponding LOOP nodes
         */
        void induction_variables(
                const NBase& ast,
                bool propagate_graph_nodes,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        void range_analysis(
                const NBase& ast,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        void cyclomatic_complexity(
                const NBase& ast,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);
        
        void auto_scoping(
                const NBase& ast,
                std::set<std::string> functions = std::set<std::string>(),
                bool call_graph = true);

        ObjectList<TaskDependencyGraph*> task_dependency_graph(
                const NBase& ast,
                std::set<std::string> functions,
                bool call_graph,
                bool taskparts_enabled,
                bool expand_tdg);

        void all_analyses(const NBase& ast, bool propagate_graph_nodes);


        // ********************* Utils ******************** //

        void print_pcfg(std::string pcfg_name);

        void print_all_pcfg();

        void print_tdg(std::string tdg_name);

        void tdgs_to_json(const ObjectList<TaskDependencyGraph*>& tdgs);
    };

    // ******* END class representing a Singleton object used for analysis purposes ******* //
    // ************************************************************************************ /
}
}

#endif      // TL_ANALYSIS_SINGLETON_HPP
