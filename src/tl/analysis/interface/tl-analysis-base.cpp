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


#include "cxx-cexpr.h"
#include "cxx-process.h"

#include "tl-analysis-base.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-auto-scope.hpp"
#include "tl-cyclomatic-complexity.hpp"
#include "tl-iv-analysis.hpp"
#include "tl-liveness.hpp"
#include "tl-loop-analysis.hpp"
#include "tl-pcfg-visitor.hpp"
#include "tl-pointer-size.hpp"
#include "tl-range-analysis.hpp"
#include "tl-reaching-definitions.hpp"
#include "tl-task-sync.hpp"
#include "tl-use-def.hpp"

namespace TL {
namespace Analysis {

    AnalysisBase::AnalysisBase(bool is_ompss_enabled)
            : _pcfgs(), _tdgs(), _all_functions(), _is_ompss_enabled(is_ompss_enabled),
              _pcfg(false), /*_constants_propagation(false),*/ _canonical(false),
              _use_def(false), _liveness(false), _loops(false),
              _reaching_definitions(false), _induction_variables(false),
              _range(false), _cyclomatic_complexity(false),
              _auto_scoping(false), _auto_deps(false), _tdg(false)
    {}

    ExtensibleGraph* AnalysisBase::get_pcfg(std::string name) const
    {
        ExtensibleGraph* pcfg = NULL;
        Name_to_pcfg_map::const_iterator pcfgs_it = _pcfgs.find(name);
        if (pcfgs_it != _pcfgs.end())
            pcfg = pcfgs_it->second;
        return pcfg;
    }

    ObjectList<ExtensibleGraph*> AnalysisBase::get_pcfgs() const
    {
        ObjectList<ExtensibleGraph*> result;
        for (Name_to_pcfg_map::const_iterator it = _pcfgs.begin(); it != _pcfgs.end(); ++it)
            result.insert(it->second);
        return result;
    }

    TaskDependencyGraph* AnalysisBase::get_tdg(std::string name) const
    {
        TaskDependencyGraph* tdg = NULL;
        Name_to_tdg_map::const_iterator tdgs_it = _tdgs.find(name);
        if (tdgs_it != _tdgs.end())
            tdg = tdgs_it->second;
        return tdg;
    }

    ObjectList<TaskDependencyGraph*> AnalysisBase::get_tdgs() const
    {
        ObjectList<TaskDependencyGraph*> result;
        for (Name_to_tdg_map::const_iterator it = _tdgs.begin(); it != _tdgs.end(); ++it)
            result.insert(it->second);
        return result;
    }

    ExtensibleGraph* AnalysisBase::create_pcfg(
            const NBase& ast,
            const std::map<Symbol, NBase>& asserted_funcs,
            std::set<Symbol>& visited_funcs)
    {
        // Generate the hashed name corresponding to the AST of the function
        std::string pcfg_name = Utils::generate_hashed_name(ast);

        // Create the PCFG
        if (VERBOSE)
            std::cerr << "Parallel Control Flow Graph (PCFG) '" << pcfg_name << "'" << std::endl;
        PCFGVisitor v(pcfg_name, ast);
        ExtensibleGraph* pcfg = v.parallel_control_flow_graph(ast, asserted_funcs);

        // Synchronize the tasks, if applies
        if (VERBOSE)
            std::cerr << "Task Synchronization of PCFG '" << pcfg_name << "'" << std::endl;
        TaskAnalysis::TaskSynchronizations task_sync_analysis(pcfg, _is_ompss_enabled);
        task_sync_analysis.compute_task_synchronizations();

        // Store the pcfg
        _pcfgs[pcfg_name] = pcfg;

        // Store the symbol of the function we just visited
        Symbol func_sym = pcfg->get_function_symbol();
        if (func_sym.is_valid())
            visited_funcs.insert(func_sym);

        return pcfg;
    }

    void AnalysisBase::parallel_control_flow_graph_rec(
            ExtensibleGraph* pcfg,
            const std::map<Symbol, NBase>& asserted_funcs,
            std::set<Symbol>& visited_funcs)
    {
        ObjectList<Symbol> called_funcs = pcfg->get_function_calls();
        for (ObjectList<Symbol>::iterator it = called_funcs.begin();
             it != called_funcs.end(); ++it)
        {
            if (visited_funcs.find(*it) == visited_funcs.end())
            {
                NBase func_ast;
                // Find the ast corresponding to this symbol in the list of functions of this translation unit
                for (ObjectList<NBase>::iterator itt = _all_functions.begin();
                     itt != _all_functions.end(); ++itt)
                {
                    if (itt->is<Nodecl::FunctionCode>() && itt->get_symbol() == *it)
                        func_ast = *itt;
                }
                if (func_ast.is_null())     // The code of the function is not reachable
                    continue;
                ExtensibleGraph* new_pcfg = create_pcfg(func_ast, asserted_funcs, visited_funcs);
                parallel_control_flow_graph_rec(new_pcfg, asserted_funcs, visited_funcs);
            }
        }
    }

    void AnalysisBase::parallel_control_flow_graph(
            const NBase& ast,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_pcfg)
            return;

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _pcfg = true;

        ObjectList<NBase> unique_asts;
        std::map<Symbol, NBase> asserted_funcs;

        // Get all unique ASTs embedded in 'ast'
        if (!ast.is<Nodecl::TopLevel>())
        {
            unique_asts.append(ast);
        }
        else
        {
            // Get all functions in \ast
            Utils::TopLevelVisitor tlv;
            tlv.walk_functions(ast);
            _all_functions = tlv.get_functions();
            if (functions.empty())
            {
                unique_asts = _all_functions;
            }
            else
            {
                std::set<std::string> not_found_functions(functions);
                for (ObjectList<NBase>::iterator it = _all_functions.begin();
                        it != _all_functions.end(); ++it)
                {
                    std::string func_name = it->get_symbol().get_name();
                    if (functions.find(func_name) != functions.end())
                    {
                        not_found_functions.erase(func_name);
                        unique_asts.append(*it);
                    }
                }
                // Warn about the functions that have not been found
                if (!not_found_functions.empty())
                {
                    std::string comma_separated_names;
                    for (std::set<std::string>::iterator it = not_found_functions.begin();
                         it != not_found_functions.end(); )
                    {
                        comma_separated_names += *it;
                        ++it;
                        if (it != not_found_functions.end())
                            comma_separated_names += ", ";
                    }
                    if (not_found_functions.size() == 1)
                    {
                        WARNING_MESSAGE("Function '%s' specified in parameter 'functions' has not been found.\n"
                                        "No analysis will be performed on that function.\n",
                                        comma_separated_names.c_str());
                    }
                    else
                    {
                        WARNING_MESSAGE("Functions '%s' specified in parameter 'functions' have not been found.\n"
                                        "No analysis will be performed on that functions.\n",
                                        comma_separated_names.c_str());
                    }
                }
            }
            asserted_funcs = tlv.get_asserted_funcs();
        }

        // Compute the PCFG corresponding to each AST
        std::set<Symbol> visited_funcs;
        for (ObjectList<NBase>::iterator it = unique_asts.begin(); it != unique_asts.end(); ++it)
        {
            create_pcfg(*it, asserted_funcs, visited_funcs);
        }

        // Make sure all called functions whose code is reachable, have been computed
        if (call_graph && !functions.empty())
        {
            ObjectList<ExtensibleGraph*> pcfgs = get_pcfgs();
            for (ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin();
                it != pcfgs.end(); ++it)
            {
                parallel_control_flow_graph_rec(*it, asserted_funcs, visited_funcs);
            }
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: PCFG computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    // TODO
//     void AnalysisBase::conditional_constant_propagation(const NBase& ast)
//     {
//         if (!_constants_propagation)
//         {
//             _constants_propagation = true;
// 
//             const ObjectList<ExtensibleGraph*>& pcfgs = parallel_control_flow_graph(ast);
//             for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
//             {
//                 if (VERBOSE)
//                     printf("Constants Propagation of PCFG '%s'\n", (*it)->get_name().c_str());
//                 std::cerr << "Constants Propagation is not yet implemented" << std::endl;
//                 // ConditionalConstantAnalysis ca(ipa);
//                 // ca.conditional_constant_propagation(pcfg);
//             }
//         }
//     }

    static void use_def_rec(
            Symbol func_sym,
            bool propagate_graph_nodes,
            std::set<Symbol>& visited_funcs,
            ObjectList<ExtensibleGraph*>& pcfgs)
    {
        // Nothing to do if we are analyzing something that:
        // - is not a function
        // - has already been analyzed
        if (!func_sym.is_valid() || (visited_funcs.find(func_sym) != visited_funcs.end()))
            return;

        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            Symbol it_func_sym((*it)->get_function_symbol());
            if (it_func_sym.is_valid() && it_func_sym == func_sym)
            {
                visited_funcs.insert(func_sym);
                if (!(*it)->usage_is_computed())
                {
                    // Recursively analyze the functions called from the current graph
                    ObjectList<Symbol> called_funcs = (*it)->get_function_calls();
                    for (ObjectList<Symbol>::iterator itf = called_funcs.begin(); itf != called_funcs.end(); ++itf)
                        use_def_rec(*itf, propagate_graph_nodes, visited_funcs, pcfgs);

                    // Analyze the current graph
                    if (VERBOSE)
                        std::cerr << "Use-Definition of PCFG '" << (*it)->get_name() << "'" << std::endl;
                    UseDef ud(*it, propagate_graph_nodes, pcfgs);
                    ud.compute_usage();
                }
            }
        }
    }

    void AnalysisBase::use_def(
            const NBase& ast,
            bool propagate_graph_nodes,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_use_def)
            return;

        // Required previous analysis
        parallel_control_flow_graph(ast, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _use_def = true;

        std::set<Symbol> visited_funcs;
        ObjectList<ExtensibleGraph*> pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (!(*it)->usage_is_computed())
            {
                PointerSize ps(*it);
                ps.compute_pointer_vars_size();
                use_def_rec((*it)->get_function_symbol(), propagate_graph_nodes, visited_funcs, pcfgs);
            }
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: USE_DEF computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    void AnalysisBase::liveness(
            const NBase& ast,
            bool propagate_graph_nodes,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_liveness)
            return;

        // Required previous analysis
        // FIXME Do we need to pass the \p propagate_graph_nodes parameter here too?
        use_def(ast, propagate_graph_nodes, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _liveness = true;

        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Liveness of PCFG '" << (*it)->get_name() << "'" << std::endl;
            Liveness l(*it, propagate_graph_nodes);
            l.compute_liveness();
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: LIVENESS computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    void AnalysisBase::reaching_definitions(
            const NBase& ast,
            bool propagate_graph_nodes,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_reaching_definitions)
            return;

        // Required previous analysis
        use_def(ast, propagate_graph_nodes, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _reaching_definitions = true;

        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Reaching Definitions of PCFG '" << (*it)->get_name() << "'" << std::endl;
            ReachingDefinitions rd(*it);
            rd.compute_reaching_definitions();
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: REACHING_DEFINITIONS computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    void AnalysisBase::induction_variables(
            const NBase& ast,
            bool propagate_graph_nodes,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_induction_variables)
            return;

        // Required previous analysis
        reaching_definitions(ast, propagate_graph_nodes, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _induction_variables = true;

        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Induction Variables of PCFG '" << (*it)->get_name() << "'" << std::endl;

            // Compute the induction variables of all loops of each PCFG
            InductionVariableAnalysis iva(*it);
            iva.compute_induction_variables();

            // Compute the limits of the induction variables
            Utils::InductionVarsPerNode ivs = iva.get_all_induction_vars();
            LoopAnalysis la(*it, ivs);
            la.compute_loop_ranges();

            if (VERBOSE)
                Utils::print_induction_vars(ivs);

            if (ANALYSIS_PERFORMANCE_MEASURE)
                fprintf(stderr, "ANALYSIS: INDUCTION_VARIABLES computation time: %lf\n", (time_nsec() - init)*1E-9);
        }
    }

    void AnalysisBase::range_analysis(
            const NBase& ast,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_range)
            return;

        // Required previous analysis
        use_def(ast, /*propagate_graph_nodes*/ true, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _range = true;

        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Range Analysis of PCFG '" << (*it)->get_name() << "'" << std::endl;

            // Compute the induction variables of all loops of each PCFG
            RangeAnalysis ra(*it);
            ra.compute_range_analysis();
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: RANGE_ANALYSIS computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    void AnalysisBase::cyclomatic_complexity(
            const NBase& ast,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_cyclomatic_complexity)
            return;

        // Required previous analysis
        parallel_control_flow_graph(ast, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _cyclomatic_complexity = true;
        
        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Cyclomatic Complexity of PCFG '" << (*it)->get_name() << "'" << std::endl;
            
            // Compute the cyclomatic complexity of each PCFG
            CyclomaticComplexity cc(*it);
            unsigned int res = cc.compute_cyclomatic_complexity();
            if (VERBOSE)
                printf(" = %d\n", res);
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: CYCLOMATIC_COMPLEXITY computation time: %lf\n", (time_nsec() - init)*1E-9);
    }
    
    void AnalysisBase::auto_scoping(
            const NBase& ast,
            std::set<std::string> functions,
            bool call_graph)
    {
        if (_auto_scoping)
            return;

        // Required previous analysis
        reaching_definitions(ast, /*propagate_graph_nodes*/ true, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();

        _auto_scoping = true;

        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if (VERBOSE)
                std::cerr << "Auto-Scoping of PCFG '" << (*it)->get_name() << "'" << std::endl;

            AutoScoping as(*it);
            as.compute_auto_scoping();
        }

        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: AUTO_SCOPING computation time: %lf\n", (time_nsec() - init)*1E-9);
    }

    ObjectList<TaskDependencyGraph*> AnalysisBase::task_dependency_graph(
            const NBase& ast,
            std::set<std::string> functions,
            bool call_graph,
            bool taskparts_enabled,
            bool expand_tdg)
    {
        if (_tdg)
            return get_tdgs();
        
        // Required previous analyses
        induction_variables(ast, /*propagate_graph_nodes*/ true, functions, call_graph);
        if (expand_tdg)
            reaching_definitions(ast, /*propagate_graph_nodes*/ true, functions, call_graph);
        else
            range_analysis(ast, functions, call_graph);

        double init = 0.0;
        if (ANALYSIS_PERFORMANCE_MEASURE)
            init = time_nsec();
        
        _tdg = true;
        
        ObjectList<TaskDependencyGraph*> tdgs;
        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
        {
            if ((*it)->get_tasks_list().empty())
            {
                if (VERBOSE)
                    std::cerr << "PCFG '" << (*it)->get_name() << "' does not contain tasks. "
                    << "No Task Dependency Graph has been generated." << std::endl;
                continue;
            }
            if (VERBOSE)
                std::cerr << "Task Dependency Graph (TDG) of PCFG '" << (*it)->get_name() << "'" << std::endl;
            
            TaskDependencyGraph* tdg;
            if (expand_tdg)
            {
                tdg = new TaskDependencyGraph(*it);
            }
            else
            {
                tdg = new TaskDependencyGraph(*it, (*it)->get_name(), taskparts_enabled);
            }
            tdgs.insert(tdg);
            _tdgs[(*it)->get_name()] = tdg;
        }
        
        if (expand_tdg)
        {
            ObjectList<ExpandedTaskDependencyGraph*> etdgs;
            for (ObjectList<TaskDependencyGraph*>::iterator it = tdgs.begin(); it != tdgs.end(); ++it)
            {
                etdgs.append((*it)->get_etdg());
            }
            TaskDependencyGraphMapper tdgm(etdgs);
            tdgm.generate_runtime_tdg();
        }
        
        if (ANALYSIS_PERFORMANCE_MEASURE)
            fprintf(stderr, "ANALYSIS: TDG computation time: %lf\n", (time_nsec() - init)*1E-9);
        
        return tdgs;
    }

    void AnalysisBase::all_analyses(const NBase& ast, bool propagate_graph_nodes)
    {
        // This launches PCFG, UseDef, Liveness, ReachingDefs and InductionVars analysis
        induction_variables(ast, propagate_graph_nodes);
        // This launches Auto-Scoping
        auto_scoping(ast);
    }

    void AnalysisBase::print_pcfg(std::string pcfg_name)
    {
        ExtensibleGraph* pcfg = get_pcfg(pcfg_name);
        if (debug_options.print_pcfg_w_analysis ||
            debug_options.print_pcfg_full)
        {   // Print analysis information
            if (VERBOSE)
                std::cerr << "Printing PCFG '" << pcfg_name << "' to DOT" << std::endl;
            pcfg->print_graph_to_dot(_use_def, _liveness, _reaching_definitions,
                                     _induction_variables, _range,
                                     _auto_scoping, _auto_deps);
        }
        else if (debug_options.print_pcfg ||
            debug_options.print_pcfg_w_context)
        {   // Do not print analysis information
            if (VERBOSE)
                std::cerr << "Printing PCFG '" << pcfg_name << "' to DOT" << std::endl;
            pcfg->print_graph_to_dot();
        }
    }

    void AnalysisBase::print_all_pcfg()
    {
        const ObjectList<ExtensibleGraph*>& pcfgs = get_pcfgs();
        if (debug_options.print_pcfg_w_analysis ||
            debug_options.print_pcfg_full)
        {   // Print analysis information
            for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
            {
                if (VERBOSE)
                    std::cerr << "Printing PCFG '" << (*it)->get_name() << "' to DOT" << std::endl;
                (*it)->print_graph_to_dot(_use_def, _liveness, _reaching_definitions,
                                          _induction_variables, _range,
                                          _auto_scoping, _auto_deps);
            }
        }
        else if (debug_options.print_pcfg ||
            debug_options.print_pcfg_w_context)
        {   // Do not print analysis information
            for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
            {
                if (VERBOSE)
                    std::cerr << "Printing PCFG '" << (*it)->get_name() << "' to DOT" << std::endl;
                (*it)->print_graph_to_dot();
            }
        }
    }
    
    void AnalysisBase::print_tdg(std::string tdg_name)
    {
        if (VERBOSE)
            std::cerr << "Printing TDG '" << tdg_name << "' to DOT" << std::endl;
        TaskDependencyGraph* tdg = get_tdg(tdg_name);
        tdg->print_tdg_to_dot();
    }
    
    void AnalysisBase::tdgs_to_json(const ObjectList<TaskDependencyGraph*>& tdgs)
    {
        TaskDependencyGraph::print_tdgs_to_json(tdgs);
    }
}
}
