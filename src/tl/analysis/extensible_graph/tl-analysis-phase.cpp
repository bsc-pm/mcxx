/*--------------------------------------------------------------------
(C) Copyright 2006-2009 Barcelona Supercomputing Center 
Centro Nacional de Supercomputacion

This file is part of Mercurium C/C++ source-to-source compiler.

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
#include "tl-analysis-common.hpp"
#include "tl-analysis-phase.hpp"
#include "tl-cfg-visitor.hpp"
#include "tl-static-analysis.hpp"

namespace TL
{
    namespace Analysis
    {
        AnalysisPhase::AnalysisPhase()
        {
            set_phase_name("Experimental phase for analysis");
            set_phase_description("This phase builds a Control Flow Graph and performs different analysis on demand. ");
        }
        
        void AnalysisPhase::run(TL::DTO& dto)
        {
            RefPtr<Nodecl::NodeclBase> nodecl = RefPtr<Nodecl::NodeclBase>::cast_dynamic(dto["nodecl"]);
            
            // *** Build the graphs for every method in the translation unit *** //
            // *** Tags the global variables used within each graph *** //
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                std::cerr << std::endl << "=== CFG Construction ===" << std::endl;
            CfgVisitor cfg_visitor;
            cfg_visitor.build_cfg(nodecl, std::string(""));
            
            ObjectList<ExtensibleGraph*> cfgs = cfg_visitor.get_cfgs();
           
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
            {
                std::cerr << std::endl << "=== GLOBAL VARIABLES USED WITHIN GRAPHS ===" << std::endl;
                for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
                {
                    std::cerr << "  ==> Graph '" << (*it)->get_name() << "'" << std::endl;
                    ObjectList<var_usage_t*> glob_vars = (*it)->get_global_variables();
                    for(ObjectList<var_usage_t*>::iterator it = glob_vars.begin(); it != glob_vars.end(); ++it)
                    {
                        std::cerr << "       - " << (*it)->get_nodecl().prettyprint() << std::endl;
                    }
                }
            }
            
            // FIXME We should do here loops analysis and know the ranges of the induction variables
            // At that point, use-def analysis may be incorrect because we don't know if two array accesses go to the same position
            
            // *** Use-def chains + IPA *** //
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                std::cerr << std::endl << "=== USE-DEF CHAINS COMPUTATION ===" << std::endl;
            for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
            {
                if ((*it)->has_use_def_computed() == '0')
                {
                    cfg_visitor.set_actual_cfg(*it);
                    Node* graph_node = (*it)->get_graph();
                    cfg_visitor.compute_use_def_chains(graph_node);
                    if ((*it)->has_use_def_computed() == '0')
                    {   // If the cfg contains some function call that has undefined behaviour, the this variables is already set to '3'
                        // Otherwise, we have entirely complete the analysis, and we set this value to '1'
                        (*it)->set_use_def_computed('1');
                    }
                }
            }
        
            // *** Loops Analysis *** //
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
            std::cerr << std::endl << "=== LOOP ANALYSIS COMPUTATION ===" << std::endl;
            for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
            {
                if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                    CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                    std::cerr << std::endl << "   ==> Graph '" << (*it)->get_name() << "'" << std::endl;
                cfg_visitor.analyse_loops((*it)->get_graph());
            }
        
            // *** Live Variable Analysis *** //
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                std::cerr << std::endl << "=== LIVE VARIABLES AND TASKS ANALYSIS  ===" << std::endl;
            for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
            {
                if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                    CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                    std::cerr << std::endl << " ==> Graph '" << (*it)->get_name() << "'" << std::endl;
                    
                StaticAnalysis::live_variable_analysis((*it)->get_graph());
                
                if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                    CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                {
                    (*it)->get_graph()->print_use_def_chains();
                    (*it)->get_graph()->print_liveness();
                }
            }
            
            // *** Auto-dependencies *** //
            if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                std::cerr << std::endl << "=== AUTO-DEPENDENCIES ===" << std::endl;
            for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
            {
                if (CURRENT_CONFIGURATION->debug_options.analysis_verbose ||
                    CURRENT_CONFIGURATION->debug_options.enable_debug_code)
                    std::cerr << std::endl << "   ==> Graph '" << (*it)->get_name() << "'" << std::endl;
                    StaticAnalysis::analyse_tasks((*it)->get_graph());
            }
            
            // Print graphs into dot files
            if (CURRENT_CONFIGURATION->debug_options.print_cfg_graphviz)
            {
                std::cerr << std::endl << "=== PRINT DOT GRAPHS ===" << std::endl;
                for (ObjectList<ExtensibleGraph*>::iterator it = cfgs.begin(); it != cfgs.end(); ++it)
                    (*it)->print_graph_to_dot();
            }
        }
    }
}

EXPORT_PHASE(TL::Analysis::AnalysisPhase);
