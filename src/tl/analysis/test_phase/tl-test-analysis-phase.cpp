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
// --------------------------------------------------------------------*/

#include "tl-test-analysis-phase.hpp"
#include "tl-analysis-base.hpp"
#include "tl-analysis-utils.hpp"
#include "tl-pcfg-visitor.hpp"

namespace TL {
namespace Analysis {

namespace {
    void tokenizer(std::string str, std::set<std::string>& result)
    {
        std::string temporary("");
        for (std::string::const_iterator it = str.begin();
             it != str.end(); ++it)
        {
            const char & c(*it);
            if (c == ',' || c == ' ')
            {
                if (temporary != "")
                {
                    std::cerr << "   -> " << temporary << std::endl;
                    result.insert(temporary);
                    temporary = "";
                }
            }
            else
            {
                temporary += c;
            }
        }
        if (temporary != "")
        {
            result.insert(temporary);
        }
    }
}

    TestAnalysisPhase::TestAnalysisPhase()
            : _pcfg_enabled_str(""), _pcfg_enabled(false),
              _use_def_enabled_str(""), _use_def_enabled(false),
              _liveness_enabled_str(""), _liveness_enabled(false),
              _reaching_defs_enabled_str(""), _reaching_defs_enabled(false),
              _induction_vars_enabled_str(""), _induction_vars_enabled(false),
              _tdg_enabled_str(""), _tdg_enabled(false),
              _range_analysis_enabled_str(""), _range_analysis_enabled(false),
              _cyclomatic_complexity_enabled_str(""), _cyclomatic_complexity_enabled(false),
              _ompss_mode_str(""), _ompss_mode_enabled(false),
              _function_str(""), _call_graph_str(""), _call_graph_enabled(true)
    {
        set_phase_name("Experimental phase for testing compiler analysis");
        set_phase_description("This is a temporal phase called with code testing purposes.");

        register_parameter("pcfg_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _pcfg_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_pcfg, this, std::placeholders::_1));

        register_parameter("use_def_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _use_def_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_use_def, this, std::placeholders::_1));

        register_parameter("liveness_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _liveness_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_liveness, this, std::placeholders::_1));

        register_parameter("reaching_defs_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _reaching_defs_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_reaching_defs, this, std::placeholders::_1));

        register_parameter("induction_vars_enabled",
                           "If set to '1' enables pcfg analysis, otherwise it is disabled",
                           _induction_vars_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_induction_vars, this, std::placeholders::_1));
                            
        register_parameter("tdg_enabled",
                           "If set to '1' enables tdg analysis, otherwise it is disabled",
                           _tdg_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_tdg, this, std::placeholders::_1));

        register_parameter("etdg_enabled",
                            "If set to '1' enables expanded-tdg analysis, otherwise it is disabled",
                            _etdg_enabled_str,
                            "0").connect(std::bind(&TestAnalysisPhase::set_etdg, this, std::placeholders::_1));

        register_parameter("range_analysis_enabled",
                           "If set to '1' enables range analysis, otherwise it is disabled",
                           _range_analysis_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_range_analsysis, this, std::placeholders::_1));
                            
        register_parameter("cyclomatic_complexity_enabled",
                           "If set to '1' enables cyclomatic complexity calculation, otherwise it is disabled",
                           _cyclomatic_complexity_enabled_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_cyclomatic_complexity, this, std::placeholders::_1));
                            
        register_parameter("ompss_mode",
                           "Enables OmpSs semantics instead of OpenMP semantics",
                           _ompss_mode_str,
                           "0").connect(std::bind(&TestAnalysisPhase::set_ompss_mode, this, std::placeholders::_1));

        register_parameter("functions",
                           "Points out the function that has to be analyzed",
                           _function_str,
                           "").connect(std::bind(&TestAnalysisPhase::set_functions, this, std::placeholders::_1));

        register_parameter("call_graph",
                           "If set to '1' enbles analyzing the call graph of all functions specified in parameter 'functions'",
                           _call_graph_str,
                           "1").connect(std::bind(&TestAnalysisPhase::set_call_graph, this, std::placeholders::_1));
    }

    void TestAnalysisPhase::run(TL::DTO& dto)
    {
        AnalysisBase analysis(_ompss_mode_enabled);

        Nodecl::NodeclBase ast = *std::static_pointer_cast<Nodecl::NodeclBase>(dto["nodecl"]);

        std::set<std::string> functions;
        tokenizer(_function_str, functions);

        // Test PCFG creation
        if (_pcfg_enabled)
        {
            if (VERBOSE)
                std::cerr << "====================  Testing PCFG creation  =================" << std::endl;
            analysis.parallel_control_flow_graph(ast, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "=================  Testing PCFG creation done  ===============" << std::endl;
        }

        if (_use_def_enabled)
        {
            if (VERBOSE)
                std::cerr << "==============  Testing Use-Definition analysis  ==============" << std::endl;
            analysis.use_def(ast, /*propagate_graph_nodes*/ true, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "============  Testing Use-Definition analysis done  ===========" << std::endl;
        }

        if (_liveness_enabled)
        {
            if (VERBOSE)
                std::cerr << "=================  Testing Liveness analysis  ==================" << std::endl;
            analysis.liveness(ast, /*propagate_graph_nodes*/ true, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "===============  Testing Liveness analysis done  ===============" << std::endl;
        }

        if (_reaching_defs_enabled)
        {
            if (VERBOSE)
                std::cerr << "===========  Testing Reaching Definitions analysis  ============" << std::endl;
            analysis.reaching_definitions(ast, /*propagate_graph_nodes*/ true, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "=========  Testing Reaching Definitions analysis done  =========" << std::endl;
        }

        if (_induction_vars_enabled)
        {
            if (VERBOSE)
                std::cerr << "=============  Testing Induction Variables analysis  ==========" << std::endl;
            analysis.induction_variables(ast, /*propagate_graph_nodes*/ true, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "==========  Testing Induction Variables analysis done  ========" << std::endl;
        }

        if (_range_analysis_enabled)
        {
            if (VERBOSE)
                std::cerr << "====================  Testing Range analysis  ===================" << std::endl;
            analysis.range_analysis(ast, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "==========  Testing Induction Variables analysis done  ==========" << std::endl;
        }
        
        ObjectList<TaskDependencyGraph*> tdgs;
        if (_tdg_enabled || _etdg_enabled)
        {
            if (VERBOSE)
                std::cerr << "====================  Testing TDG creation  ====================" << std::endl;
            tdgs = analysis.task_dependency_graph(
                ast, functions, _call_graph_enabled,
                /*taskparts*/false, _etdg_enabled);
            if (VERBOSE)
                std::cerr << "==================  Testing TDG creation done  =================" << std::endl;
        }
        
        if(_cyclomatic_complexity_enabled)
        {
            if (VERBOSE)
                std::cerr << "============  Testing Cyclomatic Complexity analysis  ===========" << std::endl;
            analysis.cyclomatic_complexity(ast, functions, _call_graph_enabled);
            if (VERBOSE)
                std::cerr << "=========  Testing Cyclomatic Complexity analysis done  =========" << std::endl;
        }
        
        if (debug_options.print_pcfg ||
            debug_options.print_pcfg_w_context ||
            debug_options.print_pcfg_w_analysis ||
            debug_options.print_pcfg_full)
        {
            if (VERBOSE)
                std::cerr << "=================  Printing PCFG to dot file  ==================" << std::endl;
            const ObjectList<ExtensibleGraph*>& pcfgs = analysis.get_pcfgs();
            for (ObjectList<ExtensibleGraph*>::const_iterator it = pcfgs.begin(); it != pcfgs.end(); ++it)
                analysis.print_pcfg((*it)->get_name());
            if (VERBOSE)
                std::cerr << "===============  Printing PCFG to dot file done  ===============" << std::endl;
        }
        
        if (debug_options.print_tdg)
        {
            if (VERBOSE)
                std::cerr << "==================  Printing TDG to dot file  =================" << std::endl;
            for (ObjectList<TaskDependencyGraph*>::iterator it = tdgs.begin(); it != tdgs.end(); ++it)
                analysis.print_tdg((*it)->get_name());
            if (VERBOSE)
                std::cerr << "===============  Printing TDG to dot file done  ===============" << std::endl;
        }
        
        if (debug_options.tdg_to_json)
        {
            if (VERBOSE)
                std::cerr << "==================  Printing TDG to json file  ================" << std::endl;
            analysis.tdgs_to_json(tdgs);
            if (VERBOSE)
                std::cerr << "===============  Printing TDG to json file done  ==============" << std::endl;
        }
    }

    void TestAnalysisPhase::set_pcfg(const std::string& pcfg_enabled_str)
    {
        if (pcfg_enabled_str == "1")
            _pcfg_enabled = true;
    }

    void TestAnalysisPhase::set_use_def(const std::string& use_def_enabled_str)
    {
        if (use_def_enabled_str == "1")
            _use_def_enabled = true;
    }

    void TestAnalysisPhase::set_liveness(const std::string& liveness_enabled_str)
    {
        if (liveness_enabled_str == "1")
            _liveness_enabled = true;
    }

    void TestAnalysisPhase::set_reaching_defs(const std::string& reaching_defs_enabled_str)
    {
        if (reaching_defs_enabled_str == "1")
            _reaching_defs_enabled = true;
    }

    void TestAnalysisPhase::set_induction_vars(const std::string& induction_vars_enabled_str)
    {
        if (induction_vars_enabled_str == "1")
            _induction_vars_enabled = true;
    }

    void TestAnalysisPhase::set_tdg(const std::string& tdg_enabled_str)
    {
        ERROR_CONDITION(_etdg_enabled,
                        "Expanded TDG and Flow TDG cannot be enabled at the same time",
                        0);

        if (tdg_enabled_str == "1")
            _tdg_enabled = true;
    }

    void TestAnalysisPhase::set_etdg(const std::string& etdg_enabled_str)
    {
        ERROR_CONDITION(_tdg_enabled,
                        "Expanded TDG and Flow TDG cannot be enabled at the same time",
                        0);
        
        if (etdg_enabled_str == "1")
            _etdg_enabled = true;
    }

    void TestAnalysisPhase::set_range_analsysis(const std::string& range_analysis_enabled_str)
    {
        if (range_analysis_enabled_str == "1")
            _range_analysis_enabled = true;
    }
    
    void TestAnalysisPhase::set_cyclomatic_complexity(const std::string& cyclomatic_complexity_enabled_str)
    {
        if (cyclomatic_complexity_enabled_str == "1")
            _cyclomatic_complexity_enabled = true;
    }
    
    void TestAnalysisPhase::set_ompss_mode(const std::string& ompss_mode_str)
    {
        if (ompss_mode_str == "1")
            _ompss_mode_enabled = true;
    }

    void TestAnalysisPhase::set_functions(const std::string& function_str)
    {
        if (function_str != "")
            _function_str = function_str;
    }

    void TestAnalysisPhase::set_call_graph(const std::string& call_graph_enabled_str)
    {
        if (call_graph_enabled_str == "0")
            _call_graph_enabled = false;
    }
}
}

EXPORT_PHASE(TL::Analysis::TestAnalysisPhase);
