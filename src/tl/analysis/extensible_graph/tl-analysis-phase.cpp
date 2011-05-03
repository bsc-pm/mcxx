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


#include "tl-analysis-phase.hpp"
#include "cxx-utils.h"
#include "extensible_graph.hpp"
#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

namespace TL
{
    AnalysisPhase::AnalysisPhase()
        : _func_defs(), _prog_units(), _sl()
    {}
    
    void AnalysisPhase::run(TL::DTO& dto)
    {
        _sl = dto["scope_link"];
        TL::AST_t translation_unit = dto["translation_unit"];        
        
        if (IS_FORTRAN_LANGUAGE)
        {
            ObjectList<AST_t> program_unit_trees = 
                translation_unit.depth_subtrees(Fortran::ProgramUnit::predicate);
            for(ObjectList<AST_t>::iterator it = program_unit_trees.begin();
                it != program_unit_trees.end();
                ++it)
            {
                Fortran::ProgramUnit prog_unit(*it, _sl);
                AST_t prog_u = prog_unit.get_ast();
                _prog_units.push_back(prog_unit);
            }
            
            // Build the CFG
            if (!_prog_units.empty())
            {
                Fortran::ProgramUnit pu = _prog_units[0];
                std::string graph_name = pu.get_ast().get_file();
                graph_name = graph_name.substr(0, graph_name.find_last_of(".")) +
                                "_" + pu.get_related_symbol().get_name();
                
                ExtensibleGraph egraph(_sl, graph_name);
                
                ObjectList<Statement> stmts = pu.get_statements();
                
                std::cout << std::endl << "**************** Building graph '" 
                                       << graph_name 
                                       << "' ****************" << std::endl;
                egraph.build_CFG(stmts);
                std::cout << "**************** END Building graph '" 
                          << graph_name 
                          << "' ****************" << std::endl << std::endl;
        
                std::cout << std::endl << "**************** Analysing graph '" 
                                       << graph_name 
                                       << "' ****************" << std::endl;
                egraph.live_variable_analysis();
                std::cout << "**************** END Analysing graph '" 
                          << graph_name
                          << "' ****************" << std::endl << std::endl;
                
                std::cout << std::endl << "**************** Printing DOT graph '" 
                                       << graph_name 
                                       << "' ****************" << std::endl;
                egraph.print_graph_to_dot();
                std::cout << "**************** END Printing DOT graph '" 
                          << graph_name 
                          << "' ****************" << std::endl << std::endl;
            }
        }
        else
        {   // C - CXX LANGUAGE
            ObjectList<AST_t> func_def_trees = 
                translation_unit.depth_subtrees(FunctionDefinition::predicate);
            for(ObjectList<AST_t>::iterator it = func_def_trees.begin();
                it != func_def_trees.end();
                ++it)
            {
                FunctionDefinition func_def(*it, _sl);
                _func_defs.push_back(func_def);
            }
            
            // Build the CFG for each function in the TranslationUnit
            for(ObjectList<FunctionDefinition>::iterator it = _func_defs.begin();
                it != _func_defs.end();
                ++it)
            {
                FunctionDefinition f = *it;
                std::string graph_name = f.get_ast().get_file();
                graph_name = graph_name.substr(0, graph_name.find_last_of(".")) +
                             "_" + f.get_function_name().prettyprint();
                
                ExtensibleGraph egraph(_sl, graph_name);
                
                std::cout << std::endl << "**************** Building graph '" 
                                       << graph_name 
                                       << "' ****************" << std::endl;
                egraph.build_CFG(f.get_function_body());
                std::cout << "**************** END Building graph '" 
                          << graph_name 
                          << "' ****************" << std::endl << std::endl;
                
                std::cout << std::endl << "**************** Analysing graph '" 
                                       << graph_name 
                                       << "'****************" << std::endl;
                egraph.live_variable_analysis();
                std::cout << "**************** END Analysing graph '" 
                          << graph_name 
                          << "'****************" << std::endl << std::endl;
                
                std::cout << std::endl << "**************** Printing DOT graph '" 
                                       << graph_name 
                                       << "' ****************" << std::endl;
                egraph.print_graph_to_dot();
                std::cout << "**************** END Printing DOT graph '" 
                          << graph_name 
                          << "' ****************" << std::endl << std::endl;
            }
        }
    }
}

EXPORT_PHASE(TL::AnalysisPhase);