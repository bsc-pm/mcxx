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
            ObjectList<AST_t> program_unit_trees = translation_unit.depth_subtrees(Fortran::ProgramUnit::predicate);
            for(ObjectList<AST_t>::iterator it = program_unit_trees.begin();
                    it != program_unit_trees.end();
                    ++it)
            {
                Fortran::ProgramUnit prog_unit(*it, _sl);
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
                egraph.build_CFG(pu.get_statements());
        
                egraph.print_graph_to_dot();
            }
        }
        else
        {   // C - CXX LANGUAGE
            ObjectList<AST_t> func_def_trees = translation_unit.depth_subtrees(FunctionDefinition::predicate);
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
                egraph.build_CFG(f.get_function_body());
                
                egraph.print_graph_to_dot();
            }
        }
    }
}

EXPORT_PHASE(TL::AnalysisPhase);