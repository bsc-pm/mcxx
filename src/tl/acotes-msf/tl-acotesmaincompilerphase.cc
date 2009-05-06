/*
    Acotes Translation Phase
    Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
    
    $Id: tl-acotestransform.cpp 1611 2007-07-10 09:28:44Z drodenas $
*/
#include "tl-acotesmaincompilerphase.h"

#include <assert.h>

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * CompilerPhase management
     * ****************************************************************/    

    /**
     * Constructor.
     */
    AcotesMainCompilerPhase::AcotesMainCompilerPhase() 
    {
    }
    
    /**
     * Destructor.
     */
    AcotesMainCompilerPhase::~AcotesMainCompilerPhase() {
    }

    void 
    AcotesMainCompilerPhase::pre_run(DTO& data_flow)
    {
    }
    
    /**
     * AcotesCompilerPhase implementation.
     */
    void 
    AcotesMainCompilerPhase::run(DTO& data_flow)
    {
        // get the translation_unit tree
        AST_t translation_unit = data_flow["translation_unit"];
        // get the scope_link
        ScopeLink scope_link = data_flow["scope_link"];
        // Get the global_scope
        Scope global_scope = scope_link.get_scope(translation_unit);

        // Instantiate a DepthTraverse
        DepthTraverse depth_traverse;

        PredicateAttr functionCallPred(LANG_IS_FUNCTION_DEFINITION) ;

        depth_traverse.add_predicate(functionCallPred, *this);

        // Parse everything
        depth_traverse.traverse(translation_unit, scope_link);
    }

    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    
    /* ****************************************************************
     * * TraverseFunction management
     * ****************************************************************/    

    /**
     * Travese functor implementation.
     */
    void 
    AcotesMainCompilerPhase::preorder(Context ctx, AST_t node)
    {
    }

    /**
     * TraverseFunctor implementation.
     */
    void 
    AcotesMainCompilerPhase::postorder(Context ctx, AST_t node)
    {
        FunctionDefinition functionDefinition(node, ctx.scope_link);
        DeclaredEntity declaration= functionDefinition.get_declared_entity();
        Symbol symbol= declaration.get_declared_symbol();
        std::string name= symbol.get_name();
        std::string main= "main";
        if (name == main) {
            ObjectList<ParameterDeclaration> parameters= declaration.get_parameter_declarations();
            
            Source acotesMainSource;
            acotesMainSource << "int acotescc__main(";
            for (unsigned i= 0; i < parameters.size(); i++) {
                if (i > 0) {
                    acotesMainSource << ",";
                }
                ParameterDeclaration parameter= parameters.at(i);
                acotesMainSource << parameter.prettyprint();
            }
            acotesMainSource << ")";
            acotesMainSource << "{";
            acotesMainSource << "msf_resources_t resources;";
            acotesMainSource << "resources.processor_type_0 = 4;";
            acotesMainSource << "resources.processor_type_1 = 0;";
            acotesMainSource << "msf_init(&resources);";
            acotesMainSource << functionDefinition.get_function_body().prettyprint();
            acotesMainSource << "msf_shutdown();";
            acotesMainSource << "return 0;";
            acotesMainSource << "}";
            AST_t acotesMain= acotesMainSource.parse_global(functionDefinition.get_ast(), functionDefinition.get_scope_link());
            functionDefinition.prepend_sibling(acotesMain);
            
            Source mainSource;
            mainSource << "int acotes__bs[16][16][16][2];";
            mainSource << "void genDefaultBufferSizes (void) {"
                       << "   int i, j, k;"
                       << "   for (i=0; i<16; i++) {"
                       << "      for (j=0; j<64; j++) {"
                       << "         for (k=0; k<64; k++) {"
                       << "            acotes__bs[i][j][k][0] = 10240;"
                       << "            acotes__bs[i][j][k][1] = 1024;"
                       << "         }"
                       << "      }"
                       << "   }"
                       << "}";
            mainSource << "void readBufferDescription (FILE * f) {"
                       << "   int n, tg, task, port, buffs, bs;"
                       << "   n = fscanf (f, \" %d %d %d %d %d \\n\", &tg, &task, &port, &buffs, &bs);"
                       << "   while (n==5) {"
                       << "      acotes__bs[tg][task][port][1] = bs;"
                       << "      acotes__bs[tg][task][port][0] = buffs*bs;"
                       << "      fprintf (stderr, \"tg %d task %d port %d buffs %d size %d\\n\", tg, task, port, buffs, bs);"
                       << "      n = fscanf (f, \" %d %d %d %d %d \\n\", &tg, &task, &port, &buffs, &bs);"
                       << "   }"
                       << "   if (n>0) fprintf (stderr, \"Incomplete last line"
                       << " in buffer description file\\n\");"
                       << "}";
            mainSource << "void getBufferDescription (void) {"
                       << "   FILE * f;"
                       << "   char * ev = getenv (\"ACOTES_BUFFER_DESC\");"
                       << "   if (ev != 0L) {"
                       << "      fprintf (stderr, \"Using file %s\\n\", ev);"
                       << "      f = fopen (ev, \"r\");"
                       << "      if (f!=0L) {"
                       << "         readBufferDescription (f);"
                       << "         fclose(f);"
                       << "      }"
                       << "      else {"
                       << "         fprintf (stderr, \"File %s not found. Using compiled buffer-sizes\\n\", ev);"
                       << "         genDefaultBufferSizes ();"
                       << "      }"
                       << "   }"
                       << "   else {"
                       << "      fprintf (stderr, \"Using compiled buffer-sizes\\n\");"
                       << "      genDefaultBufferSizes ();"
                       << "   }"
                       << "}";

            mainSource << "int main(int argc, char**argv";
            for (unsigned i= 2; i < parameters.size(); i++) {
                if (i > 0) {
                    mainSource << ",";
                }
                ParameterDeclaration parameter= parameters.at(i);
                mainSource << parameter.prettyprint();
            }
            mainSource << ")"
                    << "{"
                    << "  int result;"
                    << "     getBufferDescription();"
                    //<< "   trace_app_begin(argv[0]);"
                    << "   result= acotescc__main(";
            if (parameters.size() > 0) {
                mainSource << "argc";
            }
            if (parameters.size() > 1) {
                mainSource << ", argv";
            }
            for (unsigned i= 2; i < parameters.size(); i++) {
                if (i > 0) {
                    mainSource << ",";
                }
                ParameterDeclaration parameter= parameters.at(i);
                mainSource << parameter.get_name().prettyprint();
            }
            mainSource << ");"
                    //<< "   trace_app_end();"
                    << "  return result;"
                    << "}"
                    ;
            AST_t main= mainSource.parse_global(functionDefinition.get_ast(), functionDefinition.get_scope_link());
            functionDefinition.get_ast().replace(main);
        }
    }

    
    
} /* end namespace Acotes */ } /* end namespace TL */
