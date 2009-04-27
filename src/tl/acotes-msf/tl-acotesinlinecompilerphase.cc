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
#include "tl-acotesinlinecompilerphase.h"

#include <assert.h>
#include <tl-traverse.hpp>
#include "tl-acoteslogger.h"
#include "tl-acotesreplaceidexpression.h"

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * CompilerPhase management
     * ****************************************************************/    
    
    /**
     * Constructor.
     */
    AcotesInlineCompilerPhase::AcotesInlineCompilerPhase() 
    : PragmaCustomCompilerPhase("acotes")
    , pragmaDispatcher("acotes", on_directive_pre, on_directive_post)
    {
        on_directive_pre["inline"].connect(
            functor(&AcotesInlineCompilerPhase::onPreInlineConstruct, *this)
            );
        on_directive_post["inline"].connect(
            functor(&AcotesInlineCompilerPhase::onPostInlineConstruct, *this)
            );
    }
    
    /**
     * Destructor.
     */
    AcotesInlineCompilerPhase::~AcotesInlineCompilerPhase() {
        
    }
    
    /**
     * AcotesCompilerPhase implementation.
     */
    void 
    AcotesInlineCompilerPhase::run(DTO& data_flow)
    {
        // get the translation_unit tree
        AST_t translation_unit = data_flow["translation_unit"];
        // get the scope_link
        ScopeLink scope_link = data_flow["scope_link"];
        // Get the global_scope
        Scope global_scope = scope_link.get_scope(translation_unit);

        // Instantiate a DepthTraverse
        DepthTraverse depth_traverse;

        PredicateAttr pragmaCustomDirectivePred(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE) ;
        PredicateAttr pragmaCustomConstructPred(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT) ;
        PredicateAttr functionCallPred(LANG_IS_FUNCTION_CALL) ;
//        PredicateAttr function_call_pred(LANG_IS_FUNCTION_CALL) ;

        depth_traverse.add_predicate(pragmaCustomDirectivePred, pragmaDispatcher);
        depth_traverse.add_predicate(pragmaCustomConstructPred, pragmaDispatcher);
        depth_traverse.add_predicate(functionCallPred, *this);

        // Parse everything
        depth_traverse.traverse(translation_unit, scope_link);
    }


    
    /* ****************************************************************
     * * CompilerPhase events
     * ****************************************************************/
    
    void AcotesInlineCompilerPhase::onPreInlineConstruct(PragmaCustomConstruct construct)
    {
        FunctionDefinition functionDefinition= construct.get_enclosing_function();
        DeclaredEntity declaredEntity= functionDefinition.get_declared_entity();
        Symbol symbol= declaredEntity.get_declared_symbol();
        functionMap[symbol]= new FunctionDefinition(functionDefinition);
    }

    void AcotesInlineCompilerPhase::onPostInlineConstruct(PragmaCustomConstruct construct)
    {
        construct.get_enclosing_function().get_ast().remove_in_list();
        construct.get_ast().remove_in_list();
    }
    
    
    
    /* ****************************************************************
     * * TraverseFunction management
     * ****************************************************************/    

    /**
     * Travese functor implementation.
     */
    void 
    AcotesInlineCompilerPhase::preorder(Context ctx, AST_t node)
    {
        PredicateAttr isFunctionCall(LANG_IS_FUNCTION_CALL) ;
        
        if (isFunctionCall(node)) {
            
        }
    }

    /**
     * TraverseFunctor implementation.
     */
    void 
    AcotesInlineCompilerPhase::postorder(Context ctx, AST_t node)
    {
        // f(something) expression from node
        Expression functionCall(node, ctx.scope_link);
        assert(functionCall.is_function_call());
        
        // obtain the f if it is a symbol, it shouldn't be something like t[n]()
        Expression calledExpression= functionCall.get_called_expression();
        if (calledExpression.is_id_expression())
        {
            // Obtaint f identifier expression and symbol
            IdExpression idExpression= calledExpression.get_id_expression();
            Symbol callSymbol= idExpression.get_symbol();
            
            // If f is an inline function replace it
            if (functionMap.count(callSymbol)) {
                // Function definition to be replaced
                FunctionDefinition functionDefinition= *functionMap[callSymbol];
                DeclaredEntity declaredEntity= functionDefinition.get_declared_entity();
                ObjectList<ParameterDeclaration> parameterDeclarations= declaredEntity.get_parameter_declarations();
                ObjectList<Expression> argumentList= functionCall.get_argument_list();
                
                // Ensure formar parameters and arguments matchs
                if (parameterDeclarations.size() != argumentList.size()) {
                    AcotesLogger::error(&functionCall)
                            << "arguments doesn't match width declaration"
                            << std::endl;
                    AcotesLogger::info(&functionDefinition)
                            << "this is the function definition"
                            << "[" << declaredEntity.prettyprint() << "]"
                            << std::endl;
                    return;
                }
                
                // Create the replacement of parameters for arguments
                AcotesReplaceIdExpression replace;
                for (unsigned i= 0; i < parameterDeclarations.size(); i++) {
                    // For each paramater symbol...
                    ParameterDeclaration parameter= parameterDeclarations.at(i);
                    IdExpression parameterId= parameter.get_name();
                    Symbol parameterSymbol= parameterId.get_symbol();

                    // ...replace by the argument ast of the expression...
                    Expression argument= argumentList.at(i);
                    
                    // ... with parentesis
                    Source parentesisSource;
                    parentesisSource << "(" << argument.prettyprint() << ")";
                    AST_t parentesisAST= parentesisSource.parse_expression(argument.get_ast(), argument.get_scope_link());
                    
                    // Request the replacement of parameter<-argument
                    replace.add(parameterSymbol, parentesisAST);
                }
                
                // Create a new instance of the function body...
                Statement body= functionDefinition.get_function_body();
                Source replacementSource= body.prettyprint();
                AST_t replacementAST= replacementSource.parse_statement(body.get_ast(), body.get_scope_link());
                // ...replacing the parameters with the arguments
                replace.setRefTree(functionCall.get_ast());
                replace.replace(replacementAST, functionCall.get_scope_link());
                
                Source finallySource= replacementAST.prettyprint();
                AST_t finallyAST= finallySource.parse_statement(functionCall.get_ast(), functionCall.get_scope_link());
                // Replace the function call with the modified function body
                functionCall.get_ast().replace(finallyAST);
            }
        }
    }

    
    
} /* end namespace Acotes */ } /* end namespace TL */
