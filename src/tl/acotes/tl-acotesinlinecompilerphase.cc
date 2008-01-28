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

        PredicateAST<LANG_IS_PRAGMA_CUSTOM_DIRECTIVE> pragmaCustomDirectivePred;
        PredicateAST<LANG_IS_PRAGMA_CUSTOM_CONSTRUCT> pragmaCustomConstructPred;
        PredicateAST<LANG_IS_FUNCTION_CALL> functionCallPred;
//        PredicateAST<LANG_IS_FUNCTION_CALL> function_call_pred;

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
#if 0
        PredicateAST<LANG_IS_FOR_STATEMENT> forStatementPred;
        
        if (forStatementPred(node)) {
            ForStatement forStatement(node, ctx.scope_link);
            if (forStatement.regular_loop()) {
                AcotesStack::forStatementPush(forStatement);
            }
        } else {
            assert(0);
        }
//        AcotesTaskCall acotes_task_call(node, ctx.scope_link);
//        Task* task= acotes_task_call.get_task();
//        if (task) 
//        {
//            if (TaskStack::top()) {
//                on_task_call_pre(acotes_task_call);
//            } else {
//                ErrorReport::I()->warning(node)
//                    << "found function call to task "
//                    << "'" << task->get_name() << "'"
//                    << " outside to task or taskgroup, will not be streamized"
//                    << std::endl;
//            }
//        }
#endif
    }

    /**
     * TraverseFunctor implementation.
     */
    void 
    AcotesInlineCompilerPhase::postorder(Context ctx, AST_t node)
    {
        Expression expression(node, ctx.scope_link);
        assert(expression.is_function_call());
        Expression calledExpression= expression.get_called_expression();
        if (calledExpression.is_id_expression())
        {
            IdExpression idExpression= calledExpression.get_id_expression();
            Symbol callSymbol= idExpression.get_symbol();
            if (functionMap.count(callSymbol)) {
                FunctionDefinition functionDefinition= *functionMap[callSymbol];
                DeclaredEntity declaredEntity= functionDefinition.get_declared_entity();
                ObjectList<ParameterDeclaration> parameterDeclarations= declaredEntity.get_parameter_declarations();
                ObjectList<Expression> argumentList= expression.get_argument_list();
                
                assert(parameterDeclarations.size() == argumentList.size());
                
                ReplaceIdExpression replace;
                for (unsigned i= 0; i < parameterDeclarations.size(); i++) {
                    ParameterDeclaration parameter= parameterDeclarations.at(i);
                    Expression argument= argumentList.at(i);
                    
                    IdExpression parameterId= parameter.get_name();
                    Symbol parameterSymbol= parameterId.get_symbol();
                    
                    replace.add_replacement(parameterSymbol, argument.get_ast());
                }
                
                Statement body= functionDefinition.get_function_body();
                Statement replacement= replace.replace(body);
                
                Source source= replacement.prettyprint();
                AST_t s= source.parse_statement(expression.get_ast(), expression.get_scope_link());
                expression.get_ast().replace(s);
            }
        }
        #if 0
        PredicateAST<LANG_IS_FOR_STATEMENT> forStatementPred;
        
        if (forStatementPred(node)) {
            ForStatement forStatement(node, ctx.scope_link);
            if (forStatement.regular_loop()) {
                AcotesStack::forStatementPop();
            }
        } else {
            assert(0);
        }
        #endif
//        AcotesTaskCall acotes_task_call(node, ctx.scope_link);
//        Task* task= acotes_task_call.get_task();
//        if (task)
//        {
//            if (TaskStack::top()) {
//                on_task_call_post(acotes_task_call);
//            } 
//        }
    }

    
    
} /* end namespace Acotes */ } /* end namespace TL */
