/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2008 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
*/
#include "tl-compilerphase.hpp"
#include "tl-traverse.hpp"
#include "tl-langconstruct.hpp"
#include "tl-ast.hpp"

namespace TL
{
    // This is a TraverseASTFunctor, something that gets an AST_t
    // and returns an ASTTraversalResult
    class FunctionDefinitionASTFunctor : public TraverseASTFunctor
    {
        public:
            ASTTraversalResult operator()(AST_t &a) const
            {
                if (FunctionDefinition::predicate(a))
                {
                    return ast_traversal_result_helper(/* match */ true, /* recurse */ false);
                }
                else
                {
                    return ast_traversal_result_helper(/* match */ false, /* recurse */ true);
                }
            }
    };

    // This is a traverse functor, it will have the preorder
    // and postorder methods
    class FunctionDefinitionFunctor : public TraverseFunctor
    {
        public:
            void preorder(Context ctx, AST_t ast) 
            { 
                ScopeLink sl = ctx.scope_link;
                // Wrap into a FunctionDefinition since we know it is
                FunctionDefinition function_definition(ast, sl);

                // Get the function name
                IdExpression function_name = 
                    function_definition.get_function_name();

                // Get the body of the function definition
                Statement stm = function_definition.get_function_body();

                // Now get all function calls
                ObjectList<AST_t> function_calls = 
                    stm.get_ast().depth_subtrees(
                            PredicateAST<LANG_IS_FUNCTION_CALL>()
                            );

                std::cout 
                    << function_name.prettyprint() 
                    << " [label=\"" 
                    << function_name.prettyprint() 
                    << "\"]"
                    << std::endl;

                for (ObjectList<AST_t>::iterator it = function_calls.begin();
                        it != function_calls.end();
                        it++)
                {
                    Expression expr(*it, sl);

                    // Check if the called entity is a simple name
                    if (expr.get_called_expression().is_id_expression())
                    {
                        IdExpression id_expression = 
                            expr.get_called_expression().get_id_expression();
                        Symbol sym = id_expression.get_symbol();

                        // And check if it is a function (and not a pointer to
                        // function)
                        if (sym.is_function())
                        {
                            std::cout 
                                << function_name.prettyprint() 
                                << " -> " 
                                << id_expression.prettyprint()
                                << " [label=\"" << id_expression.get_ast().get_locus() << "\"]"
                                << std::endl;
                        }
                    }
                }
            }
            void postorder(Context, AST_t) { }
    };

    class TraverseDecls : public CompilerPhase
    {
        public:
            TraverseDecls()
            {
            }

            void run(DTO& dto)
            {
                std::cout << "digraph static_callgraph {" << std::endl;

                AST_t ast = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];

                DepthTraverse depth_traverse;
                FunctionDefinitionFunctor function_definition_functor;
                FunctionDefinitionASTFunctor traverse_ast_functor;

                depth_traverse.add_functor(traverse_ast_functor, function_definition_functor);
                depth_traverse.traverse(ast, scope_link);

                std::cout << "}" << std::endl;
            }
    };
}

EXPORT_PHASE(TL::TraverseDecls);
