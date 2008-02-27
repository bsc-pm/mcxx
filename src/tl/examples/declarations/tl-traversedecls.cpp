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
    class WhileStatementASTFunctor : public TraverseASTFunctor
    {
        public:
            ASTTraversalResult operator()(AST_t &a) const
            {
                if (WhileStatement::predicate(a))
                {
                    return ast_traversal_result_helper(/* match */ true, /* recurse */ true);
                }
                else
                {
                    return ast_traversal_result_helper(/* match */ false, /* recurse */ true);
                }
            }
    };

    // This is a traverse functor, it will have the preorder
    // and postorder methods
    class WhileStatementFunctor : public TraverseFunctor
    {
        private:
            int num_whiles;
        public:
            WhileStatementFunctor()
                : num_whiles(0) { }

            void preorder(Context, AST_t) 
            { 
            }

            void postorder(Context ctx, AST_t ast) 
            { 
                num_whiles++;

                // Wrap the tree
                WhileStatement while_statement(ast, ctx.scope_link);

                Source lowered_while, 
                       condition_declaration, 
                       condition_expression,
                       while_body;

                lowered_while
                    << "loop_start_" << num_whiles << " : {"
                    <<  condition_declaration
                    <<   "if (" << condition_expression << ")"
                    <<   "{"
                    <<         while_body
                    <<         "goto loop_start_" << num_whiles << ";"
                    <<   "}"
                    << "}"
                    ;

                Condition condition = while_statement.get_condition();

                if (condition.is_declaration())
                {
                    Declaration declaration = condition.get_declaration();
                    ObjectList<DeclaredEntity> declared_entities = declaration.get_declared_entities();

                    condition_declaration 
                        << condition.prettyprint() << ";"
                        ;
                    condition_expression
                        << declared_entities[0].prettyprint()
                        ;
                }
                else if (condition.is_expression())
                {
                    condition_expression << condition.prettyprint();
                }

                while_body << while_statement.get_body().prettyprint();

                TL::AST_t lowered_while_tree = lowered_while.parse_statement(ast, ctx.scope_link);

                ast.replace(lowered_while_tree);
            }
    };

    class TraverseDecls : public CompilerPhase
    {
        public:
            TraverseDecls()
            {
            }

            void run(DTO& dto)
            {
                AST_t ast = dto["translation_unit"];
                ScopeLink scope_link = dto["scope_link"];

                DepthTraverse depth_traverse;
                WhileStatementFunctor while_statement_functor;
                WhileStatementASTFunctor traverse_ast_functor;

                depth_traverse.add_functor(traverse_ast_functor, while_statement_functor);
                depth_traverse.traverse(ast, scope_link);
            }
    };
}

EXPORT_PHASE(TL::TraverseDecls);
