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
#include "tl-traverse.hpp"

namespace TL
{
    void DepthTraverse::add_predicate(Predicate<AST_t>& pred, TraverseFunctor& functor)
    {
        TraverseASTFunctor* ast_functor = new TraverseASTPredicate(pred);
        // Save this, otherwise we will leak memory
        _to_be_freed.push_back(ast_functor);

        CondAction c(ast_functor, &functor);
        _pred_list.push_back(c);
    }

    DepthTraverse::~DepthTraverse()
    {
        // Free helpers created in 
        // DepthTraverse::add_predicate(Predicate<AST_t>& pred, TraverseFunctor& functor)
        for (std::vector<TraverseASTFunctor*>::iterator it = _to_be_freed.begin();
                it != _to_be_freed.end(); 
                it++)
        {
            delete (*it);
        }
    }

    void DepthTraverse::add_functor(TraverseASTFunctor& ast_functor, TraverseFunctor& functor)
    {
        CondAction c(&ast_functor, &functor);
        _pred_list.push_back(c);
    }

    void DepthTraverse::traverse(TL::AST_t node, ScopeLink scope_link)
    {
        bool recurse = true;

        std::vector<TraverseFunctor*> matching_functors;

        for (std::vector<CondAction>::iterator it = _pred_list.begin();
                it != _pred_list.end();
                it++)
        {
            TraverseASTFunctor& pred = *(it->first);
            ASTTraversalResult result = pred(node);

            bool match = result.matches();

            // Only recurse if no matching functor objected to recursion
            if (match)
            {
                recurse &= result.recurse();
                matching_functors.push_back(it->second);
                break;
            }
        }

        // Create context
        Context* ctx = new Context(scope_link);

        // Run all matching preorders
        for (std::vector<TraverseFunctor*>::iterator it = matching_functors.begin();
                it != matching_functors.end();
                it++)
        {
            (*it)->preorder(*ctx, node);
        }

        if (recurse)
        {
            AST ast = node._ast;
            for (int i = 0; i < ASTNumChildren(ast); i++)
            {
                AST child = ASTChild(ast, i);

                if (child != NULL)
                {
                    AST_t w_child(child);
                    traverse(w_child, scope_link);
                }
            }
        }

        // Run all matching postorders
        for (std::vector<TraverseFunctor*>::iterator it = matching_functors.begin();
                it != matching_functors.end();
                it++)
        {
            (*it)->postorder(*ctx, node);
        }

        // Delete it
        delete ctx;
    }
}
