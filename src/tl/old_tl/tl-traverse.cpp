/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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
--------------------------------------------------------------------*/




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

        if (!node.is_valid())
            return;

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
