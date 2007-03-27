/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
        CondAction c(&pred, &functor);
        _pred_list.push_back(c);
    }

    void DepthTraverse::traverse(TL::AST_t node, ScopeLink scope_link)
    {
        TraverseFunctor no_op;
        TraverseFunctor* functor = &no_op;

        for (unsigned int i = 0; i < _pred_list.size(); i++)
        {
            Predicate<AST_t>* pred = _pred_list[i].first;

            if ((*pred)(node))
            {
                functor = _pred_list[i].second;
                break;
            }
        }

        AST ast = node._ast;

        Context ctx(scope_link);

        functor->preorder(ctx, node);

        for (int i = 0; i < ASTNumChildren(ast); i++)
        {
            AST child = ASTChild(ast, i);

            if (child != NULL)
            {
                AST_t w_child(child);
                traverse(w_child, scope_link);
            }
        }

        functor->postorder(ctx, node);
    }
}
