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
#ifndef TL_TRAVERSE_HPP
#define TL_TRAVERSE_HPP

#include <vector>
#include "tl-context.hpp"
#include "tl-scopelink.hpp"
#include "tl-ast.hpp"
#include "tl-predicate.hpp"

namespace TL
{
    class TraverseFunctor
    {
        private:
        public:
            virtual void preorder(Context, AST_t) { }
            virtual void postorder(Context, AST_t) { }

            virtual ~TraverseFunctor() { }
    };

    class Traverse { };

    class DepthTraverse : public Traverse
    {
        private:
            typedef std::pair<TraverseASTFunctor*, TraverseFunctor*> CondAction;
            std::vector<CondAction> _pred_list;
            std::vector<TraverseASTFunctor*> _to_be_freed;
        public:
            void add_predicate(Predicate<AST_t>& pred, TraverseFunctor& functor);
            void add_functor(TraverseASTFunctor& ast_functor, TraverseFunctor& functor);
            void traverse(AST_t node, ScopeLink scope_link);

            ~DepthTraverse();
    };
}

#endif // TL_TRAVERSE_HPP
