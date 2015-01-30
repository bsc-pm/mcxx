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




#ifndef TL_TRAVERSE_HPP
#define TL_TRAVERSE_HPP

#include "tl-common.hpp"
#include <vector>
#include "tl-context.hpp"
#include "tl-scopelink.hpp"
#include "tl-ast.hpp"
#include "tl-predicate.hpp"

namespace TL
{
    //! \addtogroup Traverse Traverse classes
    //! @{

    //! This class is the 'functor' used when traversing a tree
    class LIBTL_CLASS TraverseFunctor
    {
        private:
        public:
            //! Method invoked when the node is found in preorder. \see DepthTraverse
            virtual void preorder(Context, AST_t) { }
            //! Method invoked when the node is found in postorder. \see DepthTraverse
            virtual void postorder(Context, AST_t) { }

            virtual ~TraverseFunctor() { }
    };

    //! Base class for traversals
    class LIBTL_CLASS Traverse { };

    //! Class that implements a depth-first traverse along the tree
    /*!
     * This class implements the traversal as follows. By using function
     * add_functor, several TraverseASTFunctor are registered each one with
     * a, possibly repeated, TraverseFunctor.
     *
     * Then with function traverse, the traversal starts. Every node is checked
     * against every TraverseASTFunctor registered. If the functor returns
     * match then the associated TraverseFunctor will be run its methods
     * TraverseFunctor::preorder and TraverseFunctor::postorder.
     *
     * Since this is a depth-traverse, preorder means the moment when a
     * matching node is found for the first time. Then, preorder is run.  After
     * this, recursively, its children are traversed.  Once all children have
     * been traversed, postorder is run.
     *
     * Preorder can be also understood as when going from root to leaves and
     * postorder when coming back from leaves and going towards root.
     *
     */
    class LIBTL_CLASS DepthTraverse : public Traverse
    {
        private:
            typedef std::pair<TraverseASTFunctor*, TraverseFunctor*> CondAction;
            std::vector<CondAction> _pred_list;
            std::vector<TraverseASTFunctor*> _to_be_freed;
        public:
            //! Adds a predicate for the traversal
            /*!
             * \param pred Predicate telling whether a node matches
             * \param functor TraverseFunctor used to handle nodes that match \a pred
             *
             * A TraverseASTFunctor is built after \a pred .
             *
             * \deprecated Use add_functor instead.
             */
            void add_predicate(Predicate<AST_t>& pred, TraverseFunctor& functor);
            //! Adds a functor for the traversal
            /*!
             * \param ast_functor TraverseASTFunctor telling whether the current node matches or not and whether it must recurse or not
             * \param functor TraverseFunctor used to handle nodes that match \a ast_functor
             *
             * \remark If \a ast_functor does not match children will be traversed. Only when \a ast_functor returns a match it can change
             * the recursion behaviour.
             */
            void add_functor(TraverseASTFunctor& ast_functor, TraverseFunctor& functor);

            //! Traverses the tree
            /*!
             * \param node Tree where the traversal will start
             * \param scope_link ScopeLink used for the traversal
             */
            void traverse(AST_t node, ScopeLink scope_link);

            ~DepthTraverse();
    };
    
    //! @}
}

#endif // TL_TRAVERSE_HPP
