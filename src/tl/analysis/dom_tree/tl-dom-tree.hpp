/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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



#ifndef TL_DOMINATOR_TREE_HPP
#define TL_DOMINATOR_TREE_HPP

#include <set>

#include "tl-symbol.hpp"

namespace TL {
namespace Analysis {

    class DomTreeNode
    {
    private:
        Symbol _func_sym;
        std::set<DomTreeNode*> _predecessors;
        std::set<DomTreeNode*> _successors;

    public:
        DomTreeNode(Symbol func_sym);

        void add_predecessor(DomTreeNode* pred);
        void add_successor(DomTreeNode* succ);

        Symbol get_function_symbol() const;
        Nodecl::FunctionCode get_function_code();
        std::set<DomTreeNode*> get_predecessors();
        std::set<DomTreeNode*> get_successors();
    };

    class LIBTL_CLASS DominatorTree
    {
    private:
        std::string _name;
        std::set<DomTreeNode*> _roots;
        std::set<DomTreeNode*> _leafs;
        ObjectList<Symbol> _func_syms;

        void create_dominator_tree(const Nodecl::NodeclBase& nodecl);

    public:
        //! Constructor to build a new DominatorTree.
        /*! \param name Name which will identify the graph.
         *! \param name Nodecl Nodecl AST from which to build the Dominator Tree
         */
        DominatorTree(std::string name, const Nodecl::NodeclBase& nodecl);

        // *** Getters and Setters *** //
        std::string get_name() const;
        std::set<DomTreeNode*> get_roots() const;
        std::set<DomTreeNode*> get_leafs() const;
        ObjectList<Symbol> get_function_symbols() const;

        // *** Utils *** //
        void print_to_dot();
    };

}
}

#endif // TL_DOMINATOR_TREE_HPP
