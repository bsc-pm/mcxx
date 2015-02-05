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




#ifndef TL_DECLARATIONCLOSURE_HPP
#define TL_DECLARATIONCLOSURE_HPP

#include <utility>
#include <set>

#include "tl-common.hpp"
#include "tl-ast.hpp"
#include "tl-refptr.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"

namespace TL
{
    //! A class that represents a dependency relationship between
    // two symbols
    struct LIBTL_CLASS DependencyItem : public std::pair<Symbol, Symbol>
    {
        public:
            //! Creates a new dependency item
            /*!
             * \param first A symbol depending on \a second
             * \param second Symbol \a first depend on this one
             */
            DependencyItem(const Symbol &first, const Symbol &second)
                : std::pair<Symbol, Symbol>(first, second)
            {
            }
    };

    //! A class holding a set of dependencies between symbols
    class LIBTL_CLASS DeclarationDependency
    {
        private:
            // Dependency graph
            /*! Regard this as the edges of the graph */
            std::set<DependencyItem> _graph;
            // Items of the dependency graph.
            /*! Regard this as the vertices of the graph */
            std::set<Symbol> _items;
        public:
            //! Adds a symbol without any dependency
            /*!
             * \param sym Added symbol
             */
            void add_symbol(Symbol sym);
            //! Adds a symbol depending on another
            /*!
             * \param sym Added symbol
             * \param depends Symbol on which \a sym depends
             */
            void add_symbol_depending_on(Symbol sym, Symbol depends);

            // Returns the set of items
            std::set<Symbol>& items() 
            {
                return _items;
            }

            // Returns the graph of dependencies
            std::set<DependencyItem>& graph() 
            {
                return _graph;
            }
    };

    //! Class that computes the declaration closure
    /*!
     * Declaration closure means that given a set of symbols or types, this
     * class is able to return a Source that properly declares all them in the
     * convenient order, satisfying all the dependences that might arose
     * between declarations.
     *
     * The closure must be requested once all relevant symbols and types have
     * been added. Adding twice a symbol or type is not an error and might
     * happen implicitly because of dependences between declarations.
     */
    class LIBTL_CLASS DeclarationClosure
    {
        private:
            //! Dependencies
            DeclarationDependency _dependencies;
            //! Scope link
            ScopeLink _scope_link;

            //! Recursively add a type depending on a symbol
            bool add_type_rec(Type t, Symbol depending_symbol, std::set<Symbol> symbols_seen);

            //! Recursively add a symbol depending on another
            bool add_dependent_symbol(Symbol sym, Symbol depending_symbol, std::set<Symbol> symbols_seen);

            //! Declares an entity
            void declare_entity(Source &source_result,
                    Symbol declared_symbol,
                    std::set<Symbol> &items,
                    std::set<DependencyItem> &graph);
        public:
            //! Add a symbol that will be in the declaration closure
            void add(Symbol s);
            //! Add a type that will be in the declaration closure
            void add(Type t);

            //! Get the closure
            Source closure();

            //! Constructor
            DeclarationClosure(ScopeLink scope_link)
                : _scope_link(scope_link)
            {
            }
    };
}

#endif // TL_DECLARATIONCLOSURE_HPP
