#ifndef TL_DECLARATIONCLOSURE_HPP
#define TL_DECLARATIONCLOSURE_HPP

#include <utility>
#include <set>

#include "tl-ast.hpp"
#include "tl-refptr.hpp"
#include "tl-symbol.hpp"
#include "tl-source.hpp"

namespace TL
{
    //! A class that represents a dependency relationship between
    // two symbols
    struct DependencyItem : public std::pair<Symbol, Symbol>
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
    class DeclarationDependency
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
     *
     * \bug At the moment the declaration closure does not handle the case
     * where there is some sort of cyclic dependence between types like
     * classes and their forward declarations.
     */
    class DeclarationClosure
    {
        private:
            //! Dependencies
            DeclarationDependency _dependencies;
            //! Scope link
            ScopeLink _scope_link;

            //! Recursively add a type depending on a symbol
            void add_type_rec(Type t, Symbol depending_symbol);

            //! Recursively add a symbol depending on another
            void add_dependent_symbol(Symbol sym, Symbol depending_symbol);

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
