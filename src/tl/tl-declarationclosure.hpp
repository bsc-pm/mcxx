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
    struct DependencyItem : public std::pair<Symbol, Symbol>
    {
        public:
            DependencyItem(const Symbol &first, const Symbol &second)
                : std::pair<Symbol, Symbol>(first, second)
            {
            }
    };

    class DeclarationDependency
    {
        private:
            std::set<DependencyItem> _graph;
            std::set<Symbol> _items;
        public:
            void add_symbol(Symbol sym);
            void add_symbol_depending_on(Symbol sym, Symbol depends);

            std::set<Symbol>& items() 
            {
                return _items;
            }

            std::set<DependencyItem>& graph() 
            {
                return _graph;
            }
    };

    class DeclarationClosure
    {
        private:
            DeclarationDependency _dependencies;
            ScopeLink _scope_link;

            void add_type_rec(Type t, Symbol depending_symbol);

            void add_dependent_symbol(Symbol sym, Symbol depending_symbol);

        public:
            void add(Symbol s);
            void add(Type t);

            Source closure();

            DeclarationClosure(ScopeLink scope_link)
                : _scope_link(scope_link)
            {
            }
    };
}

#endif // TL_DECLARATIONCLOSURE_HPP
