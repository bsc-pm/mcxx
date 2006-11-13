#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-ast.hpp"
#include "tl-symbol.hpp"
#include "tl-scopelink.hpp"
#include <string>
#include <utility>

namespace TL
{
	class LangConstruct
	{
		protected:
			AST_t _ref;
			ScopeLink _scope_link;

		public:
			LangConstruct(AST_t ref, ScopeLink scope_link)
				: _ref(ref), _scope_link(scope_link)
			{
			}

			std::string prettyprint();

			AST_t get_ast()
			{
				return _ref;
			}

			ScopeLink get_scope_link()
			{
				return _scope_link;
			}

			Scope get_scope()
			{
				return _scope_link.get_scope(_ref);
			}
	};

	class Statement : public LangConstruct
	{
		public:
			Statement(AST_t ref, ScopeLink scope_link)
				: LangConstruct(ref, scope_link)
			{
			}

			ObjectList<Symbol> symbols();
			ObjectList<Symbol> non_local_symbols();

			ObjectList<std::pair<Symbol, AST_t> > non_local_symbol_trees();
			ObjectList<std::pair<Symbol, AST_t> > local_symbol_trees();
	};
}

#endif // TL_LANGCONSTRUCT_HPP
