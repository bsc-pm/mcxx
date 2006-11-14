#include "tl-langconstruct.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"

namespace TL
{
	std::string LangConstruct::prettyprint()
	{
		return _ref.prettyprint();
	}

	ObjectList<std::pair<Symbol, AST_t> > Statement::non_local_symbol_trees()
	{
		PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;
		ObjectList<AST_t> id_expressions = _ref.depth_subtrees().filter(id_expr_pred);

		Scope statement_scope = _scope_link.get_scope(_ref);

		ObjectList<std::pair<Symbol, AST_t> > result;

		for (ObjectList<AST_t>::iterator it = id_expressions.begin();
				it != id_expressions.end();
				it++)
		{
			AST_t& ref = *it;

			Symbol symbol = statement_scope.get_symbol_from_id_expr(ref);

			if (symbol.is_valid())
			{
				Scope ref_scope = _scope_link.get_scope(ref);
				Symbol local_symbol = ref_scope.get_symbol_from_id_expr(ref);

				if (local_symbol == symbol)
				{
					std::pair<Symbol, AST_t> p(symbol, ref);
					result.push_back(p);
				}
			}
		}

		return result;
	}

	ObjectList<Symbol> Statement::non_local_symbols()
	{
		ObjectList<std::pair<Symbol, AST_t> > symbols_trees  = non_local_symbol_trees();
		ObjectList<Symbol> raw_result;

		for (ObjectList<std::pair<Symbol, AST_t> >::iterator it = symbols_trees.begin();
				it != symbols_trees.end();
				it++)
		{
			raw_result.push_back(it->first);
		}

		ObjectSet<Symbol> result = raw_result;
		return result;
	}

	FunctionDefinition LangConstruct::get_enclosing_function()
	{
		AST_t enclosing_function = _ref.get_enclosing_function_definition();
		FunctionDefinition result(enclosing_function, _scope_link);

		return result;
	}

	bool FunctionDefinition::is_member()
	{
		return false;
	}

	bool FunctionDefinition::is_template()
	{
		return false;
	}

	void FunctionDefinition::prepend_sibling(AST_t ast)
	{
		_ref.prepend_sibling_function(ast);
	}

	std::string FunctionDefinition::get_function_name()
	{
		TL::AST_t ast = _ref.get_attribute(LANG_FUNCTION_NAME);

		return ast.prettyprint();
	}
}
