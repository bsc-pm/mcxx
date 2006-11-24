#include "tl-langconstruct.hpp"
#include "tl-predicateutils.hpp"
#include "cxx-attrnames.h"

namespace TL
{
	std::string LangConstruct::prettyprint()
	{
		return _ref.prettyprint();
	}

	ObjectList<IdExpression> Statement::non_local_symbol_occurrences()
	{
		PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;
		ObjectList<AST_t> id_expressions = _ref.depth_subtrees().filter(id_expr_pred);

		Scope statement_scope = _scope_link.get_scope(_ref);

		ObjectList<IdExpression> result;

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
					IdExpression id_expression(*it, _scope_link);
					result.push_back(id_expression);
				}
			}
		}

		return result;
	}

	ObjectList<Symbol> Statement::non_local_symbols()
	{
		ObjectList<IdExpression> id_expressions  = non_local_symbol_occurrences();
		ObjectList<Symbol> result = id_expressions.map(functor(&IdExpression::get_symbol));
		return result;
	}

	FunctionDefinition LangConstruct::get_enclosing_function()
	{
		AST_t enclosing_function = _ref.get_enclosing_function_definition();
		FunctionDefinition result(enclosing_function, _scope_link);

		return result;
	}

	void FunctionDefinition::prepend_sibling(AST_t ast)
	{
		_ref.prepend_sibling_function(ast);
	}

	IdExpression FunctionDefinition::get_function_name()
	{
		TL::AST_t ast = _ref.get_attribute(LANG_FUNCTION_NAME);

		return IdExpression(ast, _scope_link);
	}

	// Returns a flattened version of this id-expression
	std::string IdExpression::mangle_id_expression() const
	{
		std::string id_expr_str = _ref.prettyprint();
		unsigned int length = id_expr_str.size();
		for (unsigned int i = 0; i < length; i++)
		{
			if (id_expr_str[i] == ':'
					|| id_expr_str[i] == '<'
					|| id_expr_str[i] == '>')
			{
				id_expr_str[i] = '_';
			}
		}

		return id_expr_str;
	}

	std::string IdExpression::get_qualified_part() const
	{
		if (is_unqualified())
		{
			return "";
		}
		else
		{
			TL::AST_t nested_name_part = _ref.get_attribute(LANG_NESTED_NAME_SPECIFIER);
			return nested_name_part.prettyprint();
		}
	}

	std::string IdExpression::get_unqualified_part() const
	{
		TL::AST_t unqualified_part = _ref.get_attribute(LANG_UNQUALIFIED_ID);

		return unqualified_part.prettyprint();
	}

	bool IdExpression::is_qualified() const
	{
		TL::Bool is_qualif = _ref.get_attribute(LANG_IS_QUALIFIED_ID);
		return (bool)is_qualif;
	}

	bool IdExpression::is_unqualified() const
	{
		TL::Bool is_qualif = _ref.get_attribute(LANG_IS_QUALIFIED_ID);
		return !((bool)is_qualif);
	}

	Symbol IdExpression::get_symbol() const
	{
		Scope id_expr_scope = _scope_link.get_scope(_ref);
		Symbol result = id_expr_scope.get_symbol_from_id_expr(_ref);
		return result;
	}

	AST_t IdExpression::get_ast() const
	{
		return _ref;
	}
}
