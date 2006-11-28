#include "tl-langconstruct.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
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

	void ReplaceIdExpression::add_replacement(Symbol sym, AST_t ast)
	{
		_repl_map[sym] = ast;
	}

	void ReplaceIdExpression::replace(Statement stmt)
	{
		ObjectList<IdExpression> id_expressions = stmt.non_local_symbol_occurrences();

		for (ObjectList<IdExpression>::iterator it = id_expressions.begin();
				it != id_expressions.end();
				it++)
		{
			Symbol sym = it->get_symbol();

			if (_repl_map.find(sym) != _repl_map.end())
			{
				AST_t repl_ast = _repl_map[sym];
				AST_t orig_ast = it->get_ast();

				orig_ast.replace_with(repl_ast);
			}
		}
	}

	bool ReplaceIdExpression::has_replacement(Symbol sym)
	{
		return (_repl_map.find(sym) != _repl_map.end());
	}

    AST_t Expression::advance_over_nests(AST_t expr)
    {
        TL::Bool is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);

        while (is_expression_nest)
        {
            expr = expr.get_attribute(LANG_EXPRESSION_NESTED);
            is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);
        }

        return expr;
    }

    bool Expression::is_id_expression()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ID_EXPRESSION);

        return b;
    }

    IdExpression Expression::get_id_expression()
    {
        return IdExpression(_ref, this->_scope_link);
    }

    bool Expression::is_binary_operation()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_BINARY_OPERATION);

        return b;
    }

    bool Expression::is_unary_operation()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_UNARY_OPERATION);

        return b;
    }

    bool Expression::is_assignment()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ASSIGNMENT);

        return b;
    }
    
    bool Expression::is_operation_assignment()
    {
        TL::Bool b(false);
        b = TL::Bool(_ref.get_attribute(LANG_IS_MUL_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_DIV_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_ADD_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SUB_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SHL_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_SHR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_AND_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_OR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_XOR_ASSIGNMENT))
            || TL::Bool(_ref.get_attribute(LANG_IS_MOD_ASSIGNMENT));

        return b;
    }

    bool Expression::is_array_subscript()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ARRAY_SUBSCRIPT);

        return b;
    }

    bool Expression::is_casting()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CAST);

        return b;
    }

    AST_t Expression::get_cast_type()
    {
        AST_t result = _ref.get_attribute(LANG_CAST_TYPE);

        return result;
    }

    Expression Expression::get_casted_expression()
    {
        Expression result(_ref.get_attribute(LANG_CASTED_EXPRESSION), this->_scope_link);

        return result;
    }

    Expression Expression::get_first_operand()
    {
        if (this->is_assignment() || this->is_operation_assignment())
        {
            AST_t result = _ref.get_attribute(LANG_LHS_ASSIGNMENT);
            return Expression(result, this->_scope_link);
        }
        else
        {
            AST_t result = _ref.get_attribute(LANG_LHS_OPERAND);
            return Expression(result, this->_scope_link);
        }
    }

    Expression Expression::get_second_operand()
    {
        if (this->is_assignment() || this->is_operation_assignment())
        {
            AST_t result = _ref.get_attribute(LANG_RHS_ASSIGNMENT);
            return Expression(result, this->_scope_link);
        }
        else
        {
            AST_t result = _ref.get_attribute(LANG_RHS_OPERAND);
            return Expression(result, this->_scope_link);
        }
    }

    Expression Expression::get_unary_operand()
    {
        AST_t result = _ref.get_attribute(LANG_RHS_OPERAND);

        return Expression(result, this->_scope_link);
    }

    IdExpression DeclaredEntity::get_declared_entity()
    {
        // We convert it into an expression for commodity
        AST_t declared_name = _ref.get_attribute(LANG_DECLARED_NAME);

        Source declared_name_str = declared_name.prettyprint();

        AST_t expression_ast = declared_name_str.parse_expression(this->get_scope());

        Expression expression(expression_ast, this->_scope_link);

        return expression.get_id_expression();
    }

    bool DeclaredEntity::has_initializer()
    {
        AST_t initializer = _ref.get_attribute(LANG_INITIALIZER);
        
        return initializer.is_valid();
    }

    Expression DeclaredEntity::get_initializer()
    {
        AST_t initializer = _ref.get_attribute(LANG_INITIALIZER);

        return Expression(initializer, this->_scope_link);
    }

    ObjectList<DeclaredEntity> Declaration::get_declared_entities()
    {
        PredicateBool<LANG_IS_DECLARED_NAME> lang_declared_name_pred;
        PredicateBool<LANG_IS_DECLARED_PARAMETER> lang_declared_param_pred;

        ObjectList<AST_t> declared_symbols =
            this->_ref.depth_subtrees().filter(lang_declared_name_pred).filter(negate(lang_declared_param_pred));

        ObjectList<DeclaredEntity> result;
        for (ObjectList<AST_t>::iterator it = declared_symbols.begin();
                it != declared_symbols.end();
                it++)
        {
            DeclaredEntity declared(*it, this->_scope_link);

            result.push_back(declared);
        }

        return result;
    }

	bool ForStatement::check_statement()
	{
		TL::Bool b = this->_ref.get_attribute(LANG_IS_FOR_STATEMENT);

		if (!b)
		{
			std::cerr << "The given statement is not a for statement" << std::endl;
		}

		return b;
	}

	void ForStatement::gather_for_information()
	{
		// First gather init expression and lower bound
		AST_t init_expr = this->_ref.get_attribute(LANG_FOR_INIT_CONSTRUCT);

		TL::Bool is_expression = init_expr.get_attribute(LANG_IS_EXPRESSION_NEST);
		if (is_expression)
		{
            Expression expr(init_expr, this->_scope_link);

            if (expr.is_assignment())
            {
                Expression lhs_assignment = expr.get_first_operand();
                Expression rhs_assignment = expr.get_second_operand();

                if (lhs_assignment.is_id_expression())
                {
                    _induction_variable = lhs_assignment.get_ast();
                    _lower_bound = rhs_assignment.get_ast();
                }
            }
		}

		TL::Bool is_declaration = this->_ref.get_attribute(LANG_IS_DECLARATION);
		if (is_declaration)
		{
            Declaration declaration(is_declaration, this->_scope_link);

            ObjectList<DeclaredEntity> declared_symbols = declaration.get_declared_entities();

			if (declared_symbols.size() == 1)
			{
				DeclaredEntity declared_name = *(declared_symbols.begin());

                IdExpression declared_entity = declared_name.get_declared_entity();

				_induction_variable = declared_entity.get_ast();
				_lower_bound = declared_name.get_initializer().get_ast();
			}
		}

		// Now gather upper bound
	}
}
