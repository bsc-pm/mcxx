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
#include "tl-langconstruct.hpp"
#include "tl-predicateutils.hpp"
#include "tl-source.hpp"
#include "cxx-attrnames.h"
#include "cxx-exprtype.h"

namespace TL
{
    // Remove it once all LangConstruct sons have their own
    const AlwaysFalse<AST_t> LangConstruct::predicate;

    // Static predicates for LangConstructs
    const PredicateBool<LANG_IS_ID_EXPRESSION> IdExpression::predicate;
    const PredicateBool<LANG_IS_STATEMENT> Statement::predicate;
    const PredicateBool<LANG_IS_FOR_STATEMENT> ForStatement::predicate;
    const PredicateBool<LANG_IS_FUNCTION_DEFINITION> FunctionDefinition::predicate;
    const PredicateBool<LANG_IS_EXPRESSION_NEST> Expression::predicate;
    const PredicateBool<LANG_IS_PARAMETER_DECLARATION> ParameterDeclaration::predicate;
    const PredicateBool<LANG_IS_DECLARED_NAME> DeclaredEntity::predicate;
    const PredicateBool<LANG_IS_DECLARATION> Declaration::predicate;

    std::string LangConstruct::prettyprint()
    {
        return _ref.prettyprint();
    }

    ObjectList<IdExpression> LangConstruct::non_local_symbol_occurrences(SymbolsWanted symbol_filter)
    {
        PredicateBool<LANG_IS_ID_EXPRESSION> id_expr_pred;
        PredicateBool<LANG_IS_ACCESSED_MEMBER> member_access;
        ObjectList<AST_t> id_expressions = _ref.depth_subtrees()
            .filter(id_expr_pred)
            .filter(negate(member_access));

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

                    bool eligible = true;
                    if (symbol_filter == ONLY_OBJECTS)
                    {
                        eligible = symbol.is_variable();
                    }
                    else if (symbol_filter == ONLY_FUNCTIONS)
                    {
                        eligible = symbol.is_function();
                    }

                    if (eligible)
                    {
                        result.push_back(id_expression);
                    }
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

    bool Statement::is_compound_statement()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_COMPOUND_STATEMENT);
        return b;
    }

    ObjectList<Statement> Statement::get_inner_statements()
    {
        ObjectList<Statement> result;

        if (is_compound_statement())
        {
            AST_t list = _ref.get_attribute(LANG_COMPOUND_STATEMENT_LIST);
            ASTIterator ast_iterator = list.get_list_iterator();

            ast_iterator.rewind();

            while (!ast_iterator.end())
            {
                Statement st(ast_iterator.item(), _scope_link);

                result.append(st);

                ast_iterator.next();
            }
        }

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

    Statement FunctionDefinition::get_function_body()
    {
        TL::AST_t ast = _ref.get_attribute(LANG_FUNCTION_BODY);

        return Statement(ast, _scope_link);
    }

    bool FunctionDefinition::is_templated() const
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_TEMPLATED_FUNCTION_DEFINITION);
        return result;
    }

    ObjectList<AST_t> FunctionDefinition::get_template_header()
    {
        TL::AST_t start = _ref.get_attribute(LANG_TEMPLATE_HEADER);

        PredicateBool<LANG_IS_TEMPLATE_HEADER> template_header_pred;

        ObjectList<AST_t> result = start.depth_subtrees(template_header_pred);
        return result;
    }

    AST_t FunctionDefinition::get_point_of_declaration()
    {
        return _ref.get_enclosing_function_definition(/*jump_templates=*/ true);
    }

    DeclaredEntity FunctionDefinition::get_declared_entity()
    {
        AST_t declarator = _ref.get_attribute(LANG_FUNCTION_DECLARATOR);
        return DeclaredEntity(declarator, _scope_link);
    }

    bool Declaration::is_templated()
    {
        TL::Bool result = _ref.get_attribute(LANG_IS_TEMPLATED_DECLARATION);
        return result;
    }

    ObjectList<AST_t> Declaration::get_template_header()
    {
        TL::AST_t start = _ref.get_attribute(LANG_TEMPLATE_HEADER);

        PredicateBool<LANG_IS_TEMPLATE_HEADER> template_header_pred;

        ObjectList<AST_t> result = start.depth_subtrees(template_header_pred);
        return result;
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

    Declaration IdExpression::get_declaration()
    {
        AST_t point_of_declaration = this->get_symbol().get_point_of_declaration();

        TL::Bool b = point_of_declaration.get_attribute(LANG_IS_DECLARATION);

        if (b)
        {
            return Declaration(point_of_declaration, this->_scope_link);
        }
        else
        {
            std::cerr << "Cannot retrieve a proper declaration of id-expression '" 
                << _ref.prettyprint() << "' from its point of declaration '" 
                <<  point_of_declaration.prettyprint() << "'"  << std::endl;
            return Declaration(AST_t(), this->_scope_link);
        }
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

    bool IdExpression::is_template_id() const
    {
        TL::Bool ret = _ref.
            get_attribute(LANG_UNQUALIFIED_ID)
            ->get_attribute(LANG_IS_TEMPLATE_ID);
        return ret;
    }

    std::string IdExpression::get_template_arguments() const
    {
        TL::AST_t ret = _ref
            .get_attribute(LANG_UNQUALIFIED_ID)
            ->get_attribute(LANG_TEMPLATE_ARGS);
        return "<" + ret.prettyprint(/*comma=*/true) + ">";
    }

    std::string IdExpression::get_template_name() const
    {
        TL::AST_t ret = _ref.get_attribute(LANG_TEMPLATE_NAME);
        return ret.prettyprint();
    }

    std::string IdExpression::get_unqualified_part(bool with_template_id) const
    {
        TL::AST_t unqualified_part = _ref.get_attribute(LANG_UNQUALIFIED_ID);

        if (!with_template_id)
        {
            TL::Bool is_template_id = unqualified_part.get_attribute(LANG_IS_TEMPLATE_ID);
            if (!is_template_id)
            {
                return unqualified_part.prettyprint();
            }
            else
            {
                TL::AST_t template_name = unqualified_part.get_attribute(LANG_TEMPLATE_NAME);

                return template_name.prettyprint();
            }
        }
        else
        {
            return unqualified_part.prettyprint();
        }
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

    /** Beginning of deprecated functions **/
    void ReplaceIdExpression::add_replacement(Symbol sym, std::string str)
    {
        Source src;
        src << str;

        AST_t tree = src.parse_expression(sym.get_scope());

        _repl_map[sym] = tree;
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, Source src)
    {
        add_replacement(sym, src.get_source());
    }
    /** End of deprecated functions **/

    void ReplaceIdExpression::add_replacement(Symbol sym, AST_t ast)
    {
        _repl_map[sym] = ast;
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, std::string str, AST_t ref_tree, ScopeLink scope_link)
    {
        Source src;
        src << str;

        AST_t tree = src.parse_expression(ref_tree, scope_link);

        _repl_map[sym] = tree;
    }

    void ReplaceIdExpression::add_replacement(Symbol sym, Source src, AST_t ref_tree, ScopeLink scope_link)
    {
        add_replacement(sym, src, ref_tree, scope_link);
    }

    bool ReplaceIdExpression::has_replacement(Symbol sym)
    {
        return (_repl_map.find(sym) != _repl_map.end());
    }

    AST_t Expression::advance_over_nests(AST_t expr)
    {
        if (!expr.is_valid())
        {
            std::cerr << "Expression is not valid" << std::endl;
            return expr;
        }

        TL::Bool is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);

        while (is_expression_nest)
        {
            expr = expr.get_attribute(LANG_EXPRESSION_NESTED);
            is_expression_nest = expr.get_attribute(LANG_IS_EXPRESSION_NEST);
        }

        return expr;
    }

    bool Expression::is_literal()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_LITERAL);

        return b;
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

    bool Expression::is_function_call()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_FUNCTION_CALL);
        return b;
    }

    Expression Expression::get_called_expression()
    {
        TL::AST_t result = _ref.get_attribute(LANG_CALLED_EXPRESSION);
        Expression expr(result, get_scope_link());

        return expr;
    }

    ObjectList<Expression> Expression::get_argument_list()
    {
        AST_t expression_list = _ref.get_attribute(LANG_FUNCTION_ARGUMENTS);

        ObjectList<Expression> result;

		if (expression_list.is_list())
		{
			ASTIterator it = expression_list.get_list_iterator();
			it.rewind();
			while (!it.end())
			{
				Expression expr(it.item(), _scope_link);
				result.push_back(expr);
				it.next();
			}
		}

        return result;
    }

    bool Expression::is_array_subscript()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_ARRAY_SUBSCRIPT);

        return b;
    }

    Expression Expression::get_subscripted_expression()
    {
        TL::AST_t result = _ref.get_attribute(LANG_SUBSCRIPTED_EXPRESSION);

        return Expression(result, _scope_link);
    }

    Expression Expression::get_subscript_expression()
    {
        TL::AST_t result = _ref.get_attribute(LANG_SUBSCRIPT_EXPRESSION);

        return Expression(result, _scope_link);
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
        AST_t result = _ref.get_attribute(LANG_UNARY_OPERAND);

        return Expression(result, this->_scope_link);
    }

    bool Expression::is_member_access()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_MEMBER_ACCESS);
        return b;
    }

    bool Expression::is_pointer_member_access()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_POINTER_MEMBER_ACCESS);
        return b;
    }

    IdExpression Expression::get_accessed_member()
    {
        TL::AST_t ast = _ref.get_attribute(LANG_ACCESSED_MEMBER);

        return IdExpression(ast, this->get_scope_link());
    }

    Expression Expression::get_accessed_entity()
    {
        TL::AST_t ast = _ref.get_attribute(LANG_ACCESSED_ENTITY);

        return Expression(ast, this->get_scope_link());
    }

    Expression::OperationKind Expression::get_operation_kind()
    {
        if (is_unary_operation())
        {
            if( TL::Bool(_ref.get_attribute(LANG_IS_PLUS_OP)))
                return PLUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_NEGATE_OP)))
                return MINUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_NOT_OP)))
                return LOGICAL_NOT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_COMPLEMENT_OP)))
                return BITWISE_NOT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_REFERENCE_OP)))
                return REFERENCE;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DERREFERENCE_OP)))
                return DERREFERENCE;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_PREINCREMENT)))
                return PREINCREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_PREDECREMENT)))
                return PREDECREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_POSTINCREMENT)))
                return POSTINCREMENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_POSTDECREMENT)))
                return POSTDECREMENT;
        }
        else if (is_binary_operation())
        {
            if (TL::Bool(_ref.get_attribute(LANG_IS_MULT_OP)))
                return MULTIPLICATION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIVISION_OP)))
                return DIVISION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_MODULUS_OP)))
                return MODULUS;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_ADDITION_OP)))
                return ADDITION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SUBSTRACTION_OP)))
                return SUBSTRACTION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHIFT_LEFT_OP)))
                return SHIFT_LEFT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHIFT_RIGHT_OP)))
                return SHIFT_RIGHT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOWER_THAN_OP)))
                return LOWER_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_GREATER_THAN_OP)))
                return GREATER_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_GREATER_OR_EQUAL_THAN_OP)))
                return GREATER_EQUAL_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOWER_OR_EQUAL_THAN_OP)))
                return LOWER_EQUAL_THAN;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_EQUAL_OP)))
                return COMPARISON;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIFFERENT_OP)))
                return DIFFERENT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_AND_OP)))
                return BITWISE_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_XOR_OP)))
                return BITWISE_XOR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_BITWISE_OR_OP)))
                return BITWISE_OR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOGICAL_AND_OP)))
                return LOGICAL_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_LOGICAL_OR_OP)))
                return LOGICAL_OR;
        }
        else if (is_operation_assignment())
        {
            if (TL::Bool(_ref.get_attribute(LANG_IS_MUL_ASSIGNMENT)))
                return MULTIPLICATION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_DIV_ASSIGNMENT)))
                return DIVISION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_ADD_ASSIGNMENT)))
                return ADDITION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SUB_ASSIGNMENT)))
                return SUBSTRACTION;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHL_ASSIGNMENT)))
                return SHIFT_LEFT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_SHR_ASSIGNMENT)))
                return SHIFT_RIGHT;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_AND_ASSIGNMENT)))
                return BITWISE_AND;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_OR_ASSIGNMENT)))
                return BITWISE_OR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_XOR_ASSIGNMENT)))
                return BITWISE_XOR;
            else if (TL::Bool(_ref.get_attribute(LANG_IS_MOD_ASSIGNMENT)))
                return MODULUS;
        }

        return UNKNOWN;
    }

	std::string Expression::get_operator_str()
	{
		switch ((int)this->get_operation_kind())
		{
			case  ADDITION :
			case  PLUS :
				return "+";
			case  MINUS :
			case  SUBSTRACTION :
				return "-";
			case  LOGICAL_NOT :
				return "!";
			case  BITWISE_NOT :
				return "~";
			case  REFERENCE :
				return "&";
			case  MULTIPLICATION :
			case  DERREFERENCE :
				return "*";
			case  POSTDECREMENT :
			case  PREDECREMENT :
				return "--";
			case  PREINCREMENT :
			case  POSTINCREMENT :
				return "++";
			case  DIVISION :
				return "/";
			case  MODULUS :
				return "%";
			case  SHIFT_LEFT :
				return "<<";
			case  SHIFT_RIGHT :
				return ">>";
			case  LOWER_THAN :
				return "<";
			case  GREATER_THAN :
				return ">";
			case  GREATER_EQUAL_THAN :
				return ">=";
			case  LOWER_EQUAL_THAN :
				return "<=";
			case  COMPARISON :
				return "==";
			case  DIFFERENT :
				return "!=";
			case  BITWISE_AND :
				return "&";
			case  BITWISE_XOR :
				return "^";
			case  BITWISE_OR :
				return "|";
			case  LOGICAL_AND :
				return "&&";
			case  LOGICAL_OR :
				return "||";
			default:
				return "??";
		}
	}

	bool Expression::is_conditional()
	{
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITIONAL_EXPRESSION);
        return b;
	}

	Expression Expression::get_condition_expression()
	{
		TL::AST_t condition_expr = _ref.get_attribute(LANG_CONDITIONAL_EXPRESSION);
		return Expression(condition_expr, this->get_scope_link());
	}

	Expression Expression::get_true_expression()
	{
		TL::AST_t condition_expr = _ref.get_attribute(LANG_CONDITIONAL_TRUE_EXPRESSION);
		return Expression(condition_expr, this->get_scope_link());
	}

	Expression Expression::get_false_expression()
	{
		TL::AST_t condition_expr = _ref.get_attribute(LANG_CONDITIONAL_FALSE_EXPRESSION);
		return Expression(condition_expr, this->get_scope_link());
	}

    Type Expression::get_type()
    {
        bool _dummy;
        return get_type(_dummy);
    }

    Type Expression::get_type(bool &is_lvalue)
    {
        // We need access to the inner representation of the tree
        AST_t expr = this->_ref;
        AST expr_tree = expr._ast;
        Scope sc = this->get_scope();

        decl_context_t decl_context = scope_link_get_decl_context(_scope_link._scope_link, expr_tree);
        char c_is_lvalue = 0;
        type_t* expression_type = compute_expression_type(expr_tree, sc._decl_context, &c_is_lvalue);

        is_lvalue = c_is_lvalue;

        Type result(expression_type);
        return result;
    }

    IdExpression DeclaredEntity::get_declared_entity()
    {
        // We convert it into an expression for commodity
        AST_t declared_name = _ref.get_attribute(LANG_DECLARED_NAME);

        Source declared_name_str = declared_name.prettyprint();

        Scope sc = this->_scope_link.get_scope(declared_name);

        AST_t expression_ast = declared_name_str.parse_expression(this->_ref, this->_scope_link);

        Expression expression(expression_ast, this->_scope_link);

        return expression.get_id_expression();
    }

    DeclarationSpec Declaration::get_declaration_specifiers()
    {
        AST_t declaration_specifiers = _ref.get_attribute(LANG_DECLARATION_SPECIFIERS);

        DeclarationSpec result(declaration_specifiers, this->_scope_link);

        return result;
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

		AST_t declarators_tree = this->_ref.get_attribute(LANG_DECLARATION_DECLARATORS);

        ObjectList<AST_t> declared_symbols =
            declarators_tree.depth_subtrees(lang_declared_name_pred, AST_t::NON_RECURSIVE);

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
        // // std::cerr << "Gathering for-init-construct" << std::endl;
        AST_t init_expr = _ref.get_attribute(LANG_FOR_INIT_CONSTRUCT);

        TL::Bool is_expression = init_expr.get_attribute(LANG_IS_EXPRESSION_NEST);
        if (is_expression)
        {
            // // std::cerr << "Is an expression" << std::endl;
            Expression expr(init_expr, _scope_link);

            if (expr.is_assignment())
            {
                // std::cerr << "Is an assignment" << std::endl;
                Expression lhs_assignment = expr.get_first_operand();
                Expression rhs_assignment = expr.get_second_operand();

                if (lhs_assignment.is_id_expression())
                {
                    // std::cerr << "LHS of expression is id_expression" << std::endl;
                    _induction_variable = lhs_assignment.get_ast();
                    _lower_bound = rhs_assignment.get_ast();
                }
            }
        }

        TL::Bool is_declaration = init_expr.get_attribute(LANG_IS_DECLARATION);
        if (is_declaration)
        {
            // std::cerr << "Is a declaration" << std::endl;
            Declaration declaration(init_expr, this->_scope_link);

            ObjectList<DeclaredEntity> declared_symbols = declaration.get_declared_entities();

            if (declared_symbols.size() == 1)
            {
                // std::cerr << "Only one declared, ok" << std::endl;
                DeclaredEntity declared_name = *(declared_symbols.begin());

                IdExpression declared_entity = declared_name.get_declared_entity();

                _induction_variable = declared_entity.get_ast();
                AST_t initializer = declared_name.get_initializer().get_ast();

                PredicateBool<LANG_IS_EXPRESSION_NEST> expression_pred;

                ObjectList<AST_t> expressions = initializer.depth_subtrees().filter(expression_pred);
                if (!expressions.empty())
                {
                    _lower_bound = *(expressions.begin());
                }
            }
        }

        // std::cerr << "Induction variable '" << _induction_variable.prettyprint() << "'" << std::endl;
        // std::cerr << "Lower bound '" << _lower_bound.prettyprint() << "'" << std::endl;

        // Now gather upper bound
        // std::cerr << "Gathering upper bound" << std::endl;
        AST_t condition = _ref.get_attribute(LANG_FOR_CONDITION);
        is_expression = condition.get_attribute(LANG_IS_EXPRESSION_NEST);

        if (is_expression)
        {
            // std::cerr << "Upper bound is an expression, ok" << std::endl;
            Expression expression(condition, _scope_link);

            if (expression.is_binary_operation())
            {
                // std::cerr << "Upper bound appears in a binary expression" << std::endl;
                Expression right_hand = expression.get_second_operand();

                switch ((int)expression.get_operation_kind())
                {
                    case Expression::LOWER_THAN:
                        {
                            Source adjust_value;

                            adjust_value << "(" << right_hand.get_ast().prettyprint() << ") - 1";

                            _upper_bound = adjust_value.parse_expression(expression.get_ast(), expression.get_scope_link());
                            break;
                        }
                    case Expression::GREATER_THAN:
                        {
                            Source adjust_value;

                            adjust_value << "(" << right_hand.get_ast().prettyprint() << ") + 1";

                            _upper_bound = adjust_value.parse_expression(expression.get_ast(), expression.get_scope_link());
                            break;
                        }
                    case Expression::LOWER_EQUAL_THAN :
                    case Expression::GREATER_EQUAL_THAN :
                        {
                            _upper_bound = right_hand.get_ast();
                            break;
                        }
                }
            }

            // std::cerr << "Upper bound is '" << _upper_bound.prettyprint() << "'" << std::endl;
        }

        // Now get the step
        AST_t iteration_expression_tree = _ref.get_attribute(LANG_FOR_ITERATION_EXPRESSION);
        if (iteration_expression_tree.is_valid())
        {
            Expression iteration_expression(iteration_expression_tree, _scope_link);

            if (iteration_expression.is_unary_operation())
            {
                switch ((int)iteration_expression.get_operation_kind())
                {
                    case Expression::PREINCREMENT :
                    case Expression::POSTINCREMENT :
                        {
                            // var++
                            // ++var
                            Source step;
                            step << "1";
                            _step = step.parse_expression(iteration_expression.get_ast(), iteration_expression.get_scope_link());
                            break;
                        }
                    case Expression::PREDECREMENT :
                    case Expression::POSTDECREMENT :
                        {
                            // var--
                            // --var
                            Source step;
                            step << "-1";
                            _step = step.parse_expression(iteration_expression.get_ast(), iteration_expression.get_scope_link());
                            break;
                        }
                }
            }
            else if (iteration_expression.is_assignment())
            {
                Expression right_hand_of_assignment = iteration_expression.get_second_operand();

                if (right_hand_of_assignment.is_binary_operation())
                {
                    switch ((int)right_hand_of_assignment.get_operation_kind())
                    {
                        case Expression::ADDITION :
                            {
                                // var = var + incr
                                // var = incr + var
                                Expression first_sumand = right_hand_of_assignment.get_first_operand();
                                Expression second_sumand = right_hand_of_assignment.get_first_operand();

                                if (first_sumand.is_id_expression())
                                {
                                    _step = second_sumand.get_ast();
                                }
                                else if (second_sumand.is_id_expression())
                                {
                                    _step = first_sumand.get_ast();
                                }
                                break;
                            }
                        case Expression::SUBSTRACTION :
                            {
                                // var = var - incr
                                Expression subtrahend = right_hand_of_assignment.get_second_operand();
                                _step = subtrahend.get_ast();
                                break;
                            }
                    }
                }
            }
            else if (iteration_expression.is_operation_assignment())
            {
                Expression right_hand_of_assignment = iteration_expression.get_second_operand();

                switch ((int)iteration_expression.get_operation_kind())
                {
                    case Expression::ADDITION :
                        {
                            _step = right_hand_of_assignment.get_ast();
                            break;
                        }
                    case Expression::SUBSTRACTION :
                        {
                            Source adjust; 
                            adjust << " - (" << right_hand_of_assignment.get_ast().prettyprint() << ")";

                            _step = adjust.parse_expression(right_hand_of_assignment.get_ast(),
                                    right_hand_of_assignment.get_scope_link());
                            break;
                        }
                }
            }
        }
    }


    bool ForStatement::regular_loop()
    {
        return (_induction_variable.is_valid()
                && _lower_bound.is_valid() 
                && _upper_bound.is_valid() 
                && _step.is_valid());
    }

    IdExpression ForStatement::get_induction_variable()
    {
        IdExpression result(_induction_variable, _scope_link);
        return result;
    }

    Expression ForStatement::get_lower_bound()
    {
        Expression result(_lower_bound, _scope_link);
        return result;
    }

    Expression ForStatement::get_upper_bound()
    {
        Expression result(_upper_bound, _scope_link);
        return result;
    }

    Expression ForStatement::get_step()
    {
        Expression result(_step, _scope_link);
        return result;
    }

    Statement ForStatement::get_loop_body()
    {
        AST_t loop_body = _ref.get_attribute(LANG_FOR_BODY_STATEMENT);
        Statement result(loop_body, _scope_link);

        return result;
    }


    AST_t ForStatement::get_iterating_init()
    {
        AST_t result = _ref.get_attribute(LANG_FOR_INIT_CONSTRUCT);
        return result;
    }

    Expression ForStatement::get_iterating_condition()
    {
        Expression result(_ref.get_attribute(LANG_FOR_CONDITION),
                _scope_link);
        return result;
    }

    Expression ForStatement::get_iterating_expression()
    {
        Expression result(_ref.get_attribute(LANG_FOR_ITERATION_EXPRESSION),
                _scope_link);
        return result;
    }

    bool DeclaredEntity::is_functional_declaration()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_FUNCTIONAL_DECLARATOR);
        return b;
    }

    // A traverse functor only for the first level of parameter declarations
    // (We want to avoid finding inner parameters of pointer-to-function
    // parameters)
    class TraverseParameters : public Functor<ASTTraversalResult, AST_t>
    {
        private:
            PredicateBool<LANG_IS_PARAMETER_DECLARATION> _pred;
        public:
            ASTTraversalResult operator()(AST_t& a) const
            {
                bool match = _pred(a);
                bool recurse = !match;

                return ast_traversal_result_helper(match, recurse);
            }
    };

    ObjectList<ParameterDeclaration> DeclaredEntity::get_parameter_declarations()
    {
        bool _dummy;
        return get_parameter_declarations(_dummy);
    }

    ObjectList<ParameterDeclaration> DeclaredEntity::get_parameter_declarations(bool &has_ellipsis)
    {
        ObjectList<ParameterDeclaration> result;

        IdExpression entity = get_declared_entity();
        Symbol symbol = entity.get_symbol();
        Type type = symbol.get_type();

        if (!type.is_function())
        {
            std::cerr 
                << "Error: Entity '" << entity.prettyprint() << "' in '" 
                << entity.get_ast().get_locus() 
                << "' is not a function type" 
                << std::endl;
            return result;
        }
        ObjectList<Type> parameter_types = type.parameters(has_ellipsis);

        // This avoids problems with the case 'f(void)' where we have
        // some sort of declarations but the computed type does not.
        if (parameter_types.empty())
        {
            return result;
        }

        TraverseParameters traverse_parameter_declarations;
        ObjectList<AST_t> parameter_declarations = _ref.depth_subtrees(traverse_parameter_declarations);

        ObjectList<Type>::iterator it_type = parameter_types.begin();
        ObjectList<AST_t>::iterator it_tree = parameter_declarations.begin();
        while (it_tree != parameter_declarations.end())
        {
            ParameterDeclaration parameter_decl(*it_tree, _scope_link, *it_type);
            result.append(parameter_decl);

            it_tree++;
            it_type++;
        }

        return result;
    }

    bool ParameterDeclaration::is_named()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_NAMED_PARAMETER_DECLARATION);
        return b;
    }

    IdExpression ParameterDeclaration::get_name()
    {
        AST_t declaration = _ref.get_attribute(LANG_PARAMETER_DECLARATION_NAME);
        return IdExpression(declaration, _scope_link);
    }
}
