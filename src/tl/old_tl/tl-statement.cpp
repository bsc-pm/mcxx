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




#include "tl-statement.hpp"

namespace TL
{
    const PredicateAttr Statement::predicate(LANG_IS_STATEMENT);
    const PredicateAttr ForStatement::predicate(LANG_IS_FOR_STATEMENT);
    const PredicateAttr WhileStatement::predicate(LANG_IS_WHILE_STATEMENT);
    const PredicateAttr IfStatement::predicate(LANG_IS_IF_STATEMENT);
    const PredicateAttr DoWhileStatement::predicate(LANG_IS_DO_STATEMENT);
    const PredicateAttr SwitchStatement::predicate(LANG_IS_SWITCH_STATEMENT);
    const PredicateAttr CaseStatement::predicate(LANG_IS_CASE_STATEMENT);
    const PredicateAttr DefaultStatement::predicate(LANG_IS_DEFAULT_STATEMENT);
    const PredicateAttr BreakStatement::predicate(LANG_IS_BREAK_STATEMENT);
    const PredicateAttr ContinueStatement::predicate(LANG_IS_CONTINUE_STATEMENT);
    const PredicateAttr TryStatement::predicate(LANG_IS_TRY_BLOCK);
    const PredicateAttr ReturnStatement::predicate(LANG_IS_RETURN_STATEMENT);
    const PredicateAttr GotoStatement::predicate(LANG_IS_GOTO_STATEMENT);
    const PredicateAttr LabeledStatement::predicate(LANG_IS_LABELED_STATEMENT);
    const PredicateAttr EmptyStatement::predicate(LANG_IS_EMPTY_STATEMENT);

	// Falta try_block

    bool Condition::is_expression() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITION_EXPRESSION);
        return b;
    }

    bool Condition::is_declaration() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITION_DECLARATION);
        return b;
    }

    Expression Condition::get_expression() const
    {
        TL::AST_t ast = _ref.get_link_to_child(LANG_EXPRESSION_NESTED);
        Expression expr(ast, _scope_link);
        return expr;
    }

    Declaration Condition::get_declaration() const
    {
        Declaration decl(_ref, _scope_link);
        return decl;
    }

    bool Statement::is_compound_statement() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_COMPOUND_STATEMENT);
        return b;
    }

    ObjectList<Statement> Statement::get_inner_statements() const
    {
        ObjectList<Statement> result;

        if (is_compound_statement())
        {
            AST_t list = _ref.get_link_to_child(LANG_COMPOUND_STATEMENT_LIST);
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

    Statement Statement::get_pragma_line() const
    {
        AST_t pragma_line = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_LINE);
        Statement st(pragma_line, _scope_link);
        return st;
    }

    bool Statement::is_pragma_construct() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT);
        return b;
    }

    Statement Statement::get_pragma_construct_statement() const
    {
        AST_t pragma_statement = _ref.get_link_to_child(LANG_PRAGMA_CUSTOM_STATEMENT);
        Statement st(pragma_statement, _scope_link);
        return st;
    }

    bool Statement::is_pragma_directive() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE);
        return b;
    }

	bool Statement::breaks_flow()
	{
		return ( (TL::Bool) this->_ref.get_attribute(LANG_IS_FOR_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_WHILE_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_IF_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_DO_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_SWITCH_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_CASE_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_DEFAULT_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_BREAK_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_CONTINUE_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_TRY_BLOCK) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_RETURN_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_GOTO_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_LABELED_STATEMENT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_PRAGMA_CUSTOM_CONSTRUCT) ||
				(TL::Bool) this->_ref.get_attribute(LANG_IS_PRAGMA_CUSTOM_DIRECTIVE)
			   );
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
        AST_t init_expr = _ref.get_link_to_child(LANG_FOR_INIT_CONSTRUCT);

        if (!init_expr.is_valid())
            return;

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

                _induction_variable = declared_name.get_declared_tree();
                AST_t initializer = declared_name.get_initializer().get_ast();

                _lower_bound = initializer;
            }
        }

        // std::cerr << "Induction variable '" << _induction_variable.prettyprint() << "'" << std::endl;
        // std::cerr << "Lower bound '" << _lower_bound.prettyprint() << "'" << std::endl;

        // Now gather upper bound
        // std::cerr << "Gathering upper bound" << std::endl;
        AST_t condition = _ref.get_link_to_child(LANG_FOR_CONDITION);

        if (!condition.is_valid())
            return;

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
                            _operator_bound = std::string("<=");
                            break;
                        }
                    case Expression::GREATER_THAN:
                        {
                            Source adjust_value;

                            adjust_value << "(" << right_hand.get_ast().prettyprint() << ") + 1";

                            _upper_bound = adjust_value.parse_expression(expression.get_ast(), expression.get_scope_link());
                            _operator_bound = std::string(">=");
                            break;
                        }
                    case Expression::LOWER_EQUAL_THAN :
                        {
                            _upper_bound = right_hand.get_ast();
                            _operator_bound = std::string("<=");
                            break;
                        }
                    case Expression::GREATER_EQUAL_THAN :
                        {
                            _upper_bound = right_hand.get_ast();
                            _operator_bound = std::string(">=");
                            break;
                        }
                }
            }

            // std::cerr << "Upper bound is '" << _upper_bound.prettyprint() << "'" << std::endl;
        }

        // Now get the step
        AST_t iteration_expression_tree = _ref.get_link_to_child(LANG_FOR_ITERATION_EXPRESSION);

        if (!iteration_expression_tree.is_valid())
            return;

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
                                Expression second_sumand = right_hand_of_assignment.get_second_operand();

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


    bool ForStatement::regular_loop() const
    {
        return is_regular_loop();
    }

    bool ForStatement::is_regular_loop() const
    {
        return (_induction_variable.is_valid()
                && _lower_bound.is_valid() 
                && _upper_bound.is_valid() 
                && _step.is_valid());
    }

    Source ForStatement::get_bound_operator() const
    {
        return _operator_bound;
    }

    IdExpression ForStatement::get_induction_variable()
    {
        IdExpression result(_induction_variable, _scope_link);
        return result;
    }

    Expression ForStatement::get_lower_bound() const
    {
        Expression result(_lower_bound, _scope_link);
        return result;
    }

    Expression ForStatement::get_upper_bound() const
    {
        Expression result(_upper_bound, _scope_link);
        return result;
    }

    Expression ForStatement::get_step() const
    {
        Expression result(_step, _scope_link);
        return result;
    }

    Statement ForStatement::get_loop_body() const
    {
        AST_t loop_body = _ref.get_link_to_child(LANG_FOR_BODY_STATEMENT);
        Statement result(loop_body, _scope_link);

        return result;
    }


    AST_t ForStatement::get_iterating_init() const
    {
        AST_t result = _ref.get_link_to_child(LANG_FOR_INIT_CONSTRUCT);
        return result;
    }

    Expression ForStatement::get_iterating_condition() const
    {
        Expression result(_ref.get_link_to_child(LANG_FOR_CONDITION),
                _scope_link);
        return result;
    }

    Expression ForStatement::get_iterating_expression() const
    {
        Expression result(_ref.get_link_to_child(LANG_FOR_ITERATION_EXPRESSION),
                _scope_link);
        return result;
    }

    bool Statement::is_expression() const
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_EXPRESSION_STATEMENT);
        return b;
    }

    Expression Statement::get_expression() const
    {
        // It turns that expression statements can be safely wrapped in Expression already
        return Expression(_ref, _scope_link);
    }

//     bool Statement::is_labeled() const
//     {
//         TL::Bool b = _ref.get_attribute(LANG_IS_LABELED_STATEMENT);
//         return b;
//     }
// 
//     std::string Statement::get_label() const
//     {
//         std::string result = "";
// 
//         TL::AST_t ast = _ref.get_link_to_child(LANG_STATEMENT_LABEL);
//         if (ast.is_valid())
//         {
//             result = ast.prettyprint();
//         }
// 
//         return result;
//     }

    bool Statement::is_in_compound_statement() const
    {
        if (_ref.is_in_a_list())
        {
            ASTIterator list = _ref.get_enclosing_list();

            AST_t parent = list.get_parent_of_list();

            return (PredicateAttr(LANG_IS_COMPOUND_STATEMENT))(parent);
        }

        return false;
    }

    bool Statement::is_first() const
    {
        if (!is_in_compound_statement())
            return true;

        ASTIterator list = _ref.get_enclosing_list();
        return list.is_first();
    }

    bool Statement::is_last() const
    {
        if (!is_in_compound_statement())
            return true;

        ASTIterator list = _ref.get_enclosing_list();
        return list.is_last();
    }

    Statement Statement::next() const
    {
        ASTIterator list = _ref.get_enclosing_list();
        list.next();

        AST_t item = list.item();

        return Statement(item, _scope_link);
    }

    Statement Statement::previous() const
    {
        ASTIterator list = _ref.get_enclosing_list();
        list.previous();

        AST_t item = list.item();

        return Statement(item, _scope_link);
    }

    Statement WhileStatement::get_body() const
    {
        TL::AST_t body_tree = _ref.get_link_to_child(LANG_WHILE_STATEMENT_BODY);

        return Statement(body_tree, _scope_link);
    }

    Condition WhileStatement::get_condition() const
    {
        TL::AST_t condition_tree = _ref.get_link_to_child(LANG_WHILE_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Condition IfStatement::get_condition() const
    {
        TL::AST_t condition_tree = _ref.get_link_to_child(LANG_IF_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Statement IfStatement::get_then_body() const
    {
        TL::AST_t then_tree = _ref.get_link_to_child(LANG_IF_STATEMENT_THEN_BODY);
        return Statement(then_tree, _scope_link);
    }

    bool IfStatement::has_else() const
    {
        TL::AST_t else_tree = _ref.get_link_to_child(LANG_IF_STATEMENT_ELSE_BODY);
        return else_tree.is_valid();
    }

    Statement IfStatement::get_else_body() const
    {
        TL::AST_t else_tree = _ref.get_link_to_child(LANG_IF_STATEMENT_ELSE_BODY);
        return Statement(else_tree, _scope_link);
    }

    Statement DoWhileStatement::get_body() const
    {
        TL::AST_t do_while_body = _ref.get_link_to_child(LANG_DO_STATEMENT_BODY);
        return Statement(do_while_body, _scope_link);
    }

    Expression DoWhileStatement::get_expression() const
    {
        TL::AST_t do_while_body = _ref.get_link_to_child(LANG_DO_STATEMENT_EXPRESSION);
        return Expression(do_while_body, _scope_link);
    }

    Condition SwitchStatement::get_condition() const
    {
        TL::AST_t condition_tree = _ref.get_link_to_child(LANG_SWITCH_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Statement SwitchStatement::get_switch_body() const
    {
        TL::AST_t switch_body_tree = _ref.get_link_to_child(LANG_SWITCH_STATEMENT_BODY);
        return Statement(switch_body_tree, _scope_link);
    }

    Expression CaseStatement::get_case_expression() const
    {
        TL::AST_t case_expression = _ref.get_link_to_child(LANG_CASE_EXPRESSION);
        return Expression(case_expression, _scope_link);
    }

    Statement CaseStatement::get_statement() const
    {
        TL::AST_t case_statement_body = _ref.get_link_to_child(LANG_CASE_STATEMENT_BODY);
        return Statement(case_statement_body, _scope_link);
    }

    Statement DefaultStatement::get_statement() const
    {
        TL::AST_t default_statement_body = _ref.get_link_to_child(LANG_DEFAULT_STATEMENT_BODY);
        return Statement(default_statement_body, _scope_link);
    }

    void Statement::prepend(Statement st)
    {
        this->_ref.prepend(st._ref);
    }

    void Statement::append(Statement st)
    {
        this->_ref.append(st._ref);

        {
            // If parent has a scope link, discard it
            AST_t parent = this->_ref.get_parent().get_parent();
            scope_link_unset(get_scope_link().get_internal_scope_link(), parent.get_internal_ast());
        }

        {
            // Update grandparent scope link 
            AST_t grandparent = this->_ref.get_parent().get_parent();
            scope_link_set(get_scope_link().get_internal_scope_link(), grandparent.get_internal_ast(),
                    get_scope().get_decl_context());
        }
    }

    bool Statement::is_declaration() const
    {
        PredicateAttr is_decl_stmt(LANG_IS_DECLARATION_STATEMENT);
        return is_decl_stmt(_ref);
    }

    bool Statement::is_simple_declaration() const
    {
        if (is_declaration())
        {
            // Check son 0 is a declaration
            AST_t first_son = _ref.children()[0];

            if (Declaration::predicate(first_son))
            {
                return true;
            }
        }
        return false;
    }

    Declaration Statement::get_simple_declaration() const
    {
        // Check son 0 is a declaration
        AST_t first_son = _ref.children()[0];

        Declaration decl(first_son, _scope_link);

        return decl;
    }

    ObjectList<CaseStatement> SwitchStatement::get_cases() const
    {
        /*TL::AST_t case_statement_body = _ref.get_link_to_child(LANG_CASE_STATEMENT_BODY);
        std::cout << "CASE STATEMENT BODY '" << case_statement_body.prettyprint() << "'" << std::endl;
        
        ObjectList<AST_t> case_tree_list = case_statement_body.depth_subtrees(
                PredicateAttr(LANG_IS_CASE_STATEMENT), 
                // We do not want inner case statements coming from nested switch statements
                AST_t::NON_RECURSIVE);

        ObjectList<CaseStatement> result;
        for (ObjectList<AST_t>::iterator it = case_tree_list.begin();
                it != case_tree_list.end();
                it++)
        {
            CaseStatement case_statement(*it, _scope_link);
            result.append(case_statement);
        }*/

        TL::AST_t switch_statement_body = _ref.get_link_to_child(LANG_SWITCH_STATEMENT_BODY);

        ObjectList<AST_t> case_tree_list = switch_statement_body.depth_subtrees(
                PredicateAttr(LANG_IS_CASE_STATEMENT), 
                // We do not want inner case statements coming from nested switch statements
                AST_t::NON_RECURSIVE);
        ObjectList<CaseStatement> result;
        for (ObjectList<AST_t>::iterator itc = case_tree_list.begin();
            itc != case_tree_list.end();
                itc++)
        {
            CaseStatement case_statement(*itc, _scope_link);
            result.append(case_statement);
        }    

        return result;
    }

    ObjectList<DefaultStatement> SwitchStatement::get_defaults() const
    {
        TL::AST_t switch_statement_body = _ref.get_link_to_child(LANG_SWITCH_STATEMENT_BODY);
        
        ObjectList<AST_t> default_tree_list = switch_statement_body.depth_subtrees(
                PredicateAttr(LANG_IS_DEFAULT_STATEMENT), 
                // We do not want inner case statements coming from nested switch statements
                AST_t::NON_RECURSIVE);
        ObjectList<DefaultStatement> result;
        for (ObjectList<AST_t>::iterator itd = default_tree_list.begin();
                itd != default_tree_list.end();
                itd++)
        {
            DefaultStatement default_statement(*itd, _scope_link);
            result.append(default_statement);
        }
        
        return result;
    }

    Statement TryStatement::get_try_protected_block() const
    {
        TL::AST_t try_block_tree = _ref.get_link_to_child(LANG_TRY_BLOCK_BODY);
        Statement try_statement(try_block_tree, _scope_link);
        return try_statement;
    }

    ObjectList<Declaration> TryStatement::get_try_handler_declarations() const
    {
        ObjectList<Declaration> handler_decls;
        
        AST_t try_handlers_list = _ref.get_link_to_child(LANG_TRY_BLOCK_HANDLER_LIST);
        ASTIterator handlers_iterator = try_handlers_list.get_list_iterator();
                
        handlers_iterator.rewind();
        while(!handlers_iterator.end())
        {
            Declaration decl(handlers_iterator.item().children()[0], _scope_link);
            handler_decls.append(decl);
            handlers_iterator.next();
        }
        
        return handler_decls;
    }

    ObjectList<Statement> TryStatement::get_try_handler_blocks() const
    {
        ObjectList<Statement> handler_blocks;
        
        AST_t try_handlers_list = _ref.get_link_to_child(LANG_TRY_BLOCK_HANDLER_LIST);
        ASTIterator handlers_iterator = try_handlers_list.get_list_iterator();
        
        handlers_iterator.rewind();
        while(!handlers_iterator.end())
        {
            Statement block(handlers_iterator.item().children()[1], _scope_link);
            handler_blocks.append(block);
            handlers_iterator.next();
        }
        
        return handler_blocks;
    }

    bool ReturnStatement::has_return_expression() const
    {
        return TL::Bool(_ref.get_attribute(LANG_RETURN_STATEMENT_HAS_EXPRESSION));
    }

    Expression ReturnStatement::get_return_expression() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_RETURN_EXPRESSION);
        Expression expr(tree, _scope_link);
        return expr;
    }

    std::string GotoStatement::get_label() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_GOTO_STATEMENT_LABEL);
        return tree.prettyprint();
    }
    
    std::string LabeledStatement::get_label() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_STATEMENT_LABEL);
        return tree.prettyprint();
    }
    
    Statement LabeledStatement::get_labeled_statement() const
    {
        AST_t tree = _ref.get_link_to_child(LANG_LABELED_STATEMENT);
        return Statement(tree, _scope_link);
    }
}
