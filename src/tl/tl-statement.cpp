#include "tl-statement.hpp"

namespace TL
{
    const PredicateAST<LANG_IS_STATEMENT> Statement::predicate;
    const PredicateAST<LANG_IS_FOR_STATEMENT> ForStatement::predicate;
    const PredicateAST<LANG_IS_WHILE_STATEMENT> WhileStatement::predicate;
    const PredicateAST<LANG_IS_IF_STATEMENT> IfStatement::predicate;
    const PredicateAST<LANG_IS_DO_STATEMENT> DoWhileStatement::predicate;
    const PredicateAST<LANG_IS_SWITCH_STATEMENT> SwitchStatement::predicate;

    bool Condition::is_expression()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITION_EXPRESSION);
        return b;
    }

    bool Condition::is_declaration()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_CONDITION_DECLARATION);
        return b;
    }

    Expression Condition::get_expression()
    {
        TL::AST_t ast = _ref.get_attribute(LANG_EXPRESSION_NESTED);
        Expression expr(ast, _scope_link);
        return expr;
    }

    Declaration Condition::get_declaration()
    {
        Declaration decl(_ref, _scope_link);
        return decl;
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

                _induction_variable = declared_name.get_declared_tree();
                AST_t initializer = declared_name.get_initializer().get_ast();

                _lower_bound = initializer;
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

    bool Statement::is_labeled()
    {
        TL::Bool b = _ref.get_attribute(LANG_IS_LABELED_STATEMENT);
        return b;
    }

    std::string Statement::get_label()
    {
        std::string result = "";

        TL::AST_t ast = _ref.get_attribute(LANG_STATEMENT_LABEL);
        if (ast.is_valid())
        {
            result = ast.prettyprint();
        }

        return result;
    }

    bool Statement::is_in_compound_statement()
    {
        if (_ref.is_in_a_list())
        {
            ASTIterator list = _ref.get_enclosing_list();

            AST_t parent = list.get_parent_of_list();

            return PredicateAST<LANG_IS_COMPOUND_STATEMENT>()(parent);
        }

        return false;
    }

    bool Statement::is_first()
    {
        if (!is_in_compound_statement())
            return true;

        ASTIterator list = _ref.get_enclosing_list();
        return list.is_first();
    }

    bool Statement::is_last()
    {
        if (!is_in_compound_statement())
            return true;

        ASTIterator list = _ref.get_enclosing_list();
        return list.is_last();
    }

    Statement Statement::next()
    {
        ASTIterator list = _ref.get_enclosing_list();
        list.next();

        AST_t item = list.item();

        return Statement(item, _scope_link);
    }

    Statement Statement::previous()
    {
        ASTIterator list = _ref.get_enclosing_list();
        list.previous();

        AST_t item = list.item();

        return Statement(item, _scope_link);
    }

    Statement WhileStatement::get_body()
    {
        TL::AST_t body_tree = _ref.get_attribute(LANG_WHILE_STATEMENT_BODY);

        return Statement(body_tree, _scope_link);
    }

    Condition WhileStatement::get_condition()
    {
        TL::AST_t condition_tree = _ref.get_attribute(LANG_WHILE_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Condition IfStatement::get_condition()
    {
        TL::AST_t condition_tree = _ref.get_attribute(LANG_IF_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Statement IfStatement::get_then_body()
    {
        TL::AST_t then_tree = _ref.get_attribute(LANG_IF_STATEMENT_THEN_BODY);
        return Statement(then_tree, _scope_link);
    }

    bool IfStatement::has_else()
    {
        TL::AST_t else_tree = _ref.get_attribute(LANG_IF_STATEMENT_ELSE_BODY);
        return else_tree.is_valid();
    }

    Statement IfStatement::get_else_body()
    {
        TL::AST_t else_tree = _ref.get_attribute(LANG_IF_STATEMENT_ELSE_BODY);
        return Statement(else_tree, _scope_link);
    }

    Statement DoWhileStatement::get_body()
    {
        TL::AST_t do_while_body = _ref.get_attribute(LANG_DO_STATEMENT_BODY);
        return Statement(do_while_body, _scope_link);
    }

    Expression DoWhileStatement::get_expression()
    {
        TL::AST_t do_while_body = _ref.get_attribute(LANG_DO_STATEMENT_EXPRESSION);
        return Expression(do_while_body, _scope_link);
    }

    Condition SwitchStatement::get_condition()
    {
        TL::AST_t condition_tree = _ref.get_attribute(LANG_SWITCH_STATEMENT_CONDITION);
        return Condition(condition_tree, _scope_link);
    }

    Expression CaseStatement::get_case_expression()
    {
        TL::AST_t case_expression = _ref.get_attribute(LANG_CASE_EXPRESSION);
        return Expression(case_expression, _scope_link);
    }

    Statement CaseStatement::get_statement()
    {
        TL::AST_t case_statement_body = _ref.get_attribute(LANG_CASE_STATEMENT_BODY);
        return Statement(case_statement_body, _scope_link);
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

    ObjectList<CaseStatement> SwitchStatement::get_cases()
    {
        TL::AST_t case_statement_body = _ref.get_attribute(LANG_CASE_STATEMENT_BODY);

        ObjectList<AST_t> case_tree_list = case_statement_body.depth_subtrees(PredicateAST<LANG_IS_CASE_STATEMENT>(), 
                // We do not want inner case statements coming from nested switch statements
                AST_t::NON_RECURSIVE);

        ObjectList<CaseStatement> result;
        for (ObjectList<AST_t>::iterator it = case_tree_list.begin();
                it != case_tree_list.end();
                it++)
        {
            CaseStatement case_statement(*it, _scope_link);
            result.append(case_statement);
        }

        return result;
    }
}
