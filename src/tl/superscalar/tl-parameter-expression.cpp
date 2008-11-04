/*
    SMP superscalar Compiler
    Copyright (C) 2008 Barcelona Supercomputing Center

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; version 2.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "tl-langconstruct.hpp"

#include "tl-calculator.hpp"
#include "tl-exceptions.hpp"
#include "tl-parameter-expression.hpp"


namespace TL {
	
	
#if 0
	void ParameterExpression::build_term_list(Expression expr, Expression::OperationKind op, ObjectList<Expression> /* OUT */ &term_list)
	{
		if (expr.is_binary_operation() && expr.get_operation_kind() == op)
		{
			build_term_list(expr.get_first_operand(), op, term_list);
			build_term_list(expr.get_second_operand(), op, term_list);
		}
		else
		{
			term_list.push_back(expr);
		}
	}
	
	
	bool ParameterExpression::operators_are_sequence_shufflable(Expression::OperationKind op1, Expression::OperationKind op2)
	{
		if (
				(op1 == Expression::ADDITION || op1 == Expression::SUBSTRACTION)
			&&
				(op2 == Expression::ADDITION || op2 == Expression::SUBSTRACTION)
			)
		{
			return true;
		}
		
		if (
				(op1 == Expression::MULTIPLICATION || op1 == Expression::DIVISION)
			&&
				(op2 == Expression::MULTIPLICATION || op2 == Expression::DIVISION)
			)
		{
			return true;
		}
		
		if (op1 == Expression::SHIFT_LEFT && op2 == Expression::SHIFT_LEFT)
		{
			return true;
		}
		
		if (op1 == Expression::SHIFT_RIGHT && op2 == Expression::SHIFT_RIGHT)
		{
			return true;
		}
		
		if (op1 == Expression::LOGICAL_OR && op2 == Expression::LOGICAL_OR)
		{
			return true;
		}
		
		if (op1 == Expression::LOGICAL_AND && op2 == Expression::LOGICAL_AND)
		{
			return true;
		}
		
		if (op1 == Expression::BITWISE_OR && op2 == Expression::BITWISE_OR)
		{
			return true;
		}
		
		if (op1 == Expression::BITWISE_AND && op2 == Expression::BITWISE_AND)
		{
			return true;
		}
		
		if (op1 == Expression::BITWISE_XOR && op2 == Expression::BITWISE_XOR)
		{
			return true;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::is_binding_operator(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::SUBSTRACTION:
			case Expression::DIVISION:
			case Expression::MODULUS:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_NOT:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
				return true;
				break;
			case Expression::ADDITION:
			case Expression::MULTIPLICATION:
			case Expression::LOGICAL_OR:
			case Expression::LOGICAL_AND:
			case Expression::BITWISE_OR:
			case Expression::BITWISE_AND:
			case Expression::BITWISE_XOR:
			case Expression::CONDITIONAL:
				return false;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operator_has_neuter(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::MODULUS:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_NOT:
			case Expression::CONDITIONAL:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::BITWISE_XOR:
				return false;
				break;
			case Expression::ADDITION:
			case Expression::SUBSTRACTION:
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_OR:
			case Expression::LOGICAL_AND:
			case Expression::BITWISE_OR:
			case Expression::BITWISE_AND:
				return true;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	Expression ParameterExpression::get_neuter_for_operator(Expression::OperationKind op, AST_t ref_ast, ScopeLink scope_link)
	{
		switch (op)
		{
			case Expression::ADDITION:
			case Expression::SUBSTRACTION:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_OR:
			case Expression::BITWISE_OR:
				{
					Source source;
					source << "0";
					return Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				break;
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
				{
					Source source;
					source << "1";
					return Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				break;
			case Expression::LOGICAL_AND:
			case Expression::BITWISE_AND:
				// Neuter is same
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::MODULUS:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_NOT:
			case Expression::BITWISE_XOR:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::CONDITIONAL:
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operator_can_produce_identity(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::ADDITION:
			case Expression::SUBSTRACTION:
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
			case Expression::MODULUS:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_AND:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_AND:
			case Expression::BITWISE_NOT:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::CONDITIONAL:
				return false;
				break;
			case Expression::LOGICAL_OR:
			case Expression::BITWISE_OR:
			case Expression::BITWISE_XOR:
				return true;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operator_can_produce_neuter(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::MODULUS:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_OR:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_OR:
			case Expression::BITWISE_NOT:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::CONDITIONAL:
				return false;
				break;
			case Expression::ADDITION:
			case Expression::SUBSTRACTION:
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
			case Expression::LOGICAL_AND:
			case Expression::BITWISE_AND:
			case Expression::BITWISE_XOR:
				return true;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operator_neuter_is_same(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::ADDITION:
			case Expression::SUBSTRACTION:
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_AND:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_AND:
			case Expression::BITWISE_XOR:
			case Expression::BITWISE_NOT:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::CONDITIONAL:
				return false;
				break;
			case Expression::MODULUS:
			case Expression::LOGICAL_OR:
			case Expression::BITWISE_OR:
				return true;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operator_null_is_same(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
			case Expression::REFERENCE:
			case Expression::PLUS:
			case Expression::MINUS:
			case Expression::ADDITION:
			case Expression::MULTIPLICATION:
			case Expression::DIVISION:
			case Expression::MODULUS:
			case Expression::SHIFT_LEFT:
			case Expression::SHIFT_RIGHT:
			case Expression::LOGICAL_AND:
			case Expression::LOGICAL_OR:
			case Expression::LOGICAL_NOT:
			case Expression::BITWISE_AND:
			case Expression::BITWISE_OR:
			case Expression::BITWISE_NOT:
			case Expression::LOWER_THAN:
			case Expression::GREATER_THAN:
			case Expression::LOWER_EQUAL_THAN:
			case Expression::GREATER_EQUAL_THAN:
			case Expression::COMPARISON:
			case Expression::DIFFERENT:
			case Expression::PREINCREMENT:
			case Expression::POSTINCREMENT:
			case Expression::PREDECREMENT:
			case Expression::POSTDECREMENT:
			case Expression::CONDITIONAL:
				return false;
				break;
			case Expression::SUBSTRACTION:
			case Expression::BITWISE_XOR:
				return true;
				break;
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		
		return false;
	}
	
	
	bool ParameterExpression::operators_are_distributive(Expression::OperationKind op1, Expression::OperationKind op2)
	{
		if (
				(op1 == Expression::MULTIPLICATION || op1 == Expression::DIVISION)
			&&
				(op2 == Expression::ADDITION || op2 == Expression::SUBSTRACTION)
			)
		{
			return true;
		}
		
		if (op1 == Expression::LOGICAL_OR && op2 == Expression::LOGICAL_AND)
		{
			return true;
		}
		
		if (op1 == Expression::BITWISE_OR && op2 == Expression::BITWISE_AND)
		{
			return true;
		}
		
		return false;
	}
	
	
	
std::string recursion_prefix;
	
	
	
	Expression ParameterExpression::propagate_binding_operator(Expression::OperationKind op, Expression expr, Expression::OperationKind /* OUT */ &new_op)
	{
std::cout << recursion_prefix << "Propagating '" << operator_to_string(op) << "' over '" << expr.prettyprint() << "'" << std::endl;
recursion_prefix.append("\t");
		
		AST_t ref_ast = expr1.get_ast();
		ScopeLink scope_link = expr1.get_scope_link();
		
		Expression result;
		
		if (op == Expression::SUBSTRACTION)
		{
			if (expr.is_binary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				Expression expr1 = expr.get_first_operand();
				Expression expr2 = expr.get_second_operand();
				if (op0 == Expression::ADDITION)
				{
					expr1 = propagate_unary_operator(Expression::MINUS, expr1);
					expr2 = propagate_unary_operator(Expression::MINUS, expr2);
				}
				else if (op0 == Expression::SUBSTRACTION)
				{
					expr1 = propagate_unary_operator(Expression::MINUS, expr1);
					op0 = Expression::ADDITION;
				}
				else
				{
					expr1 = propagate_unary_operator(Expression::MINUS, expr1);
				}
				
				Source source;
				
				source
					<< "(" << expr1.prettyprint() << ")"
					<< operator_to_string(op0)
					<< "(" << expr2.prettyprint() << ")"
				;
				
				result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
			}
			else if (expr.is_conditional())
			{
				Expression cond = expr.get_condition_expression();
				Expression true_expr = expr.get_true_expression();
				Expression false_expr = expr.get_false_expression();
				
				true_expr = propagate_unary_operator(Expression::MINUS, true_expr);
				false_expr = propagate_unary_operator(Expression::MINUS, false_expr);
				
				Source source;
				
				source
					<< "("
						<< "(" << cond.prettyprint() << ")"
					<< "?"
						<< "(" << true_expr.prettyprint() << ")"
					<< ":"
						<< "(" << false_expr.prettyprint() << ")"
					<< ")"
				;
				
				result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
			}
			else
			{
				result = propagate_unary_operator(Expression::MINUS, expr);
			}
			new_op = Expression::ADDITION;
		}
		else
		{
			new_op = op;
			result = expr;
		}
		
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << result.prettyprint() << "'" << std::endl;
		
		return result;
	}
	
	
	Expression ParameterExpression::propagate_unary_operator(Expression::OperationKind op, Expression expr)
	{
std::cout << recursion_prefix << "Propagating '" << operator_to_string(op) << "' over '" << expr.prettyprint() << "'" << std::endl;
recursion_prefix.append("\t");
		
		AST_t ref_ast = expr1.get_ast();
		ScopeLink scope_link = expr1.get_scope_link();
		
		Expression result;
		
		if (expr.is_conditional())
		{
			Expression cond = expr.get_condition_expression();
			Expression true_expr = expr.get_true_expression();
			Expression false_expr = expr.get_false_expression();
			
			true_expr = propagate_unary_operator(op, true_expr);
			false_expr = propagate_unary_operator(op, false_expr);
			
			Source source;
			
			source
				<< "("
					<< "(" << cond.prettyprint() << ")"
				<< "?"
					<< "(" << true_expr.prettyprint() << ")"
				<< ":"
					<< "(" << false_expr.prettyprint() << ")"
				<< ")"
			;
			
			result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
		}
		else if (op == Expression::DERREFERENCE)
		{
			if (expr.is_unary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				if (op0 == Expression::REFERENCE)
				{
					result = expr.get_unary_operand();
				}
			}
		}
		else if (op == Expression::REFERENCE)
		{
			if (expr.is_unary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				if (op0 == Expression::DERREFERENCE)
				{
					result = expr.get_unary_operand();
				}
			}
		}
		else if (op == Expression::PLUS)
		{
			result = expr;
		}
		else if (op == Expression::MINUS)
		{
			expr = simplify(expr);
			ObjectList<Expression> term_list;
			build_term_list(expr, Expression::ADDITION, term_list);
			
			for (unsigned int i = 0; i < term_list.size(); i++)
			{
				Expression &term = term_list[i];
				Expression::OperationKind term_op = term.get_operation_kind();
				
				switch (term_op)
				{
					case Expression::DERREFERENCE:
					case Expression::REFERENCE:
					case Expression::SHIFT_LEFT:
					case Expression::SHIFT_RIGHT:
					case Expression::LOGICAL_AND:
					case Expression::LOGICAL_OR:
					case Expression::LOGICAL_NOT:
					case Expression::BITWISE_AND:
					case Expression::BITWISE_OR:
					case Expression::BITWISE_XOR:
					case Expression::BITWISE_NOT:
					case Expression::LOWER_THAN:
					case Expression::GREATER_THAN:
					case Expression::LOWER_EQUAL_THAN:
					case Expression::GREATER_EQUAL_THAN:
					case Expression::COMPARISON:
					case Expression::DIFFERENT:
					case Expression::PREINCREMENT:
					case Expression::POSTINCREMENT:
					case Expression::PREDECREMENT:
					case Expression::POSTDECREMENT:
						{
							Source source;
							source << "-(" << term.prettyprint() << ")";
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					case Expression::PLUS:
						{
							Source source;
							source << "-(" << term.get_unary_operand().prettyprint() << ")";
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					case Expression::MINUS:
						term = term.get_unary_operand();
						break;
					case Expression::ADDITION:
						{
							// This case should not happen since we built the term list based on the ADDITION operator
							Expression term1 = term.get_first_operand();
							Expression term2 = term.get_second_operand();
							Source source;
							source
								<< "(" << propagate_unary_operator(Expression::MINUS, term1).prettyprint() << ")"
								<< "+"
								<< "(" << propagate_unary_operator(Expression::MINUS, term2).prettyprint() << ")"
							;
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					case Expression::SUBSTRACTION:
						{
							Expression term1 = term.get_first_operand();
							Expression term2 = term.get_second_operand();
							Source source;
							source
								<< "(" << propagate_unary_operator(Expression::MINUS, term1).prettyprint() << ")"
								<< "+"
								<< "(" << term2.prettyprint() << ")"
							;
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					case Expression::MULTIPLICATION:
					case Expression::DIVISION:
					case Expression::MODULUS:
						{
							Expression term1 = term.get_first_operand();
							Expression term2 = term.get_second_operand();
							Source source;
							source
								<< "(" << propagate_unary_operator(Expression::MINUS, term1).prettyprint() << ")"
								<< operator_to_string(term_op)
								<< "(" << term2.prettyprint() << ")"
							;
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					case Expression::CONDITIONAL:
						{
							Expression cond = term.get_condition_expression();
							Expression true_term = term.get_true_expression();
							Expression false_term = term.get_false_expression();
							
							true_term = propagate_unary_operator(Expression::MINUS, true_term);
							false_term = propagate_unary_operator(Expression::MINUS, false_term);
							
							Source source;
							
							source
								<< "("
									<< "(" << cond.prettyprint() << ")"
								<< "?"
									<< "(" << true_term.prettyprint() << ")"
								<< ":"
									<< "(" << false_term.prettyprint() << ")"
								<< ")"
							;
							
							term = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
						}
						break;
					default:
						std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
						throw FatalException();
						break;
				}
			} // For each term
			
			Source source;
			source << "(" << simplify(term[0]).prettyprint() << ")";
			for (unsigned int i = 1; i < term_list.size(); i++)
			{
				Expression &term = term_list[i];
				source << " + " << "(" << simplify(term[i]).prettyprint() << ")";
			}
			result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
		}
		else if (op == Expression::LOGICAL_NOT)
		{
			if (expr.is_unary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				if (op0 == Expression::LOGICAL_NOT)
				{
					result = expr.get_unary_operand();
				}
			}
			else if (expr.is_binary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				Expression expr1 = expr.get_first_operand();
				Expression expr2 = expr.get_second_operand();
				
				if (op0 == Expression::LOGICAL_OR)
				{
					Source source;
					source
						<< "(" << propagate_unary_operator(Expression::LOGICAL_NOT, expr1).prettyprint() << ")"
						<< "&&"
						<< "(" << propagate_unary_operator(Expression::LOGICAL_NOT, expr2).prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				else if (op0 == Expression::LOGICAL_AND)
				{
					Source source;
					source
						<< "(" << propagate_unary_operator(Expression::LOGICAL_NOT, expr1).prettyprint() << ")"
						<< "||"
						<< "(" << propagate_unary_operator(Expression::LOGICAL_NOT, expr2).prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				else
				{
					Source source;
					source
						<< "!(" << expr.prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
			}
			else
			{
				Source source;
				source
					<< "!(" << expr.prettyprint() << ")"
				;
				result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
			}
		}
		else if (op == Expression::BITWISE_NOT)
		{
			expr = simplify(expr);
			
			if (expr.is_unary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				if (op0 == Expression::BITWISE_NOT)
				{
					result = expr.get_unary_operand();
				}
			}
			else if (expr.is_binary_operation())
			{
				Expression::OperationKind op0 = expr.get_operation_kind();
				Expression expr1 = expr.get_first_operand();
				Expression expr2 = expr.get_second_operand();
				
				if (op0 == Expression::BITWISEL_OR)
				{
					Source source;
					source
						<< "(" << propagate_unary_operator(Expression::BITWISE_NOT, expr1).prettyprint() << ")"
						<< "&"
						<< "(" << propagate_unary_operator(Expression::BITWISE_NOT, expr2).prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				else if (op0 == Expression::BITWISE_AND)
				{
					Source source;
					source
						<< "(" << propagate_unary_operator(Expression::BITWISE_NOT, expr1).prettyprint() << ")"
						<< "|"
						<< "(" << propagate_unary_operator(Expression::BITWISE_NOT, expr2).prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
				else
				{
					Source source;
					source
						<< "~(" << expr.prettyprint() << ")"
					;
					result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				}
			}
			else
			{
				Source source;
				source
					<< "~(" << expr.prettyprint() << ")"
				;
				result = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
			}
		}
		else
		{
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << result.prettyprint() << "'" << std::endl;
		
		return result;
	}
	
	
	std::string ParameterExpression::operator_to_string(Expression::OperationKind op)
	{
		switch (op)
		{
			case Expression::DERREFERENCE:
				return "*";
			case Expression::REFERENCE:
				return "&";
			case Expression::PLUS:
				return "+";
			case Expression::MINUS:
				return "-";
			case Expression::ADDITION:
				return "+";
			case Expression::SUBSTRACTION:
				return "-";
			case Expression::MULTIPLICATION:
				return "*";
			case Expression::DIVISION:
				return "/";
			case Expression::MODULUS:
				return "%";
			case Expression::SHIFT_LEFT:
				return "<<";
			case Expression::SHIFT_RIGHT:
				return ">>";
			case Expression::LOGICAL_OR:
				return "||";
			case Expression::LOGICAL_AND:
				return "&&";
			case Expression::LOGICAL_NOT:
				return "!";
			case Expression::BITWISE_OR:
				return "|";
			case Expression::BITWISE_AND:
				return "&";
			case Expression::BITWISE_XOR:
				return "^";
			case Expression::BITWISE_NOT:
				return "~";
			default:
				std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
				throw FatalException();
				break;
		}
		return "_ICE_";
	}
	
	
	
	bool ParameterExpression::match(Expression expr1, Expression expr2)
	{
std::cout << recursion_prefix << "Comparing '" << expr1.prettyprint() << "' with '" << expr2.prettyprint() << "'" << std::endl;
recursion_prefix.append("\t");
		
		AST_t ref_ast = expr1.get_ast();
		ScopeLink scope_link = expr1.get_scope_link();
		
		Source source;
		source << "(" << expr1.prettyprint() << ") - (" << expr2.prettyprint() << ")";
		Expression comparison_expr(source.parse_expression(ref_ast, scope_link), scope_link);
		
		Expression simplified_comparison_expr = simplify(comparison_expr);
		
		if (simplified_comparison_expr.prettyprint() == "0")
		{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
			return true;
		}
		else
		{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
			return micro_match(expr1, expr2);
		}
	}
	
	
	Expression ParameterExpression::simplify(Expression expr)
	{
std::cout << recursion_prefix << "Simplifying '" << expr.prettyprint() << "'" << std::endl;
recursion_prefix.append("\t");
		
		AST_t ref_ast = expr.get_ast();
		ScopeLink scope_link = expr.get_scope_link();
		
		if (expr.is_binary_operation())
		{
			Expression expr1 = expr.get_first_operand();
			Expression expr2 = expr.get_second_operand();
			Expression::OperationKind op = expr.get_operation_kind();
			
			// Simplify both operands
			{
				Source source;
				source
					<< "(" << simplify(expr1).prettyprint() << ")"
					<< operator_to_string(op)
					<< "(" << simplify(expr2).prettyprint() << ")"
				;
				expr = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				
				expr1 = expr.get_first_operand();
				expr2 = expr.get_second_operand();
			}
			
			// Lower binding operators if possible
			if (is_binding_operator(op))
			{
				Expression::OperationKind new_op;
				expr2 = propagate_binding_operator(op, expr2, /* OUT */ new_op);
				
				Source source;
				source
					<< "(" << expr1.prettyprint() << ")"
					<< operator_to_string(new_op)
					<< simplify(expr2).prettyprint() // expr2 has no parenthesis since new_op is applied only to its first term
				;
				expr = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
				
				expr1 = expr.get_first_operand();
				expr2 = expr.get_second_operand();
				op = expr.get_operation_kind();
				
				if (is_binding_operator(op))
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
					throw FatalException();
				}
			}
			
			// Distributive 1: (11 op1 12) op 2
			if (expr1.is_binary_operation())
			{
				Expression::OperationKind op1 = expr1.get_operation_kind();
				if (operators_are_distributive(op, op1))
				{
					Expression expr11 = expr1.get_first_operand();
					Expression expr12 = expr1.get_second_operand();
					
					Source source;
					source
						<< "(" << expr11.prettyprint() << ")" << operator_to_string(op) << "(" << expr2.prettyprint()
						<< operator_to_string(op1)
						<< "(" << expr12.prettyprint() << ")" << operator_to_string(op) << "(" << expr2.prettyprint()
					;
					expr = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
					
					expr = simplify(expr);
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << expr.prettyprint() << "'" << std::endl;
					return expr;
				}
			}
			
			// Distributive 2: 1 op (21 op2 22)
			if (expr2.is_binary_operation())
			{
				Expression::OperationKind op2 = expr2.get_operation_kind();
				if (operators_are_distributive(op, op2))
				{
					Expression expr21 = expr2.get_first_operand();
					Expression expr22 = expr2.get_second_operand();
					
					Source source;
					source
						<< "(" << expr1.prettyprint() << ")" << operator_to_string(op) << "(" << expr21.prettyprint()
						<< operator_to_string(op2)
						<< "(" << expr1.prettyprint() << ")" << operator_to_string(op) << "(" << expr22.prettyprint()
					;
					expr = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
					
					expr = simplify(expr);
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << expr.prettyprint() << "'" << std::endl;
					return expr;
				}
			}
			
			// Build a list of terms and try to simplify it
			{
				ObjectList<Expression> terms;
				ObjectList<bool> remove_term;
				
				terms = build_term_list(expr, op);
				for (unsigned int i=0; i < terms.size(); i++)
				{
					remove_term.push_back(false);
				}
				
				for (unsigned int i=0; i < terms.size(); i++)
				{
					Expression expr_i = terms[i];
					
					// Remove neuters if possible
					if (operator_has_neuter(op))
					{
						if (match(expr_i, get_neuter_for_operator(op)))
						{
							remove_term[i] = true;
							continue; // Go to next i iteration
						}
					}
					
					for (unsigned int j=i; j < terms.size(); j++)
					{
						if (remove_term[j])
						{
							continue; // j eliminated in previous i iteration
						}
						
						Expression expr_j = terms[j];
						
						// Check for operands that together produce the identity
						if (operator_can_produce_identity(op))
						{
							if (do_evaluate_to_identity(expr_i, expr_j, op))
							{
								expr = get_identity_for_operator(op);
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << expr.prettyprint() << "'" << std::endl;
								return expr;
							}
						}
						
						// Remove opposites if possible
						if (operator_can_produce_neuter(op))
						{
							if (do_evaluate_to_neuter(expr_i, expr_j, op))
							{
								remove_term[i] = true;
								remove_term[j] = true;
								break; // Go to the next i iteration
							}
						}
						
						// Remove duplicates if possible
						if (operator_neuter_is_same(op))
						{
							if (match(expr_i, expr_j))
							{
								remove_term[j] = true;
							}
						}
					}
				}
				
				Source source;
				bool has_any_term = false;
				for (unsigned int i=0; i < terms.size(); i++)
				{
					if (!remove_term[i])
					{
						if (has_any_term)
						{
							source << operator_to_string(op);
						}
						has_any_term = true;
						source << "(" << terms[i].prettyprint() << ")";
					}
				}
				
				if (!has_any_term)
				{
					if (!operator_has_neuter(op))
					{
						std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
						throw FatalException();
					}
					expr = get_neuter_for_operator(op);
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << expr.prettyprint() << "'" << std::endl;
					return expr;
				}
				
				expr = Expression(source.parse_expression(ref_ast, scope_link), scope_link);
			} // Term list reduction
			
		} // Binary operation
		else if (expr.is_unary_operation())
		{
			Expression::OperationKind op = expr.get_unary_operation();
			Expression expr1 = expr.get_unary_operand();
			
			expr = propagate_unary_operator(op, expr1);
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is '" << expr.prettyprint() << "'" << std::endl;
			return expr;
			
		}
		else
		{
			
			// Neither an unary operation nor a binary operation
			// Nothing to see here, move along ... (at some point micro_match will take care of it)
		}
		
	}
	
	
	bool ParameterExpression::micro_match(Expression expr1, Expression expr2)
	{
std::cout << recursion_prefix << "Micro-comparing '" << expr1.prettyprint() << "' with '" << expr2.prettyprint() << "'" << std::endl;
recursion_prefix.append("\t");
		if (expr1.is_id_expression())
		{
			if (!expr2.is_id_expression())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			Symbol symbol1 = expr1.get_id_expression().get_symbol();
			Symbol symbol2 = expr2.get_id_expression().get_symbol();
			
			if (symbol1.is_parameter())
			{
				if (!symbol2.is_parameter())
				{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
					return false;
				}
				
				if (symbol1.get_parameter_position() == symbol2.get_parameter_position())
				{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
					return true;
				}
				else
				{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
					return false;
				}
			}
			else
			{
				// FIXME: This is probably wrong in some cases
				if (symbol1.get_qualified_name() == symbol2.get_qualified_name())
				{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
					return true;
				}
				else
				{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
					return false;
				}
			}
		}
		else if (expr1.is_binary_operation() || expr1.is_operation_assignment())
		{
			if (expr1.is_binary_operation() != expr2.is_binary_operation())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			if (expr1.is_operation_assignment() != expr2.is_operation_assignment())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (expr1.get_operation_kind() != expr2.get_operation_kind())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			Expression expr11 = expr1.get_first_operand();
			Expression expr12 = expr1.get_second_operand();
			Expression expr21 = expr2.get_first_operand();
			Expression expr22 = expr2.get_second_operand();
			
			if (expr1.is_operation_assignment() && !match(expr11, expr21))
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			switch (expr1.get_operation_kind())
			{
				case Expression::ADDITION:
				case Expression::MULTIPLICATION:
				case Expression::LOGICAL_OR:
				case Expression::LOGICAL_AND:
				case Expression::BITWISE_OR:
				case Expression::BITWISE_AND:
				case Expression::BITWISE_XOR:
				case Expression::COMPARISON:
				case Expression::DIFFERENT:
					if (
							(match(expr11, expr21) && match(expr12, expr22))
							||
							(match(expr11, expr22) && match (expr12, expr21))
						)
					{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
						return true;
					}
					else
					{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
						return false;
					}
					;
					break;
				case Expression::SUBSTRACTION:
				case Expression::DIVISION:
				case Expression::MODULUS:
				case Expression::SHIFT_LEFT:
				case Expression::SHIFT_RIGHT:
				case Expression::LOWER_THAN:
				case Expression::GREATER_THAN:
				case Expression::LOWER_EQUAL_THAN:
				case Expression::GREATER_EQUAL_THAN:
					if (match(expr11, expr21) && match(expr12, expr22))
					{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
						return true;
					}
					else
					{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
						return false;
					}
					break;
				default:
					break;
			}
			std::cerr << __FILE__ << ":" << __LINE__ << ": Internal compiler error" << std::endl;
			throw FatalException();
		}
		else if (expr1.is_unary_operation())
		{
			if (!expr2.is_unary_operation())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (expr1.get_operation_kind() != expr2.get_operation_kind())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (match(expr1.get_unary_operand(), expr2.get_unary_operand()))
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_casting())
		{
			if (!expr2.is_casting())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			// FIXME: Compare the type of the cast (which turns out to be an AST_t)
			
			return match(expr1.get_casted_expression(), expr2.get_casted_expression());
		}
		else if (expr1.is_literal())
		{
			if (!expr2.is_literal())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			return Calculator::numbers_are_equal(expr1, expr2);
		}
		else if (expr1.is_function_call())
		{
			if (!expr2.is_function_call())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			ObjectList<Expression> arguments1 = expr1.get_argument_list();
			ObjectList<Expression> arguments2 = expr2.get_argument_list();
			if (arguments1.size() != arguments2.size())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			bool matches = match(expr1.get_called_expression(), expr2.get_called_expression());
			for (unsigned int index = 0; index < arguments1.size() && matches; index++)
			{
				matches &= match(arguments1[index], arguments2[index]);
			}
			
			if (matches)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_assignment())
		{
			if (!expr2.is_assignment())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (
					match(expr1.get_first_operand(), expr2.get_first_operand())
					&&
					match(expr1.get_second_operand(), expr2.get_second_operand())
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_array_subscript())
		{
			if (!expr2.is_array_subscript())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (
					match(expr1.get_subscript_expression(), expr2.get_subscript_expression())
					&&
					match(expr1.get_subscripted_expression(), expr2.get_subscripted_expression())
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_member_access())
		{
			if (!expr2.is_member_access())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (!match(expr1.get_accessed_entity(), expr2.get_accessed_entity()))
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			// FIXME: Do this properly
			if (
					expr1.get_accessed_member().get_symbol().get_qualified_name()
					==
					expr2.get_accessed_member().get_symbol().get_qualified_name()
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_pointer_member_access())
		{
			if (!expr2.is_pointer_member_access())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (!match(expr1.get_accessed_entity(), expr2.get_accessed_entity()))
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			// FIXME: Do this properly
			if (
					expr1.get_accessed_member().get_symbol().get_qualified_name()
					==
					expr2.get_accessed_member().get_symbol().get_qualified_name()
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			else
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
		}
		else if (expr1.is_conditional())
		{
			if (!expr2.is_conditional())
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
				return false;
			}
			
			if (
					match(expr1.get_condition_expression(), expr2.get_condition_expression())
					&& match(expr1.get_true_expression(), expr2.get_true_expression())
					&& match (expr1.get_false_expression(), expr2.get_false_expression())
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			if (
					do_evaluate_to_identity(expr1.get_condition_expression(), expr2.get_condition_expression(), Expression::LOGICAL_OR)
					&& match(expr1.get_true_expression(), expr2.get_false_expression())
					&& match (expr1.get_false_expression(), expr2.get_true_expression())
				)
			{
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is true" << std::endl;
				return true;
			}
			
		}
		
recursion_prefix.erase(recursion_prefix.begin());
std::cout << recursion_prefix << "Result is false" << std::endl;
		return false;
	}
	
	
#endif
	void ParameterExpression::substitute(/* INOUT */ Expression expression, ObjectList<Expression> parameters, ScopeLink scope_link)
	{
		ObjectList<AST_t> id_expressions = expression.get_ast().depth_subtrees(IdExpression::predicate);
		for (ObjectList<AST_t>::iterator it = id_expressions.begin(); it != id_expressions.end(); it++)
		{
			AST_t &ast = *it;
			IdExpression id_expression(ast, scope_link);
			Symbol symbol = id_expression.get_symbol();
			
			if (symbol.is_parameter())
			{
				ast.replace(parameters[symbol.get_parameter_position()].get_ast().duplicate());
			}
		}
	}
	
	
}

