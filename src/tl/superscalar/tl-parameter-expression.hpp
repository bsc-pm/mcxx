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

#ifndef TL_PARAMETER_EXPRESSION_HPP
#define TL_PARAMETER_EXPRESSION_HPP

#include "tl-langconstruct.hpp"


namespace TL {
	class ParameterExpression
	{
		private:
#if 0
			static Expression simplify(Expression expr);
			static bool micro_match(Expression expr1, Expression expr2);
			
			static void build_term_list(Expression expr, Expression::OperationKind op, ObjectList<Expression> /* OUT */ &term_list);
			
			static bool operators_are_sequence_shufflable(Expression::OperationKind op1, Expression::OperationKind op2);
			static bool is_binding_operator(Expression::OperationKind op);
			
			static bool operator_has_neuter(Expression::OperationKind op);
			static bool operator_can_produce_identity(Expression::OperationKind op);
			static bool operator_can_produce_neuter(Expression::OperationKind op);
			static bool operator_neuter_is_same(Expression::OperationKind op);
			static bool operator_null_is_same(Expression::OperationKind op);
			
			static bool operators_are_distributive(Expression::OperationKind op1, Expression::OperationKind op2);
			
			static Expression propagate_binding_operator(Expression::OperationKind op, Expression expr, Expression::OperationKind /* OUT */ &new_op);
			static Expression propagate_unary_operator(Expression::OperationKind op, Expression expr);
			
			static std::string operator_to_string(Expression::OperationKind op);
			static Expression get_neuter_for_operator(Expression::OperationKind op, AST_t ref_ast, ScopeLink scope_link);
			static Expression get_identity_for_operator(Expression::OperationKind op, AST_t ref_ast, ScopeLink scope_link);
#endif
			
		public:
#if 0
			static bool match(Expression expr1, Expression expr2);
#endif
			static void substitute(/* INOUT */ Expression &expression, ObjectList<Expression> parameters, ScopeLink scope_link);
	};
}


#endif // TL_PARAMETER_EXPRESSION_HPP
