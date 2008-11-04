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

#ifndef TL_CALCULATOR_HPP
#define TL_CALCULATOR_HPP

#include "tl-langconstruct.hpp"


namespace TL {
	namespace CalculatorInternals {
		class Number;
	}
	
	class Calculator
	{
		private:
			static CalculatorInternals::Number * parse_constant(Expression expr);
			static Expression number_to_expression(CalculatorInternals::Number *number, AST_t ref_ast, ScopeLink scope_link);
			
		public:
			static Expression binary_operation(Expression expr1, Expression::OperationKind op, Expression expr2);
			static Expression unary_operation(Expression::OperationKind op, Expression expr);
			static bool numbers_are_equal(Expression expr1, Expression expr2);
	};
}


#endif // TL_CALCULATOR_HPP
