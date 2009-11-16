/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
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
