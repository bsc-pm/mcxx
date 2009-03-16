/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007-2009 Barcelona Supercomputing Center

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

#include "tl-ast.hpp"
#include "tl-langconstruct.hpp"
#include "tl-scopelink.hpp"

#include <iostream>


namespace TL
{
	class CallToNamedFunctionPredicate : public PredicateAttr
	{
		private:
			std::string _function_name;
			ScopeLink _scope_link;
			
		public:
			CallToNamedFunctionPredicate(std::string const &function_name, ScopeLink scope_link)
				: PredicateAttr(LANG_IS_FUNCTION_CALL), _function_name(function_name), _scope_link(scope_link)
			{
			}
			
			virtual bool operator()(AST_t& ast) const
			{
				if (!PredicateAttr::operator()(ast))
				{
					return false;
				}
				
				Expression function_call(ast, _scope_link);
				
				if (!function_call.is_function_call())
				{
					std::cerr << __FILE__ << ":" << __LINE__ << ": " << "Internal compiler error" << std::endl;
					return false;
				}
				
				Expression called_expresion = function_call.get_called_expression();
				if (!called_expresion.is_id_expression())
				{
					// We do not handle indirect calls (through pointers to functions)
					return false;
				}
				
				std::string function_name = called_expresion.get_id_expression().mangle_id_expression();
				
				return (function_name == _function_name);
			}
	};
	
	
	class FunctionDeclarationPredicate : public PredicateAttr
	{
		private:
			ScopeLink _scope_link;
			
		public:
			FunctionDeclarationPredicate(ScopeLink scope_link)
				: PredicateAttr(LANG_IS_DECLARED_NAME), _scope_link(scope_link)
			{
			}
			
			virtual bool operator()(AST_t& ast) const
			{
				if (!PredicateAttr::operator()(ast))
				{
					return false;
				}
				
				DeclaredEntity declared_entity(ast, _scope_link);
				Symbol symbol = declared_entity.get_declared_symbol();
				
				return symbol.is_function();
			}
	};
	
}
