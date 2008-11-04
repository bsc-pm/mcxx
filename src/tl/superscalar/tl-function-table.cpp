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

#include "tl-traverse.hpp"

#include "tl-ast-predicates.hpp"
#include "tl-augmented-symbol.hpp"

#include "tl-function-table.hpp"


namespace TL {
	
	namespace FunctionTableInternals {
		
		class FunctionDefinitionHandler : public TraverseFunctor
		{
			private:
				FunctionTable::table_t &_table;
				
			public:
				FunctionDefinitionHandler(FunctionTable::table_t &table)
					: _table(table)
				{
				}
				
				virtual void preorder(Context ctx, AST_t node)
				{
				}
				
				virtual void postorder(Context ctx, AST_t node)
				{
					FunctionDefinition function_definition(node, ctx.scope_link);
					AugmentedSymbol symbol = function_definition.get_function_name().get_symbol();
					_table.insert( FunctionTable::table_t::value_type(symbol.get_qualified_name(), symbol) );
				}
		};
		
		
		class FunctionDeclarationHandler : public TraverseFunctor
		{
			private:
				FunctionTable::table_t &_table;
				
			public:
				FunctionDeclarationHandler(FunctionTable::table_t &table)
					: _table(table)
				{
				}
				
				virtual void preorder(Context ctx, AST_t node)
				{
				}
				
				virtual void postorder(Context ctx, AST_t node)
				{
					DeclaredEntity function_declaration(node, ctx.scope_link);
					AugmentedSymbol symbol = function_declaration.get_declared_symbol();
					_table.insert( FunctionTable::table_t::value_type(symbol.get_qualified_name(), symbol) );
				}
		};
		
	} // namespace FunctionTableInternals
	
	
	FunctionTable::FunctionTable(AST_t translation_unit, ScopeLink scope_link)
		: _table()
	{
		DepthTraverse depth_traverse;
		
		TraverseASTPredicate function_definition_traverser(FunctionDefinition::predicate, AST_t::NON_RECURSIVE);
		FunctionTableInternals::FunctionDefinitionHandler function_definition_handler(_table);
		depth_traverse.add_functor(function_definition_traverser, function_definition_handler);
		
		FunctionDeclarationPredicate function_declaration_predicate(scope_link);
		TraverseASTPredicate function_declaration_traverser(function_declaration_predicate, AST_t::NON_RECURSIVE);
		FunctionTableInternals::FunctionDeclarationHandler function_declaration_handler(_table);
		depth_traverse.add_functor(function_declaration_traverser, function_declaration_handler);
		
		depth_traverse.traverse(translation_unit, scope_link);
	}
	
}

