/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
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

