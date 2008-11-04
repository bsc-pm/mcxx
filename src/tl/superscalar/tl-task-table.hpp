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

#ifndef TL_TASK_TABLE_HPP
#define TL_TASK_TABLE_HPP

#include <set>

#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

#include "tl-augmented-symbol.hpp"


namespace TL {
	namespace TaskTableInternals {
		class FunctionDefinitionHandler;
		class FunctionDeclarationHandler;
	}
	
	class TaskTable
	{
		private:
			typedef std::set<AugmentedSymbol> table_t;
			
			table_t _table;
			
			friend class TaskTableInternals::FunctionDefinitionHandler;
			friend class TaskTableInternals::FunctionDeclarationHandler;
		
		public:
			typedef table_t::iterator iterator;
			typedef table_t::const_iterator const_iterator;
			
			TaskTable(AST_t translation_unit, ScopeLink scope_link);
			
			const_iterator begin() const
			{
				return _table.begin();
			}
			iterator begin()
			{
				return _table.begin();
			}
			
			iterator end()
			{
				return _table.end();
			}
			
	};
}


#endif // TL_TASK_TABLE_HPP
