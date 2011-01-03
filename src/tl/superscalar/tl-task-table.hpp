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
			
			TaskTable(AST_t translation_unit, ScopeLink scope_link, bool include_declarations = true);
			
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
