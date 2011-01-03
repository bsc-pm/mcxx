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




#ifndef TL_FUNCTION_TABLE_HPP
#define TL_FUNCTION_TABLE_HPP

#include <map>
#include <string>

#include "tl-ast.hpp"
#include "tl-scopelink.hpp"

#include "tl-augmented-symbol.hpp"


namespace TL {
	namespace FunctionTableInternals {
		class FunctionDefinitionHandler;
		class FunctionDeclarationHandler;
	}
	
	class FunctionTable
	{
		private:
			typedef std::map<std::string, AugmentedSymbol> table_t;
			
			table_t _table;
			
			friend class FunctionTableInternals::FunctionDefinitionHandler;
			friend class FunctionTableInternals::FunctionDeclarationHandler;
		
		public:
			typedef table_t::iterator iterator;
			typedef table_t::const_iterator const_iterator;
			
			FunctionTable(AST_t translation_unit, ScopeLink scope_link);
			
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
			
			AugmentedSymbol operator[](std::string const &name)
			{
				iterator it = _table.find(name);
				if (it != _table.end())
				{
					return it->second;
				}
				else
				{
					return AugmentedSymbol::invalid();
				}
			}
			
			iterator find(std::string const &name)
			{
				return _table.find(name);
			}
			
	};
}


#endif // TL_FUNCTION_TABLE_HPP
