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


#include <set>

#include "tl-augmented-symbol.hpp"
#include "tl-function-table.hpp"

#include "tl-function-router.hpp"


namespace TL
{

	void TL::FunctionRouter::propagate_side_cohercion(AugmentedSymbol &caller, AugmentedSymbol &callee)
	{
		if (callee.has_coherced_sides())
		{
			// End of recursion
			return;
		}
		else
		{
			callee.set_as_task_side( callee.is_on_task_side() | caller.is_on_task_side() );
			callee.set_as_non_task_side( callee.is_on_non_task_side() | caller.is_on_non_task_side() );
			
			for (
				AugmentedSymbol::call_iterator it = callee.begin_callee_functions();
				it != callee.end_callee_functions();
				it++)
			{
				AugmentedSymbol callee2 = *it;
				if (!(((Symbol&)callee) == callee2))
					propagate_side_cohercion(callee, callee2);
			}
		}
	}
	
	
	void TL::FunctionRouter::mark_task_side_recursively(AugmentedSymbol &symbol)
	{
		symbol.set_as_task_side(true);
		for (
			AugmentedSymbol::call_iterator it = symbol.begin_callee_functions();
			it != symbol.end_callee_functions();
			it++)
		{
			AugmentedSymbol callee = *it;
			if (!callee.is_task() && !callee.is_on_task_side() && !callee.has_coherced_sides())
			{
				mark_task_side_recursively(callee);
			}
		}
	}
	
	
	void TL::FunctionRouter::mark_non_task_side_recursively(AugmentedSymbol &symbol)
	{
		symbol.set_as_non_task_side(true);
		for (
			AugmentedSymbol::call_iterator it = symbol.begin_callee_functions();
			it != symbol.end_callee_functions();
			it++)
		{
			AugmentedSymbol callee = *it;
			if (!callee.is_task() && !callee.is_on_non_task_side() && !callee.has_coherced_sides())
			{
				mark_non_task_side_recursively(callee);
			}
		}
	}
	
	
	void TL::FunctionRouter::run(DTO &dto)
	{
		AST_t translation_unit( dto["translation_unit"] );
		ScopeLink scope_link( dto["scope_link"] );
		
		FunctionTable function_table(translation_unit, scope_link);
		
		// Propagate task and non-task user specifiers from functions to their called functions
		for (FunctionTable::iterator it = function_table.begin(); it != function_table.end(); it++)
		{
			AugmentedSymbol symbol = it->second;
			if (symbol.has_coherced_sides())
			{
				// Forward
				for (AugmentedSymbol::call_iterator it2 = symbol.begin_callee_functions();
					it2 != symbol.end_callee_functions();
					it2++)
				{
					AugmentedSymbol callee = *it2;
					propagate_side_cohercion(symbol, callee);
				}
			}
		}
		
		// Mark task side
		for (FunctionTable::iterator it = function_table.begin(); it != function_table.end(); it++)
		{
			AugmentedSymbol symbol = it->second;
			if (symbol.is_task())
			{
				mark_task_side_recursively(symbol);
			}
		}
		
		// Mark non task side from non called functions unless it is a static function like spu_re.
		// In that case we do not mark it since it is not used and we do not know which side it should be in.
		for (FunctionTable::iterator it = function_table.begin(); it != function_table.end(); it++)
		{
			AugmentedSymbol symbol = it->second;
			if (symbol.begin_caller_functions() == symbol.end_caller_functions()
				&& !symbol.has_coherced_sides()
				&& !symbol.calls_to_taskside_coherced_function()
				&& !symbol.is_static())
			{
				mark_non_task_side_recursively(symbol);
			}
		}
		
		// Mark the rest as non task side
		for (FunctionTable::iterator it = function_table.begin(); it != function_table.end(); it++)
		{
			AugmentedSymbol symbol = it->second;
			if (!symbol.is_on_task_side() && !symbol.is_on_non_task_side() && !symbol.has_coherced_sides())
			{
				symbol.set_as_non_task_side(true);
			}
		}
		
	}

}


EXPORT_PHASE(TL::FunctionRouter);

