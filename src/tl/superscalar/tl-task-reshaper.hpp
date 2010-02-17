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


#ifndef TL_TASK_RESHAPER_HPP
#define TL_TASK_RESHAPER_HPP

#include <map>
#include <string>

#include "tl-compilerphase.hpp"
#include "tl-langconstruct.hpp"
#include "tl-task-table.hpp"
#include "tl-traverse.hpp"
#include "tl-type.hpp"

#include "tl-ast-predicates.hpp"


namespace TL
{
	class TaskReshaper : public CompilerPhase
	{
		protected:
			class FunctionDefinitionHandler : public TraverseFunctor
			{
				private:
					
				public:
					FunctionDefinitionHandler(ScopeLink scope_link)
					{
					}
					
					virtual void preorder(Context ctx, AST_t node);
					virtual void postorder(Context ctx, AST_t node);
			};
			
			
			static PhaseStatus _status;
			
			
			void generate_reshaped_task_adapters_and_shapers(TaskTable &task_table, AST_t translation_unit, ScopeLink scope_link);
			
			
		public:
			virtual void run(DTO &dto);
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
	};
	
}


#endif // TL_TASK_RESHAPER_HPP
