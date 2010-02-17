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


#ifndef TL_TASK_ANALYSIS_HPP
#define TL_TASK_ANALYSIS_HPP


#include "tl-pragmasupport.hpp"
#include "tl-predicateutils.hpp"
#include "tl-region.hpp"

#include "tl-exceptions.hpp"


namespace TL
{

	class TaskAnalysis : public PragmaCustomCompilerPhase
	{
		private:
			static PhaseStatus _status;
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
			
			static IdExpression get_base_id_expression(Expression expression);
			static ObjectList<Expression> get_array_access_indices(Expression expression);
			static std::string direction_to_name(Region::Direction direction);
			
			Region handle_parameter(AST_t construct_ast, AST_t context_ast, ScopeLink scope_link, std::string const &parameter_specification, std::string const &line_annotation, Region::Direction direction, Region::Reduction reduction, AugmentedSymbol &parameter_symbol);
			
			void process_task(PragmaCustomConstruct construct);
			void process_task(PragmaCustomConstruct construct, AST_t context_ast, DeclaredEntity declared_entity);
			
			void process_target(PragmaCustomConstruct construct);
			void process_target_on_definition(PragmaCustomConstruct construct);
			void process_target_on_declaration(PragmaCustomConstruct construct);
			
		public:
			TaskAnalysis() : PragmaCustomCompilerPhase("css")
			{
				register_construct("task");
				register_construct("target");
				on_directive_post["task"].connect(functor(&TaskAnalysis::process_task, *this));
				on_directive_post["target"].connect(functor(&TaskAnalysis::process_target, *this));
			}
			
			virtual void run(DTO& data_flow)
			{
				_status = PHASE_STATUS_OK;
				
				try
				{
					PragmaCustomCompilerPhase::run(data_flow);
				}
				catch (FatalException ex)
				{
					_status = PHASE_STATUS_ERROR;
				}
				
				set_phase_status(_status);
			}
			
	};


}


#endif // TL_TASK_ANALYSIS_HPP
