/*
    Cell/SMP superscalar Compiler
    Copyright (C) 2007 Barcelona Supercomputing Center

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

#ifndef TL_TASK_ANALYSIS_HPP
#define TL_TASK_ANALYSIS_HPP


#include "tl-pragmasupport.hpp"
#include "tl-predicateutils.hpp"

#include "tl-exceptions.hpp"
#include "tl-function-data.hpp"


namespace TL
{

	class TaskAnalysis : public PragmaCustomCompilerPhase
	{
		private:
			FunctionMap _function_map;
			
			static IdExpression get_base_id_expression(Expression expression);
			static ObjectList<Expression> get_array_access_indices(Expression expression);
			static std::string direction_to_name(ParameterDirection direction);
			
			void handle_definition_parameter(PragmaCustomConstruct &construct, FunctionDefinition &task_definition, FunctionInfo &function_info, std::string const &parameter_specification, ParameterDirection direction);
			void handle_declaration_parameter(PragmaCustomConstruct &construct, DeclaredEntity &task_declaration, FunctionInfo &function_info, std::string const &parameter_specification, ParameterDirection direction);
			
			static PhaseStatus _status;
			
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
					_function_map = data_flow["superscalar_function_table"];
					PragmaCustomCompilerPhase::run(data_flow);
				}
				catch (FatalException ex)
				{
					_status = PHASE_STATUS_ERROR;
				}
				
				set_phase_status(_status);
			}
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
			
			void process_task(PragmaCustomConstruct construct);
			void process_target(PragmaCustomConstruct construct);
			
			void process_task_definition(PragmaCustomConstruct construct);
			void process_task_declaration(PragmaCustomConstruct construct);
			
			void process_target_on_definition(PragmaCustomConstruct construct);
			void process_target_on_declaration(PragmaCustomConstruct construct);
			
	};


}


#endif // TL_TASK_ANALYSIS_HPP
