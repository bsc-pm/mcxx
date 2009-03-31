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
