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

#ifndef TL_TRANSFORM_DIRECTIVES_HPP
#define TL_TRANSFORM_DIRECTIVES_HPP


#include "tl-pragmasupport.hpp"

#include "tl-exceptions.hpp"


namespace TL
{
	class TransformDirectives : public PragmaCustomCompilerPhase
	{
		protected:
			static PhaseStatus _status;
			
		public:
			TransformDirectives() : PragmaCustomCompilerPhase("css")
			{
				register_directive("start");
				register_directive("finish");
				register_directive("barrier");
				register_directive("wait");
				register_directive("restart");
				// task and target have been registered in TaskAnalysis
				on_directive_post["start"].connect(functor(&TransformDirectives::process_start, *this));
				on_directive_post["finish"].connect(functor(&TransformDirectives::process_finish, *this));
				on_directive_post["barrier"].connect(functor(&TransformDirectives::process_barrier, *this));
				on_directive_post["wait"].connect(functor(&TransformDirectives::process_wait, *this));
				on_directive_post["restart"].connect(functor(&TransformDirectives::process_restart, *this));
				on_directive_post["task"].connect(functor(&TransformDirectives::process_task, *this));
				on_directive_post["target"].connect(functor(&TransformDirectives::process_target, *this));
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
			
			static void fail()
			{
				_status = PHASE_STATUS_ERROR;
			}
			
			void process_start(PragmaCustomConstruct directive);
			void process_finish(PragmaCustomConstruct directive);
			void process_barrier(PragmaCustomConstruct directive);
			void process_wait(PragmaCustomConstruct directive);
			void process_restart(PragmaCustomConstruct directive);
			void process_task(PragmaCustomConstruct construct);
			void process_task_declaration(PragmaCustomConstruct construct);
			void process_task_definition(PragmaCustomConstruct construct);
			void process_target(PragmaCustomConstruct construct);
			void process_target_declaration(PragmaCustomConstruct construct);
			void process_target_definition(PragmaCustomConstruct construct);
			
	};
	
}


#endif // TL_TRANSFORM_DIRECTIVES_HPP
