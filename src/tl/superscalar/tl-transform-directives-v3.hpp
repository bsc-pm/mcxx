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
				register_directive("mutex");
				// task and target have been registered in TaskAnalysis
				on_directive_post["start"].connect(functor(&TransformDirectives::process_start, *this));
				on_directive_post["finish"].connect(functor(&TransformDirectives::process_finish, *this));
				on_directive_post["barrier"].connect(functor(&TransformDirectives::process_barrier, *this));
				on_directive_post["wait"].connect(functor(&TransformDirectives::process_wait, *this));
				on_directive_post["restart"].connect(functor(&TransformDirectives::process_restart, *this));
				on_directive_post["mutex"].connect(functor(&TransformDirectives::process_mutex, *this));
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
			void process_mutex(PragmaCustomConstruct directive);
			void process_task(PragmaCustomConstruct construct);
			void process_task_declaration(PragmaCustomConstruct construct);
			void process_task_definition(PragmaCustomConstruct construct);
			void process_target(PragmaCustomConstruct construct);
			void process_target_declaration(PragmaCustomConstruct construct);
			void process_target_definition(PragmaCustomConstruct construct);
			
	};
	
}


#endif // TL_TRANSFORM_DIRECTIVES_HPP
