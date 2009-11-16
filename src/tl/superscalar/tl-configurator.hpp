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


#ifndef TL_CONFIGURATOR_HPP
#define TL_CONFIGURATOR_HPP


#include "tl-compilerphase.hpp"


namespace TL
{
	class Configurator : public CompilerPhase
	{
		private:
			std::string _generate_task_side;
			std::string _generate_non_task_side;
			std::string _generate_task_ids;
			std::string _generate_task_adapters;
			std::string _align_memory;
			
		public:
			Configurator()
			{
				set_phase_name("Code generation configurator");
				set_phase_description("Sets up the code generation options.");
				register_parameter(
					"generate-task-side",
					"Specifies that the code corresponding to the task side must be generated.",
					_generate_task_side,
					"yes");
				register_parameter(
					"generate-non-task-side",
					"Specifies that the code corresponding to the non task side must be generated.",
					_generate_non_task_side,
					"yes");
				register_parameter(
					"generate-task-ids",
					"Specifies that task ids must be generated.",
					_generate_task_ids,
					"yes");
				register_parameter(
					"generate-task-adapters",
					"Specifies that task adapters must be generated.",
					_generate_task_adapters,
					"yes");
				register_parameter(
					"align-memory",
					"Specifies that memory allocation should be aligned.",
					_align_memory,
					"no");
			}
			
			virtual void pre_run(DTO &dto);
			virtual void run(DTO &dto);
	};
	
}


#endif // TL_CONFIGURATOR_HPP
