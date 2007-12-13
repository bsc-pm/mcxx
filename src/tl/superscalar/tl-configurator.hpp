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
					"align-memory",
					"Specifies that memory allocation should be aligned.",
					_align_memory,
					"no");
			}
			
			virtual void run(DTO &dto);
	};
	
}


#endif // TL_CONFIGURATOR_HPP
