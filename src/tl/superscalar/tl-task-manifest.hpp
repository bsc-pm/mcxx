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

#ifndef TL_TASK_MANIFEST_HPP
#define TL_TASK_MANIFEST_HPP


#include "tl-compilerphase.hpp"
#include "tl-objectlist.hpp"
#include "tl-predicateutils.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"

#include "tl-function-data.hpp"


namespace TL
{
	class TaskManifest : public CompilerPhase
	{
		private:
			std::string _manifest_filename;
			
		public:
			TaskManifest()
			{
				set_phase_name("Task Manifest Generator");
				set_phase_description("Generates the task manifest file.");
				register_parameter(
					"task-manifest",
					"Specifies the name for the task manifest file.",
					_manifest_filename,
					"");
			}
			
			virtual void run(DTO &dto);
	};
	
}




#endif // TL_TASK_MANIFEST_HPP
