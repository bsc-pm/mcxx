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

#include <fstream>

#include "tl-langconstruct.hpp"

#include "tl-ast-predicates.hpp"
#include "tl-exceptions.hpp"
#include "tl-task-manifest.hpp"
#include "tl-type-utils.hpp"


namespace TL
{
	void TaskManifest::pre_run(DTO &dto)
    {
    }

	void TaskManifest::run(DTO &dto)
	{
		FunctionMap function_map = dto["superscalar_function_table"];
		
		if (_manifest_filename == std::string(""))
		{
			return;
		}
		
		std::ofstream manifest;
		
		manifest.open(_manifest_filename.c_str());
		if (!manifest.is_open())
		{
			std::cerr << "Error creating manifest file '" << _manifest_filename << "'" << std::endl;
			set_phase_status(PHASE_STATUS_ERROR);
			return;
		}
		
		for (FunctionMap::iterator it = function_map.begin(); it != function_map.end(); it++)
		{
			FunctionInfo &function_info = it->second;
			if (function_info._is_task)
			{
				manifest << function_info._name << std::endl;
			}
		}
		
		if (manifest.fail())
		{
			manifest.close();
			std::cerr << "Error creating manifest file '" << _manifest_filename << "'" << std::endl;
			set_phase_status(PHASE_STATUS_ERROR);
			return;
		}
		
		manifest.close();
		if (manifest.fail())
		{
			std::cerr << "Error creating manifest file '" << _manifest_filename << "'" << std::endl;
			set_phase_status(PHASE_STATUS_ERROR);
			return;
		}
	}
	
}


EXPORT_PHASE(TL::TaskManifest);

