/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
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




#include <fstream>

#include "tl-langconstruct.hpp"

#include "tl-augmented-symbol.hpp"
#include "tl-task-table.hpp"

#include "tl-task-manifest.hpp"


namespace TL
{
	void TaskManifest::pre_run(DTO &dto)
    {
    }

	void TaskManifest::run(DTO &dto)
	{
		AST_t translation_unit ( dto["translation_unit"] );
		ScopeLink scope_link = dto["scope_link"];
		
		std::ofstream manifest;
		
		// Check whether to actually create it or not
		if (_manifest_filename == "")
		{
			return;
		}
		
		manifest.open(_manifest_filename.c_str());
		if (!manifest.is_open())
		{
			std::cerr << "Error creating manifest file '" << _manifest_filename << "'" << std::endl;
			set_phase_status(PHASE_STATUS_ERROR);
			return;
		}
		
		TaskTable task_table(translation_unit, scope_link);
		
		for (TaskTable::iterator it = task_table.begin(); it != task_table.end(); it++)
		{
			AugmentedSymbol symbol = *it;
			manifest << symbol.get_qualified_name() << std::endl;
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

