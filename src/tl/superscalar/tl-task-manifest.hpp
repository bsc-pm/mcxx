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




#ifndef TL_TASK_MANIFEST_HPP
#define TL_TASK_MANIFEST_HPP


#include "tl-compilerphase.hpp"
#include "tl-objectlist.hpp"
#include "tl-predicateutils.hpp"
#include "tl-scopelink.hpp"
#include "tl-traverse.hpp"


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
			
            virtual void pre_run(DTO &dto);
			virtual void run(DTO &dto);
	};
	
}




#endif // TL_TASK_MANIFEST_HPP
