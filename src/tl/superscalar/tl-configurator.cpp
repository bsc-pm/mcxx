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


#include "tl-builtin.hpp"
#include "tl-symbol.hpp"

#include "cxx-ss-attrnames.h"
#include "tl-configurator.hpp"


void TL::Configurator::pre_run(DTO &dto)
{
}


void TL::Configurator::run(DTO &dto)
{
	if (_generate_task_side == std::string("yes"))
	{
		dto.set_object("superscalar_generate_task_side", RefPtr<Bool>(new Bool(true)));
	}
	else if (_generate_task_side == std::string("no"))
	{
		dto.set_object("superscalar_generate_task_side", RefPtr<Bool>(new Bool(false)));
	}
	else
	{
		std::cerr << "Error: Invalid value '" << _generate_task_side << "' for the 'generate-task-side' option." << std::endl;
		set_phase_status(PHASE_STATUS_ERROR);
		return;
	}
	
	if (_generate_non_task_side == std::string("yes"))
	{
		dto.set_object("superscalar_generate_non_task_side", RefPtr<Bool>(new Bool(true)));
	}
	else if (_generate_non_task_side == std::string("no"))
	{
		dto.set_object("superscalar_generate_non_task_side", RefPtr<Bool>(new Bool(false)));
	}
	else
	{
		std::cerr << "Error: Invalid value '" << _generate_non_task_side << "' for the 'generate-non-task-side' option." << std::endl;
		set_phase_status(PHASE_STATUS_ERROR);
		return;
	}
	
	if (_generate_task_ids == std::string("yes"))
	{
		dto.set_object("superscalar_generate_task_ids", RefPtr<Bool>(new Bool(true)));
	}
	else if (_generate_task_ids == std::string("no"))
	{
		dto.set_object("superscalar_generate_task_ids", RefPtr<Bool>(new Bool(false)));
	}
	else
	{
		std::cerr << "Error: Invalid value '" << _generate_task_ids << "' for the 'generate-task-ids' option." << std::endl;
		set_phase_status(PHASE_STATUS_ERROR);
		return;
	}
	
	if (_generate_task_adapters == std::string("yes"))
	{
		dto.set_object("superscalar_generate_task_adapters", RefPtr<Bool>(new Bool(true)));
	}
	else if (_generate_task_adapters == std::string("no"))
	{
		dto.set_object("superscalar_generate_task_adapters", RefPtr<Bool>(new Bool(false)));
	}
	else
	{
		std::cerr << "Error: Invalid value '" << _generate_task_adapters << "' for the 'generate-task-adapters' option." << std::endl;
		set_phase_status(PHASE_STATUS_ERROR);
		return;
	}
	
	if (_align_memory == std::string("yes"))
	{
		dto.set_object("superscalar_align_memory", RefPtr<Bool>(new Bool(true)));
	}
	else if (_align_memory == std::string("no"))
	{
		dto.set_object("superscalar_align_memory", RefPtr<Bool>(new Bool(false)));
	}
	else
	{
		std::cerr << "Error: Invalid value '" << _align_memory << "' for the 'align-memory' option." << std::endl;
		set_phase_status(PHASE_STATUS_ERROR);
		return;
	}
}


EXPORT_PHASE(TL::Configurator);
