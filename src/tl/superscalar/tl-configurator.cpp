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

#include "tl-builtin.hpp"

#include "tl-configurator.hpp"


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
