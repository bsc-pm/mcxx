/*
	Acotes Translation Phase
	Copyright (C) 2007 - David Rodenas Pico <david.rodenas@bsc.es>
    Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
    Universitat Politecnica de Catalunya

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
#include "tl-statetransformhelper.hpp"

#include <assert.h>

#include "tl-taskinfo.hpp"
#include "tl-symboltransformhelper.hpp"

namespace TL
{

// copy_from_state -------------------------------------------------------------
std::string 
StateTransformHelper::
copy_from_state
		( TaskInfo* task_info
		)
{
	std::stringstream ss;
	
	ss << SymbolTransformHelper::copy_all_from_struct
			( task_info->get_lastprivates()
			, task_info->get_state_name()
			);
	
	return ss.str();
}

// copy_from_state_all ---------------------------------------------------------
std::string 
StateTransformHelper::
copy_from_state_all
		( const std::set<TaskInfo*>& task_info_set
		)
{
	std::stringstream ss;

	ss << "";
	for		( std::set<TaskInfo*>::iterator it= task_info_set.begin()
			; it != task_info_set.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << copy_from_state(task_info);
	}
	ss << "";
				
	return ss.str();
}

// copy_to_state ---------------------------------------------------------------
std::string 
StateTransformHelper::
copy_to_state
		( TaskInfo* task_info
		)
{
	std::stringstream ss;
	
	ss << SymbolTransformHelper::copy_all_to_struct
			( task_info->get_firstprivates()
			, task_info->get_state_name()
			);
	
	return ss.str();
}

// copy_to_state_all -----------------------------------------------------------
std::string 
StateTransformHelper::
copy_to_state_all
		( const std::set<TaskInfo*>& task_info_set
		)
{
	std::stringstream ss;

	ss << "";
	for		( std::set<TaskInfo*>::iterator it= task_info_set.begin()
			; it != task_info_set.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << copy_to_state(task_info);
	}
	ss << "";
				
	return ss.str();
}

// declare ---------------------------------------------------------------------
std::string 
StateTransformHelper::
declare
		( TaskInfo* task_info
		)
{
	std::stringstream ss;
	
	ss 	<< task_info->get_struct_state_name()
		<< " "
		<< task_info->get_state_name()
		<< ";";
	
	return ss.str();
}

// declare_all -----------------------------------------------------------------
std::string 
StateTransformHelper::
declare_all
		( const std::set<TaskInfo*>& task_info_set
		)
{
	std::stringstream ss;

	for		( std::set<TaskInfo*>::iterator it= task_info_set.begin()
			; it != task_info_set.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << declare(task_info);
	}
				
	return ss.str();
}

// StateTransformHelper constructor --------------------------------------------
StateTransformHelper::
StateTransformHelper()
{
	// no constructor
	assert(0);
}


}
