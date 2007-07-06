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
    
    $Id$
*/
#include "tl-threadtransformhelper.hpp"

#include <assert.h>

#include "tl-tasktransformhelper.hpp"

namespace TL
{
		
// create ----------------------------------------------------------------------
std::string
ThreadTransformHelper:: 
create
		( TaskInfo* task
		)
{
	std::stringstream ss;

	ss		<< "pthread_create"
			<< "( &(" << name(task) << ")"
			<< ", (void*) 0"
			<< ", " << TaskTransformHelper::outline_name(task)
			<< ", (void*) &" << task->get_state_name()
			<< ");"	
			;

	return ss.str();
}

// create_all ------------------------------------------------------------------
std::string 
ThreadTransformHelper:: 
create_all
		( const std::set<TaskInfo*>& ts
		)
{
	std::stringstream ss;
	
	for		( std::set<TaskInfo*>::iterator it= ts.begin()
			; it != ts.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << create(task_info);
	}
	
	return ss.str();
}

// declare ---------------------------------------------------------------------
std::string
ThreadTransformHelper:: 
declare
		( TaskInfo* task
		)
{
	std::stringstream ss;

	ss		<< "pthread_t "
			<< name(task)
			<< ";"	
			;

	return ss.str();
}

// declare_all ------------------------------------------------------------------
std::string 
ThreadTransformHelper:: 
declare_all
		( const std::set<TaskInfo*>& ts
		)
{
	std::stringstream ss;
	
	for		( std::set<TaskInfo*>::iterator it= ts.begin()
			; it != ts.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << declare(task_info);
	}
	
	return ss.str();
}

// join ------------------------------------------------------------------------
std::string
ThreadTransformHelper:: 
join
		( TaskInfo* task
		)
{
	std::stringstream ss;

	ss		<< "pthread_join"
			<< "( " << name(task) 
			<< ", (void*) 0"
			<< ");"	
			;

	return ss.str();
}

// join_all --------------------------------------------------------------------
std::string 
ThreadTransformHelper:: 
join_all
		( const std::set<TaskInfo*>& ts
		)
{
	std::stringstream ss;
	
	for		( std::set<TaskInfo*>::iterator it= ts.begin()
			; it != ts.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		ss << join(task_info);
	}
	
	return ss.str();
}

// name ------------------------------------------------------------------------
std::string
ThreadTransformHelper:: 
name
		( TaskInfo* task
		)
{
	std::stringstream ss;
	
	ss 		<< "acolib__"
			<< task->get_name()
			<< "_thread"
			;

	return ss.str();
}

// ThreadTransformHelper constructor -------------------------------------------
ThreadTransformHelper::ThreadTransformHelper()
{
	// Is a helper, no instances
	assert(0);
}

}
