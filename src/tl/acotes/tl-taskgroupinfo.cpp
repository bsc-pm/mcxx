/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#include "tl-taskgroupinfo.hpp"


#include <assert.h>
#include <sstream>


#include "tl-streaminfo.hpp"
#include "tl-taskinfo.hpp"
#include "tl-transform.hpp"

namespace TL {

// TaskgroupInfo constructor ---------------------------------------------------
TaskgroupInfo::
TaskgroupInfo
		( 
		) 
{
	_task_info_phantom= new TaskInfo(this);
	
	init_name();
}
	
// TaskgroupInfo destructor ----------------------------------------------------
TaskgroupInfo::
~TaskgroupInfo
		( 
		) 
{
	// delete phantom
	delete _task_info_phantom;
	
	// remove all created streaminfo
	for 	( std::set<StreamInfo*>::iterator it= _stream_info_set.begin()
			; it != _stream_info_set.end()
			; it++
			)
	{
		StreamInfo* stream_info= *it;
		
		delete stream_info;
	}			
	
	// remove all created taskinfo
	for 	( std::set<TaskInfo*>::iterator it= _task_info_set.begin()
			; it != _task_info_set.end()
			; it++
			)
	{
		TaskInfo* task_info= *it;
		
		delete task_info;
	}			
	
	// remove all created transforms
	for 	( std::list<Transform*>::iterator it= _transform_list.begin()
			; it != _transform_list.end()
			; it++
			)
	{
		Transform* transform= *it;
		
		delete transform;
	}			
}

// add_transform ---------------------------------------------------------------
void 
TaskgroupInfo::
add_transform
		( Transform* transform
		)
{
	assert(transform);
	
	_transform_list.push_back(transform);
}

// get_name --------------------------------------------------------------------
const std::string& 
TaskgroupInfo::
get_name
		(
		) const
{
	return _name;
}

// get_stream_info_set ---------------------------------------------------------
const std::set<StreamInfo*>& 
TaskgroupInfo::
get_stream_info_set
		( void
		) const
{
	return _stream_info_set;
}

// get_task_info_phantom -------------------------------------------------------
TaskInfo* 
TaskgroupInfo::
get_task_info_phantom
		(
		) const
{
	return _task_info_phantom;
}

// get_stream_info_set ---------------------------------------------------------
const std::set<TaskInfo*>& 
TaskgroupInfo::
get_task_info_set
		( void
		) const
{
	return _task_info_set;
}

// compute_graph ---------------------------------------------------------------
void
TaskgroupInfo::
compute_graph
	(
	)
{
//	TaskInfo* task_info_implicit= _task_info_phantom;
//			->get_task_info_first_child();
	
	_task_info_phantom->compute_graph();
}

// new_stream_info -------------------------------------------------------------
StreamInfo*
TaskgroupInfo::
new_stream_info
		( const Symbol& symbol
		, TaskInfo* task_info_ostream
		, TaskInfo* task_info_istream
		)
{
	// Createa a new taskinfo for this taskgroup
	StreamInfo* stream_info= new StreamInfo
			( symbol
			, task_info_ostream
			, task_info_istream
			);
	
	_stream_info_set.insert(stream_info);
	
	return stream_info;
}

// new_task_info ---------------------------------------------------------------
TaskInfo*
TaskgroupInfo::
new_task_info
		(
		)
{
	// Createa a new taskinfo for this taskgroup
	TaskInfo* task_info= new TaskInfo(this);
	
	_task_info_set.insert(task_info);
	
	return task_info;
}

// transform_all ---------------------------------------------------------------
void
TaskgroupInfo::
transform_all
		( 
		)
{
	// Apply all transformations and remove it
	for 	( std::list<Transform*>::iterator it= _transform_list.begin()
			; it != _transform_list.end()
			; it++
			)
	{
		Transform* transform= *it;
		
		transform->transform();
	}			
}

// initializatiors -------------------------------------------------------------
void
TaskgroupInfo::
init_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "taskgroup_"
		<< (long) this 
		;
	
	_name= ss.str();
}	

} // end namespace TL
