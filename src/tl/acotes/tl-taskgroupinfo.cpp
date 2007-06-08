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
#include "tl-taskgroupinfo.hpp"


#include <assert.h>
#include <sstream>


#include "tl-streaminfo.hpp"
#include "tl-targetstreaminfo.hpp"
#include "tl-taskinfo.hpp"
#include "tl-transform.hpp"
#include "tl-transformtaskgroupreplace.hpp"

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
	
	// remove all target streams
	for		( std::map<std::string,TargetStreamInfo*>::iterator it= 
					_target_stream_info_map.begin()
			; it != _target_stream_info_map.end()
			; it++
			)
	{
		TargetStreamInfo* target_stream_info= (*it).second;
		
		delete target_stream_info;
	}
	
	delete _transform_taskgroup_replace;
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

// get_target_stream_info ------------------------------------------------------
TargetStreamInfo*            
TaskgroupInfo::
get_target_stream_info
		( const Symbol& symbol
		, const std::string& label
		)
{
	std::string key= label;
	TargetStreamInfo* target_stream_info;
	
	if (_target_stream_info_map.count(key) == 0)
	{
		target_stream_info= new TargetStreamInfo(symbol, label);
		_target_stream_info_map[key]= target_stream_info;
	}
	else // if (_target_stream_info_map.count(key) != 0)
	{
		target_stream_info= _target_stream_info_map[key];
	}
	
	return target_stream_info;
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

// get_transform_taskgroup_replace ---------------------------------------------
TransformTaskgroupReplace*   
TaskgroupInfo::
get_transform_taskgroup_replace
		( void
		) const
{	
	return _transform_taskgroup_replace;
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
	StreamInfo* stream_info= StreamInfo::create
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

// set_transform_taskgroup_replace ---------------------------------------------
void   
TaskgroupInfo::
set_transform_taskgroup_replace
		( TransformTaskgroupReplace* transform_taskgroup_replace
		) 
{	
	assert(transform_taskgroup_replace);
	
	_transform_taskgroup_replace= transform_taskgroup_replace;
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
