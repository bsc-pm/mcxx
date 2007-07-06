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
#include "tl-targetinfo.hpp"

#include "tl-targetstreaminfo.hpp"
//#include "tl-taskinfo.hpp"
#include "tl-taskgroupinfo.hpp"

namespace TL
{

// TargetInfo constructor ------------------------------------------------------
TargetInfo::
TargetInfo
		( TaskgroupInfo* taskgroup_info
		, TaskInfo* task_info
		)
		: _task_info(task_info)
		, _taskgroup_info(taskgroup_info)
{
}

// TargetInfo destructor -------------------------------------------------------
TargetInfo::
~TargetInfo()
{
}

// add_input -------------------------------------------------------------------
void 
TargetInfo::
add_input
		( const Symbol& symbol
        , const std::string& label
		)
{
	TargetStreamInfo* target_stream_info=
            _taskgroup_info->get_target_stream_info(label);
            
    target_stream_info->set_control_task(symbol, _task_info);
	_input_stream_info_set.insert(target_stream_info->get_input_stream_info());
}

// add_output ------------------------------------------------------------------
void 
TargetInfo::
add_output
		( const Symbol& symbol
        , const std::string& label
		)
{
    TargetStreamInfo* target_stream_info=
            _taskgroup_info->get_target_stream_info(label);
            
    target_stream_info->set_close_task(symbol, _task_info);
	_output_stream_info_set.insert(target_stream_info->get_output_stream_info());
}

// get_input_stream_info_set ---------------------------------------------------
const 
std::set<InputStreamInfo*>& 
TargetInfo::
get_input_stream_info_set
		( void
		) const
{
	return _input_stream_info_set;
}

// get_output_stream_info_set -------------------------------------------------
const 
std::set<OutputStreamInfo*>& 
TargetInfo::
get_output_stream_info_set
		( void
		) const
{
	return _output_stream_info_set;
}


}
