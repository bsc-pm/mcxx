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
#include "tl-targetstreaminfo.hpp"

#include <assert.h>

#include "tl-inputstreaminfo.hpp"
#include "tl-outputstreaminfo.hpp"
#include "tl-streaminfo.hpp"
#include "tl-taskgroupinfo.hpp"
#include "tl-taskinfo.hpp"

namespace TL
{

// TargetStreamInfo constructor ------------------------------------------------
TargetStreamInfo::
TargetStreamInfo
		( const std::string& label
		) 
        : _input_stream_info((InputStreamInfo*)0)
		, _label(label)
        , _output_stream_info((OutputStreamInfo*)0)
		, _stream_info((StreamInfo*)0)
{
}

// TargetStreamInfo destructor -------------------------------------------------
TargetStreamInfo::
~TargetStreamInfo()
{
    // fails if no link created, !_input || !_output
    assert(_stream_info);
}

// get_input_stream_info -------------------------------------------------------
InputStreamInfo*   
TargetStreamInfo::
get_input_stream_info
        ( void
        ) const
{
    return _input_stream_info;
}

// get_label ------------------------------------------------------------------- 			
const std::string& 
TargetStreamInfo::
get_label
		( void
		) const
{
	return _label;
}

// get_output_stream_info ------------------------------------------------------
OutputStreamInfo*   
TargetStreamInfo::
get_output_stream_info
        ( void
        ) const
{
    return _output_stream_info;
}

// set_close_task --------------------------------------------------------------
void               
TargetStreamInfo::
set_close_task
        ( const Symbol& symbol
        , TaskInfo* task_info
        )
{
    set_output_task(symbol, task_info);
    
    task_info->add_loop_close(_output_stream_info);
}

// set_control_task ------------------------------------------------------------
void               
TargetStreamInfo::
set_control_task
        ( const Symbol& symbol
        , TaskInfo* task_info
        )
{
    set_input_task(symbol, task_info);
    
    task_info->add_loop_control(_input_stream_info);
}
 
// set_pop_task ----------------------------------------------------------------
void               
TargetStreamInfo::
set_pop_task
        ( const Symbol& symbol
        , TaskInfo* task_info
        )
{
    set_input_task(symbol, task_info);
    
    task_info->add_loop_pop(_input_stream_info);
}

// set_push_stack --------------------------------------------------------------
void               
TargetStreamInfo::
set_push_task
        ( const Symbol& symbol
        , TaskInfo* task_info
        )
{
    set_output_task(symbol, task_info);
    
    task_info->add_loop_push(_output_stream_info);
}

// init_stream_info ------------------------------------------------------------
void
TargetStreamInfo::
init_stream_info
		( void
		)
{
	assert(_input_stream_info);
	assert(_output_stream_info);
    assert(!_stream_info);
	
	assert	(  
			_input_stream_info->get_task_info()->get_taskgroup_info()
			== 
			_output_stream_info->get_task_info()->get_taskgroup_info()
			);	

	TaskgroupInfo* taskgroup_info= 
            _input_stream_info->get_task_info()->get_taskgroup_info();
	_stream_info= taskgroup_info->
			new_stream_info(_output_stream_info, _input_stream_info);
}

// set_input_task --------------------------------------------------------------
void 
TargetStreamInfo::
set_input_task
        ( const Symbol& symbol
        , TaskInfo* task_info
        )
{
    assert(task_info);
    assert(!_input_stream_info);
    
    _input_stream_info= new InputStreamInfo(symbol, task_info, _label);
    
    if (_output_stream_info) {
        init_stream_info();
    }
}

// set_output_task -------------------------------------------------------------
void 
TargetStreamInfo::
set_output_task
        ( const Symbol& symbol
        , TaskInfo* task_info)
{
    assert(task_info);
    assert(!_output_stream_info);
    
    _output_stream_info= new OutputStreamInfo(symbol, task_info, _label);

    if (_input_stream_info) {
        init_stream_info();
    }
}

}
