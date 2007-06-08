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
#include "tl-targetstreaminfo.hpp"

#include <assert.h>

#include "tl-streaminfo.hpp"
#include "tl-taskgroupinfo.hpp"
#include "tl-taskinfo.hpp"

namespace TL
{

// TargetStreamInfo constructor ------------------------------------------------
TargetStreamInfo::
TargetStreamInfo
		( const Symbol& symbol
		, const std::string& label
		)
		: _task_info_istream((TaskInfo*)0)
		, _label(label)
		, _task_info_ostream((TaskInfo*)0)
		, _symbol(symbol)
{
	init_name();
}

// TargetStreamInfo destructor -------------------------------------------------
TargetStreamInfo::
~TargetStreamInfo()
{
}

// get_input_stream_info -------------------------------------------------------
InputStreamInfo*   
TargetStreamInfo::
get_input_stream_info
        ( void
        ) const
{
    return get_stream_info()->get_input_stream_info();
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

// get_name --------------------------------------------------------------------
const std::string& 
TargetStreamInfo::
get_name
		( void
		) const
{
	return _name;
}

// get_output_stream_info ------------------------------------------------------
OutputStreamInfo*   
TargetStreamInfo::
get_output_stream_info
        ( void
        ) const
{
    return get_stream_info()->get_output_stream_info();
}

// get_target_info -------------------------------------------------------------
StreamInfo*
TargetStreamInfo::
get_stream_info
		( void
		) const
{
	// is not setted until both target_info_ostream and istream are setted.
	assert(_stream_info);
	
	return _stream_info;
}

// get_symbol ------------------------------------------------------------------
const Symbol&      
TargetStreamInfo::
get_symbol
		( void
		) const
{
	return _symbol;
}

// set_input_task_info ---------------------------------------------------------
void               
TargetStreamInfo::
set_task_info_istream
		( TaskInfo *input_task_info
		, bool task_controled_stream
		)
{
	assert(input_task_info);
	assert(!_task_info_istream);
	
	_task_info_istream= input_task_info;
	_task_info_istream_pop= task_controled_stream;
	
	if (_task_info_ostream) { init_stream_info(); } 
}

// set_output_task_info --------------------------------------------------------
void               
TargetStreamInfo::
set_task_info_ostream
		( TaskInfo *output_task_info
		, bool task_controled_stream
		)
{
	assert(output_task_info);
	assert(!_task_info_ostream);
	
	_task_info_ostream= output_task_info;
	_task_info_ostream_push= task_controled_stream;

	if (_task_info_istream) { init_stream_info(); } 
}

// init_name -------------------------------------------------------------------
void
TargetStreamInfo::
init_name
		( void
		)
{
    std::stringstream ss;
    
    ss  << "acolib__" 
        << _label; 
    
    _name= ss.str();
}

// init_stream_info ------------------------------------------------------------
void
TargetStreamInfo::
init_stream_info
		( void
		)
{
	assert(_task_info_istream);
	assert(_task_info_ostream);
	
	assert	(  
			_task_info_istream->get_taskgroup_info()
			== 
			_task_info_ostream->get_taskgroup_info()
			);	

	TaskgroupInfo* taskgroup_info= _task_info_istream->get_taskgroup_info();
	_stream_info= taskgroup_info->
			new_stream_info(_symbol, _task_info_ostream, _task_info_istream);
			
	if (_task_info_istream_pop)
	{
		_task_info_istream->add_loop_pop(_stream_info->get_input_stream_info());
	}
	else
	{
		_task_info_istream->add_loop_control(_stream_info->get_input_stream_info());
	}
	if (_task_info_ostream_push)
	{
		_task_info_ostream->add_loop_push(_stream_info->get_output_stream_info());
	}
	else
	{
		_task_info_ostream->add_loop_close(_stream_info->get_output_stream_info());
	}
}


}
