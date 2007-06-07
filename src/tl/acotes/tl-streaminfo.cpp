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
#include "tl-streaminfo.hpp"
				
#include <assert.h>
#include <sstream>


#include "tl-taskinfo.hpp"

namespace TL {

// StreamInfo constructor ------------------------------------------------------
StreamInfo::
StreamInfo
		( const Symbol& symbol
		, TaskInfo* task_info_ostream
		, TaskInfo* task_info_istream
		)
		: _symbol(symbol)
		, _task_info_istream(task_info_istream)
		, _task_info_ostream(task_info_ostream)
{
	assert(task_info_ostream);
	assert(task_info_istream);
	
	init_name();
	init_istream_name();
	init_ostream_name();
	init_symbol_name();
}

// get_istream_name ------------------------------------------------------------
const std::string& 
StreamInfo::
get_istream_name
		( void
		) const
{
	return _istream_name;
}

// get_name --------------------------------------------------------------------
const std::string& 
StreamInfo::
get_name
		( void
		) const
{
	return _name;
}

// get_ostream_name ------------------------------------------------------------
const std::string& 
StreamInfo::
get_ostream_name
		( void
		) const
{
	return _ostream_name;
}

// get_queue_length ------------------------------------------------------------
int 
StreamInfo::
get_queue_length
		( void
		) const
{
	return 128;
}

// get_symbol ------------------------------------------------------------------
const Symbol&      
StreamInfo::
get_symbol(void) const
{
	return _symbol;
}

// get_symbol_name -------------------------------------------------------------
const std::string&
StreamInfo::
get_symbol_name
		( void
		) const
{
	return _symbol_name;
}

// get_task_info_istream -------------------------------------------------------
TaskInfo* 
StreamInfo::
get_task_info_istream
		( void
		) const
{
	return _task_info_istream;
}

// get_task_info_ostream
TaskInfo* 
StreamInfo::
get_task_info_ostream
		( void
		) const
{
	return _task_info_ostream;
}



// init_istream_name -----------------------------------------------------------
void
StreamInfo::
init_istream_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "istream_"
		<< (long) this 
		;
	
	_istream_name= ss.str();
}

// init_name -------------------------------------------------------------------
void
StreamInfo::
init_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "stream_"
		<< (long) this 
		;
	
	_name= ss.str();
}

// init_ostream_name -----------------------------------------------------------
void
StreamInfo::
init_ostream_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "ostream_"
		<< (long) this 
		;
	
	_ostream_name= ss.str();
}

// init_symbol_name ------------------------------------------------------------
void
StreamInfo::
init_symbol_name
		(
		)
{
	_symbol_name= _symbol.get_name();
}



} // end namespace TL
