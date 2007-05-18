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

namespace TL
{

// TargetStreamInfo constructor ------------------------------------------------
TargetStreamInfo::
TargetStreamInfo
		( const Symbol& symbol
		, const std::string& label
		)
		: _input_task_info((TaskInfo*)0)
		, _label(label)
		, _output_task_info((TaskInfo*)0)
		, _symbol(symbol)
{
	init_name();
	init_istream_name();
	init_ostream_name();
}

// TargetStreamInfo destructor -------------------------------------------------
TargetStreamInfo::
~TargetStreamInfo()
{
}

// compute_name ----------------------------------------------------------------
std::string 
TargetStreamInfo::
compute_name
		( const Symbol& symbol
		, const std::string& label
		)
{
	std::stringstream ss;
	
	ss 	<< "acolib__" 
		<< symbol.get_name() 
		<< "__" 
		<< label; 
	
	return ss.str();
}

// get_istream_name ------------------------------------------------------------
const std::string& 
TargetStreamInfo::
get_istream_name
		( void
		) const
{
	return _istream_name;
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

// get_ostream_name ------------------------------------------------------------
const std::string& 
TargetStreamInfo::
get_ostream_name
		( void
		) const
{
	return _ostream_name;
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
set_input_task_info
		( TaskInfo *input_task_info
		)
{
	assert(!_input_task_info);
	
	_input_task_info= input_task_info;
}

// set_output_task_info --------------------------------------------------------
void               
TargetStreamInfo::
set_output_task_info
		( TaskInfo *output_task_info
		)
{
	assert(!_output_task_info);
	
	_output_task_info= output_task_info;
}

// init_istream_name -----------------------------------------------------------
void
TargetStreamInfo::
init_istream_name
		( void
		)
{
	std::stringstream ss;
	
	ss << "istream_" << get_name();
	
	_istream_name= ss.str();
}

// init_name -------------------------------------------------------------------
void
TargetStreamInfo::
init_name
		( void
		)
{
	_name= compute_name(_symbol, _label);
}

// init_ostream_name -----------------------------------------------------------
void
TargetStreamInfo::
init_ostream_name
		( void
		)
{
	std::stringstream ss;
	
	ss << "ostream_" << get_name();
	
	_ostream_name= ss.str();
}


}
