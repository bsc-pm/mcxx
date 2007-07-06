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
#ifndef TLTARGETSTREAMINFO_HPP_
#define TLTARGETSTREAMINFO_HPP_

#include <string>

#include "tl-symbol.hpp"

namespace TL
{

class InputStreamInfo;
class OutputStreamInfo;
class TaskInfo;
class StreamInfo;

class TargetStreamInfo
{
public:
	TargetStreamInfo(const std::string& label);
	virtual ~TargetStreamInfo();
	
    InputStreamInfo*   get_input_stream_info(void) const;
	const std::string& get_label(void) const;
    OutputStreamInfo*  get_output_stream_info(void) const;
    void               set_control_task(const Symbol& symbol, TaskInfo* task_info); 
    void               set_pop_task(const Symbol& symbol, TaskInfo* task_info);
    void               set_push_task(const Symbol& symbol, TaskInfo* task_info);
    void               set_close_task(const Symbol& symbol, TaskInfo* task_info); 
	
private:
    InputStreamInfo*  _input_stream_info;
    std::string       _label;
    OutputStreamInfo* _output_stream_info;
	StreamInfo*       _stream_info;
	
	void init_stream_info(void);    
    void set_input_task(const Symbol& symbol, TaskInfo* task_info);
    void set_output_task(const Symbol& symbol, TaskInfo* task_info);
};

}

#endif /*TLTARGETSTREAMINFO_HPP_*/
