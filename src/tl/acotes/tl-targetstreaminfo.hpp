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
#ifndef TLTARGETSTREAMINFO_HPP_
#define TLTARGETSTREAMINFO_HPP_

#include <string>

#include "tl-symbol.hpp"

namespace TL
{

class TaskInfo;

class TargetStreamInfo
{
public:
	TargetStreamInfo(const Symbol& symbol, const std::string& label);
	virtual ~TargetStreamInfo();
	
	static std::string compute_name(const Symbol& symbol, const std::string& 
			label);
			
	const std::string& get_label(void) const;
	const std::string& get_name(void) const;
	const Symbol&      get_symbol(void) const;
	void               set_input_task_info(TaskInfo *input_task_info);
	void               set_output_task_info(TaskInfo *input_task_info);
	
private:
	TaskInfo*   _input_task_info;
	std::string _label;
	std::string _name;
	TaskInfo*   _output_task_info;
	Symbol      _symbol;
	
	void init_name(void);
};

}

#endif /*TLTARGETSTREAMINFO_HPP_*/
