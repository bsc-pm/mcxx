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
#ifndef TLTARGETINFO_HPP_
#define TLTARGETINFO_HPP_

#include <string>

namespace TL
{

class TaskgroupInfo;
class TaskInfo;

class TargetInfo
{
public:
	TargetInfo(TaskgroupInfo* taskgroup_info, TaskInfo* task_info, const 
			std::string& label);
	virtual ~TargetInfo();
	
private:
	std::string    _label;
	TaskInfo*      _task_info;
	TaskgroupInfo* _taskgroup_info;
	
	
};

}

#endif /*TLTARGETINFO_HPP_*/
