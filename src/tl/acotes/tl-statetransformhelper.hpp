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
#ifndef TLSTATETRANSFORMHELPER_HPP_
#define TLSTATETRANSFORMHELPER_HPP_

#include <set>
#include <string>

namespace TL
{
	
class TaskInfo;

class StateTransformHelper
{
public:	
	static std::string copy_from_state(TaskInfo* task_info);
	static std::string copy_from_state_all(const std::set<TaskInfo*>& 
			task_info_set);
	static std::string copy_to_state(TaskInfo* task_info);
	static std::string copy_to_state_all(const std::set<TaskInfo*>& 
			task_info_set);
	static std::string declare(TaskInfo* task_info);
	static std::string declare_all(const std::set<TaskInfo*>& task_info_set);

private:
	StateTransformHelper();
};

}

#endif /*TLSTATETRANSFORMHELPER_HPP_*/
