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
#ifndef TLMINTAKATRANSFORMHELPER_HPP_
#define TLMINTAKATRANSFORMHELPER_HPP_

#include <string>

namespace TL
{

class TaskInfo;
class TaskgroupInfo;

class MintakaTransformHelper
{
public:
	static std::string declare_initialized_condition(void);
	static std::string initialized_condition_name(void);
	static std::string initialize_task(TaskInfo*);
	static std::string initialize_taskgroup(TaskgroupInfo*);
	static std::string iteration_begin(void);
	static std::string iteration_end(void);
	static std::string finalize_task(TaskInfo*);
	static std::string finalize_taskgroup(TaskgroupInfo*);

private:
	MintakaTransformHelper();
	
	static int s_task_count;
	
	static const int ITERATION_EVENT= 7000;
};

}

#endif /*TLMINTAKATRANSFORMHELPER_HPP_*/
