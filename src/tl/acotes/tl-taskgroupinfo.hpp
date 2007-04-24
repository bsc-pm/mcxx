/*
    Mercurium C/C++ Compiler
    Copyright (C) 2006-2007 - Roger Ferrer Ibanez <roger.ferrer@bsc.es>
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
#ifndef TLTASKGROUPINFO_HPP_
#define TLTASKGROUPINFO_HPP_

#include <list>
#include <set>
#include <string>

#include "tl-symbol.hpp"

namespace TL
{
	class TaskInfo;
	class Transform;
	class StreamInfo;
	
	class TaskgroupInfo
	{
	public:
		TaskgroupInfo();
		~TaskgroupInfo();
		
		void                         add_transform(Transform* transform);
		const std::string&           get_name(void) const;
		const std::set<StreamInfo*>& get_stream_info_set(void) const;
		TaskInfo*                    get_task_info_phantom(void) const;
		const std::set<TaskInfo*>&   get_task_info_set(void) const;
		void                         compute_graph(void);
		StreamInfo*                  new_stream_info(const Symbol& symbol, 
				TaskInfo* task_info_ostream, TaskInfo* task_info_istream);
		TaskInfo*                    new_task_info(void);
		void                         transform_all(void);
		
	private:
		// TaskgroupInfo fields ------------------------------------------------
		std::string           _name;
		std::set<StreamInfo*> _stream_info_set;
		TaskInfo*             _task_info_phantom;
		std::set<TaskInfo*>   _task_info_set;
		std::list<Transform*> _transform_list;
		
		void init_name(void);
	};

} // end namespace TL

#endif /*TLTASKGROUPINFO_HPP_*/
