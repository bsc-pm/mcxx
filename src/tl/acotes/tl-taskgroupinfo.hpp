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
#ifndef TLTASKGROUPINFO_HPP_
#define TLTASKGROUPINFO_HPP_

#include <list>
#include <map>
#include <set>
#include <string>

#include "tl-symbol.hpp"

namespace TL
{
    
    class InputStreamInfo;
    class OutputStreamInfo;
	class TaskInfo;
	class Transform;
	class TransformTaskgroupReplace;
    class StreamInfo;
	class TargetStreamInfo;
	
	class TaskgroupInfo
	{
	public:
		TaskgroupInfo();
		~TaskgroupInfo();
		
		const std::string&           get_name(void) const;
		const std::set<StreamInfo*>& get_stream_info_set(void) const;
		TargetStreamInfo*            get_target_stream_info(const std::string& 
                label);
		TaskInfo*                    get_task_info_phantom(void) const;
		const std::set<TaskInfo*>&   get_task_info_set(void) const;
		TransformTaskgroupReplace*   get_transform_taskgroup_replace(void) const;
		void                         compute_graph(void);
        StreamInfo*                  new_stream_info(const Symbol& symbol, 
                TaskInfo* task_info_ostream, TaskInfo* task_info_istream);
		StreamInfo*                  new_stream_info(OutputStreamInfo*
                output_stream_info, InputStreamInfo* input_stream_info);
		TaskInfo*                    new_task_info(void);
		void                         set_transform_taskgroup_replace(
				TransformTaskgroupReplace* transform_taskgroup_replace);
		
	private:
		// TaskgroupInfo fields ------------------------------------------------
		std::string                             _name;
		std::set<StreamInfo*>                   _stream_info_set;
		std::map<std::string,TargetStreamInfo*> _target_stream_info_map;
		TaskInfo*                               _task_info_phantom;
		std::set<TaskInfo*>                     _task_info_set;
		TransformTaskgroupReplace*              _transform_taskgroup_replace;
		
		void init_name(void);
	};

} // end namespace TL

#endif /*TLTASKGROUPINFO_HPP_*/
