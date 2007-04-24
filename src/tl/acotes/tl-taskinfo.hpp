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
#ifndef TLTASKINFO_HPP_
#define TLTASKINFO_HPP_

#include <list>
#include <set>
#include <string>

#include "tl-symbol.hpp"

namespace TL
{
	class StreamInfo;
	class TaskgroupInfo;
	
	class TaskInfo
	{
	public:
		TaskInfo(TaskgroupInfo* taskgroup_info); 

        void                         add_firstprivate(const Symbol& symbol);
		void                         add_input(const Symbol& symbol);
		void                         add_istream(StreamInfo* is);
        void                         add_lastprivate(const Symbol& symbol);
		void                         add_loop_pop_istream(StreamInfo* is);
		void                         add_loop_push_ostream(StreamInfo* os);
		void                         add_loop_control_istream(StreamInfo* is);
		void                         add_loop_close_ostream(StreamInfo* os);
		void                         add_output(const Symbol& symbol);
		void                         add_ostream(StreamInfo* os);
		void                         add_private(const Symbol& symbol);
		void                         add_replace_pop_istream(StreamInfo* is);
		void                         add_replace_push_ostream(StreamInfo* os);
		void                         add_task_info_child(TaskInfo* child); 
		void                         compute_graph(void);
		const std::set<Symbol>&      get_firstprivates(void) const;
		const std::set<Symbol>&      get_lastprivates(void) const;
		const std::set<StreamInfo*>& get_loop_pop_istream_set(void) const;
		const std::set<StreamInfo*>& get_loop_push_ostream_set(void) const;
		const std::set<StreamInfo*>& get_loop_control_istream_set(void) const;
		const std::set<StreamInfo*>& get_loop_close_ostream_set(void) const;
		const std::string&           get_name(void) const;
		const std::set<Symbol>&      get_privates(void) const;
		const std::set<StreamInfo*>& get_replace_pop_istream_set(void) const;
		const std::set<StreamInfo*>& get_replace_push_ostream_set(void) const;
		const std::list<TaskInfo*>&  get_task_info_children(void) const;
		TaskInfo*                    get_task_info_first_child(void) const;
		TaskInfo*                    get_task_info_parent(void) const;
		bool                         has_istream(StreamInfo* is) const;
		bool                         has_ostream(StreamInfo* os) const;		
		bool                         has_task_info_children(void) const;

	private:
		std::string           _name;
		std::set<Symbol>      _firstprivates;
		std::set<Symbol>      _inputs;
		std::set<StreamInfo*> _istream_set;
		std::set<Symbol>      _lastprivates;
		std::set<StreamInfo*> _loop_pop_istream_set;
		std::set<StreamInfo*> _loop_push_ostream_set;
		std::set<StreamInfo*> _loop_control_istream_set;
		std::set<StreamInfo*> _loop_close_ostream_set;
		std::set<Symbol>      _outputs;
		std::set<StreamInfo*> _ostream_set;
		std::set<Symbol>      _privates;
		std::set<StreamInfo*> _replace_pop_istream_set;
		std::set<StreamInfo*> _replace_push_ostream_set;
		std::list<TaskInfo*>  _task_info_children;
		TaskInfo*             _task_info_parent;
		TaskgroupInfo* const  _taskgroup_info;
		
		void init_name();

		void compute_graph_input(const Symbol& symbol);
		void compute_graph_inputs(void);
		void compute_graph_output(const Symbol& symbol);
		void compute_graph_outputs(void);
	};
	
}

#endif /*TLTASKINFO_HPP_*/
