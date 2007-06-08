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
#ifndef TLTASKINFO_HPP_
#define TLTASKINFO_HPP_

#include <list>
#include <set>
#include <string>

#include "tl-symbol.hpp"

#include "tl-visibilityinfo.hpp"

namespace TL
{
class FordistributeInfo;
class InputStreamInfo;
class OutputStreamInfo;
class TargetInfo;
class TaskgroupInfo;

class TaskInfo : public VisibilityInfo 
{
public:
	TaskInfo(TaskgroupInfo* taskgroup_info);
	virtual ~TaskInfo(void); 

	void  add_export(const Symbol& symbol);
    void  add_fordistribute(FordistributeInfo* fordistribute);
	void  add_import(const Symbol& symbol);
	void  add_input(const Symbol& symbol);
	void  add_istream(InputStreamInfo* is);
	void  add_loop_pop(InputStreamInfo* is);
	void  add_loop_push(OutputStreamInfo* os);
	void  add_loop_control(InputStreamInfo* is);
	void  add_loop_close(OutputStreamInfo* os);
	void  add_output(const Symbol& symbol);
	void  add_ostream(OutputStreamInfo* os);
	void  add_replace_pop(InputStreamInfo* is);
	void  add_replace_push(OutputStreamInfo* os);
	void  add_target_input(const Symbol& symbol, const std::string& label);
	void  add_target_output(const Symbol& symbol, const std::string& label);
	void  add_task_info_child(TaskInfo* child);
	void  add_shortcut(const Symbol& symbol);
     
	void compute_graph(void);
    
	const std::set<Symbol>&             get_exports(void) const;
	const std::set<FordistributeInfo*>& get_fordistribute_info_set(void) const;
	const std::set<Symbol>&             get_imports(void) const;
	const std::set<InputStreamInfo*>&   get_loop_pop_set(void) const;
	const std::set<OutputStreamInfo*>&  get_loop_push_set(void) const;
	const std::set<InputStreamInfo*>&   get_loop_control_set(void) const;
	const std::set<OutputStreamInfo*>&  get_loop_close_set(void) const;
	const std::string&                  get_name(void) const;
	const std::set<InputStreamInfo*>&   get_replace_pop_set(void) const;
	const std::set<OutputStreamInfo*>&  get_replace_push_set(void) const;
	const std::string&                  get_state_name(void) const;
	const std::string&                  get_struct_state_name(void) const;
	const std::list<TaskInfo*>&         get_task_info_children(void) const;
	TaskInfo*                           get_task_info_first_child(void) const;
	TaskInfo*                           get_task_info_parent(void) const;
	TaskgroupInfo*                      get_taskgroup_info(void) const;
    
	bool             has_istream(InputStreamInfo* is) const;
	bool             has_ostream(OutputStreamInfo* os) const;		
	bool             has_task_info_children(void) const;
	bool             is_input(const Symbol& symbol) const;
	bool             is_output(const Symbol& symbol) const;
	bool             is_shortcut(const Symbol& symbol) const;
    
	TargetInfo*      new_target_info(const std::string& label);

private:
	std::string                  _name;
	std::set<Symbol>             _exports;
	std::set<FordistributeInfo*> _fordistribute_info_set;
	std::set<Symbol>             _imports;
	std::set<Symbol>             _inputs;
	std::set<InputStreamInfo*>   _istream_set;
	std::set<InputStreamInfo*>   _loop_pop_set;
	std::set<OutputStreamInfo*>  _loop_push_set;
	std::set<InputStreamInfo*>   _loop_control_set;
	std::set<OutputStreamInfo*>  _loop_close_set;
	std::set<Symbol>             _outputs;
	std::set<OutputStreamInfo*>  _ostream_set;
	std::set<Symbol>             _shortcuts;
	std::string                  _state_name;
	std::string                  _struct_state_name;
	std::set<InputStreamInfo*>   _replace_pop_set;
	std::set<OutputStreamInfo*>  _replace_push_set;
	std::set<TargetInfo*>        _target_info_set;
	std::list<TaskInfo*>         _task_info_children;
	TaskInfo*                    _task_info_parent;
	TaskgroupInfo* const         _taskgroup_info;
	
	void init_name(void);
	void init_state_name(void);
	void init_struct_state_name(void);

	void      compute_graph_input(const Symbol& symbol);
	void      compute_graph_inputs(void);
	void      compute_graph_output(const Symbol& symbol);
	void      compute_graph_outputs(void);
	void      compute_graph_shortcut(const Symbol& symbol);
	TaskInfo* compute_graph_shortcut_output(const Symbol& symbol, TaskInfo* 
			output);
	void      compute_graph_shortcuts(void);
};
	
}

#endif /*TLTASKINFO_HPP_*/
