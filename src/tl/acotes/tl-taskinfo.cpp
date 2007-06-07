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
#include "tl-taskinfo.hpp"

#include <assert.h>

#include "tl-streaminfo.hpp"
#include "tl-targetinfo.hpp"
#include "tl-targetstreaminfo.hpp"
#include "tl-taskgroupinfo.hpp"

namespace TL
{

// TaskInfo constructor --------------------------------------------------------
TaskInfo::
TaskInfo
		( TaskgroupInfo* taskgroup_info
		) 
		: _task_info_parent((TaskInfo*)0)
		, _taskgroup_info(taskgroup_info)
{
	assert(taskgroup_info);
	
	init_name();
	init_state_name();
	init_struct_state_name();
}

// TaskInfo destructor ---------------------------------------------------------
TaskInfo::
~TaskInfo(void)
{
	for 	( std::set<TargetInfo*>::iterator it= _target_info_set.begin()
			; it != _target_info_set.end()
			; it++
			)
	{
		TargetInfo* target_info= *it;
		
		delete target_info;
	}
}
		
// add_export ------------------------------------------------------------------
void                    
TaskInfo::     
add_export
		( const Symbol& symbol
		)
{
	add_firstprivate(symbol);
	
	_exports.insert(symbol);
}
		
// add_fordistribute -----------------------------------------------------------
void
TaskInfo::
add_fordistribute(FordistributeInfo* fordistribute_info)
{
	assert(fordistribute_info);
	
    inherit_visibilities((VisibilityInfo*)fordistribute_info);
	_fordistribute_info_set.insert(fordistribute_info);
}

// add_import ------------------------------------------------------------------
void                    
TaskInfo::     
add_import
		( const Symbol& symbol
		)
{
	add_firstprivate(symbol);
	
	_imports.insert(symbol);
}
		
// add_input -------------------------------------------------------------------
void
TaskInfo::
add_input
		( const Symbol& symbol
		)
{
	_inputs.insert(symbol);
	
	add_private(symbol);		
}

// add_istream -----------------------------------------------------------------
void
TaskInfo::
add_istream
		( StreamInfo* is      
		)
{
	assert(is);
	assert(is->get_task_info_istream() == this);
	assert(!has_istream(is));
	
	_istream_set.insert(is);
}

// add_loop_pop_istream --------------------------------------------------------
void
TaskInfo::
add_loop_pop_istream
		( StreamInfo* is
		)
{
	add_loop_control_istream(is);
		
	_loop_pop_istream_set.insert(is);
}

// add_loop_push_ostream -------------------------------------------------------
void
TaskInfo::
add_loop_push_ostream
		( StreamInfo* os
		)
{
	add_loop_close_ostream(os);
	
	_loop_push_ostream_set.insert(os);
}

// add_loop_control_istream ----------------------------------------------------
void                        
TaskInfo::
add_loop_control_istream
		( StreamInfo* is
		)
{
	add_istream(is);
	
	_loop_control_istream_set.insert(is);
}

// add_loop_close_ostream ------------------------------------------------------
void                        
TaskInfo::
add_loop_close_ostream
		( StreamInfo* os
		)
{
	add_ostream(os);
	
	_loop_close_ostream_set.insert(os);
}

// add_output ------------------------------------------------------------------
void
TaskInfo::
add_output
		( const Symbol& symbol
		)
{
	_outputs.insert(symbol);
	
	add_private(symbol);		
}

// add_ostream -----------------------------------------------------------------
void                        
TaskInfo::
add_ostream
		( StreamInfo* os
		)
{
	assert(os);
	assert(os->get_task_info_ostream() == this);
	assert(!has_ostream(os));
	
	_ostream_set.insert(os);
}

// add_replace_pop_istream -----------------------------------------------------
void                        
TaskInfo::
add_replace_pop_istream
		( StreamInfo* is
		)
{
	assert(is);
	assert(has_ostream(is));
	assert(_task_info_parent);
	
	_task_info_parent->add_istream(is);
	_replace_pop_istream_set.insert(is);
}

// add_replace_push_ostream ----------------------------------------------------
void
TaskInfo::
add_replace_push_ostream
		( StreamInfo* os
		)
{
	assert(os);
	assert(has_istream(os));
	assert(_task_info_parent);
	
	_task_info_parent->add_loop_close_ostream(os);
	_replace_push_ostream_set.insert(os);
}

// add_target_input ------------------------------------------------------------
void                         
TaskInfo::
add_target_input
		( const Symbol& symbol
		, const std::string& label
		)
{
	add_private(symbol);

	TargetStreamInfo* target_stream_info= 
			_taskgroup_info->get_target_stream_info(symbol, label);
			
	target_stream_info->set_task_info_istream(this, true);
}

// add_target_output -----------------------------------------------------------
void
TaskInfo::
add_target_output
		( const Symbol& symbol
		, const std::string& label
		)
{
	add_private(symbol);

	TargetStreamInfo* target_stream_info= 
			_taskgroup_info->get_target_stream_info(symbol, label);
			
	target_stream_info->set_task_info_ostream(this, true);
}

// add_task_info_children ------------------------------------------------------
void
TaskInfo::
add_task_info_child
		( TaskInfo* child
		) 
{
	assert(child);
	assert(!child->_task_info_parent);
	
	child->_task_info_parent= this;
	_task_info_children.push_back(child);
}

// add_shortcut ----------------------------------------------------------------
void                         
TaskInfo::
add_shortcut
		( const Symbol& symbol
		)
{
	_shortcuts.insert(symbol);
}

// compute_graph ---------------------------------------------------------------
void                        
TaskInfo::
compute_graph
		( void
		)
{
	for		( std::list<TaskInfo*>::iterator it= _task_info_children.begin()
			; it != _task_info_children.end()
			; it++)
	{
		TaskInfo* child= *it;
		
		child->compute_graph();
	}
	
	compute_graph_shortcuts();
	compute_graph_inputs();
	compute_graph_outputs();
}

// get_exports -----------------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_exports
		( void
		) const
{
	return _exports;
}

// get_fordistribute_info_set --------------------------------------------------
const std::set<FordistributeInfo*>& 
TaskInfo::
get_fordistribute_info_set
		( void
		) const
{
	return _fordistribute_info_set;
}

// get_imports -----------------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_imports
		( void
		) const
{
	return _imports;
}

// get_loop_pop_istream_set ----------------------------------------------------
const std::set<StreamInfo*>&
TaskInfo:: 
get_loop_pop_istream_set
		( void
		) const
{
	return _loop_pop_istream_set;
}
		
// get_loop_push_ostream_set ---------------------------------------------------
const std::set<StreamInfo*>& 
TaskInfo:: 
get_loop_push_ostream_set
		( void
		) const
{
	return _loop_push_ostream_set;
}
		
// get_loop_control_istream_set ------------------------------------------------
const std::set<StreamInfo*>& 
TaskInfo:: 
get_loop_control_istream_set
		( void
		) const
{
	return _loop_control_istream_set;
}
		
// get_loop_close_ostream_set --------------------------------------------------
const std::set<StreamInfo*>& 
TaskInfo:: 
get_loop_close_ostream_set
		( void
		) const
{
	return _loop_close_ostream_set;
}

// get_name --------------------------------------------------------------------
const std::string&
TaskInfo::
get_name
		(
		) const
{
	return _name;
}

// get_replace_pop_istream_set -------------------------------------------------
const std::set<StreamInfo*>& 
TaskInfo::
get_replace_pop_istream_set
		( void
		) const
{
	return _replace_pop_istream_set;
}

// get_replace_push_ostream_set ------------------------------------------------
const std::set<StreamInfo*>& 
TaskInfo::
get_replace_push_ostream_set
		( void
		) const
{
	return _replace_push_ostream_set;
}

// get_state_name --------------------------------------------------------------
const std::string&
TaskInfo::
get_state_name
		( void
		) const
{
	return _state_name;
}

// get_struct_state_name -------------------------------------------------------
const std::string&
TaskInfo::
get_struct_state_name
		( void
		) const
{
	return _struct_state_name;
}

// get_task_info_children ------------------------------------------------------
const std::list<TaskInfo*>& 
TaskInfo::
get_task_info_children
		( void
		) const
{
	return _task_info_children;
}

// get_task_info_first_child ---------------------------------------------------
TaskInfo*                   
TaskInfo::
get_task_info_first_child
		( void
		) const
{
	assert(has_task_info_children());
	
	TaskInfo* task_info_first_child= _task_info_children
			.begin()
			.operator*();
			
	return task_info_first_child;
}

// get_task_info_parent --------------------------------------------------------
TaskInfo*
TaskInfo::
get_task_info_parent
		( 
		) const
{
	return _task_info_parent;
} 		

// get_taskgroup_info ----------------------------------------------------------
TaskgroupInfo*
TaskInfo::
get_taskgroup_info
		( void
		) const
{
	return _taskgroup_info;
}

// has_istream -----------------------------------------------------------------
bool
TaskInfo::
has_istream
		( StreamInfo* is
		) const
{
	assert(is);
	
	bool in= _istream_set.find(is) != _istream_set.end();
	
	return in;
}

// has_ostream -----------------------------------------------------------------
bool
TaskInfo::
has_ostream
		( StreamInfo* os
		) const
{
	assert(os);
	
	bool in= _ostream_set.find(os) != _ostream_set.end();
	
	return in;
}

// has_task_info_children ------------------------------------------------------
bool                        
TaskInfo::
has_task_info_children
		( void
		) const
{
	bool empty_task_info_children= _task_info_children
			.empty();
			
	return !empty_task_info_children;
}		

// is_input --------------------------------------------------------------------
bool
TaskInfo::
is_input
		( const Symbol& symbol
		) const
{
	bool in= _inputs.find(symbol) != _inputs.end();
	
	return in;
}

// is_output -------------------------------------------------------------------
bool
TaskInfo::
is_output
		( const Symbol& symbol
		) const
{
	bool in= _outputs.find(symbol) != _outputs.end();
	
	return in;
}

// is_shortcut -----------------------------------------------------------------
bool
TaskInfo::
is_shortcut
		( const Symbol& symbol
		) const
{
	bool in= _shortcuts.find(symbol) != _shortcuts.end();
	
	return in;
}

// new_target_info -------------------------------------------------------------
TargetInfo*                  
TaskInfo::
new_target_info
		( const std::string& label
		)
{
	TargetInfo* target_info= new TargetInfo(_taskgroup_info, this, label);
	
	_target_info_set.insert(target_info);
	
	return target_info;
}

// init_name -------------------------------------------------------------------
void
TaskInfo::
init_name
		(
		)
{
	std::stringstream ss;
	static int last_num= 0;
	
	ss
		<< "task_"
		<< last_num 
			;
	last_num++;
		
	_name= ss.str();
}

// init_state_name -------------------------------------------------------------
void
TaskInfo::
init_state_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "state_"
		<< get_name() 
		;
		
	_state_name= ss.str();
}

// init_struct_state_name ------------------------------------------------------
void
TaskInfo::
init_struct_state_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "struct state_"
		<< get_name() 
		;
		
	_struct_state_name= ss.str();
}

// compute_graph_input ---------------------------------------------------------
void 
TaskInfo::
compute_graph_input
		( const Symbol& input
		)
{
	assert(_task_info_parent);
	
	if		(  !is_shortcut(input) 
			&& !_task_info_parent->is_shortcut(input)
			)
	{
		StreamInfo* stream_info= _taskgroup_info
				->new_stream_info(input, _task_info_parent, this);
				
		// is input for this task
		add_loop_pop_istream(stream_info);
		add_replace_push_ostream(stream_info);
	}
}

// compute_graph_inputs --------------------------------------------------------
void 
TaskInfo::
compute_graph_inputs
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _inputs.begin()
			; it != _inputs.end()
			; it++
			)
	{
		Symbol input= *it;
		
		compute_graph_input(input);
	}
}

// compute_graph_output --------------------------------------------------------
void 
TaskInfo::
compute_graph_output
		( const Symbol& output
		)
{
	assert(_task_info_parent);
	
	if		(  !is_shortcut(output) 
			&& !_task_info_parent->is_shortcut(output)
			)
	{
		StreamInfo* stream_info= _taskgroup_info
				->new_stream_info(output, this, _task_info_parent);
				
		// is output for this task
		add_loop_push_ostream(stream_info);
		add_replace_pop_istream(stream_info);
	} 
}

// compute_graph_outputs -------------------------------------------------------
void 
TaskInfo::
compute_graph_outputs
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _outputs.begin()
			; it != _outputs.end()
			; it++
			)
	{
		Symbol output= *it;
		
		compute_graph_output(output);
	}
}

// compute_graph_shortcut ------------------------------------------------------
void 
TaskInfo::
compute_graph_shortcut
		( const Symbol& symbol
		)
{
	TaskInfo* output= (TaskInfo*)0;

	output= compute_graph_shortcut_output(symbol, output);	
}

// compute_graph_shortcut_output
TaskInfo* 
TaskInfo::
compute_graph_shortcut_output
		( const Symbol& symbol
		, TaskInfo* output
		)
{
	// if here is also shortcutted
	if (is_shortcut(symbol))
	{
		// for each initial input before output 
		for		( std::list<TaskInfo*>::iterator it= _task_info_children.begin()
				; it != _task_info_children.end()
				; it++
				)
		{
			TaskInfo* child_task_info= *it;
			
			// if symbol is input calls to it as symbol/output
			// if symbol is output overwrites it, 
			// if both, do both
			output= child_task_info->
					compute_graph_shortcut_output(symbol, output); 
		}
	
	// if not shortcutted output
	} else /* if (!is_shortcut(symbol)) */ {
		// if it is input shortcut
		if (is_input(symbol) && output) {
			StreamInfo* stream_info= _taskgroup_info
					->new_stream_info(symbol, output, this);
					
			// connect!!
			// is input for this task
			this->add_loop_pop_istream(stream_info);
			// output for the other task
			output->add_loop_push_ostream(stream_info);
		} 
		// if it is output is the next output
		if (is_output(symbol)) {
			output= this;
		}
	}
	
	return output;
}

// compute_graph_shortcut ------------------------------------------------------
void 
TaskInfo::
compute_graph_shortcuts
		( void
		)
{
	for		( std::set<Symbol>::iterator it= _shortcuts.begin()
			; it != _shortcuts.end()
			; it++
			)
	{
		Symbol shortcut= *it;
		
		compute_graph_shortcut(shortcut);
	}
}


} // end namespace TL
