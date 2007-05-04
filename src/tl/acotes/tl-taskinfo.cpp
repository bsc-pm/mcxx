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
#include "tl-taskinfo.hpp"

#include <assert.h>

#include "tl-streaminfo.hpp"
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
}
		
// add_firstprivate ------------------------------------------------------------
void                    
TaskInfo::     
add_firstprivate
		( const Symbol& symbol
		)
{
	add_private(symbol);
	
	_firstprivates.insert(symbol);
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

// add_lastprivate -------------------------------------------------------------
void
TaskInfo::                         
add_lastprivate
		( const Symbol& symbol
		)
{
	add_private(symbol);
	
	_lastprivates.insert(symbol);
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

// add_private -----------------------------------------------------------------
void     
TaskInfo::                    
add_private
		( const Symbol& symbol
		)
{
	assert(!is_reference(symbol));
	
	_privates.insert(symbol);
}

// add_reference ---------------------------------------------------------------
void     
TaskInfo::                    
add_reference
		( const Symbol& symbol
		)
{
	assert(!is_private(symbol));
	
	_references.insert(symbol);
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
	
	compute_graph_inputs();
	compute_graph_outputs();
}

// get_firstprivates -----------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_firstprivates
		( void
		) const
{
	return _firstprivates;
}

// get_lastprivates ------------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_lastprivates
		( void
		) const
{
	return _lastprivates;
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

// get_privates ----------------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_privates
		( void
		) const
{
	return _privates;
}

// get_references --------------------------------------------------------------
const std::set<Symbol>&      
TaskInfo::
get_references
		( void
		) const
{
	return _references;
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

// is_private ------------------------------------------------------------------
bool
TaskInfo::
is_private
		( const Symbol& symbol
		) const
{
	bool in= _privates.find(symbol) != _privates.end();
	
	return in;
}

// is_reference ----------------------------------------------------------------
bool
TaskInfo::
is_reference
		( const Symbol& symbol
		) const
{
	bool in= _references.find(symbol) != _references.end();
	
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

// initializatiors -------------------------------------------------------------
void
TaskInfo::
init_name
		(
		)
{
	std::stringstream ss;
	
	ss
		<< "task_"
			<< (long) this 
			;
		
		_name= ss.str();
}

// compute_graph_input ---------------------------------------------------------
void 
TaskInfo::
compute_graph_input
		( const Symbol& input
		)
{
	assert(_task_info_parent);
	
	StreamInfo* stream_info= _taskgroup_info
			->new_stream_info(input, _task_info_parent, this);
			
	// is input for this task
	add_loop_pop_istream(stream_info);
	add_replace_push_ostream(stream_info);
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

	StreamInfo* stream_info= _taskgroup_info
			->new_stream_info(output, this, _task_info_parent);
			
	// is output for this task
	add_loop_push_ostream(stream_info);
	add_replace_pop_istream(stream_info);
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


} // end namespace TL
