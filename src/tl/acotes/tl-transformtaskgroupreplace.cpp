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
#include "tl-transformtaskgroupreplace.hpp"

#include <assert.h>

#include "tl-mintakatransformhelper.hpp"
#include "tl-statetransformhelper.hpp"
#include "tl-streamtransformhelper.hpp"
#include "tl-tasktransformhelper.hpp"
#include "tl-threadtransformhelper.hpp"

namespace TL
{

// TransformTaskgroupReplace ---------------------------------------------------
TransformTaskgroupReplace::
TransformTaskgroupReplace
		( const PragmaCustomConstruct& pragma_custom_construct
		, TaskgroupInfo* taskgroup_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _taskgroup_info(taskgroup_info)
{
	assert(taskgroup_info);
}

TransformTaskgroupReplace::~TransformTaskgroupReplace()
{
}

// transform -------------------------------------------------------------------
void
TransformTaskgroupReplace::
transform
		( void
		)
{
	// Replaces the task
	Source task_replace_src= this->generate_replace();
	AST_t task_replace_tree= task_replace_src.parse_statement
			( _pragma_custom_construct.get_ast()
			, _pragma_custom_construct.get_scope_link()
			);
	_pragma_custom_construct.get_ast().replace(task_replace_tree);
}

// generate_body ---------------------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_body
		( void
		)
{
	std::stringstream ss;
	
	Statement taskgroup_body= _pragma_custom_construct.get_statement();
	ss << taskgroup_body.prettyprint();
	
	return ss.str();
}

// generate_connect ------------------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_connect_streams
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			connect_all(_taskgroup_info->get_stream_info_set());
	
	return ss.str();
}

// generate_close_streams ------------------------------------------------------
std::string
TransformTaskgroupReplace::
generate_close_streams
		( void
		)
{
	std::stringstream ss;
	
	TaskInfo* task_info_phantom= 
			_taskgroup_info->get_task_info_phantom();
	const std::set<StreamInfo*>& close_set=
			task_info_phantom->get_loop_close_ostream_set();
			
	ss << StreamTransformHelper::
			close_all(close_set);	
			
	return ss.str();
}

// generate_create_states ------------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_create_states
		( void
		)
{
	std::stringstream ss;
	
	ss << StateTransformHelper::
			declare_all(_taskgroup_info->get_task_info_set());
	ss << StateTransformHelper::
			copy_to_state_all(_taskgroup_info->get_task_info_set());
	
	return ss.str();
}

// generate_create_streams -----------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_create_streams
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			create_all(_taskgroup_info->get_stream_info_set());
	
	return ss.str();
}

// generate_create_threads -----------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_create_threads(void)
{
	std::stringstream ss;
	
	ss << ThreadTransformHelper::
			create_all(_taskgroup_info->get_task_info_set());
	
	return ss.str();
}

// generate_declare_threads ----------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_declare_threads(void)
{
	std::stringstream ss;
	
	ss << ThreadTransformHelper::
			declare_all(_taskgroup_info->get_task_info_set());
	
	return ss.str();
}

// generate_destroy_streams ----------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_destroy_streams
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			destroy_all(_taskgroup_info->get_stream_info_set());
	
	return ss.str();
}

// generate_join_threads -------------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_join_threads(void)
{
	std::stringstream ss;
	
	ss << ThreadTransformHelper::
			join_all(_taskgroup_info->get_task_info_set());
	
	return ss.str();
}

// generate_recover_states -----------------------------------------------------
std::string 
TransformTaskgroupReplace::
generate_recover_states
		( void
		)
{
	std::stringstream ss;
	
	ss << StateTransformHelper::
			copy_from_state_all(_taskgroup_info->get_task_info_set());
	
	return ss.str();
}

// generate_replace ------------------------------------------------------------
std::string
TransformTaskgroupReplace::
generate_replace
		( void
		)
{
	std::stringstream ss;
	
	ss 		<< "{"
			<< MintakaTransformHelper::initialize_taskgroup(_taskgroup_info)
			<< generate_create_states()
			<< generate_create_streams()
			<< generate_connect_streams()
			<< generate_declare_threads()
			<< generate_create_threads()
			<< MintakaTransformHelper::iteration_begin()
			<< generate_body()
			<< MintakaTransformHelper::iteration_end()
			<< generate_close_streams()
			<< generate_join_threads()
			<< MintakaTransformHelper::finalize_taskgroup(_taskgroup_info)
			<< generate_destroy_streams()
			<< generate_recover_states()
			<< "}"
			;
	
	return ss.str();
}

}
