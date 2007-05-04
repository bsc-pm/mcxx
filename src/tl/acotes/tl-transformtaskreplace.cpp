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
#include "tl-transformtaskreplace.hpp"

#include <assert.h>

#include "tl-streamtransformhelper.hpp"
#include "tl-streaminfo.hpp"
#include "tl-taskinfo.hpp"

namespace TL
{

// TransformTaskReplace constructor --------------------------------------------
TransformTaskReplace::
TransformTaskReplace
		( const PragmaCustomConstruct& pragma_custom_construct
		, TaskInfo* task_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _task_info(task_info)
{
	assert(task_info);
}
		
// transform -------------------------------------------------------------------
void
TransformTaskReplace::
transform
		(
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
		
// generate_replace_poppes -----------------------------------------------------
const std::string
TransformTaskReplace::
generate_replace_popes
		(
		) const
{
	std::stringstream ss;
	
	const std::set<StreamInfo*>& streams= 
			_task_info->get_replace_pop_istream_set();
			
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* stream= *it;
		
		ss << StreamTransformHelper::pop(stream);
	}
	
	return ss.str();
}
		
// generate_replace_pushes -----------------------------------------------------
const std::string
TransformTaskReplace::
generate_replace_pushes
		(
		) const
{
	std::stringstream ss;
	
	const std::set<StreamInfo*>& streams= 
			_task_info->get_replace_push_ostream_set();
			
	for		( std::set<StreamInfo*>::iterator it= streams.begin()
			; it != streams.end()
			; it++
			)
	{
		StreamInfo* stream= *it;

		ss << StreamTransformHelper::push(stream);
	}
	
	return ss.str();
}
		
// generate_replace ------------------------------------------------------------
const std::string
TransformTaskReplace::
generate_replace
		(
		) const
{
	std::stringstream ss;
	
	ss
		<< "{"
		<< generate_replace_pushes()
		<< generate_replace_popes()
		<< "}" 
		;
	
	return ss.str();
}

} // end namespace TL

