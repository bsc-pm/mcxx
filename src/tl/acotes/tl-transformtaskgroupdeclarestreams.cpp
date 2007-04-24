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
#include "tl-transformtaskgroupdeclarestreams.hpp"

#include <assert.h>

#include "tl-streamtransformhelper.hpp"
#include "tl-taskgroupinfo.hpp"

namespace TL
{

// TransformTaskgroupDeclareStreams constructor --------------------------------
TransformTaskgroupDeclareStreams::
TransformTaskgroupDeclareStreams
		( const PragmaCustomConstruct& pragma_custom_construct
		, TaskgroupInfo* taskgroup_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _taskgroup_info(taskgroup_info)
{
	assert(taskgroup_info);
}

// TransformTaskgroupDeclareStreams destructor ---------------------------------
TransformTaskgroupDeclareStreams::
~TransformTaskgroupDeclareStreams
		(
		)
{
}

// transform -------------------------------------------------------------------
void
TransformTaskgroupDeclareStreams::
transform
		( void
		)
{	
	Source declares_src= this->generate_declares();
	
	// Pragma code add
	FunctionDefinition function_definition 
		= _pragma_custom_construct.get_enclosing_function();
		
	AST_t function_ast = function_definition.get_ast();
	ScopeLink function_scope_link = function_definition.get_scope_link();
	
	AST_t task_add_tree = declares_src
			.parse_global(function_ast, function_scope_link);
		
	function_definition.get_ast().prepend_sibling_function(task_add_tree);
	
}

// generate_declares -----------------------------------------------------------
std::string
TransformTaskgroupDeclareStreams::
generate_declares
		( void
		) const
{
	std::stringstream ss;
	
	const std::set<StreamInfo*>& stream_info_set= 
			_taskgroup_info->get_stream_info_set();
	
	for		( std::set<StreamInfo*>::iterator it= stream_info_set.begin()
			; it != stream_info_set.end()
			; it++)
	{
		StreamInfo* s= *it;
		
		ss << StreamTransformHelper::declare(s);
	}

	return ss.str();
}

}
