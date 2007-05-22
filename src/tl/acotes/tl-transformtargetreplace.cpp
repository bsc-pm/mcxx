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
#include "tl-transformtargetreplace.hpp"

#include "tl-streamtransformhelper.hpp"
#include "tl-targetinfo.hpp"
#include "tl-targetstreaminfo.hpp"

namespace TL
{

// TransformTargetReplace constructor ------------------------------------------
TransformTargetReplace::
TransformTargetReplace
		( const PragmaCustomConstruct& pragma_custom_construct
		, TargetInfo* target_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _target_info(target_info)
{
}

// TransformTargetReplace destructor -------------------------------------------
TransformTargetReplace::
~TransformTargetReplace()
{
}

// transform -------------------------------------------------------------------
void
TransformTargetReplace::
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
TransformTargetReplace::
generate_body
		( void
		)
{
	std::stringstream ss;
	
	Statement taskgroup_body= _pragma_custom_construct.get_statement();
	ss << taskgroup_body.prettyprint();
	
	return ss.str();
}

// generate_pops ---------------------------------------------------------------
std::string
TransformTargetReplace::
generate_pops
		( void
		)
{
	std::stringstream ss;
	
	std::set<TargetStreamInfo*> tss= _target_info->get_istream_target_info_set();
	for		( std::set<TargetStreamInfo*>::iterator it= tss.begin()
			; it != tss.end()
			; it++
			)
	{
		TargetStreamInfo* s= *it;
		
		ss << StreamTransformHelper::pop(s->get_stream_info());
	}
	
	
	return ss.str();
}

// generate_pushes -------------------------------------------------------------
std::string
TransformTargetReplace::
generate_pushes
		( void
		)
{
	std::stringstream ss;
	
	std::set<TargetStreamInfo*> tss= _target_info->get_ostream_target_info_set();
	for		( std::set<TargetStreamInfo*>::iterator it= tss.begin()
			; it != tss.end()
			; it++
			)
	{
		TargetStreamInfo* s= *it;
		
		ss << StreamTransformHelper::push(s->get_stream_info());
	}
	
	
	return ss.str();
}

// generate_replace ------------------------------------------------------------
std::string
TransformTargetReplace::
generate_replace
		( void
		)
{
	std::stringstream ss;
	
	ss	<< "{"
		<< generate_pops()
		<< generate_body()
		<< generate_pushes()
		<< "}"
		;
	
	return ss.str();
}

}
