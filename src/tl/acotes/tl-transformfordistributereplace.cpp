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
#include "tl-transformfordistributereplace.hpp"

#include <assert.h>

#include "tl-fordistributeinfo.hpp"

namespace TL
{

// TransformFordistribute constructor ------------------------------------------
TransformFordistributeReplace::
TransformFordistributeReplace
		( PragmaCustomConstruct pragma_custom_construct
		, FordistributeInfo* fordistribute_info
		)
		: _fordistribute_info(fordistribute_info)
		, _pragma_custom_construct(pragma_custom_construct) 
{
	assert(fordistribute_info);
}

// TransformFordistribute destructor -------------------------------------------
TransformFordistributeReplace::
~TransformFordistributeReplace()
{
	delete _fordistribute_info;
}

// transform -------------------------------------------------------------------
void
TransformFordistributeReplace::
transform
		( void
		)
{
	// Replaces the task
	Source replace_src= this->generate_replace();
	AST_t replace_tree= replace_src.parse_statement
			( _pragma_custom_construct.get_ast()
			, _pragma_custom_construct.get_scope_link()
			);
	_pragma_custom_construct.get_ast().replace(replace_tree);
	
}

// generate_body ---------------------------------------------------------------
std::string 
TransformFordistributeReplace::
generate_body
		( void
		)
{
	std::stringstream ss;
	
	ForStatement for_statement= _fordistribute_info->get_for_statement();
	ss << for_statement.prettyprint();
	
	return ss.str();
}

// generate_replace ------------------------------------------------------------
std::string
TransformFordistributeReplace::
generate_replace
		( void
		)
{
	std::stringstream ss;
	
	ss << generate_body();
	
	return ss.str();
}

}
