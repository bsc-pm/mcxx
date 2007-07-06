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
#include "tl-transformforreplicatereplace.hpp"

#include <assert.h>

#include "tl-forreplicateinfo.hpp"

namespace TL
{

// TransformForreplicate constructor ------------------------------------------
TransformForreplicateReplace::
TransformForreplicateReplace
		( PragmaCustomConstruct pragma_custom_construct
		, ForreplicateInfo* forreplicate_info
		)
		: _forreplicate_info(forreplicate_info)
		, _pragma_custom_construct(pragma_custom_construct) 
{
	assert(forreplicate_info);
}

// TransformForreplicate destructor -------------------------------------------
TransformForreplicateReplace::
~TransformForreplicateReplace()
{
	delete _forreplicate_info;
}

// transform -------------------------------------------------------------------
void
TransformForreplicateReplace::
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
TransformForreplicateReplace::
generate_body
		( void
		)
{
	std::stringstream ss;
	
	ForStatement for_statement= _forreplicate_info->get_for_statement();
	ss << for_statement.prettyprint();
	
	return ss.str();
}

// generate_replace ------------------------------------------------------------
std::string
TransformForreplicateReplace::
generate_replace
		( void
		)
{
	std::stringstream ss;
	
	ss << generate_body();
	
	return ss.str();
}

}
