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
#include "tl-transformmintakaoutline.hpp"

#include "tl-mintakatransformhelper.hpp"

namespace TL
{


bool TransformMintakaOutline::s_global_condition_declared= false;

// TransformMintakaOutline constructor -----------------------------------------
TransformMintakaOutline::
TransformMintakaOutline
		( const PragmaCustomConstruct& pragma_custom_construct
		)
		: _pragma_custom_construct(pragma_custom_construct)
{
}

// TransformMintakaOutline destructor ------------------------------------------
TransformMintakaOutline::~TransformMintakaOutline()
{
}

//  generate_declare_initialized_condition -------------------------------------
std::string 
TransformMintakaOutline::
generate_declare_initialized_condition
		( void
		)
{
	std::stringstream ss;
	
	ss << MintakaTransformHelper::declare_initialized_condition();
	
	return ss.str();	
}

// transform -------------------------------------------------------------------
void
TransformMintakaOutline::
transform
		( void
		)
{
	std::cerr << "mintaka outline transform: begin" << std::endl;
	if (!s_global_condition_declared)
	{
		std::cerr << "mintaka outline transform: added" << std::endl;
		s_global_condition_declared= true;
		Source declare_src= generate_declare_initialized_condition();
		
		// Add outline task
		FunctionDefinition function_definition 
			= _pragma_custom_construct.get_enclosing_function();
			
		AST_t function_ast = function_definition.get_ast();
		ScopeLink function_scope_link = function_definition.get_scope_link();
		
		AST_t task_add_tree = declare_src
				.parse_global(function_ast, function_scope_link);
			
		function_definition.get_ast().prepend_sibling_function(task_add_tree);	
	}
	std::cerr << "mintaka outline transform: end" << std::endl;
}

}
