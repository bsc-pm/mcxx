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
#include "tl-transformtaskoutline.hpp"

#include <assert.h>

#include "tl-fordistributetransformhelper.hpp"
#include "tl-mintakatransformhelper.hpp"
#include "tl-streamtransformhelper.hpp"
#include "tl-symboltransformhelper.hpp"
#include "tl-tasktransformhelper.hpp"
#include "tl-taskgroupinfo.hpp"


namespace TL
{

// TransformTaskOutline constructor --------------------------------------------
TransformTaskOutline::
TransformTaskOutline
		( const PragmaCustomConstruct& pragma_custom_construct
		, TaskInfo* task_info
		)
		: _pragma_custom_construct(pragma_custom_construct)
		, _task_info(task_info)
{
}

// TransformTaskOutline destructor ---------------------------------------------
TransformTaskOutline::
~TransformTaskOutline
		(
		)
{
}

// transform -------------------------------------------------------------------
void
TransformTaskOutline::
transform
		(
		)
{
	Source declares_src= this->generate_outline();
	
	// Add outline task
	FunctionDefinition function_definition 
		= _pragma_custom_construct.get_enclosing_function();
		
	AST_t function_ast = function_definition.get_ast();
	ScopeLink function_scope_link = function_definition.get_scope_link();
	
	AST_t task_add_tree = declares_src
			.parse_global(function_ast, function_scope_link);
		
	function_definition.get_ast().prepend_sibling_function(task_add_tree);
}

// generate_argument_name ------------------------------------------------------
std::string 
TransformTaskOutline::
generate_argument_name
		( void
		)
{
	std::stringstream ss;
	
	ss << _task_info->get_state_name() << "_argument";
	
	return ss.str();
}

// generate_body ---------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_body
		( void
		)
{
	std::stringstream ss;
	
	// original body
	Statement task_body= _pragma_custom_construct.get_statement();
	
	ss << task_body.prettyprint();
	
	return ss.str();
}

// generate_closes -------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_closes
		( void
		)
{
	std::stringstream ss;

	ss 	<< ""
		<<   StreamTransformHelper::
				close_all(_task_info->get_loop_close_ostream_set())
		<<   StreamTransformHelper::
				close_all(_task_info->get_exports())
		<< ""
		;
			
	return ss.str();
}

// generate_declares -----------------------------------------------------------
std::string 
TransformTaskOutline::
generate_declares
		( void
		)
{
	std::stringstream ss;
	
	ss << SymbolTransformHelper::declare_all(_task_info->get_privates());
	
	return ss.str();
}

// generate_eos ----------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_eos
		( void
		)
{
	std::stringstream ss;
	
	ss		<< "!("
			<< StreamTransformHelper::
					eos_any(_task_info->get_loop_control_istream_set())
			<< " || "
			<< StreamTransformHelper::
					eos_any(_task_info->get_imports())
			<< ")"
			;
			
	return ss.str();
}

// generate_fordistributes -----------------------------------------------------
std::string 
TransformTaskOutline::
generate_fordistributes
        ( void
        )
{
    std::stringstream ss;
    
    ss << FordistributeTransformHelper::
            headers(_task_info->get_fordistribute_info_set());
    
    return ss.str();
}

// generate_outline ------------------------------------------------------------
std::string
TransformTaskOutline::
generate_outline
		(
		)
{
	std::stringstream ss;
	
	ss		<< "static void* "
			<< TaskTransformHelper::outline_name(_task_info)
			<< "(void * " << generate_argument_name() << ")"
			<< "{"
			<<   MintakaTransformHelper::initialize_task(_task_info)
			<<   generate_declares()
			<<   generate_state()
			<<   generate_pops()
			<<   "while (" << generate_eos() << ")"
            <<   generate_fordistributes()
			<<   "{"
			<<     MintakaTransformHelper::iteration_begin()
			<<     generate_body()
			<<     MintakaTransformHelper::iteration_end()
			<<     generate_pushes()
			<<     generate_pops()
			<<   "}"
			<<   generate_closes()
			<<   generate_return_state()
			<<   MintakaTransformHelper::finalize_task(_task_info)
			<< "}"
			;
	
	return ss.str();
}

// generate_peeks --------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_peeks
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			peek_all(_task_info->get_loop_pop_istream_set());
	
	return ss.str();
}

// generate_pops ---------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_pops
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			pop_all(_task_info->get_loop_pop_istream_set());
	
	return ss.str();
}

// generate_pops_expression ----------------------------------------------------
std::string 
TransformTaskOutline::
generate_pops_expression
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			pop_all_expression(_task_info->get_loop_pop_istream_set());
	
	return ss.str();
}

// generate_pushes -------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_pushes
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			push_all(_task_info->get_loop_push_ostream_set());
	
	return ss.str();
}

// generate_recover_state ------------------------------------------------------
std::string 
TransformTaskOutline::
generate_recover_state
		( void
		)
{
	std::stringstream ss;

	ss	<< generate_struct_state_name() << " " << generate_state_name()
		<< "= "
		<< "(" << generate_struct_state_name() << "*) " 
		<< generate_argument_name() 
		<< ";" 	
		;
	
	return ss.str();
}

// generate_return_state -------------------------------------------------------
std::string 
TransformTaskOutline::
generate_return_state
		( void
		)
{
	std::stringstream ss;

	ss	<< ""
		//<< generate_recover_state()
		<< SymbolTransformHelper::
				copy_all_to_struct
						( _task_info->get_lastprivates()
						, generate_state_name() 
						)
		<< ""
		;
	
	return ss.str();
}

// generate_state --------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_state
		( void
		)
{
	std::stringstream ss;

	ss	<< ""
		<< generate_recover_state()
		<< SymbolTransformHelper::
				copy_all_from_struct
						( _task_info->get_firstprivates()
						, generate_state_name() 
						)
		<< ""
		;
	
	return ss.str();
}

// generate_state_name ---------------------------------------------------------
std::string 
TransformTaskOutline::
generate_state_name
		( void
		)
{	
	std::stringstream ss;
	
	ss << "(*" << _task_info->get_state_name() << ")";
	
	return ss.str();
}

// generate_struct_state_name --------------------------------------------------
std::string 
TransformTaskOutline::
generate_struct_state_name
		( void
		)
{
	std::stringstream ss;
	
	ss << _task_info->get_struct_state_name();
	
	return ss.str();
}

// generate_waits --------------------------------------------------------------
std::string 
TransformTaskOutline::
generate_waits
		( void
		)
{
	std::stringstream ss;
	
	ss << StreamTransformHelper::
			wait_istream_all(_task_info->get_loop_pop_istream_set());
	ss << StreamTransformHelper::
			wait_ostream_all(_task_info->get_loop_push_ostream_set());
	
	return ss.str();
}

}
