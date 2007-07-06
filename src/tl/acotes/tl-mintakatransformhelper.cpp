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
#include "tl-mintakatransformhelper.hpp"

#include <assert.h>
#include <sstream>

namespace TL
{

int MintakaTransformHelper::s_task_count= 0;


// declare_initialized_condition -----------------------------------------------
std::string 
MintakaTransformHelper::
declare_initialized_condition
		( void
		)
{
	std::stringstream ss;
	
	ss 		<< "static int "
			<< initialized_condition_name()
			<< "= 0"
			<< ";"
			;
	
	return ss.str();
}

// initialized_condition_name --------------------------------------------------
std::string 
MintakaTransformHelper::
initialized_condition_name
		( void
		)
{
	std::string name= "acotescc_mintaka__initialized";
	
	return name;
}

// initialize_task -------------------------------------------------------------
std::string 
MintakaTransformHelper::
initialize_task
		( TaskInfo*
		)
{
	std::stringstream ss;
	
	s_task_count++;
	int task_num= s_task_count + 1;
	
	ss		<< ""
			<< "mintaka_thread_begin(1, "<< task_num << ");"
			<< ""
			;
	
	return ss.str();
}

// initialize_taskgroup --------------------------------------------------------
std::string 
MintakaTransformHelper::
initialize_taskgroup
		( TaskgroupInfo*
		)
{
	std::stringstream ss;
	
	ss		<< ""
			<<   initialized_condition_name() << "++;"
			<<   "if (" << initialized_condition_name() << " == 1)"
			<<   "{"
			<<     "mintaka_set_filebase(\"acotrace\");"
			<<     "mintaka_app_begin();"
			<<     "mintaka_thread_begin(1, 1);"
			<<   "}"
			<< ""
			;
	
	return ss.str();
}

// iteration_begin -------------------------------------------------------------
std::string 
MintakaTransformHelper::
iteration_begin
		( void
		)
{
	std::stringstream ss;

	ss	<< ""
		<<   "mintaka_event(" << ITERATION_EVENT << ", 1);"
	 	<<   "mintaka_state_run();"
	 	<< ""
	 	;
	
	return ss.str();
}

// iteration_end ---------------------------------------------------------------
std::string 
MintakaTransformHelper::
iteration_end
		( void
		)
{
	std::stringstream ss;

	ss	<< ""
		<<   "mintaka_event(" << ITERATION_EVENT << ", 0);"
	 	<<   "mintaka_state_schedule();"
	 	<< ""
	 	;
	
	return ss.str();
}


// finalize_task ---------------------------------------------------------------
std::string 
MintakaTransformHelper::
finalize_task
		( TaskInfo*
		)
{
	std::stringstream ss;
	
	ss		<< ""
			<< "mintaka_thread_end();"			
			<< ""
			;
	
	return ss.str();
}

// finalize_taskgroup ----------------------------------------------------------
std::string 
MintakaTransformHelper::
finalize_taskgroup
		( TaskgroupInfo*
		)
{
	std::stringstream ss;
	
	ss		<< ""
			<<   initialized_condition_name() << "--;"
			<<   "if (" << initialized_condition_name() << " == 0)"
			<<   "{"
			<<     "mintaka_thread_end();"
			<<     "mintaka_app_end();"
			<<     "mintaka_merge();"
			<<   "}"			
			<< ""
			;
	
	return ss.str();
}

// MintakaTransformHelper constructor ------------------------------------------
MintakaTransformHelper::MintakaTransformHelper()
{
	// is a helper, cannot have instances
	assert(0);
}

}
