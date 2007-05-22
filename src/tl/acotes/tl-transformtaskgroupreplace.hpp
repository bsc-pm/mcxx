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
#ifndef TLTRANSFORMTASKGROUPREPLACE_HPP_
#define TLTRANSFORMTASKGROUPREPLACE_HPP_

#include <string>

#include "tl-generatorlist.hpp"
#include "tl-pragmasupport.hpp"
#include "tl-transformlist.hpp"
#include "tl-taskgroupinfo.hpp"

namespace TL
{

class TransformTaskgroupReplace : public TL::Transform
{
public:
	TransformTaskgroupReplace
			( const PragmaCustomConstruct& pragma_custom_construct
			, TaskgroupInfo* taskgroup_info
			);
	virtual ~TransformTaskgroupReplace();
		
	void         add_create_generator(Generator* generator);
	void         add_previous_transform(Transform* transform);
	virtual void transform(void);
	
private:
	GeneratorList         _create_generator_list;
	PragmaCustomConstruct _pragma_custom_construct;
	TransformList         _previous_transform_list;
	TaskgroupInfo*        _taskgroup_info;
	
	std::string generate_body(void);
	std::string generate_connect_streams(void);
	std::string generate_close_streams(void);
	std::string generate_create_states(void);
	std::string generate_create_streams(void);
	std::string generate_create_threads(void);
	std::string generate_declare_threads(void);
	std::string generate_destroy_streams(void);
	std::string generate_join_threads(void);
	std::string generate_recover_states(void);
	std::string generate_replace(void);
};

}

#endif /*TLTRANSFORMTASKGROUPREPLACE_HPP_*/
