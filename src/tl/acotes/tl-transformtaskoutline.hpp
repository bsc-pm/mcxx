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
#ifndef TLTRANSFORMTASKOUTLINE_HPP_
#define TLTRANSFORMTASKOUTLINE_HPP_

#include <string>

#include "tl-pragmasupport.hpp"
#include "tl-taskinfo.hpp"
#include "tl-transform.hpp"

namespace TL 
{

class TransformTaskOutline : public Transform
{
public:
	TransformTaskOutline
			( const PragmaCustomConstruct& pragma_custom_construct
			, TaskInfo* task_info
			);
	virtual ~TransformTaskOutline();
	
	virtual void transform(void);
	
private:
	PragmaCustomConstruct _pragma_custom_construct;
	TaskInfo*             _task_info;
	
	std::string generate_body(void);
	std::string generate_closes(void);
	std::string generate_declares(void);
	std::string generate_eos(void);
	std::string generate_outline(void);
	std::string generate_peeks(void);
	std::string generate_pops(void);
	std::string generate_pushes(void);
	std::string generate_waits(void);
};

}

#endif /*TLTRANSFORMTASKOUTLINE_HPP_*/
