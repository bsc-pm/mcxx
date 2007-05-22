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
#ifndef TLTRANSFORMTARGETREPLACE_HPP_
#define TLTRANSFORMTARGETREPLACE_HPP_

#include <string>

#include "tl-pragmasupport.hpp"
#include "tl-transform.hpp"

namespace TL
{

class TargetInfo;

class TransformTargetReplace : public TL::Transform
{
public:
	TransformTargetReplace(const PragmaCustomConstruct& pragma_custom_construct, 
			TargetInfo* target_info);
	virtual ~TransformTargetReplace();
	
	virtual void transform(void);
	
private:
	PragmaCustomConstruct _pragma_custom_construct;
	TargetInfo*           _target_info;
	
	std::string generate_body(void);
	std::string generate_pops(void);
	std::string generate_pushes(void);
	std::string generate_replace(void);
};

}

#endif /*TLTRANSFORMTARGETREPLACE_HPP_*/
