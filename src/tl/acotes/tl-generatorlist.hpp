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
#ifndef TLGENERATORLIST_HPP_
#define TLGENERATORLIST_HPP_

#include "tl-generator.hpp"

#include <list>

namespace TL
{

class GeneratorList : public TL::Generator
{
public:
	GeneratorList();
	virtual ~GeneratorList();
	
	void                add(Generator* generator);
	void                delete_all(void);
	virtual std::string generate(void);

private:
	std::list<Generator*> _generator_list;
};

}

#endif /*TLGENERATORLIST_HPP_*/
