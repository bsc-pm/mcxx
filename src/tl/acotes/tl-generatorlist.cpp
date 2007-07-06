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
#include "tl-generatorlist.hpp"

#include <assert.h>
#include <sstream>

namespace TL
{

// GeneratorList constructor ---------------------------------------------------
GeneratorList::
GeneratorList()
{
}

// GeneratorList destructor ----------------------------------------------------
GeneratorList::
~GeneratorList()
{
}

// add -------------------------------------------------------------------------
void
GeneratorList::
add
		( Generator* generator
		)
{
	assert(generator);
	
	_generator_list.push_back(generator);
}

// delete_all ------------------------------------------------------------------
void
GeneratorList::
delete_all
		( void
		)
{
	for		( std::list<Generator*>::iterator it= _generator_list.begin()
			; it != _generator_list.end()
			; it++
			)
	{
		Generator* generator= *it;
		
		delete generator;
	}
	
	_generator_list.clear();
}

// generate --------------------------------------------------------------------
std::string
GeneratorList::
generate
		( void
		)
{
	std::stringstream ss;
	
	for		( std::list<Generator*>::iterator it= _generator_list.begin()
			; it != _generator_list.end()
			; it++
			)
	{
		Generator* generator= *it;
		
		ss << generator->generate();
	}
	
	return ss.str();
}

}
