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
#include "tl-transformlist.hpp"

#include <assert.h>

namespace TL
{

// TransformList constructor ---------------------------------------------------
TransformList::
TransformList()
{
}

// TransformList destructor ----------------------------------------------------
TransformList::
~TransformList()
{
}

// add -------------------------------------------------------------------------
void
TransformList::
add
		( Transform* transform
		)
{
	assert(transform);
	
	_transform_list.push_back(transform);
}

// delete_all ------------------------------------------------------------------
void
TransformList::
delete_all
		( void
		)
{
	for		( std::list<Transform*>::iterator it= _transform_list.begin()
			; it != _transform_list.end()
			; it++
			)
	{
		Transform* transform= *it;
		
		delete transform;
	}
	
	_transform_list.clear();
}

// transform -------------------------------------------------------------------
void
TransformList::
transform
		( void
		)
{
	for		( std::list<Transform*>::iterator it= _transform_list.begin()
			; it != _transform_list.end()
			; it++
			)
	{
		Transform* transform= *it;
		
		transform->transform();
	}
}

} // end namespace TL
