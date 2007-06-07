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
#include "tl-visibilityinfo.hpp"

namespace TL
{

// VisibilityInfo constructor --------------------------------------------------
VisibilityInfo::VisibilityInfo()
{
}

// VisibilityInfo destructor ---------------------------------------------------
VisibilityInfo::
~VisibilityInfo()
{
}


// add_firstprivate ------------------------------------------------------------
void                    
VisibilityInfo::
add_firstprivate
		( const Symbol& symbol
		)
{
	add_private(symbol);
	_firstprivates.insert(symbol);
}

// add_lastprivate -------------------------------------------------------------
void                    
VisibilityInfo::
add_lastprivate
		( const Symbol& symbol
		)
{
	add_private(symbol);
	_lastprivates.insert(symbol);
}

// add_private -----------------------------------------------------------------
void                    
VisibilityInfo::
add_private
		( const Symbol& symbol
		)
{
	_privates.insert(symbol);
}

// get_firstprivates -----------------------------------------------------------
const std::set<Symbol>&                    
VisibilityInfo::
get_firstprivates
		( void
		) const
{
	return _firstprivates;
}

// get_lastprivates ------------------------------------------------------------
const std::set<Symbol>&                    
VisibilityInfo::
get_lastprivates
		( void
		) const
{
	return _lastprivates;
}

// get_privates ----------------------------------------------------------------
const std::set<Symbol>&                    
VisibilityInfo::
get_privates
		( void
		) const
{
	return _privates;
}
bool                    
VisibilityInfo::
is_firstprivate
		( const Symbol& symbol
		) const
{
	bool in= _firstprivates.find(symbol) != _firstprivates.end();
	
	return in;
}
bool                    
VisibilityInfo::
is_lastprivate
		( const Symbol& symbol
		) const
{
	bool in= _lastprivates.find(symbol) != _lastprivates.end();
	
	return in;
}
bool                    
VisibilityInfo::
is_private(const Symbol& symbol) const
{
	bool in= _privates.find(symbol) != _privates.end();
	
	return in;
}


}
