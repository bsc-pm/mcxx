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
#include "tl-forreplicateinfo.hpp"

namespace TL
{

// ForreplicateInfo constructor -----------------------------------------------
ForreplicateInfo::
ForreplicateInfo(const ForStatement& for_statement)
	: _for_statement(for_statement)
{	
    if (_for_statement.regular_loop())
    {
        Symbol induction_variable= 
                _for_statement.get_induction_variable().get_symbol();
                
        add_private(induction_variable);
    }    
}

// ForreplicateInfo destructor ------------------------------------------------
ForreplicateInfo::
~ForreplicateInfo()
{
}


// get_for_statement -----------------------------------------------------------
const ForStatement& 
ForreplicateInfo::
get_for_statement()
{
	return _for_statement;
}

}
