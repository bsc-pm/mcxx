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
#include "tl-fordistributetransformhelper.hpp"

#include <assert.h>

#include "tl-langconstruct.hpp"
#include "tl-fordistributeinfo.hpp"

namespace TL
{

// header ----------------------------------------------------------------------
std::string 
FordistributeTransformHelper::
header(FordistributeInfo* fordistribute_info)
{
    assert(fordistribute_info);
    
    std::stringstream ss;
    
    ForStatement for_statement= fordistribute_info->get_for_statement();
    
    ss      << "for"
            << "("
            << for_statement.get_iterating_init().prettyprint()
//            << ";"
            << for_statement.get_iterating_condition().prettyprint()
            << ";"
            << for_statement.get_iterating_expression().prettyprint()
            << ")"
            ;
    
    return ss.str();
}

// headers ---------------------------------------------------------------------
std::string
FordistributeTransformHelper::
headers(const std::set<FordistributeInfo*>& s)
{
    std::stringstream ss;
    
    for     ( std::set<FordistributeInfo*>::iterator it= s.begin()
            ; it != s.end()
            ; it++)
    {
        FordistributeInfo* fordistribute_info= *it;
        
        ss << header(fordistribute_info);
    }
        
    return ss.str();
}


// FordistributeTransformHelper constructor ------------------------------------
FordistributeTransformHelper::
FordistributeTransformHelper()
{
    // is a helper
    assert(0);
}

// FordistributeTransformHelper destructor -------------------------------------
FordistributeTransformHelper::
~FordistributeTransformHelper()
{
}

}
