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
#include "tl-visibilitysupporthelper.hpp"

#include "tl-visibilityinfo.hpp"

namespace TL
{

// add_clauses -----------------------------------------------------------------
void 
VisibilitySupportHelper::
add_clauses
        ( VisibilityInfo* info
        , PragmaCustomConstruct pragma_custom_construct
        )
{
    ObjectList<IdExpression> vars;
    
    // Adds private to task information
    vars= pragma_custom_construct
            .get_clause("private")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_private(symbol);
    } 
    // Adds firstprivate to task information
    vars= pragma_custom_construct
            .get_clause("firstprivate")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_firstprivate(symbol);
    } 
    // Adds lastprivate to task information
    vars= pragma_custom_construct
            .get_clause("lastprivate")
            .id_expressions();
    for     ( ObjectList<IdExpression>::iterator it= vars.begin()
            ; it != vars.end()
            ; it++
            )
    {
        IdExpression var= *it;
        Symbol symbol= var.get_symbol();
        
        info->add_lastprivate(symbol);
    } 

}


// VisibilitySupportHelper constructor -----------------------------------------
VisibilitySupportHelper::
VisibilitySupportHelper()
{
}

// VisibilitySupportHelper destructor ------------------------------------------
VisibilitySupportHelper::
~VisibilitySupportHelper()
{
}

}
