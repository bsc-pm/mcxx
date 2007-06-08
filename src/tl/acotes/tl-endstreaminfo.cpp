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
#include "tl-endstreaminfo.hpp"

namespace TL
{

// EndStreamInfo constructor ---------------------------------------------------
EndStreamInfo::
EndStreamInfo
        ( const Symbol& symbol
        , TaskInfo* task_info
        , std::string type
        , std::string label
        )
        : _symbol(symbol)
        , _task_info(task_info)
{
    init_name(type, label);
    init_symbol_name();
}

// EndStreamInfo destructor ----------------------------------------------------
EndStreamInfo::~EndStreamInfo()
{
}

// get_name --------------------------------------------------------------------
std::string 
EndStreamInfo::
get_name
        ( void
        ) const
{
    return _name;
}

// get_symbol ------------------------------------------------------------------
Symbol      
EndStreamInfo::
get_symbol
        ( void
        ) const
{
    return _symbol;
}

// get_symbol_name -------------------------------------------------------------
std::string 
EndStreamInfo::
get_symbol_name
        ( void
        ) const
{
    return _symbol_name;
}

// get_task_info ---------------------------------------------------------------
TaskInfo*   
EndStreamInfo::
get_task_info
        ( void
        ) const
 {
    return _task_info;
 }
    

// init_name -------------------------------------------------------------------
void 
EndStreamInfo::
init_name(std::string type, std::string label)
{
    static int number= -1; number++;
    
    std::stringstream ss;
    
    ss      << "acolib__"
            << type
            << "__"
            << label
            << "__"
            << number
            ; 
    
    _name= ss.str();  
}

// init_symbol_name ------------------------------------------------------------
void
EndStreamInfo::
init_symbol_name(void)
{
    _symbol_name= _symbol.get_name();
} 
}
