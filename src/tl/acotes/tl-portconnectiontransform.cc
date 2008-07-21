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
    
    $Id: tl-acotestransform.cpp 1611 2007-07-10 09:28:44Z drodenas $
*/
#include "tl-portconnectiontransform.h"


#include <assert.h>
#include <sstream>
#include "ac-port.h"
#include "ac-portconnection.h"
#include "ac-task.h"

namespace TL { namespace Acotes {
    
        
    /* ******************************************************
     * * No constructor
     * ******************************************************/
    
    PortConnectionTransform::PortConnectionTransform(const std::string &d)
            : driver(d)
    {
    }
    
    
    /* ******************************************************
     * * Auxiliary generators
     * ******************************************************/
    
    std::string PortConnectionTransform::generatePortConnection(PortConnection* portConnection)
    {
        assert(portConnection);
        
        std::stringstream ss;
        
        ss << "port_connect"
                << "( " << portConnection->getOutput()->getTask()->getName()
                << ", " << portConnection->getOutput()->getNumber()
                << ", " << portConnection->getInput()->getTask()->getName()
                << ", " << portConnection->getInput()->getNumber()
                << ");";
        
        return ss.str();
    }
    
    
    
} /* end namespace Acotes */ } /* end namespace TL */
