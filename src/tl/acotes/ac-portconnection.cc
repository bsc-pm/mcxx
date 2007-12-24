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
#include "ac-portconnection.h"

#include <assert.h>

#include "ac-port.h"
#include "ac-task.h"
#include "ac-taskgroup.h"

namespace TL { namespace Acotes {
    
    
    /* ******************************************************
     * * PortConnection creation
     * ******************************************************/
    
    PortConnection* PortConnection::create(Port* output, Port* input)
    {
        assert(output);
        assert(input);
        assert(output->isOutput());
        assert(input->isInput());
        assert(output->getTask());
        assert(input->getTask());
        assert(output->getTask()->getTaskgroup() == input->getTask()->getTaskgroup());
        assert(!input->hasPortConnection());
        
        PortConnection* portConnection= new PortConnection();
        portConnection->setOutput(output);
        portConnection->setInput(input);
        output->addPortConnection(portConnection);
        input->addPortConnection(portConnection);
        
        Taskgroup* taskgroup= output->getTask()->getTaskgroup();
        taskgroup->addPortConnection(portConnection);
        
        return portConnection;
    }
    
    PortConnection::PortConnection() 
    : input(NULL), output(NULL)
    {
    }
     
    
} /* end namespace Acotes */ } /* end namespace TL */

