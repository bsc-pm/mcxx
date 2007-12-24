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
#include "ac-port.h"

#include <assert.h>

#include "ac-portconnection.h"
#include "ac-task.h"
#include "ac-taskgroup.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Port creation
     * ****************************************************************/
    
    /**
     * Create a new instance of a virtual input port.
     */
    Port* Port::createVirtualInputPort(Task* task)
    {
        Port* port= createPort(task);
        port->input= true;
        
        return port;
    }
    
    /** 
     * Create a new instance of a virtual output port.
     */
    Port* Port::createVirtualOutputPort(Task* task)
    {
        Port* port= createPort(task);
        port->output= true;
        
        return port;
    }
    
    /** 
     * Create a new instance of a port.
     */
    Port* Port::createPort(Task* task)
    {
        assert(task);
        
        Port* port= new Port();
        port->setTask(task);
        
        return port;
    }
    
    /**
     * Port constructor.
     */
    Port::Port()
    : task(NULL), number(-1)
    , input(false), output(false)
    , variable(NULL)
    , control(false)
    , artificial(false)
    {
    }
    
    
       
    /* ****************************************************************
     * * Task relationship support
     * ****************************************************************/
    
    /**
     * Sets the task of a port.
     * <p>
     * It also adds the port towards the task and retrieves its portNumber.
     */
    void Port::setTask(Task* task) {
        assert(task);
        assert(!this->task /* call only once */);
        
        this->task= task;
        this->number= task->addPort(this);
    }

    
    
    /* ****************************************************************
     * * Port connection relationship
     * ****************************************************************/
    
    /**
     * Adds a new port connection to this port.
     * <p>
     * Method called by portcollection.
     */
    void Port::addPortConnection(PortConnection* portConnection) {
        assert(portConnection);
        assert(portConnection->getInput() == this || portConnection->getOutput() == this);
        
        portConnectionVector.push_back(portConnection);
    }
    
    /**
     * Request the uniq port connection associated to this port.
     * <p>
     *
     */
    PortConnection* Port::getPortConnection() {
        assert(portConnectionVector.size() == 1);
        
        PortConnection* portConnection= portConnectionVector.at(0);
        return portConnection;
    }
    
    /**
     * Get the artificial counterpart if it exists.
     * <p>
     * Returns the counterpart or null if it doesn't exists.
     */
    Port* Port::getArtificialCounterpart() {
        Port* result= NULL;
        
        if (isControl() && portConnectionVector.size() == 1) {
            Port* counterpart= NULL;
            PortConnection* portConnection= getPortConnection();
            
            if (isInput()) {
                counterpart= portConnection->getOutput();
            } else if (isOutput()) {
                counterpart= portConnection->getInput();
            } else {
                assert(0);
            }
            
            if (counterpart->isArtificial()) {
                result= counterpart;
            }
        }
        
        return result;
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
