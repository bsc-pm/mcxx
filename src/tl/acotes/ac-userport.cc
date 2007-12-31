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
#include "ac-userport.h"

#include <assert.h>
#include "ac-port.h"
#include "ac-task.h"
#include "tl-variableclause.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * UserPort creation
     * ****************************************************************/
    
    UserPort* UserPort::create(TL::LangConstruct* construct, TL::LangConstruct* body, Task* task)
    {
        UserPort* result= new UserPort();
        
        result->setConstruct(construct);
        result->setBody(body);
        result->setTask(task);
        
        return result;
    }
    
    UserPort::UserPort()
    : construct(NULL), body(NULL)
    , task(NULL)
    {
    }
    
    
        
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    void UserPort::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct /* call only once */);
        
        this->construct= construct;
    }
    
    void UserPort::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body /*call only once */);
        
        this->body= body;
    }
    
    
    
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/
    
    void UserPort::setTask(Task* task) 
    {
        assert(task);
        assert(!this->task /* call only once */);
        
        this->task= task;
        task->addUserPort(this);
    }
        
        
    /* ****************************************************************
     * * UserPort creation
     * ****************************************************************/
    
    void UserPort::addInputPort(Port* inport)
    {
        assert(inport);
        assert(inport->getTask() == this->getTask());
        assert(inport->isNamed());
        
        inputPortVector.push_back(inport);
    }
    
    void UserPort::addOutputPort(Port* outport)
    {
        assert(outport);
        assert(outport->getTask() == this->getTask());
        assert(outport->isNamed());
        
        outputPortVector.push_back(outport);
    }
    
} /* end namespace Acotes */ } /* end namespace TL */
