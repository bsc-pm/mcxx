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
#include "ac-sharedcheck.h"

#include <assert.h>
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Constructor
     * ****************************************************************/
    
    SharedCheck* SharedCheck::create(TL::LangConstruct* construct, TL::LangConstruct* body, Task* task)
    {
        assert(construct);
        assert(body);
        assert(task);
        
        SharedCheck* result= new SharedCheck();
        result->setConstruct(construct);
        result->setBody(body);
        result->setTask(task);
        
        return result;
    }
    
    SharedCheck::SharedCheck()
    : construct(NULL), body(NULL)
    , task(NULL)
    {
    }
    
        
        
    /* ****************************************************************
     * * Body
     * ****************************************************************/
        
    void SharedCheck::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct);
        
        this->construct= construct;
    }
    
    void SharedCheck::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body);
        
        this->body= body;
    }
        
    
        
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/

    void SharedCheck::setTask(Task* task)
    {
        assert(task);
        assert(!this->task);
        
        this->task= task;
        task->addSharedCheck(this);
    }
    
    
    
    /* ****************************************************************
     * * State relationship
     * ****************************************************************/
   
    void SharedCheck::addVariable(Variable* variable)
    {
        assert(variable);
        assert(variable->getTask() == task);
        assert(task->hasState(variable->getSymbol()));
        assert(task->getState(variable->getSymbol())->isShared());

        State* state= task->getState(variable->getSymbol());
        stateVector.push_back(state);
    }
    

} /* end namespace Acotes */ } /* end namespace TL */
