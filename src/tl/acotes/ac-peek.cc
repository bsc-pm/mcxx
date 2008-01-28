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
#include "ac-peek.h"

#include <assert.h>
#include "ac-port.h"
#include "ac-state.h"
#include "ac-task.h"
#include "ac-variable.h"

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Creator
     * ****************************************************************/
    
    Peek* Peek::create(TL::LangConstruct* construct, TL::LangConstruct* body, Port* port, State* history, State* index)
    {
        assert(construct);
        assert(body);
        assert(port);
        assert(history);
        
        Peek* result= new Peek();
        result->setConstruct(construct);
        result->setBody(body);
        result->setPort(port);
        result->setHistory(history);
        if (index) {
            result->setIndex(index);
        }
        
        return result;
    }
    
    Peek::Peek()
    : construct(NULL), body(NULL)
    , port(NULL)
    , history(NULL)
    , index(NULL)
    {
    }
    
    
        
    /* ****************************************************************
     * * LangConstruct support
     * ****************************************************************/
    
    void Peek::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct);
        
        this->construct= construct;
    }
    
    void Peek::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body);
        
        this->body= body;
    }
    
    
    
    /* ****************************************************************
     * * Task
     * ****************************************************************/
    
     Task* Peek::getTask() const
     {
         return getPort()->getTask();
     }
    
    
    
    /* ****************************************************************
     * * Input port relationship
     * ****************************************************************/
    
    void Peek::setPort(Port* port)
    {
        assert(port);
        assert(!this->port);
        assert(!history || history->getTask() == port->getTask());
        
        this->port= port;
        port->setPeek(this);
    }
        
        
    
    /* ****************************************************************
     * * History variable
     * ****************************************************************/
    
    int Peek::getPeekWindow() 
    {
        int result= history->getVariable()->getElementCount();
        
        return result;
    }
    
    void Peek::setHistory(State* state)
    {
        assert(state);
        assert(!this->history);
        assert(!port || state->getTask() == port->getTask());
        assert(state->isCopyIn());
        
        this->history= state;
    }
    
    
    
    /* ****************************************************************
     * * Optative index variable
     * ****************************************************************/
    
    void Peek::setIndex(State* state)
    {
        assert(state);
        assert(!this->index);
        assert(history);
        assert(state->getTask() == state->getTask());
        assert(state->isCopyIn());
        
        this->index= state;
    }

    
    
} /* end namespace Acotes */ } /* end namespace TL */
