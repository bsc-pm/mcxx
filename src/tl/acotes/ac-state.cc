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
#include "ac-state.h"

#include <assert.h>
#include "ac-variable.h"
#include "ac-task.h"
#include "ac-taskgroup.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Creator
     * ****************************************************************/
    
    /** 
     * Creates a fully functional new state variable.
     */
    State* State::create(Variable* variable)
    {
        assert(variable);
        assert(variable->hasTask());
        
        State* state= new State();
        state->setVariable(variable);
        state->setTask(variable->getTask());
        
        return state;
    }
    
    /** 
     * Creates a fully functional new state variable.
     */
    State* State::createCopyIn(Variable* variable)
    {
        assert(variable);
        
        State* state= create(variable);
        state->setCopyIn(true);
        
        return state;
    }
    
    /** 
     * Creates a fully functional new state variable.
     */
    State* State::createCopyOut(Variable* variable)
    {
        assert(variable);
        
        State* state= create(variable);
        state->setCopyOut(true);
        
        return state;
    }
 
    /**
     * Default private constructor.
     */
    State::State() 
    : task(NULL), number(-1)
    , variable(NULL)
    , copyIn(false), copyOut(false)
    {   
    }
    
    
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/
    
    /**
     * Sets the task for this state.
     * <p>
     * It calls to add state from task and retrieves the state number.
     */
    void State::setTask(Task* task) {
        assert(task);
        assert(!this->task /* call only once */);
        
        this->task= task;
        number= task->addState(this);
    }
    
    
    /* ****************************************************************
     * * Variable relataionship
     * ****************************************************************/

    /**
     * Sets the variable for the state.
     */
    void State::setVariable(Variable* variable) {
        assert(variable);
        assert(!this->variable /* call only once */);
        
        this->variable= variable;
    }
    

    
    /* ****************************************************************
     * * CopyIn or CopyOut definition   
     * ****************************************************************/
    
    void State::setCopyIn(bool value) {
        assert(value);
        assert(!this->copyIn /* call only once */);
        assert(variable);
        assert(variable->hasTask());
        assert(variable->getTask()->hasTaskgroup());
        
        this->copyIn= value;
        variable->getTask()->getTaskgroup()->addCopyStateVector(this);
    }
    
    void State::setCopyOut(bool value) {
        assert(value);
        assert(!this->copyIn /* call only once */);
        assert(variable);
        assert(variable->hasTask());
        assert(variable->getTask()->hasTaskgroup());
        
        this->copyOut= value;
        variable->getTask()->getTaskgroup()->addCopyStateVector(this);
    }

} /* end namespace Acotes */ } /* end namespace TL */

