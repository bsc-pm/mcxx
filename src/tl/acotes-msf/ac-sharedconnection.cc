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
#include "ac-sharedconnection.h"

#include <assert.h>
#include "ac-state.h"
#include "ac-task.h"
#include "ac-taskgroup.h"

namespace TL { namespace Acotes {
    
    /* ****************************************************************
     * * Creation
     * ****************************************************************/
    
    SharedConnection* SharedConnection::create(State* source, State* target)
    {
        assert(source);
        assert(target);
        assert(source->getTask() != target->getTask());
        assert(source->getTask()->getTaskgroup() == target->getTask()->getTaskgroup());
        
        SharedConnection* result= new SharedConnection();
        result->setSource(source);
        result->setTarget(target);
        
        return result;
    }
        
    SharedConnection::SharedConnection()
    : source(NULL), target(NULL)
    {
    }
        
    /* ****************************************************************
     * * State handling
     * ****************************************************************/
    
    void SharedConnection::setSource(State* state)
    {
        assert(state);
        assert(state->isUpdateShared());
        assert(!this->source);
        
        this->source= state;
        state->getTask()->getTaskgroup()->addSharedConnection(this);
    }
    void SharedConnection::setTarget(State* state)
    {
        assert(state);
        assert(state->isShared());
        assert(!this->target);
        
        this->target= state;
    }
    
} /* end namespace Acotes */ } /* end namespace TL */

