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
#include "ac-teamreplicate.h"

#include <assert.h>
#include "ac-task.h"

namespace TL { namespace Acotes {
 
    /* ****************************************************************
     * * Creator
     * ****************************************************************/
    
    TeamReplicate* TeamReplicate::create(TL::LangConstruct* construct, TL::LangConstruct* body, Task* task)
    {
        assert(construct);
        assert(body);
        assert(task);
        
        TeamReplicate* result= new TeamReplicate();
        result->setConstruct(construct);
        result->setBody(body);
        result->setTask(task);
        
        return result;
    }
    
    TeamReplicate::TeamReplicate()
    : construct(NULL), body(NULL)
    , task(NULL)
    {        
    }
    
        
  
    /* ****************************************************************
     * * LangConstruct handling 
     * ****************************************************************/
    
    void TeamReplicate::setConstruct(TL::LangConstruct* construct)
    {
        assert(construct);
        assert(!this->construct);
        
        this->construct= construct;
    }
    
    void TeamReplicate::setBody(TL::LangConstruct* body)
    {
        assert(body);
        assert(!this->body);
        
        this->body= body;
    }

    
    
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/
    
    void TeamReplicate::setTask(Task* task)
    {
        assert(task);
        assert(task->isTeam());
        assert(!this->task);
        
        this->task= task;
        task->addTeamReplicate(this);
    }

    
    
} /* end namespace Acotes */ } /* end namespace TL */

