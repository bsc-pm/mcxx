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
#include "ac-forreplicate.h"

#include <assert.h>
#include <ac-task.h>
#include <ac-variable.h>

namespace TL { namespace Acotes {

    /* ****************************************************************
     * * Creation
     * ****************************************************************/
    
    ForReplicate* ForReplicate::create(TL::ForStatement* forStatement, Task* task)
    {
        assert(forStatement);
        assert(task);
        
        ForReplicate* result= new ForReplicate();
        result->setForStatement(forStatement);
        result->setTask(task);
        
        return result;
    }
    
    ForReplicate::ForReplicate()
    : task(NULL)
    , forStatement(NULL)
    {
    }
     
    
    
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/
    
    void ForReplicate::setTask(Task* task)
    {
        assert(task);
        assert(!this->task);
        assert(forStatement);
        
        this->task= task;
        TL::Symbol symbol= forStatement->get_induction_variable().get_symbol();
        if (!task->hasVariable(symbol)) {
            Variable::create(task, symbol);
        }
        task->addForReplicate(this);
    }
     
    
    
    /* ****************************************************************
     * * ForStatement handling
     * ****************************************************************/
    
    void ForReplicate::setForStatement(TL::ForStatement* forStatement)
    {
        assert(forStatement);
        assert(!this->forStatement);
        
        this->forStatement= forStatement;                
    };
    
    
} /* end namespace TL */ } /* end namespace Acotes */

