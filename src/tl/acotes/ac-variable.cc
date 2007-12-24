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
#include "ac-variable.h"

#include <assert.h>
#include <ac-task.h>

namespace TL { namespace Acotes {
    

    /* ****************************************************************
     * * Creation support
     * ****************************************************************/
    
    /** 
     * Variable creator.
     * <p>
     * Creates the variable with the requested symbol at task.
     */
    Variable* Variable::create(Task* task, TL::Symbol symbol) 
    {
        assert(task);
        assert(!task->getVariable(symbol));
        
        Variable* variable= new Variable();
        variable->setSymbol(new TL::Symbol(symbol));
        variable->setTask(task);
        
        variable->setName(symbol.get_name());
        variable->setArray(false);
        variable->setElementType(new TL::Type(symbol.get_type()));
        variable->setElementCount(1);
        
        return variable;
    }
    
    /**
     * Variable default constructor.
     */
    Variable::Variable()
    : task(NULL)
    , symbol(NULL)
    , array(false), elementType(NULL), elementCount(0)
    {
        
    }
    
    
    /* ****************************************************************
     * * Name
     * ****************************************************************/

    /**
     * Sets the name for this variable.
     */
    void Variable::setName(const std::string &name)
    {
        this->name= name;
    }
    
    
    
    /* ****************************************************************
     * * Task relationship
     * ****************************************************************/
    
    /**
     * Set the task for this variable instance.
     * <p>
     * It calls to addVariable of task.
     * Method called by the creator.
     */
    void Variable::setTask(Task* task)
    {
        assert(task);
        assert(!this->task /* Call once */);
        
        this->task= task;
        task->addVariable(this);
    }
        
    
    
    /* ****************************************************************
     * * Symbol support
     * ****************************************************************/

    /**
     * Query if it has the same symbol.
     */
    bool Variable::hasSymbol(TL::Symbol symbol) const {
        bool result;
        
        result= this->symbol && *this->symbol == symbol;
        
        return result;
    }
    
    /**
     * Sets the symbol of this variable.
     * <p>
     * Method called by the creator.
     */
    void Variable::setSymbol(TL::Symbol* symbol)
    {
        this->symbol= symbol;
    }
    
} /* end namespace Acotes */ } /* end namespace TL */

