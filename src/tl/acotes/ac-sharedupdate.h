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
// 
// File:   ac-sharedupdate.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 11:03
//

#ifndef _AC_SHAREDUPDATE_H
#define	_AC_SHAREDUPDATE_H

#include <vector>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
        
    class State;
    class Task;
    class Variable;
    
    class SharedUpdate {
        
    // -- Constructor
    public:
        static SharedUpdate* create(TL::LangConstruct* construct, Task* task);
    private:
        SharedUpdate();
        
    // -- Body
    public:
        TL::LangConstruct* getConstruct() const { return construct; }
    private:
        void setConstruct(TL::LangConstruct* construct);
        TL::LangConstruct* construct;
        
    // -- Task relationship
    private:
        void setTask(Task* task);
        Task* getTask() const { return task; }
        Task* task;
        
    // -- State relationship
    public:
        void addVariable(Variable* variable);
        const std::vector<State*> &getStateVector() const { return stateVector; }
    private:
        std::vector<State*> stateVector;
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _AC_SHAREDUPDATE_H */

