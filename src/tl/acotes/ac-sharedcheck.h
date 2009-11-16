/*--------------------------------------------------------------------
  (C) Copyright 2006-2009 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/

// 
// File:   ac-sharedcheck.h
// Author: drodenas
//
// Created on 30 / desembre / 2007, 17:05
//

#ifndef _AC_SHAREDCHECK_H
#define	_AC_SHAREDCHECK_H

#include <vector>
#include <tl-langconstruct.hpp>

namespace TL { namespace Acotes {
    
    class Task;
    class State;
    class Variable;
    
    class SharedCheck {
        
    // -- Constructor
    public:
        static SharedCheck* create(TL::LangConstruct* construct, Task* task);
    private:
        SharedCheck();
        
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


#endif	/* _AC_SHAREDCHECK_H */

