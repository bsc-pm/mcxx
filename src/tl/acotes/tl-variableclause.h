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
// File:   tl-variableclause.h
// Author: drodenas
//
// Created on 24 / desembre / 2007, 12:04
//

#ifndef _TL_VARIABLECLAUSE_H
#define	_TL_VARIABLECLAUSE_H

#include "tl-pragmasupport.hpp"

namespace TL { namespace Acotes {

    class Task;
    class Variable;
    
    class VariableClause
    : public TL::PragmaCustomClause
    {
    // -- Constructor
    public:
        VariableClause(TL::PragmaCustomClause clause, Task* task);
    private:
        Task* task;
        
    // -- Variable support
    public:
        Variable* getVariable(unsigned position);
        bool hasLabel(unsigned position);
        std::string getLabel(unsigned position);
        unsigned getVariableCount();
    private:
        TL::Expression getExpression(unsigned position);
        Variable* getNonArrayVariable(TL::Expression e);
        Variable* getArrayVariable(TL::Expression e);
        
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_VARIABLECLAUSE_H */

