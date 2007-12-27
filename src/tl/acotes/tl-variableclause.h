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
        TL::Symbol getSymbol(unsigned position);
        
    };
    
} /* end namespace Acotes */ } /* end namespace TL */


#endif	/* _TL_VARIABLECLAUSE_H */

